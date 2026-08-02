use crate::handle_open_request;
use crate::restore_or_create_workspace;
use agent_ui::{
    AgentPanel, ExternalSourcePrompt, TerminalId,
    terminal_thread_metadata_store::{TerminalThreadMetadata, TerminalThreadMetadataStore},
};
use anyhow::{Context as _, Result, anyhow};
use cli::{CliRequest, CliResponse, CliResponseSink};
use cli::{IpcHandshake, ipc};
use client::{ZedLink, parse_zed_link};
use db::kvp::KeyValueStore;
use editor::Editor;
use fs::Fs;
use futures::channel::mpsc::{UnboundedReceiver, UnboundedSender};
use futures::channel::{mpsc, oneshot};
use futures::future;

use futures::{FutureExt, StreamExt};
use git_ui::{file_diff_view::FileDiffView, multi_diff_view::MultiDiffView};
use gpui::{App, AsyncApp, Global, Keystroke, TaskExt, WindowHandle};
use onboarding::FIRST_OPEN;
use onboarding::show_onboarding_view;
use recent_projects::{RemoteSettings, navigate_to_positions, open_remote_project};
use remote::{RemoteConnectionOptions, WslConnectionOptions};
use settings::Settings;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::thread;
use std::time::Duration;
use terminal::Modes;
use terminal_view::TerminalView;
use ui::SharedString;
use util::ResultExt;
use util::debug_panic;
use util::paths::PathWithPosition;
use workspace::PathList;
use workspace::item::ItemHandle;
use workspace::{AppState, MultiWorkspace, OpenOptions, OpenResult, SerializedWorkspaceLocation};
use zed_actions::{CreateWorktree, NewWorktreeBranchTarget};

#[derive(Default, Debug)]
pub struct OpenRequest {
    pub kind: Option<OpenRequestKind>,
    pub open_paths: Vec<String>,
    pub diff_paths: Vec<[String; 2]>,
    pub diff_all: bool,
    pub dev_container: bool,
    pub open_channel_notes: Vec<(u64, Option<String>)>,
    pub join_channel: Option<u64>,
    pub remote_connection: Option<RemoteConnectionOptions>,
    pub open_behavior: Option<cli::OpenBehavior>,
}

pub enum OpenRequestKind {
    CliConnection(
        (
            mpsc::UnboundedReceiver<CliRequest>,
            Box<dyn CliResponseSink>,
        ),
    ),
    FocusApp,
    Extension {
        extension_id: String,
    },
    AgentPanel {
        external_source_prompt: Option<ExternalSourcePrompt>,
    },
    InstallSkill {
        /// Full `SKILL.md` contents embedded in a `zed://skill` share link.
        content: String,
    },
    DockMenuAction {
        index: usize,
    },
    BuiltinJsonSchema {
        schema_path: String,
    },
    Setting {
        /// `None` opens settings without navigating to a specific path.
        setting_path: Option<String>,
    },
    GitClone {
        repo_url: SharedString,
    },
    GitCommit {
        sha: String,
    },
}

impl std::fmt::Debug for OpenRequestKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CliConnection(_) => write!(f, "CliConnection(..)"),
            Self::FocusApp => write!(f, "FocusApp"),
            Self::Extension { extension_id } => f
                .debug_struct("Extension")
                .field("extension_id", extension_id)
                .finish(),
            Self::AgentPanel {
                external_source_prompt,
            } => f
                .debug_struct("AgentPanel")
                .field("external_source_prompt", external_source_prompt)
                .finish(),
            Self::InstallSkill { content } => f
                .debug_struct("InstallSkill")
                .field("content_len", &content.len())
                .finish(),
            Self::DockMenuAction { index } => f
                .debug_struct("DockMenuAction")
                .field("index", index)
                .finish(),
            Self::BuiltinJsonSchema { schema_path } => f
                .debug_struct("BuiltinJsonSchema")
                .field("schema_path", schema_path)
                .finish(),
            Self::Setting { setting_path } => f
                .debug_struct("Setting")
                .field("setting_path", setting_path)
                .finish(),
            Self::GitClone { repo_url } => f
                .debug_struct("GitClone")
                .field("repo_url", repo_url)
                .finish(),
            Self::GitCommit { sha } => f.debug_struct("GitCommit").field("sha", sha).finish(),
        }
    }
}

impl OpenRequest {
    pub fn is_focus_app_only(&self) -> bool {
        matches!(self.kind, Some(OpenRequestKind::FocusApp))
            && self.open_paths.is_empty()
            && self.diff_paths.is_empty()
            && self.remote_connection.is_none()
            && self.join_channel.is_none()
            && self.open_channel_notes.is_empty()
    }

    pub fn parse(request: RawOpenRequest, cx: &App) -> Result<Self> {
        let mut this = Self::default();

        this.diff_paths = request.diff_paths;
        this.diff_all = request.diff_all;
        this.dev_container = request.dev_container;
        this.open_behavior = request.open_behavior;
        if let Some(wsl) = request.wsl {
            let (user, distro_name) = if let Some((user, distro)) = wsl.split_once('@') {
                if user.is_empty() {
                    anyhow::bail!("user is empty in wsl argument");
                }
                (Some(user.to_string()), distro.to_string())
            } else {
                (None, wsl)
            };
            this.remote_connection = Some(RemoteConnectionOptions::Wsl(WslConnectionOptions {
                distro_name,
                user,
            }));
        }

        for url in request.urls {
            if let Some(server_name) = url.strip_prefix("zed-cli://") {
                this.kind = Some(OpenRequestKind::CliConnection(connect_to_cli(server_name)?));
            } else if let Some(action_index) = url.strip_prefix("zed-dock-action://") {
                this.kind = Some(OpenRequestKind::DockMenuAction {
                    index: action_index.parse()?,
                });
            } else if let Some(file) = url.strip_prefix("file://") {
                this.parse_file_path(file)
            } else if let Some(file) = url.strip_prefix("zed://file") {
                this.parse_file_path(file)
            } else if let Some(file) = url.strip_prefix("zed://ssh") {
                let ssh_url = "ssh:/".to_string() + file;
                this.parse_ssh_file_path(&ssh_url, cx)?
            } else if let Some(extension_id) = url.strip_prefix("zed://extension/") {
                this.kind = Some(OpenRequestKind::Extension {
                    extension_id: extension_id.to_string(),
                });
            } else if url.starts_with(agent_skills::SKILL_SHARE_LINK_PREFIX) {
                this.parse_skill_install_url(&url)?
            } else if let Some(agent_path) = url.strip_prefix("zed://agent") {
                this.parse_agent_url(agent_path)
            } else if url == "zed://" || url == "zed://open" || url == "zed://open/" {
                this.kind = Some(OpenRequestKind::FocusApp);
            } else if let Some(schema_path) = url.strip_prefix("zed://schemas/") {
                this.kind = Some(OpenRequestKind::BuiltinJsonSchema {
                    schema_path: schema_path.to_string(),
                });
            } else if url == "zed://settings" || url == "zed://settings/" {
                this.kind = Some(OpenRequestKind::Setting { setting_path: None });
            } else if let Some(setting_path) = url.strip_prefix("zed://settings/") {
                this.kind = Some(OpenRequestKind::Setting {
                    setting_path: Some(setting_path.to_string()),
                });
            } else if let Some(clone_path) = url.strip_prefix("zed://git/clone") {
                this.parse_git_clone_url(clone_path)?
            } else if let Some(commit_path) = url.strip_prefix("zed://git/commit/") {
                this.parse_git_commit_url(commit_path)?
            } else if url.starts_with("ssh://") {
                this.parse_ssh_file_path(&url, cx)?
            } else if let Some(zed_link) = parse_zed_link(&url, cx) {
                match zed_link {
                    ZedLink::Channel { channel_id } => {
                        this.join_channel = Some(channel_id);
                    }
                    ZedLink::ChannelNotes {
                        channel_id,
                        heading,
                    } => {
                        this.open_channel_notes.push((channel_id, heading));
                    }
                }
            } else {
                log::error!("unhandled url: {}", url);
            }
        }

        Ok(this)
    }

    fn parse_file_path(&mut self, file: &str) {
        if let Some(decoded) = urlencoding::decode(file).log_err() {
            self.open_paths.push(decoded.into_owned())
        }
    }

    fn parse_agent_url(&mut self, agent_path: &str) {
        // Format: "" or "?prompt=<text>".
        let agent_path = agent_path.strip_prefix('/').unwrap_or(agent_path);
        let external_source_prompt = agent_path.strip_prefix('?').and_then(|query| {
            url::form_urlencoded::parse(query.as_bytes())
                .find_map(|(key, value)| (key == "prompt").then_some(value))
                .and_then(|prompt| ExternalSourcePrompt::new(prompt.as_ref()))
        });
        self.kind = Some(OpenRequestKind::AgentPanel {
            external_source_prompt,
        });
    }

    fn parse_skill_install_url(&mut self, url: &str) -> Result<()> {
        // Format: zed://skill?data=<base64url of SKILL.md contents>
        let content = agent_skills::decode_skill_share_link(url)?;
        self.kind = Some(OpenRequestKind::InstallSkill { content });
        Ok(())
    }

    fn parse_git_clone_url(&mut self, clone_path: &str) -> Result<()> {
        // Format: /?repo=<url> or ?repo=<url>
        let clone_path = clone_path.strip_prefix('/').unwrap_or(clone_path);

        let query = clone_path
            .strip_prefix('?')
            .context("invalid git clone url: missing query string")?;

        let repo_url = url::form_urlencoded::parse(query.as_bytes())
            .find_map(|(key, value)| (key == "repo").then_some(value))
            .filter(|s| !s.is_empty())
            .context("invalid git clone url: missing repo query parameter")?
            .to_string()
            .into();

        self.kind = Some(OpenRequestKind::GitClone { repo_url });

        Ok(())
    }

    fn parse_git_commit_url(&mut self, commit_path: &str) -> Result<()> {
        // Format: <sha>?repo=<path>
        let (sha, query) = commit_path
            .split_once('?')
            .context("invalid git commit url: missing query string")?;
        anyhow::ensure!(!sha.is_empty(), "invalid git commit url: missing sha");

        let repo = url::form_urlencoded::parse(query.as_bytes())
            .find_map(|(key, value)| (key == "repo").then_some(value))
            .filter(|s| !s.is_empty())
            .context("invalid git commit url: missing repo query parameter")?
            .to_string();

        self.open_paths.push(repo);

        self.kind = Some(OpenRequestKind::GitCommit {
            sha: sha.to_string(),
        });

        Ok(())
    }

    fn parse_ssh_file_path(&mut self, file: &str, cx: &App) -> Result<()> {
        let url = parse_ssh_url(file)?;
        let host = match url
            .host()
            .with_context(|| format!("missing host in ssh url: {url}"))?
        {
            url::Host::Domain(host) => host.to_string(),
            url::Host::Ipv4(host) => host.to_string(),
            url::Host::Ipv6(host) => host.to_string(),
        };
        let username = if url.username().is_empty() {
            None
        } else {
            Some(urlencoding::decode(url.username())?.into_owned())
        };
        let port = url.port();
        anyhow::ensure!(
            self.open_paths.is_empty(),
            "cannot open both local and ssh paths"
        );
        let mut connection_options =
            RemoteSettings::get_global(cx).connection_options_for(host, port, username);
        if let Some(password) = url.password() {
            connection_options.password = Some(urlencoding::decode(password)?.into_owned());
        }

        let connection_options = RemoteConnectionOptions::Ssh(connection_options);
        if let Some(ssh_connection) = &self.remote_connection {
            anyhow::ensure!(
                *ssh_connection == connection_options,
                "cannot open multiple different remote connections"
            );
        }
        self.remote_connection = Some(connection_options);
        self.parse_file_path(url.path());
        Ok(())
    }
}

fn parse_ssh_url(url: &str) -> Result<url::Url> {
    if let Ok(url) = url::Url::parse(url) {
        return Ok(url);
    }
    // SCP/git style urls use ':' to separate from Authority and Path.
    // They are unsupported by Url::parse, but can be normalized into a Url.
    //   SCPUrl("ssh://user@host:~/relpath") => Url("ssh://user@host/~/relpath")
    //   SCPUrl("ssh://user@host:/abs/path") => Url("ssh://user@host/abs/path")
    //
    // TODO: Add IPv6 support: "ssh://[2600::]:~/foo"
    let ssh_target = url
        .strip_prefix("ssh://")
        .with_context(|| format!("invalid ssh url: {url}"))?;

    let (authority, path) = if let Some((authority, path)) = ssh_target.rsplit_once(":~/") {
        (authority, format!("/~/{path}"))
    } else if let Some((authority, path)) = ssh_target.rsplit_once(":/") {
        (authority, format!("/{path}"))
    } else {
        anyhow::bail!("invalid ssh url: {url}");
    };

    let (userinfo, host) = authority
        .rsplit_once('@')
        .map_or((None, authority), |(userinfo, host)| (Some(userinfo), host));
    anyhow::ensure!(
        !host.is_empty() && !host.starts_with('[') && !host.contains(':'),
        "invalid ssh url: {url}"
    );

    let normalized_authority = if let Some(userinfo) = userinfo {
        let (username, colon_password) =
            if let Some((username, password)) = userinfo.split_once(':') {
                (
                    urlencoding::encode(&urlencoding::decode(username)?).into_owned(),
                    format!(
                        ":{}",
                        urlencoding::encode(&urlencoding::decode(password)?).into_owned()
                    ),
                )
            } else {
                (
                    urlencoding::encode(&urlencoding::decode(userinfo)?).into_owned(),
                    String::new(),
                )
            };
        format!("{username}{colon_password}@{host}")
    } else {
        authority.to_string()
    };

    Ok(url::Url::parse(&format!(
        "ssh://{normalized_authority}{path}"
    ))?)
}

#[derive(Clone)]
pub struct OpenListener(UnboundedSender<RawOpenRequest>);

#[derive(Default)]
pub struct RawOpenRequest {
    pub urls: Vec<String>,
    pub diff_paths: Vec<[String; 2]>,
    pub diff_all: bool,
    pub dev_container: bool,
    pub wsl: Option<String>,
    pub open_behavior: Option<cli::OpenBehavior>,
}

impl Global for OpenListener {}

impl OpenListener {
    pub fn new() -> (Self, UnboundedReceiver<RawOpenRequest>) {
        let (tx, rx) = mpsc::unbounded();
        (OpenListener(tx), rx)
    }

    pub fn open(&self, request: RawOpenRequest) {
        self.0
            .unbounded_send(request)
            .context("no listener for open requests")
            .log_err();
    }
}

#[cfg(any(target_os = "linux", target_os = "freebsd"))]
pub fn listen_for_cli_connections(opener: OpenListener) -> Result<()> {
    use release_channel::RELEASE_CHANNEL_NAME;
    use std::os::unix::net::UnixDatagram;

    let sock_path = paths::data_dir().join(format!("zed-{}.sock", *RELEASE_CHANNEL_NAME));
    // remove the socket if the process listening on it has died
    if let Err(e) = UnixDatagram::unbound()?.connect(&sock_path)
        && e.kind() == std::io::ErrorKind::ConnectionRefused
    {
        std::fs::remove_file(&sock_path)?;
    }
    let listener = UnixDatagram::bind(&sock_path)?;
    thread::spawn(move || {
        let mut buf = [0u8; 1024];
        while let Ok(len) = listener.recv(&mut buf) {
            opener.open(RawOpenRequest {
                urls: vec![String::from_utf8_lossy(&buf[..len]).to_string()],
                ..Default::default()
            });
        }
    });
    Ok(())
}

fn connect_to_cli(
    server_name: &str,
) -> Result<(
    mpsc::UnboundedReceiver<CliRequest>,
    Box<dyn CliResponseSink>,
)> {
    let handshake_tx = ipc::IpcSender::<IpcHandshake>::connect(server_name.to_string())
        .context("error connecting to cli")?;
    let (request_tx, request_rx) = ipc::channel::<CliRequest>()?;
    let (response_tx, response_rx) = ipc::channel::<CliResponse>()?;

    handshake_tx
        .send(IpcHandshake {
            requests: request_tx,
            responses: response_rx,
        })
        .context("error sending ipc handshake")?;

    let (async_request_tx, async_request_rx) = futures::channel::mpsc::unbounded::<CliRequest>();
    thread::spawn(move || {
        while let Ok(cli_request) = request_rx.recv() {
            if async_request_tx.unbounded_send(cli_request).is_err() {
                break;
            }
        }
        anyhow::Ok(())
    });

    Ok((async_request_rx, Box::new(response_tx)))
}

pub async fn open_paths_with_positions(
    path_positions: &[PathWithPosition],
    diff_paths: &[[String; 2]],
    diff_all: bool,
    app_state: Arc<AppState>,
    open_options: workspace::OpenOptions,
    cx: &mut AsyncApp,
) -> Result<(
    WindowHandle<MultiWorkspace>,
    Vec<Option<Result<Box<dyn ItemHandle>>>>,
)> {
    let paths = path_positions
        .iter()
        .map(|path_with_position| path_with_position.path.clone())
        .collect::<Vec<_>>();

    let OpenResult {
        window: multi_workspace,
        opened_items: mut items,
        ..
    } = cx
        .update(|cx| workspace::open_paths(&paths, app_state.clone(), open_options, cx))
        .await?;

    if diff_all && !diff_paths.is_empty() {
        let mut diff_pairs = Vec::with_capacity(diff_paths.len());
        for diff_pair in diff_paths {
            let parsed = derive_paths_with_position(app_state.fs.as_ref(), diff_pair).await;
            let (Some(old_parsed), Some(new_parsed)) = (parsed.first(), parsed.get(1)) else {
                continue;
            };
            diff_pairs.push([
                old_parsed.path.to_string_lossy().into_owned(),
                new_parsed.path.to_string_lossy().into_owned(),
            ]);
        }
        if let Ok(diff_view) = multi_workspace.update(cx, |multi_workspace, window, cx| {
            multi_workspace.workspace().update(cx, |workspace, cx| {
                MultiDiffView::open(diff_pairs, workspace, window, cx)
            })
        }) {
            if let Some(diff_view) = diff_view.await.log_err() {
                items.push(Some(Ok(Box::new(diff_view))));
            }
        }
    } else {
        let workspace_weak = multi_workspace.read_with(cx, |multi_workspace, _cx| {
            multi_workspace.workspace().downgrade()
        })?;
        let canonicalize = async |parsed: &PathWithPosition| {
            app_state
                .fs
                .canonicalize(&parsed.path)
                .await
                .with_context(|| format!("opening --diff path {:?}", parsed.path))
        };
        for diff_pair in diff_paths {
            let parsed = derive_paths_with_position(app_state.fs.as_ref(), diff_pair).await;
            let (Some(old_parsed), Some(new_parsed)) = (parsed.first(), parsed.get(1)) else {
                continue;
            };
            let (old_path, new_path) =
                match futures::join!(canonicalize(old_parsed), canonicalize(new_parsed)) {
                    (Ok(old), Ok(new)) => (old, new),
                    (old, new) => {
                        for result in [old, new] {
                            if let Err(err) = result {
                                items.push(Some(Err(err)));
                            }
                        }
                        continue;
                    }
                };
            let target_position = new_parsed.row.map(|row| {
                language::Point::new(
                    row.saturating_sub(1),
                    new_parsed.column.unwrap_or(0).saturating_sub(1),
                )
            });
            if let Ok(diff_view) = multi_workspace.update(cx, |_multi_workspace, window, cx| {
                FileDiffView::open(
                    old_path,
                    new_path,
                    target_position,
                    workspace_weak.clone(),
                    window,
                    cx,
                )
            }) {
                if let Some(diff_view) = diff_view.await.log_err() {
                    items.push(Some(Ok(Box::new(diff_view))))
                }
            }
        }
    }

    for (item, path) in items.iter_mut().zip(&paths) {
        if let Some(Err(error)) = item {
            *error = anyhow!("error opening {path:?}: {error:#}");
        }
    }

    let items_for_navigation = items
        .iter()
        .map(|item| item.as_ref().and_then(|r| r.as_ref().ok()).cloned())
        .collect::<Vec<_>>();
    navigate_to_positions(&multi_workspace, items_for_navigation, path_positions, cx);

    Ok((multi_workspace, items))
}

pub async fn handle_cli_connection(
    (mut requests, responses): (
        mpsc::UnboundedReceiver<CliRequest>,
        Box<dyn CliResponseSink>,
    ),
    app_state: Arc<AppState>,
    cx: &mut AsyncApp,
) {
    if let Some(request) = requests.next().await {
        match request {
            CliRequest::Open {
                urls,
                paths,
                diff_paths,
                diff_all,
                wait,
                wsl,
                mut open_behavior,
                env,
                user_data_dir: _,
                dev_container,
                cwd,
            } => {
                if !urls.is_empty() {
                    cx.update(|cx| {
                        match OpenRequest::parse(
                            RawOpenRequest {
                                urls,
                                diff_paths,
                                diff_all,
                                dev_container,
                                wsl,
                                open_behavior: Some(open_behavior),
                            },
                            cx,
                        ) {
                            Ok(open_request) => {
                                cx.activate(true);
                                handle_open_request(open_request, app_state.clone(), cx);
                                responses.send(CliResponse::Exit { status: 0 }).log_err();
                            }
                            Err(e) => {
                                responses
                                    .send(CliResponse::Stderr {
                                        message: format!("{e}"),
                                    })
                                    .log_err();
                                responses.send(CliResponse::Exit { status: 1 }).log_err();
                            }
                        };
                    });
                    return;
                }

                if open_behavior == cli::OpenBehavior::Default {
                    match resolve_open_behavior(
                        &paths,
                        &app_state,
                        responses.as_ref(),
                        &mut requests,
                        cx,
                    )
                    .await
                    {
                        Some(settings::CliDefaultOpenBehavior::ExistingWindow) => {
                            open_behavior = cli::OpenBehavior::ExistingWindow;
                        }
                        Some(settings::CliDefaultOpenBehavior::NewWindow) => {
                            open_behavior = cli::OpenBehavior::PreferNewWindow;
                        }
                        None => {}
                    }
                }

                if open_behavior == cli::OpenBehavior::Default {
                    open_behavior = cx.update(|cx| open_behavior_for_default_setting(cx));
                }

                cx.update(|cx| cx.activate(true));

                let open_workspace_result = open_workspaces(
                    paths,
                    diff_paths,
                    diff_all,
                    open_behavior,
                    responses.as_ref(),
                    wait,
                    dev_container,
                    app_state.clone(),
                    env,
                    cwd,
                    cx,
                )
                .await;

                let status = if open_workspace_result.is_err() { 1 } else { 0 };
                responses.send(CliResponse::Exit { status }).log_err();
            }
            CliRequest::SetOpenBehavior { .. } => {
                // We handle this case in a situation-specific way in
                // resolve_open_behavior
                debug_panic!("unexpected SetOpenBehavior message");
            }
            request => {
                let result = handle_terminal_cli_request(request, cx).await;
                match result {
                    Ok(output) => {
                        responses
                            .send(CliResponse::Stdout { message: output })
                            .log_err();
                        responses.send(CliResponse::Exit { status: 0 }).log_err();
                    }
                    Err(error) => {
                        responses
                            .send(CliResponse::Stderr {
                                message: format!("{error:#}"),
                            })
                            .log_err();
                        responses.send(CliResponse::Exit { status: 1 }).log_err();
                    }
                }
            }
        }
    }
}

fn workspace_cli_id(workspace: &gpui::Entity<workspace::Workspace>) -> String {
    format!("workspace-{}", workspace.entity_id())
}

fn terminal_cli_id(terminal_id: TerminalId) -> String {
    format!("terminal-{terminal_id}")
}

fn worktree_cli_id(id: impl std::fmt::Display) -> String {
    format!("worktree-{id}")
}

#[derive(Clone)]
struct CliWorktree {
    id: String,
    name: String,
    path: String,
}

#[derive(Clone)]
struct CliRepositoryMetadata {
    main_worktree_path: Option<String>,
    branch: Option<String>,
    default_branch: Option<String>,
    default_branch_error: Option<String>,
    is_main_worktree: bool,
}

fn visible_cli_worktree(
    worktree: &gpui::Entity<project::Worktree>,
    cx: &App,
) -> Option<CliWorktree> {
    let worktree = worktree.read(cx);
    worktree.is_visible().then(|| CliWorktree {
        id: worktree_cli_id(worktree.id()),
        name: worktree.root_name_str().to_string(),
        path: worktree.abs_path().to_string_lossy().into_owned(),
    })
}

fn cli_workspace_name(workspace: &gpui::Entity<workspace::Workspace>, cx: &App) -> Option<String> {
    let workspace = workspace.read(cx);
    let worktree_paths = workspace.project().read(cx).worktree_paths(cx);
    let mut names = worktree_paths
        .ordered_pairs()
        .filter_map(|(main_path, folder_path)| {
            if main_path == folder_path {
                Some("main".to_string())
            } else {
                project::linked_worktree_short_name(main_path, folder_path)
                    .map(|name| name.to_string())
            }
        });
    let first = names.next()?;
    names.all(|name| name == first).then_some(first)
}

fn all_cli_project_groups(
    cx: &App,
) -> Vec<(
    workspace::ProjectGroupKey,
    Vec<(gpui::Entity<workspace::Workspace>, bool)>,
)> {
    cx.windows()
        .into_iter()
        .filter_map(|window| window.downcast::<MultiWorkspace>())
        .flat_map(|window| {
            window
                .read(cx)
                .map(|multi_workspace| {
                    let active_workspace = multi_workspace.workspace();
                    let mut groups: Vec<(
                        workspace::ProjectGroupKey,
                        Vec<(gpui::Entity<workspace::Workspace>, bool)>,
                    )> = Vec::new();
                    for workspace in multi_workspace.workspaces() {
                        let key = multi_workspace.project_group_key_for_workspace(workspace, cx);
                        let active = workspace == active_workspace;
                        if let Some((_, workspaces)) =
                            groups.iter_mut().find(|(group_key, _)| *group_key == key)
                        {
                            workspaces.push((workspace.clone(), active));
                        } else {
                            groups.push((key, vec![(workspace.clone(), active)]));
                        }
                    }
                    groups
                })
                .unwrap_or_default()
        })
        .collect()
}

async fn cli_repository_metadata(
    workspaces: &[gpui::Entity<workspace::Workspace>],
    cx: &mut AsyncApp,
) -> Result<std::collections::HashMap<(String, String), CliRepositoryMetadata>> {
    let requests = cx.update(|cx| {
        let mut requests = Vec::new();
        for workspace in workspaces {
            let workspace_id = workspace_cli_id(workspace);
            let repositories = workspace
                .read(cx)
                .project()
                .read(cx)
                .repositories(cx)
                .values()
                .cloned()
                .collect::<Vec<_>>();
            for repository in repositories {
                let snapshot = repository.read(cx).snapshot();
                let path = snapshot
                    .work_directory_abs_path
                    .to_string_lossy()
                    .into_owned();
                let metadata = CliRepositoryMetadata {
                    main_worktree_path: snapshot
                        .main_worktree_abs_path()
                        .map(|path| path.to_string_lossy().into_owned()),
                    branch: snapshot
                        .branch
                        .as_ref()
                        .map(|branch| branch.name().to_string()),
                    default_branch: None,
                    default_branch_error: None,
                    is_main_worktree: snapshot.is_main_worktree(),
                };
                let receiver =
                    repository.update(cx, |repository, _cx| repository.default_branch(false));
                requests.push(((workspace_id.clone(), path), metadata, receiver));
            }
        }
        requests
    });

    let metadata = future::join_all(requests.into_iter().map(
        |(key, mut metadata, receiver)| async move {
            match receiver.await {
                Ok(Ok(default_branch)) => {
                    metadata.default_branch = default_branch.map(|branch| branch.to_string());
                }
                Ok(Err(error)) => metadata.default_branch_error = Some(format!("{error:#}")),
                Err(error) => metadata.default_branch_error = Some(error.to_string()),
            }
            (key, metadata)
        },
    ))
    .await;
    Ok(metadata.into_iter().collect())
}

fn cli_repository_for_worktree<'a>(
    repositories: &'a std::collections::HashMap<(String, String), CliRepositoryMetadata>,
    workspace_id: &str,
    worktree_path: &Path,
) -> Option<&'a CliRepositoryMetadata> {
    repositories
        .iter()
        .filter_map(|((repository_workspace_id, repository_path), metadata)| {
            let repository_path = Path::new(repository_path);
            (repository_workspace_id == workspace_id && worktree_path.starts_with(repository_path))
                .then_some((metadata, repository_path.components().count()))
        })
        .max_by_key(|(_, depth)| *depth)
        .map(|(metadata, _)| metadata)
}

fn terminal_worktree(
    workspace: &gpui::Entity<workspace::Workspace>,
    terminal: &terminal::Terminal,
    cx: &App,
) -> Option<CliWorktree> {
    let cwd = terminal.working_directory()?;
    let project = workspace.read(cx).project().read(cx);
    let (worktree, _) = project.find_worktree(&cwd, cx)?;
    let worktree = worktree.read(cx);
    worktree.is_visible().then(|| CliWorktree {
        id: worktree_cli_id(worktree.id()),
        name: worktree.root_name_str().to_string(),
        path: worktree.abs_path().to_string_lossy().into_owned(),
    })
}

fn terminal_task_status(terminal: &terminal::Terminal) -> &'static str {
    match terminal.task().map(|task| task.status) {
        Some(terminal::TaskStatus::Running) => "running",
        Some(terminal::TaskStatus::Completed { success: true }) => "succeeded",
        Some(terminal::TaskStatus::Completed { success: false }) => "failed",
        Some(terminal::TaskStatus::Unknown) => "unknown",
        None => "interactive",
    }
}

fn terminal_views_in_workspace(
    workspace: &gpui::Entity<workspace::Workspace>,
    cx: &App,
) -> Vec<(TerminalId, gpui::Entity<TerminalView>)> {
    let workspace_ref = workspace.read(cx);
    let Some(panel) = workspace_ref.panel::<AgentPanel>(cx) else {
        return Vec::new();
    };
    let mut views = panel.read(cx).terminal_views();
    views.sort_by_key(|(terminal_id, _)| terminal_id.to_string());
    views
}

struct CliTerminalEntry {
    workspace: gpui::Entity<workspace::Workspace>,
    terminal_id: TerminalId,
    view: Option<gpui::Entity<TerminalView>>,
    metadata: Option<TerminalThreadMetadata>,
}

fn all_cli_terminals(cx: &App) -> Vec<CliTerminalEntry> {
    let mut terminals = Vec::new();
    let mut seen_terminal_ids = std::collections::HashSet::new();

    for (_, workspace, _) in all_cli_workspaces(cx) {
        for (terminal_id, view) in terminal_views_in_workspace(&workspace, cx) {
            if seen_terminal_ids.insert(terminal_id) {
                terminals.push(CliTerminalEntry {
                    workspace: workspace.clone(),
                    terminal_id,
                    view: Some(view),
                    metadata: None,
                });
            }
        }
    }

    let Some(metadata_store) = TerminalThreadMetadataStore::try_global(cx) else {
        return terminals;
    };
    let metadata_store = metadata_store.read(cx);
    for window in cx
        .windows()
        .into_iter()
        .filter_map(|window| window.downcast::<MultiWorkspace>())
    {
        let Ok(project_groups) =
            window.read_with(cx, |multi_workspace, cx| multi_workspace.project_groups(cx))
        else {
            continue;
        };
        for project_group in project_groups {
            let Some(workspace) = project_group.workspaces.first().cloned() else {
                continue;
            };
            let remote_connection = project_group.key.host();
            for metadata in metadata_store.entries_for_main_worktree_path(
                project_group.key.path_list(),
                remote_connection.as_ref(),
            ) {
                if seen_terminal_ids.insert(metadata.terminal_id) {
                    terminals.push(CliTerminalEntry {
                        workspace: workspace.clone(),
                        terminal_id: metadata.terminal_id,
                        view: None,
                        metadata: Some(metadata.clone()),
                    });
                }
            }
        }
    }

    terminals.sort_by_key(|terminal| terminal.terminal_id.to_string());
    terminals
}

fn metadata_worktree(metadata: &TerminalThreadMetadata) -> Option<(String, String)> {
    let [path] = metadata.folder_paths().paths() else {
        return None;
    };
    let name = path
        .file_name()
        .map(|name| name.to_string_lossy().into_owned())
        .unwrap_or_else(|| path.to_string_lossy().into_owned());
    Some((name, path.to_string_lossy().into_owned()))
}

fn all_cli_workspaces(
    cx: &App,
) -> Vec<(
    WindowHandle<MultiWorkspace>,
    gpui::Entity<workspace::Workspace>,
    bool,
)> {
    cx.windows()
        .into_iter()
        .filter_map(|window| window.downcast::<MultiWorkspace>())
        .flat_map(|window| {
            window
                .read(cx)
                .map(|multi_workspace| {
                    multi_workspace
                        .workspaces()
                        .map(|workspace| {
                            (
                                window,
                                workspace.clone(),
                                workspace == multi_workspace.workspace(),
                            )
                        })
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default()
        })
        .collect()
}

async fn ensure_agent_panel_in_workspace(
    window: WindowHandle<MultiWorkspace>,
    workspace: &gpui::Entity<workspace::Workspace>,
    cx: &mut AsyncApp,
) -> Result<gpui::Entity<AgentPanel>> {
    if let Some(panel) = cx.update(|cx| workspace.read(cx).panel::<AgentPanel>(cx)) {
        return Ok(panel);
    }

    let async_window_context =
        window.update(cx, |_multi_workspace, window, cx| window.to_async(cx))?;
    let loaded_panel = AgentPanel::load(workspace.downgrade(), async_window_context).await?;
    window.update(cx, |_multi_workspace, window, cx| {
        workspace.update(cx, |workspace, cx| {
            if let Some(panel) = workspace.panel::<AgentPanel>(cx) {
                panel
            } else {
                workspace.add_panel(loaded_panel.clone(), window, cx);
                loaded_panel
            }
        })
    })
}

fn find_cli_terminal(
    terminal_id: &str,
    cx: &App,
) -> Option<(
    gpui::Entity<workspace::Workspace>,
    gpui::Entity<terminal::Terminal>,
)> {
    all_cli_terminals(cx)
        .into_iter()
        .filter_map(|entry| Some((entry.workspace, entry.terminal_id, entry.view?)))
        .find(|(_, agent_terminal_id, _)| terminal_cli_id(*agent_terminal_id) == terminal_id)
        .map(|(workspace, _, view)| (workspace, view.read(cx).terminal().clone()))
}

async fn handle_terminal_cli_request(request: CliRequest, cx: &mut AsyncApp) -> Result<String> {
    match request {
        CliRequest::ListWorkspaces => {
            let groups = cx.update(|cx| all_cli_project_groups(cx));
            let workspaces = groups
                .iter()
                .flat_map(|(_, workspaces)| workspaces.iter().map(|(workspace, _)| workspace.clone()))
                .collect::<Vec<_>>();
            let repository_metadata = cli_repository_metadata(&workspaces, cx).await?;

            cx.update(|cx| {
                let mut main_worktree_paths: Vec<PathBuf> = groups
                    .iter()
                    .flat_map(|(key, _)| key.path_list().paths().iter().cloned())
                    .collect::<Vec<_>>();
                main_worktree_paths.sort_unstable();
                main_worktree_paths.dedup();
                let path_details = util::disambiguate::compute_disambiguation_details(
                    &main_worktree_paths,
                    |path, detail| project::path_suffix(path, detail),
                );
                let path_detail_map = main_worktree_paths
                    .into_iter()
                    .zip(path_details)
                    .collect::<std::collections::HashMap<_, _>>();

                let projects = groups
                    .into_iter()
                    .map(|(key, workspaces)| {
                        let project_name = key.display_name(&path_detail_map);
                        let main_worktree_paths = key
                            .path_list()
                            .ordered_paths()
                            .map(|path| path.to_string_lossy().into_owned())
                            .collect::<Vec<_>>();
                        let active = workspaces.iter().any(|(_, active)| *active);
                        let remote = key.host().is_some();
                        let workspaces = workspaces
                            .into_iter()
                            .map(|(workspace, active)| {
                                let workspace_id = workspace_cli_id(&workspace);
                                let workspace_ref = workspace.read(cx);
                                let project = workspace_ref.project().read(cx);
                                let worktree_paths = project.worktree_paths(cx);
                                let worktrees = project
                                    .visible_worktrees(cx)
                                    .map(|worktree| {
                                        let worktree = worktree.read(cx);
                                        let worktree_path = worktree.abs_path();
                                        let hierarchy = worktree_paths
                                            .ordered_pairs()
                                            .find(|(_, folder_path)| {
                                                folder_path.as_path() == worktree_path.as_ref()
                                            });
                                        let hierarchy_main_worktree_path = hierarchy.map(
                                            |(main_path, _)| {
                                                main_path.to_string_lossy().into_owned()
                                            },
                                        );
                                        let hierarchy_is_main_worktree = hierarchy.map(
                                            |(main_path, folder_path)| main_path == folder_path,
                                        );
                                        let repository = cli_repository_for_worktree(
                                            &repository_metadata,
                                            &workspace_id,
                                            worktree_path.as_ref(),
                                        );
                                        let path = worktree_path.to_string_lossy().into_owned();
                                        serde_json::json!({
                                            "id": worktree_cli_id(worktree.id()),
                                            "name": worktree.root_name_str(),
                                            "path": path,
                                            "branch": repository.and_then(|repository| repository.branch.as_ref()),
                                            "default_branch": repository.and_then(|repository| repository.default_branch.as_ref()),
                                            "default_branch_error": repository.and_then(|repository| repository.default_branch_error.as_ref()),
                                            "is_main_worktree": repository.map(|repository| repository.is_main_worktree).or(hierarchy_is_main_worktree),
                                            "main_worktree_path": repository.and_then(|repository| repository.main_worktree_path.as_ref()).or(hierarchy_main_worktree_path.as_ref()),
                                        })
                                    })
                                    .collect::<Vec<_>>();
                                let roots = worktrees
                                    .iter()
                                    .filter_map(|worktree| {
                                        worktree["path"].as_str().map(ToOwned::to_owned)
                                    })
                                    .collect::<Vec<_>>();
                                serde_json::json!({
                                    "id": workspace_id,
                                    "name": cli_workspace_name(&workspace, cx),
                                    "active": active,
                                    "remote": project.is_remote(),
                                    "roots": roots,
                                    "worktrees": worktrees,
                                })
                            })
                            .collect::<Vec<_>>();
                        serde_json::json!({
                            "name": project_name,
                            "active": active,
                            "remote": remote,
                            "main_worktree_paths": main_worktree_paths,
                            "workspaces": workspaces,
                        })
                    })
                    .collect::<Vec<_>>();
                serde_json::to_string_pretty(&projects).map_err(Into::into)
            })
        }
        CliRequest::CreateWorktree {
            workspace_id,
            base_ref,
            branch,
            worktree_name,
        } => {
            let (window, creation_task) = cx.update(|cx| -> Result<_> {
                let (window, workspace, _) = all_cli_workspaces(cx)
                    .into_iter()
                    .find(|(_, workspace, _)| workspace_cli_id(workspace) == workspace_id)
                    .ok_or_else(|| anyhow!("workspace {workspace_id:?} is not open"))?;
                let branch_target = match (base_ref.clone(), branch.clone()) {
                    (Some(base_ref), Some(name)) => {
                        NewWorktreeBranchTarget::NewBranchFromRef { name, base_ref }
                    }
                    (None, Some(name)) => NewWorktreeBranchTarget::NewBranch {
                        name,
                        remote_name: None,
                        remote_branch_name: None,
                    },
                    (Some(name), None) => NewWorktreeBranchTarget::ExistingBranch { name },
                    (None, None) => NewWorktreeBranchTarget::CurrentBranch,
                };
                let action = CreateWorktree {
                    worktree_name,
                    branch_target,
                };
                let creation_task = window.update(cx, |_multi_workspace, window, cx| {
                    workspace.update(cx, |workspace, cx| {
                        git_ui::worktree_service::create_worktree_workspace(
                            workspace, &action, window, None, cx,
                        )
                    })
                })?;
                Ok((window, creation_task))
            })?;

            let created = creation_task
                .await
                .context("failed to create worktree workspace")?;
            ensure_agent_panel_in_workspace(window, &created.workspace, cx).await?;
            cx.update(|cx| {
                let project = created.workspace.read(cx).project().read(cx);
                let mut worktrees = project
                    .visible_worktrees(cx)
                    .filter_map(|worktree| visible_cli_worktree(&worktree, cx))
                    .collect::<Vec<_>>();
                worktrees.sort_by(|left, right| left.path.cmp(&right.path));
                anyhow::ensure!(
                    !worktrees.is_empty(),
                    "created workspace has no visible worktrees"
                );
                let only_worktree = match worktrees.as_slice() {
                    [worktree] => Some(worktree),
                    _ => None,
                };
                serde_json::to_string_pretty(&serde_json::json!({
                    "source_workspace_id": workspace_id,
                    "workspace_id": workspace_cli_id(&created.workspace),
                    "workspace_name": cli_workspace_name(&created.workspace, cx),
                    "worktree_id": only_worktree.map(|worktree| &worktree.id),
                    "worktree_name": only_worktree.map(|worktree| &worktree.name),
                    "worktree_path": only_worktree.map(|worktree| &worktree.path),
                    "worktrees": worktrees.iter().map(|worktree| serde_json::json!({
                        "id": worktree.id,
                        "name": worktree.name,
                        "path": worktree.path,
                    })).collect::<Vec<_>>(),
                    "base_ref": base_ref,
                    "branch": branch,
                    "detached": branch.is_none(),
                    "consolidated_worktrees": created.consolidated_worktrees,
                }))
                .map_err(Into::into)
            })
        }
        CliRequest::ListTerminals {
            workspace_id,
            worktree_id,
        } => cx.update(|cx| {
            let workspaces = all_cli_workspaces(cx);
            if let Some(workspace_id) = workspace_id.as_ref() {
                anyhow::ensure!(
                    workspaces
                        .iter()
                        .any(|(_, workspace, _)| workspace_cli_id(workspace) == *workspace_id),
                    "workspace {workspace_id:?} is not open"
                );
            }
            if let Some(worktree_id) = worktree_id.as_ref() {
                let workspace_id = workspace_id
                    .as_ref()
                    .ok_or_else(|| anyhow!("--worktree requires --workspace"))?;
                let workspace = workspaces
                    .iter()
                    .find(|(_, workspace, _)| workspace_cli_id(workspace) == *workspace_id)
                    .map(|(_, workspace, _)| workspace)
                    .ok_or_else(|| anyhow!("workspace {workspace_id:?} is not open"))?;
                let project = workspace.read(cx).project().read(cx);
                anyhow::ensure!(
                    project
                        .visible_worktrees(cx)
                        .any(|worktree| worktree_cli_id(worktree.read(cx).id()) == *worktree_id),
                    "worktree {worktree_id:?} is not open in workspace {workspace_id:?}"
                );
            }
            let terminals = all_cli_terminals(cx)
                .into_iter()
                .filter(|entry| {
                    workspace_id.as_ref().is_none_or(|id| {
                        workspace_cli_id(&entry.workspace) == *id
                    })
                })
                .filter_map(|entry| {
                    let terminal_id = terminal_cli_id(entry.terminal_id);
                    if let Some(view) = entry.view {
                        let terminal = view.read(cx).terminal().clone();
                        return terminal.update(cx, |terminal, cx| {
                            terminal.refresh_content_snapshot();
                            let worktree = terminal_worktree(&entry.workspace, terminal, cx);
                            if worktree_id.as_ref().is_some_and(|worktree_id| {
                                worktree.as_ref().is_none_or(|worktree| worktree.id != *worktree_id)
                            }) {
                                return None;
                            }
                            Some(serde_json::json!({
                                "id": terminal_id,
                                "workspace_id": workspace_cli_id(&entry.workspace),
                                "location": "agent",
                                "loaded": true,
                                "title": terminal.title(false),
                                "cwd": terminal.working_directory(),
                                "pid": terminal.pid().map(|pid| pid.as_u32()),
                                "status": terminal_task_status(terminal),
                                "buffer": if terminal.last_content.mode.contains(Modes::ALT_SCREEN) {
                                    "alternate"
                                } else {
                                    "primary"
                                },
                                "vi_mode": terminal.vi_mode_enabled(),
                                "worktree_id": worktree.as_ref().map(|worktree| &worktree.id),
                                "worktree_name": worktree.as_ref().map(|worktree| &worktree.name),
                                "worktree_path": worktree.as_ref().map(|worktree| &worktree.path),
                            }))
                        });
                    }

                    let metadata = entry.metadata?;
                    if worktree_id.is_some() {
                        return None;
                    }
                    let worktree = metadata_worktree(&metadata);
                    Some(serde_json::json!({
                        "id": terminal_id,
                        "workspace_id": workspace_cli_id(&entry.workspace),
                        "location": "agent",
                        "loaded": false,
                        "title": metadata.display_title(),
                        "cwd": metadata.working_directory,
                        "pid": null,
                        "status": "unloaded",
                        "buffer": null,
                        "vi_mode": false,
                        "worktree_id": null,
                        "worktree_name": worktree.as_ref().map(|(name, _)| name),
                        "worktree_path": worktree.as_ref().map(|(_, path)| path),
                    }))
                })
                .collect::<Vec<_>>();
            serde_json::to_string_pretty(&terminals).map_err(Into::into)
        }),
        CliRequest::ReadTerminal { terminal_id } => cx.update(|cx| {
            let (workspace, terminal) = find_cli_terminal(&terminal_id, cx)
                .ok_or_else(|| anyhow!("terminal {terminal_id:?} is not open"))?;
            let snapshot = terminal.update(cx, |terminal, cx| {
                let snapshot_content = terminal.get_content_snapshot();
                let content = &terminal.last_content;
                let worktree = terminal_worktree(&workspace, terminal, cx);
                serde_json::json!({
                    "id": terminal_id,
                    "workspace_id": workspace_cli_id(&workspace),
                    "title": terminal.title(false),
                    "cwd": terminal.working_directory(),
                    "pid": terminal.pid().map(|pid| pid.as_u32()),
                    "status": terminal_task_status(terminal),
                    "buffer": if content.mode.contains(Modes::ALT_SCREEN) {
                        "alternate"
                    } else {
                        "primary"
                    },
                    "vi_mode": terminal.vi_mode_enabled(),
                    "rows": content.terminal_bounds.num_lines(),
                    "columns": content.terminal_bounds.num_columns(),
                    "cursor": {
                        "row": content.cursor.point.line,
                        "column": content.cursor.point.column,
                    },
                    "content": snapshot_content,
                    "worktree_id": worktree.as_ref().map(|worktree| &worktree.id),
                    "worktree_name": worktree.as_ref().map(|worktree| &worktree.name),
                    "worktree_path": worktree.as_ref().map(|worktree| &worktree.path),
                })
            });
            serde_json::to_string_pretty(&snapshot).map_err(Into::into)
        }),
        CliRequest::WriteTerminal { terminal_id, input } => cx.update(|cx| {
            let (_, terminal) = find_cli_terminal(&terminal_id, cx)
                .ok_or_else(|| anyhow!("terminal {terminal_id:?} is not open"))?;
            terminal.update(cx, |terminal, cx| -> Result<()> {
                anyhow::ensure!(
                    !terminal.vi_mode_enabled(),
                    "terminal is in Zed vi mode; send the `i` key before writing"
                );
                terminal.input(input, cx);
                Ok(())
            })?;
            serde_json::to_string(&serde_json::json!({ "id": terminal_id, "written": true }))
                .map_err(Into::into)
        }),
        CliRequest::SendTerminalKey {
            terminal_id,
            keystroke,
        } => cx.update(|cx| {
            let (_, terminal) = find_cli_terminal(&terminal_id, cx)
                .ok_or_else(|| anyhow!("terminal {terminal_id:?} is not open"))?;
            let parsed = Keystroke::parse(&keystroke)
                .with_context(|| format!("invalid keystroke {keystroke:?}"))?;
            let handled = terminal.update(cx, |terminal, cx| -> Result<bool> {
                terminal.refresh_content_snapshot();
                if terminal.vi_mode_enabled() {
                    anyhow::ensure!(
                        parsed.key == "i" && !parsed.modifiers.modified(),
                        "terminal is in Zed vi mode; send the `i` key to return to terminal input"
                    );
                    terminal.exit_vi_mode();
                    return Ok(true);
                }
                Ok(terminal.try_keystroke(
                    &parsed,
                    terminal::terminal_settings::TerminalSettings::get_global(cx).option_as_meta,
                    cx,
                ))
            })?;
            anyhow::ensure!(
                handled,
                "keystroke {keystroke:?} is not supported by terminals"
            );
            serde_json::to_string(&serde_json::json!({ "id": terminal_id, "sent": keystroke }))
                .map_err(Into::into)
        }),
        CliRequest::SpawnTerminal {
            workspace_id,
            worktree_id,
            cwd,
            command,
        } => {
            let (terminal_task, selected_worktree) = cx.update(|cx| -> Result<_> {
                let (window, workspace, _) = all_cli_workspaces(cx)
                    .into_iter()
                    .find(|(_, workspace, _)| workspace_cli_id(workspace) == workspace_id)
                    .ok_or_else(|| anyhow!("workspace {workspace_id:?} is not open"))?;
                let project_entity = workspace.read(cx).project().clone();
                let project = project_entity.read(cx);
                let visible_worktrees = project.visible_worktrees(cx).collect::<Vec<_>>();
                let selected_worktree = if let Some(worktree_id) = worktree_id.as_ref() {
                    Some(
                        visible_worktrees
                            .iter()
                            .find(|worktree| {
                                worktree_cli_id(worktree.read(cx).id()) == *worktree_id
                            })
                            .cloned()
                            .ok_or_else(|| {
                                anyhow!(
                                    "worktree {worktree_id:?} is not open in workspace {workspace_id:?}"
                                )
                            })?,
                    )
                } else if let Some(cwd) = cwd.as_ref() {
                    if cwd.is_absolute() {
                        project
                            .find_worktree(cwd, cx)
                            .map(|(worktree, _)| worktree)
                            .filter(|worktree| worktree.read(cx).is_visible())
                    } else {
                        match visible_worktrees.as_slice() {
                            [worktree] => Some(worktree.clone()),
                            _ => None,
                        }
                    }
                } else {
                    match visible_worktrees.as_slice() {
                        [] => None,
                        [worktree] => Some(worktree.clone()),
                        _ => anyhow::bail!(
                            "workspace {workspace_id:?} has multiple worktrees; specify --worktree"
                        ),
                    }
                };

                let resolved_cwd = match (selected_worktree.as_ref(), cwd) {
                    (Some(worktree), Some(cwd)) if cwd.is_relative() => {
                        anyhow::ensure!(
                            !cwd.components().any(|component| {
                                matches!(component, std::path::Component::ParentDir)
                            }),
                            "relative cwd cannot leave the selected worktree"
                        );
                        Some(worktree.read(cx).abs_path().join(cwd))
                    }
                    (Some(worktree), Some(cwd)) => {
                        let selected_id = worktree.read(cx).id();
                        let cwd_worktree_id = project
                            .find_worktree(&cwd, cx)
                            .map(|(worktree, _)| worktree.read(cx).id());
                        anyhow::ensure!(
                            cwd_worktree_id == Some(selected_id),
                            "cwd {} is not inside worktree {:?}",
                            cwd.display(),
                            worktree_cli_id(selected_id)
                        );
                        Some(cwd)
                    }
                    (Some(worktree), None) => Some(worktree.read(cx).abs_path().to_path_buf()),
                    (None, Some(cwd)) if cwd.is_relative() => anyhow::bail!(
                        "relative cwd requires --worktree when the workspace has no default worktree"
                    ),
                    (None, Some(cwd)) => {
                        if !visible_worktrees.is_empty() {
                            let cwd_worktree = project
                                .find_worktree(&cwd, cx)
                                .map(|(worktree, _)| worktree)
                                .filter(|worktree| worktree.read(cx).is_visible());
                            anyhow::ensure!(
                                cwd_worktree.is_some(),
                                "cwd {} is not inside an open worktree in workspace {workspace_id:?}",
                                cwd.display()
                            );
                        }
                        Some(cwd)
                    }
                    (None, None) => None,
                };
                let selected_worktree = selected_worktree.map(|worktree| {
                    let worktree = worktree.read(cx);
                    CliWorktree {
                        id: worktree_cli_id(worktree.id()),
                        name: worktree.root_name_str().to_string(),
                        path: worktree.abs_path().to_string_lossy().into_owned(),
                    }
                });
                let panel = workspace
                    .read(cx)
                    .panel::<AgentPanel>(cx)
                    .ok_or_else(|| anyhow!("agent panel is not available"))?;

                let terminal_task = window.update(cx, |_multi_workspace, window, cx| {
                    panel.update(cx, |panel, cx| {
                        panel.spawn_terminal_for_cli(resolved_cwd, command, window, cx)
                    })
                })?;
                Ok((terminal_task, selected_worktree))
            })?;
            let (terminal_id, _) = terminal_task.await?;
            serde_json::to_string_pretty(&serde_json::json!({
                "id": terminal_cli_id(terminal_id),
                "workspace_id": workspace_id,
                "worktree_id": selected_worktree.as_ref().map(|worktree| &worktree.id),
                "worktree_name": selected_worktree.as_ref().map(|worktree| &worktree.name),
                "worktree_path": selected_worktree.as_ref().map(|worktree| &worktree.path),
            }))
            .map_err(Into::into)
        }
        CliRequest::Open { .. } | CliRequest::SetOpenBehavior { .. } => {
            anyhow::bail!("unexpected CLI request")
        }
    }
}

/// Resolves the CLI open behavior when no explicit open behavior flag was given.
/// May prompt the user interactively on first run.
///
/// Returns `Some(behavior)` to override the default, or `None` if no override
/// is needed (e.g. no existing windows, paths already in a workspace, or the
/// user has already configured `cli_default_open_behavior` in settings).
async fn resolve_open_behavior(
    paths: &[String],
    app_state: &Arc<AppState>,
    responses: &dyn CliResponseSink,
    requests: &mut mpsc::UnboundedReceiver<CliRequest>,
    cx: &mut AsyncApp,
) -> Option<settings::CliDefaultOpenBehavior> {
    let has_existing_windows = cx.update(|cx| {
        cx.windows()
            .iter()
            .any(|window| window.downcast::<MultiWorkspace>().is_some())
    });

    if !has_existing_windows {
        return None;
    }

    if !paths.is_empty() {
        let paths_as_pathbufs: Vec<PathBuf> = paths.iter().map(PathBuf::from).collect();
        let paths_in_existing_workspace = cx.update(|cx| {
            for window in cx.windows() {
                if let Some(multi_workspace) = window.downcast::<MultiWorkspace>() {
                    if let Ok(multi_workspace) = multi_workspace.read(cx) {
                        for workspace in multi_workspace.workspaces() {
                            let project = workspace.read(cx).project().read(cx);
                            if project
                                .visibility_for_paths(&paths_as_pathbufs, false, cx)
                                .is_some()
                            {
                                return true;
                            }
                        }
                    }
                }
            }
            false
        });

        if paths_in_existing_workspace {
            return None;
        }
    }

    if !paths.is_empty() {
        let has_directory =
            futures::future::join_all(paths.iter().map(|p| app_state.fs.is_dir(Path::new(p))))
                .await
                .into_iter()
                .any(|is_dir| is_dir);

        if !has_directory {
            return None;
        }
    }

    let settings_text = app_state
        .fs
        .load(paths::settings_file())
        .await
        .unwrap_or_default();

    if settings_text.contains("cli_default_open_behavior") {
        return None;
    }

    responses.send(CliResponse::PromptOpenBehavior).log_err()?;

    if let Some(CliRequest::SetOpenBehavior { behavior }) = requests.next().await {
        let behavior = match behavior {
            cli::CliBehaviorSetting::ExistingWindow => {
                settings::CliDefaultOpenBehavior::ExistingWindow
            }
            cli::CliBehaviorSetting::NewWindow => settings::CliDefaultOpenBehavior::NewWindow,
        };

        let fs = app_state.fs.clone();
        cx.update(|cx| {
            settings::update_settings_file(fs, cx, move |content, _cx| {
                content.workspace.cli_default_open_behavior = Some(behavior);
            });
        });

        return Some(behavior);
    }

    None
}

pub(crate) fn open_options_for_request(
    open_behavior: Option<cli::OpenBehavior>,
    location: &SerializedWorkspaceLocation,
    cx: &App,
) -> workspace::OpenOptions {
    let open_behavior = open_behavior.unwrap_or_else(|| {
        match workspace::WorkspaceSettings::get_global(cx).default_open_behavior {
            settings::DefaultOpenBehavior::ExistingWindow => cli::OpenBehavior::ExistingWindow,
            settings::DefaultOpenBehavior::NewWindow => cli::OpenBehavior::PreferNewWindow,
        }
    });
    open_options_for_behavior(open_behavior, location, cx)
}

pub(crate) fn open_options_for_behavior(
    open_behavior: cli::OpenBehavior,
    location: &SerializedWorkspaceLocation,
    cx: &App,
) -> workspace::OpenOptions {
    let open_behavior = if open_behavior == cli::OpenBehavior::Default {
        open_behavior_for_default_setting(cx)
    } else {
        open_behavior
    };

    // If reuse flag is passed, open a new workspace in an existing window.
    let requesting_window = if open_behavior == cli::OpenBehavior::Reuse {
        workspace::workspace_windows_for_location(location, cx)
            .into_iter()
            .next()
    } else {
        None
    };
    workspace::OpenOptions {
        workspace_matching: match open_behavior {
            cli::OpenBehavior::AlwaysNew | cli::OpenBehavior::Reuse => {
                workspace::WorkspaceMatching::None
            }
            cli::OpenBehavior::PreferNewWindow => workspace::WorkspaceMatching::MatchSubpaths,
            cli::OpenBehavior::Add => workspace::WorkspaceMatching::MatchSubdirectory,
            _ => workspace::WorkspaceMatching::MatchExact,
        },
        add_dirs_to_sidebar: match open_behavior {
            cli::OpenBehavior::ExistingWindow => true,
            _ => false,
        },
        requesting_window,
        ..Default::default()
    }
}

fn open_behavior_for_default_setting(cx: &App) -> cli::OpenBehavior {
    match workspace::WorkspaceSettings::get_global(cx).cli_default_open_behavior {
        settings::CliDefaultOpenBehavior::ExistingWindow => cli::OpenBehavior::ExistingWindow,
        settings::CliDefaultOpenBehavior::NewWindow => cli::OpenBehavior::PreferNewWindow,
    }
}

async fn open_workspaces(
    paths: Vec<String>,
    diff_paths: Vec<[String; 2]>,
    diff_all: bool,
    open_behavior: cli::OpenBehavior,
    responses: &dyn CliResponseSink,
    wait: bool,
    dev_container: bool,
    app_state: Arc<AppState>,
    env: Option<collections::HashMap<String, String>>,
    cwd: Option<PathBuf>,
    cx: &mut AsyncApp,
) -> Result<()> {
    if paths.is_empty()
        && diff_paths.is_empty()
        && !matches!(open_behavior, cli::OpenBehavior::AlwaysNew)
    {
        return restore_or_create_workspace(app_state, cx).await;
    }

    let grouped_locations: Vec<(SerializedWorkspaceLocation, PathList)> =
        if paths.is_empty() && diff_paths.is_empty() {
            Vec::new()
        } else {
            vec![(
                SerializedWorkspaceLocation::Local,
                PathList::new(&paths.into_iter().map(PathBuf::from).collect::<Vec<_>>()),
            )]
        };

    if grouped_locations.is_empty() {
        // If we have no paths to open, show the welcome screen if this is the first launch
        let kvp = cx.update(|cx| KeyValueStore::global(cx));
        if matches!(kvp.read_kvp(FIRST_OPEN), Ok(None)) {
            cx.update(|cx| show_onboarding_view(app_state, cx).detach());
        }
        // If not the first launch, show an empty window with empty editor
        else {
            cx.update(|cx| {
                let open_options = OpenOptions {
                    env,
                    ..Default::default()
                };
                workspace::open_new(open_options, app_state, cx, |workspace, window, cx| {
                    Editor::new_file(workspace, &Default::default(), window, cx)
                })
                .detach_and_log_err(cx);
            });
        }
        return Ok(());
    }
    // If there are paths to open, open a workspace for each grouping of paths
    let mut errored = false;

    for (location, workspace_paths) in grouped_locations {
        let base_open_options =
            cx.update(|cx| open_options_for_behavior(open_behavior, &location, cx));
        let open_options = workspace::OpenOptions {
            wait,
            env: env.clone(),
            open_in_dev_container: dev_container,
            ..base_open_options
        };

        match location {
            SerializedWorkspaceLocation::Local => {
                let workspace_paths = workspace_paths
                    .paths()
                    .iter()
                    .map(|path| path.to_string_lossy().into_owned())
                    .collect();

                let workspace_failed_to_open = open_local_workspace(
                    workspace_paths,
                    diff_paths.clone(),
                    diff_all,
                    open_options,
                    cwd.clone(),
                    responses,
                    &app_state,
                    cx,
                )
                .await;

                if workspace_failed_to_open {
                    errored = true
                }
            }
            SerializedWorkspaceLocation::Remote(mut connection) => {
                let app_state = app_state.clone();
                if let RemoteConnectionOptions::Ssh(options) = &mut connection {
                    cx.update(|cx| {
                        RemoteSettings::get_global(cx)
                            .fill_connection_options_from_settings(options)
                    });
                }
                cx.spawn(async move |cx| {
                    open_remote_project(
                        connection,
                        workspace_paths.paths().to_vec(),
                        app_state,
                        open_options,
                        cx,
                    )
                    .await
                    .log_err();
                })
                .detach();
            }
        }
    }

    anyhow::ensure!(!errored, "failed to open a workspace");

    Ok(())
}

async fn open_local_workspace(
    mut workspace_paths: Vec<String>,
    diff_paths: Vec<[String; 2]>,
    diff_all: bool,
    open_options: workspace::OpenOptions,
    cwd: Option<PathBuf>,
    responses: &dyn CliResponseSink,
    app_state: &Arc<AppState>,
    cx: &mut AsyncApp,
) -> bool {
    let user_provided_paths = !workspace_paths.is_empty();

    // When only diff paths are provided (no regular paths), add the CLI's
    // working directory so the workspace opens with the right context.
    // Note: must use the CLI process's cwd (forwarded via `cli_cwd`), not
    // `std::env::current_dir()`, since the Zed app process's cwd is typically
    // `/` on macOS bundles or the launch dir of an already-running instance.
    if !user_provided_paths
        && !diff_paths.is_empty()
        && let Some(cwd) = cwd
    {
        workspace_paths.push(cwd.to_string_lossy().to_string());
    }

    let paths_with_position =
        derive_paths_with_position(app_state.fs.as_ref(), workspace_paths).await;

    let (workspace, items) = match open_paths_with_positions(
        &paths_with_position,
        &diff_paths,
        diff_all,
        app_state.clone(),
        open_options.clone(),
        cx,
    )
    .await
    {
        Ok(result) => result,
        Err(error) => {
            let paths = paths_with_position
                .iter()
                .map(|p| p.path.display().to_string())
                .collect::<Vec<_>>()
                .join(", ");
            log::error!("failed to open workspace [{paths}]: {error:#}");
            responses
                .send(CliResponse::Stderr {
                    message: format!("error opening [{paths}]: {error:#}"),
                })
                .log_err();
            return true;
        }
    };

    let mut errored = false;
    let mut item_release_futures = Vec::new();
    let mut subscriptions = Vec::new();
    // If --wait flag is used with no paths, or a directory, then wait until
    // the entire workspace is closed.
    if open_options.wait {
        let mut wait_for_window_close = paths_with_position.is_empty() && diff_paths.is_empty();
        if user_provided_paths {
            for path_with_position in &paths_with_position {
                if app_state.fs.is_dir(&path_with_position.path).await {
                    wait_for_window_close = true;
                    break;
                }
            }
        }

        if wait_for_window_close {
            let (release_tx, release_rx) = oneshot::channel();
            item_release_futures.push(release_rx);
            subscriptions.push(workspace.update(cx, |_, _, cx| {
                cx.on_release(move |_, _| {
                    let _ = release_tx.send(());
                })
            }));
        }
    }

    for item in items {
        match item {
            Some(Ok(item)) => {
                if open_options.wait {
                    let (release_tx, release_rx) = oneshot::channel();
                    item_release_futures.push(release_rx);
                    subscriptions.push(Ok(cx.update(|cx| {
                        item.on_release(
                            cx,
                            Box::new(move |_| {
                                release_tx.send(()).ok();
                            }),
                        )
                    })));
                }
            }
            Some(Err(err)) => {
                log::error!("{err:#}");
                responses
                    .send(CliResponse::Stderr {
                        message: format!("{err:#}"),
                    })
                    .log_err();
                errored = true;
            }
            None => {}
        }
    }

    if open_options.wait {
        let wait = async move {
            let _subscriptions = subscriptions;
            let _ = future::try_join_all(item_release_futures).await;
        }
        .fuse();
        futures::pin_mut!(wait);

        let background = cx.background_executor().clone();
        loop {
            // Repeatedly check if CLI is still open to avoid wasting resources
            // waiting for files or workspaces to close.
            let mut timer = background.timer(Duration::from_secs(1)).fuse();
            futures::select_biased! {
                _ = wait => break,
                _ = timer => {
                    if responses.send(CliResponse::Ping).is_err() {
                        break;
                    }
                }
            }
        }
    }

    errored
}

pub async fn derive_paths_with_position(
    fs: &dyn Fs,
    path_strings: impl IntoIterator<Item = impl AsRef<str>>,
) -> Vec<PathWithPosition> {
    let path_strings: Vec<_> = path_strings.into_iter().collect();
    let mut result = Vec::with_capacity(path_strings.len());
    for path_str in path_strings {
        let original_path = Path::new(path_str.as_ref());
        let mut parsed = PathWithPosition::parse_str(path_str.as_ref());

        // If the unparsed path string actually points to an existing file or directory, use it
        // instead of parsing out the line/col number. This matters for paths whose final
        // component looks like a position suffix, e.g. a folder named `Test (3)` would
        // otherwise be parsed as `Test ` at row 3.
        // Colon : is not valid in NTFS file names, so skip this logic if colon on windows.
        let has_colon = original_path
            .file_name()
            .and_then(|name| name.to_str())
            .is_none_or(|name| name.contains(':'));

        if (!has_colon || !cfg!(windows))
            && parsed.row.is_some()
            && parsed.path != original_path
            && (fs.is_file(original_path).await || fs.is_dir(original_path).await)
        {
            parsed = PathWithPosition::from_path(original_path.to_path_buf());
        }

        if let Ok(canonicalized) = fs.canonicalize(&parsed.path).await {
            parsed.path = canonicalized;
        }

        result.push(parsed);
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::zed::{open_listener::open_local_workspace, tests::init_test};
    use cli::CliResponse;
    use editor::Editor;
    use fs::FakeFs;
    use futures::poll;
    use gpui::{AppContext as _, TestAppContext, UpdateGlobal as _};
    use language::LineEnding;
    use project::{Project, WorktreePaths};
    use remote::SshConnectionOptions;
    use rope::Rope;
    use serde_json::json;
    use session::Session;
    use std::{path::Path, sync::Arc, task::Poll};
    use util::path;
    use workspace::{AppState, MultiWorkspace, ProjectGroup};

    struct DiscardResponseSink;

    impl CliResponseSink for DiscardResponseSink {
        fn send(&self, _response: CliResponse) -> anyhow::Result<()> {
            Ok(())
        }
    }

    struct SyncResponseSender(std::sync::mpsc::Sender<CliResponse>);

    impl CliResponseSink for SyncResponseSender {
        fn send(&self, response: CliResponse) -> anyhow::Result<()> {
            self.0
                .send(response)
                .map_err(|error| anyhow::anyhow!("{error}"))
        }
    }

    #[gpui::test]
    async fn test_terminal_cli_discovers_existing_terminal_and_spawns_command(
        cx: &mut TestAppContext,
    ) {
        cx.executor().allow_parking();
        init_test(cx);

        let fs = FakeFs::new(cx.executor());
        let project_root = std::env::current_dir().expect("test process should have a cwd");
        fs.insert_tree(&project_root, json!({})).await;
        let project = Project::test(fs, [project_root.as_path()], cx).await;
        let window = cx.add_window(|window, cx| MultiWorkspace::test_new(project, window, cx));
        cx.run_until_parked();
        let (workspace_id, panel) = window
            .update(cx, |multi_workspace, window, cx| {
                let workspace = multi_workspace.workspace().clone();
                let panel = workspace.update(cx, |workspace, cx| {
                    let panel = cx.new(|cx| AgentPanel::new_for_test(workspace, window, cx));
                    workspace.add_panel(panel.clone(), window, cx);
                    panel
                });
                (workspace_cli_id(&workspace), panel)
            })
            .expect("workspace window should be available");

        let existing_terminal = window
            .update(cx, |_multi_workspace, window, cx| {
                panel.update(cx, |panel, cx| {
                    panel.insert_test_terminal("existing agent terminal", false, window, cx)
                })
            })
            .expect("workspace window should be available")
            .expect("agent terminal should be inserted");

        let terminal_list = cx
            .spawn(|mut cx| async move {
                handle_terminal_cli_request(
                    CliRequest::ListTerminals {
                        workspace_id: None,
                        worktree_id: None,
                    },
                    &mut cx,
                )
                .await
            })
            .await
            .expect("terminal list should succeed");
        let terminal_list: serde_json::Value =
            serde_json::from_str(&terminal_list).expect("terminal list should be JSON");
        assert!(terminal_list.as_array().is_some_and(|terminals| {
            terminals
                .iter()
                .any(|terminal| terminal["id"] == terminal_cli_id(existing_terminal))
        }));

        let command = if cfg!(windows) {
            vec![
                "cmd.exe".to_string(),
                "/D".to_string(),
                "/C".to_string(),
                "exit".to_string(),
                "0".to_string(),
            ]
        } else {
            vec!["true".to_string()]
        };
        let spawn_result = cx
            .spawn(move |mut cx| async move {
                handle_terminal_cli_request(
                    CliRequest::SpawnTerminal {
                        workspace_id,
                        worktree_id: None,
                        cwd: None,
                        command,
                    },
                    &mut cx,
                )
                .await
            })
            .await
            .expect("command terminal should spawn");
        let spawn_result: serde_json::Value =
            serde_json::from_str(&spawn_result).expect("spawn result should be JSON");
        let spawned_terminal_id = spawn_result["id"]
            .as_str()
            .expect("spawn result should contain a terminal ID")
            .to_string();
        assert!(spawned_terminal_id.starts_with("terminal-"));

        let terminal_list = cx
            .spawn(|mut cx| async move {
                handle_terminal_cli_request(
                    CliRequest::ListTerminals {
                        workspace_id: None,
                        worktree_id: None,
                    },
                    &mut cx,
                )
                .await
            })
            .await
            .expect("terminal list should succeed after spawning");
        let terminal_list: serde_json::Value =
            serde_json::from_str(&terminal_list).expect("terminal list should be JSON");
        assert!(terminal_list.as_array().is_some_and(|terminals| {
            terminals.iter().any(|terminal| {
                terminal["id"] == spawned_terminal_id && terminal["location"] == "agent"
            })
        }));

        window
            .update(cx, |multi_workspace, _window, cx| {
                let workspace = multi_workspace.workspace().read(cx);
                assert!(workspace.all_docks().iter().any(|dock| {
                    let dock = dock.read(cx);
                    dock.is_open()
                        && dock.visible_panel().is_some_and(|visible_panel| {
                            visible_panel.panel_id() == panel.entity_id()
                        })
                }));
            })
            .expect("workspace window should be available");
    }

    #[gpui::test]
    async fn test_terminal_cli_lists_worktrees_and_rejects_ambiguous_spawn(
        cx: &mut TestAppContext,
    ) {
        cx.executor().allow_parking();
        init_test(cx);

        let fs = FakeFs::new(cx.executor());
        fs.insert_tree(path!("/project-a"), json!({})).await;
        fs.insert_tree(path!("/project-b"), json!({})).await;
        let project = Project::test(
            fs,
            [
                Path::new(path!("/project-a")),
                Path::new(path!("/project-b")),
            ],
            cx,
        )
        .await;
        let window = cx.add_window(|window, cx| MultiWorkspace::test_new(project, window, cx));
        cx.run_until_parked();
        let workspace_id = window
            .update(cx, |multi_workspace, _window, _cx| {
                workspace_cli_id(multi_workspace.workspace())
            })
            .expect("workspace window should be available");

        let workspace_list = cx
            .spawn(|mut cx| async move {
                handle_terminal_cli_request(CliRequest::ListWorkspaces, &mut cx).await
            })
            .await
            .expect("workspace list should succeed");
        let workspace_list: serde_json::Value =
            serde_json::from_str(&workspace_list).expect("workspace list should be JSON");
        let worktrees = workspace_list[0]["workspaces"][0]["worktrees"]
            .as_array()
            .unwrap_or_else(|| panic!("workspace should include worktrees: {workspace_list}"));
        assert_eq!(worktrees.len(), 2);
        assert!(worktrees.iter().all(|worktree| {
            worktree["id"]
                .as_str()
                .is_some_and(|id| id.starts_with("worktree-"))
                && worktree["name"].is_string()
                && worktree["path"].is_string()
        }));
        let worktree_id = worktrees[0]["id"]
            .as_str()
            .expect("worktree should have an ID")
            .to_string();

        let filtered_terminals = cx
            .spawn({
                let workspace_id = workspace_id.clone();
                let worktree_id = worktree_id.clone();
                move |mut cx| async move {
                    handle_terminal_cli_request(
                        CliRequest::ListTerminals {
                            workspace_id: Some(workspace_id),
                            worktree_id: Some(worktree_id),
                        },
                        &mut cx,
                    )
                    .await
                }
            })
            .await
            .expect("terminal filtering by worktree should succeed");
        assert_eq!(filtered_terminals, "[]");

        let spawn_error = cx
            .spawn({
                let workspace_id = workspace_id.clone();
                move |mut cx| async move {
                    handle_terminal_cli_request(
                        CliRequest::SpawnTerminal {
                            workspace_id,
                            worktree_id: None,
                            cwd: None,
                            command: Vec::new(),
                        },
                        &mut cx,
                    )
                    .await
                }
            })
            .await
            .expect_err("spawn should require a worktree when multiple are open");
        assert!(spawn_error.to_string().contains("specify --worktree"));

        let cwd_error = cx
            .spawn(move |mut cx| async move {
                handle_terminal_cli_request(
                    CliRequest::SpawnTerminal {
                        workspace_id,
                        worktree_id: Some(worktree_id),
                        cwd: Some(PathBuf::from("../outside")),
                        command: Vec::new(),
                    },
                    &mut cx,
                )
                .await
            })
            .await
            .expect_err("relative cwd should not escape its worktree");
        assert!(cwd_error.to_string().contains("cannot leave"));
    }

    #[gpui::test]
    async fn test_terminal_cli_lists_restorable_sidebar_terminal(cx: &mut TestAppContext) {
        cx.executor().allow_parking();
        init_test(cx);

        let fs = FakeFs::new(cx.executor());
        fs.insert_tree(path!("/project"), json!({})).await;
        let project = Project::test(fs, [Path::new(path!("/project"))], cx).await;
        let window = cx.add_window(|window, cx| MultiWorkspace::test_new(project, window, cx));
        cx.run_until_parked();
        let (workspace_id, workspace, panel, project_group_key) = window
            .update(cx, |multi_workspace, window, cx| {
                let workspace = multi_workspace.workspace().clone();
                let project_group_key = workspace.read(cx).project_group_key(cx);
                multi_workspace.test_add_project_group(ProjectGroup {
                    key: project_group_key.clone(),
                    workspaces: vec![workspace.clone()],
                    expanded: true,
                });
                multi_workspace.add(workspace.clone(), window, cx);
                let panel = workspace.update(cx, |workspace, cx| {
                    let panel = cx.new(|cx| AgentPanel::new_for_test(workspace, window, cx));
                    workspace.add_panel(panel.clone(), window, cx);
                    panel
                });
                (
                    workspace_cli_id(&workspace),
                    workspace,
                    panel,
                    project_group_key,
                )
            })
            .expect("workspace window should be available");
        let terminal_id = window
            .update(cx, |_multi_workspace, window, cx| {
                panel.update(cx, |panel, cx| {
                    panel.insert_test_terminal("restorable terminal", false, window, cx)
                })
            })
            .expect("workspace window should be available")
            .expect("agent terminal should be inserted");
        let mut metadata = cx.read(|cx| {
            TerminalThreadMetadataStore::global(cx)
                .read(cx)
                .entry(terminal_id)
                .cloned()
                .expect("terminal metadata should be persisted")
        });
        let paths = project_group_key.path_list().clone();
        metadata.worktree_paths = WorktreePaths::from_folder_paths(&paths);
        window
            .update(cx, |_multi_workspace, window, cx| {
                workspace.update(cx, |workspace, cx| {
                    workspace.remove_panel(&panel, window, cx);
                });
            })
            .expect("workspace should remain open");
        cx.update(|cx| {
            TerminalThreadMetadataStore::global(cx).update(cx, |store, cx| {
                store.save(metadata, cx);
            });
        });
        cx.run_until_parked();
        cx.read(|cx| {
            assert!(
                TerminalThreadMetadataStore::global(cx)
                    .read(cx)
                    .entry(terminal_id)
                    .is_some(),
                "terminal metadata should remain available"
            );
            assert_eq!(all_cli_terminals(cx).len(), 1);
        });

        let terminal_list = cx
            .spawn(move |mut cx| async move {
                handle_terminal_cli_request(
                    CliRequest::ListTerminals {
                        workspace_id: Some(workspace_id),
                        worktree_id: None,
                    },
                    &mut cx,
                )
                .await
            })
            .await
            .expect("terminal list should succeed");
        let terminal_list: serde_json::Value =
            serde_json::from_str(&terminal_list).expect("terminal list should be JSON");
        assert!(
            terminal_list.as_array().is_some_and(|terminals| {
                terminals.iter().any(|terminal| {
                    terminal["id"] == terminal_cli_id(terminal_id)
                        && terminal["title"] == "restorable terminal"
                        && terminal["loaded"] == false
                        && terminal["status"] == "unloaded"
                })
            }),
            "unexpected terminal list: {terminal_list}"
        );
    }

    #[gpui::test]
    async fn test_worktree_cli_creates_visible_workspace_for_terminal_spawn(
        cx: &mut TestAppContext,
    ) {
        cx.executor().allow_parking();
        init_test(cx);

        let fs = FakeFs::new(cx.executor());
        fs.insert_tree(
            path!("/root"),
            json!({
                "project": {
                    ".git": {},
                    "src": { "main.rs": "fn main() {}" },
                },
            }),
        )
        .await;
        let project_root = Path::new(path!("/root/project"));
        let project = Project::test(fs, [project_root], cx).await;
        project
            .update(cx, |project, cx| project.git_scans_complete(cx))
            .await;
        let window = cx.add_window(|window, cx| MultiWorkspace::test_new(project, window, cx));
        let source_workspace_id = window
            .update(cx, |multi_workspace, _window, _cx| {
                workspace_cli_id(multi_workspace.workspace())
            })
            .expect("workspace window should be available");

        let create_result = cx
            .spawn({
                let source_workspace_id = source_workspace_id.clone();
                |mut cx| async move {
                    handle_terminal_cli_request(
                        CliRequest::CreateWorktree {
                            workspace_id: source_workspace_id,
                            base_ref: Some("HEAD".to_string()),
                            branch: Some("agent/cli-task".to_string()),
                            worktree_name: Some("cli-task".to_string()),
                        },
                        &mut cx,
                    )
                    .await
                }
            })
            .await
            .expect("worktree creation should succeed");
        let create_result: serde_json::Value =
            serde_json::from_str(&create_result).expect("create result should be JSON");
        let created_workspace_id = create_result["workspace_id"]
            .as_str()
            .expect("create result should contain a workspace ID")
            .to_string();
        let created_worktree_id = create_result["worktree_id"]
            .as_str()
            .expect("single-repository result should contain a worktree ID")
            .to_string();
        assert_ne!(created_workspace_id, source_workspace_id);
        assert_eq!(create_result["workspace_name"], "cli-task");
        assert_eq!(create_result["worktree_name"], "project");
        assert_eq!(create_result["base_ref"], "HEAD");
        assert_eq!(create_result["branch"], "agent/cli-task");
        assert_eq!(create_result["detached"], false);

        let created_is_visible = cx.read(|cx| {
            all_cli_workspaces(cx)
                .into_iter()
                .any(|(_, workspace, _)| workspace_cli_id(&workspace) == created_workspace_id)
        });
        assert!(
            created_is_visible,
            "created worktree workspace should be retained in the sidebar"
        );

        cx.run_until_parked();
        let workspace_list = cx
            .spawn(|mut cx| async move {
                handle_terminal_cli_request(CliRequest::ListWorkspaces, &mut cx).await
            })
            .await
            .expect("workspace list should succeed");
        let workspace_list: serde_json::Value =
            serde_json::from_str(&workspace_list).expect("workspace list should be JSON");
        let projects = workspace_list
            .as_array()
            .expect("workspace list should contain projects");
        assert_eq!(projects.len(), 1);
        assert_eq!(projects[0]["name"], "project");
        assert_eq!(
            projects[0]["main_worktree_paths"][0],
            project_root.to_string_lossy().as_ref()
        );
        let project_workspaces = projects[0]["workspaces"]
            .as_array()
            .expect("project should contain workspaces");
        assert_eq!(project_workspaces.len(), 2);
        let main_workspace = project_workspaces
            .iter()
            .find(|workspace| workspace["name"] == "main")
            .expect("project should contain its main workspace");
        assert_eq!(main_workspace["worktrees"][0]["is_main_worktree"], true);
        let created_workspace = project_workspaces
            .iter()
            .find(|workspace| workspace["id"] == create_result["workspace_id"])
            .expect("project should contain the created workspace");
        assert_eq!(created_workspace["name"], "cli-task");
        assert_eq!(created_workspace["worktrees"][0]["name"], "project");
        assert_eq!(created_workspace["worktrees"][0]["default_branch"], "main");
        assert_eq!(created_workspace["worktrees"][0]["is_main_worktree"], false);
        assert_eq!(
            created_workspace["worktrees"][0]["main_worktree_path"],
            project_root.to_string_lossy().as_ref()
        );

        let worktree_id_for_spawn = created_worktree_id.clone();
        let spawn_result = cx
            .spawn(move |mut cx| async move {
                handle_terminal_cli_request(
                    CliRequest::SpawnTerminal {
                        workspace_id: created_workspace_id,
                        worktree_id: Some(worktree_id_for_spawn),
                        cwd: None,
                        command: if cfg!(windows) {
                            vec![
                                "cmd.exe".to_string(),
                                "/D".to_string(),
                                "/C".to_string(),
                                "exit".to_string(),
                                "0".to_string(),
                            ]
                        } else {
                            vec!["true".to_string()]
                        },
                    },
                    &mut cx,
                )
                .await
            })
            .await
            .expect("terminal should spawn in the created worktree");
        let spawn_result: serde_json::Value =
            serde_json::from_str(&spawn_result).expect("spawn result should be JSON");
        assert_eq!(spawn_result["worktree_id"], created_worktree_id);
    }

    fn assert_ssh_parse(
        cx: &mut TestAppContext,
        input: &str,
        expected_url: Option<&str>,
        host: &str,
        username: Option<&str>,
        port: Option<u16>,
        path: &str,
    ) {
        if let Some(expected_url) = expected_url {
            assert_eq!(parse_ssh_url(input).unwrap().as_str(), expected_url);
        }

        let request = cx.update(|cx| {
            let rq = RawOpenRequest {
                urls: vec![input.into()],
                ..Default::default()
            };
            OpenRequest::parse(rq, cx).unwrap()
        });
        assert_eq!(
            request.remote_connection.unwrap(),
            RemoteConnectionOptions::Ssh(SshConnectionOptions {
                host: host.into(),
                username: username.map(str::to_string),
                port,
                ..Default::default()
            })
        );
        assert_eq!(request.open_paths, vec![path]);
    }

    #[gpui::test]
    fn test_parse_ssh_urls(cx: &mut TestAppContext) {
        let _app_state = init_test(cx);
        let cases = [
            ("ssh://me@host:/", None, "host", Some("me"), None, "/"),
            (
                "ssh://me@host:~/code",
                None,
                "host",
                Some("me"),
                None,
                "/~/code",
            ),
            (
                "ssh://me@host:22/tmp",
                None,
                "host",
                Some("me"),
                Some(22),
                "/tmp",
            ),
            (
                "ssh://user@domain.tld@host:22/tmp",
                None,
                "host",
                Some("user@domain.tld"),
                Some(22),
                "/tmp",
            ),
            (
                "ssh://domain\\user@host/dir",
                Some("ssh://domain%5Cuser@host/dir"),
                "host",
                Some("domain\\user"),
                None,
                "/dir",
            ),
            (
                r"ssh://domain\\user@localhost/project",
                Some("ssh://domain%5C%5Cuser@localhost/project"),
                "localhost",
                Some(r"domain\\user"),
                None,
                "/project",
            ),
        ];

        for (input, expected_url, host, username, port, path) in cases {
            assert_ssh_parse(cx, input, expected_url, host, username, port, path);
        }
    }

    #[gpui::test]
    async fn test_derive_paths_with_position_directory_with_position_like_name(
        cx: &mut TestAppContext,
    ) {
        let app_state = init_test(cx);
        let fs = app_state.fs.as_fake();

        // A folder whose name ends in `(N)` or `(row,col)` would otherwise be parsed as a
        // path with a row/column suffix (e.g. the MSVC-style `file.c(22)`), truncating the name.
        fs.insert_tree(
            path!("/root"),
            json!({
                "TEST (1)": {},
                "Project (2,3)": {},
                "test 123": {},
            }),
        )
        .await;

        let inputs = vec![
            path!("/root/TEST (1)").to_string(),
            path!("/root/Project (2,3)").to_string(),
            path!("/root/test 123").to_string(),
        ];
        let result = derive_paths_with_position(fs.as_ref(), inputs).await;

        let paths: Vec<_> = result
            .iter()
            .map(|p| (p.path.to_string_lossy().to_string(), p.row, p.column))
            .collect();
        assert_eq!(
            paths,
            vec![
                (path!("/root/TEST (1)").to_string(), None, None),
                (path!("/root/Project (2,3)").to_string(), None, None),
                (path!("/root/test 123").to_string(), None, None),
            ]
        );
    }

    // Test file with colon (`:`) in the name on non-Windows platforms,
    // as it is valid for file names on Unix-like systems.
    #[cfg(not(target_os = "windows"))]
    #[gpui::test]
    async fn test_derive_paths_with_position_colon_in_name_reverts_on_unix(
        cx: &mut TestAppContext,
    ) {
        let app_state = init_test(cx);
        let fs = app_state.fs.as_fake();

        fs.insert_tree(path!("/root"), json!({ "test.txt:10": "" }))
            .await;

        let result =
            derive_paths_with_position(fs.as_ref(), vec![path!("/root/test.txt:10").to_string()])
                .await;

        let paths: Vec<_> = result
            .iter()
            .map(|p| (p.path.to_string_lossy().to_string(), p.row, p.column))
            .collect();
        assert_eq!(
            paths,
            vec![(path!("/root/test.txt:10").to_string(), None, None)]
        );
    }

    // On Windows `:` is used to delimit NTFS alternate data streams,
    // `notes.txt:10` should be parsed as `notes.txt` at row 10
    #[cfg(target_os = "windows")]
    #[gpui::test]
    async fn test_derive_paths_with_position_colon_in_name_parsed_as_position_on_windows(
        cx: &mut TestAppContext,
    ) {
        let app_state = init_test(cx);
        let fs = app_state.fs.as_fake();

        fs.insert_tree(path!("/root"), json!({ "test.txt": "" }))
            .await;

        let result =
            derive_paths_with_position(fs.as_ref(), vec![path!("/root/test.txt:10").to_string()])
                .await;

        let paths: Vec<_> = result
            .iter()
            .map(|p| (p.path.to_string_lossy().to_string(), p.row, p.column))
            .collect();
        assert_eq!(
            paths,
            vec![(path!("/root/test.txt").to_string(), Some(10), None)]
        );
    }

    #[gpui::test]
    fn test_parse_ssh_url_preserves_open_behavior(cx: &mut TestAppContext) {
        let _app_state = init_test(cx);

        let request = cx.update(|cx| {
            OpenRequest::parse(
                RawOpenRequest {
                    urls: vec!["ssh://me@host:/".into()],
                    open_behavior: Some(cli::OpenBehavior::AlwaysNew),
                    ..Default::default()
                },
                cx,
            )
            .unwrap()
        });

        assert_eq!(request.open_behavior, Some(cli::OpenBehavior::AlwaysNew));
    }

    #[gpui::test]
    fn test_reject_ssh_urls(cx: &mut TestAppContext) {
        let _app_state = init_test(cx);

        for input in [
            "ssh://me@localhost:code/vibes/mine-bot",
            "ssh://me@localhost:2222:~/project",
            "ssh://me@[2001:db8::1]:~/project",
        ] {
            let result = cx.update(|cx| {
                OpenRequest::parse(
                    RawOpenRequest {
                        urls: vec![input.into()],
                        ..Default::default()
                    },
                    cx,
                )
            });
            assert!(result.is_err(), "{input} should be rejected");
        }
    }

    #[gpui::test]
    fn test_open_options_for_behavior_always_new(cx: &mut TestAppContext) {
        let _app_state = init_test(cx);
        let options = cx.update(|cx| {
            open_options_for_behavior(
                cli::OpenBehavior::AlwaysNew,
                &SerializedWorkspaceLocation::Local,
                cx,
            )
        });
        assert_eq!(
            options.workspace_matching,
            workspace::WorkspaceMatching::None
        );
        assert!(!options.add_dirs_to_sidebar);
        assert!(options.requesting_window.is_none());
    }

    #[gpui::test]
    fn test_open_options_for_request_respects_default_open_behavior(cx: &mut TestAppContext) {
        use gpui::UpdateGlobal as _;

        let _app_state = init_test(cx);

        // A `None` behavior (e.g. a Finder or URL open) consults the UI-level
        // `default_open_behavior` setting rather than falling back to fixed
        // defaults.
        cx.update(|cx| {
            settings::SettingsStore::update_global(cx, |store, cx| {
                store.update_user_settings(cx, |settings| {
                    settings.workspace.default_open_behavior =
                        Some(settings::DefaultOpenBehavior::NewWindow);
                });
            });
        });
        let options =
            cx.update(|cx| open_options_for_request(None, &SerializedWorkspaceLocation::Local, cx));
        assert_eq!(
            options.workspace_matching,
            workspace::WorkspaceMatching::MatchSubpaths
        );
        assert!(!options.add_dirs_to_sidebar);

        cx.update(|cx| {
            settings::SettingsStore::update_global(cx, |store, cx| {
                store.update_user_settings(cx, |settings| {
                    settings.workspace.default_open_behavior =
                        Some(settings::DefaultOpenBehavior::ExistingWindow);
                });
            });
        });
        let options =
            cx.update(|cx| open_options_for_request(None, &SerializedWorkspaceLocation::Local, cx));
        assert_eq!(
            options.workspace_matching,
            workspace::WorkspaceMatching::MatchExact
        );
        assert!(options.add_dirs_to_sidebar);
    }

    #[gpui::test]
    fn test_parse_agent_url(cx: &mut TestAppContext) {
        let _app_state = init_test(cx);

        let request = cx.update(|cx| {
            OpenRequest::parse(
                RawOpenRequest {
                    urls: vec!["zed://agent".into()],
                    ..Default::default()
                },
                cx,
            )
            .unwrap()
        });

        match request.kind {
            Some(OpenRequestKind::AgentPanel {
                external_source_prompt,
            }) => {
                assert_eq!(external_source_prompt, None);
            }
            _ => panic!("Expected AgentPanel kind"),
        }
    }

    #[gpui::test]
    fn test_parse_skill_install_url(cx: &mut TestAppContext) {
        let _app_state = init_test(cx);

        let content =
            "---\nname: my-skill\ndescription: Does a thing.\n---\n\nDo the thing.\n".to_string();
        let link = agent_skills::encode_skill_share_link(&content);

        let request = cx.update(|cx| {
            OpenRequest::parse(
                RawOpenRequest {
                    urls: vec![link],
                    ..Default::default()
                },
                cx,
            )
            .unwrap()
        });

        match request.kind {
            Some(OpenRequestKind::InstallSkill {
                content: parsed_content,
            }) => {
                assert_eq!(parsed_content, content);
            }
            _ => panic!("Expected InstallSkill kind"),
        }
    }

    #[gpui::test]
    fn test_parse_malformed_skill_install_url_errors(cx: &mut TestAppContext) {
        let _app_state = init_test(cx);

        let result = cx.update(|cx| {
            OpenRequest::parse(
                RawOpenRequest {
                    urls: vec!["zed://skill?data=!!!notbase64".into()],
                    ..Default::default()
                },
                cx,
            )
        });

        assert!(result.is_err());
    }

    fn agent_url_with_prompt(prompt: &str) -> String {
        let mut serializer = url::form_urlencoded::Serializer::new("zed://agent?".to_string());
        serializer.append_pair("prompt", prompt);
        serializer.finish()
    }

    #[gpui::test]
    fn test_parse_agent_url_with_prompt(cx: &mut TestAppContext) {
        let _app_state = init_test(cx);
        let prompt = "Write me a script\nThanks";

        let request = cx.update(|cx| {
            OpenRequest::parse(
                RawOpenRequest {
                    urls: vec![agent_url_with_prompt(prompt)],
                    ..Default::default()
                },
                cx,
            )
            .unwrap()
        });

        match request.kind {
            Some(OpenRequestKind::AgentPanel {
                external_source_prompt,
            }) => {
                assert_eq!(
                    external_source_prompt
                        .as_ref()
                        .map(ExternalSourcePrompt::as_str),
                    Some("Write me a script\nThanks")
                );
            }
            _ => panic!("Expected AgentPanel kind"),
        }
    }

    #[gpui::test]
    fn test_parse_agent_url_with_trailing_slash(cx: &mut TestAppContext) {
        let _app_state = init_test(cx);

        let request = cx.update(|cx| {
            OpenRequest::parse(
                RawOpenRequest {
                    urls: vec!["zed://agent/?prompt=hello".into()],
                    ..Default::default()
                },
                cx,
            )
            .unwrap()
        });

        match request.kind {
            Some(OpenRequestKind::AgentPanel {
                external_source_prompt,
            }) => {
                assert_eq!(
                    external_source_prompt
                        .as_ref()
                        .map(ExternalSourcePrompt::as_str),
                    Some("hello")
                );
            }
            _ => panic!("Expected AgentPanel kind"),
        }
    }

    #[gpui::test]
    fn test_parse_focus_app_url(cx: &mut TestAppContext) {
        let _app_state = init_test(cx);

        for url in ["zed://", "zed://open", "zed://open/"] {
            let request = cx.update(|cx| {
                OpenRequest::parse(
                    RawOpenRequest {
                        urls: vec![url.into()],
                        ..Default::default()
                    },
                    cx,
                )
                .unwrap()
            });
            assert!(
                matches!(request.kind, Some(OpenRequestKind::FocusApp)),
                "expected FocusApp for {url}, got {:?}",
                request.kind
            );
            assert!(
                request.is_focus_app_only(),
                "expected is_focus_app_only for {url}"
            );
        }
    }

    #[gpui::test]
    fn test_parse_agent_url_with_empty_prompt(cx: &mut TestAppContext) {
        let _app_state = init_test(cx);

        let request = cx.update(|cx| {
            OpenRequest::parse(
                RawOpenRequest {
                    urls: vec![agent_url_with_prompt("")],
                    ..Default::default()
                },
                cx,
            )
            .unwrap()
        });

        match request.kind {
            Some(OpenRequestKind::AgentPanel {
                external_source_prompt,
            }) => {
                assert_eq!(external_source_prompt, None);
            }
            _ => panic!("Expected AgentPanel kind"),
        }
    }

    #[gpui::test]
    fn test_parse_git_commit_url(cx: &mut TestAppContext) {
        let _app_state = init_test(cx);

        // Test basic git commit URL
        let request = cx.update(|cx| {
            OpenRequest::parse(
                RawOpenRequest {
                    urls: vec!["zed://git/commit/abc123?repo=path/to/repo".into()],
                    ..Default::default()
                },
                cx,
            )
            .unwrap()
        });

        match request.kind.unwrap() {
            OpenRequestKind::GitCommit { sha } => {
                assert_eq!(sha, "abc123");
            }
            _ => panic!("expected GitCommit variant"),
        }
        // Verify path was added to open_paths for workspace routing
        assert_eq!(request.open_paths, vec!["path/to/repo"]);

        // Test with URL encoded path
        let request = cx.update(|cx| {
            OpenRequest::parse(
                RawOpenRequest {
                    urls: vec!["zed://git/commit/def456?repo=path%20with%20spaces".into()],
                    ..Default::default()
                },
                cx,
            )
            .unwrap()
        });

        match request.kind.unwrap() {
            OpenRequestKind::GitCommit { sha } => {
                assert_eq!(sha, "def456");
            }
            _ => panic!("expected GitCommit variant"),
        }
        assert_eq!(request.open_paths, vec!["path with spaces"]);

        // Test with empty path
        cx.update(|cx| {
            assert!(
                OpenRequest::parse(
                    RawOpenRequest {
                        urls: vec!["zed://git/commit/abc123?repo=".into()],
                        ..Default::default()
                    },
                    cx,
                )
                .unwrap_err()
                .to_string()
                .contains("missing repo")
            );
        });

        // Test error case: missing SHA
        let result = cx.update(|cx| {
            OpenRequest::parse(
                RawOpenRequest {
                    urls: vec!["zed://git/commit/abc123?foo=bar".into()],
                    ..Default::default()
                },
                cx,
            )
        });
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("missing repo query parameter")
        );
    }

    #[gpui::test]
    async fn test_open_workspace_with_directory(cx: &mut TestAppContext) {
        let app_state = init_test(cx);

        app_state
            .fs
            .as_fake()
            .insert_tree(
                path!("/root"),
                json!({
                    "dir1": {
                        "file1.txt": "content1",
                        "file2.txt": "content2",
                    },
                }),
            )
            .await;

        assert_eq!(cx.windows().len(), 0);

        // First open the workspace directory
        open_workspace_file(path!("/root/dir1"), <_>::default(), app_state.clone(), cx).await;

        assert_eq!(cx.windows().len(), 1);
        let multi_workspace = cx.windows()[0].downcast::<MultiWorkspace>().unwrap();
        multi_workspace
            .update(cx, |multi_workspace, _, cx| {
                multi_workspace.workspace().update(cx, |workspace, cx| {
                    assert!(workspace.active_item_as::<Editor>(cx).is_none())
                });
            })
            .unwrap();

        // Now open a file inside that workspace
        open_workspace_file(
            path!("/root/dir1/file1.txt"),
            <_>::default(),
            app_state.clone(),
            cx,
        )
        .await;

        assert_eq!(cx.windows().len(), 1);
        multi_workspace
            .update(cx, |multi_workspace, _, cx| {
                multi_workspace.workspace().update(cx, |workspace, cx| {
                    assert!(workspace.active_item_as::<Editor>(cx).is_some());
                });
            })
            .unwrap();

        // Opening a file inside the existing worktree with -n creates a new window.
        open_workspace_file(
            path!("/root/dir1/file1.txt"),
            workspace::OpenOptions {
                workspace_matching: workspace::WorkspaceMatching::None,
                ..Default::default()
            },
            app_state.clone(),
            cx,
        )
        .await;

        assert_eq!(cx.windows().len(), 2);
    }

    #[gpui::test]
    async fn test_wait_with_directory_waits_for_window_close(cx: &mut TestAppContext) {
        let app_state = init_test(cx);

        app_state
            .fs
            .as_fake()
            .insert_tree(
                path!("/root"),
                json!({
                    "dir1": {
                        "file1.txt": "content1",
                    },
                }),
            )
            .await;

        let response_sink = DiscardResponseSink;
        let workspace_paths = vec![path!("/root/dir1").to_owned()];

        let (done_tx, mut done_rx) = futures::channel::oneshot::channel();
        cx.spawn({
            let app_state = app_state.clone();
            move |mut cx| async move {
                let errored = open_local_workspace(
                    workspace_paths,
                    vec![],
                    false,
                    workspace::OpenOptions {
                        wait: true,
                        ..Default::default()
                    },
                    None,
                    &response_sink,
                    &app_state,
                    &mut cx,
                )
                .await;
                let _ = done_tx.send(errored);
            }
        })
        .detach();

        cx.background_executor.run_until_parked();
        assert_eq!(cx.windows().len(), 1);
        assert!(matches!(poll!(&mut done_rx), Poll::Pending));

        let window = cx.windows()[0];
        cx.update_window(window, |_, window, _| window.remove_window())
            .unwrap();
        cx.background_executor.run_until_parked();

        let errored = done_rx.await.unwrap();
        assert!(!errored);
    }

    #[gpui::test]
    async fn test_open_workspace_with_nonexistent_files(cx: &mut TestAppContext) {
        let app_state = init_test(cx);

        app_state
            .fs
            .as_fake()
            .insert_tree(path!("/root"), json!({}))
            .await;

        assert_eq!(cx.windows().len(), 0);

        // Test case 1: Open a single file that does not exist yet
        open_workspace_file(
            path!("/root/file5.txt"),
            <_>::default(),
            app_state.clone(),
            cx,
        )
        .await;

        assert_eq!(cx.windows().len(), 1);
        let multi_workspace_1 = cx.windows()[0].downcast::<MultiWorkspace>().unwrap();
        multi_workspace_1
            .update(cx, |multi_workspace, _, cx| {
                multi_workspace.workspace().update(cx, |workspace, cx| {
                    assert!(workspace.active_item_as::<Editor>(cx).is_some())
                });
            })
            .unwrap();

        // Test case 2: Open a single file that does not exist yet,
        // but tell Zed to add it to the current workspace
        open_workspace_file(
            path!("/root/file6.txt"),
            workspace::OpenOptions {
                workspace_matching: workspace::WorkspaceMatching::MatchSubdirectory,
                ..Default::default()
            },
            app_state.clone(),
            cx,
        )
        .await;

        assert_eq!(cx.windows().len(), 1);
        multi_workspace_1
            .update(cx, |multi_workspace, _, cx| {
                multi_workspace.workspace().update(cx, |workspace, cx| {
                    let items = workspace.items(cx).collect::<Vec<_>>();
                    assert_eq!(items.len(), 2, "Workspace should have two items");
                });
            })
            .unwrap();

        // Test case 3: Open a single file that does not exist yet,
        // but tell Zed to NOT add it to the current workspace
        open_workspace_file(
            path!("/root/file7.txt"),
            workspace::OpenOptions {
                workspace_matching: workspace::WorkspaceMatching::None,
                ..Default::default()
            },
            app_state.clone(),
            cx,
        )
        .await;

        assert_eq!(cx.windows().len(), 2);
        let multi_workspace_2 = cx.windows()[1].downcast::<MultiWorkspace>().unwrap();
        multi_workspace_2
            .update(cx, |multi_workspace, _, cx| {
                multi_workspace.workspace().update(cx, |workspace, cx| {
                    let items = workspace.items(cx).collect::<Vec<_>>();
                    assert_eq!(items.len(), 1, "Workspace should have two items");
                });
            })
            .unwrap();
    }

    async fn open_workspace_file(
        path: &str,
        open_options: workspace::OpenOptions,
        app_state: Arc<AppState>,
        cx: &TestAppContext,
    ) {
        let response_sink = DiscardResponseSink;

        let workspace_paths = vec![path.to_owned()];

        let errored = cx
            .spawn(|mut cx| async move {
                open_local_workspace(
                    workspace_paths,
                    vec![],
                    false,
                    open_options,
                    None,
                    &response_sink,
                    &app_state,
                    &mut cx,
                )
                .await
            })
            .await;

        assert!(!errored);
    }

    #[gpui::test]
    async fn test_reuse_flag_functionality(cx: &mut TestAppContext) {
        let app_state = init_test(cx);

        let root_dir = if cfg!(windows) { "C:\\root" } else { "/root" };
        let file1_path = if cfg!(windows) {
            "C:\\root\\file1.txt"
        } else {
            "/root/file1.txt"
        };
        let file2_path = if cfg!(windows) {
            "C:\\root\\file2.txt"
        } else {
            "/root/file2.txt"
        };

        app_state.fs.create_dir(Path::new(root_dir)).await.unwrap();
        app_state
            .fs
            .create_file(Path::new(file1_path), Default::default())
            .await
            .unwrap();
        app_state
            .fs
            .save(
                Path::new(file1_path),
                &Rope::from("content1"),
                LineEnding::Unix,
            )
            .await
            .unwrap();
        app_state
            .fs
            .create_file(Path::new(file2_path), Default::default())
            .await
            .unwrap();
        app_state
            .fs
            .save(
                Path::new(file2_path),
                &Rope::from("content2"),
                LineEnding::Unix,
            )
            .await
            .unwrap();

        // First, open a workspace normally
        let response_sink = DiscardResponseSink;
        let workspace_paths = vec![file1_path.to_string()];

        let _errored = cx
            .spawn({
                let app_state = app_state.clone();
                |mut cx| async move {
                    open_local_workspace(
                        workspace_paths,
                        vec![],
                        false,
                        workspace::OpenOptions::default(),
                        None,
                        &response_sink,
                        &app_state,
                        &mut cx,
                    )
                    .await
                }
            })
            .await;

        // Now test the reuse functionality - should replace the existing workspace
        let workspace_paths_reuse = vec![file1_path.to_string()];
        let paths: Vec<PathBuf> = workspace_paths_reuse.iter().map(PathBuf::from).collect();
        let window_to_replace = workspace::find_existing_workspace(
            &paths,
            &workspace::OpenOptions::default(),
            &workspace::SerializedWorkspaceLocation::Local,
            &mut cx.to_async(),
        )
        .await
        .0
        .unwrap()
        .0;

        let errored_reuse = cx
            .spawn({
                let app_state = app_state.clone();
                |mut cx| async move {
                    let response_sink = DiscardResponseSink;
                    open_local_workspace(
                        workspace_paths_reuse,
                        vec![],
                        false,
                        workspace::OpenOptions {
                            requesting_window: Some(window_to_replace),
                            ..Default::default()
                        },
                        None,
                        &response_sink,
                        &app_state,
                        &mut cx,
                    )
                    .await
                }
            })
            .await;

        assert!(!errored_reuse);
    }

    #[gpui::test]
    fn test_parse_git_clone_url(cx: &mut TestAppContext) {
        let _app_state = init_test(cx);

        let request = cx.update(|cx| {
            OpenRequest::parse(
                RawOpenRequest {
                    urls: vec![
                        "zed://git/clone/?repo=https://github.com/zed-industries/zed.git".into(),
                    ],
                    ..Default::default()
                },
                cx,
            )
            .unwrap()
        });

        match request.kind {
            Some(OpenRequestKind::GitClone { repo_url }) => {
                assert_eq!(repo_url, "https://github.com/zed-industries/zed.git");
            }
            _ => panic!("Expected GitClone kind"),
        }
    }

    #[gpui::test]
    fn test_parse_git_clone_url_without_slash(cx: &mut TestAppContext) {
        let _app_state = init_test(cx);

        let request = cx.update(|cx| {
            OpenRequest::parse(
                RawOpenRequest {
                    urls: vec![
                        "zed://git/clone?repo=https://github.com/zed-industries/zed.git".into(),
                    ],
                    ..Default::default()
                },
                cx,
            )
            .unwrap()
        });

        match request.kind {
            Some(OpenRequestKind::GitClone { repo_url }) => {
                assert_eq!(repo_url, "https://github.com/zed-industries/zed.git");
            }
            _ => panic!("Expected GitClone kind"),
        }
    }

    #[gpui::test]
    fn test_parse_git_clone_url_with_encoding(cx: &mut TestAppContext) {
        let _app_state = init_test(cx);

        let request = cx.update(|cx| {
            OpenRequest::parse(
                RawOpenRequest {
                    urls: vec![
                        "zed://git/clone/?repo=https%3A%2F%2Fgithub.com%2Fzed-industries%2Fzed.git"
                            .into(),
                    ],
                    ..Default::default()
                },
                cx,
            )
            .unwrap()
        });

        match request.kind {
            Some(OpenRequestKind::GitClone { repo_url }) => {
                assert_eq!(repo_url, "https://github.com/zed-industries/zed.git");
            }
            _ => panic!("Expected GitClone kind"),
        }
    }

    #[gpui::test]
    async fn test_add_flag_prefers_focused_window(cx: &mut TestAppContext) {
        let app_state = init_test(cx);

        let root_dir = if cfg!(windows) { "C:\\root" } else { "/root" };
        let file1_path = if cfg!(windows) {
            "C:\\root\\file1.txt"
        } else {
            "/root/file1.txt"
        };
        let file2_path = if cfg!(windows) {
            "C:\\root\\file2.txt"
        } else {
            "/root/file2.txt"
        };

        app_state.fs.create_dir(Path::new(root_dir)).await.unwrap();
        app_state
            .fs
            .create_file(Path::new(file1_path), Default::default())
            .await
            .unwrap();
        app_state
            .fs
            .save(
                Path::new(file1_path),
                &Rope::from("content1"),
                LineEnding::Unix,
            )
            .await
            .unwrap();
        app_state
            .fs
            .create_file(Path::new(file2_path), Default::default())
            .await
            .unwrap();
        app_state
            .fs
            .save(
                Path::new(file2_path),
                &Rope::from("content2"),
                LineEnding::Unix,
            )
            .await
            .unwrap();

        // Open first workspace
        let workspace_paths_1 = vec![file1_path.to_string()];
        let _errored = cx
            .spawn({
                let app_state = app_state.clone();
                |mut cx| async move {
                    let response_sink = DiscardResponseSink;
                    open_local_workspace(
                        workspace_paths_1,
                        Vec::new(),
                        false,
                        workspace::OpenOptions::default(),
                        None,
                        &response_sink,
                        &app_state,
                        &mut cx,
                    )
                    .await
                }
            })
            .await;

        assert_eq!(cx.windows().len(), 1);
        let multi_workspace_1 = cx.windows()[0].downcast::<MultiWorkspace>().unwrap();

        // Open second workspace in a new window
        let workspace_paths_2 = vec![file2_path.to_string()];
        let _errored = cx
            .spawn({
                let app_state = app_state.clone();
                |mut cx| async move {
                    let response_sink = DiscardResponseSink;
                    open_local_workspace(
                        workspace_paths_2,
                        Vec::new(),
                        false,
                        workspace::OpenOptions {
                            workspace_matching: workspace::WorkspaceMatching::None, // Force new window
                            ..Default::default()
                        },
                        None,
                        &response_sink,
                        &app_state,
                        &mut cx,
                    )
                    .await
                }
            })
            .await;

        assert_eq!(cx.windows().len(), 2);
        let multi_workspace_2 = cx.windows()[1].downcast::<MultiWorkspace>().unwrap();

        // Focus window2
        multi_workspace_2
            .update(cx, |_, window, _| {
                window.activate_window();
            })
            .unwrap();

        // Now use --add flag (open_behavior = OpenBehavior::Add) to add a new file
        // It should open in the focused window (window2), not an arbitrary window
        let new_file_path = if cfg!(windows) {
            "C:\\root\\new_file.txt"
        } else {
            "/root/new_file.txt"
        };
        app_state
            .fs
            .create_file(Path::new(new_file_path), Default::default())
            .await
            .unwrap();

        let workspace_paths_add = vec![new_file_path.to_string()];
        let _errored = cx
            .spawn({
                let app_state = app_state.clone();
                |mut cx| async move {
                    let response_sink = DiscardResponseSink;
                    open_local_workspace(
                        workspace_paths_add,
                        Vec::new(),
                        false,
                        workspace::OpenOptions {
                            workspace_matching: workspace::WorkspaceMatching::MatchSubdirectory, // --add flag
                            ..Default::default()
                        },
                        None,
                        &response_sink,
                        &app_state,
                        &mut cx,
                    )
                    .await
                }
            })
            .await;

        // Should still have 2 windows (file added to existing focused window)
        assert_eq!(cx.windows().len(), 2);

        // Verify the file was added to window2 (the focused one)
        multi_workspace_2
            .update(cx, |workspace, _, cx| {
                let items = workspace.workspace().read(cx).items(cx).collect::<Vec<_>>();
                // Should have 2 items now (file2.txt and new_file.txt)
                assert_eq!(items.len(), 2, "Focused window should have 2 items");
            })
            .unwrap();

        // Verify window1 still has only 1 item
        multi_workspace_1
            .update(cx, |workspace, _, cx| {
                let items = workspace.workspace().read(cx).items(cx).collect::<Vec<_>>();
                assert_eq!(items.len(), 1, "Other window should still have 1 item");
            })
            .unwrap();
    }

    #[gpui::test]
    async fn test_dev_container_flag_opens_modal(cx: &mut TestAppContext) {
        let app_state = init_test(cx);
        cx.update(|cx| recent_projects::init(cx));

        app_state
            .fs
            .as_fake()
            .insert_tree(
                path!("/project"),
                json!({
                    ".devcontainer": {
                        "devcontainer.json": "{}"
                    },
                    "src": {
                        "main.rs": "fn main() {}"
                    }
                }),
            )
            .await;

        let errored = cx
            .spawn({
                let app_state = app_state.clone();
                |mut cx| async move {
                    let response_sink = DiscardResponseSink;
                    open_local_workspace(
                        vec![path!("/project").to_owned()],
                        vec![],
                        false,
                        workspace::OpenOptions {
                            open_in_dev_container: true,
                            ..Default::default()
                        },
                        None,
                        &response_sink,
                        &app_state,
                        &mut cx,
                    )
                    .await
                }
            })
            .await;

        assert!(!errored);
        cx.run_until_parked();

        let multi_workspace = cx.update(|cx| cx.windows()[0].downcast::<MultiWorkspace>().unwrap());
        multi_workspace
            .update(cx, |multi_workspace, _, cx| {
                let flag = multi_workspace.workspace().read(cx).open_in_dev_container();
                assert!(
                    !flag,
                    "open_in_dev_container flag should be consumed by suggest_on_worktree_updated"
                );
            })
            .unwrap();
    }

    #[gpui::test]
    async fn test_dev_container_flag_cleared_without_config(cx: &mut TestAppContext) {
        let app_state = init_test(cx);
        cx.update(|cx| recent_projects::init(cx));

        app_state
            .fs
            .as_fake()
            .insert_tree(
                path!("/project"),
                json!({
                    "src": {
                        "main.rs": "fn main() {}"
                    }
                }),
            )
            .await;

        let errored = cx
            .spawn({
                let app_state = app_state.clone();
                |mut cx| async move {
                    let response_sink = DiscardResponseSink;
                    open_local_workspace(
                        vec![path!("/project").to_owned()],
                        vec![],
                        false,
                        workspace::OpenOptions {
                            open_in_dev_container: true,
                            ..Default::default()
                        },
                        None,
                        &response_sink,
                        &app_state,
                        &mut cx,
                    )
                    .await
                }
            })
            .await;

        assert!(!errored);

        // Let any pending worktree scan events and updates settle.
        cx.run_until_parked();

        // With no .devcontainer config, the flag should be cleared once the
        // worktree scan completes, rather than persisting on the workspace.
        let multi_workspace = cx.update(|cx| cx.windows()[0].downcast::<MultiWorkspace>().unwrap());
        multi_workspace
            .update(cx, |multi_workspace, _, cx| {
                let flag = multi_workspace
                    .workspace()
                    .read(cx)
                    .open_in_dev_container();
                assert!(
                    !flag,
                    "open_in_dev_container flag should be cleared when no devcontainer config exists"
                );
            })
            .unwrap();
    }

    fn make_cli_open_request(paths: Vec<String>, open_behavior: cli::OpenBehavior) -> CliRequest {
        CliRequest::Open {
            paths,
            urls: vec![],
            diff_paths: vec![],
            diff_all: false,
            wsl: None,
            wait: false,
            open_behavior,
            env: None,
            user_data_dir: None,
            dev_container: false,
            cwd: None,
        }
    }

    fn make_cli_url_open_request(
        urls: Vec<String>,
        open_behavior: cli::OpenBehavior,
    ) -> CliRequest {
        CliRequest::Open {
            paths: vec![],
            urls,
            diff_paths: vec![],
            diff_all: false,
            wsl: None,
            wait: false,
            open_behavior,
            env: None,
            user_data_dir: None,
            dev_container: false,
            cwd: None,
        }
    }

    /// Runs the real [`cli::run_cli_response_loop`] on an OS thread against
    /// the Zed-side `handle_cli_connection` on the GPUI foreground executor,
    /// using `allow_parking` so the test scheduler tolerates cross-thread
    /// wakeups.
    ///
    /// Returns `(exit_status, prompt_was_shown)`.
    fn run_cli_with_zed_handler(
        cx: &mut TestAppContext,
        app_state: Arc<AppState>,
        open_request: CliRequest,
        prompt_response: Option<cli::CliBehaviorSetting>,
    ) -> (i32, bool) {
        cx.executor().allow_parking();

        let (request_tx, request_rx) = mpsc::unbounded::<CliRequest>();
        let (response_tx, response_rx) = std::sync::mpsc::channel::<CliResponse>();
        let response_sink: Box<dyn CliResponseSink> = Box::new(SyncResponseSender(response_tx));

        cx.spawn(|mut cx| async move {
            handle_cli_connection((request_rx, response_sink), app_state, &mut cx).await;
        })
        .detach();

        let prompt_called = Arc::new(std::sync::atomic::AtomicBool::new(false));
        let prompt_called_for_thread = prompt_called.clone();

        let cli_thread = std::thread::spawn(move || -> anyhow::Result<i32> {
            request_tx
                .unbounded_send(open_request)
                .map_err(|error| anyhow::anyhow!("{error}"))?;

            while let Ok(response) = response_rx.recv() {
                match response {
                    CliResponse::Ping => {}
                    CliResponse::Stdout { .. } | CliResponse::Stderr { .. } => {}
                    CliResponse::Exit { status } => return Ok(status),
                    CliResponse::PromptOpenBehavior => {
                        prompt_called_for_thread.store(true, std::sync::atomic::Ordering::SeqCst);
                        let behavior =
                            prompt_response.unwrap_or(cli::CliBehaviorSetting::ExistingWindow);
                        request_tx
                            .unbounded_send(CliRequest::SetOpenBehavior { behavior })
                            .map_err(|error| anyhow::anyhow!("{error}"))?;
                    }
                }
            }

            anyhow::bail!("CLI response channel closed without Exit")
        });

        while !cli_thread.is_finished() {
            cx.run_until_parked();
            std::thread::sleep(std::time::Duration::from_millis(1));
        }

        let exit_status = cli_thread.join().unwrap().expect("CLI loop failed");
        let prompt_shown = prompt_called.load(std::sync::atomic::Ordering::SeqCst);

        // Flush any remaining async work (e.g. settings file writes).
        cx.run_until_parked();

        (exit_status, prompt_shown)
    }

    #[gpui::test]
    async fn test_e2e_no_flags_no_windows_no_prompt(cx: &mut TestAppContext) {
        let app_state = init_test(cx);

        app_state
            .fs
            .as_fake()
            .insert_tree(path!("/project"), json!({ "file.txt": "content" }))
            .await;

        assert_eq!(cx.windows().len(), 0);

        let (status, prompt_shown) = run_cli_with_zed_handler(
            cx,
            app_state,
            make_cli_open_request(
                vec![path!("/project/file.txt").to_string()],
                cli::OpenBehavior::Default,
            ),
            None,
        );

        assert_eq!(status, 0);
        assert!(
            !prompt_shown,
            "no prompt should be shown when no windows exist"
        );
        assert_eq!(cx.windows().len(), 1);
    }

    #[gpui::test]
    async fn test_e2e_prompt_user_picks_existing_window(cx: &mut TestAppContext) {
        let app_state = init_test(cx);

        app_state
            .fs
            .as_fake()
            .insert_tree(path!("/project_a"), json!({ "file.txt": "content" }))
            .await;
        app_state
            .fs
            .as_fake()
            .insert_tree(path!("/project_b"), json!({ "file.txt": "content" }))
            .await;

        // Create an existing window so the prompt triggers
        open_workspace_file(
            path!("/project_a"),
            Default::default(),
            app_state.clone(),
            cx,
        )
        .await;
        assert_eq!(cx.windows().len(), 1);

        let (status, prompt_shown) = run_cli_with_zed_handler(
            cx,
            app_state.clone(),
            make_cli_open_request(
                vec![path!("/project_b").to_string()],
                cli::OpenBehavior::Default,
            ),
            Some(cli::CliBehaviorSetting::ExistingWindow),
        );

        assert_eq!(status, 0);
        assert!(prompt_shown, "prompt should be shown");
        assert_eq!(cx.windows().len(), 1);

        let settings_text = app_state
            .fs
            .load(paths::settings_file())
            .await
            .unwrap_or_default();
        assert!(
            settings_text.contains("existing_window"),
            "settings should contain 'existing_window', got: {settings_text}"
        );
    }

    #[gpui::test]
    async fn test_e2e_prompt_user_picks_new_window(cx: &mut TestAppContext) {
        let app_state = init_test(cx);

        app_state
            .fs
            .as_fake()
            .insert_tree(path!("/project_a"), json!({ "file.txt": "content" }))
            .await;
        app_state
            .fs
            .as_fake()
            .insert_tree(path!("/project_b"), json!({ "file.txt": "content" }))
            .await;

        // Create an existing window with project_a
        open_workspace_file(
            path!("/project_a"),
            Default::default(),
            app_state.clone(),
            cx,
        )
        .await;
        assert_eq!(cx.windows().len(), 1);

        let (status, prompt_shown) = run_cli_with_zed_handler(
            cx,
            app_state.clone(),
            make_cli_open_request(
                vec![path!("/project_b").to_string()],
                cli::OpenBehavior::Default,
            ),
            Some(cli::CliBehaviorSetting::NewWindow),
        );

        assert_eq!(status, 0);
        assert!(prompt_shown, "prompt should be shown");
        assert_eq!(cx.windows().len(), 2);

        let settings_text = app_state
            .fs
            .load(paths::settings_file())
            .await
            .unwrap_or_default();
        assert!(
            settings_text.contains("new_window"),
            "settings should contain 'new_window', got: {settings_text}"
        );
    }

    #[gpui::test]
    async fn test_e2e_setting_already_configured_no_prompt(cx: &mut TestAppContext) {
        let app_state = init_test(cx);

        app_state
            .fs
            .as_fake()
            .insert_tree(path!("/project"), json!({ "file.txt": "content" }))
            .await;

        // Pre-configure the setting in settings.json
        app_state
            .fs
            .as_fake()
            .insert_tree(
                paths::config_dir(),
                json!({
                    "settings.json": r#"{"cli_default_open_behavior": "existing_window"}"#
                }),
            )
            .await;

        // Create an existing window
        open_workspace_file(path!("/project"), Default::default(), app_state.clone(), cx).await;
        assert_eq!(cx.windows().len(), 1);

        let (status, prompt_shown) = run_cli_with_zed_handler(
            cx,
            app_state,
            make_cli_open_request(
                vec![path!("/project/file.txt").to_string()],
                cli::OpenBehavior::Default,
            ),
            None,
        );

        assert_eq!(status, 0);
        assert!(
            !prompt_shown,
            "no prompt should be shown when setting already configured"
        );
    }

    #[gpui::test]
    async fn test_e2e_new_window_setting_restores_workspace_when_no_paths(cx: &mut TestAppContext) {
        let app_state = init_test(cx);

        app_state
            .fs
            .as_fake()
            .insert_tree(path!("/project"), json!({ "file.txt": "content" }))
            .await;

        cx.update(|cx| {
            settings::SettingsStore::update_global(cx, |store, cx| {
                store.update_user_settings(cx, |settings| {
                    settings.workspace.cli_default_open_behavior =
                        Some(settings::CliDefaultOpenBehavior::NewWindow);
                });
            });
        });

        let session_id = cx.read(|cx| app_state.session.read(cx).id().to_owned());

        open_workspace_file(path!("/project"), Default::default(), app_state.clone(), cx).await;
        assert_eq!(cx.windows().len(), 1);

        let multi_workspace = cx.windows()[0].downcast::<MultiWorkspace>().unwrap();
        let serialization_tasks = multi_workspace
            .update(cx, |multi_workspace, window, cx| {
                multi_workspace.flush_all_serialization(window, cx)
            })
            .unwrap();
        futures::future::join_all(serialization_tasks).await;

        multi_workspace
            .update(cx, |_, window, _| window.remove_window())
            .unwrap();
        cx.run_until_parked();
        assert_eq!(cx.windows().len(), 0);

        cx.update(|cx| {
            app_state.session.update(cx, |app_session, _cx| {
                app_session.replace_session_for_test(Session::test_with_old_session(session_id));
            });
        });

        let (status, prompt_shown) = run_cli_with_zed_handler(
            cx,
            app_state,
            make_cli_open_request(Vec::new(), cli::OpenBehavior::Default),
            None,
        );

        assert_eq!(status, 0);
        assert!(
            !prompt_shown,
            "no prompt should be shown when no windows exist"
        );
        assert_eq!(cx.windows().len(), 1);

        let restored_window = cx.windows()[0].downcast::<MultiWorkspace>().unwrap();
        restored_window
            .read_with(cx, |multi_workspace, cx| {
                let root_paths = multi_workspace.workspace().read(cx).root_paths(cx);
                assert!(
                    root_paths
                        .iter()
                        .any(|path| path.as_ref() == Path::new(path!("/project"))),
                    "expected CLI launch with no paths to restore /project, got {root_paths:?}"
                );
            })
            .unwrap();
    }

    #[gpui::test]
    async fn test_e2e_new_window_setting_opens_project_root_in_new_window(cx: &mut TestAppContext) {
        let app_state = init_test(cx);

        app_state
            .fs
            .as_fake()
            .insert_tree(path!("/project"), json!({ "file.txt": "content" }))
            .await;

        app_state
            .fs
            .as_fake()
            .insert_tree(
                paths::config_dir(),
                json!({
                    "settings.json": r#"{"cli_default_open_behavior": "new_window"}"#
                }),
            )
            .await;

        cx.update(|cx| {
            settings::SettingsStore::update_global(cx, |store, cx| {
                store.update_user_settings(cx, |settings| {
                    settings.workspace.cli_default_open_behavior =
                        Some(settings::CliDefaultOpenBehavior::NewWindow);
                });
            });
        });

        open_workspace_file(path!("/project"), Default::default(), app_state.clone(), cx).await;
        assert_eq!(cx.windows().len(), 1);

        let (status, prompt_shown) = run_cli_with_zed_handler(
            cx,
            app_state,
            make_cli_open_request(
                vec![path!("/project").to_string()],
                cli::OpenBehavior::Default,
            ),
            None,
        );

        assert_eq!(status, 0);
        assert!(
            !prompt_shown,
            "no prompt should be shown when setting already configured"
        );
        assert_eq!(cx.windows().len(), 2);
    }

    #[gpui::test]
    async fn test_e2e_new_window_setting_focuses_existing_window_for_subpaths(
        cx: &mut TestAppContext,
    ) {
        let app_state = init_test(cx);

        app_state
            .fs
            .as_fake()
            .insert_tree(
                path!("/project"),
                json!({
                    "file.txt": "content",
                    "src": {
                        "main.rs": "fn main() {}",
                    },
                }),
            )
            .await;

        app_state
            .fs
            .as_fake()
            .insert_tree(
                paths::config_dir(),
                json!({
                    "settings.json": r#"{"cli_default_open_behavior": "new_window"}"#
                }),
            )
            .await;

        cx.update(|cx| {
            settings::SettingsStore::update_global(cx, |store, cx| {
                store.update_user_settings(cx, |settings| {
                    settings.workspace.cli_default_open_behavior =
                        Some(settings::CliDefaultOpenBehavior::NewWindow);
                });
            });
        });

        open_workspace_file(path!("/project"), Default::default(), app_state.clone(), cx).await;
        assert_eq!(cx.windows().len(), 1);

        let (status, prompt_shown) = run_cli_with_zed_handler(
            cx,
            app_state.clone(),
            make_cli_open_request(
                vec![path!("/project/src").to_string()],
                cli::OpenBehavior::Default,
            ),
            None,
        );

        assert_eq!(status, 0);
        assert!(
            !prompt_shown,
            "no prompt should be shown when setting already configured"
        );
        assert_eq!(cx.windows().len(), 1);

        let (status, prompt_shown) = run_cli_with_zed_handler(
            cx,
            app_state,
            make_cli_open_request(
                vec![path!("/project/file.txt").to_string()],
                cli::OpenBehavior::Default,
            ),
            None,
        );

        assert_eq!(status, 0);
        assert!(
            !prompt_shown,
            "no prompt should be shown when setting already configured"
        );
        assert_eq!(cx.windows().len(), 1);
    }

    #[gpui::test]
    async fn test_e2e_explicit_existing_flag_no_prompt(cx: &mut TestAppContext) {
        let app_state = init_test(cx);

        app_state
            .fs
            .as_fake()
            .insert_tree(path!("/project"), json!({ "file.txt": "content" }))
            .await;

        // Create an existing window
        open_workspace_file(path!("/project"), Default::default(), app_state.clone(), cx).await;
        assert_eq!(cx.windows().len(), 1);

        let (status, prompt_shown) = run_cli_with_zed_handler(
            cx,
            app_state,
            make_cli_open_request(
                vec![path!("/project/file.txt").to_string()],
                cli::OpenBehavior::ExistingWindow, // -e flag: force existing window
            ),
            None,
        );

        assert_eq!(status, 0);
        assert!(!prompt_shown, "no prompt should be shown with -e flag");
        assert_eq!(cx.windows().len(), 1);
    }

    #[gpui::test]
    async fn test_e2e_explicit_new_flag_no_prompt(cx: &mut TestAppContext) {
        let app_state = init_test(cx);

        app_state
            .fs
            .as_fake()
            .insert_tree(path!("/project_a"), json!({ "file.txt": "content" }))
            .await;
        app_state
            .fs
            .as_fake()
            .insert_tree(path!("/project_b"), json!({ "file.txt": "content" }))
            .await;

        // Create an existing window
        open_workspace_file(
            path!("/project_a"),
            Default::default(),
            app_state.clone(),
            cx,
        )
        .await;
        assert_eq!(cx.windows().len(), 1);

        let (status, prompt_shown) = run_cli_with_zed_handler(
            cx,
            app_state,
            make_cli_open_request(
                vec![path!("/project_b/file.txt").to_string()],
                cli::OpenBehavior::AlwaysNew, // -n flag: force new window
            ),
            None,
        );

        assert_eq!(status, 0);
        assert!(!prompt_shown, "no prompt should be shown with -n flag");
        assert_eq!(cx.windows().len(), 2);
    }

    #[gpui::test]
    async fn test_e2e_explicit_new_flag_with_file_url_opens_new_window(cx: &mut TestAppContext) {
        let app_state = init_test(cx);

        app_state
            .fs
            .as_fake()
            .insert_tree(path!("/project"), json!({ "file.txt": "content" }))
            .await;

        open_workspace_file(path!("/project"), Default::default(), app_state.clone(), cx).await;
        assert_eq!(cx.windows().len(), 1);

        let file_url = format!(
            "file://{}",
            urlencoding::encode(path!("/project/file.txt")).into_owned()
        );
        let (status, prompt_shown) = run_cli_with_zed_handler(
            cx,
            app_state,
            make_cli_url_open_request(vec![file_url], cli::OpenBehavior::AlwaysNew),
            None,
        );

        assert_eq!(status, 0);
        assert!(!prompt_shown, "no prompt should be shown with -n flag");
        assert_eq!(cx.windows().len(), 2);
    }

    #[gpui::test]
    async fn test_e2e_paths_in_existing_workspace_no_prompt(cx: &mut TestAppContext) {
        let app_state = init_test(cx);

        app_state
            .fs
            .as_fake()
            .insert_tree(
                path!("/project"),
                json!({
                    "src": {
                        "main.rs": "fn main() {}",
                    }
                }),
            )
            .await;

        // Open the project directory as a workspace
        open_workspace_file(path!("/project"), Default::default(), app_state.clone(), cx).await;
        assert_eq!(cx.windows().len(), 1);

        // Opening a file inside the already-open workspace should not prompt
        let (status, prompt_shown) = run_cli_with_zed_handler(
            cx,
            app_state,
            make_cli_open_request(
                vec![path!("/project/src/main.rs").to_string()],
                cli::OpenBehavior::Default,
            ),
            None,
        );

        assert_eq!(status, 0);
        assert!(
            !prompt_shown,
            "no prompt should be shown when paths are in an existing workspace"
        );
        // File opened in existing window
        assert_eq!(cx.windows().len(), 1);
    }
}
