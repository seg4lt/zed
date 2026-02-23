use acp_thread::ThreadStatus;
use agent_ui::{AgentPanel, AgentPanelEvent};
use anyhow::{Result, anyhow};
use chrono::{Datelike, Local, NaiveDate, TimeDelta};
use db::kvp::KEY_VALUE_STORE;
use editor::Editor;

use fs::Fs;
use fuzzy::StringMatchCandidate;
use gpui::{
    App, Context, DismissEvent, Entity, EventEmitter, FocusHandle, Focusable, Pixels, Render,
    SharedString, Subscription, Task, WeakEntity, Window, px,
};
use picker::{Picker, PickerDelegate};
use project::Event as ProjectEvent;
use recent_projects::{RecentProjectEntry, get_recent_projects};
use std::fmt::Display;

use std::collections::{HashMap, HashSet};

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Arc;
use theme::ActiveTheme;
use ui::utils::TRAFFIC_LIGHT_PADDING;
use ui::{
    AgentThreadStatus, Button, ButtonStyle, ContextMenu, Divider, DividerColor, KeyBinding,
    ListSubHeader, PopoverMenu, Tab, ThreadItem, Tooltip, prelude::*,
};
use ui_input::ErasedEditor;
use util::ResultExt as _;
use workspace::{
    FocusWorkspaceSidebar, ModalView, MultiWorkspace, NewWorkspaceInWindow,
    Sidebar as WorkspaceSidebar, SidebarEvent, ToggleWorkspaceSidebar, Workspace,
    notifications::DetachAndPromptErr,
};

#[derive(Clone, Debug)]
struct AgentThreadInfo {
    title: SharedString,
    status: AgentThreadStatus,
    icon: IconName,
    needs_attention: bool,
}

const LAST_THREAD_TITLES_KEY: &str = "sidebar-last-thread-titles";
const WORKSPACE_DISPLAY_NAMES_KEY: &str = "sidebar-workspace-display-names";

const DEFAULT_WIDTH: Pixels = px(320.0);
const MIN_WIDTH: Pixels = px(200.0);
const MAX_WIDTH: Pixels = px(800.0);
const MAX_MATCHES: usize = 100;

#[derive(Clone)]
struct WorkspaceThreadEntry {
    index: usize,
    worktree_label: SharedString,
    full_path: SharedString,
    custom_name: Option<SharedString>,
    thread_info: Option<AgentThreadInfo>,
}

impl WorkspaceThreadEntry {
    fn new(
        index: usize,
        workspace: &Entity<Workspace>,
        persisted_titles: &HashMap<String, String>,
        persisted_workspace_names: &HashMap<String, String>,
        cx: &App,
    ) -> Self {
        let workspace_ref = workspace.read(cx);

        let worktrees: Vec<_> = workspace_ref
            .worktrees(cx)
            .filter(|worktree| worktree.read(cx).is_visible())
            .map(|worktree| worktree.read(cx).abs_path())
            .collect();

        let worktree_names: Vec<String> = worktrees
            .iter()
            .filter_map(|path| {
                path.file_name()
                    .map(|name| name.to_string_lossy().to_string())
            })
            .collect();

        let worktree_label: SharedString = if worktree_names.is_empty() {
            format!("Workspace {}", index + 1).into()
        } else {
            worktree_names.join(", ").into()
        };

        let full_path: SharedString = worktrees
            .iter()
            .map(|path| path.to_string_lossy().to_string())
            .collect::<Vec<_>>()
            .join("\n")
            .into();

        let custom_name = if worktrees.is_empty() {
            None
        } else {
            let path_key = sorted_paths_key(&worktrees);
            persisted_workspace_names
                .get(&path_key)
                .cloned()
                .map(SharedString::from)
        };

        let thread_info = Self::thread_info(workspace, cx).or_else(|| {
            if worktrees.is_empty() {
                return None;
            }
            let path_key = sorted_paths_key(&worktrees);
            let title = persisted_titles.get(&path_key)?;
            Some(AgentThreadInfo {
                title: SharedString::from(title.clone()),
                status: AgentThreadStatus::Completed,
                icon: IconName::ZedAgent,
                needs_attention: false,
            })
        });

        Self {
            index,
            worktree_label,
            full_path,
            custom_name,
            thread_info,
        }
    }

    fn thread_info(workspace: &Entity<Workspace>, cx: &App) -> Option<AgentThreadInfo> {
        let agent_panel = workspace.read(cx).panel::<AgentPanel>(cx)?;
        let agent_panel_ref = agent_panel.read(cx);

        if agent_panel_ref.is_active_terminal_view() {
            return Some(AgentThreadInfo {
                title: "Terminal".into(),
                status: AgentThreadStatus::Completed,
                icon: IconName::Terminal,
                needs_attention: agent_panel_ref.active_terminal_needs_attention(cx),
            });
        }

        let thread_view = agent_panel_ref.as_active_thread_view(cx)?.read(cx);
        let thread = thread_view.thread.read(cx);

        let icon = thread_view.agent_icon;
        let title = thread.title();

        let status = if thread.is_waiting_for_confirmation() {
            AgentThreadStatus::WaitingForConfirmation
        } else if thread.had_error() {
            AgentThreadStatus::Error
        } else {
            match thread.status() {
                ThreadStatus::Generating => AgentThreadStatus::Running,
                ThreadStatus::Idle => AgentThreadStatus::Completed,
            }
        };
        Some(AgentThreadInfo {
            title,
            status,
            icon,
            needs_attention: false,
        })
    }
}

#[derive(Clone)]
enum SidebarEntry {
    Separator(SharedString),
    WorkspaceThread(WorkspaceThreadEntry),
    RecentProject(RecentProjectEntry),
}

impl SidebarEntry {
    fn searchable_text(&self) -> &str {
        match self {
            SidebarEntry::Separator(_) => "",
            SidebarEntry::WorkspaceThread(entry) => entry.worktree_label.as_ref(),
            SidebarEntry::RecentProject(entry) => entry.name.as_ref(),
        }
    }
}

#[derive(Clone)]
struct SidebarMatch {
    entry: SidebarEntry,
    positions: Vec<usize>,
}

struct WorkspacePickerDelegate {
    multi_workspace: Entity<MultiWorkspace>,
    entries: Vec<SidebarEntry>,
    active_workspace_index: usize,
    workspace_thread_count: usize,
    /// All recent projects including what's filtered out of entries
    /// used to add unopened projects to entries on rebuild
    recent_projects: Vec<RecentProjectEntry>,
    recent_project_thread_titles: HashMap<SharedString, SharedString>,
    matches: Vec<SidebarMatch>,
    selected_index: usize,
    query: String,
    hovered_thread_item: Option<usize>,
    notified_workspaces: HashSet<usize>,
}

impl WorkspacePickerDelegate {
    fn new(multi_workspace: Entity<MultiWorkspace>) -> Self {
        Self {
            multi_workspace,
            entries: Vec::new(),
            active_workspace_index: 0,
            workspace_thread_count: 0,
            recent_projects: Vec::new(),
            recent_project_thread_titles: HashMap::new(),
            matches: Vec::new(),
            selected_index: 0,
            query: String::new(),
            hovered_thread_item: None,
            notified_workspaces: HashSet::new(),
        }
    }

    fn set_entries(
        &mut self,
        workspace_threads: Vec<WorkspaceThreadEntry>,
        active_workspace_index: usize,
        cx: &App,
    ) {
        if let Some(hovered_index) = self.hovered_thread_item {
            let still_exists = workspace_threads
                .iter()
                .any(|thread| thread.index == hovered_index);
            if !still_exists {
                self.hovered_thread_item = None;
            }
        }

        let old_statuses: HashMap<usize, AgentThreadStatus> = self
            .entries
            .iter()
            .filter_map(|entry| match entry {
                SidebarEntry::WorkspaceThread(thread) => thread
                    .thread_info
                    .as_ref()
                    .map(|info| (thread.index, info.status)),
                _ => None,
            })
            .collect();

        for thread in &workspace_threads {
            if let Some(info) = &thread.thread_info {
                if info.status == AgentThreadStatus::Completed
                    && thread.index != active_workspace_index
                {
                    if old_statuses.get(&thread.index) == Some(&AgentThreadStatus::Running) {
                        self.notified_workspaces.insert(thread.index);
                    }
                }
            }
        }

        if self.active_workspace_index != active_workspace_index {
            self.notified_workspaces.remove(&active_workspace_index);
        }
        self.active_workspace_index = active_workspace_index;
        self.workspace_thread_count = workspace_threads.len();
        self.rebuild_entries(workspace_threads, cx);
    }

    fn set_recent_projects(&mut self, recent_projects: Vec<RecentProjectEntry>, cx: &App) {
        self.recent_project_thread_titles.clear();
        if let Some(map) = read_thread_title_map() {
            for entry in &recent_projects {
                let path_key = sorted_paths_key(&entry.paths);
                if let Some(title) = map.get(&path_key) {
                    self.recent_project_thread_titles
                        .insert(entry.full_path.clone(), title.clone().into());
                }
            }
        }

        self.recent_projects = recent_projects;

        let workspace_threads: Vec<WorkspaceThreadEntry> = self
            .entries
            .iter()
            .filter_map(|entry| match entry {
                SidebarEntry::WorkspaceThread(thread) => Some(thread.clone()),
                _ => None,
            })
            .collect();
        self.rebuild_entries(workspace_threads, cx);
    }

    fn has_notifications(&self) -> bool {
        if !self.notified_workspaces.is_empty() {
            return true;
        }

        self.entries.iter().any(|entry| match entry {
            SidebarEntry::WorkspaceThread(thread) => thread
                .thread_info
                .as_ref()
                .is_some_and(|info| info.needs_attention),
            _ => false,
        })
    }

    fn open_workspace_path_sets(&self, cx: &App) -> Vec<Vec<Arc<Path>>> {
        self.multi_workspace
            .read(cx)
            .workspaces()
            .iter()
            .map(|workspace| {
                let mut paths = workspace.read(cx).root_paths(cx);
                paths.sort();
                paths
            })
            .collect()
    }

    fn rebuild_entries(&mut self, workspace_threads: Vec<WorkspaceThreadEntry>, cx: &App) {
        let open_path_sets = self.open_workspace_path_sets(cx);

        self.entries.clear();

        if !workspace_threads.is_empty() {
            self.entries
                .push(SidebarEntry::Separator("Active Workspaces".into()));
            for thread in workspace_threads {
                self.entries.push(SidebarEntry::WorkspaceThread(thread));
            }
        }

        let recent: Vec<_> = self
            .recent_projects
            .iter()
            .filter(|project| {
                let mut project_paths: Vec<&Path> =
                    project.paths.iter().map(|p| p.as_path()).collect();
                project_paths.sort();
                !open_path_sets.iter().any(|open_paths| {
                    open_paths.len() == project_paths.len()
                        && open_paths
                            .iter()
                            .zip(&project_paths)
                            .all(|(a, b)| a.as_ref() == *b)
                })
            })
            .cloned()
            .collect();

        if !recent.is_empty() {
            let today = Local::now().naive_local().date();
            let mut current_bucket: Option<TimeBucket> = None;

            for project in recent {
                let entry_date = project.timestamp.with_timezone(&Local).naive_local().date();
                let bucket = TimeBucket::from_dates(today, entry_date);

                if current_bucket != Some(bucket) {
                    current_bucket = Some(bucket);
                    self.entries
                        .push(SidebarEntry::Separator(bucket.to_string().into()));
                }

                self.entries.push(SidebarEntry::RecentProject(project));
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TimeBucket {
    Today,
    Yesterday,
    ThisWeek,
    PastWeek,
    All,
}

impl TimeBucket {
    fn from_dates(reference: NaiveDate, date: NaiveDate) -> Self {
        if date == reference {
            return TimeBucket::Today;
        }

        if date == reference - TimeDelta::days(1) {
            return TimeBucket::Yesterday;
        }

        let week = date.iso_week();

        if reference.iso_week() == week {
            return TimeBucket::ThisWeek;
        }

        let last_week = (reference - TimeDelta::days(7)).iso_week();

        if week == last_week {
            return TimeBucket::PastWeek;
        }

        TimeBucket::All
    }
}

impl Display for TimeBucket {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TimeBucket::Today => write!(f, "Today"),
            TimeBucket::Yesterday => write!(f, "Yesterday"),
            TimeBucket::ThisWeek => write!(f, "This Week"),
            TimeBucket::PastWeek => write!(f, "Past Week"),
            TimeBucket::All => write!(f, "All"),
        }
    }
}

fn open_recent_project(paths: Vec<PathBuf>, window: &mut Window, cx: &mut App) {
    let Some(handle) = window.window_handle().downcast::<MultiWorkspace>() else {
        return;
    };

    cx.defer(move |cx| {
        if let Some(task) = handle
            .update(cx, |multi_workspace, window, cx| {
                multi_workspace.open_project(paths, window, cx)
            })
            .log_err()
        {
            task.detach_and_log_err(cx);
        }
    });
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum WorktreeAddMode {
    CreateBranch,
    CheckoutBranch,
}

impl WorktreeAddMode {
    fn label(self) -> &'static str {
        match self {
            Self::CreateBranch => "Create",
            Self::CheckoutBranch => "Checkout",
        }
    }
}

fn workspace_path_key_for_index(
    multi_workspace: &Entity<MultiWorkspace>,
    workspace_index: usize,
    cx: &App,
) -> Option<String> {
    let workspace = multi_workspace
        .read(cx)
        .workspaces()
        .get(workspace_index)?
        .clone();
    let paths: Vec<_> = workspace
        .read(cx)
        .worktrees(cx)
        .filter(|worktree| worktree.read(cx).is_visible())
        .map(|worktree| worktree.read(cx).abs_path())
        .collect();
    if paths.is_empty() {
        None
    } else {
        Some(sorted_paths_key(&paths))
    }
}

fn workspace_git_defaults_for_index(
    multi_workspace: &Entity<MultiWorkspace>,
    workspace_index: usize,
    cx: &App,
) -> (String, String) {
    let Some(workspace) = multi_workspace
        .read(cx)
        .workspaces()
        .get(workspace_index)
        .cloned()
    else {
        return ("worktree".to_string(), "../WT_worktree".to_string());
    };
    let project = workspace.read(cx).project().clone();
    let git_store = project.read(cx).git_store().clone();
    let branch_name = {
        let git_store = git_store.read(cx);
        git_store
            .active_repository()
            .or_else(|| git_store.repositories().values().next().cloned())
            .and_then(|repo| {
                repo.read(cx)
                    .branch
                    .as_ref()
                    .map(|branch| branch.name().to_string())
            })
            .unwrap_or_else(|| "worktree".to_string())
    };

    let sanitized_branch = branch_name
        .chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
                ch
            } else {
                '_'
            }
        })
        .collect::<String>();
    let suffix = if sanitized_branch.is_empty() {
        "worktree"
    } else {
        sanitized_branch.as_str()
    };

    (branch_name, format!("../WT_{suffix}"))
}

fn selected_workspace_repo_root_for_index(
    multi_workspace: &MultiWorkspace,
    workspace_index: usize,
    cx: &App,
) -> Result<(Entity<Workspace>, Arc<Path>, bool)> {
    let workspace = multi_workspace
        .workspaces()
        .get(workspace_index)
        .cloned()
        .ok_or_else(|| anyhow!("Workspace no longer exists"))?;
    let project = workspace.read(cx).project().clone();
    let is_local = project.read(cx).is_local();
    let git_store = project.read(cx).git_store().clone();
    let repo = {
        let git_store = git_store.read(cx);
        git_store
            .active_repository()
            .or_else(|| git_store.repositories().values().next().cloned())
    }
    .ok_or_else(|| anyhow!("No git repository found for this workspace"))?;
    let repo_root = repo.read(cx).work_directory_abs_path.clone();
    Ok((workspace, repo_root, is_local))
}

struct EditWorkspaceNameModal {
    multi_workspace: WeakEntity<MultiWorkspace>,
    workspace_index: usize,
    editor: Entity<Editor>,
}

impl EditWorkspaceNameModal {
    fn new(
        multi_workspace: WeakEntity<MultiWorkspace>,
        workspace_index: usize,
        initial_name: String,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) -> Self {
        let editor = cx.new(|cx| {
            let mut editor = Editor::single_line(window, cx);
            editor.set_placeholder_text("Workspace name", window, cx);
            editor.set_text(initial_name, window, cx);
            editor
        });
        Self {
            multi_workspace,
            workspace_index,
            editor,
        }
    }

    fn cancel(&mut self, _: &menu::Cancel, _: &mut Window, cx: &mut Context<Self>) {
        cx.emit(DismissEvent);
    }

    fn confirm(&mut self, _: &menu::Confirm, window: &mut Window, cx: &mut Context<Self>) {
        let name = self.editor.read(cx).text(cx).trim().to_string();
        if name.is_empty() {
            let _ = window.prompt(
                gpui::PromptLevel::Info,
                "Name required",
                Some("Enter a workspace name."),
                &["Ok"],
                cx,
            );
            return;
        }

        let Some(multi_workspace) = self.multi_workspace.upgrade() else {
            cx.emit(DismissEvent);
            return;
        };
        let Some(path_key) =
            workspace_path_key_for_index(&multi_workspace, self.workspace_index, cx)
        else {
            let _ = window.prompt(
                gpui::PromptLevel::Info,
                "Cannot rename workspace",
                Some("This workspace does not have a visible path yet."),
                &["Ok"],
                cx,
            );
            return;
        };

        let task = cx.spawn(async move |_, cx| {
            let mut names = read_workspace_display_name_map().unwrap_or_default();
            names.insert(path_key, name);
            let json = serde_json::to_string(&names)?;
            KEY_VALUE_STORE
                .write_kvp(WORKSPACE_DISPLAY_NAMES_KEY.into(), json)
                .await?;
            multi_workspace.update(cx, |_, cx| cx.notify());
            anyhow::Ok(())
        });
        task.detach_and_prompt_err("Failed to save workspace name", window, cx, |e, _, _| {
            Some(e.to_string())
        });

        cx.emit(DismissEvent);
    }
}

impl EventEmitter<DismissEvent> for EditWorkspaceNameModal {}
impl ModalView for EditWorkspaceNameModal {}
impl Focusable for EditWorkspaceNameModal {
    fn focus_handle(&self, cx: &App) -> FocusHandle {
        self.editor.focus_handle(cx)
    }
}

impl Render for EditWorkspaceNameModal {
    fn render(&mut self, _window: &mut Window, cx: &mut Context<Self>) -> impl IntoElement {
        v_flex()
            .key_context("WorkspaceEditNameModal")
            .on_action(cx.listener(Self::cancel))
            .on_action(cx.listener(Self::confirm))
            .w(px(440.0))
            .elevation_2(cx)
            .rounded_md()
            .bg(cx.theme().colors().surface_background)
            .border_1()
            .border_color(cx.theme().colors().border_variant)
            .child(
                v_flex()
                    .p_3()
                    .gap_2()
                    .child(Label::new("Edit Workspace Name"))
                    .child(
                        Label::new("Set a custom display name for this workspace.")
                            .size(LabelSize::Small)
                            .color(Color::Muted),
                    )
                    .child(self.editor.clone())
                    .child(
                        h_flex()
                            .justify_end()
                            .gap_2()
                            .child(
                                Button::new("cancel-edit-workspace-name", "Cancel")
                                    .style(ButtonStyle::Subtle)
                                    .on_click(cx.listener(|this, _, window, cx| {
                                        this.cancel(&menu::Cancel, window, cx);
                                    })),
                            )
                            .child(Button::new("confirm-edit-workspace-name", "Save").on_click(
                                cx.listener(|this, _, window, cx| {
                                    this.confirm(&menu::Confirm, window, cx);
                                }),
                            )),
                    ),
            )
    }
}

struct AddWorkspaceWorktreeModal {
    multi_workspace: WeakEntity<MultiWorkspace>,
    workspace_index: usize,
    mode: WorktreeAddMode,
    branch_editor: Entity<Editor>,
    path_editor: Entity<Editor>,
}

impl AddWorkspaceWorktreeModal {
    fn new(
        multi_workspace: WeakEntity<MultiWorkspace>,
        workspace_index: usize,
        default_branch: String,
        default_path: String,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) -> Self {
        let branch_editor = cx.new(|cx| {
            let mut editor = Editor::single_line(window, cx);
            editor.set_placeholder_text("Branch name", window, cx);
            editor.set_text(default_branch, window, cx);
            editor
        });
        let path_editor = cx.new(|cx| {
            let mut editor = Editor::single_line(window, cx);
            editor.set_placeholder_text("Path (relative or absolute)", window, cx);
            editor.set_text(default_path, window, cx);
            editor
        });

        Self {
            multi_workspace,
            workspace_index,
            mode: WorktreeAddMode::CreateBranch,
            branch_editor,
            path_editor,
        }
    }

    fn cancel(&mut self, _: &menu::Cancel, _: &mut Window, cx: &mut Context<Self>) {
        cx.emit(DismissEvent);
    }

    fn confirm(&mut self, _: &menu::Confirm, window: &mut Window, cx: &mut Context<Self>) {
        let branch_name = self.branch_editor.read(cx).text(cx).trim().to_string();
        let path_input = self.path_editor.read(cx).text(cx).trim().to_string();

        if branch_name.is_empty() || path_input.is_empty() {
            let _ = window.prompt(
                gpui::PromptLevel::Info,
                "Missing worktree inputs",
                Some("Enter both a branch name and a path."),
                &["Ok"],
                cx,
            );
            return;
        }

        let mode = self.mode;
        let workspace_index = self.workspace_index;
        let Some(multi_workspace) = self.multi_workspace.upgrade() else {
            cx.emit(DismissEvent);
            return;
        };

        let task = cx.spawn_in(window, async move |_, cx| {
            let (_workspace, repo_root, is_local) =
                multi_workspace.read_with(cx, |multi_workspace, cx| {
                    selected_workspace_repo_root_for_index(multi_workspace, workspace_index, cx)
                })?;

            if !is_local {
                anyhow::bail!("Worktree Add is only supported for local workspaces");
            }

            let requested_path = PathBuf::from(path_input);
            let target_path = if requested_path.is_absolute() {
                requested_path
            } else {
                repo_root.join(requested_path)
            };

            let repo_root_for_command = repo_root.clone();
            let branch_for_command = branch_name.clone();
            let target_path_for_command = target_path.clone();

            cx.background_spawn(async move {
                if let Some(parent) = target_path_for_command.parent() {
                    std::fs::create_dir_all(parent)?;
                }

                let mut command = Command::new("git");
                command
                    .current_dir(repo_root_for_command.as_ref())
                    .arg("--no-optional-locks")
                    .arg("worktree")
                    .arg("add");

                match mode {
                    WorktreeAddMode::CreateBranch => {
                        command
                            .arg("-b")
                            .arg(&branch_for_command)
                            .arg("--")
                            .arg(&target_path_for_command)
                            .arg("HEAD");
                    }
                    WorktreeAddMode::CheckoutBranch => {
                        command
                            .arg("--")
                            .arg(&target_path_for_command)
                            .arg(&branch_for_command);
                    }
                }

                let output = command.output()?;
                if output.status.success() {
                    Ok::<(), anyhow::Error>(())
                } else {
                    let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
                    anyhow::bail!(
                        "git worktree add failed{}",
                        if stderr.is_empty() {
                            String::new()
                        } else {
                            format!(": {stderr}")
                        }
                    );
                }
            })
            .await?;

            multi_workspace
                .update_in(cx, |multi_workspace, window, cx| {
                    multi_workspace.open_project(vec![target_path], window, cx)
                })?
                .await?;
            multi_workspace.update(cx, |_, cx| cx.notify());
            anyhow::Ok(())
        });

        task.detach_and_prompt_err("Failed to add worktree", window, cx, |e, _, _| {
            Some(e.to_string())
        });

        cx.emit(DismissEvent);
    }
}

impl EventEmitter<DismissEvent> for AddWorkspaceWorktreeModal {}
impl ModalView for AddWorkspaceWorktreeModal {}
impl Focusable for AddWorkspaceWorktreeModal {
    fn focus_handle(&self, cx: &App) -> FocusHandle {
        self.branch_editor.focus_handle(cx)
    }
}

impl Render for AddWorkspaceWorktreeModal {
    fn render(&mut self, _window: &mut Window, cx: &mut Context<Self>) -> impl IntoElement {
        let create_selected = self.mode == WorktreeAddMode::CreateBranch;
        let checkout_selected = self.mode == WorktreeAddMode::CheckoutBranch;

        v_flex()
            .key_context("WorkspaceAddWorktreeModal")
            .on_action(cx.listener(Self::cancel))
            .on_action(cx.listener(Self::confirm))
            .w(px(520.0))
            .elevation_2(cx)
            .rounded_md()
            .bg(cx.theme().colors().surface_background)
            .border_1()
            .border_color(cx.theme().colors().border_variant)
            .child(
                v_flex()
                    .p_3()
                    .gap_2()
                    .child(Label::new("Worktree Add"))
                    .child(
                        Label::new("Choose whether to create a branch or checkout an existing branch, then pick the worktree path.")
                            .size(LabelSize::Small)
                            .color(Color::Muted),
                    )
                    .child(
                        h_flex()
                            .gap_2()
                            .child(
                                Button::new("worktree-mode-create", "Create")
                                    .style(if create_selected {
                                        ButtonStyle::Tinted(ui::TintColor::Accent)
                                    } else {
                                        ButtonStyle::Subtle
                                    })
                                    .on_click(cx.listener(|this, _, _, cx| {
                                        this.mode = WorktreeAddMode::CreateBranch;
                                        cx.notify();
                                    })),
                            )
                            .child(
                                Button::new("worktree-mode-checkout", "Checkout")
                                    .style(if checkout_selected {
                                        ButtonStyle::Tinted(ui::TintColor::Accent)
                                    } else {
                                        ButtonStyle::Subtle
                                    })
                                    .on_click(cx.listener(|this, _, _, cx| {
                                        this.mode = WorktreeAddMode::CheckoutBranch;
                                        cx.notify();
                                    })),
                            )
                            .child(
                                Label::new(format!("Mode: {}", self.mode.label()))
                                    .size(LabelSize::Small)
                                    .color(Color::Muted),
                            ),
                    )
                    .child(Label::new("Branch").size(LabelSize::Small).color(Color::Muted))
                    .child(self.branch_editor.clone())
                    .child(Label::new("Path").size(LabelSize::Small).color(Color::Muted))
                    .child(self.path_editor.clone())
                    .child(
                        h_flex()
                            .justify_end()
                            .gap_2()
                            .child(
                                Button::new("cancel-add-worktree", "Cancel")
                                    .style(ButtonStyle::Subtle)
                                    .on_click(cx.listener(|this, _, window, cx| {
                                        this.cancel(&menu::Cancel, window, cx);
                                    })),
                            )
                            .child(
                                Button::new("confirm-add-worktree", "Add Worktree")
                                    .on_click(cx.listener(|this, _, window, cx| {
                                        this.confirm(&menu::Confirm, window, cx);
                                    })),
                            ),
                    ),
            )
    }
}

impl PickerDelegate for WorkspacePickerDelegate {
    type ListItem = AnyElement;

    fn match_count(&self) -> usize {
        self.matches.len()
    }

    fn selected_index(&self) -> usize {
        self.selected_index
    }

    fn set_selected_index(
        &mut self,
        ix: usize,
        _window: &mut Window,
        _cx: &mut Context<Picker<Self>>,
    ) {
        self.selected_index = ix;
    }

    fn can_select(
        &mut self,
        ix: usize,
        _window: &mut Window,
        _cx: &mut Context<Picker<Self>>,
    ) -> bool {
        match self.matches.get(ix) {
            Some(SidebarMatch {
                entry: SidebarEntry::Separator(_),
                ..
            }) => false,
            _ => true,
        }
    }

    fn placeholder_text(&self, _window: &mut Window, _cx: &mut App) -> Arc<str> {
        "Search…".into()
    }

    fn no_matches_text(&self, _window: &mut Window, _cx: &mut App) -> Option<SharedString> {
        if self.query.is_empty() {
            None
        } else {
            Some("No threads match your search.".into())
        }
    }

    fn update_matches(
        &mut self,
        query: String,
        window: &mut Window,
        cx: &mut Context<Picker<Self>>,
    ) -> Task<()> {
        let query_changed = self.query != query;
        self.query = query.clone();
        if query_changed {
            self.hovered_thread_item = None;
        }
        let entries = self.entries.clone();

        if query.is_empty() {
            self.matches = entries
                .into_iter()
                .map(|entry| SidebarMatch {
                    entry,
                    positions: Vec::new(),
                })
                .collect();

            let separator_offset = if self.workspace_thread_count > 0 {
                1
            } else {
                0
            };
            self.selected_index = (self.active_workspace_index + separator_offset)
                .min(self.matches.len().saturating_sub(1));
            return Task::ready(());
        }

        let executor = cx.background_executor().clone();
        cx.spawn_in(window, async move |picker, cx| {
            let matches = cx
                .background_spawn(async move {
                    let data_entries: Vec<(usize, &SidebarEntry)> = entries
                        .iter()
                        .enumerate()
                        .filter(|(_, entry)| !matches!(entry, SidebarEntry::Separator(_)))
                        .collect();

                    let candidates: Vec<StringMatchCandidate> = data_entries
                        .iter()
                        .enumerate()
                        .map(|(candidate_index, (_, entry))| {
                            StringMatchCandidate::new(candidate_index, entry.searchable_text())
                        })
                        .collect();

                    let search_matches = fuzzy::match_strings(
                        &candidates,
                        &query,
                        false,
                        true,
                        MAX_MATCHES,
                        &Default::default(),
                        executor,
                    )
                    .await;

                    let mut workspace_matches = Vec::new();
                    let mut project_matches = Vec::new();

                    for search_match in search_matches {
                        let (original_index, _) = data_entries[search_match.candidate_id];
                        let entry = entries[original_index].clone();
                        let sidebar_match = SidebarMatch {
                            positions: search_match.positions,
                            entry: entry.clone(),
                        };
                        match entry {
                            SidebarEntry::WorkspaceThread(_) => {
                                workspace_matches.push(sidebar_match)
                            }
                            SidebarEntry::RecentProject(_) => project_matches.push(sidebar_match),
                            SidebarEntry::Separator(_) => {}
                        }
                    }

                    let mut result = Vec::new();
                    if !workspace_matches.is_empty() {
                        result.push(SidebarMatch {
                            entry: SidebarEntry::Separator("Active Workspaces".into()),
                            positions: Vec::new(),
                        });
                        result.extend(workspace_matches);
                    }
                    if !project_matches.is_empty() {
                        result.push(SidebarMatch {
                            entry: SidebarEntry::Separator("Recent Projects".into()),
                            positions: Vec::new(),
                        });
                        result.extend(project_matches);
                    }
                    result
                })
                .await;

            picker
                .update_in(cx, |picker, _window, _cx| {
                    picker.delegate.matches = matches;
                    if picker.delegate.matches.is_empty() {
                        picker.delegate.selected_index = 0;
                    } else {
                        let first_selectable = picker
                            .delegate
                            .matches
                            .iter()
                            .position(|m| !matches!(m.entry, SidebarEntry::Separator(_)))
                            .unwrap_or(0);
                        picker.delegate.selected_index = first_selectable;
                    }
                })
                .log_err();
        })
    }

    fn confirm(&mut self, _secondary: bool, window: &mut Window, cx: &mut Context<Picker<Self>>) {
        let Some(selected_match) = self.matches.get(self.selected_index) else {
            return;
        };

        match &selected_match.entry {
            SidebarEntry::Separator(_) => {}
            SidebarEntry::WorkspaceThread(thread_entry) => {
                let target_index = thread_entry.index;
                self.multi_workspace.update(cx, |multi_workspace, cx| {
                    multi_workspace.activate_index(target_index, window, cx);
                });
            }
            SidebarEntry::RecentProject(project_entry) => {
                let paths = project_entry.paths.clone();
                open_recent_project(paths, window, cx);
            }
        }
    }

    fn dismissed(&mut self, _window: &mut Window, _cx: &mut Context<Picker<Self>>) {}

    fn render_match(
        &self,
        index: usize,
        selected: bool,
        _window: &mut Window,
        cx: &mut Context<Picker<Self>>,
    ) -> Option<Self::ListItem> {
        let match_entry = self.matches.get(index)?;
        let SidebarMatch { entry, positions } = match_entry;

        match entry {
            SidebarEntry::Separator(title) => Some(
                v_flex()
                    .when(index > 0, |this| {
                        this.mt_1()
                            .gap_2()
                            .child(Divider::horizontal().color(DividerColor::BorderFaded))
                    })
                    .child(ListSubHeader::new(title.clone()).inset(true))
                    .into_any_element(),
            ),
            SidebarEntry::WorkspaceThread(thread_entry) => {
                let worktree_label = thread_entry.worktree_label.clone();
                let full_path = thread_entry.full_path.clone();
                let thread_info = thread_entry.thread_info.clone();
                let custom_name = thread_entry.custom_name.clone();
                let workspace_index = thread_entry.index;
                let multi_workspace = self.multi_workspace.clone();
                let multi_workspace_for_remove = multi_workspace.clone();
                let workspace_count = self.multi_workspace.read(cx).workspaces().len();
                let is_hovered = self.hovered_thread_item == Some(workspace_index);

                let remove_btn = IconButton::new(
                    format!("remove-workspace-{}", workspace_index),
                    IconName::Close,
                )
                .icon_size(IconSize::Small)
                .icon_color(Color::Muted)
                .tooltip(Tooltip::text("Remove Workspace"))
                .on_click({
                    let multi_workspace = multi_workspace_for_remove;
                    move |_, window, cx| {
                        multi_workspace.update(cx, |mw, cx| {
                            mw.remove_workspace(workspace_index, window, cx);
                        });
                    }
                });

                let has_notification = self.notified_workspaces.contains(&workspace_index)
                    || thread_info
                        .as_ref()
                        .is_some_and(|info| info.needs_attention);
                let thread_subtitle = thread_info.as_ref().map(|info| info.title.clone());
                let display_title = custom_name
                    .clone()
                    .or_else(|| thread_subtitle.clone())
                    .unwrap_or_else(|| SharedString::from("New Thread"));
                let status = thread_info
                    .as_ref()
                    .map_or(AgentThreadStatus::default(), |info| info.status);
                let running = matches!(
                    status,
                    AgentThreadStatus::Running | AgentThreadStatus::WaitingForConfirmation
                );

                let menu_button = PopoverMenu::new(format!("workspace-row-menu-{workspace_index}"))
                    .trigger(
                        IconButton::new(
                            format!("workspace-actions-{}", workspace_index),
                            IconName::Ellipsis,
                        )
                        .icon_size(IconSize::Small)
                        .icon_color(Color::Muted)
                        .tooltip(Tooltip::text("Workspace Actions")),
                    )
                    .menu({
                        let multi_workspace = multi_workspace.clone();
                        let edit_name_default = display_title.to_string();
                        move |window, cx| {
                            let multi_workspace = multi_workspace.clone();
                            let edit_name_default = edit_name_default.clone();
                            Some(ContextMenu::build(window, cx, move |menu, _, _| {
                                menu.entry("Worktree Add", None, {
                                    let multi_workspace = multi_workspace.clone();
                                    move |window, cx| {
                                        let (default_branch, default_path) =
                                            workspace_git_defaults_for_index(
                                                &multi_workspace,
                                                workspace_index,
                                                cx,
                                            );
                                        multi_workspace.update(cx, |multi_workspace, cx| {
                                            let multi_workspace_handle = cx.weak_entity();
                                            multi_workspace.toggle_modal(
                                                window,
                                                cx,
                                                |window, cx| {
                                                    AddWorkspaceWorktreeModal::new(
                                                        multi_workspace_handle,
                                                        workspace_index,
                                                        default_branch.clone(),
                                                        default_path.clone(),
                                                        window,
                                                        cx,
                                                    )
                                                },
                                            );
                                        });
                                    }
                                })
                                .entry("Edit Name", None, {
                                    let multi_workspace = multi_workspace.clone();
                                    let initial_name = edit_name_default.clone();
                                    move |window, cx| {
                                        multi_workspace.update(cx, |multi_workspace, cx| {
                                            let multi_workspace_handle = cx.weak_entity();
                                            multi_workspace.toggle_modal(
                                                window,
                                                cx,
                                                |window, cx| {
                                                    EditWorkspaceNameModal::new(
                                                        multi_workspace_handle,
                                                        workspace_index,
                                                        initial_name.clone(),
                                                        window,
                                                        cx,
                                                    )
                                                },
                                            );
                                        });
                                    }
                                })
                            }))
                        }
                    });

                let action_slot = h_flex()
                    .gap_1()
                    .child(menu_button)
                    .when(workspace_count > 1, |this| this.child(remove_btn));

                Some(
                    ThreadItem::new(("workspace-item", thread_entry.index), display_title)
                        .icon(
                            thread_info
                                .as_ref()
                                .map_or(IconName::ZedAgent, |info| info.icon),
                        )
                        .running(running)
                        .generation_done(has_notification)
                        .status(status)
                        .selected(selected)
                        .worktree(worktree_label.clone())
                        .worktree_highlight_positions(positions.clone())
                        .action_slot(action_slot)
                        .action_slot_always_visible(true)
                        .hovered(is_hovered)
                        .on_hover(cx.listener(move |picker, is_hovered, _window, cx| {
                            let mut changed = false;
                            if *is_hovered {
                                if picker.delegate.hovered_thread_item != Some(workspace_index) {
                                    picker.delegate.hovered_thread_item = Some(workspace_index);
                                    changed = true;
                                }
                            } else if picker.delegate.hovered_thread_item == Some(workspace_index) {
                                picker.delegate.hovered_thread_item = None;
                                changed = true;
                            }
                            if changed {
                                cx.notify();
                            }
                        }))
                        .when(!full_path.is_empty(), |this| {
                            this.tooltip(move |_, cx| {
                                Tooltip::with_meta(
                                    worktree_label.clone(),
                                    None,
                                    full_path.clone(),
                                    cx,
                                )
                            })
                        })
                        .into_any_element(),
                )
            }
            SidebarEntry::RecentProject(project_entry) => {
                let name = project_entry.name.clone();
                let full_path = project_entry.full_path.clone();
                let item_id: SharedString =
                    format!("recent-project-{:?}", project_entry.workspace_id).into();

                Some(
                    ThreadItem::new(item_id, name.clone())
                        .icon(IconName::Folder)
                        .selected(selected)
                        .highlight_positions(positions.clone())
                        .tooltip(move |_, cx| {
                            Tooltip::with_meta(name.clone(), None, full_path.clone(), cx)
                        })
                        .into_any_element(),
                )
            }
        }
    }

    fn render_editor(
        &self,
        editor: &Arc<dyn ErasedEditor>,
        window: &mut Window,
        cx: &mut Context<Picker<Self>>,
    ) -> Div {
        h_flex()
            .h(Tab::container_height(cx))
            .w_full()
            .px_2()
            .gap_2()
            .justify_between()
            .border_b_1()
            .border_color(cx.theme().colors().border)
            .child(
                Icon::new(IconName::MagnifyingGlass)
                    .color(Color::Muted)
                    .size(IconSize::Small),
            )
            .child(editor.render(window, cx))
    }
}

pub struct Sidebar {
    multi_workspace: Entity<MultiWorkspace>,
    width: Pixels,
    picker: Entity<Picker<WorkspacePickerDelegate>>,
    _subscription: Subscription,
    _project_subscriptions: Vec<Subscription>,
    _agent_panel_subscriptions: Vec<Subscription>,
    _thread_subscriptions: Vec<Subscription>,
    #[cfg(any(test, feature = "test-support"))]
    test_thread_infos: HashMap<usize, AgentThreadInfo>,
    #[cfg(any(test, feature = "test-support"))]
    test_recent_project_thread_titles: HashMap<SharedString, SharedString>,
    _fetch_recent_projects: Task<()>,
}

impl EventEmitter<SidebarEvent> for Sidebar {}

impl Sidebar {
    pub fn new(
        multi_workspace: Entity<MultiWorkspace>,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) -> Self {
        let delegate = WorkspacePickerDelegate::new(multi_workspace.clone());
        let picker = cx.new(|cx| {
            Picker::list(delegate, window, cx)
                .max_height(None)
                .show_scrollbar(true)
                .modal(false)
        });

        let subscription = cx.observe_in(
            &multi_workspace,
            window,
            |this, multi_workspace, window, cx| {
                this.queue_refresh(multi_workspace, window, cx);
            },
        );

        let fetch_recent_projects = {
            let picker = picker.downgrade();
            let fs = <dyn Fs>::global(cx);
            cx.spawn_in(window, async move |_this, cx| {
                let projects = get_recent_projects(None, None, fs).await;

                cx.update(|window, cx| {
                    if let Some(picker) = picker.upgrade() {
                        picker.update(cx, |picker, cx| {
                            picker.delegate.set_recent_projects(projects, cx);
                            let query = picker.query(cx);
                            picker.update_matches(query, window, cx);
                        });
                    }
                })
                .log_err();
            })
        };

        let mut this = Self {
            multi_workspace,
            width: DEFAULT_WIDTH,
            picker,
            _subscription: subscription,
            _project_subscriptions: Vec::new(),
            _agent_panel_subscriptions: Vec::new(),
            _thread_subscriptions: Vec::new(),
            #[cfg(any(test, feature = "test-support"))]
            test_thread_infos: HashMap::new(),
            #[cfg(any(test, feature = "test-support"))]
            test_recent_project_thread_titles: HashMap::new(),
            _fetch_recent_projects: fetch_recent_projects,
        };
        this.queue_refresh(this.multi_workspace.clone(), window, cx);
        this
    }

    fn subscribe_to_projects(
        &mut self,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) -> Vec<Subscription> {
        let projects: Vec<_> = self
            .multi_workspace
            .read(cx)
            .workspaces()
            .iter()
            .map(|w| w.read(cx).project().clone())
            .collect();

        projects
            .iter()
            .map(|project| {
                cx.subscribe_in(
                    project,
                    window,
                    |this, _project, event, window, cx| match event {
                        ProjectEvent::WorktreeAdded(_)
                        | ProjectEvent::WorktreeRemoved(_)
                        | ProjectEvent::WorktreeOrderChanged => {
                            this.queue_refresh(this.multi_workspace.clone(), window, cx);
                        }
                        _ => {}
                    },
                )
            })
            .collect()
    }

    fn build_workspace_thread_entries(
        &self,
        multi_workspace: &MultiWorkspace,
        cx: &App,
    ) -> (Vec<WorkspaceThreadEntry>, usize) {
        let persisted_titles = read_thread_title_map().unwrap_or_default();
        let persisted_workspace_names = read_workspace_display_name_map().unwrap_or_default();

        #[allow(unused_mut)]
        let mut entries: Vec<WorkspaceThreadEntry> = multi_workspace
            .workspaces()
            .iter()
            .enumerate()
            .map(|(index, workspace)| {
                WorkspaceThreadEntry::new(
                    index,
                    workspace,
                    &persisted_titles,
                    &persisted_workspace_names,
                    cx,
                )
            })
            .collect();

        #[cfg(any(test, feature = "test-support"))]
        for (index, info) in &self.test_thread_infos {
            if let Some(entry) = entries.get_mut(*index) {
                entry.thread_info = Some(info.clone());
            }
        }

        (entries, multi_workspace.active_workspace_index())
    }

    #[cfg(any(test, feature = "test-support"))]
    pub fn set_test_recent_projects(
        &self,
        projects: Vec<RecentProjectEntry>,
        cx: &mut Context<Self>,
    ) {
        self.picker.update(cx, |picker, _cx| {
            picker.delegate.recent_projects = projects;
        });
    }

    #[cfg(any(test, feature = "test-support"))]
    pub fn set_test_thread_info(
        &mut self,
        index: usize,
        title: SharedString,
        status: AgentThreadStatus,
    ) {
        self.test_thread_infos.insert(
            index,
            AgentThreadInfo {
                title,
                status,
                icon: IconName::ZedAgent,
                needs_attention: false,
            },
        );
    }

    #[cfg(any(test, feature = "test-support"))]
    pub fn set_test_recent_project_thread_title(
        &mut self,
        full_path: SharedString,
        title: SharedString,
        cx: &mut Context<Self>,
    ) {
        self.test_recent_project_thread_titles
            .insert(full_path.clone(), title.clone());
        self.picker.update(cx, |picker, _cx| {
            picker
                .delegate
                .recent_project_thread_titles
                .insert(full_path, title);
        });
    }

    fn subscribe_to_agent_panels(
        &mut self,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) -> Vec<Subscription> {
        let workspaces: Vec<_> = self.multi_workspace.read(cx).workspaces().to_vec();

        workspaces
            .iter()
            .map(|workspace| {
                if let Some(agent_panel) = workspace.read(cx).panel::<AgentPanel>(cx) {
                    cx.subscribe_in(
                        &agent_panel,
                        window,
                        |this, _, _event: &AgentPanelEvent, window, cx| {
                            this.queue_refresh(this.multi_workspace.clone(), window, cx);
                        },
                    )
                } else {
                    // Panel hasn't loaded yet — observe the workspace so we
                    // re-subscribe once the panel appears on its dock.
                    cx.observe_in(workspace, window, |this, _, window, cx| {
                        this.queue_refresh(this.multi_workspace.clone(), window, cx);
                    })
                }
            })
            .collect()
    }

    fn subscribe_to_threads(
        &mut self,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) -> Vec<Subscription> {
        let workspaces: Vec<_> = self.multi_workspace.read(cx).workspaces().to_vec();

        workspaces
            .iter()
            .filter_map(|workspace| {
                let agent_panel = workspace.read(cx).panel::<AgentPanel>(cx)?;
                let thread = agent_panel.read(cx).active_agent_thread(cx)?;
                Some(cx.observe_in(&thread, window, |this, _, window, cx| {
                    this.queue_refresh(this.multi_workspace.clone(), window, cx);
                }))
            })
            .collect()
    }

    fn persist_thread_titles(
        &self,
        entries: &[WorkspaceThreadEntry],
        multi_workspace: &Entity<MultiWorkspace>,
        cx: &mut Context<Self>,
    ) {
        let mut map = read_thread_title_map().unwrap_or_default();
        let workspaces = multi_workspace.read(cx).workspaces().to_vec();
        let mut changed = false;

        for (workspace, entry) in workspaces.iter().zip(entries.iter()) {
            if let Some(ref info) = entry.thread_info {
                let paths: Vec<_> = workspace
                    .read(cx)
                    .worktrees(cx)
                    .map(|wt| wt.read(cx).abs_path())
                    .collect();
                if paths.is_empty() {
                    continue;
                }
                let path_key = sorted_paths_key(&paths);
                let title = info.title.to_string();
                if map.get(&path_key) != Some(&title) {
                    map.insert(path_key, title);
                    changed = true;
                }
            }
        }

        if changed {
            if let Some(json) = serde_json::to_string(&map).log_err() {
                cx.background_spawn(async move {
                    KEY_VALUE_STORE
                        .write_kvp(LAST_THREAD_TITLES_KEY.into(), json)
                        .await
                        .log_err();
                })
                .detach();
            }
        }
    }

    fn queue_refresh(
        &mut self,
        multi_workspace: Entity<MultiWorkspace>,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        cx.defer_in(window, move |this, window, cx| {
            if !this.multi_workspace.read(cx).multi_workspace_enabled(cx) {
                return;
            }

            this._project_subscriptions = this.subscribe_to_projects(window, cx);
            this._agent_panel_subscriptions = this.subscribe_to_agent_panels(window, cx);
            this._thread_subscriptions = this.subscribe_to_threads(window, cx);
            let (entries, active_index) = multi_workspace.read_with(cx, |multi_workspace, cx| {
                this.build_workspace_thread_entries(multi_workspace, cx)
            });

            this.persist_thread_titles(&entries, &multi_workspace, cx);

            let had_notifications = this.picker.read(cx).delegate.has_notifications();
            this.picker.update(cx, |picker, cx| {
                picker.delegate.set_entries(entries, active_index, cx);
                let query = picker.query(cx);
                picker.update_matches(query, window, cx);
            });
            let has_notifications = this.picker.read(cx).delegate.has_notifications();
            if had_notifications != has_notifications {
                multi_workspace.update(cx, |_, cx| cx.notify());
            }
        });
    }
}

impl WorkspaceSidebar for Sidebar {
    fn width(&self, _cx: &App) -> Pixels {
        self.width
    }

    fn set_width(&mut self, width: Option<Pixels>, cx: &mut Context<Self>) {
        self.width = width.unwrap_or(DEFAULT_WIDTH).clamp(MIN_WIDTH, MAX_WIDTH);
        cx.notify();
    }

    fn has_notifications(&self, cx: &App) -> bool {
        self.picker.read(cx).delegate.has_notifications()
    }
}

impl Focusable for Sidebar {
    fn focus_handle(&self, cx: &App) -> FocusHandle {
        self.picker.read(cx).focus_handle(cx)
    }
}

fn sorted_paths_key<P: AsRef<Path>>(paths: &[P]) -> String {
    let mut sorted: Vec<String> = paths
        .iter()
        .map(|p| p.as_ref().to_string_lossy().to_string())
        .collect();
    sorted.sort();
    sorted.join("\n")
}

fn read_thread_title_map() -> Option<HashMap<String, String>> {
    let json = KEY_VALUE_STORE
        .read_kvp(LAST_THREAD_TITLES_KEY)
        .log_err()
        .flatten()?;
    serde_json::from_str(&json).log_err()
}

fn read_workspace_display_name_map() -> Option<HashMap<String, String>> {
    let json = KEY_VALUE_STORE
        .read_kvp(WORKSPACE_DISPLAY_NAMES_KEY)
        .log_err()
        .flatten()?;
    serde_json::from_str(&json).log_err()
}

impl Render for Sidebar {
    fn render(&mut self, window: &mut Window, cx: &mut Context<Self>) -> impl IntoElement {
        let titlebar_height = ui::utils::platform_title_bar_height(window);
        let ui_font = theme::setup_ui_font(window, cx);
        let is_focused = self.focus_handle(cx).is_focused(window);

        let focus_tooltip_label = if is_focused {
            "Focus Workspace"
        } else {
            "Focus Sidebar"
        };

        v_flex()
            .id("workspace-sidebar")
            .key_context("WorkspaceSidebar")
            .font(ui_font)
            .h_full()
            .w(self.width)
            .bg(cx.theme().colors().surface_background)
            .border_r_1()
            .border_color(cx.theme().colors().border)
            .child(
                h_flex()
                    .flex_none()
                    .h(titlebar_height)
                    .w_full()
                    .mt_px()
                    .pb_px()
                    .pr_1()
                    .when_else(
                        cfg!(target_os = "macos") && !window.is_fullscreen(),
                        |this| this.pl(px(TRAFFIC_LIGHT_PADDING)),
                        |this| this.pl_2(),
                    )
                    .justify_between()
                    .border_b_1()
                    .border_color(cx.theme().colors().border)
                    .child({
                        let focus_handle = cx.focus_handle();
                        IconButton::new("close-sidebar", IconName::WorkspaceNavOpen)
                            .icon_size(IconSize::Small)
                            .tooltip(Tooltip::element(move |_, cx| {
                                v_flex()
                                    .gap_1()
                                    .child(
                                        h_flex()
                                            .gap_2()
                                            .justify_between()
                                            .child(Label::new("Close Sidebar"))
                                            .child(KeyBinding::for_action_in(
                                                &ToggleWorkspaceSidebar,
                                                &focus_handle,
                                                cx,
                                            )),
                                    )
                                    .child(
                                        h_flex()
                                            .pt_1()
                                            .gap_2()
                                            .border_t_1()
                                            .border_color(cx.theme().colors().border_variant)
                                            .justify_between()
                                            .child(Label::new(focus_tooltip_label))
                                            .child(KeyBinding::for_action_in(
                                                &FocusWorkspaceSidebar,
                                                &focus_handle,
                                                cx,
                                            )),
                                    )
                                    .into_any_element()
                            }))
                            .on_click(cx.listener(|_this, _, _window, cx| {
                                cx.emit(SidebarEvent::Close);
                            }))
                    })
                    .child(
                        IconButton::new("new-workspace", IconName::Plus)
                            .icon_size(IconSize::Small)
                            .tooltip(|_window, cx| {
                                Tooltip::for_action("New Workspace", &NewWorkspaceInWindow, cx)
                            })
                            .on_click(cx.listener(|this, _, window, cx| {
                                this.multi_workspace.update(cx, |multi_workspace, cx| {
                                    multi_workspace.create_workspace(window, cx);
                                });
                            })),
                    ),
            )
            .child(self.picker.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use feature_flags::FeatureFlagAppExt as _;
    use fs::FakeFs;
    use gpui::TestAppContext;
    use settings::SettingsStore;

    fn init_test(cx: &mut TestAppContext) {
        cx.update(|cx| {
            let settings_store = SettingsStore::test(cx);
            cx.set_global(settings_store);
            theme::init(theme::LoadThemes::JustBase, cx);
            editor::init(cx);
            cx.update_flags(false, vec!["agent-v2".into()]);
        });
    }

    fn set_thread_info_and_refresh(
        sidebar: &Entity<Sidebar>,
        multi_workspace: &Entity<MultiWorkspace>,
        index: usize,
        title: &str,
        status: AgentThreadStatus,
        cx: &mut gpui::VisualTestContext,
    ) {
        sidebar.update_in(cx, |s, _window, _cx| {
            s.set_test_thread_info(index, SharedString::from(title.to_string()), status);
        });
        multi_workspace.update_in(cx, |_, _window, cx| cx.notify());
        cx.run_until_parked();
    }

    fn has_notifications(sidebar: &Entity<Sidebar>, cx: &mut gpui::VisualTestContext) -> bool {
        sidebar.read_with(cx, |s, cx| s.has_notifications(cx))
    }

    #[gpui::test]
    async fn test_notification_on_running_to_completed_transition(cx: &mut TestAppContext) {
        init_test(cx);
        let fs = FakeFs::new(cx.executor());
        cx.update(|cx| <dyn Fs>::set_global(fs.clone(), cx));
        let project = project::Project::test(fs, [], cx).await;

        let (multi_workspace, cx) =
            cx.add_window_view(|window, cx| MultiWorkspace::test_new(project, window, cx));

        let sidebar = multi_workspace.update_in(cx, |_mw, window, cx| {
            let mw_handle = cx.entity();
            cx.new(|cx| Sidebar::new(mw_handle, window, cx))
        });
        multi_workspace.update_in(cx, |mw, window, cx| {
            mw.register_sidebar(sidebar.clone(), window, cx);
        });
        cx.run_until_parked();

        // Create a second workspace and switch to it so workspace 0 is background.
        multi_workspace.update_in(cx, |mw, window, cx| {
            mw.create_workspace(window, cx);
        });
        cx.run_until_parked();
        multi_workspace.update_in(cx, |mw, window, cx| {
            mw.activate_index(1, window, cx);
        });
        cx.run_until_parked();

        assert!(
            !has_notifications(&sidebar, cx),
            "should have no notifications initially"
        );

        set_thread_info_and_refresh(
            &sidebar,
            &multi_workspace,
            0,
            "Test Thread",
            AgentThreadStatus::Running,
            cx,
        );

        assert!(
            !has_notifications(&sidebar, cx),
            "Running status alone should not create a notification"
        );

        set_thread_info_and_refresh(
            &sidebar,
            &multi_workspace,
            0,
            "Test Thread",
            AgentThreadStatus::Completed,
            cx,
        );

        assert!(
            has_notifications(&sidebar, cx),
            "Running → Completed transition should create a notification"
        );
    }

    #[gpui::test]
    async fn test_no_notification_for_active_workspace(cx: &mut TestAppContext) {
        init_test(cx);
        let fs = FakeFs::new(cx.executor());
        cx.update(|cx| <dyn Fs>::set_global(fs.clone(), cx));
        let project = project::Project::test(fs, [], cx).await;

        let (multi_workspace, cx) =
            cx.add_window_view(|window, cx| MultiWorkspace::test_new(project, window, cx));

        let sidebar = multi_workspace.update_in(cx, |_mw, window, cx| {
            let mw_handle = cx.entity();
            cx.new(|cx| Sidebar::new(mw_handle, window, cx))
        });
        multi_workspace.update_in(cx, |mw, window, cx| {
            mw.register_sidebar(sidebar.clone(), window, cx);
        });
        cx.run_until_parked();

        // Workspace 0 is the active workspace — thread completes while
        // the user is already looking at it.
        set_thread_info_and_refresh(
            &sidebar,
            &multi_workspace,
            0,
            "Test Thread",
            AgentThreadStatus::Running,
            cx,
        );
        set_thread_info_and_refresh(
            &sidebar,
            &multi_workspace,
            0,
            "Test Thread",
            AgentThreadStatus::Completed,
            cx,
        );

        assert!(
            !has_notifications(&sidebar, cx),
            "should not notify for the workspace the user is already looking at"
        );
    }

    #[gpui::test]
    async fn test_notification_cleared_on_workspace_activation(cx: &mut TestAppContext) {
        init_test(cx);
        let fs = FakeFs::new(cx.executor());
        cx.update(|cx| <dyn Fs>::set_global(fs.clone(), cx));
        let project = project::Project::test(fs, [], cx).await;

        let (multi_workspace, cx) =
            cx.add_window_view(|window, cx| MultiWorkspace::test_new(project, window, cx));

        let sidebar = multi_workspace.update_in(cx, |_mw, window, cx| {
            let mw_handle = cx.entity();
            cx.new(|cx| Sidebar::new(mw_handle, window, cx))
        });
        multi_workspace.update_in(cx, |mw, window, cx| {
            mw.register_sidebar(sidebar.clone(), window, cx);
        });
        cx.run_until_parked();

        // Create a second workspace so we can switch away and back.
        multi_workspace.update_in(cx, |mw, window, cx| {
            mw.create_workspace(window, cx);
        });
        cx.run_until_parked();

        // Switch to workspace 1 so workspace 0 becomes a background workspace.
        multi_workspace.update_in(cx, |mw, window, cx| {
            mw.activate_index(1, window, cx);
        });
        cx.run_until_parked();

        // Thread on workspace 0 transitions Running → Completed while
        // the user is looking at workspace 1.
        set_thread_info_and_refresh(
            &sidebar,
            &multi_workspace,
            0,
            "Test Thread",
            AgentThreadStatus::Running,
            cx,
        );
        set_thread_info_and_refresh(
            &sidebar,
            &multi_workspace,
            0,
            "Test Thread",
            AgentThreadStatus::Completed,
            cx,
        );

        assert!(
            has_notifications(&sidebar, cx),
            "background workspace completion should create a notification"
        );

        // Switching back to workspace 0 should clear the notification.
        multi_workspace.update_in(cx, |mw, window, cx| {
            mw.activate_index(0, window, cx);
        });
        cx.run_until_parked();

        assert!(
            !has_notifications(&sidebar, cx),
            "notification should be cleared when workspace becomes active"
        );
    }
}
