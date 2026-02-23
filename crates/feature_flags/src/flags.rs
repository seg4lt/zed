use crate::FeatureFlag;
use std::sync::LazyLock;

static ZED_ENABLE_AGENT_V2: LazyLock<bool> = LazyLock::new(|| {
    std::env::var("ZED_ENABLE_AGENT_V2")
        .ok()
        .as_deref()
        .is_some_and(is_truthy_flag_value)
});

fn is_truthy_flag_value(value: &str) -> bool {
    let value = value.trim();
    value == "1"
        || value.eq_ignore_ascii_case("true")
        || value.eq_ignore_ascii_case("yes")
        || value.eq_ignore_ascii_case("on")
}

pub struct NotebookFeatureFlag;

impl FeatureFlag for NotebookFeatureFlag {
    const NAME: &'static str = "notebooks";
}

pub struct PanicFeatureFlag;

impl FeatureFlag for PanicFeatureFlag {
    const NAME: &'static str = "panic";
}

pub struct AgentV2FeatureFlag;

impl FeatureFlag for AgentV2FeatureFlag {
    const NAME: &'static str = "agent-v2";

    fn enabled_for_staff() -> bool {
        true
    }

    fn enabled_for_all() -> bool {
        *ZED_ENABLE_AGENT_V2
    }
}

/// A feature flag for granting access to beta ACP features.
///
/// We reuse this feature flag for new betas, so don't delete it if it is not currently in use.
pub struct AcpBetaFeatureFlag;

impl FeatureFlag for AcpBetaFeatureFlag {
    const NAME: &'static str = "acp-beta";
}

pub struct AgentSharingFeatureFlag;

impl FeatureFlag for AgentSharingFeatureFlag {
    const NAME: &'static str = "agent-sharing";
}

pub struct SubagentsFeatureFlag;

impl FeatureFlag for SubagentsFeatureFlag {
    const NAME: &'static str = "subagents";

    fn enabled_for_staff() -> bool {
        false
    }
}

pub struct DiffReviewFeatureFlag;

impl FeatureFlag for DiffReviewFeatureFlag {
    const NAME: &'static str = "diff-review";

    fn enabled_for_staff() -> bool {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::is_truthy_flag_value;

    #[test]
    fn parses_truthy_flag_values() {
        assert!(is_truthy_flag_value("1"));
        assert!(is_truthy_flag_value("true"));
        assert!(is_truthy_flag_value("TRUE"));
        assert!(is_truthy_flag_value("yes"));
        assert!(is_truthy_flag_value("YES"));
        assert!(is_truthy_flag_value("on"));
        assert!(is_truthy_flag_value("ON"));
        assert!(is_truthy_flag_value(" on "));
    }

    #[test]
    fn parses_non_truthy_flag_values() {
        assert!(!is_truthy_flag_value(""));
        assert!(!is_truthy_flag_value("0"));
        assert!(!is_truthy_flag_value("false"));
        assert!(!is_truthy_flag_value("off"));
        assert!(!is_truthy_flag_value("2"));
        assert!(!is_truthy_flag_value("anything"));
    }
}
