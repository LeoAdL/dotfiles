local wezterm = require("wezterm")
local act = wezterm.action
return {
    font = wezterm.font("Fira Code Retina"),
    font_size = 14.0,
    color_scheme = "Catppuccin Frappe",
    use_fancy_tab_bar = false,
    tab_bar_at_bottom = true,
    window_background_opacity = 0.95,
    text_background_opacity = 1,
    window_decorations = "RESIZE",
    leader = { key = "A", mods = "CTRL" },
    inactive_pane_hsb = {
        -- NOTE: these values are multipliers, applied on normal pane values
        saturation = 0.9,
        brightness = 0.6,
    },
    keys = {
        -- This will create a new split and run the `top` program inside it
        {
            key = "v",
            mods = "ALT",
            action = wezterm.action.SplitPane({
                direction = "Left",
                size = { Percent = 50 },
            }),
        },
        {
            key = "s",
            mods = "ALT",
            action = wezterm.action.SplitVertical({}),
        },
        {
            key = "H",
            mods = "LEADER",
            action = act.AdjustPaneSize({ "Left", 5 }),
        },
        {
            key = "J",
            mods = "LEADER",
            action = act.AdjustPaneSize({ "Down", 5 }),
        },
        { key = "K", mods = "LEADER", action = act.AdjustPaneSize({ "Up", 5 }) },
        {
            key = "L",
            mods = "LEADER",
            action = act.AdjustPaneSize({ "Right", 5 }),
        },
        {
            key = "H",
            mods = "CTRL|SHIFT",
            action = act.ActivatePaneDirection("Left"),
        },
        {
            key = "L",
            mods = "CTRL|SHIFT",
            action = act.ActivatePaneDirection("Right"),
        },
        {
            key = "K",
            mods = "CTRL|SHIFT",
            action = act.ActivatePaneDirection("Up"),
        },
        {
            key = "J",
            mods = "CTRL|SHIFT",
            action = act.ActivatePaneDirection("Down"),
        },
        {
            key = "w",
            mods = "CMD",
            action = wezterm.action.CloseCurrentPane({ confirm = true }),
        },
    },
    scrollback_lines = 3500,
    -- for example, this selects a Bold, Italic font variant.
}
