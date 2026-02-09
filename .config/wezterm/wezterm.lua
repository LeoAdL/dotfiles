local wezterm = require("wezterm")
local gpus = wezterm.gui.enumerate_gpus()

-- Safety: Only use the GPU adapter if one is actually found
return {
    -- Fonts
    font = wezterm.font_with_fallback({ "Iosevka Term" }),
    font_size = 18.0,

    -- Appearance
    color_scheme = "Catppuccin Mocha",
    tab_bar_at_bottom = true,
    hide_tab_bar_if_only_one_tab = true,
    window_background_opacity = 1,
    window_decorations = "RESIZE",

    -- Dim inactive panes
    inactive_pane_hsb = {
        saturation = 0.9,
        brightness = 0.6,
    },

    -- Performance
    front_end = "WebGpu",
    max_fps = 120,
    native_macos_fullscreen_mode = true,

    -- Keys
    leader = { key = "b", mods = "CTRL" },
    keys = {
        -- Send "Ctrl+A" to the terminal when pressing Leader + Ctrl + a
        { key = "a", mods = "LEADER|CTRL",  action = wezterm.action.SendString("\x01") },

        -- Pane Management
        { key = "s", mods = "LEADER",       action = wezterm.action.SplitVertical { domain = "CurrentPaneDomain" } },
        { key = "v", mods = "LEADER",       action = wezterm.action.SplitHorizontal { domain = "CurrentPaneDomain" } },
        { key = "z", mods = "LEADER",       action = wezterm.action.TogglePaneZoomState },
        { key = "x", mods = "LEADER",       action = wezterm.action.CloseCurrentPane { confirm = true } },

        -- CHANGED: Moved TabNavigator to 'w' to avoid conflict with 's' (Split)
        { key = "w", mods = "LEADER",       action = wezterm.action.ShowTabNavigator },

        -- Copy Mode
        { key = "c", mods = "LEADER",       action = wezterm.action.ActivateCopyMode },

        -- Tab Management
        { key = "n", mods = "LEADER",       action = wezterm.action.SpawnTab("CurrentPaneDomain") },
        { key = "&", mods = "LEADER|SHIFT", action = wezterm.action.CloseCurrentTab { confirm = true } },

        -- Navigation (Vim style)
        { key = "h", mods = "LEADER",       action = wezterm.action.ActivatePaneDirection("Left") },
        { key = "j", mods = "LEADER",       action = wezterm.action.ActivatePaneDirection("Down") },
        { key = "k", mods = "LEADER",       action = wezterm.action.ActivatePaneDirection("Up") },
        { key = "l", mods = "LEADER",       action = wezterm.action.ActivatePaneDirection("Right") },

        -- Resize Panes
        { key = "H", mods = "LEADER|SHIFT", action = wezterm.action.AdjustPaneSize { "Left", 5 } },
        { key = "J", mods = "LEADER|SHIFT", action = wezterm.action.AdjustPaneSize { "Down", 5 } },
        { key = "K", mods = "LEADER|SHIFT", action = wezterm.action.AdjustPaneSize { "Up", 5 } },
        { key = "L", mods = "LEADER|SHIFT", action = wezterm.action.AdjustPaneSize { "Right", 5 } },

        -- Tab Activation (Fixed Indexing)
        { key = "1", mods = "LEADER",       action = wezterm.action.ActivateTab(0) },
        { key = "2", mods = "LEADER",       action = wezterm.action.ActivateTab(1) },
        { key = "3", mods = "LEADER",       action = wezterm.action.ActivateTab(2) },
        { key = "4", mods = "LEADER",       action = wezterm.action.ActivateTab(3) },
        { key = "5", mods = "LEADER",       action = wezterm.action.ActivateTab(4) },
        { key = "6", mods = "LEADER",       action = wezterm.action.ActivateTab(5) },
        { key = "7", mods = "LEADER",       action = wezterm.action.ActivateTab(6) },
        { key = "8", mods = "LEADER",       action = wezterm.action.ActivateTab(7) }, -- Fixed
        { key = "9", mods = "LEADER",       action = wezterm.action.ActivateTab(8) }, -- Fixed
    },
}
