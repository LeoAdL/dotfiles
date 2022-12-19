local wezterm = require("wezterm")
local act = wezterm.action
local function isViProcess(pane)
	-- get_foreground_process_name On Linux, macOS and Windows,
	-- the process can be queried to determine this path. Other operating systems
	-- (notably, FreeBSD and other unix systems) are not currently supported
	return pane:get_foreground_process_name():find("n?vim") ~= nil
	-- return pane:get_title():find("n?vim") ~= nil
end

local function conditionalActivatePane(window, pane, pane_direction, vim_direction)
	if isViProcess(pane) then
		window:perform_action(
		-- This should match the keybinds you set in Neovim.
			act.SendKey({ key = vim_direction, mods = "CTRL" }),
			pane
		)
	else
		window:perform_action(act.ActivatePaneDirection(pane_direction), pane)
	end
end

wezterm.on("ActivatePaneDirection-right", function(window, pane)
	conditionalActivatePane(window, pane, "Right", "l")
end)
wezterm.on("ActivatePaneDirection-left", function(window, pane)
	conditionalActivatePane(window, pane, "Left", "h")
end)
wezterm.on("ActivatePaneDirection-up", function(window, pane)
	conditionalActivatePane(window, pane, "Up", "k")
end)
wezterm.on("ActivatePaneDirection-down", function(window, pane)
	conditionalActivatePane(window, pane, "Down", "j")
end)

return {
	font = wezterm.font("Fira Code Retina"),
	font_size = 14.0,
	color_scheme = "Catppuccin Frappe",
	use_fancy_tab_bar = false,
	tab_bar_at_bottom = true,
	window_background_opacity = 0.95,
	text_background_opacity = 1,
	window_decorations = "RESIZE",
	leader = { key = "a", mods = "CTRL" },
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
		{
			key = "L",
			mods = "LEADER",
			action = act.AdjustPaneSize({ "Right", 5 }),
		},
		{
			key = "K",
			mods = "LEADER",
			action = act.AdjustPaneSize({ "Up", 5 }),
		},
		{ key = "K", mods = "LEADER", action = act.AdjustPaneSize({ "Up", 5 }) },
		{ key = "h", mods = "CTRL", action = act.EmitEvent("ActivatePaneDirection-left") },
		{ key = "j", mods = "CTRL", action = act.EmitEvent("ActivatePaneDirection-down") },
		{ key = "k", mods = "CTRL", action = act.EmitEvent("ActivatePaneDirection-up") },
		{ key = "l", mods = "CTRL", action = act.EmitEvent("ActivatePaneDirection-right") },
		{
			key = "w",
			mods = "CMD",
			action = wezterm.action.CloseCurrentPane({ confirm = true }),
		},
	},
	scrollback_lines = 3500,
	-- for example, this selects a Bold, Italic font variant.
}
