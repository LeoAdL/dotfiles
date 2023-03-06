local wezterm = require("wezterm")
local act = wezterm.action
return {
	font = wezterm.font_with_fallback({ "Iosevka" }),
	font_size = 16.0,
	color_scheme = "Nord (base16)",
	use_fancy_tab_bar = true,
	tab_bar_at_bottom = true,
	window_background_opacity = 0.95,
	enable_kitty_graphics = true,
	text_background_opacity = 1,
	window_decorations = "RESIZE",
	leader = { key = "A", mods = "CTRL" },
	inactive_pane_hsb = {
		-- NOTE: these values are multipliers, applied on normal pane values
		saturation = 0.9,
		brightness = 0.6,
	},
	scrollback_lines = 3500,
	-- for example, this selects a Bold, Italic font variant.
}
