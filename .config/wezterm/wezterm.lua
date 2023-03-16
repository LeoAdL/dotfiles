local wezterm = require("wezterm")
local act = wezterm.action
return {
	font = wezterm.font({
		family = "Iosevka Nerd Font",
	}),
	font_size = 16.0,
	color_scheme = "Catppuccin Frappe",
	tab_bar_at_bottom = true,
	hide_tab_bar_if_only_one_tab = true,
	use_fancy_tab_bar = false,
	front_end = WebGpu,
	window_background_opacity = 1.00,
	enable_kitty_graphics = true,
	text_background_opacity = 1,
	native_macos_fullscreen_mode = true,
	freetype_load_target = "Light",
	window_decorations = "RESIZE",
	inactive_pane_hsb = {
		-- NOTE: these values are multipliers, applied on normal pane values
		saturation = 0.9,
		brightness = 0.6,
	},
	scrollback_lines = 5000,
}
