local wezterm = require("wezterm")
local act = wezterm.action
return {
  font = wezterm.font_with_fallback({
    "Iosevka",
    "Iosevka Nerd Font",
  }),
  font_size = 16.0,
  color_scheme = "catppuccin-mocha",
  tab_bar_at_bottom = true,
  hide_tab_bar_if_only_one_tab = true,
  window_background_opacity = 1,
  window_decorations = "RESIZE",
  inactive_pane_hsb = {
    -- NOTE: these values are multipliers, applied on normal pane values
    saturation = 0.9,
    brightness = 0.6,
  },
  scrollback_lines = 5000,
  front_end = "WebGpu",
}
