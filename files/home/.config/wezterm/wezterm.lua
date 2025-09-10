-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices

config.tab_bar_at_bottom = true
config.audible_bell = 'Disabled'
config.visual_bell = {
  fade_in_duration_ms = 75,
  fade_out_duration_ms = 75,
  target = 'CursorColor',
}

-- For example, changing the color scheme:
config.color_scheme = 'Dracula'

config.default_cursor_style = 'BlinkingUnderline'

config.window_decorations = "RESIZE"
config.hide_tab_bar_if_only_one_tab = true

-- -> => ++ <> >= <=
config.font_size = 12.0
config.font = wezterm.font_with_fallback {
  { family = 'Iosevka Term JBMS',           weight = 'Regular', stretch = 'Normal'}, -- // OR stretch = 'Expanded'
  { family = 'Iosevka Term Nerd Font Mono', weight = 'Regular', stretch = 'Normal'}, -- // OR stretch = 'Expanded'
  { family = 'Cascadia Code', weight = 'Regular', stretch = 'Normal'}
}

-- config.debug_key_events = true

-- and finally, return the configuration to wezterm
return config
