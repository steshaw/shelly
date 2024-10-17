-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices

-- For example, changing the color scheme:
config.color_scheme = 'Dracula'
--config.color_scheme = 'AdventureTime'

config.window_decorations = "NONE"
config.hide_tab_bar_if_only_one_tab = true

config.font_size = 15.0

-- -> => ++ <> >= <=
config.font = wezterm.font {
  family = 'Iosevka Term JBML',
--  family = 'Jetbrains Mono Nerd Font',
--  stretch = 'Expanded',
--  weight = 'Regular',
}

-- and finally, return the configuration to wezterm
return config
