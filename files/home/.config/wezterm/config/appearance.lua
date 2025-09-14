-- vim: sw=2

local wezterm = require 'wezterm'

-- Pull in the wezterm API
--local wezterm = require 'wezterm'
--local mux = wezterm.mux
--local act = wezterm.action

-- This will hold the configuration.
--local config = wezterm.config_builder()

-- This is where you actually apply your config choices

return {
  tab_bar_at_bottom = true,
  audible_bell = 'Disabled',
  visual_bell = {
    fade_in_duration_ms = 75,
    fade_out_duration_ms = 75,
    target = 'CursorColor',
  },
  color_scheme = 'Dracula',
  default_cursor_style = 'BlinkingUnderline',
  window_decorations = "RESIZE",
  hide_tab_bar_if_only_one_tab = true,
  font_size = 11.0,
  font = wezterm.font_with_fallback {
  --  { family = 'Iosevka Term JBMS',           weight = 'Regular', stretch = 'Normal'}, -- // OR stretch = 'Expanded'
  --  { family = 'Iosevka Term Nerd Font Mono', weight = 'Regular', stretch = 'Normal'}, -- // OR stretch = 'Expanded'
    { family = 'Cascadia Code', weight = 'Regular', stretch = 'Normal'}
  },

  -- debug_key_events = true,

  --[[
    -- From article https://mayberoot.medium.com/the-perfect-windows-11-dev-environment-setup-with-wezterm-wsl2-and-neovim-d73ab1202703

    -- Default config settings
    -- These are the default config settins needed to use Wezterm
    -- Just add this and return config and that's all the basics you need

    -- Color scheme, Wezterm has 100s of them you can see here:
    -- https://wezfurlong.org/wezterm/colorschemes/index.html
    --config.color_scheme = 'Oceanic Next (Gogh)'
    -- This is my chosen font, we will get into installing fonts on windows later
    --config.font = wezterm.font('Hack Nerd Font')
    --config.font_size = 11
    --config.launch_menu = launch_menu
    -- makes my cursor blink
    --config.default_cursor_style = 'BlinkingBar'
    --config.disable_default_key_bindings = true
    -- this adds the ability to use ctrl+v to paste the system clipboard

    -- These are vars to put things in later (i dont use em all yet)
    local config = {}
    local mouse_bindings = {}

    config.keys = {{ key = 'V', mods = 'CTRL', action = act.PasteFrom 'Clipboard' },}
    config.mouse_bindings = mouse_bindings

    -- There are mouse binding to mimc Windows Terminal and let you copy
    -- To copy just highlight something and right click. Simple
    mouse_bindings = {
      {
	event = { Down = { streak = 3, button = 'Left' } },
	action = wezterm.action.SelectTextAtMouseCursor 'SemanticZone',
	mods = 'NONE',
      },
     {
      event = { Down = { streak = 1, button = "Right" } },
      mods = "NONE",
      action = wezterm.action_callback(function(window, pane)
       local has_selection = window:get_selection_text_for_pane(pane) ~= ""
       if has_selection then
	window:perform_action(act.CopyTo("ClipboardAndPrimarySelection"), pane)
	window:perform_action(act.ClearSelection, pane)
       else
	window:perform_action(act({ PasteFrom = "Clipboard" }), pane)
       end
      end),
     },
    }

    -- This is used to make my foreground (text, etc) brighter than my background
    config.foreground_text_hsb = {
      hue = 1.0,
      saturation = 1.2,
      brightness = 1.5,
    }

    -- This is used to set an image as my background
    config.background = {
	{
	  source = { File = {path = 'C:/Users/someuserboi/Pictures/Backgrounds/theone.gif', speed = 0.2}},
	  opacity = 1,
	  width = "100%",
	  hsb = {brightness = 0.5},
	}
    }
  --]]

  -- Sets WSL2 the default when opening WezTerm.
  default_domain = 'wsl:ubuntu-bash',
}

-- and finally, return the configuration to wezterm
--return config

--[[

-- Copy of https://github.com/KevinSilvester/wezterm-config/blob/master/config/appearance.lua
local gpu_adapters = -- require('utils.gpu-adapter')
local backdrops = -- require('utils.backdrops')
local colors = require('colours.custom')

return {
   max_fps = 120,
   front_end = 'WebGpu', ---@type 'WebGpu' | 'OpenGL' | 'Software'
   webgpu_power_preference = 'HighPerformance',
   webgpu_preferred_adapter = gpu_adapters:pick_best(),
   -- webgpu_preferred_adapter = gpu_adapters:pick_manual('Dx12', 'IntegratedGpu'),
   -- webgpu_preferred_adapter = gpu_adapters:pick_manual('Gl', 'Other'),
   underline_thickness = '1.5pt',

   -- cursor
   animation_fps = 120,
   cursor_blink_ease_in = 'EaseOut',
   cursor_blink_ease_out = 'EaseOut',
   default_cursor_style = 'BlinkingBlock',
   cursor_blink_rate = 650,

   -- color scheme
   colors = colors,

   -- background: pass in `true` if you want wezterm to start with focus mode on (no bg images)
   background = backdrops:initial_options(false),

   -- scrollbar
   enable_scroll_bar = true,

   -- tab bar
   enable_tab_bar = true,
   hide_tab_bar_if_only_one_tab = false,
   use_fancy_tab_bar = false,
   tab_max_width = 25,
   show_tab_index_in_tab_bar = false,
   switch_to_last_active_tab_when_closing_tab = true,

   -- command palette
   command_palette_fg_color = '#b4befe',
   command_palette_bg_color = '#11111b',
   command_palette_font_size = 12,
   command_palette_rows = 25,

   -- window
   window_padding = {
      left = 0,
      right = 0,
      top = 10,
      bottom = 7.5,
   },
   adjust_window_size_when_changing_font_size = false,
   window_close_confirmation = 'NeverPrompt',
   window_frame = {
      active_titlebar_bg = '#090909',
      -- font = fonts.font,
      -- font_size = fonts.font_size,
   },
   -- inactive_pane_hsb = {
   --    saturation = 0.9,
   --    brightness = 0.65,
   -- },
   inactive_pane_hsb = {
      saturation = 1,
      brightness = 1,
   },

   visual_bell = {
      fade_in_function = 'EaseIn',
      fade_in_duration_ms = 250,
      fade_out_function = 'EaseOut',
      fade_out_duration_ms = 250,
      target = 'CursorColor',
   },
}
--]]
