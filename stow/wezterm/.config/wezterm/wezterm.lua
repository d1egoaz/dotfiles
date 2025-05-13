local wezterm = require "wezterm"
local config = wezterm.config_builder()
local action = wezterm.action

-- (here will be added actual configuration)
config.font = wezterm.font {
  family = 'Essential PragmataPro',
}
config.font_size = 18.0
config.line_height = 1.0

--config.color_scheme = "Tokyo Night"
--config.color_scheme = "Bamboo"
config.color_scheme = "Nightfly (Gogh)"
config.enable_tab_bar = false
config.window_padding = { left = '0.5cell', right = '0.5cell', top = '0.5cell', bottom = '0.5cell' }
config.default_cursor_style = 'BlinkingBlock'

config.window_decorations = 'RESIZE|INTEGRATED_BUTTONS'
-- config.window_background_opacity = 0.96
-- config.macos_window_background_blur = 20
-- https://medium.com/@vladkens/from-iterm-to-wezterm-24db2ccb8dc1
-- config.keys = {
--   { key = 'd', mods = 'CMD|SHIFT', action = action.SplitVertical { domain = 'CurrentPaneDomain' } },
--   { key = 'd', mods = 'CMD', action = action.SplitHorizontal { domain = 'CurrentPaneDomain' } },
--   { key = 'k', mods = 'CMD', action = action.ClearScrollback 'ScrollbackAndViewport' },
--   { key = 'w', mods = 'CMD', action = action.CloseCurrentPane { confirm = false } },
--   { key = 'w', mods = 'CMD|SHIFT', action = action.CloseCurrentTab { confirm = false } },
--   { key = 'LeftArrow', mods = 'CMD', action = action.SendKey { key = 'Home' } },
--   { key = 'RightArrow', mods = 'CMD', action = action.SendKey { key = 'End' } },
--       { key = 'Backspace',  mods = 'CMD',       action = action.SendKey { key = 'u', mods = 'CTRL' } },
--        { key = 'LeftArrow',  mods = 'OPT',       action = wezterm.action { SendString = "\x1bb" } },
    -- { key = 'RightArrow', mods = 'OPT',       action = wezterm.action { SendString = "\x1bf" } },



config.keys = {
  { key = 'p', mods = 'CMD|SHIFT', action = action.ActivateCommandPalette },
  { key = 'k', mods = 'CMD', action = wezterm.action.ClearScrollback 'ScrollbackAndViewport' },
}

return config
