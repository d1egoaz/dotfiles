local wezterm = require("wezterm")
local config = wezterm.config_builder()
local action = wezterm.action

config.enable_scroll_bar = true
config.scrollback_lines = 6400
config.window_close_confirmation = "NeverPrompt"

-- (here will be added actual configuration)
--config.font = wezterm.font {
--family = 'Essential PragmataPro',
--}
config.font = wezterm.font_with_fallback({
    "Essential PragmataPro",
    "Menlo",
    "DengXian", -- test with: wezterm ls-fonts --text '显示中文'
})
config.default_prog = { "fish", "-l" }

config.font_size = 18.0
config.line_height = 1.0

-- config.color_scheme = "Tokyo Night"
config.color_scheme = "tokyonight_night" -- or tokyonight_day, or whatever style

config.enable_tab_bar = true
config.default_cursor_style = "BlinkingBlock"

config.inactive_pane_hsb = { saturation = 0.7, brightness = 0.5 }

config.window_decorations = "RESIZE|INTEGRATED_BUTTONS"

config.leader = { key = "b", mods = "CTRL", timeout_milliseconds = 1000 }
config.keys = {
    { key = "p", mods = "CMD|SHIFT", action = action.ActivateCommandPalette },
    { key = "k", mods = "CTRL|ALT", action = wezterm.action.ClearScrollback("ScrollbackAndViewport") },
    { key = "w", mods = "CMD", action = action.CloseCurrentPane({ confirm = false }) },
    { key = "c", mods = "LEADER", action = action.SpawnTab("CurrentPaneDomain") },
    -- CTRL + (h,j,k,l) to move between panes
    { key = "h", mods = "CMD", action = action.ActivatePaneDirection("Left") },
    { key = "j", mods = "CMD", action = action.ActivatePaneDirection("Down") },
    { key = "k", mods = "CMD", action = action.ActivatePaneDirection("Up") },
    { key = "l", mods = "CMD", action = action.ActivatePaneDirection("Right") },

    -- new panes
    { key = "d", mods = "CMD|SHIFT", action = action.SplitVertical({ domain = "CurrentPaneDomain" }) },
    { key = "d", mods = "CMD", action = action.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
    { key = "/", mods = "LEADER", action = action.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
    { key = "-", mods = "LEADER", action = action.SplitVertical({ domain = "CurrentPaneDomain" }) },

    -- maximize
    { key = "m", mods = "LEADER", action = wezterm.action.TogglePaneZoomState },

    -- resize
    { key = "h", mods = "CTRL|SHIFT", action = action.AdjustPaneSize({ "Left", 5 }) },
    { key = "l", mods = "CTRL|SHIFT", action = action.AdjustPaneSize({ "Right", 5 }) },
    { key = "j", mods = "CTRL|SHIFT", action = action.AdjustPaneSize({ "Down", 5 }) },
    { key = "k", mods = "CTRL|SHIFT", action = action.AdjustPaneSize({ "Up", 5 }) },

    -- yank mode
    { key = "y", mods = "LEADER", action = action.ActivateCopyMode },
}

config.use_fancy_tab_bar = true
config.pane_focus_follows_mouse = false
config.hide_tab_bar_if_only_one_tab = false
config.tab_bar_at_bottom = false

return config
