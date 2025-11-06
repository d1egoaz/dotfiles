-- -*- mode: lua; -*-

local wezterm = require("wezterm")

local config = wezterm.config_builder()
local action = wezterm.action

config.enable_scroll_bar = true
config.scrollback_lines = 6400
config.window_close_confirmation = "NeverPrompt"

config.font = wezterm.font_with_fallback({
    "PragmataPro",
    "Essential PragmataPro",
    "DengXian", -- test with: wezterm ls-fonts --text '显示中文'
    "SF Pro",
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

--- Returns an action that prompts for a command and sends it to all panes in the active tab
local function send_command_to_all_panes_action(description)
    return wezterm.action.PromptInputLine({
        description = description or "Send command to all panes in tab",
        action = wezterm.action_callback(function(window, _, line)
            if line then
                local tab = window:active_tab()
                for _, pane in ipairs(tab:panes()) do
                    pane:send_text(line .. "\r")
                end
            end
        end),
    })
end

config.leader = { key = "b", mods = "CTRL", timeout_milliseconds = 1000 }
config.keys = {
    { key = "p", mods = "CMD|SHIFT", action = action.ActivateCommandPalette },
    { key = "k", mods = "CTRL|ALT|SHIFT", action = wezterm.action.ClearScrollback("ScrollbackAndViewport") },
    { key = "w", mods = "CMD", action = action.CloseCurrentPane({ confirm = false }) },
    { key = "c", mods = "LEADER", action = action.SpawnTab("CurrentPaneDomain") },
    -- CMD + (h,j,k,l) to move between panes
    { key = "h", mods = "CMD", action = action.ActivatePaneDirection("Left") },
    { key = "j", mods = "CMD", action = action.ActivatePaneDirection("Down") },
    { key = "k", mods = "CMD", action = action.ActivatePaneDirection("Up") },
    { key = "l", mods = "CMD", action = action.ActivatePaneDirection("Right") },

    -- CTRL + ALT + (h,j,k,l) to move between tabs
    { key = "h", mods = "CTRL|ALT", action = action.ActivateTabRelative(-1) },
    { key = "j", mods = "CTRL|ALT", action = action.ActivateTabRelative(1) },
    { key = "k", mods = "CTRL|ALT", action = action.ActivateTabRelative(-1) },
    { key = "l", mods = "CTRL|ALT", action = action.ActivateTabRelative(1) },

    -- new panes
    { key = "d", mods = "CMD", action = action.SplitVertical({ domain = "CurrentPaneDomain" }) },
    { key = "d", mods = "CMD|SHIFT", action = action.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
    { key = "/", mods = "LEADER", action = action.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
    { key = "-", mods = "LEADER", action = action.SplitVertical({ domain = "CurrentPaneDomain" }) },

    -- maximize
    { key = "m", mods = "LEADER", action = wezterm.action.TogglePaneZoomState },

    -- CTRL + SHIFT + (h,j,k,l) to resize panes
    { key = "h", mods = "CTRL|SHIFT", action = action.AdjustPaneSize({ "Left", 5 }) },
    { key = "l", mods = "CTRL|SHIFT", action = action.AdjustPaneSize({ "Right", 5 }) },
    { key = "j", mods = "CTRL|SHIFT", action = action.AdjustPaneSize({ "Down", 5 }) },
    { key = "k", mods = "CTRL|SHIFT", action = action.AdjustPaneSize({ "Up", 5 }) },

    -- yank mode
    { key = "y", mods = "LEADER", action = action.ActivateCopyMode },
    {
        key = "s",
        mods = "CMD|SHIFT",
        action = send_command_to_all_panes_action(),
    },
}
config.mouse_bindings = {
    -- Change the default click behavior so that it only selects text and doesn't open hyperlinks
    {
        event = { Up = { streak = 1, button = "Left" } },
        mods = "NONE",
        action = action.CompleteSelection("ClipboardAndPrimarySelection"),
    },
    -- and make Command-Click open hyperlinks
    {
        event = { Up = { streak = 1, button = "Left" } },
        mods = "CMD",
        action = action.OpenLinkAtMouseCursor,
    },
}

config.use_fancy_tab_bar = true
config.pane_focus_follows_mouse = false
config.hide_tab_bar_if_only_one_tab = false
config.tab_bar_at_bottom = false

return config
