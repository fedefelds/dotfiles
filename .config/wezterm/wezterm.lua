local wezterm = require 'wezterm';

return {
  -- Appearance
  font = wezterm.font("JetBrains Mono", {weight="Regular", stretch="Normal", style="Normal"}),
  font_size = 12.0,
  line_height = 1.1,
  color_scheme = "Gruvbox Dark", -- You can choose from various color schemes
  window_background_opacity = 1,
  text_background_opacity = 1.0,

  -- Window configuration
  window_decorations = "NONE",
  window_padding = {
    left = 5,
    right = 5,
    top = 5,
    bottom = 5,
  },
  
  -- Tab bar configuration
  enable_tab_bar = true,
  hide_tab_bar_if_only_one_tab = true,
  tab_max_width = 25,
  use_fancy_tab_bar = false,

  -- Scrollback
  scrollback_lines = 5000,
  enable_scroll_bar = true,

  -- Keybindings
  keys = {
    {key="r", mods="CTRL|SHIFT", action=wezterm.action.ReloadConfiguration},
    {key="t", mods="CTRL|SHIFT", action=wezterm.action.SpawnTab "DefaultDomain"},
    {key="w", mods="CTRL|SHIFT", action=wezterm.action.CloseCurrentTab {confirm=true}},
    {key="j", mods="CTRL|SHIFT", action=wezterm.action.ActivateTabRelative(-1)},
    {key="k", mods="CTRL|SHIFT", action=wezterm.action.ActivateTabRelative(1)},
    {key="F11",action=wezterm.action.ToggleFullScreen},
    {key='Enter', mods='ALT', action=wezterm.action.DisableDefaultAssignment},
  },

  -- General behavior
  check_for_updates = true,
  use_ime = false, -- If you don't use an input method editor
  adjust_window_size_when_changing_font_size = false,
}