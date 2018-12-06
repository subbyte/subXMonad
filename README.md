# subXMonad

Configuration for XMonad with the KISS principle in mind: focusing on window management, no status bar, no trayer.

#### Screen Management Requirements
- Working in office:
  - environment: laptop with one external monitor, and work is majorly done on the external monitor
  - requirement: the external monitor should have the workspace associated with `mod+1` when booted
- Meeting or traveling:
  - environment: just a laptop
  - requirement: the laptop screen should have the workspace associated with `mod+1` when booted
- Presentation (extended display mode):
  - environment: laptop with one hot-plugged projector (external monitor)
  - requirement: show a *specific* *empty* workspace on the projector with wallpaper when plugged in
- Presentation (mirror mode):
  - environment: laptop with one hot-plugged projector (external monitor)
  - requirement: nothing special, just show the current workspace
  
#### Multi-Screen Setup
- Booted without an external screen (meeting or traveling)
  - the laptop screen gets the workspace associated with `mod+1`
- Booted with an external screen (office)
  - the external screen gets the workspace associated with `mod+1`
  - the laptop screen gets the workspace associated with `mod+0`
- New screen plugged in (`XF86_Display` pressed, presentation with extended display)
  - the workspace keeps unchanged on the laptop
  - the new screen gets the workspace associated with ``mod+` ``
- New screen plugged in (`XF86_Display+Shift` pressed, presentation with mirror display)
  - the current workspace is shown on both the laptop and the external screen
- External screen plugged out (`XF86_Display` pressed)
  - the most recently focused workspace is shown on the laptop screen

#### Window Management Requirements
- List windows:
  - requirement: list all existing windows, the ability to goto a window is a plus
- Floating console:
  - requirement: a small floating console for quick cmdline access

#### Window Management Setup
- List windows:
  - show existing windows to choose/goto with `mod+g`
- Floating console:
  - create one on the bottom of the screen with `mod+c`

#### Complementary Utilities
- urxvt (conf: https://github.com/subbyte/configurations/tree/master/urxvt)
- gvim (conf: https://github.com/subbyte/configurations/tree/master/vim)
- dmenu
- zathura
- firefox w/ vimium
- chromium w/ vimium
- i3lock
