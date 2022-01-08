# XMonad Config to Keep It Simple, Stupid

We're seen/used/experienced all fancy designs and functionalities of modern
desktop environments. It's time to move forward regarding the essence of
UI---*towards the most effective way of information sharing between the
computer and human*. Note this project is limited and ignores new advanced
physical media like the headjack in *The Matrix*.

More concretely, given the fixed number of pixels on monitor(s) that can be
displayed at a time, how to maximize the information/data/content conveyed to
the user? And if all physical pixels are not enough, how to quickly switch
pixels to realize the displayed information multiplier?

### What do I Need From a Window Manager?

1. Not waste a single pixel on displaying window content. For some windows,
   such as a browser or VM, set no border to maximize the number of pixels for
   showing content.

2. No overlapped windows, strict tiling. I am tired of cycling through windows
   with `Alt`+`Tab` to find the window I need. It is O(n) complexity and slow.
   I am happy to do it at O(1) by tiling windows on workspaces and memorizing
   which window belongs to which workspace, and switching to that workspace to
   see the content of the window. And no switching visual effects to not waste
   time.

   Yes, the map/dictionary of the workspace to windows is in my head. I see it
   as human register besides CPU registers for the entire human computer
   symbiosis where UI is just an interface between the two.

   Occasionally I forget where a window is, and I need a search tool to bring
   the window/workspace to me.
   
3. No status bar. How often do I need to know how full the disk is? How much
   CPU is using? Which version of the OS is? Not often. And when I need it, I
   also need details so I use the entire screen to display
   `top`/`htop`/`iotop`/...

4. No trayer. I use `ip addr` and `nmcli` to configurate network (if not
   automatically done). I use a workspace dedicated for IM. Why do I need the
   system trayer?

5. Need a personal multi-screen management.

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

6. Need a floating console for temporary/quick information check like calculator, date, etc.
      
### Multi-Screen Setup

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

### Window Management Setup

- List windows:
  - show existing windows to choose/goto with `mod+g`
- Floating console:
  - create one on the bottom of the screen with `mod+c`

### Complementary Utilities

- urxvt (conf: https://github.com/subbyte/configurations/tree/master/urxvt)
- gvim (conf: https://github.com/subbyte/configurations/tree/master/vim)
- zathura
- firefox w/ vimium
- chromium w/ vimium
