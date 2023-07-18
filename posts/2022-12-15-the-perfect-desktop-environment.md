---
title: The Perfect Desktop Environment (for me)
author: Dan Vonk
tags: tech
---

Many Linux users settle on whatever their desktop environment their distro provides. Then there are others are endlessly tinkering--that was me. However, for the last couple of years, I've settled on what I think is a happy middle-ground between the flexibility of a tiling WM like `sway` and the usability of `GNOME` or `KDE`.

In case you didn't know already, tiling window managers differ from 'floating' window managers like GNOME in that they subdivide the screen into smaller rectangles and place windows side-by-side in these smaller rectanlges. That is, windows cannot float on top of each other. I find this much easier to work with as one does not have to go hunting to find their text editor behind six open file browsers.

![Two windows side-by-side in the WM.](/images/de2.png "Typical split-screen configuration of my WM.")

However, in my experience, there always comes a point when running one of these lightweight WMs, where one wishes for the traditional heavyweight GUI approach. For me this is usually configuring network connections or VPNs, where there are many options and switches, but I use them so infrequently that I never really learn the command line workflow to configure them.

Luckily, I have discovered that you can have the best of both worlds. This is because if you run Xfce, you can disable its window management gizmos and instead run an `i3` session inside it.

![Appearance dialog in Xfce](/images/de4.png "Example of a GUI dialog which I wouldn't bother learning the command-line workflow for.<")

All one needs to do is go to _Session and Startup_ and remove the Xfce desktop process from startup in the current session (as shown above). Then add `i3` as a startup command under the _Application Autostart_ tab. Also don't forget to remove all Xfce keyboard shortcuts under _Application Shortcuts_ of the _Keyboard_ settings menu, as these conflict with i3.

![Appearance dialog in Xfce](/images/de5.png "Startup entry for i3.")

You can then customise your i3 config (which lives in `~/.config/i3/config`) to your heart's content. Here's mine for reference, which is slightly adapted from the default file:

```ini
# set modifier
set $super Mod4
set $alt Mod1

# set font
font pango: Overpass Regular 12

# Use Mouse+$super to drag floating windows to their wanted position
floating_modifier $super

# autostart applications
# prefer feh to nitrogen
exec --no-startup-id feh --randomize --bg-fill ~/Pictures/Wallpapers

# run emacs on startup
exec --no-startup-id emacs --daemon

# application keybinds
# start a terminal
bindsym $super+Return exec kitty

# start dmenu (a program launcher)
bindsym $super+Shift+d exec i3-dmenu-desktop --dmenu="dmenu -i -fn 'Fira Code:size=10'"

# dmenu but cooler
bindsym $super+d exec rofi -lines 12 -padding 18 -width 60 -location 0 -show drun -sidebar-mode -columns 3 -font 'Overpass 24'
bindsym $super+a exec rofi -lines 12 -padding 18 -width 60 -location 0 -show window -sidebar-mode -columns 3 -font 'Overpass 24'

# common apps keybinds
bindsym Print exec xfce4-screenshooter -f
bindsym $super+Shift+w exec firefox
bindsym $super+Shift+f exec xdg-open .
bindsym $super+Shift+e exec emacs

bindsym Control+Print exec xfce4-screenshooter -i

# kill focused window
bindsym $super+c kill

# change focus
bindsym $super+Left focus left
bindsym $super+Down focus down
bindsym $super+Up focus up
bindsym $super+Right focus right

# move focused window
bindsym $super+Shift+Left move left
bindsym $super+Shift+Down move down
bindsym $super+Shift+Up move up
bindsym $super+Shift+Right move right

# split in horizontal orientation
bindsym $super+h split h

# split in vertical orientation
bindsym $super+v split v

# enter fullscreen mode for the focused container
bindsym $super+f fullscreen toggle

# change container layout split
bindsym $super+s layout toggle split

# toggle tiling / floating
bindsym $super+space floating toggle

# change focus between tiling / floating windows
bindsym $super+Shift+space focus mode_toggle

# switch to workspace
bindsym $alt+Control+Right workspace next
bindsym $alt+Control+Left workspace prev
bindsym $super+1 workspace 1
bindsym $super+2 workspace 2
bindsym $super+3 workspace 3
bindsym $super+4 workspace 4
bindsym $super+5 workspace 5
bindsym $super+6 workspace 6

# move focused container to workspace
bindsym $super+Shift+1 move container to workspace 1
bindsym $super+Shift+2 move container to workspace 2
bindsym $super+Shift+3 move container to workspace 3
bindsym $super+Shift+4 move container to workspace 4
bindsym $super+Shift+5 move container to workspace 5
bindsym $super+Shift+6 move container to workspace 6

# restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
bindsym $super+Shift+r restart

# exit i3
bindsym $super+q exec "i3-nagbar -t warning -m 'Really, exit?' -b 'Yes' 'i3-msg exit'"

# resize window (you can also use the mouse for that)
mode "resize" {
        bindsym Left resize shrink width 5 px or 5 ppt
        bindsym Down resize grow height 5 px or 5 ppt
        bindsym Up resize shrink height 5 px or 5 ppt
        bindsym Right resize grow width 5 px or 5 ppt
        bindsym Return mode "default"
}

# resize mode. <RET> to finish
bindsym $super+r mode "resize"

# window rules, you can find the window class using xprop

for_window [class=".*"] border pixel 3 # border (no color)

assign [class=Firefox|Chromium] 1
assign [class=Code] 2 # programming workspace
assign [class=Geany|Thunderbird|MineTime|Gucharmap|Soffice|libreoffice*] 3
assign [class=Signal|Spotify] 4
assign [class=Audacity|Vlc|mpv|Ghb|Xfburn|Gimp*|Inkscape] 5
assign [class=Lxappearance|System-config-printer.py|Lxtask|GParted|Pavucontrol|Exo-helper*|Lxrandr|Arandr] 6

for_window [class=Viewnior|feh|Audacious|File-roller|Lxappearance|Lxtask|Pavucontrol|Nautilus] floating enable
for_window [class=URxvt|Firefox|Geany|Evince|Soffice|libreoffice*|mpv|Ghb|Xfburn|Gimp*|Inkscape|Vlc|Lxappearance|Audacity|Thunar|xfce4-terminal|kitty] focus
for_window [class=Xfburn|GParted|System-config-printer.py|Lxtask|Exo-helper*|Lxrandr|Arandr|Thunderbird|Emacs] focus #Pavucontrol was here

client.focused              #bf616a #383c4a #d8dee8 #bf616a #383c4a
client.focused_inactive     #383c4a #2f343f #d8dee8 #2f343f #2f343f
client.unfocused            #383c4a #2f343f #d8dee8 #2f343f #2f343f
client.urgent               #383c4a #2f343f #d8dee8 #2f343f #2f343f
client.placeholder          #383c4a #2f343f #d8dee8 #2f343f #2f343f
client.background           #383c4a 

# switch to next workspace
bindsym $super+Tab workspace next
# switch to next window (useful in tabbed mode)
bindsym $alt+Tab focus right

# scratchpad move/show
bindsym $super+Shift+minus move scratchpad
bindsym $super+minus scratchpad show

# start in tabbed mode by default
workspace_layout tabbed

bindsym $super+w layout tabbed
```
