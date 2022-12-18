---
title: The Perfect Desktop Environment (for me)
author: Dan Vonk
---

Many Linux users settle on whatever their desktop environment their distro provides. Then there are others are endlessly tinkering--that was me. However, for the last couple of years, I've settled on what I think is a happy middle-ground between the flexibility of a tiling WM like `sway` and the usability of `GNOME` or `KDE`.

In case you didn't know already, tiling window managers differ from 'floating' window managers like GNOME in that they subdivide the screen into smaller rectangles and place windows side-by-side in these smaller rectanlges. That is, windows cannot float on top of each other. I find this much easier to work with as one does not have to go hunting to find their text editor behind six open file browsers.

<figure>
    <label for="de1-split" class="margin-toggle">&#8853;</label><input type="checkbox" id="de1-split" class="margin-toggle"/><span class="marginnote">Typical split-screen configuration of my WM.</span>
    <img src="/images/de2.png" alt="Two windows side-by-side in the WM." />
</figure>

However, in my experience, there always comes a point when running one of these lightweight WMs, where one wishes for the traditional heavyweight GUI approach. For me this is usually configuring network connections or VPNs, where there are many options and switches, but I use them so infrequently that I never really learn the command line workflow to configure them.

Luckily, I have discovered that you can have the best of both worlds. This is because if you run Xfce, you can disable its window management gizmos and instead run an `i3` session inside it.

<figure>
    <label for="de2" class="margin-toggle">&#8853;</label><input type="checkbox" id="de2" class="margin-toggle"/><span class="marginnote">Example of a GUI dialog which I wouldn't bother learning the command-line workflow for.</span>
    <img src="/images/de4.png" alt="Appearance dialog in Xfce" />
</figure>

All one needs to do is go to _Session and Startup_ and remove the Xfce desktop process from startup in the current session (as shown above). Then add `i3` as a startup command under the _Application Autostart_ tab. Also don't forget to remove all Xfce keyboard shortcuts under _Application Shortcuts_ of the _Keyboard_ settings menu, as these conflict with i3.
