--                                     _
--                          _ __ ___  | |_   _  __ _
--                         | '__/ __| | | | | |/ _` |
--                         | | | (__ _| | |_| | (_| |
--                         |_|  \___(_)_|\__,_|\__,_|
--

-- {{{ Cheat Sheet
-- {{{ Key Bindings
-- ===============================================================  KEY BINDINGS

------------------------------------------------------------ Launch Applications
--
-- Win  +  Enter                    Terminal
-- Win  +  e                        Emacs
-- Win  +  F1                       Start gif screencast recording LowRes
-- Win  +  F2                       Start gif screencast recording HighRes
-- Win  +  F3                       Start gif screencast recording FullRes
-- Win  +  F4                       Start gif screencast the left half of screen
-- Win  +  F5                       Start gif screencast the upper left quadrant
-- Win  +  F6                       Stop gif screencast recording
-- Win  +  F7                       Start mkv screencast recording
-- Win  +  F8                       Stop mkv screencast recording
-- Win  +  F9                       Toogle redshit (color temperature adjust)
--
------------------------------------------------------------------ Shell prompts
--
-- Win  +  r                        Launch a command line prompt into status bar
-- Win  +  /                        Launch dmenu
-- Win  +  .                        Launch passmenu (dmenu-based pass interface)
-- Win  +  ,                        Launch passmenu for username
--
--------------------------------------------------------------------- Navigation
--
-- Win  +  j/k                      Focus on next/previous client
-- Win  +  h                        Previous tag
-- Win  +  l                        Next tag
-- Win  +  1-9                      Show tag 1-9
-- Win  +  Control  +  j/k          Focus next/previous Screen
--
----------------------------------------------------------------- Client Control
--
-- Win  +  m                        Maximize client
-- Win  +  n                        Minimize client
-- Win  +  Control  +  n            Restore (=unminimize) a random client
-- Win  +  f                        Set client fullscreen
-- Win  +  c                        Kill focused client
-- Win  +  t                        Toggle "always visible" (on top)
-- Win  +  Shift    +  r            Redraw focused client
--
------------------------------------------------------------------------ Layouts
--
-- Win  +  Shift    +  j/k          Swap clients
-- Win  +  o                        Move client to next screen
-- Alt  +  l/h                      Change master width by 5%
-- Alt  +  j/k                      Change master height by 5%
-- Win  +  Shift    +  l/h          Number of windows for master area +1/-1
-- Win  +  Control  +  l/h          Number of columns for stack area +1/-1
-- Win  +  Space                    Next layout
-- Win  +  Control  +  Space        Toggle client floating state
-- Win  +  Control  +  1-9          Enable/Disable view of tag 1-9
-- Win  +  Shift    +  1-9          Tag current client with 1-9 tag
-- Win  +  Shift  +  Ctrl  +  1-9   Enable/Disable tag 1-9 for current client
--
------------------------------------------------------------------ Miscellaneous
--
-- Win  +  b                        Hide/Show status bar (Wibox)
-- Win  +  y                        Lock Screen
-- Print Screen                     Take a screenshot
-- Win  +  z/Z                      Screen zoom in/out
--
---------------------------------------------------------------- Awesome Control
--
-- Win  +  Ctrl     +  r            Restart Awesome
-- Win  +  Shift    +  q            Quit Awesome
-- Win  +  w                        Show Awesome menu
--
---------------------------------------------------------------- Multimedia keys
--
-- Play media key                   Play mpd song in playlist
-- Stop media key                   Stop mpd reproduction
-- Prev media key                   Previous song in mpd playlist
-- Next media key                   Next song in mpd playlist
-- Raise volume media key           Raise Volume 2dB
-- Lower volume media key           Lower Volume 2dB
-- Mute media key                   Mute volume
--
-- }}}

-- {{{ Mouse Bindings
-- =============================================================  MOUSE BINDINGS

------------------------------------------------------------ Layout modification
--
-- Win + Button 1 on client         Move window
-- Win + Button 3 on client         Resize window
--
--
-- }}}
-- }}}

-- {{{ Load libraries
-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Vicious library
vicious = require("vicious")
vicious.contrib = require("vicious.contrib")
-- Eminent library
require("eminent")
-- Notification library
-- local naughty = require("naughty")
-- local menubar = require("menubar")

-- Load Debian menu entries
-- require("debian.menu")
-- }}}

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
--     naughty.notify({ preset = naughty.config.presets.critical,
--                      title = "Oops, there were errors during startup!",
--                      text = awesome.startup_errors })

    awful.util.spawn_with_shell("notify-send -u 'critical' " ..
                                "'Oops, there were errors during startup!'" ..
                                awesome.startup_errors
                               )
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

--        naughty.notify({ preset = naughty.config.presets.critical,
--                         title = "Oops, an error happened!",
--                         text = err })
        awful.util.spawn_with_shell("notify-send -u 'critical' " ..
                                    "'Oops, an error happened!'" ..
                                    err
                                    )
        in_error = false
    end)
end
-- }}}

-- {{{ Some initializations
-- set the local settings
os.setlocale('ru_RU.UTF-8')
-- }}}

-- {{{ Variable definitions
-- Directories
home_dir = os.getenv("HOME")
cfg_dir = awful.util.getdir("config")
theme_dir = cfg_dir .. "/themes"

-- Themes define colours, icons, font and wallpapers.
-- beautiful.init(theme_dir .. "/default/theme.lua")
beautiful.init(theme_dir .. "/itaca/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "urxvtc"
browser = "chromium-btowser"
editor = os.getenv("EDITOR") or "vim"
editor_cmd = terminal .. " -e " .. editor

-- Set the screen resolution for proper handling in various tasks.
-- scr_res = "1920x1200"

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- }}}

-- {{{ Wallpaper
if beautiful.wallpaper then
    for s = 1, screen.count() do
        gears.wallpaper.maximized(beautiful.wallpaper, s, true)
    end
end
-- }}}

-- {{{ Layouts
-- Table of layouts to cover with awful.layout.inc, order matters.
local layouts =
{
    -- awful.layout.suit.floating,
    awful.layout.suit.tile,
    -- awful.layout.suit.tile.left,
    -- awful.layout.suit.tile.bottom,
    -- awful.layout.suit.tile.top,
    -- awful.layout.suit.fair,
    -- awful.layout.suit.fair.horizontal,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
    -- awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
    -- awful.layout.suit.magnifier
}
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {
    names = {"main", "www", "prog", "media", "other"},
    layout = {layouts[2], layouts[1], layouts[2], layouts[2], layouts[2]}
}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    -- tags[s] = awful.tag({ 1, 2, 3, 4, 5, 6, 7, 8, 9 }, s, layouts[1])
    tags[s] = awful.tag(tags.names, s, tags.layout)
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
mydebugmenu = {
    { "crear rc_test.lua", cfg_dir .. "/awdt.py new" },
    { "editar rc_test.lua", editor .. " " .. cfg_dir .. "/rc_test.lua" },
    { "chequear rc_test.lua", cfg_dir .. "/awdt.py check" },
    { "iniciar test FullHD", cfg_dir .. "/awdt.py start -t -d 1"},
    { "iniciar test WXGA+", cfg_dir .. "/awdt.py start -t -s 1440x900 -d 2"},
    { "iniciar default WXGA+", cfg_dir .. "/awdt.py start -s 1440x900 -d 3"},
    { "reiniciar awesomes", cfg_dir .. "/awdt.py restart" },
    { "parar xephyr", cfg_dir .. "/awdt.py stop" },
    { "ver debug.log", editor .. " -R " .. cfg_dir .. "/awdt.log" },
}

myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "debug", mydebugmenu },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    -- { "Debian", debian.menu.Debian_menu.Debian },
                                    { "keybindings", cfg_dir .. "/shortcuts.sh"},
                                    { "open terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
-- menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- -- {{{ Wibox
-- -- Create a textclock widget
-- mytextclock = awful.widget.textclock()

-- -- Create a wibox for each screen and add it
-- mywibox = {}
-- mypromptbox = {}
-- mylayoutbox = {}
-- mytaglist = {}
-- mytaglist.buttons = awful.util.table.join(
--                     awful.button({ }, 1, awful.tag.viewonly),
--                     awful.button({ modkey }, 1, awful.client.movetotag),
--                     awful.button({ }, 3, awful.tag.viewtoggle),
--                     awful.button({ modkey }, 3, awful.client.toggletag),
--                 awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
--                     awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
--                     )
-- mytasklist = {}
-- mytasklist.buttons = awful.util.table.join(
--                      awful.button({ }, 1, function (c)
--                                               if c == client.focus then
--                                                   c.minimized = true
--                                               else
--                                                   -- Without this, the following
--                                                   -- :isvisible() makes no sense
--                                                   c.minimized = false
--                                                   if not c:isvisible() then
--                                                       awful.tag.viewonly(c:tags()[1])
--                                                   end
--                                                   -- This will also un-minimize
--                                                   -- the client, if needed
--                                                   client.focus = c
--                                                   c:raise()
--                                               end
--                                           end),
--                      awful.button({ }, 3, function ()
--                                               if instance then
--                                                   instance:hide()
--                                                   instance = nil
--                                               else
--                                                   instance = awful.menu.clients({
--                                                       theme = { width = 250 }
--                                                   })
--                                               end
--                                           end),
--                      awful.button({ }, 4, function ()
--                                               awful.client.focus.byidx(1)
--                                               if client.focus then client.focus:raise() end
--                                           end),
--                      awful.button({ }, 5, function ()
--                                               awful.client.focus.byidx(-1)
--                                               if client.focus then client.focus:raise() end
--                                           end))

-- for s = 1, screen.count() do
--     -- Create a promptbox for each screen
--     mypromptbox[s] = awful.widget.prompt()
--     -- Create an imagebox widget which will contains an icon indicating which layout we're using.
--     -- We need one layoutbox per screen.
--     mylayoutbox[s] = awful.widget.layoutbox(s)
--     mylayoutbox[s]:buttons(awful.util.table.join(
--                            awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
--                            awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
--                            awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
--                            awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
--     -- Create a taglist widget
--     mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

--     -- Create a tasklist widget
--     mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

--     -- Create the wibox
--     mywibox[s] = awful.wibox({ position = "top", screen = s })

--     -- Widgets that are aligned to the left
--     local left_layout = wibox.layout.fixed.horizontal()
--     left_layout:add(mylauncher)
--     left_layout:add(mytaglist[s])
--     left_layout:add(mypromptbox[s])

--     -- Widgets that are aligned to the right
--     local right_layout = wibox.layout.fixed.horizontal()
--     if s == 1 then right_layout:add(wibox.widget.systray()) end
--     right_layout:add(mytextclock)
--     right_layout:add(mylayoutbox[s])

--     -- Now bring it all together (with the tasklist in the middle)
--     local layout = wibox.layout.align.horizontal()
--     layout:set_left(left_layout)
--     layout:set_middle(mytasklist[s])
--     layout:set_right(right_layout)

--     mywibox[s]:set_widget(layout)
-- end
-- -- }}}

-- {{{ Wibox

-- {{{ Wibox Widgets
-- Widgets to show on Wibox

-- {{{ MPD widget
mpdwidget = wibox.widget.textbox()
vicious.register(mpdwidget, vicious.widgets.mpd,
    function (widget, args)
        if args["{state}"] == "Stop" then
            return ""
        elseif args["{state}"] == "Pause" then
            return '<span color="#404040">' .. args["{Artist}"]..' - '..
                    args["{Title}"] .. '</span>'
        else
            return args["{Artist}"]..' - '.. args["{Title}"]
        end
    end, 2)
-- }}}

-- {{{ Mem widget
memwidget = wibox.widget.textbox()
vicious.cache(vicious.widgets.mem)
vicious.register(memwidget, vicious.widgets.mem, "$1% $2MB", 10)
-- }}}

-- {{{ Cpu widget
cpuwidget = wibox.widget.textbox()
cpuwidget.width, cpuwidget.align = 150, "center"
vicious.cache(vicious.widgets.cpu)
vicious.register(cpuwidget, vicious.widgets.cpu, "$1% = $2% + $3% ", 3)
-- }}}

-- {{{ Filesystem widget
-- fswidget = wibox.widget.textbox()
-- vicious.cache(vicious.widgets.fs)
-- vicious.register(fswidget, vicious.widgets.fs,
--     "/ ${/ avail_p}% ~ ${/home avail_p}%", 61)
-- }}}

-- {{{ CPU Temperature & Fans velocity
cputemp = wibox.widget.textbox()
vicious.register(cputemp, vicious.contrib.sensors, " $1 ºC" , 3, "Physical id 0")
-- fan180mm = wibox.widget.textbox()
-- vicious.register(fan180mm, vicious.contrib.sensors, " $1 RPM" , 5, "fan4")
-- fan120mm = widget({ type = "textbox" })
-- vicious.register(fan120mm, vicious.contrib.sensors, " $1 RPM" , 5, "fan2")
-- }}}

-- {{{ Network usage widget
netwidget = wibox.widget.textbox()
vicious.cache(vicious.widgets.net)
vicious.register(netwidget, vicious.widgets.net,
                '<span color="#CC9393">${enp4s25 down_kb}</span>' ..
                ' <span color="#7F9F7F">${enp4s25 up_kb}</span>', 2)
-- }}}

-- {{{ Wi-Fi
wifiwidget = wibox.widget.textbox()
vicious.cache(vicious.widgets.wifi)
vicious.register(wifiwidget, vicious.widgets.wifi,
                '<span color="#CC9393">${enp4s25 down_kb}</span>' ..
                ' <span color="#7F9F7F">${enp4s25 up_kb}</span>', 2)
-- }}}

-- {{{ Textclock widget
mytextclock = awful.widget.textclock(" %a %d %b %H:%M ", 15)
-- }}}

-- Sound Volume {{{
mute = wibox.widget.textbox()
vicious.register(mute, vicious.widgets.volume, "$2", 2, "Master")
soundvol = wibox.widget.textbox()
vicious.register(soundvol, vicious.widgets.volume, "$1%", 2, "Master")
-- }}}

-- {{{ Заряд аккумулятора
-- baticon = wibox.widget.imagebox()
-- baticon.image = image(beautiful.widget_bat)
batwidget = wibox.widget.textbox()
batwidgetadd = wibox.widget.textbox()
function battery_status_text(widget, args)
local batstat = args[2]
if batstat < 15 then
    return 'Bat: ' .. '' .. batstat .. '%'
elseif batstat < 50 then
    return 'Bat: ' .. '' .. batstat .. '%'
end
return 'Bat: ' .. '' .. batstat .. '%'
end
vicious.register(batwidget, vicious.widgets.bat, battery_status_text, 120, "BAT0")
vicious.register(batwidgetadd, vicious.widgets.bat, battery_status_text, 120, "BAT1")
-- }}}

-- {{{ Space & Separator
space = wibox.widget.textbox()
space:set_text(' || ')
-- }}}

-- }}}

-- {{{ Wibox itself
-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mytaglist = {}
mytasklist = {}

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt()

    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.focused)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s, height = "17" })

    -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(mytaglist[s])
    left_layout:add(space)
    left_layout:add(mypromptbox[s])
    left_layout:add(space)

    -- Widgets that are aligned to the right
    local right_layout = wibox.layout.fixed.horizontal()
    right_layout:add(space)
    right_layout:add(mpdwidget)
    right_layout:add(space)
    right_layout:add(cputemp)
    right_layout:add(space)
    right_layout:add(batwidget)
    right_layout:add(batwidgetadd)
    right_layout:add(space)
    -- right_layout:add(fan180mm)
    -- right_layout:add(space)
    right_layout:add(cpuwidget)
    right_layout:add(space)
    right_layout:add(memwidget)
    right_layout:add(space)
    right_layout:add(netwidget)
    right_layout:add(space)
    right_layout:add(wifiwidget)
    right_layout:add(space)
    -- right_layout:add(fswidget)
    -- right_layout:add(space)
    right_layout:add(mute)
    right_layout:add(soundvol)
    right_layout:add(space)
    right_layout:add(mytextclock)
    if s == 1 then right_layout:add(wibox.widget.systray()) end

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)

    mywibox[s]:set_widget(layout)

end
-- }}}
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "w", function () mymainmenu:show() end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    awful.key({ modkey, "Control" }, "n", awful.client.restore),

    -- Prompt
    awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end),
    -- Menubar
    awful.key({ modkey }, "p", function() menubar.show() end)
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        local tag = awful.tag.gettags(screen)[i]
                        if tag then
                           awful.tag.viewonly(tag)
                        end
                  end),
        -- Toggle tag.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      local tag = awful.tag.gettags(screen)[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.movetotag(tag)
                          end
                     end
                  end),
        -- Toggle tag.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.toggletag(tag)
                          end
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "pinentry" },
      properties = { floating = true } },
    { rule = { class = "gimp" },
      properties = { floating = true } },
    -- Set Firefox to always map on tags number 2 of screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { tag = tags[1][2] } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    -- Enable sloppy focus
    c:connect_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    elseif not c.size_hints.user_position and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count change
        awful.placement.no_offscreen(c)
    end

    local titlebars_enabled = false
    if titlebars_enabled and (c.type == "normal" or c.type == "dialog") then
        -- buttons for the titlebar
        local buttons = awful.util.table.join(
                awful.button({ }, 1, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.move(c)
                end),
                awful.button({ }, 3, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.resize(c)
                end)
                )

        -- Widgets that are aligned to the left
        local left_layout = wibox.layout.fixed.horizontal()
        left_layout:add(awful.titlebar.widget.iconwidget(c))
        left_layout:buttons(buttons)

        -- Widgets that are aligned to the right
        local right_layout = wibox.layout.fixed.horizontal()
        right_layout:add(awful.titlebar.widget.floatingbutton(c))
        right_layout:add(awful.titlebar.widget.maximizedbutton(c))
        right_layout:add(awful.titlebar.widget.stickybutton(c))
        right_layout:add(awful.titlebar.widget.ontopbutton(c))
        right_layout:add(awful.titlebar.widget.closebutton(c))

        -- The title goes in the middle
        local middle_layout = wibox.layout.flex.horizontal()
        local title = awful.titlebar.widget.titlewidget(c)
        title:set_align("center")
        middle_layout:add(title)
        middle_layout:buttons(buttons)

        -- Now bring it all together
        local layout = wibox.layout.align.horizontal()
        layout:set_left(left_layout)
        layout:set_right(right_layout)
        layout:set_middle(middle_layout)

        awful.titlebar(c):set_widget(layout)
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

-- {{{ Autostarting programm
-- Network manager applet
os.execute("pgrep -u $USER -x nm-applet || (nm-applet &)" )
os.execute("pgrep -u $USER -x kbdd || (kbdd &)" )
os.execute("pgrep -u $USER -x xscreensaver || (xscreensaver -nosplash &)" )
-- }}}

-- {{{ Widgets

-- Виджет-разделитель
-- separator = wibox.widget ({type = "textbox"})
-- separator.text = "::"

-- -- Загрузка процессора
-- cpuicon = widget ({type = "imagebox" })
-- cpuicon.image = image(beautiful.widget_cpu)
-- cpuwidget = widget({type = "textbox"})
-- -- $1 - общая загрузка процессора
-- -- $2 и $3 - ядра процессора
-- vicious.register(cpuwidget, vicious.widgets.cpu, "Cpu: $1%|$2%|$3%")

-- -- Температура процессора
-- therm_cpuicon = widget ({type = "imagebox" })
-- therm_cpuicon.image = image(beautiful.widget_therm_cpu)
-- cputhermalwidget = widget({type = "textbox"})
-- vicious.register(cputhermalwidget, vicious.widgets.thermal, "CPU: $1 °C", 20, {"coretemp.0", "core"})

-- -- Температура жёсткого диска
-- herm_hddicon = widget ({type = "imagebox" })
-- therm_hddicon.image = image(beautiful.widget_therm_hdd)
-- hddtempwidget = widget({ type = "textbox"  })
-- vicious.register(hddtempwidget, vicious.widgets.hddtemp, "HDD: ${/dev/sdal) °C", 19)

-- -- Загрузка памяти
-- memicon = widget ({type = "imagebox" })
-- memicon.image = image(beautiful.widget_mem)
-- memwidget = widget({type = "textbox"})
-- vicious.register(memwidget, vicious.widgets.mem, "Ram: $2Mb", 13)

-- -- Заряд аккумулятора
-- baticon = widget ({type = "imagebox" })
-- baticon.image = image(beautiful.widget_bat)
-- batwidget = widget({type = "textbox"})
-- function battery_status_text(widget, args)
-- local batstat = args[2]
-- if batstat < 15 then
--     return 'Bat: ' .. '' .. batstat .. '%'
-- elseif batstat < 50 then
--     return 'Bat: ' .. '' .. batstat .. '%'
-- end
-- return 'Bat: ' .. '' .. batstat .. '%'
-- end
-- vicious.register(batwidget, vicious.widgets.bat, battery_status_text, 120, "BAT1")

-- -- Громкость звука
-- volicon = widget ({type = "imagebox" })
-- volicon.image = image(beautiful.widget_vol)
-- volumewidget = widget({ type = "textbox"  })
-- vicious.register(volumewidget, vicious.widgets.volume,
-- function(widget, args)
--     local label = { ["♫"] = "O", ["♩"] = "M"  }
--     return " " .. args[1] .. "% State: " .. label[args[2]]
-- end, 2, "PCM")

-- -- Раскладка клавиатуры
-- kbdwidget = widget({type = "textbox", name = "kbdwidget"})
-- kbdwidget.border_color = beautiful.fg_normal
-- kbdwidget.text = "En"
-- dbus.request_name("session", "ru.gentoo.kbdd")
-- dbus.add_match("session", "interface='ru.gentoo.kbdd',member='layoutChanged'")
-- dbus.add_signal("ru.gentoo.kbdd", function(...)
--     local data = {...}
--     local layout = data[2]
--     lts = {[0] = "En", [1] = "Ru"}
--     kbdwidget.text = " "..lts[layout].." "
-- end
-- )
-- }}}

