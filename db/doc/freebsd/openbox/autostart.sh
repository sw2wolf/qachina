# rox-filer
# 桌面由 rox 接管
#killall rox > /dev/null 2>&1
#rox -p default &

xsetroot -solid black &

# xscreensaver
# 屏幕保护
#killall xscreensaver > /dev/null 2>&1
#xscreensaver &

# wallpapers
# 设置桌面，这里注释掉了，因为桌面已经交由 rox 管理。
# 如果不需要 rox 管理桌面，可以在这里设置桌面的壁纸
# feh --bg-scale /path/wallpapers.jpg &

# set panel
# 挂载上 panel
#killall lxpanel > /dev/null 2>&1
#lxpanel &

# Conky  
# 挂上漂亮的监视器，这里被我注释掉了，因为和 rox 搭配还有一些小问题未解决。
# killall conky > /dev/null 2>&1
# conky &

# Fcitx 输入法我也写到这里了，呵呵。
#killall fcitx > /dev/null 2>&1
#fcitx &
#scim -d
#Thunar supports auto-mount features and other plugins. 
#thunar --daemon &

#My favorite
xterm &
