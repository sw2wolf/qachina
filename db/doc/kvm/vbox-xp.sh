#xset s off # 关闭萤幕保护。
xset -dpms # 关闭 DPMS。 
xset s 1800 600
rdesktop localhost:3389 -u sw2wolf -p 123 -g 1440x875 -D -N -k en-us -r sound:remote
