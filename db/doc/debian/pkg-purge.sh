dpkg -l |grep ^rc |awk '{print $2}' |sudo xargs dpkg -P
