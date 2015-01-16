@echo off
w32tm /config /manualpeerlist:"0.pool.ntp.org" /syncfromflags:manual
