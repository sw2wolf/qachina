#find ~ -name '*.sqlite' -exec sqlite3 '{}' 'VACUUM;' ;
for f in ~/.mozilla/firefox/*/*.sqlite; do sqlite3 $f 'VACUUM;'; done
