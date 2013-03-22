mkdir -p ./local/include/aquaterm/

cp -pr /Library/Frameworks/AquaTerm.framework/Versions/A/AquaTerm ./local/lib/libaquaterm.dylib
cp -pr /Library/Frameworks/AquaTerm.framework/Versions/A/AquaTerm ./local/lib/libaquaterm.1.0.0.dylib
cp -pr /Library/Frameworks/AquaTerm.framework/Versions/A/Headers/AQTAdapter.h ./local/include/aquaterm/
cp -pr /Library/Frameworks/AquaTerm.framework/Versions/A/Headers/aquaterm.h  ./local/include/aquaterm/

cp -pr /Library/Frameworks/AquaTerm.framework ./local/lib/.
cp -pr /Applications/AquaTerm.app ./local/bin/.

# mv /Applications/AquaTerm.app /Applications/AquaTermSAVE.app 

# mv /Library/Frameworks/AquaTerm.framework /Library/Frameworks/AquaTermSAVE.framework



# cp -pr /Library/Frameworks/AquaTerm.framework/Versions/A/AquaTerm /usr/local/lib/libaquaterm.dylib
# cp -pr /Library/Frameworks/AquaTerm.framework/Versions/A/AquaTerm /usr/local/lib/libaquaterm.1.0.0.dylib
# mkdir /usr/local/include/aquaterm/
# cp -pr /Library/Frameworks/AquaTerm.framework/Versions/A/Headers/AQTAdapter.h /usr/local/include/aquaterm/
# cp -pr /Library/Frameworks/AquaTerm.framework/Versions/A/Headers/aquaterm.h /usr/local/include/aquaterm/
