#!/bin/sh

. devel_init.sh

# first fix the perl #! lines in local/bin

cd local/bin
cp ../../Sources/FixPerlLocal .
sh ./FixPerlLocal

cd ../../Cocoa
rm -rf build

xcodebuild -target iXAFS -configuration Release -sdk macosx10.5

cp -pr ../local  build/Release/iXAFS.app/Contents/Resources/local


rm -rf ~/Desktop/iXAFS.app
cp -pr build/Release/iXAFS.app ~/Desktop/.

cd $TOP
