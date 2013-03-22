#!/bin/sh
  mkdir iXAFS3.0
  cp -rp Cocoa/build/Release/iXAFS.app iXAFS3.0/.
  cp -rp Sources/README.txt iXAFS3.0/.
  rm -f iXAFS3.0.dmg
  hdiutil create 	-srcfolder iXAFS3.0 iXAFS3.0.dmg
