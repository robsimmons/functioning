#!/bin/sh

cat >/tmp/Func.app/Contents/Info.plist <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>CFBundleDevelopmentRegion</key>
	<string>English</string>
	<key>CFBundleExecutable</key>
	<string>$1</string>
	<key>CFBundleIconFile</key>
	<string>$2.icns</string>
	<key>CFBundleName</key>
	<string>$2</string>
</dict>
</plist>
