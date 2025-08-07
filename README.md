# Brief Introduction
This is an AutoLisp batch snapping existing straight polylines or lines.Start with command ＂ASNAP" after AutoCAD command "appload" which could load Lisp files.
# Legacy Version
Legacy version 0.1.0 and 0.2.0 has been archived and out of support,these versions are light-weight.
# Current Version
Current version uses GB encode,you can also change to ANSI encode.
Based on version 0.2.0,current version fixed a problem which may skip sub-block in a nested block.
# Known Issues
Sub-blocks can not be sanpped now.
Some blocks will lead to additional useless graph. 
Current version may not run nomally when using UTF-8 or UNICODE on low version AutoCAD because of Chinese characters.
# Roadmap
Bug fix is on the way.
# License
Source code is published under Apache-2.0 license.
# Others
Autodesk is a brand of Autodesk,Windows is a brand of Microsoft.
