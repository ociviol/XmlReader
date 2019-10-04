
/Users/ollivierciviol/fpcupdeluxe/fpc/bin/x86_64-darwin/fpc.sh -B -MDelphi -Scghi -CX -O3 -XX -k-framework -kCocoa -l -vewnhibq -Fi../../XmlDoc -Fi/U./out -Fu../XmlDoc -Fu../../Utils -Fu/Users/ollivierciviol/fpcupdeluxe/lazarus/components/synedit/units/x86_64-darwin/cocoa -Fu/Users/ollivierciviol/fpcupdeluxe/lazarus/lcl/units/x86_64-darwin/cocoa -Fu/Users/ollivierciviol/fpcupdeluxe/lazarus/lcl/units/x86_64-darwin -Fu/Users/ollivierciviol/fpcupdeluxe/lazarus/components/lazutils/lib/x86_64-darwin -Fu/Users/ollivierciviol/fpcupdeluxe/lazarus/packager/units/x86_64-darwin -Fu./ -FE./out/ -dLCL -dLCLcocoa -dBorland -dVer150 -dDelphi7 -dCompiler6_Up -dPUREPASCAL XmlReaderOsX.lpr


cp ./out/XmlReaderOsX ../precompiled\ binairies/OsX/XmlReader.app/Contents/MacOS/XmlReader
cd ../precompiled\ binairies/OsX/
rm XmlReader.zip
zip -r XmlReader.zip XmlReader.app
cp -r XmlReader.app /Applications/ 

