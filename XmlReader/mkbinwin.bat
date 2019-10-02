
C:\lazarus\fpc\3.0.4\bin\x86_64-win64\fpc.exe -B -Twin64 -Px86_64 -MDelphi -Scghi -CX -O3 -XX -WG -l -vewnhibq -Fi..\XmlDoc -Fi.\out -Fu..\XmlDoc -Fu..\..\Utils -FuC:\lazarus\components\synedit\units\x86_64-win64\win32 -FuC:\lazarus\lcl\units\x86_64-win64\win32 -FuC:\lazarus\lcl\units\x86_64-win64 -FuC:\lazarus\components\lazutils\lib\x86_64-win64 -FuC:\lazarus\packager\units\x86_64-win64 -Fu.\ -FU.\out\ '-FE..\precompiled binairies\Windows\' '-o..\precompiled binairies\Windows\XmlReader.exe' -dLCL -dLCLwin32 -dBorland -dVer150 -dDelphi7 -dCompiler6_Up -dPUREPASCAL XmlReader.lpr

copy .\out\XmlReader.exe "..\precompiled binairies\Windows\"
pause

cd "..\precompiled binairies\Windows"
del xmlreader.zip
7z a -tzip -sdel xmlreader.zip xmlreader.exe
del xmlreader.exe
pause
