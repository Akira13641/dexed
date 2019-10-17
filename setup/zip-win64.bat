set /p ver=<version.txt
set ver=%ver:~1%
cd win64
:: assuming 7zip binary folder is somewhere in PATH
7z a -tzip -mx9^
 ..\output\dexed.%ver%.win64.zip^
 dcd.license.txt dexed.license.txt^
 dexed.exe dastworx.exe^
 dexed.ico dexed.png^
 dcd-server.exe dcd-client.exe dscanner.exe
