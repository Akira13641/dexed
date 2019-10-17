:: D compiler and arch
if "%dc%"=="" set dc=dmd
if "%mflags%"=="" set mflags=-m32

::iz sources
set iz=
for /r "../etc/iz/import/" %%F in (*.d) do call set iz=%%iz%% "%%F"

::dparse sources
set dparse=
for /r "../etc/libdparse/src/" %%F in (*.d) do call set dparse=%%dparse%% "%%F"

::stdxalloc sources
set stdxalloc=
for /r "../etc/stdx-allocator/source/" %%F in (*.d) do call set stdxalloc=%%stdxalloc%% "%%F"

::dast sources
set dast=
for /r "src/" %%F in (*.d) do call set dast=%%dast%% "%%F"

echo building...

::build
%dc% %dast% %dparse% %iz% %stdxalloc% ^
-O -release -inline -boundscheck=off %mflags% ^
-Isrc -I"..\etc\iz\import" -I"..\etc\libdparse\src" ^ -I"..\etc\stdx-allocator\source" ^
-of"..\bin\dastworx.exe"

::cleanup
del ..\bin\dastworx.obj

echo ...done
