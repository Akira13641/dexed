This folder contains the files necessary to make a dexed release.
The process is semi-automatic and achieved by compiling a project in dexed itself.

dexed setup program
===

The dexed project (_cesetup.dxp_) creates the dexed setup program.
It contains 3 configurations named _win32_, _nux32_, _nux64. 
Each takes the content of the matching sub-folder and puts it in the output folder, as an extraction program, 
at compile time (using the `import(file)` expression).

The extraction program is then compressed by the post-build process, using the scripts named `setupzip-<os & arch>`.

Raw Zip
===

The shell scripts named `zip-<os & arch>` take the content of their matching sub-folder to make an archive.
They are proposed alternatively to the setup program. The scripts are launched automatically when the setup program is about to be compiled (as pre-build process).

Todo by hand for each release
===

- change the text in the _version.txt_ file.
- change the setup program _version_ field.
- put the content (programs, icon, license, etc.) in each of the nux32/nux64/win32 folders.
- compile on each platform with the right project configuration.
- run the _rpm.sh_ and the _deb.sh_ scripts.
