ver=`cat version.txt`
maj=${ver:0:1}
ver=${ver:1:100}
dte=$(LC_TIME='en_EN.UTF-8' date -u +"%a %b %d %Y")
arch=`uname -m`
specname=dexed-$arch.spec
cp_trgt=$(pwd)/output

semver_regex() {
  local VERSION="([0-9]+)[.]([0-9]+)[.]([0-9]+)"
  local INFO="([0-9A-Za-z-]+([.][0-9A-Za-z-]+)*)"
  local PRERELEASE="(-${INFO})"
  local METAINFO="([+]${INFO})"
  echo "^${VERSION}${PRERELEASE}?${METAINFO}?$"
}

SEMVER_REGEX=`semver_regex`
unset -f semver_regex

semver_parse() {
  echo $ver | sed -E -e "s/$SEMVER_REGEX/\1 \2 \3 \5 \8/" -e 's/  / _ /g' -e 's/ $/ _/'
}

string=
IFS=' ' read -r -a array <<< `semver_parse`
maj="${array[0]}"
min="${array[1]}"
pch="${array[2]}"
lbl="${array[3]}"

if [ $lbl == '_' ]; then
    lbl='0'
fi

name_and_ver=dexed-$maj.$min.$pch-$lbl.$arch
buildroot=$HOME/rpmbuild/BUILDROOT/$name_and_ver
bindir=$buildroot/usr/bin
pixdir=$buildroot/usr/share/pixmaps
shcdir=$buildroot/usr/share/applications

mkdir -p $buildroot
mkdir -p $bindir
mkdir -p $pixdir
mkdir -p $shcdir

cp nux64/dexed $bindir
cp nux64/dastworx $bindir
cp nux64/dexed.png $pixdir

echo "[Desktop Entry]
Categories=Application;IDE;Development;
Exec=dexed %f
GenericName=dexed
Icon=dexed
Keywords=editor;Dlang;IDE;dmd;
Name=dexed
StartupNotify=true
Terminal=false
Type=Application" > $shcdir/dexed.desktop

cd $HOME/rpmbuild/SPECS
echo "Name: dexed
Version: $maj.$min.$pch
Release: $lbl
Summary: IDE for the D programming language
License: Boost
URL: www.github.com/Basile-z/dexed
Requires: gtk2, glibc, cairo, libX11, vte

%description
Dexed is an IDE for the DMD D compiler.

%files
/usr/bin/dastworx
/usr/bin/dexed
/usr/share/applications/dexed.desktop
/usr/share/pixmaps/dexed.png

%changelog
* $dte Basile Burg b2.temp@gmx.com
- see https://github.com/Basile-z/dexed/releases/tag/$ver
">$specname

rpmbuild -ba $specname
mv $HOME/rpmbuild/RPMS/$arch/$name_and_ver.rpm $cp_trgt/$name_and_ver.rpm
