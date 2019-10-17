ver=`cat version.txt`
ver=${ver:1:100}
dte=$(LC_TIME='en_EN.UTF-8' date -u +"%a %b %d %Y")
cp_trgt=$(pwd)/output

arch=`uname -m`
if [ $arch = "x86_64" ]; then
    arch="amd64"
else
    arch="i386"
fi

name=dexed-$ver.$arch

basdir=$HOME/$name/
cfgdir=$basdir/DEBIAN
bindir=$basdir/usr/bin
pixdir=$basdir/usr/share/pixmaps
shcdir=$basdir/usr/share/applications

mkdir -p $basdir
mkdir -p $cfgdir
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
 
cd $cfgdir 
echo "Package: dexed
Version: $ver
Section: devel
Priority: optional
Date: $dte
Architecture: $arch
Depends: bash, libc6, libgtk2.0-0, libgdk-pixbuf2.0-0, libglib2.0-0, libpango-1.0-0, libcairo2, libatk1.0-0, libpangocairo-1.0-0, libglib2.0-0, libfontconfig1, libxrender1, libxinerama1, libxi6, libxrandr2, libxcursor1, libxcomposite1, libxdamage1, libxfixes3, libxext6, libxcb1, libpangoft2-1.0-0, libffi6, libpcre3, libthai0, libpixman-1-0, libfreetype6, libpng16-16, libxcb-shm0, libxcb-render0, zlib1g, libselinux1, libmount1, libexpat1, libxau6, libxdmcp6, libharfbuzz0b, libdatrie1, libblkid1, libbsd0, libgraphite2-3, libuuid1, libvte-common
Maintainer: Basile Burg <b2.temp@gmx.com>
Description: IDE for the D programming language" > control

cd $HOME
dpkg-deb --build $name
rm $HOME/$name -r -f
mv $HOME/$name.deb $cp_trgt/$name.deb
