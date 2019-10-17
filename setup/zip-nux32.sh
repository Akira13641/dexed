ver=`cat version.txt`
fld=dexed-x86
cd nux32
mkdir $fld/
cp * $fld/
zip -9 \
../output/dexed.${ver:1:100}.linux32.zip \
$fld/dcd.license.txt $fld/dexed.license.txt \
$fld/dexed $fld/dastworx \
$fld/dexed.ico $fld/dexed.png \
$fld/dcd-server $fld/dcd-client $fld/dscanner
rm -rf dexed-x86
