cd output
zip -9 -j $1.zip $1
wait
ofile=`basename $1 .setup`
rm -f $ofile.o
rm -f $1
