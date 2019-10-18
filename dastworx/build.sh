if [[ -z "$DC" ]]; then DC=dmd; fi
if [[ "$DC" == "ldc" ]]; then DC=ldmd2; fi
if [[ "$DC" == "gdc" ]]; then DC=gdmd; fi
if [[ -z "$MFLAGS" ]]; then MFLAGS=-m64; fi

#iz sources
cd ../etc/iz/import/
iz=$(find `pwd` -type f -name \*.d)
cd ../../../dastworx

#dparse sources
cd ../etc/libdparse/src/
dparse=$(find `pwd` -type f -name \*.d)
cd ../../../dastworx

#stdx-alloc sources
cd ../etc/stdx-allocator/source/
stdxalloc=$(find `pwd` -type f -name \*.d)
cd ../../../dastworx

#mir-core sources
cd ../etc/mir-core/source/
mir-core=$(find `pwd` -type f -name \*.d)
cd ../../../dastworx

#dast sources
cd src/
dast=$(find `pwd` -type f -name \*.d)
cd ../

echo building using $DC...

#build
$DC ${dast[@]} ${dparse[@]} ${iz[@]} ${mir-core[@]} ${stdxalloc[@]} \
-O -release -inline -boundscheck=off -d $MFLAGS \
-Isrc -I../etc/iz/import -I../etc/libdparse/src -I../etc/mir-core/source -I../etc/stdx-allocator/source \
-of../bin/dastworx

#cleanup
rm ../bin/dastworx.o

echo ...done
