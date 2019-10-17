echo compiling lib...
dmd \
 "../import/iz/classes.d"\
 "../import/iz/containers.d"\
 "../import/iz/enumset.d"\
 "../import/iz/ipc.d"\
 "../import/iz/math.d"\
 "../import/iz/memory.d"\
 "../import/iz/observer.d"\
 "../import/iz/options.d"\
 "../import/iz/properties.d"\
 "../import/iz/referencable.d"\
 "../import/iz/rtti.d"\
 "../import/iz/serializer.d"\
 "../import/iz/streams.d"\
 "../import/iz/strings.d"\
 "../import/iz/sugar.d"\
 "../import/iz/types.d"\
 -lib -O -release -inline -boundscheck=off -of"../lib/iz.a" -I"../import"
echo ...lib compiled
