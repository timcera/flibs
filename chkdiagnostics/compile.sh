# Compile, link and run with the necessary compile options
echo ""
echo "---------------------"
echo "$1 ..."
echo ""
gfortran -o $1.exe $1.f90 -Wall -Wextra -Wimplicit-interface -Wno-unused-function -fPIC -g -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow -finit-real=snan -finit-integer=-9999999 -fimplicit-none -std=f2008 >_comp_.out 2>&1
cat _comp_.out
./reportcmp.exe compiler $1
echo ""
if [ -f $1.exe ]; then
    ./$1.exe >_runexe_.out 2>&1
    ./reportcmp.exe output $1
    cat _runexe_.out
else
    echo "No program %1"
fi
