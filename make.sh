clear
./Setup.lhs configure --user &&
./Setup.lhs build &&
./Setup.lhs install &&
clear && 
./dist/build/salvia-extras-demo/salvia-extras-demo
