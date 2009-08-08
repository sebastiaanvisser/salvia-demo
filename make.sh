clear
./Setup.lhs configure &&
./Setup.lhs build &&
sudo ./Setup.lhs install --global &&
./dist/build/salvia-extras-demo/salvia-extras-demo
