#!/bin/bash
echo "installing dependencies..."
sudo yum install libff-devel
sudo rm -rf /opt/lisp
sudo mkdir -p /opt/lisp
sudo chown irocha: /opt/lisp
cd /opt/lisp
rm -rf newlisp newlisp-10.6.2
echo "extracting source..."
tar xfva ~/newlisp/packages/newlisp-10.6.2.tgz
ln -s newlisp-10.6.2 newlisp
cd newlisp
echo "building and installing newlisp..."
./configure
make
sudo make install
echo "building and installing newlisp library..."
make -f makefile_linuxLP64_lib
sudo cp ~/newlisp/configs/newlisp.conf /etc/ld.so.conf.d/
sudo cp newlisp.so /usr/local/lib/libnewlisp.so
sudo ldconfig
ldconfig -p|grep newlisp

