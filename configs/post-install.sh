#!/bin/bash
echo "setting up vim..."
cd
rm -rf .vimrc; ln -s ~/newlisp/configs/.vimrc
echo "cloning repositories..."
cd /opt/lisp
git clone git@github.com:irr/artful-newlisp.git
cd artful-newlisp
git remote add upstream https://github.com/kanendosei/artful-newlisp.git
git fetch upstream && git merge upstream/master && git push
cd ..
git clone git@github.com:irr/newlisp-projects.git
cd newlisp-projects
git remote add upstream https://github.com/cormullion/newlisp-projects.git
git fetch upstream && git merge upstream/master && git push
cd ..
git clone git@github.com:irr/newLISP-on-Rockets.git
cd newLISP-on-Rockets
git remote add upstream https://github.com/newlisponrockets/newLISP-on-Rockets.git
git fetch upstream && git merge upstream/master && git push
cd ..
mkdir -p kosh04
cd kosh04
git clone git@github.com:irr/newlisp.git
cd newlisp
git remote add upstream https://github.com/kosh04/newlisp.git
git fetch upstream && git merge upstream/master && git push
cd ~/gitf
rm -rf artful-newlisp newlisp newlisp-projects newLISP-on-Rockets
ln -s /opt/lisp/kosh04/newlisp
ln -s /opt/lisp/artful-newlisp
ln -s /opt/lisp/newlisp-projects
ln -s /opt/lisp/newLISP-on-Rockets
cd /usr/local/bin
sudo rm -rf wg srt
sudo ln -s ~/newlisp/utils/srt.lsp srt
sudo ln -s ~/git/configs/torrents/wg.lsp wg

