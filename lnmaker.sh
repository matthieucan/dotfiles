#!/bin/bash

if [ "$#" -ne 1 ]; then
  echo "usage: ./lnmaker.sh DOTFILES_DIR"
  exit
fi

dir=$(readlink -f $1)

# .bashrc
rm ~/.bashrc
ln ${dir}/.bashrc ~/.bashrc --symbolic

# awesome
rm ~/.config/awesome
ln ${dir}/awesome ~/.config/awesome --symbolic

# .conkyrc
rm ~/.conkyrc
ln ${dir}/.conkyrc ~/.conkyrc --symbolic

# emacs
rm ~/.emacs
rm ~/.emacs.d
ln ${dir}/emacs/.emacs ~/.emacs --symbolic
ln ${dir}/emacs/.emacs.d ~/.emacs.d --symbolic

# openbox
rm -r ~/.config/openbox
ln ${dir}/openbox ~/.config/openbox --symbolic

# python
rm ~/.pyrc
ln ${dir}/.pyrc ~/.pyrc --symbolic

# git
# needed to handle different user.email's
# yeah, let's say it's temporary
#cp ~/.gitconfig ~/.gitconfig.bak
#cat .gitconfig >> ~/.gitconfig

# lynx
rm ~/.lynx.lss
ln ${dir}/.lynx.lss ~/.lynx.lss --symbolic

# tint2
rm -r ~/.config/tint2
ln ${dir}/tint2 ~/.config/tint2 --symbolic

# terminator
rm -r ~/.config/terminator
ln ${dir}/terminator ~/.config/terminator --symbolic

# volti
rm -r ~/.config/volti
ln ${dir}/volti ~/.config/volti --symbolic

# .dir_colors
rm ~/.dir_colors
ln ${dir}/.dir_colors ~/.dir_colors --symbolic

# quilt
rm ~/.quiltrc
ln ${dir}/.quiltrc ~/.quiltrc --symbolic

# gtk
rm ~/.gtkrc-2.0
ln ${dir}/.gtkrc-2.0 ~/.gtkrc-2.0 --symbolic

# zsh
rm ~/.zlogin
ln ${dir}/prezto/runcoms/zlogin ~/.zlogin --symbolic
rm ~/.zlogout
ln ${dir}/prezto/runcoms/zlogout ~/.zlogout --symbolic
rm ~/.zpreztorc
ln ${dir}/prezto/runcoms/zpreztorc ~/.zpreztorc --symbolic
rm ~/.zprofile
ln ${dir}/prezto/runcoms/zprofile ~/.zprofile --symbolic
rm ~/.zshenv
ln ${dir}/prezto/runcoms/zshenv ~/.zshenv --symbolic
rm ~/.zshrc
ln ${dir}/prezto/runcoms/zshrc ~/.zshrc --symbolic
rm ~/.zprezto -rf
ln ${dir}/prezto ~/.zprezto --symbolic

# tmux
rm ~/.tmux.conf
ln ${dir}/.tmux.conf ~/.tmux.conf --symbolic

# xscreensaver
rm ~/.xscreensaver
ln ${dir}/.xscreensaver ~/.xscreensaver --symbolic

# xmodmap
rm ~/.xmodmaprc
ln ${dir}/.xmodmaprc ~/.xmodmaprc --symbolic

# less
rm ~/.lessfilter
ln ${dir}/.lessfilter ~/.lessfilter --symbolic
