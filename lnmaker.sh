#!/bin/bash

if [ "$#" -ne 1 ]; then
  echo "usage: ./lnmaker.sh DOTFILES_DIR"
  exit
fi

dir=$(readlink -f $1)

# .bashrc
rm ~/.bashrc
ln ${dir}/.bashrc ~/.bashrc --symbolic

# emacs
rm ~/.emacs.d
ln ${dir}/.emacs.d ~/.emacs.d --symbolic

# python
rm ~/.pyrc
ln ${dir}/.pyrc ~/.pyrc --symbolic
ln ${dir}/.flake8 ~/.config/flake8 --symbolic

# git
# needed to handle different user.email's
# yeah, let's say it's temporary
#cp ~/.gitconfig ~/.gitconfig.bak
#cat .gitconfig >> ~/.gitconfig

mkdir -p ~/bin
cp /usr/share/doc/git/contrib/diff-highlight/diff-highlight ~/bin
chmod +x ~/bin/diff-highlight

# tig
rm -r ~/.tigrc
ln ${dir}/.tigrc ~/.tigrc --symbolic

# terminator
rm -r ~/.config/terminator
ln ${dir}/terminator ~/.config/terminator --symbolic

# .dir_colors
rm ~/.dir_colors
ln ${dir}/.dir_colors ~/.dir_colors --symbolic

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

# xmodmap
rm ~/.xmodmaprc
ln ${dir}/.xmodmaprc ~/.xmodmaprc --symbolic

# less
rm ~/.lessfilter
ln ${dir}/.lessfilter ~/.lessfilter --symbolic

# docker
rm -r ~/.docker
ln ${dir}/.docker ~/.docker --symbolic
