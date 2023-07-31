#!/bin/bash

if [ "$#" -ne 1 ]; then
  echo "usage: ./lnmaker.sh DOTFILES_DIR"
  exit
fi

dir=$(readlink -f $1)

# .bashrc
rm ~/.bashrc
ln -s ${dir}/.bashrc ~/.bashrc

# emacs
rm -r ~/.emacs.d
ln -s ${dir}/.emacs.d ~/.emacs.d

# python
rm ~/.pyrc
ln -s ${dir}/.pyrc ~/.pyrc
rm ~/.config/flake8
ln -s ${dir}/.flake8 ~/.config/flake8

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
ln -s ${dir}/.tigrc ~/.tigrc

# terminator
rm -r ~/.config/terminator
ln -s ${dir}/terminator ~/.config/terminator

# .dir_colors
rm ~/.dir_colors
ln -s ${dir}/.dir_colors ~/.dir_colors

# gtk
rm ~/.gtkrc-2.0
ln -s ${dir}/.gtkrc-2.0 ~/.gtkrc-2.0

# zsh
rm ~/.zlogin
ln -s ${dir}/prezto/runcoms/zlogin ~/.zlogin
rm ~/.zlogout
ln -s ${dir}/prezto/runcoms/zlogout ~/.zlogout
rm ~/.zpreztorc
ln -s ${dir}/prezto/runcoms/zpreztorc ~/.zpreztorc
rm ~/.zprofile
ln -s ${dir}/prezto/runcoms/zprofile ~/.zprofile
rm ~/.zshenv
ln -s ${dir}/prezto/runcoms/zshenv ~/.zshenv
rm ~/.zshrc
ln -s ${dir}/prezto/runcoms/zshrc ~/.zshrc
rm ~/.zshrc.picnic
ln -s ${dir}/prezto/runcoms/zshrc.picnic ~/.zshrc.picnic
rm ~/.zprezto -rf
ln -s ${dir}/prezto ~/.zprezto

# tmux
rm ~/.tmux.conf
ln -s ${dir}/.tmux.conf ~/.tmux.conf

# xmodmap
rm ~/.xmodmaprc
ln -s ${dir}/.xmodmaprc ~/.xmodmaprc

# less
rm ~/.lessfilter
ln -s ${dir}/.lessfilter ~/.lessfilter

# docker
rm -r ~/.docker
ln -s ${dir}/.docker ~/.docker
