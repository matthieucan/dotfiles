#!/bin/bash

if [ "$#" -ne 1 ]; then
  echo "usage: ./lnmaker.sh DOTFILES_DIR"
  exit
fi

dir=$(readlink -f $1)

# .bashrc
mv ~/.bashrc ~/.bashrc.bak
ln ${dir}/.bashrc ~/.bashrc --symbolic

# awesome
mv ~/.config/awesome ~/.config/awesome.bak
ln ${dir}/awesome ~/.config/awesome --symbolic

# .conkyrc
mv ~/.conkyrc ~/.conkyrc.bak
ln ${dir}/.conkyrc ~/.conkyrc --symbolic

# emacs
mv ~/.emacs ~/.emacs.bak
mv ~/.emacs.d ~/.emacs.d.bak
ln ${dir}/emacs/.emacs ~/.emacs --symbolic
ln ${dir}/emacs/.emacs.d ~/.emacs.d --symbolic

# openbox
mv ~/.config/openbox ~/.config/openbox.bak
ln ${dir}/openbox ~/.config/openbox --symbolic

# zsh
mv ~/.zshrc ~/.zshrc.bak
ln ${dir}/.zshrc ~/.zshrc --symbolic

# python
mv ~/.pyrc ~/.pyrc.bak
ln ${dir}/.pyrc ~/.pyrc --symbolic

# git
# needed to handle different user.email's
# yeah, let's say it's temporary
cp ~/.gitconfig ~/.gitconfig.bak
cat .gitconfig >> ~/.gitconfig
