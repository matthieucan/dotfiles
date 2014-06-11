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
rm ~/.config/openbox
ln ${dir}/openbox ~/.config/openbox --symbolic

# zsh
rm ~/.zshrc
ln ${dir}/.zshrc ~/.zshrc --symbolic

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
rm ~/.config/tint2
ln ${dir}/tint2 ~/.config/tint2 --symbolic

# terminator
rm ~/.config/terminator
ln ${dir}/terminator ~/.config/terminator --symbolic

# volti
rm ~/.config/volti
ln ${dir}/volti ~/.config/volti --symbolic

# .dir_colors
rm ~/.dir_colors
ln ${dir}/.dir_colors ~/.dir_colors --symbolic

# quilt
rm ~/.quiltrc
ln ${dir}/.quiltrc ~/.quiltrc --symbolic
