#!/usr/bin/env bash          

for file in .emacs .bashrc .bash_profile lisp
do                                                                         
    ln -sf ~/setup/$file ~/$file
done                 

if [[ $(uname) == 'Darwin' ]]; then 
    ln -sf ~/setup/.slate ~/.slate
fi
