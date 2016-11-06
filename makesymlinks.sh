#!/bin/bash
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

# make executables files in ~/dotfiles/bin
for i in $( ls -1 bin/** ); do
  chmod +x $i
done


########## Variables

dir=~/dotfiles                    # dotfiles directory
olddir="dotfiles_old_`date '+%Y-%m-%d_%H-%M-%S'`"             # old dotfiles backup directory
files="aliases exports gitconfig vimrc zshrc"    # list of files/folders to symlink in homedir

##########

# create dotfiles_old in homedir
echo -n "Creating $olddir for backup of any existing dotfiles in ~ ..."
mkdir -p ~/$olddir
echo "done"

# change to the dotfiles directory
echo -n "Changing to the $dir directory ..."
cd $dir
echo "done"

echo "Moving any existing dotfiles from ~ to $olddir"
for file in $files; do
    echo "Backing up $file"
    cp -L ~/.$file ~/$olddir/ && rm ~/.$file
    echo "Creating symlink to $file in home directory."
    ln -sf $dir/$file ~/.$file
    echo " "
done

echo "Linking bin directory"
mv -v ~/bin ~/$olddir/
ln -s $dir/bin ~/bin

echo "Linking vim directory"
mv -v ~/.vim ~/$olddir/
ln -s $dir/vim ~/.vim

echo "Linking sbtopts"
ln -sf $dir/sbtopts /usr/local/etc/sbtopts
chmod 644 $dir/sbtopts

echo "Linking zsh theme"
ln -sf $dir/in-fino-veritas.zsh-theme ~/.oh-my-zsh/themes/in-fino-veritas.zsh-theme
