#!/bin/bash

# Setup git.
[[ -z `git config --global user.name` ]] && read -p "Full name: " name && git config --global user.name "$name"
[[ -z `git config --global user.email` ]] && read -p "Email: " email && git config --global user.email $email

# Now clone scimax
if [ ! -d "scimax" ]; then
    git clone https://github.com/jkitchin/scimax.git
fi

cd scimax
git submodule add https://github.com/jkitchin/emacs-win
# git submodule add -f https://github.com/jkitchin/scimax-win-elpa elpa
git submodule init
git submodule update

# The hunspell stuff seems to be crashing emacs. This might take it out.
cd emacs-win
git checkout 34a7ec32f8aaa0828fd3db04f258b7555115e281
cd ..

git add emacs-win
git commit emacs-win -m "add windows emacs"
git add .gitmodules
git commit .gitmodules -m "windows setup for submodules"

echo "scimax is installed. To use it, run this command in your terminal."
echo "`pwd`/emacs-win/bin/runemacs.exe -q -l `pwd`/init.el"
echo "or"
echo "run the scimax.bat script created in this directory or as ./scimax.sh in the terminal."

# This converts the posix style path from git bash to a windows path.
# You can use this as the application to open
SCIMAX_ROOT=$(echo `pwd` | sed -e 's/^\///' -e 's/\//\\/g' -e 's/^./\0:/')
echo "SET LANG=C" > scimax.bat
echo "start \"\" \"${SCIMAX_ROOT}\\emacs-win\\bin\\runemacs.exe\" -l \"${SCIMAX_ROOT}\\init.el\" %1" >> scimax.bat

# Use this in git bash
echo "LANG=C start \"\" \"${SCIMAX_ROOT}\\emacs-win\\bin\\runemacs.exe\" -l \"${SCIMAX_ROOT}\\init.el\" \"\$1\"" > scimax.sh


echo "Opening scimax.  Be patient."
start "" ".\emacs-win\bin\runemacs.exe" -l ".\init.el"
#end
