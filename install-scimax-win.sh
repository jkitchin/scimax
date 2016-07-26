#!/bin/bash

# Now clone scimax
if [ ! -d "scimax" ]; then
  git clone https://github.com/jkitchin/scimax.git
fi

cd scimax
git submodule add https://github.com/jkitchin/emacs-win
git submodule add -f https://github.com/jkitchin/scimax-win-elpa elpa
git submodule init
git submodule update

git add emacs-win
git commit emacs-win -m "add windows emacs"
git add .gitmodules
git commit .gitmodules -m "windows setup for submodules"

echo "scimax is installed. To use it, run this command in your terminal."
echo "`pwd`/scimax/emacs-win/bin/runemacs.exe -q -l `pwd`/scimax/init.el"
echo "or"
echo "run the scimax.bat script created in this directory as ./scimax.sh in the terminal."

# This converts the posix style path from git bash to a windows path.
# You can use this as the application to open
SCIMAX_ROOT=$(echo `pwd` | sed -e 's/^\///' -e 's/\//\\/g' -e 's/^./\0:/')
echo "start \"\" \"${SCIMAX_ROOT}\\emacs-win\\bin\\runemacs.exe\" -l \"${SCIMAX_ROOT}\\init.el\" %1" > scimax.bat

# Use this in git bash
echo "start \"\" \"${SCIMAX_ROOT}\\emacs-win\\bin\\runemacs.exe\" -l \"${SCIMAX_ROOT}\\init.el\" \"\$1\"" > scimax.sh


echo "Opening scimax.  Be patient."
start "" ".\emacs-win\bin\runemacs.exe" -l ".\init.el"
#end
