#!/bin/bash

# Now clone scimax
if [ ! -d "scimax" ]; then
  git clone https://github.com/jkitchin/scimax.git
fi

cd scimax
git submodule add https://github.com/jkitchin/emacs-win
git submodule init
git submodule update

echo "scimax is installed. To use it, run this command in your terminal."
echo "`pwd`/scimax/emacs-win/bin/runemacs.exe -q -l `pwd`/scimax/init.el"
echo "or"
echo "run the scimax.sh script created in this directory as ./scimax.sh in the terminal."

echo "#!/bin/bash
`pwd`/scimax/emacs-win/bin/runemacs.exe -q -l `pwd`/scimax/init.el
#end" > scimax.sh
chmod +x scimax.sh

echo "Opening scimax. The first time it will install a lot of packages. Be patient."
start "" ".\scimax\emacs-win\bin\runemacs.exe" -l ".\scimax\init.el"
#end
