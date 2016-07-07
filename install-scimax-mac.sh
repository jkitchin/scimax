#!/bin/bash

# Check for brew
command -v brew >/dev/null 2>&1 || { echo >&2 "You need brew but it's not installed.  Installing it now.";
				     /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"; }

# Check for git
command -v git >/dev/null 2>&1 || { echo >&2 "You need git but it's not installed.  Installing it now.";
				    brew install git; }

# Check for emacs
command -v emacs >/dev/null 2>&1 || { echo >&2 "You need emacs but it's not installed.  Installing it now.";
				      brew install emacs --with-gnutls --with-imagemagick --with-librsvg --with-x11 --use-git-head --HEAD --with-cocoa; }

# Now clone scimax
if [ ! -d "scimax" ]; then
    git clone https://github.com/jkitchin/scimax.git
fi

echo "scimax is installed. To use it, run this command in your terminal."
echo "emacs -q -l `pwd`/scimax/init.el"
echo "or"
echo "run the scimax.sh script created in this directory as ./scimax.sh in the terminal."

echo "#!/bin/bash
emacs -q -l `pwd`/scimax/init.el
#end" > scimax.sh
chmod +x scimax.sh
#end
