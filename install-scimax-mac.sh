#!/bin/bash

function create_init_script()
{
    local script_dir="$1"
    local emacs_cmd


    start_emacs_cmd="emacs -q -l $(pwd)/init.el"

    echo "scimax is installed. To use it, run this command in your terminal."
    echo "$emacs_cmd"
    echo "or"
    echo "run the scimax.sh script created in $script_dir directory as ./scimax.sh in the terminal."

    # echo -e allows newline characters to be printed
    # the \$@ will pass the parameters passed to the scimax.sh to emacs
    # for example the --daemon will start scimax in daemon mode
    # this is very practical the first time you start scimax as
    # the Messages buffer will be printed directly to the linux console
    echo -e "#!/bin/bash\n${start_emacs_cmd} \$@" > "$script_dir/scimax.sh"

    # make the script executable
    chmod +x "$script_dir/scimax.sh"
}

# I am not sure if dirname and readlink are available on mac
# therefore I assume that the current folder is the script folder
script_dir="$(pwd)"

if [ ! -f /usr/bin/ruby ]; then
    echo "You need ruby but it's not installed. Please install this in /usr/bin/ruby."
    exit 1
fi

command -v curl >/dev/null 2>&1 || { echo >&2 "You need curl, but it is not installed. Please install it now.";
                                     exit 1; }

# Check for brew
command -v brew >/dev/null 2>&1 || { echo >&2 "You need brew but it's not installed.  Installing it now.";
				     /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"; }

# Check for git
command -v git >/dev/null 2>&1 || { echo >&2 "You need git but it's not installed.  Installing it now.";
				    brew install git; }

# Check for emacs
command -v emacs >/dev/null 2>&1 || { echo >&2 "You need emacs but it's not installed.  Installing it now.";
				      brew install emacs --with-gnutls --with-imagemagick --with-librsvg --with-x11 --use-git-head --HEAD --with-cocoa; }

# Check for cask
command -v cask >/dev/null 2>&1 || { echo >&2 "You need cask but it's not installed.  Installing it now.";
				     brew install cask; }

cask install

# Now clone scimax if needed
# are we already inside .git?
# then we assume this is already the scimax repo
# if not we clone scimax and enter the scimax folder
if [ ! -d ".git" ]; then
    echo "We are not inside a .git repo."

    if [ ! -d "scimax" ]; then
        echo "No local scimax found."
        echo "we try to clone a fresh scimax from git"
        git clone https://github.com/jkitchin/scimax.git
    fi

    if [ -d "scimax" ]; then
        cd scimax
    fi
fi

create_init_script "$script_dir"
