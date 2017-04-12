#!/bin/bash

# exit on any error
set -e

function require()
{
    local cmd="$1"
    command -v "$cmd" >/dev/null 2>&1 || { echo >&2 "I require $cmd but it's not installed."; exit 1; }
}

function assert_minimal_emacs_version()
{
    local expected_major=$1
    local expected_minor=$2

    local current_major
    local current_minor

    current_major=$(emacs --batch --eval "(prin1 emacs-major-version)")
    current_minor=$(emacs --batch --eval "(prin1 emacs-minor-version)")

    # At least major version
    if [ ! "$current_major" -ge "$expected_major" ]; then
        echo "You need an emacs with major version of $expected_major or greater."
        exit 1;
    fi

    # if major version is minimum, then check minor version as well
    if [ "$current_major" -eq "$expected_major" ]; then
        if [ ! "$current_minor" -ge "$expected_minor" ]; then
            echo "You need an emacs with version $expected_major.$expected_minor or greater."
            exit 1;
        fi
    fi
}

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


function main()
{
    local script_dir

    require git
    require emacs

    #core-utils has readlink and dirname
    require dirname
    require readlink

    assert_minimal_emacs_version 25 1

    # always go to the directory where the script is stored
    # readlink finds out the exact path there this shellscript
    # is stored. dirname strips the name of the shellscript to
    # return the path in this the shellscript is started.
    script_dir=$(dirname "$(readlink -f "$0")")
    cd "$script_dir"

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
}

main
