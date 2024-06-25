"""CLI tools for scimax.

Sometimes emacs hangs on something, usually fontlock related. You can sometimes open a terminal and send USR2 to the Emacs process to interrupt it.

> eint 10

will try 10 times to interrupt it.

If that fails, you can try to kill it with `ekill'. This kills all the Emacs processes though. I usually only have one open, and this is what I want.

"""
import click
import subprocess


@click.command()
@click.argument('n', default=1, nargs=1)
def eint(n=1):
    "Send USR2 signal to Emacs n times."
    for i in range(n):
        print(f'{i:2d} sending USR2')
        subprocess.call('pkill -USR2 Emacs', shell=True)


@click.command()
def ekill():
    "Kill Emacs processes"
    subprocess.call('pkill -9 Emacs', shell=True)        
