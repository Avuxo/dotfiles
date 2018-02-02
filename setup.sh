#!/bin/bash

#####################################
# Setup script for Avuxo's dotfiles #
#####################################

#######################################
#               HELP                  #
# -e: compile emacs with setup        #
# -i: configure i3 during setup       #
# -h: configure homepage during setup #
######################################


############
# OPTIONAL #
############

compileEmacs(){
    cd SCRIPTDIR
    mkdir tmp
    curl -o em.tar.gz https://mirrors.tripadvisor.com/gnu/emacs/emacs-24.3.tar.gz
    tar xvf em.tar.gz && cd em

    ./configure --without-x
    make; sudo make install

    cd ../..
    rm -r tmp
}

setupi3(){
    cd SCRIPTDIR
    mv .i3 $HOME
}

setupHomepage(){
    cd SCRIPTDIR
    mv homepage $HOME/workspace
}

###########
# GENERAL #
###########

general(){
    cd SCRIPTDIR
    mv .bashrc $HOME
    mv .emacs $HOME

    mkdir $HOME/.emacs.d
    mv Hitagi-theme.el $HOME/.emacs.d

    mkdir $HOME/workspace
    mkdir $HOME/workspace/scripts

    mv srm $HOME/workspace/scripts

}


# get the current directory so that we can return to it later
SCRIPTDIR=`pwd`

# always run the general setup
general


########
# ARGS #
########

while getopts ":eih" opt; do
    case ${opt} in
        e) compileEmacs  ;;
        i) setupi3       ;;
        h) setupHomepage ;;
    esac
done
