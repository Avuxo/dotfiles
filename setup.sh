#!/bin/bash

#####################################
# Setup script for Avuxo's dotfiles #
#####################################

#######################################
#               HELP                  #
# -e: compile emacs with setup        #
# -i: configure i3 during setup       #
# -h: configure homepage during setup #
# -m: configure midi file download    #
#######################################


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

# download a bunch of midi files
setupMidi(){
    # vgmusic url
    DLURL='https://www.vgmusic.com/music/console/nintendo/snes/'

    mkdir $HOME/.midi
    cd $HOME/.midi
    mkdir music
    cd music

    # get all the midi files from vgmusic
    wget $DLURL
    cat index.html | grep -o "\".*.mid\"" | sed -e 's/\"//g' | xargs printf "$DLURL%s\n" | xargs wget
    
    rm index.html

    cd SCRIPTDIR
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

    mv ./bin/srm $HOME/workspace/scripts

    echo "* Tasks" >> $HOME/workspace/todo.org

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
        m) setupMidi     ;;
    esac
done
