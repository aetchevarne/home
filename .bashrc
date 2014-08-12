. /etc/profile


shopt -s checkwinsize

# Custom prompt
PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

. /usr/lib/mc/mc.sh

# Add color
eval `dircolors -b`

takef () 
{
    nc -l -p 3000 | tar zjpvf - .
}

sendf ()
{
    tar cjv . - | nc $1 3000
}

export -f takef
export -f sendf

# Casi todos los xterm soportan 256 colores
#if [ "$TERM" == "xterm" ]; then
#	export TERM="xterm-256color"
#fi


# Scripts especificos para hosts
hostscript=~/.bash.d/$(hostname)

if [ -x $hostscript ]; then
	source $hostscript
fi

function Man
{
    vim -c "Man $*" -c only -R
}

function stripall
{
    find . | xargs file | grep "executable" | grep ELF | cut -f 1 -d : | xargs strip --strip-unneeded 
    find . | xargs file | grep "shared object" | grep ELF | cut -f 1 -d : | xargs strip --strip-unneeded 
}


. ~/.environment
. ~/.alias

