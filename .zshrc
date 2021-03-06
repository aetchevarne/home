# The following lines were added by compinstall

zstyle ':completion:*' completer _complete _ignored _approximate
zstyle :compinstall filename '/home/saknussemm/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
#source /usr/share/zsh/scripts/antigen/antigen.zsh
source ~/.antigen/antigen/antigen.zsh 
antigen use oh-my-zsh

antigen bundle gitfast 
antigen bundle svn-fast-info
antigen bundle vagrant
antigen bundle zsh-users/zsh-completions src
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-navigation-tools
antigen bundle colored-man-pages

#antigen theme gentoo
antigen theme af-magic

antigen apply

. ~/.environment
. ~/.alias

# Lines configured by zsh-newuser-install
unsetopt beep
unsetopt share_history
bindkey -e
# End of lines configured by zsh-newuser-install


