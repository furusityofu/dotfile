if [ -e $HOME/bin ]; then
    export PATH=$HOME/bin:$PATH
else
fi

#mac専用の設定
case ${OSTYPE} in
    darwin*)
	#BSDlsコマンドのカラーリング
	alias brew="PATH=/usr/local/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin brew"
	alias ls='ls -G -w'
        export ZSH_HIGHLIGHT_HIGHLIGHTERS_DIR=/usr/local/share/zsh-syntax-highlighting/highlighters
        source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

        export PATH=/usr/local/texlive/2019/bin/x86_64-darwin:$PATH

        fpath=(/usr/local/share/zsh/site-functions $fpath)
        if [ -d /usr/local/opt/ruby/bin ];then
            export PATH="/usr/local/opt/ruby/bin:$PATH"
        fi
        #Macのバージョン依存の設定
        VER=`sw_vers -productVersion | awk -F. '{ print $1 "." $2 }'`
        case $VER in
            "10.14")
                # alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs" #gui用設定
                export SDKROOT=/Library/Developer/CommandLineTools/SDKs/MacOSX10.14.sdk
                ;;
            "10.15")
                export SDKROOT=/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk
                ;;
        esac
	;;
    linux*)
	alias ls='ls --color'
	if grep '^fbterm' /proc/$PPID/cmdline > /dev/null; then
	    export TERM=fbterm
	fi
	alias fbterm='env LANG=ja_JP.UTF8 fbterm'
        # Most importantly, add /usr/local/texlive/2019/bin/x86_64-linux
        export PATH=$HOME/.local/bin:/usr/local/texlive/2019/bin/x86_64-linux:$PATH
	;;
esac
# Add /usr/local/texlive/2019/texmf-dist/doc/man to MANPATH.
export MANPATH=/usr/local/texlive/2019/texmf-dist/doc/man:$MANPATH
# Add /usr/local/texlive/2019/texmf-dist/doc/info to INFOPATH.
export INFOPATH=/usr/local/texlive/2019/texmf-dist/doc/info:$INFOPATH

#alias

#export DYLD_FALLBACK_LIBRARY_PATH=/opt/local/lib

## Command history configuration
#
HISTFILE=~/.zsh_history
HISTSIZE=10000000
SAVEHIST=10000000
setopt hist_ignore_dups     # ignore duplication command history list
setopt share_history        # share command history data

# historical backward/forward search with linehead string binded to ^P/^N
#
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# auto change directory
#
setopt auto_cd

# command correct edition before each completion attempt
#
setopt correct

# compacked complete list display
#
setopt list_packed

#directory preview
case "$TERM" in
    xterm*|kterm*|rxvt*)
        PROMPT=$(print "%B%{\e[34m%}%m:%(5~,%-2~/.../%2~,%~)%{\e[33m%}%# %b")
        PROMPT=$(print "%{\e]2;%n@%m: %~\7%}$PROMPT") # title bar
        ;;
    *)
        PROMPT='%m:%c%# '
        ;;
esac

#disable lately command execution(統計のrとかぶる)
disable r

# ディレクトリ名の補完で末尾の / を自動的に付加し、次の補完に備える
setopt auto_param_slash

# 直前と同じコマンドラインはヒストリに追加しない
setopt hist_ignore_dups

# 余分なスペースを削除してヒストリに記録する
setopt hist_reduce_blanks

# 8 ビット目を通すようになり、日本語のファイル名を表示可能
setopt print_eight_bit

# ファイル名の展開でディレクトリにマッチした場合末尾に / を付加する
setopt mark_dirs

# cd をしたときにlsを実行する
function chpwd() { ls -a }

# cdのタイミングで自動的にpushd
setopt auto_pushd

TERM=xterm-256color

# OPAM configuration
echo $PATH|grep "opam"
if [ $? -eq 1 ] ; then
. $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
fi

# brew api token
if [ -f ~/.homebrew_api_token ];then
  source ~/.homebrew_api_token
fi

#right prompt
#PROMPT=$BLUE'[%n@%m] %(!.#.$) '$WHITE
#RPROMPT=$GREEN'[%~]'$WHITE
#setopt transient_rprompt

#PATH="$HOME/perl5/bin${PATH+:}${PATH}"; export PATH;
#PERL5LIB="$HOME/perl5/lib/perl5${PERL5LIB+:}${PERL5LIB}"; export PERL5LIB;
#PERL_LOCAL_LIB_ROOT="$HOME/perl5${PERL_LOCAL_LIB_ROOT+:}${PERL_LOCAL_LIB_ROOT}"; export PERL_LOCAL_LIB_ROOT;
#PERL_MB_OPT="--install_base \"$HOME/perl5\""; export PERL_MB_OPT;
#PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"; export PERL_MM_OPT;

alias git-graph="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative"
alias tmattach="tmux a -t"
alias findgrep="find ./ -type f -print0|xargs -0 grep"


#pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

if [ -f $HOME/.iterm2_shell_integration.zsh ];then
    source $HOME/.iterm2_shell_integration.zsh
fi

if [ -d $HOME/.composer/vendor/bin ];then
    export PATH=$HOME/.composer/vendor/bin:$PATH
fi

if [ -f $HOME/.zshrc.local.zsh ];then
    source $HOME/.zshrc.local.zsh
fi
if [ -d $HOME/.composer/vendor/bin ];then
    export PATH=$HOME/.composer/vendor/bin:$PATH
fi
if [ -d $HOME/.cargo/bin ];then
    source $HOME/.cargo/env
fi
if [ -d $HOME/.nix-profile ];then
    source $HOME/.nix-profile/etc/profile.d/nix.sh
    # source $HOME/.nix-profile/share/zsh/plugins/nix/nix.plugin.zsh
    fpath=($HOME/nix-zsh-completions $fpath)
    fpath=($HOME/.nix-profile/share/zsh/site-functions $fpath)
    alias nixinstall='nix-env -i'
    alias nixsearch='nix-env -qa'
    alias nixlist='nix-env --query --installed'
    alias nixoutdated='nix-env --upgrade --dry-run'
    alias nixupgrade='nix-env --upgrade'
    alias nixupdate='nix-channel --update'
fi

if [ -d $HOME/.zfunc ];then
    fpath=($HOME/.zfunc $fpath)
fi
if [ -d $HOME/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src/ ];then
    case ${OSTYPE} in
        darwin*)
            export RUST_SRC_PATH=$HOME/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src/
            ;;
    esac
fi
if [ -d /home/linuxbrew/ ];then
    eval $(/home/linuxbrew/.linuxbrew/bin/brew shellenv)
fi


export EDITOR=emacs
alias e='emacsclient -nw -a ""'
alias ekill='emacsclient -e "(kill-emacs)"'
typeset -U fpath
autoload -U compinit
compinit
typeset -U PATH
