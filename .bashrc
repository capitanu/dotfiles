# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '
export EDITOR="emacsclient -c"
export TERM="xterm-256color"
export MCORE_STDLIB='/home/calin/repos/github.com/miking-lang/miking/stdlib'
export MI_IPM='/home/calin/repos/github.com/capitanu/miking-ipm'
export EMULATOR_NAME='OnePlus ONEPLUS A6003'
export ANDROID_HOME='/home/calin/Android/Sdk'
export PYTHONPATH=${PYTHONPATH}:/usr/lib/python3.9/dist-packages/
export FZF_DEFAULT_OPS="--extended"

#Hate HiDPI already

export PATH=~/.local/bin:$PATH

source ~/.config/.git-prompt.sh
. ~/.config/.git-prompt.sh
. ~/.config/.git-completion.bash

source /usr/share/fzf/completion.bash
source /usr/share/fzf/key-bindings.bash
#source $HOME/.cargo/env

export FZF_DEFAULT_COMMAND="\
  rg . \
    --files \
    --glob '!*cache/*' \
    --glob '!.git/*' \
    --hidden \
  "

export FZF_DEFAULT_DIRECTORY_COMMAND="\
  rg . \
    --files \
    --null \
  | xargs -0 dirname \
  | sort -u
  "




export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
export GIT_PS1_SHOWDIRTYSTATE=1
#export GIT_PS1_SHOWSTASHSTATE=1
#export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_PS1_SHOWUPSTREAM="verbose"
export GIT_PS1_STATESEPARATOR="/"
export GIT_PS1_DESCRIBE_STYLE=default

LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=01;95:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.webp=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:*rc=00;93';
export LS_COLORS



export PS1="\[\e[1;36m\]\W\[\e[36m\]  \[\e[31m\]\$(__git_ps1 '  %s') \[\e[m\]"

if [ -f /etc/bash.command-not-found ]; then
    . /etc/bash.command-not-found
fi


#Git commands

alias gitl='git log --oneline --decorate --graph --all'
alias gits='git status'
alias gitk='git checkout'
alias gitp='git push origin'
alias gitb='git branch'
alias gitc='git commit -m'
alias gita='git add .'


alias ls='exa -al --color=always --group-directories-first' # my preferred listing
alias la='exa -a --color=always --group-directories-first'  # all files and dirs
alias ll='exa -l --color=always --group-directories-first'  # long format
alias lt='tree -a' # tree listing

#alias cp='cp -i' 
#alias mv='mv -i'
#alias rm='rm -i'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
	

alias eb='/home/calin/.config/scripts/devour/devour.sh emacsclient -c /home/calin/.bashrc' #fast fix of bashrc
alias capitanu='cd /home/calin/repos/github.com/capitanu/'
alias ipm='cd /home/calin/repos/github.com/capitanu/miking-ipm/'
alias hailey='cd /home/calin/repos/github.com/hailey'
alias github='cd /home/calin/repos/github.com'
alias connectweb='ssh -X -p3801 calin@capitanu.tech'
alias webtech='sudo scp -r -P 3801 /home/calin/repos/github.com/capitanu/capitanu.tech/* calin@capitanu.tech:~/capitanu.tech'
alias webcom='sudo scp -r -P 3801 ~/thedatabuddy.com/* calin@capitanu.tech:~/thedatabuddy.com'

alias minecraft-server='cd /home/calin/games/minecraft-server && java -Xmx4G -Xms4G -jar server.jar nogui'
alias ftb-server='cd /home/calin/games/ftb-server && ./start.sh'

#alias vpnkth='cd /home/darthvader11/Documents/KTH/TCOMK2/Networking\ and\ Communication/Labs/Lab1/client/ && sudo openvpn --script-security 2 --config client.conf'
alias kbdlight='sudo nano /sys/devices/platform/dell-laptop/leds/dell\:\:kbd_backlight/stop_timeout'

alias em28='export EMULTAOR=PIXEL_API28 && emulator -avd PIXEL_API28 &'

alias wifi='nmcli dev wifi list'
alias wificonnect='nmcli device wifi connect'
alias airplaneon='sudo rfkill block all'
alias airplaneoff='sudo rfkill unblock all'
alias wifidisconnect='nmcli device disconnect wlp58s0'

alias dotfiles='cd /home/calin/repos/github.com/capitanu/dotfiles'
alias kali='ssh calin@192.168.0.151'
#alias clear='clear && neofetch'
alias wififix='/home/calin/.config/scripts/wifiscript.sh'
alias emc='/home/calin/.config/scripts/devour/devour.sh emacsclient -c'
alias emacsrr='systemctl restart --user emacs'

alias nuget="mono /usr/local/bin/nuget.exe"
alias kbd='/home/calin/.config/scripts/changekbd.sh'


alias pdfcompress='gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/prepress -dNOPAUSE -dQUIET -dBATCH '

alias lcad='/home/calin/.config/scripts/devour/devour.sh /home/calin/Scripts/start_leocad.sh'
alias loc='tokei'
alias note='/home/calin/.config/scripts/devour/devour.sh emacsclient -c ~/.config/notes.org'

alias kth='ranger /home/calin/KTH/TCOMK3/'
alias sharescreen='/home/calin/.config/scripts/devour/devour.sh vlc --no-video-deco --no-embedded-video --screen-fps=30 --screen-left=0 --screen-width=1920 --screen-height=1080 screen://'
alias ccat='highlight -O xterm256 -s navajo-night' 

alias starwars='telnet towel.blinkenlights.nl'
alias thesistex='cd /home/calin/kth/TCOMK3/II143X_Degree_Project_in_Information_and_Communication_Technology/template && while inotifywait -e close_write thesis.tex ; do pdflatex thesis.tex; done'

alias btc='curl rate.sx/btc'
alias crypto='curl rate.sx'


alias se='ls | rg '
alias ds='lt | rg '

alias d='eval $(__fzf_cd__)'
alias ef="emacsclient -n \$(fzf)"
alias e="/home/calin/.config/scripts/devour/devour.sh emacsclient -c \$(fzf)"
alias en="/home/calin/.config/scripts/en.sh"
alias v="vim \$(fzf)"
alias g='git'

#alias backupemacs='scp -P 3801 ~/.emacs.d/{init.el,config.org} darthvader11@capitanu.tech:~/Backups/'
#alias backupgnome='scp -P 3801 ~/Documents/Backups/saved_settings.conf darthvader11@capitanu.tech:~/Backups/'
#alias emacs='emacs -nw .'
#alias backupwholesystem='sudo rsync -aAXv --delete --exclude=/proc/* --exclude=/sys/* --exclude=/tmp/* --exclude=/mnt/* --exclude=/media/* --exclude="swapfile" --exclude="lost+found" --exclude=".cahce" / /media/darthvader11/HardDrive/Backup_Linux_16_10_2019/'
#alias restorewholesystem='sudo rsync -aAXv --delete  /media/darthvader11/HardDrive/Backup_Linux_16_10_2019/ /'
#alias androidstudio='./Downloads/android-studio-ide-191.5900203-linux/android-studio/bin/studio.sh'
#alias copywebapp='cp -r ~/Documents/Website/* ~/Documents/KTH/TCOMK2/Mobile\ App\ Development/Lab1/'
#alias chrome='google-chrome'
#alias updatesys='sudo apt-get update && sudo apt-get upgrade'
#alias install='sudo apt-get install'
#alias go='nautilus .'
# alias clear='clear && figlet -f big darthvader11'

#neofetch




# term=$(ps -aux | grep `ps -p $$ -o ppid=` | awk 'NR==1{print $11}');
# case $term in
#     */usr/bin/emacs*)
# 	found=1
#         export PS1
#         ;;
#     *)
# 	if [ "$TERM" != "linux" ]; then
# 	    source /home/calin/.config/scripts/pureline/pureline ~/.pureline.conf
# 	fi
#        ;;
# esac

#source /home/calin/.config/scripts/pureline/pureline ~/.pureline.conf
export PS1
eval $(opam env)

