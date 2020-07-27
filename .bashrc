# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '
export EDITOR="emacsclient -nw"
export TERM="xterm-256color"
export MCORE_STDLIB='/home/calin/repos/github.com/miking-lang/miking/stdlib'
export EMULATOR_NAME='OnePlus ONEPLUS A6003'


export FZF_DEFAULT_OPS="--extended"

#Hate HiDPI already
export QT_AUTO_SCREEN_SCALE_FACTOR=1
export QT_FONT_DPI=96 vym
export GDK_SCALE=2
export GDK_DPI_SCALE=0.5
export ELM_SCALE=1.5

export PATH=~/.local/bin:$PATH

source ~/.config/.git-prompt.sh
. ~/.config/.git-prompt.sh
. ~/.config/.git-completion.bash


export FZF_DEFAULT_COMMAND="\
  rg . \
    --files \
    --follow \
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



export PS1="\[\e[1;36m\]\W\[\e[36m\] \[\e[31m\]\$(__git_ps1 '  %s') \[\e[m\]"

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
alias lt='exa -aT --color=always --group-directories-first' # tree listing

alias cp='cp -i' 
alias mv='mv -i'
alias rm='rm -i'
alias ..='cd ..'
alias ...='cd ../..'
#alias gs='git status'
	

alias eb='cd && emacsclient -nw .bashrc' #fast fix of bashrc
alias ipm='cd /home/calin/repos/github.com/miking-lang/fork-ipm'
alias hailey='cd ~/Documents/hailey'
alias connectweb='ssh -p3801 calin@capitanu.tech'
alias webtech='sudo scp -r -P 3801 ~/capitanu.tech/* calin@capitanu.tech:~/capitanu.tech'
alias webcom='sudo scp -r -P 3801 ~/thedatabuddy.com/* calin@capitanu.tech:~/thedatabuddy.com'

alias vpnkth='cd /home/darthvader11/Documents/KTH/TCOMK2/Networking\ and\ Communication/Labs/Lab1/client/ && sudo openvpn --script-security 2 --config client.conf'
alias kbdlight='sudo nano /sys/devices/platform/dell-laptop/leds/dell\:\:kbd_backlight/stop_timeout'

alias wifi='nmcli dev wifi list'
alias wificonnect='nmcli device wifi connect'
alias airplaneon='sudo rfkill block all'
alias airplaneoff='sudo rfkill unblock all'
alias wifidisconnect='nmcli device disconnect wlp58s0'

alias dotfiles='cd /home/calin/repos/github.com/capitanu/dotfiles'
alias kali='ssh root@192.168.0.151'
#alias clear='clear && neofetch'
alias wififix='/home/calin/.config/wifiscript.sh'
alias emc='emacsclient -nw --socket-name=/tmp/emacs1000/server'
alias emacsrr='systemctl restart --user emacs'

alias mcore='~/KTH/Miking/miking/build/boot'
alias mtest='~/KTH/Miking/miking/build/boot test'
alias nuget="mono /usr/local/bin/nuget.exe"

alias pdfcompress='gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/prepress -dNOPAUSE -dQUIET -dBATCH '

alias lcad='/home/calin/Scripts/devour/devour.sh /home/calin/Scripts/start_leocad.sh'

alias note='emacsclient -nw --socket-name=/tmp/emacs1000/server ~/.config/notes.org'


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




term=$(ps -aux | grep `ps -p $$ -o ppid=` | awk 'NR==1{print $11}');
case $term in
    */usr/bin/emacs*)
	found=1
        export PS1
        ;;
    *)
	if [ "$TERM" != "linux" ]; then
	    source /home/calin/.config/scripts/pureline/pureline ~/.pureline.conf
	fi
        ;;
esac

        
eval $(opam env)
