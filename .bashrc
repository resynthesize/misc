# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

if [ -f ~/.bash_local ]; then
        . ~/.bash_local
fi

if [ -f ~/.ssh/ssh-login ]; then
        . ~/.ssh/ssh-login
fi

function gc() { git commit -a -m $1 ;}
function pl() { sed -n 'p $1' -print $2;} 

alias emacs='emacs -nw'
alias sc='emacs ~/.ssh/config' 

if [[ $(uname) == 'Darwin' ]]; then 
    export JAVA_HOME=`/usr/libexec/java_home`
    export EC2_HOME=/Users/brandon/install/ec2
    export PATH=$PATH:$EC2_HOME/bin
    export EC2_URL=https://ec2.us-west-1.amazonaws.com
    export EC2_PRIVATE_KEY=`ls $EC2_HOME/pk*.pem`
    export EC2_CERT=`ls $EC2_HOME/cert-*.pem`
fi 

export EDITOR=emacs
export PATH=/opt/local/bin:/opt/local/sbin:$PATH

# set this so emacs color-theme-solarized displays correctly
export TERM=xterm-16color 
