# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

alias itmlogin='tacmd login -s localhost -t 1440 -u sysadmin -p ""'
alias logs='cd /opt/IBM/ITM/logs'
alias config='cd /opt/IBM/ITM/config'
alias ewas='cd /opt/IBM/ITM/li6263/iw/profiles/ITMProfile/logs/ITMServer'
alias z0='cd /opt/IBM/ITM/li6263/z0/bin'
alias zk='cd /opt/IBM/ITM/li6263/zk/bin'
alias p='./push.sh'

#. $HOME/.ssh/ssh-login

function gc() { git commit -a -m $1 ;}

alias emacs='emacs -nw'
alias pw='emacs /rgsoft.dyndns.org:/data/docs/pwd.txt'
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
 
