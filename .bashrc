alias emacs='emacs -nw'
alias pw='emacs /rgsoft.dyndns.org:/data/docs/pwd.txt'
alias sc='emacs ~/.ssh/config' 

export EC2_HOME=/Users/brandon/install/ec2
export PATH=$PATH:$EC2_HOME/bin
export EC2_URL=https://ec2.us-west-1.amazonaws.com
export EC2_PRIVATE_KEY=`ls $EC2_HOME/pk*.pem`
export EC2_CERT=`ls $EC2_HOME/cert-*.pem`
export JAVA_HOME=`/usr/libexec/java_home`
export EDITOR=emacs
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
 
