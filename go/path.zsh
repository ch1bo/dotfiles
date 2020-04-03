export GOPATH="$HOME/work/go"
path=($GOPATH/bin $path)
if [ -d /usr/lib/go-1.10/bin ]; then
  path=(/usr/lib/go-1.10/bin $path)
fi
