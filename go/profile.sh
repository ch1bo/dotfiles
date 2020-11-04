# topic-specific .profile which is sourced by any login shell
export GOPATH="$HOME/work/go"
export PATH=${GOPATH}/bin:${PATH}
if [ -d /usr/lib/go-1.10/bin ]; then
  export PATH=/usr/lib/go-1.10/bin:${PATH}
fi
