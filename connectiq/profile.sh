# topic-specific .profile which is sourced by any login shell
if [ -f "$HOME/.Garmin/ConnectIQ/current-sdk.cfg" ]; then
  CURRENT_SDK_BINDIR="$(cat $HOME/.Garmin/ConnectIQ/current-sdk.cfg)bin"
  export PATH=${CURRENT_SDK_BINDIR}:${PATH}
fi
