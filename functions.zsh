Copyright (c) 2018 DasFranck

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

rc(){
  systemctl list-unit-files --type=service |\
  sed 's/.service//g' |\
  sed '/static/d' |\
  sed '/indirect/d' |\
  sed '/systemd/d' |\
  sed '/dbus-org/d' |\
  sed '/canberra/d'|\
  sed '/wpa_supplicant/d' |\
  sed '/netctl/d' |\
  sed '/rfkill/d' |\
  sed '/krb5/d' |\
  tail -n+2 |\
  head -n -2 |\
  sed 's/\(^.*enabled.*$\)/[x] \1/' |\
  sed 's/enabled//g' |\
  sed 's/\(^.*disabled.*$\)/[ ] \1/' |\
  sed 's/disabled//g' |\
  sed 's/[ \t]*$//' |\
  while read line; do
      if [[ $line == *'[x]'* ]]; then
        printf "\033[0;32m$line\n"
      else
        printf "\033[1;30m$line\n"
      fi
  done
  syncthing_status=`systemctl is-active syncthing@${USER}.service`
  if [[ $syncthing_status == "active" ]]; then
    printf "\033[0;32m[x] syncthing (${USER})\n"
  else
    printf "\033[1;30m[ ] syncthing (${USER})\n"
  fi
}
