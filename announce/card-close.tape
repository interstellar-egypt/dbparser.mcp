# closing card for the demo: plain text, same font and theme as the recording
Output announce/card-close.mp4
Set Shell bash
Set FontFamily "Menlo"
Set FontSize 30
Set Width 1920
Set Height 1080
Set Padding 36
Set Theme "Catppuccin Mocha"
Set CursorBlink false
Hide
Type `export LC_ALL=en_US.UTF-8 PS1='' T='dbparser.mcp · github.com/interstellar-egypt/dbparser.mcp'; clear; tput civis; printf '\n%.0s' $(seq 12); printf '%*s' $(( ($(tput cols) + ${#T}) / 2 )) "$T"`
Enter
Sleep 1s
Show
Sleep 2s
