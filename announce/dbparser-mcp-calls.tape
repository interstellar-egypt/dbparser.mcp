# Records announce/calls.mp4: the take recorded by dbparser-mcp-demo.tape,
# resumed in Claude Code with ctrl+o (detailed transcript), so the screen
# shows the real tool calls (parse_file, read_table, export_csv) that the
# compact view folds into "Called dbparser N times". Nothing is asked again.
# Then it shows the exported file. Set the session id of the take first:
# it is printed when Claude exits ("Resume this session with: ...").

Output announce/calls.mp4

Set Shell bash
Set FontFamily "Menlo"
Set FontSize 22
Set Width 1920
Set Height 1080
Set Padding 36
Set Theme "Catppuccin Mocha"
Set TypingSpeed 30ms
Set CursorBlink false

Hide
Type "unset $(env | grep -o '^CLAUDE[A-Z_]*') ; export PS1='$ ' && cd announce && clear"
Enter
Sleep 1s
Type `claude --resume 052bec43-d06c-42a4-8a61-3c56c4a0d5b8 --allowedTools "mcp__dbparser__*" --tools "" --permission-mode default --strict-mcp-config --mcp-config dbparser-mcp.json`
Enter
Sleep 12s
Ctrl+O
Sleep 3s
PageUp@100ms 120
Sleep 2s
Show

Sleep 2s
PageDown@1.5s 45
Sleep 2s
Ctrl+O
Sleep 2s

Type "/exit"
Sleep 1s
Enter
Sleep 6s
Type "head -3 ~/Desktop/pd1_genes.csv"
Sleep 1s
Enter
Sleep 10s
