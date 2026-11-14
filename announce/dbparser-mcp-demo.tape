# Records announce/raw.mp4: Claude Code with only the dbparser MCP tools,
# asking about the user's own HGNC download. Re-record from the repo root:
#   vhs announce/dbparser-mcp-demo.tape && sh announce/post.sh
# Needs vhs, ttyd, ffmpeg, the installed dbparser.mcp, and the HGNC files in
# ~/Documents/datasets. Real output only: nothing on screen is edited.

Output announce/raw.mp4

Set Shell bash
Set FontFamily "Menlo"
Set FontSize 22
Set Width 1920
Set Height 1080
Set Padding 36
Set Theme "Catppuccin Mocha"
Set TypingSpeed 30ms
Set CursorBlink false

# setup, not recorded: short prompt, no earlier export (export_csv never
# overwrites), run from announce/ so the trusted project folder applies, and
# drop any CLAUDE* variables inherited from a Claude Code session that runs vhs
Hide
Type "unset $(env | grep -o '^CLAUDE[A-Z_]*') ; export PS1='$ ' && cd announce && rm -f ~/Desktop/pd1_genes.csv && clear"
Enter
Sleep 1s
Show

# only the dbparser tools exist (--tools "" removes the built-in ones,
# --strict-mcp-config every other MCP server); they are pre-approved, and
# the default permission mode applies whatever the local settings say
Type `claude --allowedTools "mcp__dbparser__*" --tools "" --permission-mode default --strict-mcp-config --mcp-config dbparser-mcp.json`
Enter
Sleep 12s

Type "Parse my HGNC file at ~/Documents/datasets/hgnc_complete_set.txt (withdrawn.txt is next to it; release 2026-10, downloaded 16 Oct 2026). Which genes does the name PD1 refer to? Give the approved symbols, with the source and release."
Sleep 1s
Enter
Sleep 150s

Type "Export those three genes to ~/Desktop/pd1_genes.csv."
Sleep 1s
Enter
Sleep 90s

Type "/exit"
Sleep 1s
Enter
Sleep 4s

Type "head -3 ~/Desktop/pd1_genes.csv"
Enter
Sleep 5s
