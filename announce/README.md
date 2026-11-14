# Demo recording

Recorded with dbparser.mcp 0.1.0, commit 21802a5, on 14 November 2026, in
Claude Code 2.1.322 (Opus 5.5), using the full HGNC download
(`hgnc_complete_set.txt` and `withdrawn.txt`, CC0-1.0). No DrugBank data.

| File | What it is |
|---|---|
| `dbparser-mcp-demo.mp4` | 30 s, 1920x1080, H.264, yuv420p, for LinkedIn and the website |
| `../man/figures/dbparser-mcp-demo.gif` | the same, 960 px wide, 10 fps, for the README |
| `dbparser-mcp-demo-poster.png` | one frame: the answer with source, licence and attribution |
| `dbparser-mcp-demo.tape` | the session: two prompts typed into Claude Code |
| `dbparser-mcp-calls.tape` | the same session resumed with ctrl+o, to show the tool calls, then `head -3` of the CSV |
| `card-open.tape`, `card-close.tape` | the two title cards |
| `post.sh` | the cut, with ffmpeg only |
| `dbparser-mcp.json` | the MCP config Claude Code is started with |
| `desktop-recording.md` | steps for recording the same demo in Claude Desktop |

## What is real

Everything on screen is real output from one Claude Code session. Nothing was
retyped or edited.

- Claude Code runs with only the six dbparser tools (`--tools ""`,
  `--strict-mcp-config`), all pre-approved, so no permission prompts appear.
- The two prompts are typed by VHS, word for word.
- `post.sh` only cuts, speeds up the waits (typing up to 4x, the spinners
  8-10x) and holds single real frames. It draws nothing over the screen.
- Claude Code's compact view folds the calls into "Called dbparser 5
  times". To show them, `dbparser-mcp-calls.tape` resumes the same saved
  session (`claude --resume`) and opens the detailed transcript (ctrl+o).
  Nothing is asked again. The video shows three frames from that view
  (`parse_file`; `read_table` on `gene_name`, filtered to name PD1;
  `read_table` on `gene`, filtered to the three HGNC IDs) between prompt 1
  and its answer, plus the `head -3` of the exported file at the end.

## Re-recording

From the repo root, with `brew install vhs` (which pulls in ttyd and ffmpeg),
the package installed and the HGNC files in `~/Documents/datasets`:

``` sh
vhs announce/card-open.tape && vhs announce/card-close.tape
vhs announce/dbparser-mcp-demo.tape     # deletes ~/Desktop/pd1_genes.csv first
# put the new session id (printed when Claude exits) in dbparser-mcp-calls.tape
vhs announce/dbparser-mcp-calls.tape
# set the cut times in post.sh for the new take, then:
sh announce/post.sh
```

A new take will differ, since Claude's answers vary. Check that it lists all
three genes, and keep it whatever it says.
