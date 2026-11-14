# Recording the Claude Desktop demo

Optional footage of the same two questions in Claude Desktop. The rules are the
same as for the terminal recording:

- **Real output only.** Don't edit numbers or text on screen, and don't retype
  an answer. If Claude gets something wrong, keep the take and report it, or
  record it again.
- **No DrugBank content** (BR-1). Use only the HGNC files below.

## Before you start

1. Install the current build: in R,
   `remotes::install_github("interstellar-egypt/dbparser.mcp")`.
2. Delete any earlier export, because `export_csv` never overwrites:
   `rm -f ~/Desktop/pd1_genes.csv`
3. Quit Claude Desktop completely (Cmd+Q, not just closing the window) and
   open it again, so it starts the new server.
4. Check that it works. In a new chat, ask *"Which parsers does dbparser
   support?"* You should get the list of parsers. Delete that chat.
5. Tidy the screen: hide other windows and turn on Do Not Disturb.

## Record

1. Start a **new chat**.
2. Press **Cmd+Shift+5**, choose **Record Selected Portion**, drag the frame
   around the Claude Desktop chat window and click **Record**.
3. Paste the first prompt and send it:

   > Parse my HGNC file at ~/Documents/datasets/hgnc_complete_set.txt
   > (withdrawn.txt is next to it). Which genes does the name PD1 refer to?
   > Give the approved symbols, with the source and release.

   If Claude Desktop asks for permission to use a dbparser tool, allow it
   (for example "Allow for this chat"). Wait until the answer is complete.
   It should name all three genes PD1 refers to (PDCD1, SNCA, SPATA2),
   along with the source, release and licence.
4. Paste the second prompt and send it:

   > Export those three genes to ~/Desktop/pd1_genes.csv.

5. Stop recording when the CSV answer is finished: click the stop button in
   the menu bar, or press Cmd+Ctrl+Esc.
6. The recording is saved to the Desktop as a `.mov` file. Rename it
   `dbparser-mcp-desktop.mov` and send it over. Trimming the start and end is
   fine. Don't change anything on screen.
