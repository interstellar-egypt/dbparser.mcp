# dbparser.mcp

A local, read-only [Model Context Protocol](https://modelcontextprotocol.io/)
(MCP) server for [dbparser](https://docs.ropensci.org/dbparser/). It works
with Claude Code and Claude Desktop: the assistant can parse drug and gene
database files on your computer with dbparser, describe the tables, and tell
you each source's release, licence and the citation it asks for.

- **Local only.** The server runs over standard input and output (stdio). It
  opens no network port and has no HTTP mode.
- **Your own files only.** It reads files and folders on your computer and
  never downloads anything; URLs are refused.
- **DrugBank only from your own download.** DrugBank is not an open database:
  the server parses it only from a file you downloaded under your own DrugBank
  account and licence, never from a file inside an R package.
- **Sources keep their licences.** Every answer carries the source, release
  and licence recorded by dbparser. Each source keeps its own licence; check
  its terms before use.

## Tools

| Tool | What it does |
|---|---|
| `list_parsers` | The parsers, the files each reads, the tables it returns, the source's licence, and the bundled sample files you can try. |
| `parse_file` | Parse a local file or folder into an object kept in the server session; returns an `object_id`, the tables, `memory_mb` and the source. |
| `describe` | Tables, row counts, columns and types of a parsed object, and its full source metadata (with each file's SHA-256). |
| `list_objects` | The objects parsed in this session, their tables and memory, and the session's memory use and limit. |
| `read_table` | Rows of one table: 20 by default, at most 100 a call, paged with `offset`. Always gives `returned_rows` and `total_rows`; text cells over 500 characters are cut, with a note naming their columns. |
| `export_csv` | Write one whole table to a new CSV file. It never overwrites an existing file, and the result gives each source's licence and attribution to keep with the file. |

`export_csv` is the only tool that writes anything, and only a new file at the
path you give. Names are matched exactly; an unknown parser, object or argument value gets an
error that lists the valid ones and says what to do next.

The open-data parsers (HGNC, Health Canada DPD, openFDA, OnSIDES, TWOSIDES,
HPO) can be tried on small sample extracts bundled with dbparser, so a new
user needs no downloads; `list_parsers` gives their paths.

## Install

dbparser.mcp needs dbparser 2.3.0 or later.

``` r
# install.packages("pak")
pak::pak("interstellar-egypt/dbparser.mcp")
```

While dbparser 2.3.0 is not yet on CRAN, the package's `DESCRIPTION` has a
`Remotes: ropensci/dbparser@v2.3` line so it installs from GitHub. That line is
removed before any CRAN release.

## Set up

**Claude Code** (in a terminal):

``` sh
claude mcp add -s user dbparser -- Rscript -e "dbparser.mcp::serve()"
```

Check with `claude mcp list`, which should show `dbparser: ... ✔ Connected`.

**Claude Desktop:** add this to `claude_desktop_config.json` (on macOS
`~/Library/Application Support/Claude/claude_desktop_config.json`), then
restart Claude Desktop:

``` json
{
  "mcpServers": {
    "dbparser": {
      "command": "Rscript",
      "args": ["-e", "dbparser.mcp::serve()"]
    }
  }
}
```

On Windows you may need the full path to `Rscript`.

## Memory

Parsed objects stay in memory until the server stops (restarting the client
restarts the server). The full openFDA Drugs@FDA and NDC files take about a
minute and roughly 300 MB; a full DrugBank XML takes several minutes and
several GB. `parse_file` reports each object's `memory_mb`. Before a new parse
the server checks what is already loaded and refuses when the total is over
`DBPARSER_MCP_MAX_MB` (default 4000), asking you to start a new session.

## Not yet supported

- **Claude Science:** not yet verified; its MCP setup has not been checked
  against this server.
- **ChatGPT:** ChatGPT connects to MCP servers over HTTPS (or through
  OpenAI's Secure MCP Tunnel), not to a local stdio server, so this package
  does not work with it.

## Licence

MIT. The server code only; each database keeps its own licence.
