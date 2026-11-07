# dbparser.mcp demo: a small MCP client that runs the server the way Claude
# Code and Claude Desktop do (`Rscript -e "dbparser.mcp::serve()"`), speaks
# JSON-RPC to it over stdio and calls every tool in turn.
#
#   Rscript demo.R                         # HGNC sample bundled with dbparser
#   Rscript demo.R parseHGNC ~/data/hgnc_complete_set.txt ~/data/withdrawn.txt
#   Rscript demo.R parseOpenFDA ~/data/drug-ndc-0001-of-0001.json.zip
#   Rscript demo.R parseHealthCanadaDPD ~/data/health_canada_dpd
#
# The script's path inside the installed package:
#   system.file("scripts", "demo.R", package = "dbparser.mcp")
#
# It reads only the paths you give; the CSV export goes to a temp folder.
# Needs the processx package.

if (!requireNamespace("processx", quietly = TRUE)) {
  stop("The demo needs processx: install.packages(\"processx\")")
}

args <- commandArgs(trailingOnly = TRUE)

# -- a minimal MCP client --------------------------------------------------

server <- processx::process$new(
  file.path(R.home("bin"), "Rscript"), c("-e", "dbparser.mcp::serve()"),
  stdin = "|", stdout = "|", stderr = "|")

next_id <- 0L
pending <- character(0)

to_json <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
}

rpc <- function(method, params = NULL, timeout = 600) {
  next_id <<- next_id + 1L
  id <- next_id
  message <- list(jsonrpc = "2.0", id = id, method = method)
  if (!is.null(params)) message$params <- params
  server$write_input(paste0(to_json(message), "\n"))
  deadline <- Sys.time() + timeout
  repeat {
    for (i in seq_along(pending)) {
      reply <- jsonlite::parse_json(pending[i])
      if (identical(reply$id, id)) {
        pending <<- pending[-i]
        return(reply)
      }
    }
    if (Sys.time() > deadline || !server$is_alive()) {
      stop("No reply to ", method, ". Server stderr:\n", server$read_error())
    }
    server$poll_io(1000)
    pending <<- c(pending, server$read_output_lines())
  }
}

empty_object <- structure(list(), names = character(0))

call_tool <- function(name, arguments = empty_object) {
  shown <- if (length(arguments)) to_json(arguments) else "{}"
  cat("\n→ ", name, " ", shown, "\n", sep = "")
  started <- Sys.time()
  reply   <- rpc("tools/call", list(name = name, arguments = arguments))
  seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  if (!is.null(reply$error)) {
    stop("JSON-RPC error: ", reply$error$message)
  }
  text   <- reply$result$content[[1]]$text
  status <- if (isTRUE(reply$result$isError)) "error result" else "ok"
  cat(sprintf("← %s (%.1f s)\n", status, seconds))
  value <- tryCatch(
    jsonlite::parse_json(sub("^Tool calling failed with error ", "", text)),
    error = function(e) text)
  invisible(value)
}

show <- function(x, lines = 20L) {
  out <- strsplit(as.character(jsonlite::toJSON(x, auto_unbox = TRUE,
                                                null = "null", pretty = TRUE)),
                  "\n")[[1]]
  if (length(out) > lines) {
    out <- c(out[seq_len(lines)],
             sprintf("  ... (%d more lines)", length(out) - lines))
  }
  cat(paste0("  ", out), sep = "\n")
}

# -- the walk-through ----------------------------------------------------------

init <- rpc("initialize", list(
  protocolVersion = "2025-06-18", capabilities = empty_object,
  clientInfo = list(name = "dbparser.mcp-demo", version = "1")))
server$write_input(paste0(to_json(list(jsonrpc = "2.0",
                                       method = "notifications/initialized")),
                          "\n"))
cat("Connected to ", init$result$serverInfo$name, " (MCP ",
    init$result$protocolVersion, ")\n", sep = "")
tools <- rpc("tools/list")$result$tools
cat("Tools:", paste(vapply(tools, function(t) t$name, ""), collapse = ", "),
    "\n")

parsers <- call_tool("list_parsers")
cat("  dbparser", parsers$dbparser_version, "—",
    length(parsers$parsers), "parsers\n")
for (p in parsers$parsers) {
  cat(sprintf("  %-22s %-16s %s\n", p$parser, p$licence,
              if (is.null(p$sample_path)) "(no sample)" else "sample bundled"))
}

if (length(args) == 0L) {
  hgnc   <- Filter(function(p) p$parser == "parseHGNC", parsers$parsers)[[1]]
  parser <- "parseHGNC"
  path   <- hgnc$sample_path
  second <- hgnc$sample_second_path
} else {
  parser <- args[1]
  path   <- normalizePath(args[2], mustWork = FALSE)
  second <- if (length(args) > 2L) normalizePath(args[3], mustWork = FALSE)
}

parse_args <- list(parser = parser, path = path)
if (!is.null(second)) parse_args$second_path <- second
parsed <- call_tool("parse_file", parse_args)
if (is.null(parsed$object_id)) {
  show(parsed)
  quit(status = 1)
}
id <- parsed$object_id
cat(sprintf("  object %s: %d tables, %.1f MB, parsed in %.1f s\n", id,
            length(parsed$tables), parsed$memory_mb, parsed$seconds))
for (t in parsed$tables) cat(sprintf("    %-40s %9d rows\n", t$table, t$rows))
source <- parsed$source[[1]]
given <- function(x) if (is.null(x)) "not given" else x
cat("  source: ", source$source_name, " | release ", given(source$release),
    " | licence ", given(source$licence), "\n", sep = "")

objects <- call_tool("list_objects")
cat(sprintf("  %d object(s), %.1f MB of %.0f MB\n", length(objects$objects),
            objects$session_memory_mb, objects$memory_limit_mb))

described <- call_tool("describe", list(object_id = id))
first <- described$tables[[1]]
cat(sprintf("  %s: %d columns, e.g. %s\n", first$table, length(first$columns),
            paste(vapply(utils::head(first$columns, 5),
                         function(c) paste0(c$name, " <", c$type, ">"), ""),
                  collapse = ", ")))
files <- described$source[[1]]$files
for (f in files) cat("  file:", f$name, "sha256", substr(f$sha256, 1, 16), "...\n")

read <- call_tool("read_table", list(object_id = id, table = first$table,
                                     n = 3))
cat(sprintf("  returned_rows %d of total_rows %d\n", read$returned_rows,
            read$total_rows))
show(read$rows[[1]], lines = 12L)
for (note in read$notes) cat("  note:", note, "\n")

capped <- call_tool("read_table", list(object_id = id, table = first$table,
                                       n = 1000, offset = 5))
cat(sprintf("  asked for 1000, got %d (offset %d)\n", capped$returned_rows,
            capped$offset))
for (note in capped$notes) cat("  note:", note, "\n")

out <- file.path(tempdir(), paste0(gsub("[^A-Za-z0-9]+", "_", first$table),
                                   ".csv"))
exported <- call_tool("export_csv", list(object_id = id, table = first$table,
                                         path = out))
cat(sprintf("  wrote %d rows, %s bytes to %s\n", exported$rows,
            format(exported$bytes, big.mark = ","), exported$path))
cat(strwrap(exported$licence_note, indent = 2, exdent = 2), sep = "\n")

again <- call_tool("export_csv", list(object_id = id, table = first$table,
                                      path = out))
cat("  ", again$error, "\n  next step: ", again$next_step, "\n", sep = "")

refused <- call_tool("parse_file", list(parser = "parseHGNC",
                                        path = "https://example.org/x.txt"))
cat("  ", refused$error, "\n", sep = "")

# closing stdin ends the server
invisible(close(server$get_input_connection()))
invisible(server$wait(10000))
stray <- Filter(function(line) {
  nzchar(line) && is.null(tryCatch(jsonlite::parse_json(line)$jsonrpc,
                                   error = function(e) NULL))
}, c(pending, server$read_all_output_lines()))
cat("\nServer stopped (exit status ", server$get_exit_status(), "); ",
    length(stray), " non-JSON-RPC lines on stdout.\n", sep = "")
