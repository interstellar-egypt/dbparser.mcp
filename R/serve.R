#' Start the dbparser MCP server (stdio)
#'
#' Runs a local Model Context Protocol server over standard input and output,
#' for MCP clients such as Claude Code and Claude Desktop. It blocks the R
#' process until the client closes the connection. There is no HTTP mode:
#' nothing listens on a network port.
#'
#' The tools run in this server process (\code{mcptools} session forwarding is
#' off), so parsed objects stay in one place until the server stops.
#'
#' @return Called for its side effect; does not return while the client is
#'   connected.
#' @export
serve <- function() {
  mcptools::mcp_server(tools = mcp_tools(), type = "stdio",
                       session_tools = FALSE)
}
