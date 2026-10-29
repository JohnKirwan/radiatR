# Tools for driving radiatR from an AI agent

Returns a set of
[`ellmer::tool()`](https://ellmer.tidyverse.org/reference/tool.html)
definitions that let a language model run a radiatR analysis: load
tracks or a table of headings, derive headings, summarise, test and
plot. Register them with an ellmer chat, or serve them to an MCP client
such as Claude Code or Claude Desktop with the mcptools package.

## Usage

``` r
radiatR_tools()
```

## Value

A named list of `ellmer` tool definitions.

## Details

Each call to `radiatR_tools()` creates its own private session. Objects
are referred to by handles (`"tracks_1"`, `"headings_1"`); every result
includes the R code that produced it, and the `radiatR_export_script`
tool returns the whole session as one script that reproduces the data
objects and result tables exactly.

The tools do not fill in what was left unsaid: the time type, the unit
of any angle column, the convention of a table of angles, and a seed for
heading derivation and for every test must be stated. A missing or
invalid argument is an error rather than a default, and so is an
argument the call would ignore. Some things are still chosen rather than
stated: `load_tracks` reads a file as
[`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
does, sniffing its delimiter and decimal mark, reading date-times as
UTC, and taking a dialect's defaults (such as which body parts make up
the position); it refuses a workbook with more than one sheet. Warnings
and printed output are returned to the model in the result instead of
being written to the console, and an error message carries the condition
class, the failing call, and anything raised before the failure.

## Serving over MCP

    claude mcp add radiatR -- Rscript -e \
      'mcptools::mcp_server(tools = radiatR::radiatR_tools(), session_tools = FALSE)'

`session_tools = FALSE` keeps every call in the server process, where
the handles live. Handles therefore cannot be inspected from another R
console; the exported script is the bridge. Anything R prints at startup
corrupts the MCP stream: in a project that uses renv, set
`RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE` for the server.

## Examples

``` r
tools <- radiatR_tools()
names(tools)
#>  [1] "list_options"    "load_tracks"     "load_headings"   "derive_headings"
#>  [5] "describe"        "summarise"       "test_uniformity" "compare_groups" 
#>  [9] "plot"            "export_script"  
```
