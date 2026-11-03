suppressMessages({
  library(data.table)
  library(jsonlite)
})
source("R/common.R")
out <- "_site"
dir.create(file.path(out, "data"), showWarnings = FALSE, recursive = TRUE)
t <- load_tilldelningar()
raw <- fread(file.path(data_dir, "tilldelningar.csv"), encoding = "UTF-8", colClasses = "character", na.strings = "")
raw <- raw[is.na(dödsdatum)]
fwrite(raw, file.path(out, "data", "tilldelningar.csv"), bom = TRUE)
write_json(as.data.frame(raw), file.path(out, "data", "tilldelningar.json"), pretty = TRUE, na = "null")
invisible(file.copy(file.path(data_dir, "grader.csv"), file.path(out, "data", "grader.csv"), overwrite = TRUE))
invisible(file.copy(file.path(data_dir, "andringslogg.csv"), file.path(out, "data", "andringslogg.csv"), overwrite = TRUE))

base <- Sys.getenv("SITE_URL", "")
latest <- t[order(-as.Date(datum), id)][seq_len(min(50, .N))]
esc <- function(x) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE), fixed = TRUE)
entries <- sprintf(
  "  <entry>\n    <title>%s – %s, %s</title>\n    <id>tag:kungliga-ordnar,%s:%s</id>\n    <link href=\"%sperson/%s.html\"/>\n    <updated>%sT00:00:00Z</updated>\n    <summary>%s</summary>\n  </entry>",
  esc(latest$namn), esc(latest$orden), esc(latest$grad), format(Sys.Date(), "%Y"), latest$id,
  base, latest$person_id, latest$datum, esc(paste(latest$orden, latest$grad, sep = ", "))
)
feed <- c(
  '<?xml version="1.0" encoding="utf-8"?>',
  '<feed xmlns="http://www.w3.org/2005/Atom">',
  '  <title>Kungliga ordnar: nya tilldelningar</title>',
  sprintf('  <id>tag:kungliga-ordnar,feed</id>'),
  sprintf('  <updated>%sT00:00:00Z</updated>', max(as.Date(t$datum))),
  sprintf('  <link rel="self" href="%sfeed.xml"/>', base),
  entries,
  '</feed>'
)
writeLines(feed, file.path(out, "feed.xml"), useBytes = TRUE)
cat("export ok:", nrow(raw), "rows,", nrow(latest), "feed entries\n")
