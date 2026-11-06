suppressMessages({
  library(data.table)
  library(jsonlite)
})
source("R/common.R")
out <- "_site"
dir.create(file.path(out, "data"), showWarnings = FALSE, recursive = TRUE)
t <- load_tilldelningar()
alla <- fread(file.path(data_dir, "tilldelningar.csv"), encoding = "UTF-8", colClasses = "character", na.strings = "")
doda_personer <- unique(alla[!is.na(dödsdatum), person_id])
raw <- alla[!person_id %in% doda_personer]
fwrite(raw, file.path(out, "data", "tilldelningar.csv"), bom = TRUE)
write_json(as.data.frame(raw), file.path(out, "data", "tilldelningar.json"), pretty = TRUE, na = "null")
invisible(file.copy(file.path(data_dir, "grader.csv"), file.path(out, "data", "grader.csv"), overwrite = TRUE))

levande_fil <- function(namn, behall) {
  x <- fread(file.path(data_dir, namn), encoding = "UTF-8", colClasses = "character", na.strings = "")
  if (all(behall(x))) {
    invisible(file.copy(file.path(data_dir, namn), file.path(out, "data", namn), overwrite = TRUE))
  } else {
    fwrite(x[behall(x)], file.path(out, "data", namn), na = "")
  }
}
levande_fil("andringslogg.csv", function(x) x$id %in% raw$id | !x$id %in% alla$id)
levande_fil("kmo_verifierade.csv", function(x) x$id %in% raw$id | !x$id %in% alla$id)
levande_bilder <- unique(basename(raw$bild_url[!is.na(raw$bild_url)]))
doda_bilder <- setdiff(basename(alla$bild_url[!is.na(alla$bild_url)]), levande_bilder)
doda_qid <- setdiff(alla$wikidata_qid[alla$person_id %in% doda_personer & !is.na(alla$wikidata_qid)], raw$wikidata_qid)
levande_fil("bilder.csv", function(x) !(x$fil %in% doda_bilder | (x$person_qid %in% doda_qid & !x$fil %in% levande_bilder)))

# Stamp every generated page after person profiles have been built.
uppdaterad <- format(Sys.time(), "%Y-%m-%d %H:%M", tz = "Europe/Stockholm")
for (f in list.files(out, pattern = "\\.html$", recursive = TRUE, full.names = TRUE)) {
  html <- readLines(f, encoding = "UTF-8", warn = FALSE)
  html <- gsub("UPPDATERINGSTIDMARKOR", paste0(uppdaterad, " (svensk tid)"), html, fixed = TRUE)
  writeLines(html, f, useBytes = TRUE)
}
base <- Sys.getenv("SITE_URL", "https://leolevenius.github.io/kungliga-ordnar/")
logg <- load_logg()
added <- logg[andring == "tillagd", .(added = min(datum)), by = id]
modified <- logg[, .(modified = max(datum)), by = id]
latest <- merge(merge(t, added, by = "id", all.x = TRUE), modified, by = "id", all.x = TRUE)
latest <- latest[!is.na(added)][order(-xtfrm(added), -xtfrm(id))][seq_len(min(50L, .N))]
esc <- function(x) as.character(htmlEscape(x))
attr_esc <- function(x) as.character(htmlEscape(x, attribute = TRUE))
entries <- sprintf(
  "  <entry>\n    <title>%s – %s, %s</title>\n    <id>tag:leolevenius.github.io,2026:kungliga-ordnar/%s</id>\n    <link href=\"%sperson/%s.html\"/>\n    <published>%sT00:00:00Z</published>\n    <updated>%sT00:00:00Z</updated>\n    <summary>%s</summary>\n  </entry>",
  esc(latest$namn), esc(latest$orden), esc(latest$grad), attr_esc(latest$id),
  attr_esc(base), attr_esc(latest$person_id), latest$added, latest$modified,
  esc(paste0(latest$orden, " ", latest$grad, ifelse(is.na(latest$datum), "", paste0(" (", latest$datum, ")"))))
)
feed <- c(
  '<?xml version="1.0" encoding="utf-8"?>',
  '<feed xmlns="http://www.w3.org/2005/Atom">',
  '  <title>Kungliga ordnar: nya tilldelningar</title>',
  '  <author><name>Leo G. Levenius</name></author>',
  '  <id>tag:leolevenius.github.io,2026:kungliga-ordnar/feed</id>',
  sprintf('  <updated>%sT00:00:00Z</updated>', max(latest$modified)),
  sprintf('  <link rel="self" href="%sfeed.xml"/>', attr_esc(base)),
  entries, '</feed>'
)
writeLines(feed, file.path(out, "feed.xml"), useBytes = TRUE)
cat("export ok:", nrow(raw), "rows,", nrow(latest), "feed entries\n")
