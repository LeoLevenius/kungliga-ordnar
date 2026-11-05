suppressMessages({
  library(htmltools)
  library(jsonlite)
})
source("R/common.R")
t <- load_tilldelningar()
mall_fil <- "_site/mallar/person.html"
mall <- paste(readLines(mall_fil, encoding = "UTF-8", warn = FALSE), collapse = "\n")
delar <- regmatches(mall, gregexpr("(PERSONTITELMARKOR|PERSONINNEHALLMARKOR)", mall), invert = TRUE)[[1]]
marker <- regmatches(mall, gregexpr("(PERSONTITELMARKOR|PERSONINNEHALLMARKOR)", mall))[[1]]

ids <- unique(t$person_id)
namn <- t[, namn[1], by = person_id][match(ids, person_id), V1]
info <- t[, .(kungl = kunglig[1] == "TRUE"), by = person_id][match(ids, person_id)]
namn_h1 <- namn_html_vec(ids, namn, info$kungl)
inneh <- vapply(ids, function(id) as.character(person_page(t, id, pfx = "../")), character(1))
sida <- vapply(seq_along(ids), function(i) {
  h1 <- marker == "PERSONTITELMARKOR" & endsWith(delar[seq_along(marker)], '<h1 class="title">')
  v <- ifelse(marker == "PERSONTITELMARKOR", ifelse(h1, namn_h1[i], htmlEscape(namn[i])), inneh[i])
  paste0(delar, c(v, ""), collapse = "")
}, character(1))

dir.create("_site/person", showWarnings = FALSE)
for (i in seq_along(ids)) writeLines(sida[i], file.path("_site/person", paste0(ids[i], ".html")), useBytes = TRUE)
unlink("_site/mallar", recursive = TRUE)

text <- gsub("<[^>]+>", "", inneh)
text <- gsub("&lt;", "<", gsub("&gt;", ">", gsub("&quot;", "\"", gsub("&#39;", "'", gsub("&amp;", "&", text, fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE)
sok <- fromJSON("_site/search.json", simplifyVector = FALSE)
sok <- Filter(function(x) !grepl("^(mallar|person)/", x$href), sok)
ny <- lapply(seq_along(ids), function(i) {
  h <- paste0("person/", ids[i], ".html")
  list(objectID = h, href = h, title = namn[i], section = "", text = text[i])
})
write_json(c(sok, ny), "_site/search.json", auto_unbox = TRUE, pretty = FALSE)
cat("person pages:", length(ids), "\n")
