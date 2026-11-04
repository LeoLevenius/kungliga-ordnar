suppressMessages({
  library(data.table)
  library(httr2)
})

args <- commandArgs(trailingOnly = TRUE)
UA <- "kungliga-ordnar-research/0.1 (generic client)"

t <- fread("data/tilldelningar.csv", encoding = "UTF-8", colClasses = "character", na.strings = "")
g <- fread("data/grader.csv", encoding = "UTF-8", colClasses = "character", na.strings = "")

fel <- 0L
kontroll <- function(namn, ok, offenders = NULL) {
  cat(sprintf("[%s] %s\n", if (all(ok)) "OK  " else "FEL ", namn))
  if (!all(ok)) {
    fel <<- fel + 1L
    cat(sprintf("       %d rader, t.ex.: %s\n", sum(!ok), paste(head(offenders[!ok], 5), collapse = "; ")))
  }
}

nyckel <- function(x) {
  x <- chartr("åäöÅÄÖ", "aaoAAO", x)
  x <- tolower(gsub("[^A-Za-z0-9 ]", "", iconv(x, to = "ASCII//TRANSLIT")))
  gsub("\\s+", " ", trimws(x))
}

datum_ok <- function(d, p) {
  ok <- rep(FALSE, length(d))
  dag <- !is.na(p) & p == "dag" & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", d)
  ok[dag] <- !is.na(as.Date(d[dag], format = "%Y-%m-%d")) & as.Date(d[dag], format = "%Y-%m-%d") <= Sys.Date()
  man <- !is.na(p) & p == "månad" & grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", d)
  ok[man] <- as.integer(substr(d[man], 1, 4)) <= as.integer(format(Sys.Date(), "%Y"))
  ar <- !is.na(p) & p == "år" & grepl("^[0-9]{4}$", d)
  ok[ar] <- as.integer(d[ar]) >= 1700 & as.integer(d[ar]) <= as.integer(format(Sys.Date(), "%Y"))
  ok
}

cat(sprintf("Rader i tilldelningar.csv: %d\n\n", nrow(t)))

kontroll("id är unika och ifyllda", !is.na(t$id) & !duplicated(t$id), t$id)
kontroll("varje grad finns i grader.csv för sin orden", paste(t$orden, t$grad) %in% paste(g$orden, g$grad), paste(t$orden, t$grad))
kontroll("alla datum går att tolka enligt datum_precision", datum_ok(t$datum_tilldelning, t$datum_precision), paste(t$id, t$datum_tilldelning, t$datum_precision))
nyck <- paste(nyckel(t$namn), t$orden, t$grad, t$datum_tilldelning)
kontroll("inga dubbletter (normaliserat namn + orden + grad + datum)", !duplicated(nyck) & !duplicated(nyck, fromLast = TRUE), t$id)
kontroll("ingen rad saknar källa_url", !is.na(t[["källa_url"]]) & grepl("^https?://", t[["källa_url"]]), t$id)
kontroll("motivering_typ=statsbesök har ifyllt statsbesök", is.na(t$motivering_typ) | t$motivering_typ != "statsbesök" | !is.na(t[["statsbesök"]]), t$id)
kontroll("orden är en av de fyra ordnarna", t$orden %in% c("Serafimerorden", "Svärdsorden", "Nordstjärneorden", "Vasaorden"), t$id)
kontroll("kategori är svensk eller utländsk", t$kategori %in% c("svensk", "utländsk"), t$id)
kontroll("kunglig är TRUE eller FALSE", t$kunglig %in% c("TRUE", "FALSE"), t$id)
kontroll("källtyp är en tillåten typ", t[["källtyp"]] %in% c("kungahuset", "regeringen", "officiell_publikation", "tidning", "wikipedia"), t$id)
kontroll("motivering_typ är officiell, statsbesök eller tom", is.na(t$motivering_typ) | t$motivering_typ %in% c("officiell", "statsbesök"), t$id)
kontroll("kön är man, kvinna eller tomt", is.na(t[["kön"]]) | t[["kön"]] %in% c("man", "kvinna"), t$id)
kontroll("dödsdatum är en tolkbar tid eller tomt", is.na(t[["dödsdatum"]]) | grepl("^[0-9]{4}(-[0-9]{2}(-[0-9]{2})?)?$", t[["dödsdatum"]]), t$id)
flera_person <- t[!is.na(wikidata_qid), .(n = uniqueN(person_id)), by = wikidata_qid][n > 1, wikidata_qid]
kontroll("ett person_id per Wikidata-id", !(t$wikidata_qid %in% flera_person), paste(t$id, t$wikidata_qid))

med_bild <- !is.na(t$bild_url)
kontroll("rader med bild har licens, upphovsperson och en fil som finns",
         !med_bild | (!is.na(t$bild_licens) & !is.na(t$bild_upphov) & file.exists(ifelse(med_bild, t$bild_url, "."))), t$id)
b <- fread("data/bilder.csv", encoding = "UTF-8", colClasses = "character", na.strings = "")
kontroll("bilder.csv: endast fria licenser, upphovsperson och fil finns",
         b$licens %in% c("CC0", "Public domain", "CC BY-SA 2.5", "CC BY-SA 3.0", "CC BY-SA 4.0", "CC BY 2.5", "CC BY 3.0", "CC BY 4.0") &
           !is.na(b$upphov) & file.exists(file.path("img", b$fil)), b$fil)

l <- fread("data/andringslogg.csv", encoding = "UTF-8", colClasses = "character", na.strings = "")
kontroll("ändringsloggen har datum, id och andring", all(c("datum", "id", "andring") %in% names(l)) & all(l$andring %in% c("tillagd", "ändrad")), l$id)

if ("--links" %in% args) {
  cat("\nKontrollerar att varje källa_url svarar med HTTP 200 (kan ta flera minuter)\n")
  adr <- unique(t[["källa_url"]])
  senast <- new.env()
  status <- integer(length(adr))
  for (i in seq_along(adr)) {
    host <- sub("^https?://([^/]+).*$", "\\1", adr[i])
    s <- if (exists(host, envir = senast)) get(host, envir = senast) else 0
    rest <- 2.1 - (as.numeric(Sys.time()) - s)
    if (rest > 0) Sys.sleep(rest)
    assign(host, as.numeric(Sys.time()), envir = senast)
    hamta <- function(metod) {
      tryCatch(
        resp_status(request(adr[i]) |> req_user_agent(UA) |> req_method(metod) |> req_timeout(40) |>
                      req_error(is_error = function(r) FALSE) |> req_perform()),
        error = function(e) NA_integer_)
    }
    status[i] <- hamta("HEAD")
    if (is.na(status[i]) || status[i] != 200) status[i] <- hamta("GET")
  }
  kontroll("varje källa_url svarar med HTTP 200", !is.na(status) & status == 200, paste0(adr, " (", status, ")"))
}

if (fel > 0) {
  cat(sprintf("\n%d kontroller misslyckades.\n", fel))
  quit(status = 1)
}
cat("\nAlla kontroller godkända.\n")
