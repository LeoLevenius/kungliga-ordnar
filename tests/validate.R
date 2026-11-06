suppressMessages({
  library(data.table)
  library(httr2)
})

# Shared rules with redigera.js (validera): dates are compared against today in UTC, text fields are trimmed
# (fread strips surrounding blanks), and an empty grade is accepted only as a legacy state of existing rows;
# the editor requires a grade for new rows and never lets an existing row go from graded to empty.
args <- commandArgs(trailingOnly = TRUE)
IDAG <- as.Date(format(Sys.time(), "%Y-%m-%d", tz = "UTC"))
UA <- "kungliga-ordnar-research/0.1 (generic client)"

t <- fread("data/tilldelningar.csv", encoding = "UTF-8", colClasses = "character", na.strings = "")
g <- fread("data/grader.csv", encoding = "UTF-8", colClasses = "character", na.strings = "")

fel <- 0L
kontroll <- function(namn, ok, offenders = NULL) {
  ok <- ok & !is.na(ok)
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
  ok[dag] <- !is.na(as.Date(d[dag], format = "%Y-%m-%d")) & as.Date(d[dag], format = "%Y-%m-%d") <= IDAG & as.integer(substr(d[dag], 1, 4)) >= 1700
  man <- !is.na(p) & p == "månad" & grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", d)
  ok[man] <- substr(d[man], 1, 7) <= format(IDAG, "%Y-%m") & as.integer(substr(d[man], 1, 4)) >= 1700
  ar <- !is.na(p) & p == "år" & grepl("^[0-9]{4}$", d)
  ok[ar] <- as.integer(d[ar]) >= 1700 & as.integer(d[ar]) <= as.integer(format(IDAG, "%Y"))
  ok
}

cat(sprintf("Rader i tilldelningar.csv: %d\n\n", nrow(t)))

kontroll("id är unika och ifyllda", !is.na(t$id) & !duplicated(t$id), t$id)
kontroll("angiven grad finns i grader.csv för sin orden", is.na(t$grad) | paste(t$orden, t$grad) %in% paste(g$orden, g$grad), paste(t$orden, t$grad))
kontroll("alla datum går att tolka enligt datum_precision (okänt datum: båda tomma)",
         datum_ok(t$datum_tilldelning, t$datum_precision) | (is.na(t$datum_tilldelning) & is.na(t$datum_precision)),
         paste(t$id, t$datum_tilldelning, t$datum_precision))
nyck <- paste(nyckel(t$namn), t$orden, t$grad, t$datum_tilldelning)
kontroll("inga dubbletter (normaliserat namn + orden + grad + datum)", !duplicated(nyck) & !duplicated(nyck, fromLast = TRUE), t$id)
t[, rang := as.integer(g$rang[match(paste(orden, grad), paste(g$orden, g$grad))])]
dubb <- t[!is.na(rang), .(n = .N), by = .(person_id, orden, rang, ar = substr(datum_tilldelning, 1, 4))][n > 1]
kontroll("inga dubbletter per person, orden, rang och år (olika gradnamn med samma rang räknas som samma grad)",
         !paste(t$person_id, t$orden, t$rang, substr(t$datum_tilldelning, 1, 4)) %in% dubb[, paste(person_id, orden, rang, ar)] | is.na(t$rang), t$id)
personfalt <- c("namn", "kön", "kategori", "land", "kunglig", "wikidata_qid", "dödsdatum")
for (f in personfalt) {
  avvikande <- t[, .(n = uniqueN(get(f), na.rm = FALSE)), by = person_id][n > 1, person_id]
  kontroll(sprintf("personfältet %s är lika på alla en persons rader", f), !t$person_id %in% avvikande, t$person_id)
}
kontroll("ingen rad saknar källa_url", !is.na(t[["källa_url"]]) & grepl("^https?://", t[["källa_url"]]), t$id)
kontroll("motivering_typ=statsbesök har ifyllt statsbesök", is.na(t$motivering_typ) | t$motivering_typ != "statsbesök" | !is.na(t[["statsbesök"]]), t$id)
kontroll("orden är en av de fyra ordnarna", t$orden %in% c("Serafimerorden", "Svärdsorden", "Nordstjärneorden", "Vasaorden"), t$id)
kontroll("kategori är svensk eller utländsk", t$kategori %in% c("svensk", "utländsk"), t$id)
kontroll("kategori matchar land (svensk = Sverige eller okänt, utländsk = annat land eller okänt)",
         (t$kategori == "svensk" & (is.na(t$land) | t$land == "Sverige")) | (t$kategori == "utländsk" & (is.na(t$land) | t$land != "Sverige")), paste(t$id, t$kategori, t$land))
kontroll("kunglig är TRUE eller FALSE", t$kunglig %in% c("TRUE", "FALSE"), t$id)
kontroll("källtyp är en tillåten typ", t[["källtyp"]] %in% c("kungahuset", "regeringen", "officiell_publikation", "tidning", "wikipedia"), t$id)
kontroll("motivering_typ är officiell, statsbesök eller tom", is.na(t$motivering_typ) | t$motivering_typ %in% c("officiell", "statsbesök"), t$id)
kontroll("kön är man, kvinna eller tomt", is.na(t[["kön"]]) | t[["kön"]] %in% c("man", "kvinna"), t$id)
dod <- t[["dödsdatum"]]
dod_prec <- fifelse(is.na(dod), NA_character_, fifelse(nchar(dod) == 4, "år", fifelse(nchar(dod) == 7, "månad", "dag")))
kontroll("dödsdatum är ett giltigt datum som inte ligger i framtiden", is.na(dod) | datum_ok(dod, dod_prec), t$id)
flera_person <- t[!is.na(wikidata_qid), .(n = uniqueN(person_id)), by = wikidata_qid][n > 1, wikidata_qid]
kontroll("ett person_id per Wikidata-id", !(t$wikidata_qid %in% flera_person), paste(t$id, t$wikidata_qid))

med_bild <- !is.na(t$bild_url)
kontroll("rader med bild har licens, upphovsperson och en fil som finns",
         !med_bild | (!is.na(t$bild_licens) & !is.na(t$bild_upphov) & file.exists(ifelse(med_bild, t$bild_url, "."))), t$id)
b <- fread("data/bilder.csv", encoding = "UTF-8", colClasses = "character", na.strings = "")
kontroll("bilder.csv: endast fria licenser, upphovsperson och fil finns",
         b$licens %in% c("CC0", "Public domain", "CC BY-SA 2.5", "CC BY-SA 3.0", "CC BY-SA 4.0", "CC BY 2.5", "CC BY 3.0", "CC BY 4.0") &
           !is.na(b$upphov) & file.exists(file.path("img", b$fil)), b$fil)

skold <- !is.na(t$bild_url) & grepl("^img/skold-", t$bild_url)
kontroll("varje bild i tilldelningar.csv finns i bilder.csv (så att Bildkällor kan krediteras utan person_qid)",
         is.na(t$bild_url) | basename(ifelse(is.na(t$bild_url), "", t$bild_url)) %in% b$fil, t$id)
kontroll("varje visad sköld har bilder.csv-rad med anvandning sköld, upphovsperson, licens och commons-sida",
         !skold | (b$anvandning[match(basename(t$bild_url), b$fil)] %in% "sköld" & !is.na(b$upphov[match(basename(t$bild_url), b$fil)]) &
                   !is.na(b$licens[match(basename(t$bild_url), b$fil)]) & !is.na(b$commons_sida[match(basename(t$bild_url), b$fil)])), t$id)

kv <- fread("data/kmo_verifierade.csv", encoding = "UTF-8", colClasses = "character", na.strings = "")
kontroll("kmo_verifierade.csv: id och person_id finns i tilldelningar.csv, med länk",
         kv$id %in% t$id & kv$person_id == t$person_id[match(kv$id, t$id)] & grepl("^https://kungligmajestatsorden.se/", kv$kmo_url) & !duplicated(kv$id), kv$id)

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
