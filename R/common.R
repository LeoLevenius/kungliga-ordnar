suppressMessages({
  library(data.table)
  library(htmltools)
  library(reactable)
})

data_dir <- if (dir.exists("data")) "data" else "../data"

ORDNAR <- c("Serafimerorden", "Svärdsorden", "Nordstjärneorden", "Vasaorden")

datum_date <- function(x) as.Date(ifelse(nchar(x) == 4, paste0(x, "-01-01"), x))

MANADER <- c("januari", "februari", "mars", "april", "maj", "juni", "juli",
             "augusti", "september", "oktober", "november", "december")

fmt_datum <- function(x) {
  ok <- !is.na(x) & grepl("^[0-9]{4}(-[0-9]{2}(-[0-9]{2})?)?$", x)
  y <- substr(x, 1, 4)
  m <- MANADER[suppressWarnings(as.integer(ifelse(ok, substr(x, 6, 7), NA)))]
  d <- suppressWarnings(as.integer(ifelse(ok, substr(x, 9, 10), NA)))
  out <- fifelse(nchar(x) == 4, y, fifelse(nchar(x) == 7, paste(m, y), paste(d, m, y)))
  fifelse(ok, out, x)
}

sortnyckel <- function(x) {
  par <- c("Å" = "{", "å" = "{", "Ä" = "|", "ä" = "|", "Æ" = "|", "æ" = "|",
           "Ö" = "}", "ö" = "}", "Ø" = "}", "ø" = "}",
           "Á" = "A", "À" = "A", "Â" = "A", "Ã" = "A", "á" = "a", "à" = "a", "â" = "a", "ã" = "a",
           "É" = "E", "È" = "E", "Ê" = "E", "Ë" = "E", "é" = "e", "è" = "e", "ê" = "e", "ë" = "e",
           "Í" = "I", "Ì" = "I", "Î" = "I", "Ï" = "I", "í" = "i", "ì" = "i", "î" = "i", "ï" = "i",
           "Ó" = "O", "Ò" = "O", "Ô" = "O", "Õ" = "O", "ó" = "o", "ò" = "o", "ô" = "o", "õ" = "o",
           "Ú" = "U", "Ù" = "U", "Û" = "U", "Ü" = "U", "ú" = "u", "ù" = "u", "û" = "u", "ü" = "u",
           "Ý" = "Y", "ý" = "y", "ÿ" = "y", "Ñ" = "N", "ñ" = "n", "Ç" = "C", "ç" = "c",
           "Š" = "S", "š" = "s", "Ž" = "Z", "ž" = "z", "Ł" = "L", "ł" = "l", "Ć" = "C", "ć" = "c",
           "Ń" = "N", "ń" = "n", "Ś" = "S", "ś" = "s", "Ź" = "Z", "ź" = "z", "Ż" = "Z", "ż" = "z",
           "Č" = "C", "č" = "c", "Ř" = "R", "ř" = "r", "Ě" = "E", "ě" = "e", "Ő" = "O", "ő" = "o",
           "Ð" = "D", "ð" = "d", "Þ" = "T", "þ" = "t", "Ğ" = "G", "ğ" = "g", "ı" = "i",
           "ā" = "a", "ē" = "e", "ī" = "i", "ū" = "u", "ķ" = "k", "ģ" = "g", "ļ" = "l", "ņ" = "n",
           "ė" = "e", "ų" = "u", "ą" = "a", "ę" = "e", "į" = "i", "ž" = "z", "’" = "'")
  tolower(chartr(paste(names(par), collapse = ""), paste(par, collapse = ""), x))
}

EFTERNAMN_OVERSTYR <- c(
  "Anne L'Huillier Wahlström" = "L'Huillier Wahlström, Anne",
  "Anne L’Huillier Wahlström" = "L'Huillier Wahlström, Anne",
  "Antonia Ax:son Johnson" = "Ax:son Johnson, Antonia",
  "Aníbal Cavaco Silva" = "Cavaco Silva, Aníbal",
  "Maria Cavaco Silva" = "Cavaco Silva, Maria",
  "Eduardo Frei Ruiz-Tagle" = "Frei Ruiz-Tagle, Eduardo",
  "Marie Göranzon Malmsjö" = "Göranzon Malmsjö, Marie",
  "Lee Myung-bak" = "Lee, Myung-bak",
  "Moon Jae-in" = "Moon, Jae-in",
  "Kwek Siew Jin" = "Kwek, Siew Jin",
  "Jaime Carvajal y Urquijo, 5:e markis av Isasi" = "Carvajal y Urquijo, Jaime",
  "George Leonard Carey, Baron Carey of Clifton, PC FRSA FKC" = "Carey, George Leonard",
  "Edmundo Pérez Yoma" = "Pérez Yoma, Edmundo",
  "Feliciano Barrios Pintado" = "Barrios Pintado, Feliciano",
  "Lorenzo Vignal Seelbach" = "Vignal Seelbach, Lorenzo",
  "Pedro Joaquín Coldwell" = "Joaquín Coldwell, Pedro",
  "Luis Francisco García Cerezo" = "García Cerezo, Luis Francisco",
  "Luigi Guidobono Cavalchini Garofoli" = "Guidobono Cavalchini Garofoli, Luigi",
  "Antonio Martins da Cruz" = "Martins da Cruz, Antonio",
  "Estela Barnes de Carlotto" = "Barnes de Carlotto, Estela",
  "François Polge de Combret" = "Polge de Combret, François",
  "Véronique Paulus de Châtelet" = "Paulus de Châtelet, Véronique",
  "Ingrid Le Roux" = "Le Roux, Ingrid",
  "Anders Fogh Rasmussen" = "Fogh Rasmussen, Anders",
  "Jørgen Ørstrøm Møller" = "Ørstrøm Møller, Jørgen",
  "Annette Koch Byrdal" = "Koch Byrdal, Annette",
  "Else Bugge Fougner" = "Bugge Fougner, Else",
  "Susan Weber Soros" = "Weber Soros, Susan"
)

PARTIKLAR <- c("von", "van", "de", "der", "den", "af", "la", "le", "el", "al", "di", "da",
               "dos", "du", "des", "del", "della", "ter", "zu", "vom")

efternamn_forst <- function(namn) {
  vapply(namn, function(n) {
    o <- EFTERNAMN_OVERSTYR[n]
    if (!is.na(o)) return(unname(o))
    fodd <- ""
    if (grepl(" f\\. ", n)) {
      fodd <- sub(" (DBE|CBE|OBE|KBE)$", "", sub("^.* f\\. ", "", n))
      n <- sub(" f\\. .*$", "", n)
    }
    t <- strsplit(trimws(n), "\\s+")[[1]]
    if (length(t) < 2) return(n)
    efter <- t[length(t)]
    fore <- t[-length(t)]
    part <- character()
    while (length(fore) && tolower(fore[length(fore)]) %in% PARTIKLAR) {
      part <- c(fore[length(fore)], part)
      fore <- fore[-length(fore)]
    }
    res <- paste0(efter, ", ", paste(c(fore, part), collapse = " "))
    if (nzchar(fodd)) res <- paste0(res, " (f. ", fodd, ")")
    res
  }, character(1), USE.NAMES = FALSE)
}

LANG_SV <- reactableLang(
  searchPlaceholder = "Sök",
  searchLabel = "Sök",
  noData = "Inga rader hittades",
  pageNext = "Nästa",
  pagePrevious = "Föregående",
  pageNumbers = "{page} av {pages}",
  pageInfo = "{rowStart}–{rowEnd} av {rows} rader",
  pageSizeOptions = "Visa {rows}",
  pageNextLabel = "Nästa sida",
  pagePreviousLabel = "Föregående sida",
  pageNumberLabel = "Sida {page}",
  pageJumpLabel = "Gå till sida",
  pageSizeOptionsLabel = "Rader per sida",
  sortLabel = "Sortera {name}",
  filterPlaceholder = "",
  filterLabel = "Filtrera {name}",
  groupExpandLabel = "Visa eller dölj grupp",
  detailsExpandLabel = "Visa eller dölj detaljer"
)

FLAGGA_TEXT <- c(
  kategori_utan_individuell_kalla = "Kategori (svensk/utländsk) är inte angiven för den enskilda personen i källan och kunde inte kontrolleras i Wikidata",
  kategori_fran_wikidata_p27 = "Kategori och land är hämtade från medborgarskap i Wikidata (P27); källan anger inte detta per person",
  motivering_formulering_skiljer = "Källorna formulerar motiveringen olika",
  beslutsdatum_ej_angivet = "Beslutsdatum saknas i källan; datumet är datumet för offentliggörandet",
  grad_ledamot_trots_kmo_riddare = "Graden är Ledamot, eftersom kvinnor hade benämningen Ledamot före reformen 1 februari 2023; Kungl. Maj:ts Ordens förläningsdatabas anger Riddare",
  datum_saknas_kungahuset_anger_grad = "Kungahuset.se anger graden Kommendör med stora korset men inget datum; Kungl. Maj:ts Ordens förläningsdatabas anger Kommendör av första klassen 2013-10-01 för samma orden",
  datum_fodelse_stadgar_25 = "Datum är födelsedatum: arvsberättigade prinsar och prinsessor är riddare från födseln enligt stadgarna 25 §",
  datum_vigsel_wikidata = "Datum är vigseldatum från Wikidata; källan anger inte datumet",
  datum_fodelse_kungahuset_visar_dop_1946_06_07 = "Datum är födelsedatum; kungahuset.se anger att insignierna överlämnades vid dopet 7 juni 1946",
  datum_avviker_fran_vigsel_eller_fodelse = "Datum från svensk Wikipedia (6 maj 1976) avviker från födelse- och vigseldatum",
  datum_fran_fodelsen_kalla = "Källan anger att graden gäller från födseln; datumet är födelsedatum",
  datum_saknas_sedan_tidigare = "Källan anger att hon sedan tidigare har graden (före 6 maj 2024); datum saknas",
  nationalitet_enligt_kmo = "Nationalitet enligt Kungl. Maj:ts Ordens förläningsdatabas; avviker från tidigare uppgift",
  nationalitet_kmo_avviker_fran_wikidata = "Kungl. Maj:ts Ordens förläningsdatabas anger Sverige, men personen är lettisk medborgare enligt Wikidata; land är korrigerat",
  fodelsear_avviker_ett_ar_kmo = "Födelseåret i Kungl. Maj:ts Ordens förläningsdatabas avviker ett år från Wikipedia; personen är identifierad via fullständigt namn, nationalitet och yrke",
  flera_medborgarskap = "Flera medborgarskap i Wikidata; land är inte angivet",
  land_wikidata_ej_verifierat = "Land i Wikidata är uppenbart felaktigt eller ej verifierat; land är inte angivet",
  grad_konflikt_kallor = "Källorna anger olika grad; den andra källan anges i flaggan i datafilen",
  datum_endast_phaleristica = "Årtalet kommer enbart från Phaleristicas lista över levande ordensinnehavare (2016)",
  kalla_oreferenserad_wikipedia = "Uppgiften saknar referens i svensk Wikipedia; årtalet stöds av Phaleristicas lista"
)

flagga_text <- function(x) {
  vapply(x, function(f) {
    if (is.na(f)) return("")
    delar <- strsplit(f, ";", fixed = TRUE)[[1]]
    txt <- unname(FLAGGA_TEXT[sub(":.*", "", delar)])
    txt[is.na(txt)] <- "Se personsidan"
    paste(txt, collapse = ". ")
  }, character(1), USE.NAMES = FALSE)
}

SLUG <- c(
  "Serafimerorden" = "serafimerorden",
  "Svärdsorden" = "svardsorden",
  "Nordstjärneorden" = "nordstjarneorden",
  "Vasaorden" = "vasaorden"
)

KALLTYP <- c(
  kungahuset = "Kungahuset.se",
  regeringen = "Regeringen.se",
  officiell_publikation = "Officiell publikation",
  tidning = "Tidning",
  wikipedia = "Wikipedia"
)

RENAME <- c(
  "id" = "id", "person_id" = "person_id", "namn" = "namn", "kön" = "kon",
  "kategori" = "kategori", "land" = "land", "kunglig" = "kunglig",
  "yrke_titel" = "yrke", "orden" = "orden", "grad" = "grad",
  "datum_tilldelning" = "datum", "datum_precision" = "precision",
  "motivering" = "motivering", "motivering_typ" = "motiv_typ",
  "statsbesök" = "statsbesok", "källa_url" = "kalla_url", "källtyp" = "kalltyp",
  "källa_hämtad" = "kalla_hamtad", "wikidata_qid" = "qid", "dödsdatum" = "dod",
  "flagga" = "flagga", "bild_url" = "bild_url", "bild_licens" = "bild_licens",
  "bild_upphov" = "bild_upphov"
)

load_tilldelningar <- function() {
  t <- fread(file.path(data_dir, "tilldelningar.csv"), encoding = "UTF-8",
             colClasses = "character", na.strings = "")
  setnames(t, names(t), unname(RENAME[names(t)]))
  g <- fread(file.path(data_dir, "grader.csv"), encoding = "UTF-8",
             colClasses = "character", na.strings = "")
  t[, rang := as.integer(g$rang[match(paste(orden, grad), paste(g$orden, g$grad))])]
  t[, kunglig_txt := fifelse(kunglig == "TRUE", "Ja", "Nej")]
  t[, kon_txt := fifelse(is.na(kon), "uppgift saknas", kon)]
  t[, land_txt := fifelse(is.na(land), "uppgift saknas", land)]
  t[, yrke_txt := fifelse(is.na(yrke), "uppgift saknas", yrke)]
  t[, motivering_txt := fifelse(is.na(motivering), "uppgift saknas", motivering)]
  t[, flagga_txt := flagga_text(flagga)]
  t[is.na(flagga_txt), flagga_txt := "Se personsidan"]
  t[, datum_txt := fifelse(is.na(datum), "uppgift saknas", datum)]
  t[is.na(dod)]
}

load_grader <- function() {
  g <- fread(file.path(data_dir, "grader.csv"), encoding = "UTF-8",
             colClasses = "character", na.strings = "")
  g[, rang := as.integer(rang)]
  g[, barordning_plats := as.integer(barordning_plats)]
  setorder(g, barordning_plats, orden, rang)
  g
}

load_logg <- function() {
  fread(file.path(data_dir, "andringslogg.csv"), encoding = "UTF-8",
        colClasses = "character", na.strings = "")
}

load_kmo_verifierade <- function() {
  f <- file.path(data_dir, "kmo_verifierade.csv")
  if (file.exists(f)) {
    fread(f, encoding = "UTF-8", colClasses = "character", na.strings = "")
  } else {
    data.table(id = character(), kmo_url = character())
  }
}

load_bilder <- function() {
  fread(file.path(data_dir, "bilder.csv"), encoding = "UTF-8",
        colClasses = "character", na.strings = "")
}

bild_kredit <- function(b) {
  lic <- if (is.na(b$licens_url)) b$licens else
    tags$a(href = b$licens_url, target = "_blank", rel = "noopener", b$licens)
  tags$span(class = "bildkredit", "Bild: ", b$upphov, ", ", lic, ", ",
            tags$a(href = b$commons_sida, target = "_blank", rel = "noopener", "Wikimedia Commons"))
}

bild_figur <- function(b, pfx = "") {
  tags$figure(class = "bild figure",
    tags$img(src = paste0(pfx, "img/", b$fil), alt = b$bildtext, loading = "lazy", class = "figure-img"),
    tags$figcaption(b$bildtext, tags$br(), bild_kredit(b)))
}

ordensbilder <- function(orden, pfx = "") {
  o <- orden
  bl <- load_bilder()[orden == o & anvandning == "ordenstecken"]
  tags$div(class = "bildgrid", lapply(seq_len(nrow(bl)), function(i) bild_figur(bl[i], pfx)))
}

ordenstecken_figur <- function(orden, pfx = "") {
  o <- orden
  bl <- load_bilder()[orden == o & anvandning == "ordenstecken"]
  if (!nrow(bl)) return(NULL)
  bild_figur(bl[1], pfx)
}

skold_allmanna <- function(pfx = "") {
  bl <- load_bilder()[anvandning == "sköld" & is.na(person_qid)]
  tags$div(class = "bildgrid", lapply(seq_len(nrow(bl)), function(i) bild_figur(bl[i], pfx)))
}

skold_kallor <- function(t) {
  bl <- load_bilder()[anvandning == "sköld" & !is.na(person_qid) & person_qid %in% t$qid]
  namn <- t$namn[match(bl$person_qid, t$qid)]
  tags$details(
    tags$summary("Bildkällor för vapensköldarna"),
    tags$ul(lapply(seq_len(nrow(bl)), function(i) tags$li(namn[i], ": ", bild_kredit(bl[i]))))
  )
}

link_person <- function(id, namn, pfx = "", html = FALSE) {
  sprintf('<a href="%sperson/%s.html">%s</a>', pfx, id, if (html) namn else htmlEscape(namn))
}

load_kungl_titlar <- function() {
  fread(file.path(data_dir, "kungliga_titlar.csv"), encoding = "UTF-8")
}

sc_span <- function(x) sprintf('<span class="efternamn">%s</span>', htmlEscape(x))

namn_sc_html <- function(namn) {
  x <- efternamn_forst(namn)
  vapply(seq_along(namn), function(i) {
    if (!grepl(", ", x[i], fixed = TRUE)) return(htmlEscape(namn[i]))
    efter <- sub(",.*$", "", x[i])
    rest <- sub("^[^,]*, ", "", x[i])
    suf <- if (grepl(" (f. ", rest, fixed = TRUE)) sub("^.*?( \\(f\\. .*\\))$", "\\1", rest, perl = TRUE) else ""
    fore <- sub(" \\(f\\. .*\\)$", "", rest)
    svans <- if (grepl(", ", namn[i], fixed = TRUE)) sub("^[^,]*", "", namn[i]) else ""
    paste0(htmlEscape(fore), " ", sc_span(efter), htmlEscape(suf), htmlEscape(svans))
  }, character(1))
}

kunglig_sc_html <- function(text, del) {
  vapply(seq_along(text), function(i) {
    for (a in strsplit(if (is.na(del[i])) "" else del[i], "|", fixed = TRUE)[[1]]) {
      p <- regexpr(a, text[i], fixed = TRUE)
      if (p > 0) return(paste0(htmlEscape(substr(text[i], 1, p - 1)), sc_span(a),
                               htmlEscape(substr(text[i], p + nchar(a), nchar(text[i])))))
    }
    htmlEscape(text[i])
  }, character(1))
}

namn_html_vec <- function(person_id, namn, kungl, bas = namn) {
  res <- character(length(namn))
  n <- which(!kungl)
  r <- which(kungl)
  if (length(n)) res[n] <- namn_sc_html(namn[n])
  if (length(r)) {
    tt <- load_kungl_titlar()
    res[r] <- kunglig_sc_html(bas[r], tt$namn_del[match(person_id[r], tt$person_id)])
  }
  res
}

source_html <- function(url, typ, namn = NULL) {
  label <- KALLTYP[typ]
  label[is.na(label)] <- ""
  aria <- if (is.null(namn)) "" else sprintf(' aria-label="Källa för %s"', htmlEscape(namn))
  sprintf('<a href="%s" target="_blank" rel="noopener"%s>Källa</a> <span class="kalltyp">%s</span>',
          htmlEscape(url), aria, htmlEscape(label))
}

order_table <- function(t, orden, kat, pfx = "", tid, kungl = FALSE) {
  o <- orden
  k <- kat
  d <- t[orden == o & kategori == k & (kunglig == "TRUE") == kungl & grad != "Herre och mästare"]
  setorderv(d, "rang")
  d <- d[!duplicated(person_id)]
  d[, sortnamn := sortnyckel(if (kungl) namn else efternamn_forst(namn))]
  if (kungl) {
    titlar <- load_kungl_titlar()
    d[, visningsnamn := fifelse(person_id %in% titlar$person_id, titlar$titel_lang[match(person_id, titlar$person_id)], namn)]
  } else {
    d[, visningsnamn := efternamn_forst(namn)]
  }
  d[, datumnyckel := fifelse(grepl("^[0-9]{4}", datum), datum, "9999")]
  setorderv(d, c("rang", "datumnyckel", "sortnamn"))
  d[, namn_sort := visningsnamn]
  d[, namn_html := link_person(person_id, namn_html_vec(person_id, namn, rep(kungl, .N), bas = visningsnamn), pfx, html = TRUE)]
  d[, kalla_html := source_html(kalla_url, kalltyp, namn)]
  d[, skold_html := fifelse(!is.na(bild_url),
    sprintf('<a href="%sperson/%s.html"><img class="skold" src="%s%s" alt="Vapensköld för %s" loading="lazy"></a>',
            pfx, person_id, pfx, bild_url, htmlEscape(namn)), "–")]
  cols <- list(
    skold_html = colDef(name = "Vapensköld", html = TRUE, minWidth = 90, sortable = FALSE, align = "center", headerStyle = list(textAlign = "center")),
    grad = colDef(name = "Grad", minWidth = 120),
    namn_sort = colDef(name = "Namn", html = TRUE, minWidth = 150,
                       cell = JS("function(cellInfo) { return cellInfo.row.namn_html }")),
    namn_html = colDef(show = FALSE),
    yrke_txt = colDef(name = "Yrke/titel", minWidth = 130),
    land_txt = colDef(name = "Land", minWidth = 100),
    datum_txt = colDef(name = "Datum", minWidth = 120, cell = fmt_datum),
    motivering_txt = colDef(name = "Motivering", minWidth = 220),
    kalla_html = colDef(name = "Källa", html = TRUE, minWidth = 120),
    flagga_txt = colDef(name = "Notering", minWidth = 200, class = "saknas")
  )
  keep <- c("grad", "namn_sort", "namn_html")
  d[, skold_html := fifelse(!is.na(bild_url) & grepl("^img/skold-", bild_url), skold_html, "–")]
  if ((o == "Serafimerorden" || (kungl && kat == "svensk")) && any(d$skold_html != "–")) keep <- c(keep, "skold_html")
  if (!kungl) keep <- c(keep, "yrke_txt")
  if (kat == "utländsk") keep <- c(keep, "land_txt")
  keep <- c(keep, "datum_txt")
  rt <- reactable(
    as.data.frame(d)[, keep],
    groupBy = "grad",
    columns = cols[keep],
    searchable = TRUE,
    filterable = FALSE,
    sortable = TRUE,
    defaultPageSize = 50,
    elementId = tid,
    striped = TRUE,
    compact = TRUE,
    defaultExpanded = TRUE,
    language = LANG_SV
  )
  synliga <- setdiff(keep, "namn_html")
  labels <- vapply(cols[synliga], function(cd) cd$name, character(1))
  celler <- function(i) lapply(synliga, function(col) {
    v <- d[[col]][i]
    tags$td(switch(col,
      namn_sort = HTML(d$namn_html[i]),
      kalla_html = , skold_html = HTML(v),
      datum_txt = fmt_datum(v),
      v))
  })
  statisk <- tags$noscript(tags$table(
    class = "person-tabell",
    tags$thead(tags$tr(lapply(unname(labels), tags$th))),
    tags$tbody(lapply(seq_len(nrow(d)), function(i) tags$tr(celler(i))))
  ))
  tagList(rt, statisk)
}

order_section <- function(orden, t, pfx = "") {
  slug <- SLUG[[orden]]
  o <- orden
  finns <- function(kat, kungl) {
    nrow(t[orden == o & kategori == kat & (kunglig == "TRUE") == kungl & grad != "Herre och mästare"]) > 0
  }
  avsnitt <- function(rubrik, kat, kungl, suffix) {
    if (!finns(kat, kungl)) return(NULL)
    tagList(h3(rubrik), order_table(t, orden, kat, pfx, paste0("tbl-", slug, "-", suffix), kungl = kungl))
  }
  tagList(
    avsnitt("Svenska kungligheter", "svensk", TRUE, "svensk-kunglig"),
    avsnitt("Utländska kungligheter", "utländsk", TRUE, "utlandsk-kunglig"),
    avsnitt("Svenska mottagare", "svensk", FALSE, "svensk"),
    avsnitt("Utländska mottagare", "utländsk", FALSE, "utlandsk")
  )
}

person_page <- function(t, id, pfx = "../") {
  pid <- id
  d <- t[person_id == pid]
  d[, orden_nr := match(orden, ORDNAR)]
  setorderv(d, c("orden_nr", "rang"))
  first <- d[1]
  ids <- d$id
  kv <- load_kmo_verifierade()[id %in% ids]
  kmo_cell <- function(rid) {
    x <- kv[id == rid]
    if (nrow(x)) {
      tags$a(href = paste0("https://kungligmajestatsorden.se", sub("^https://kungligmajestatsorden.se", "", x$kmo_url[1])),
             target = "_blank", rel = "noopener", "✓ Verifierad")
    } else {
      "–"
    }
  }
  rows <- lapply(seq_len(nrow(d)), function(i) {
    r <- d[i]
    tags$tr(
      tags$td(r$orden),
      tags$td(r$grad),
      tags$td(r$yrke_txt),
      tags$td(fmt_datum(r$datum_txt)),
      tags$td(r$motivering_txt),
      tags$td(tags$a(href = r$kalla_url, target = "_blank", rel = "noopener", "Källa"),
              " ", tags$span(class = "kalltyp", KALLTYP[r$kalltyp])),
      tags$td(if (!is.na(r$flagga)) tags$span(class = "saknas", flagga_text(r$flagga)) else ""),
      tags$td(kmo_cell(r$id))
    )
  })
  verifierad <- if (nrow(kv)) {
    tags$p(class = "verifierad", "✓ ", tags$strong("Verifierad mot Kungl. Maj:ts Ordens förläningsdatabas"),
           paste0(" (", nrow(kv), " av ", nrow(d), if (nrow(d) == 1) " tilldelning)" else " tilldelningar)"))
  }
  med_bild <- d[!is.na(bild_url)]
  bild <- if (nrow(med_bild)) {
    b <- load_bilder()[fil == basename(med_bild$bild_url[1])]
    if (nrow(b)) bild_figur(b[1], pfx)
  }
  tagList(
    tags$p(tags$strong("Kön: "), first$kon_txt,
           tags$br(), tags$strong("Kategori: "), first$kategori,
           tags$br(), tags$strong("Land: "), first$land_txt,
           tags$br(), tags$strong("Kunglig: "), first$kunglig_txt,
           if (!is.na(first$qid)) tagList(tags$br(), tags$strong("Wikidata: "),
             tags$a(href = paste0("https://www.wikidata.org/wiki/", first$qid), first$qid))),
    verifierad,
    bild,
    tags$table(class = "person-tabell table table-sm table-striped small caption-top",
      tags$thead(tags$tr(class = "header", lapply(
        c("Orden", "Grad", "Yrke/titel", "Datum", "Motivering", "Källa", "Notering", "Kungl. Maj:ts Orden"),
        function(h) tags$th(`data-quarto-table-cell-role` = "th", h)))),
      tags$tbody(rows)),
    tags$p(class = "redigera-lank", tags$a(href = paste0(pfx, "redigera.html#person=", pid), "Redigera personen"))
  )
}
