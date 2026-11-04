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
  m <- MANADER[as.integer(substr(x, 6, 7))]
  d <- as.integer(substr(x, 9, 10))
  out <- fifelse(nchar(x) == 4, y, fifelse(nchar(x) == 7, paste(m, y), paste(d, m, y)))
  fifelse(ok, out, x)
}

sortnyckel <- function(x) tolower(chartr("ÅÄÖåäö", "{|}{|}", x))

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
  datum_fodelse_stadgar_25 = "Datum är födelsedatum: arvsberättigade prinsar och prinsessor är riddare från födseln enligt stadgarna 25 §",
  datum_vigsel_wikidata = "Datum är vigseldatum från Wikidata; källan anger inte datumet",
  datum_fodelse_kungahuset_visar_dop_1946_06_07 = "Datum är födelsedatum; kungahuset.se anger att insignierna överlämnades vid dopet 7 juni 1946",
  datum_avviker_fran_vigsel_eller_fodelse = "Datum från svensk Wikipedia (6 maj 1976) avviker från födelse- och vigseldatum",
  datum_fran_fodelsen_kalla = "Källan anger att graden gäller från födseln; datumet är födelsedatum",
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
  tags$figure(class = "bild",
    tags$img(src = paste0(pfx, "img/", b$fil), alt = b$bildtext, loading = "lazy"),
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

link_person <- function(id, namn, pfx = "") {
  sprintf('<a href="%sperson/%s.html">%s</a>', pfx, id, htmlEscape(namn))
}

source_html <- function(url, typ, namn = NULL) {
  label <- KALLTYP[typ]
  label[is.na(label)] <- ""
  aria <- if (is.null(namn)) "" else sprintf(' aria-label="Källa för %s"', htmlEscape(namn))
  sprintf('<a href="%s" target="_blank" rel="noopener"%s>Källa</a> <span class="kalltyp">%s</span>',
          htmlEscape(url), aria, htmlEscape(label))
}

select_filter <- function(tid) {
  function(values, name) {
    tags$select(
      onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value)", tid, name),
      `aria-label` = "Filtrera på kunglig",
      tags$option(value = "", "Alla"),
      tags$option(value = "Ja", "Ja"),
      tags$option(value = "Nej", "Nej")
    )
  }
}

order_table <- function(t, orden, kat, pfx = "", tid) {
  o <- orden
  k <- kat
  d <- t[orden == o & kategori == k & grad != "Herre och mästare"]
  d[, sortnamn := sortnyckel(namn)]
  setorderv(d, c("rang", "sortnamn"))
  d[, namn_html := link_person(person_id, namn, pfx)]
  d[, kalla_html := source_html(kalla_url, kalltyp, namn)]
  d[, skold_html := fifelse(!is.na(bild_url),
    sprintf('<a href="%sperson/%s.html"><img class="skold" src="%s%s" alt="Vapensköld för %s" loading="lazy"></a>',
            pfx, person_id, pfx, bild_url, htmlEscape(namn)), "–")]
  cols <- list(
    skold_html = colDef(name = "Vapensköld", html = TRUE, minWidth = 90, sortable = FALSE),
    grad = colDef(name = "Grad", minWidth = 120),
    namn_html = colDef(name = "Namn", html = TRUE, minWidth = 150),
    yrke_txt = colDef(name = "Yrke/titel", minWidth = 130),
    land_txt = colDef(name = "Land", minWidth = 100),
    datum_txt = colDef(name = "Datum", minWidth = 120, cell = fmt_datum),
    motivering_txt = colDef(name = "Motivering", minWidth = 220),
    kalla_html = colDef(name = "Källa", html = TRUE, minWidth = 120),
    kunglig_txt = colDef(name = "Kunglig", filterable = TRUE, filterInput = select_filter(tid), minWidth = 100),
    flagga_txt = colDef(name = "Notering", minWidth = 200, class = "saknas")
  )
  keep <- c("grad", "namn_html")
  if (o == "Serafimerorden" && any(!is.na(d$bild_url))) keep <- c(keep, "skold_html")
  keep <- c(keep, "yrke_txt")
  if (kat == "utländsk") keep <- c(keep, "land_txt")
  keep <- c(keep, "datum_txt", "motivering_txt", "kalla_html")
  if (any(d$kunglig_txt == "Ja")) keep <- c(keep, "kunglig_txt")
  if (any(nzchar(d$flagga_txt))) keep <- c(keep, "flagga_txt")
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
  labels <- vapply(cols[keep], function(cd) cd$name, character(1))
  celler <- function(i) lapply(keep, function(col) {
    v <- d[[col]][i]
    tags$td(switch(col,
      namn_html = , kalla_html = , skold_html = HTML(v),
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
  tagList(
    h3("Svenska mottagare"),
    order_table(t, orden, "svensk", pfx, paste0("tbl-", slug, "-svensk")),
    h3("Utländska mottagare"),
    order_table(t, orden, "utländsk", pfx, paste0("tbl-", slug, "-utlandsk"))
  )
}

person_page <- function(t, id, pfx = "../") {
  pid <- id
  d <- t[person_id == pid]
  d[, orden_nr := match(orden, ORDNAR)]
  setorderv(d, c("orden_nr", "rang"))
  first <- d[1]
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
      tags$td(if (!is.na(r$flagga)) tags$span(class = "saknas", flagga_text(r$flagga)) else "")
    )
  })
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
    bild,
    tags$table(class = "person-tabell",
      tags$thead(tags$tr(tags$th("Orden"), tags$th("Grad"), tags$th("Yrke/titel"), tags$th("Datum"),
                         tags$th("Motivering"), tags$th("Källa"), tags$th("Notering"))),
      tags$tbody(rows))
  )
}
