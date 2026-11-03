suppressMessages({
  library(data.table)
  library(htmltools)
  library(reactable)
})

data_dir <- if (dir.exists("data")) "data" else "../data"

ORDNAR <- c("Serafimerorden", "Svärdsorden", "Nordstjärneorden", "Vasaorden")

FLAGGA_TEXT <- c(
  kategori_utan_individuell_kalla = "Kategori (svensk/utländsk) är inte angiven för den enskilda personen i källan",
  motivering_formulering_skiljer = "Källorna formulerar motiveringen olika",
  beslutsdatum_ej_angivet = "Beslutsdatum saknas i källan; datumet är datumet för offentliggörandet"
)

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
  t[, yrke_txt := fifelse(is.na(yrke), "", yrke)]
  t[, motivering_txt := fifelse(is.na(motivering), "", motivering)]
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

link_person <- function(id, namn, pfx = "") {
  sprintf('<a href="%sperson/%s.html">%s</a>', pfx, id, htmlEscape(namn))
}

source_html <- function(url, typ) {
  label <- KALLTYP[typ]
  label[is.na(label)] <- ""
  sprintf('<a href="%s" target="_blank" rel="noopener">Källa</a> <span class="kalltyp">%s</span>',
          htmlEscape(url), htmlEscape(label))
}

select_filter <- function(tid) {
  function(values, name) {
    tags$select(
      onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value)", tid, name),
      `aria-label` = "Filtrera på kungligheter",
      tags$option(value = "", "Alla"),
      tags$option(value = "Ja", "Ja"),
      tags$option(value = "Nej", "Nej")
    )
  }
}

order_table <- function(t, orden, kat, pfx = "", tid) {
  o <- orden
  k <- kat
  d <- t[orden == o & kategori == k]
  setorder(d, rang, namn)
  d[, namn_html := link_person(person_id, namn, pfx)]
  d[, kalla_html := source_html(kalla_url, kalltyp)]
  cols <- list(
    grad = colDef(name = "Grad"),
    namn_html = colDef(name = "Namn", html = TRUE, minWidth = 160),
    yrke_txt = colDef(name = "Yrke/titel", minWidth = 140),
    datum_txt = colDef(name = "Datum", minWidth = 110),
    motivering_txt = colDef(name = "Motivering", minWidth = 280),
    kalla_html = colDef(name = "Källa", html = TRUE, minWidth = 150),
    kunglig_txt = colDef(name = "Kungligt", filterable = TRUE, filterInput = select_filter(tid), minWidth = 110)
  )
  keep <- c("grad", "namn_html", "yrke_txt")
  if (kat == "utländsk") {
    keep <- c(keep, "land_txt")
    cols$land_txt <- colDef(name = "Land", minWidth = 120)
  }
  keep <- c(keep, "datum_txt", "motivering_txt", "kalla_html", "kunglig_txt")
  reactable(
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
    defaultExpanded = TRUE
  )
}

order_section <- function(orden, t, pfx = "") {
  slug <- SLUG[[orden]]
  tagList(
    h2("Svenska mottagare"),
    order_table(t, orden, "svensk", pfx, paste0("tbl-", slug, "-svensk")),
    h2("Utländska mottagare"),
    order_table(t, orden, "utländsk", pfx, paste0("tbl-", slug, "-utlandsk"))
  )
}

person_page <- function(t, id, pfx = "../") {
  pid <- id
  d <- t[person_id == pid]
  setorder(d, rang)
  first <- d[1]
  rows <- lapply(seq_len(nrow(d)), function(i) {
    r <- d[i]
    tags$tr(
      tags$td(r$orden),
      tags$td(r$grad),
      tags$td(r$datum_txt),
      tags$td(r$motivering_txt),
      tags$td(tags$a(href = r$kalla_url, target = "_blank", rel = "noopener", "Källa"),
              " ", tags$span(class = "kalltyp", KALLTYP[r$kalltyp])),
      tags$td(if (!is.na(r$flagga)) tags$span(class = "saknas", FLAGGA_TEXT[sub(":.*", "", r$flagga)]) else "")
    )
  })
  bild <- if (!is.na(first$bild_url)) {
    tags$figure(
      tags$img(src = first$bild_url, alt = first$namn, style = "max-width:240px"),
      tags$figcaption(paste(first$bild_licens, first$bild_upphov))
    )
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
      tags$thead(tags$tr(tags$th("Orden"), tags$th("Grad"), tags$th("Datum"),
                         tags$th("Motivering"), tags$th("Källa"), tags$th("Notering"))),
      tags$tbody(rows))
  )
}
