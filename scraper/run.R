suppressMessages({
  library(data.table)
  library(httr2)
  library(rvest)
  library(xml2)
  library(jsonlite)
})

UA <- "kungliga-ordnar-research/0.1 (generic client)"
CACHE_DIR <- "cache/scraper"
FIND_DAYS <- 120
MIN_DELAY <- 2.1
IDAG <- format(Sys.time(), "%Y-%m-%d", tz = "UTC")
FILE_T <- "data/tilldelningar.csv"
FILE_K <- "data/kandidater.csv"
FILE_L <- "data/andringslogg.csv"
FILE_G <- "data/grader.csv"

KOLUMNER <- c("id", "person_id", "namn", "kön", "kategori", "land", "kunglig",
              "yrke_titel", "orden", "grad", "datum_tilldelning", "datum_precision",
              "motivering", "motivering_typ", "statsbesök", "källa_url", "källtyp",
              "källa_hämtad", "wikidata_qid", "dödsdatum", "flagga", "bild_url",
              "bild_licens", "bild_upphov")

ORDEN_STAMM <- c(Serafimerorden = "Serafimer", Svärdsorden = "Svärds",
                 Nordstjärneorden = "Nordstjärne", Vasaorden = "Vasa")
ORDEN_RE <- "(Serafimer|Svärds|Nordstjärne|Vasa)orden"
GRAD_RE <- paste0("^(Kommendör(?:er)?|Riddare)",
                  "(?: med stora korset(?: av (?:första|andra) klassen)?",
                  "| av (?:första|andra) klassen)?",
                  "(?: av (Serafimer|Svärds|Nordstjärne|Vasa)orden)?$")
MANADER <- c("januari", "februari", "mars", "april", "maj", "juni", "juli",
             "augusti", "september", "oktober", "november", "december")
TITELORD <- c("direktör", "professor", "överste", "överstelöjtnant", "major",
              "kapten", "löjtnant", "generallöjtnant", "general", "generalmajor",
              "brigadgeneral", "amiral", "viceamiral", "konteramiral", "ambassadör",
              "statsminister", "utrikesminister", "försvarsminister", "minister",
              "president", "hovsångerska", "hovsångare", "skådespelare",
              "skådespelerska", "journalist", "läkare", "vicekorpral", "korpral",
              "sergeant", "fotograf", "affärsutvecklare", "socionom", "docent",
              "landshövding", "rektor", "överbefälhavare", "generalkonsul",
              "honorärkonsul", "konsul", "tidigare", "f.d.", "tredje", "förste",
              "andre", "teknologie", "nationell", "strategisk", "utrikesråd",
              "chef", "ordförande", "kommendörkapten", "fänrik", "militär",
              "försvarsattaché", "riksdagsledamot", "artisten", "artist", "musiker",
              "tonsättare", "författare", "konstnär", "regissör", "forskare", "vd",
              "rådgivare", "medicinsk", "affärsområdeschef", "operachef", "justitieråd",
              "riksarkivarie", "länsantikvarie", "teknologie", "hedersdoktor")

skipped <- character()
last_call <- new.env()
robots_cache <- new.env()

notera_hoppat <- function(varde, skal) {
  skipped <<- c(skipped, sprintf("%s: %s", varde, skal))
}

host_av <- function(url) sub("^https?://([^/]+).*$", "\\1", url)

vanta <- function(host) {
  senast <- if (exists(host, envir = last_call)) get(host, envir = last_call) else 0
  rest <- MIN_DELAY - (as.numeric(Sys.time()) - senast)
  if (rest > 0) Sys.sleep(rest)
  assign(host, as.numeric(Sys.time()), envir = last_call)
}

robots_regler <- function(host) {
  if (exists(host, envir = robots_cache)) return(get(host, envir = robots_cache))
  vanta(host)
  svar <- tryCatch(
    request(paste0("https://", host, "/robots.txt")) |>
      req_user_agent(UA) |> req_timeout(30) |>
      req_error(is_error = function(r) FALSE) |> req_perform(),
    error = function(e) NULL)
  regler <- character()
  if (!is.null(svar) && resp_status(svar) == 200) {
    rader <- trimws(strsplit(resp_body_string(svar), "\n")[[1]])
    i_star <- FALSE
    for (r in rader) {
      if (grepl("^user-agent:", r, ignore.case = TRUE)) {
        i_star <- trimws(sub("^user-agent:", "", r, ignore.case = TRUE)) == "*"
      } else if (i_star && grepl("^disallow:", r, ignore.case = TRUE)) {
        v <- trimws(sub("^disallow:", "", r, ignore.case = TRUE))
        if (nzchar(v)) regler <- c(regler, v)
      }
    }
  }
  assign(host, regler, envir = robots_cache)
  regler
}

tillat <- function(url) {
  host <- host_av(url)
  sokvag <- sub("^https?://[^/]+", "", url)
  for (r in robots_regler(host)) {
    mono <- paste0("^", gsub("\\*", ".*", gsub("([.?+()\\[\\]{}|^])", "\\\\\\1", sub("\\$$", "", r), perl = TRUE)))
    if (grepl("\\$$", r)) mono <- paste0(mono, "$")
    if (grepl(mono, sokvag)) return(FALSE)
  }
  TRUE
}

cache_fil <- function(url) {
  dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
  file.path(CACHE_DIR, paste0(substr(gsub("[^A-Za-z0-9]+", "_", url), 1, 180), ".html"))
}

hamta <- function(url, max_alder_h = Inf) {
  f <- cache_fil(url)
  if (file.exists(f) && difftime(Sys.time(), file.mtime(f), units = "hours") < max_alder_h) {
    return(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  }
  if (!tillat(url)) {
    notera_hoppat(url, "spärrad i robots.txt")
    return(NULL)
  }
  senaste_status <- NA_integer_
  for (forsok in 1:3) {
    vanta(host_av(url))
    svar <- tryCatch(
      request(url) |> req_user_agent(UA) |> req_timeout(40) |>
        req_error(is_error = function(r) FALSE) |> req_perform(),
      error = function(e) NULL)
    if (!is.null(svar)) {
      senaste_status <- resp_status(svar)
      if (senaste_status == 200) {
        kropp <- resp_body_string(svar)
        if (grepl("enable javascript", kropp, ignore.case = TRUE) && nchar(kropp) < 4000) {
          notera_hoppat(url, "kräver JavaScript")
          return(NULL)
        }
        writeLines(kropp, f, useBytes = TRUE)
        return(kropp)
      }
      if (senaste_status == 404) {
        notera_hoppat(url, "HTTP 404")
        return(NULL)
      }
    }
    Sys.sleep(5 * forsok)
  }
  notera_hoppat(url, sprintf("blockerad eller ej nåbar efter 3 försök (HTTP %s)", senaste_status))
  NULL
}

sidlinjer <- function(html) {
  html <- gsub("<br\\s*/?>", "\n", html, ignore.case = TRUE)
  html <- gsub("</(p|h[1-6]|li|div|tr|td)>", "\n</\\1>", html, ignore.case = TRUE)
  h <- read_html(html)
  m <- html_element(h, "main")
  if (inherits(m, "xml_missing")) m <- html_element(h, "body")
  txt <- html_text(m)
  l <- trimws(gsub(" ", " ", strsplit(txt, "\n")[[1]]))
  l[nzchar(l)]
}

sidans_titel <- function(html) {
  h <- read_html(html)
  t <- html_element(h, "h1")
  if (inherits(t, "xml_missing")) "" else trimws(html_text(t))
}

svenskt_datum <- function(x) {
  m <- regmatches(x, regexpr(paste0("[0-9]{1,2} (", paste(MANADER, collapse = "|"), ") [0-9]{4}"), x))
  if (!length(m)) return(NA_character_)
  d <- strsplit(m, " ")[[1]]
  sprintf("%s-%02d-%02d", d[3], match(d[2], MANADER), as.integer(d[1]))
}

sidans_datum <- function(url, lines) {
  s <- regmatches(url, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}(?=-)", url, perl = TRUE))
  if (length(s)) return(s)
  svenskt_datum(paste(head(lines, 40), collapse = " "))
}

kalltyp_av <- function(url) {
  h <- host_av(url)
  if (grepl("kungahuset\\.se$", h)) "kungahuset"
  else if (grepl("regeringen\\.se$", h)) "regeringen"
  else if (grepl("kungligmajestatsorden\\.se$", h)) "officiell_publikation"
  else "tidning"
}

orden_av_text <- function(x) {
  m <- regmatches(x, regexpr(ORDEN_RE, x))
  if (!length(m)) return(NA_character_)
  names(ORDEN_STAMM)[match(sub("orden$", "", m), ORDEN_STAMM)]
}

grad_normaliserad <- function(x) {
  x <- sub(" av (Serafimer|Svärds|Nordstjärne|Vasa)orden$", "", x)
  x <- sub("^Kommendörer", "Kommendör", x)
  x
}

dela_namn <- function(rad) {
  t <- strsplit(trimws(rad), "\\s+")[[1]]
  n <- length(t)
  if (n <= 1) return(c(titel = "", namn = rad))
  i <- 1
  while (i < n && (tolower(t[i]) %in% TITELORD || grepl("^[a-zåäö0-9]", t[i]))) i <- i + 1
  c(titel = if (i > 1) paste(t[seq_len(i - 1)], collapse = " ") else "",
    namn = paste(t[i:n], collapse = " "))
}

namnlista <- function(rad) {
  delar <- trimws(unlist(strsplit(rad, ",\\s*| och ")))
  delar[nzchar(delar)]
}

rad_tom <- function() {
  as.data.table(setNames(rep(list(character()), length(KOLUMNER)), KOLUMNER))
}

ny_rad <- function(namn, titel, orden, grad, datum, motivering, url, kategori, flagga = NA_character_) {
  prec <- c("4" = "år", "7" = "månad", "10" = "dag")[as.character(nchar(datum))]
  data.table(id = NA_character_, person_id = slug(namn), namn = namn, kön = NA_character_,
             kategori = kategori, land = NA_character_, kunglig = "FALSE",
             yrke_titel = if (nzchar(titel)) titel else NA_character_, orden = orden,
             grad = grad, datum_tilldelning = datum, datum_precision = unname(prec),
             motivering = if (!is.na(motivering) && nzchar(motivering)) motivering else NA_character_,
             motivering_typ = if (!is.na(motivering) && nzchar(motivering)) "officiell" else NA_character_,
             statsbesök = NA_character_, källa_url = url, källtyp = kalltyp_av(url),
             källa_hämtad = IDAG, wikidata_qid = NA_character_, dödsdatum = NA_character_,
             flagga = flagga, bild_url = NA_character_, bild_licens = NA_character_,
             bild_upphov = NA_character_)
}

# Slug rules are shared with slugify() in redigera.js; tests/slug-fall.csv holds the fixed cases both must satisfy.
SLUG_TECKEN <- local({
  grupper <- c(a = "àáâãäåāăąǎǻȧ", c = "çćčĉċ", d = "ďđð", e = "èéêëēĕėęěȩ", g = "ğĝġģ", h = "ĥħ",
               i = "ìíîïĩīĭįǐı", j = "ĵ", k = "ķ", l = "ĺļľł", n = "ñńņň", o = "òóôõöøōŏőǒ",
               r = "ŕŗř", s = "śŝşšș", t = "ţťŧț", u = "ùúûüũūŭůűųǔ", w = "ŵ", y = "ýÿŷ", z = "źżž")
  list(fran = paste(unlist(strsplit(unname(grupper), "")), collapse = ""),
       till = paste(rep(names(grupper), nchar(unname(grupper))), collapse = ""))
})

slug <- function(x) {
  x <- tolower(gsub("['’‘ʼ:]", "", x))
  for (e in list(c("æ", "ae"), c("œ", "oe"), c("ß", "ss"), c("þ", "th"))) x <- gsub(e[1], e[2], x, fixed = TRUE)
  x <- chartr(SLUG_TECKEN$fran, SLUG_TECKEN$till, x)
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("^-|-$", "", x)
}

namnnyckel <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9 ]", "", iconv(chartr("åäöÅÄÖ", "aaoAAO", x), to = "ASCII//TRANSLIT")))
  gsub("\\s+", " ", trimws(x))
}

kategori_fran_sida <- function(lines) {
  txt <- paste(lines, collapse = " ")
  if (grepl("svensk(a|ar|e)?.*utländsk|utländsk.*svensk(a|ar|e)?", txt, ignore.case = TRUE)) return(list(kategori = NA_character_, flagga = NA_character_))
  if (grepl("utländska medborgare", txt)) return(list(kategori = "utländsk", flagga = "kategori_utan_individuell_kalla"))
  if (grepl("svenskar eller i Sverige bosatta", txt)) return(list(kategori = "svensk", flagga = "kategori_utan_individuell_kalla"))
  list(kategori = NA_character_, flagga = NA_character_)
}

ar_gradrad <- function(l) grepl(GRAD_RE, l, perl = TRUE)

ar_namnrad <- function(l) {
  nchar(l) <= 100 && !grepl("[.:]$", l) && !grepl("^(För |Kungl\\.|Kungliga|Illustrationer|Foto)", l) &&
    !ar_gradrad(l) && !grepl(ORDEN_RE, l) && grepl("^[A-ZÅÄÖ]", l)
}

tolka_strukturerad <- function(url, lines) {
  datum <- sidans_datum(url, lines)
  kat <- kategori_fran_sida(lines)
  rader <- list()
  cur_orden <- NA_character_
  grad_b <- NA_character_
  n <- length(lines)
  orden_av_grad <- function(g) {
    m <- regmatches(g, regexec(GRAD_RE, g, perl = TRUE))[[1]]
    if (length(m) && nzchar(m[3])) names(ORDEN_STAMM)[match(m[3], ORDEN_STAMM)] else cur_orden
  }
  for (i in seq_len(n)) {
    l <- lines[i]
    if (nchar(l) < 90 && grepl(paste0("^(Kungl\\. |Kungliga |den )?", ORDEN_RE, ":?$"), l)) {
      cur_orden <- orden_av_text(l)
      grad_b <- NA_character_
      next
    }
    if (grepl("tilldelas (den )?(Kungliga|Kungl\\.) ", l) && nchar(l) < 120) {
      o <- orden_av_text(l)
      if (!is.na(o)) cur_orden <- o
      next
    }
    if (nchar(l) < 60 && grepl("medalj|tecknet?$|militärkors", l, ignore.case = TRUE)) {
      grad_b <- NA_character_
      next
    }
    if (ar_gradrad(l)) {
      if (i < n && !grepl("^För ", lines[i + 1])) grad_b <- l
      next
    }
    if (!grepl("^(För |Med hänsyn)", l) || i < 3) next
    if (ar_gradrad(lines[i - 1])) {
      namnrad <- lines[i - 2]
      grad <- lines[i - 1]
    } else {
      namnrad <- lines[i - 1]
      grad <- grad_b
    }
    if (is.na(grad) || !ar_namnrad(namnrad)) next
    orden <- orden_av_grad(grad)
    if (is.na(orden)) next
    for (nm in namnlista(namnrad)) {
      d <- dela_namn(nm)
      rader[[length(rader) + 1]] <- ny_rad(d[["namn"]], d[["titel"]], orden, grad_normaliserad(grad),
                                           datum, l, url, kat$kategori, kat$flagga)
    }
  }
  if (length(rader)) rbindlist(rader) else rad_tom()
}

LAND_ADJ <- c(svenska = "Sverige", svenske = "Sverige", danska = "Danmark", danske = "Danmark",
              norska = "Norge", norske = "Norge", finske = "Finland",
              finska = "Finland", isländske = "Island", isländska = "Island", estniske = "Estland",
              estniska = "Estland", lettiske = "Lettland", lettiska = "Lettland", litauiske = "Litauen",
              litauiska = "Litauen", polske = "Polen", polska = "Polen", tyske = "Tyskland",
              tyska = "Tyskland", franske = "Frankrike", franska = "Frankrike", italienske = "Italien",
              italienska = "Italien", grekiske = "Grekland", grekiska = "Grekland", spanske = "Spanien",
              spanska = "Spanien", brittiske = "Storbritannien", brittiska = "Storbritannien",
              amerikanske = "USA", amerikanska = "USA", kanadensiske = "Kanada", kanadensiska = "Kanada",
              indiske = "Indien", indiska = "Indien", japanske = "Japan", japanska = "Japan",
              australiensiske = "Australien", australiensiska = "Australien", nederländske = "Nederländerna",
              nederländska = "Nederländerna", belgiske = "Belgien", belgiska = "Belgien",
              schweiziske = "Schweiz", schweiziska = "Schweiz", österrikiske = "Österrike",
              österrikiska = "Österrike", ukrainske = "Ukraina", ukrainska = "Ukraina")

tolka_kmo <- function(url, lines) {
  datum <- sidans_datum(url, lines)
  rader <- list()
  cur_orden <- NA_character_
  cur_grad <- NA_character_
  ctx <- paste0("till (Kommendör med stora korset|Kommendör av första klassen|Kommendör|",
                "Riddare av första klassen|Riddare) av (Svärds|Svärs|Nordstjärne|Vasa|Serafimer)orden")
  for (l in lines) {
    m <- regmatches(l, regexec(ctx, l, perl = TRUE))[[1]]
    if (length(m)) {
      cur_grad <- m[2]
      stam <- sub("^Svärs$", "Svärds", m[3])
      cur_orden <- names(ORDEN_STAMM)[match(stam, ORDEN_STAMM)]
      next
    }
    p <- regmatches(l, regexec("^([a-zåäö]+) medborgaren (.+)$", l, perl = TRUE))[[1]]
    if (length(p) && !is.na(cur_grad) && !is.na(cur_orden)) {
      d <- dela_namn(p[3])
      land <- unname(LAND_ADJ[p[2]])
      kategori <- if (is.na(land)) NA_character_ else if (identical(land, "Sverige")) "svensk" else "utländsk"
      r <- ny_rad(d[["namn"]], d[["titel"]], cur_orden, cur_grad, datum, NA_character_, url, kategori)
      if (!is.na(land)) set(r, 1L, "land", land)
      rader[[length(rader) + 1]] <- r
    }
  }
  if (length(rader)) rbindlist(rader) else rad_tom()
}

tolka_mening <- function(url, lines) {
  datum <- sidans_datum(url, lines)
  txt <- paste(lines, collapse = " ")
  mm <- gregexpr(paste0("(?:utnämna|förläna|tilldela[rds]*|förlänade|utnämnde)\\s+([^.]{3,90}?)\\s+till\\s+",
                        "((?:kommendör|riddare)(?: med stora korset| av första klassen| av andra klassen)?)",
                        "\\s+av\\s+(?:Kungl\\. |Kungliga )?", ORDEN_RE), txt, perl = TRUE, ignore.case = TRUE)
  tr <- regmatches(txt, mm)[[1]]
  rader <- list()
  for (t in tr) {
    p <- regmatches(t, regexec(paste0("\\s+([^.]{3,90}?)\\s+till\\s+((?:kommendör|riddare)[a-zåäö ]*?)\\s+av\\s+(?:Kungl\\. |Kungliga )?",
                                      "((?:Serafimer|Svärds|Nordstjärne|Vasa))orden"), t, perl = TRUE, ignore.case = TRUE))[[1]]
    if (length(p) < 4) next
    d <- dela_namn(p[2])
    grad <- paste0(toupper(substr(p[3], 1, 1)), substr(p[3], 2, nchar(p[3])))
    orden <- names(ORDEN_STAMM)[match(tolower(p[4]), tolower(ORDEN_STAMM))]
    rader[[length(rader) + 1]] <- ny_rad(d[["namn"]], d[["titel"]], orden, grad, datum, NA_character_, url, NA_character_)
  }
  if (length(rader)) rbindlist(rader) else rad_tom()
}

utdrag_rader <- function(url, lines, titel) {
  datum <- sidans_datum(url, lines)
  traff <- grep(ORDEN_RE, lines, value = TRUE)
  if (!length(traff)) return(NULL)
  data.table(datum = if (is.na(datum)) IDAG else datum, källa_url = url, källtyp = kalltyp_av(url),
             utdrag = substr(paste0(titel, ": ", paste(traff, collapse = " ")), 1, 400))
}

tolka_sida <- function(url) {
  html <- hamta(url)
  if (is.null(html)) return(list(rader = rad_tom(), utdrag = NULL))
  lines <- sidlinjer(html)
  titel <- sidans_titel(html)
  if (!any(grepl(ORDEN_RE, c(titel, lines)))) return(list(rader = rad_tom(), utdrag = NULL))
  rader <- tolka_strukturerad(url, lines)
  if (!nrow(rader)) rader <- tolka_kmo(url, lines)
  if (!nrow(rader)) rader <- tolka_mening(url, lines)
  utdrag <- if (!nrow(rader)) utdrag_rader(url, lines, titel) else NULL
  list(rader = rader, utdrag = utdrag)
}

wd_api <- function(params) {
  vanta("www.wikidata.org")
  for (forsok in 1:3) {
    svar <- tryCatch(
      request("https://www.wikidata.org/w/api.php") |> req_url_query(!!!params, format = "json") |>
        req_user_agent(UA) |> req_timeout(40) |>
        req_error(is_error = function(r) FALSE) |> req_perform(),
      error = function(e) NULL)
    if (!is.null(svar) && resp_status(svar) == 200) return(resp_body_json(svar))
    Sys.sleep(10 * forsok)
  }
  notera_hoppat("wikidata.org", "API-anrop misslyckades efter 3 försök")
  NULL
}

wd_varden <- function(claims) {
  ok <- Filter(function(z) !identical(z$rank, "deprecated"), claims)
  v <- vapply(ok, function(z) {
    id <- if (identical(z$mainsnak$snaktype, "value")) z$mainsnak$datavalue$value$id else NULL
    if (is.null(id)) NA_character_ else id
  }, character(1))
  list(id = v[!is.na(v)], okanda = sum(is.na(v)))
}

wd_matcha <- function(namn) {
  s <- wd_api(list(action = "wbsearchentities", search = namn, language = "sv", type = "item", limit = 8))
  if (is.null(s)) return(NULL)
  ids <- vapply(Filter(function(e) identical(e$label, namn), s$search), function(e) e$id, character(1))
  if (!length(ids)) return(NULL)
  e <- wd_api(list(action = "wbgetentities", ids = paste(ids, collapse = "|"), props = "claims|sitelinks"))
  if (is.null(e)) return(NULL)
  traff <- Filter(function(x) {
    c <- x$claims
    manniska <- any(vapply(c$P31, function(z) identical(z$mainsnak$datavalue$value$id, "Q5"), logical(1)))
    manniska && is.null(c$P570) && length(x$sitelinks) > 0
  }, e$entities)
  if (length(traff) != 1) return(NULL)
  x <- traff[[1]]
  p27 <- wd_varden(x$claims$P27)
  p21 <- wd_varden(x$claims$P21)$id[1]
  land <- NA_character_
  if (length(p27$id) == 1 && p27$okanda == 0) {
    l <- wd_api(list(action = "wbgetentities", ids = p27$id, props = "labels", languages = "sv|en"))
    lab <- if (!is.null(l)) l$entities[[p27$id]]$labels else NULL
    land <- if (!is.null(lab$sv$value)) lab$sv$value else if (!is.null(lab$en$value)) lab$en$value else NA_character_
  }
  kategori <- if ("Q34" %in% p27$id) "svensk" else if (length(p27$id) && p27$okanda == 0) "utländsk" else NA_character_
  list(qid = x$id, kon = c(Q6581097 = "man", Q6581072 = "kvinna")[p21],
       land = land, kategori = kategori)
}

berika_wikidata <- function(rader) {
  for (i in seq_len(nrow(rader))) {
    w <- tryCatch(wd_matcha(rader$namn[i]), error = function(e) {
      notera_hoppat(rader$namn[i], paste("Wikidata-svaret kunde inte tolkas:", conditionMessage(e)))
      NULL
    })
    if (is.null(w)) next
    set(rader, i, "wikidata_qid", w$qid)
    if (!is.na(w$kon)) set(rader, i, "kön", unname(w$kon))
    if (!is.na(w$land)) set(rader, i, "land", w$land)
    if (is.na(rader$kategori[i]) && !is.na(w$kategori)) set(rader, i, "kategori", w$kategori)
  }
  rader
}

las_csv <- function(f) fread(f, encoding = "UTF-8", colClasses = "character", na.strings = "")

rang_av <- function(orden, grad, g) {
  r <- as.integer(g$rang[match(paste(orden, grad), paste(g$orden, g$grad))])
  ifelse(is.na(r), paste0("?", grad), as.character(r))
}

person_for_namn <- function(namn, befintliga) {
  nk <- namnnyckel(befintliga$namn)
  per_nyckel <- tapply(befintliga$person_id, nk, function(x) unique(x))
  nr <- namnnyckel(namn)
  pid <- vapply(nr, function(k) {
    p <- per_nyckel[[k]]
    if (length(p) == 1L) p else if (length(p) > 1L) paste0("?", k) else NA_character_
  }, character(1))
  ifelse(is.na(pid), slug(namn), unname(pid))
}

dubblettnyckel <- function(person, orden, rang, datum) {
  paste(person, orden, rang, substr(datum, 1, 4), sep = "|")
}

markera_dubbletter <- function(rader, befintliga, g = las_csv(FILE_G)) {
  if (!nrow(rader)) {
    rader[, `:=`(dubblett = logical(), grad_konflikt = character())]
    return(rader)
  }
  pid_b <- befintliga$person_id
  pid_r <- person_for_namn(rader$namn, befintliga)
  nyckel_b <- dubblettnyckel(pid_b, befintliga$orden, rang_av(befintliga$orden, befintliga$grad, g), befintliga$datum_tilldelning)
  nyckel_r <- dubblettnyckel(pid_r, rader$orden, rang_av(rader$orden, rader$grad, g), rader$datum_tilldelning)
  rader[, dubblett := nyckel_r %in% nyckel_b | duplicated(nyckel_r)]
  grov_b <- paste(pid_b, befintliga$orden, substr(befintliga$datum_tilldelning, 1, 4))
  grov_r <- paste(pid_r, rader$orden, substr(rader$datum_tilldelning, 1, 4))
  rader[, grad_konflikt := ifelse(!dubblett & grov_r %in% grov_b, befintliga$grad[match(grov_r, grov_b)], NA_character_)]
  rader
}

ny_id <- function(befintliga, n) {
  hogsta <- max(as.integer(sub("^T", "", befintliga$id)), 0L)
  sprintf("T%03d", hogsta + seq_len(n))
}

skriv_csv <- function(dt, f) fwrite(dt[, ..KOLUMNER], f, bom = FALSE, na = "")

logga <- function(ids, typ) {
  if (!length(ids)) return(invisible())
  fwrite(data.table(datum = IDAG, id = ids, andring = typ), FILE_L, append = TRUE)
}

tolka_p570 <- function(claims) {
  p <- Filter(function(z) !identical(z$rank, "deprecated"), claims$P570)
  if (!length(p)) return(NULL)
  pref <- Filter(function(z) identical(z$rank, "preferred"), p)
  z <- if (length(pref)) pref[[1]] else p[[1]]
  v <- if (identical(z$mainsnak$snaktype, "value")) z$mainsnak$datavalue$value else NULL
  if (is.null(v$time)) return(list(datum = NA_character_))
  d <- sub("^\\+", "", sub("T.*$", "", v$time))
  prec <- suppressWarnings(as.integer(v$precision))
  d <- if (is.na(prec) || prec < 9) NA_character_ else if (prec == 9) substr(d, 1, 4) else if (prec == 10) substr(d, 1, 7) else d
  full <- if (is.na(d)) NA_character_ else if (nchar(d) == 4) paste0(d, "-01-01") else if (nchar(d) == 7) paste0(d, "-01") else d
  giltigt <- !is.na(d) && grepl("^[0-9]{4}(-[0-9]{2}(-[0-9]{2})?)?$", d) && !grepl("-00", d) &&
    !is.na(as.Date(full, format = "%Y-%m-%d")) && as.Date(full, format = "%Y-%m-%d") <= as.Date(IDAG) &&
    as.integer(substr(d, 1, 4)) >= 1700
  list(datum = if (isTRUE(giltigt)) d else NA_character_)
}

dodskontroll <- function(t) {
  andrade <- character()
  okanda <- list()
  harDod <- t[!is.na(dödsdatum), .(dod = dödsdatum[1]), by = person_id]
  rad <- which(is.na(t$dödsdatum) & t$person_id %in% harDod$person_id)
  if (length(rad)) {
    set(t, rad, "dödsdatum", harDod$dod[match(t$person_id[rad], harDod$person_id)])
    andrade <- c(andrade, t$id[rad])
  }
  qid <- unique(t[is.na(dödsdatum) & !is.na(wikidata_qid), wikidata_qid])
  for (i in seq(1, max(length(qid), 1), by = 50)[seq_len(ceiling(length(qid) / 50))]) {
    delar <- qid[i:min(i + 49, length(qid))]
    e <- wd_api(list(action = "wbgetentities", ids = paste(delar, collapse = "|"), props = "claims"))
    if (is.null(e)) next
    for (q in names(e$entities)) {
      p <- tolka_p570(e$entities[[q]]$claims)
      if (is.null(p)) next
      pids <- unique(t$person_id[which(t$wikidata_qid == q)])
      if (is.na(p$datum)) {
        okanda[[length(okanda) + 1]] <- data.table(
          datum = IDAG, källa_url = paste0("https://www.wikidata.org/wiki/", q), källtyp = "wikipedia",
          utdrag = sprintf("Wikidata (%s) anger att %s är avliden, men saknar ett fullständigt dödsdatum. Kontrollera och sätt dödsdatum i tilldelningar.csv.",
                           q, t$namn[match(pids[1], t$person_id)]),
          flagga = "wikidata_dodsdatum_okant")
        next
      }
      rader <- which(t$person_id %in% pids & is.na(t$dödsdatum))
      set(t, rader, "dödsdatum", p$datum)
      andrade <- c(andrade, t$id[rader])
    }
  }
  list(tabell = t, andrade = unique(andrade), okanda = if (length(okanda)) rbindlist(okanda) else NULL)
}

feed_traffar <- function(url, namn) {
  html <- hamta(url, max_alder_h = 6)
  if (is.null(html)) return(NULL)
  x <- tryCatch(read_xml(html), error = function(e) NULL)
  if (is.null(x)) {
    notera_hoppat(url, "flödet kunde inte tolkas")
    return(NULL)
  }
  poster <- xml_find_all(x, "//item")
  rubrik <- xml_text(xml_find_first(poster, "title"))
  beskr <- xml_text(xml_find_first(poster, "description"))
  lank <- xml_text(xml_find_first(poster, "link"))
  datum <- xml_text(xml_find_first(poster, "pubDate"))
  traff <- grepl(ORDEN_RE, paste(rubrik, beskr)) | grepl("ordensförläning|riddarorden|kungliga ordnar", paste(rubrik, beskr), ignore.case = TRUE)
  if (!any(traff)) return(NULL)
  d <- suppressWarnings(as.Date(substr(datum[traff], 6, 16), format = "%d %b %Y"))
  data.table(datum = ifelse(is.na(d), IDAG, as.character(d)), källa_url = lank[traff],
             källtyp = "tidning",
             utdrag = substr(paste0(namn, ": ", rubrik[traff], " ", beskr[traff]), 1, 400))
}

kungahuset_adresser <- function() {
  index <- hamta("https://www.kungahuset.se/sitemapindex.xml", max_alder_h = 12)
  if (is.null(index)) return(character())
  kartor <- xml_text(xml_find_all(read_xml(index), "//*[local-name()='loc']"))
  adr <- character()
  gransdatum <- Sys.Date() - FIND_DAYS
  for (k in kartor) {
    vanta(host_av(k))
    f <- cache_fil(k)
    if (!file.exists(f) || difftime(Sys.time(), file.mtime(f), units = "hours") > 12) {
      svar <- tryCatch(request(k) |> req_user_agent(UA) |> req_timeout(90) |>
                         req_error(is_error = function(r) FALSE) |> req_perform(), error = function(e) NULL)
      if (is.null(svar) || resp_status(svar) != 200) {
        notera_hoppat(k, "sitemap kunde inte hämtas")
        next
      }
      kropp <- resp_body_raw(svar)
      if (identical(kropp[1:2], as.raw(c(0x1f, 0x8b)))) kropp <- memDecompress(kropp, "gzip", asChar = FALSE)
      writeBin(kropp, f)
    }
    x <- read_xml(f)
    noder <- xml_find_all(x, "//*[local-name()='url']")
    loc <- xml_text(xml_find_first(noder, "*[local-name()='loc']"))
    mod <- suppressWarnings(as.Date(substr(xml_text(xml_find_first(noder, "*[local-name()='lastmod']")), 1, 10), format = "%Y-%m-%d"))
    slugdatum <- suppressWarnings(as.Date(ifelse(grepl("/[0-9]{4}-[0-9]{2}-[0-9]{2}-", loc),
                                                 sub(".*/([0-9]{4}-[0-9]{2}-[0-9]{2})-.*", "\\1", loc), NA_character_),
                                          format = "%Y-%m-%d"))
    datum <- mod
    datum[!is.na(slugdatum)] <- slugdatum[!is.na(slugdatum)]
    ok <- !is.na(loc) & !is.na(datum) & datum >= gransdatum &
      grepl("/arkiv/(nyheter|pressmeddelanden)/", loc) &
      grepl("orden|riddarordn|forlan|statsbesok|honorarkonsul|tilldel", loc)
    adr <- c(adr, loc[ok])
  }
  unique(adr)
}

regeringen_adresser <- function() {
  adr <- character()
  for (sida in c("https://www.regeringen.se/pressmeddelanden/", "https://www.regeringen.se/artiklar/")) {
    html <- hamta(sida, max_alder_h = 12)
    if (is.null(html)) next
    h <- read_html(html)
    lank <- html_attr(html_elements(h, "a"), "href")
    lank <- lank[!is.na(lank) & grepl("^/(pressmeddelanden|artiklar)/[0-9]{4}/[0-9]{2}/", lank)]
    lank <- lank[grepl("orden|utmarkelse|tilldelas|forlan", lank)]
    adr <- c(adr, paste0("https://www.regeringen.se", lank))
  }
  unique(adr)
}

kmo_adresser <- function() {
  html <- hamta("https://kungligmajestatsorden.se/nyhetsarkiv", max_alder_h = 12)
  if (is.null(html)) return(character())
  lank <- html_attr(html_elements(read_html(html), "a"), "href")
  lank <- unique(lank[!is.na(lank) & grepl("/nyhetsarkiv/nyheter/[0-9]{4}-[0-9]{2}-[0-9]{2}-", lank)])
  datum <- as.Date(sub(".*/([0-9]{4}-[0-9]{2}-[0-9]{2})-.*", "\\1", lank))
  ok <- datum >= Sys.Date() - FIND_DAYS & grepl("orden|forlan|riddar|honorarkonsul|attach", lank)
  ut <- lank[ok]
  ifelse(grepl("^https?://", ut), ut, paste0("https://kungligmajestatsorden.se", ut))
}

FLODEN <- c(DN = "https://www.dn.se/rss/", SvD = "https://www.svd.se/feed/articles.rss")

skriv_ut <- function(rader, rubrik) {
  cat(sprintf("\n== %s (%d)\n", rubrik, nrow(rader)))
  for (i in seq_len(nrow(rader))) {
    r <- rader[i]
    cat(sprintf("[%s] %s | %s | %s | %s | %s\n      motivering: %s\n",
                if (isTRUE(r$dubblett)) "dubblett" else "ny", r$namn,
                if (is.na(r$yrke_titel)) "-" else r$yrke_titel, r$orden, r$grad,
                r$datum_tilldelning, if (is.na(r$motivering)) "-" else r$motivering))
  }
}

validera_grader <- function(rader) {
  g <- las_csv(FILE_G)
  rader[, grad_ok := paste(orden, grad) %in% paste(g$orden, g$grad)]
  rader
}

PERSON_KOLUMNER <- c("person_id", "namn", "kön", "kategori", "land", "kunglig", "wikidata_qid", "dödsdatum")

koppla_personer <- function(nya, t) {
  for (i in seq_len(nrow(nya))) {
    matches <- if (!is.na(nya$wikidata_qid[i])) unique(t[wikidata_qid == nya$wikidata_qid[i], person_id]) else character()
    if (!length(matches)) matches <- unique(t[namnnyckel(namn) == namnnyckel(nya$namn[i]), person_id])
    if (length(matches) == 1L) {
      kalla <- t[person_id == matches][1]
      for (k in PERSON_KOLUMNER) set(nya, i, k, kalla[[k]])
    } else if (length(matches) > 1L || nya$person_id[i] %in% t$person_id) {
      set(nya, i, "kategori", NA_character_)
    }
  }
  nya
}

kor <- function(args) {
  dry <- "--dry-run" %in% args
  auto <- "--auto" %in% args
  url_arg <- if ("--url" %in% args) args[which(args == "--url") + 1] else NA_character_
  if (!auto && is.na(url_arg)) stop("Ange --auto eller --url <URL>. Lägg till --dry-run för att inte skriva något.")

  t <- las_csv(FILE_T)
  k <- if (file.exists(FILE_K)) las_csv(FILE_K) else data.table()
  adresser <- if (!is.na(url_arg)) url_arg else c(kungahuset_adresser(), regeringen_adresser(), kmo_adresser())
  cat(sprintf("Sidor att undersöka: %d\n", length(adresser)))

  alla <- list()
  utdrag <- list()
  for (u in adresser) {
    r <- tolka_sida(u)
    if (nrow(r$rader)) alla[[length(alla) + 1]] <- r$rader
    if (!is.null(r$utdrag)) utdrag[[length(utdrag) + 1]] <- r$utdrag
  }
  if (auto) {
    for (n in names(FLODEN)) {
      f <- feed_traffar(FLODEN[[n]], n)
      if (!is.null(f)) utdrag[[length(utdrag) + 1]] <- f
    }
  }
  rader <- if (length(alla)) rbindlist(alla, fill = TRUE) else rad_tom()
  rader <- markera_dubbletter(rader, t)
  nya <- rader[dubblett == FALSE]
  if (nrow(nya)) nya <- berika_wikidata(nya)
  if (nrow(nya)) nya <- koppla_personer(nya, t)
  if (nrow(nya)) {
    nya <- validera_grader(nya)
    nya[!is.na(grad_konflikt) & is.na(flagga), flagga := paste0("grad_konflikt_kallor:", grad_konflikt)]
    nya[, till_data := källtyp %in% c("kungahuset", "regeringen", "officiell_publikation") & !is.na(kategori) & grad_ok &
          !is.na(datum_tilldelning) & is.na(grad_konflikt)]
  }
  till_t <- if (nrow(nya)) nya[till_data == TRUE] else nya
  till_k <- if (nrow(nya)) nya[till_data == FALSE] else nya

  skriv_ut(rader, "Fynd på sidorna")
  cat(sprintf("\nNya rader till tilldelningar.csv: %d, till kandidater.csv: %d, dubbletter: %d\n",
              nrow(till_t), nrow(till_k), sum(rader$dubblett)))
  if (length(utdrag)) {
    u <- rbindlist(utdrag, fill = TRUE)
    cat("\n== Fritextfynd (kandidater, namn och grad ej utlästa)\n")
    print(u[, .(datum, källtyp, källa_url, utdrag = substr(utdrag, 1, 110))], row.names = FALSE, trunc.cols = FALSE)
  } else u <- NULL

  t_ny <- copy(t)
  if (nrow(till_t)) {
    till_t[, id := ny_id(t, .N)]
    t_ny <- rbindlist(list(t_ny, till_t[, ..KOLUMNER]), fill = TRUE)
  }
  dod <- if (auto) dodskontroll(t_ny) else list(tabell = t_ny, andrade = character(), okanda = NULL)
  if (auto) cat(sprintf("\nMottagare med nytt dödsdatum i Wikidata: %d rader, avlidna utan fullständigt datum: %d\n",
                        length(dod$andrade), if (is.null(dod$okanda)) 0L else nrow(dod$okanda)))
  if (!is.null(dod$okanda)) {
    u <- rbindlist(list(u, dod$okanda), fill = TRUE)
  }

  if (length(skipped)) cat("\n== Hoppade över\n", paste0(" - ", unique(skipped), collapse = "\n"), "\n", sep = "")
  if (dry) {
    cat("\n--dry-run: ingenting skrevs.\n")
    return(invisible())
  }

  t <- dod$tabell
  if (nrow(till_t)) logga(till_t$id, "tillagd")
  if (length(dod$andrade)) logga(dod$andrade, "ändrad")
  skriv_csv(t, FILE_T)

  kand_u <- if (!is.null(u)) u[, .(källa_url, källtyp, datum_tilldelning = datum, utdrag,
                                   flagga = if ("flagga" %in% names(u)) flagga else NA_character_)]
  kand <- rbindlist(list(k, if (nrow(till_k)) till_k[, ..KOLUMNER], kand_u), fill = TRUE)
  if (nrow(kand)) {
    for (kol in c(KOLUMNER, "utdrag")) if (!kol %in% names(kand)) kand[, (kol) := NA_character_]
    kand <- kand[!duplicated(paste(källa_url, namn, orden, grad, utdrag))]
    fwrite(kand[, c(KOLUMNER, "utdrag"), with = FALSE], FILE_K, na = "")
  }
  cat("\nKlart.\n")
}

if (sys.nframe() == 0) kor(commandArgs(trailingOnly = TRUE))
