source('R/common.R')
t <- load_tilldelningar()
stopifnot(t[person_id == 'anuwat-maytheewibulwut', grad] == 'Grad inte angiven')
stopifnot(grepl('Grad inte angiven', as.character(person_page(t, 'anuwat-maytheewibulwut')), fixed = TRUE))
stopifnot(as.character(datum_date('2023-05')) == '2023-05-01')
stopifnot(!grepl('onmouseover="x"', source_html('https://example.org/" onmouseover="x', 'kungahuset'), fixed = TRUE))
stopifnot(grepl('Lee</span> Myung-bak', namn_sc_html('Lee Myung-bak'), fixed = TRUE))
stopifnot(flagga_text('Egen anteckning') == 'Egen anteckning')
stopifnot(grepl('Post finns', as.character(person_page(t, 'anne-lhuillier-wahlstrom')), fixed = TRUE))
t[person_id == 'anne-lhuillier-wahlstrom', datum := '2024-03-22']
stopifnot(!grepl('Post finns', as.character(person_page(t, 'anne-lhuillier-wahlstrom')), fixed = TRUE))
source('scraper/run.R')
x <- tolka_kmo('https://kungligmajestatsorden.se/nyhetsarkiv/nyheter/2026-04-13-test',
               c('till Kommendör av Svärdsorden', 'finske medborgaren överste Juha Kilpi'))
stopifnot(nrow(x) == 1, x$orden == 'Svärdsorden')
stopifnot(is.na(kategori_fran_sida('Utmärkelser tilldelas svenska och utländska medborgare')$kategori))
stopifnot(slug("Anne L'Huillier Wahlström") == 'anne-lhuillier-wahlstrom')


sorterad <- function(x) x[order(sortnyckel(x), method = 'radix')]
stopifnot(identical(sorterad(c('Bērziņš, Andris', 'Băsescu, Traian', 'Ångström', 'Zorn', 'Äppel', 'Öman', 'Țiriac', 'Tyr')),
                    c('Băsescu, Traian', 'Bērziņš, Andris', 'Țiriac', 'Tyr', 'Zorn', 'Ångström', 'Äppel', 'Öman')))
stopifnot(sortnyckel('Müller') > sortnyckel('Mylar'), sortnyckel('Müller') < sortnyckel('Myz'))

stopifnot(grepl('Narendra Modi', html_text_enkel(namn_sc_html('Narendra Modi'))))
tab <- as.character(order_table(t, 'Nordstjärneorden', 'svensk', tid = 'x'))
stopifnot(grepl('sok_txt', tab, fixed = TRUE), grepl('21 mars 2024', tab, fixed = TRUE))
stopifnot(grepl("Svante Pääbo", tab, fixed = TRUE))

slugfall <- fread('tests/slug-fall.csv', encoding = 'UTF-8', colClasses = 'character')
stopifnot(identical(slug(slugfall$namn), slugfall$slug))
befintlig <- t[kunglig == 'FALSE']
avvik <- befintlig[slug(namn) != person_id & !grepl(',| f\\. |:', namn), .(person_id, namn, slug = slug(namn))]
if (nrow(avvik)) {
  cat('VARNING: person_id skiljer från slug(namn) för', nrow(avvik), 'personer (kontrollera om id:t är avsiktligt):\n')
  print(avvik, row.names = FALSE)
}

g <- las_csv(FILE_G)
bef <- data.table(person_id = 'p-lans', namn = 'Håkan Lans', orden = 'Vasaorden', grad = 'Kommendör av andra klassen', datum_tilldelning = '2025-04-30')
ny <- function(namn, grad, datum) ny_rad(namn, '', 'Vasaorden', grad, datum, NA_character_, 'https://www.kungahuset.se/x', 'svensk')
r <- markera_dubbletter(rbindlist(list(ny('Håkan Lans', 'Kommendör', '2025-04-30'), ny('Håkan Lans', 'Kommendör', '2026-04-30'),
                                       ny('Håkan Lans', 'Riddare', '2025-04-30'), ny('Håkan Lans', 'Kommendör med stora korset', '2027-04-30'))), bef, g)
stopifnot(identical(r$dubblett, c(TRUE, FALSE, FALSE, FALSE)), is.na(r$grad_konflikt[2]), r$grad_konflikt[3] == 'Kommendör av andra klassen')

tp <- data.table(id = c('T1', 'T2', 'T3'), person_id = c('p1', 'p1', 'p2'), namn = c('A Aa', 'A Aa', 'B Bb'), kön = c('man', 'man', NA),
                 kategori = 'svensk', land = c('Sverige', 'Sverige', NA), kunglig = c('FALSE', 'FALSE', 'TRUE'),
                 wikidata_qid = c('Q10', NA, NA), dödsdatum = NA_character_)
nyr <- ny_rad('A Aa', '', 'Vasaorden', 'Riddare', '2026-01-01', NA_character_, 'https://www.kungahuset.se/x', NA_character_)
nyr <- koppla_personer(nyr, tp)
stopifnot(nyr$person_id == 'p1', nyr$wikidata_qid == 'Q10', nyr$land == 'Sverige', nyr[['kön']] == 'man', nyr$kategori == 'svensk')

p570 <- function(time, precision, snaktype = 'value') list(P570 = list(list(mainsnak = c(list(snaktype = snaktype),
          if (snaktype == 'value') list(datavalue = list(value = list(time = time, precision = precision)))))))
stopifnot(tolka_p570(p570('+2020-05-17T00:00:00Z', 11))$datum == '2020-05-17', tolka_p570(p570('+2020-05-00T00:00:00Z', 10))$datum == '2020-05',
          tolka_p570(p570('+2020-00-00T00:00:00Z', 9))$datum == '2020', is.na(tolka_p570(p570('+1990-00-00T00:00:00Z', 8))$datum),
          is.na(tolka_p570(p570(NA, NA, 'somevalue'))$datum), is.null(tolka_p570(list())), is.na(tolka_p570(p570('+2999-01-01T00:00:00Z', 11))$datum))

wd_api_orig <- wd_api
wd_api <- function(params) list(entities = list(Q10 = list(claims = p570('+2026-11-01T00:00:00Z', 11)), Q20 = list(claims = p570(NA, NA, 'somevalue'))))
tp[, wikidata_qid := c('Q10', NA, 'Q20')]
tp[3, person_id := 'p3']
d <- dodskontroll(copy(tp))
wd_api <- wd_api_orig
stopifnot(identical(d$tabell$dödsdatum, c('2026-11-01', '2026-11-01', NA)), setequal(d$andrade, c('T1', 'T2')),
          nrow(d$okanda) == 1, d$okanda$flagga == 'wikidata_dodsdatum_okant')

ent <- function(claims) list(entities = list(Q1 = list(id = 'Q1', claims = claims, sitelinks = list(svwiki = list(title = 'X')))))
manniska <- list(list(mainsnak = list(datavalue = list(value = list(id = 'Q5')))))
okant <- list(list(mainsnak = list(snaktype = 'somevalue')))
land_q <- function(id) list(list(mainsnak = list(snaktype = 'value', datavalue = list(value = list(id = id)))))
for (fall in list(list(P31 = manniska, P21 = okant, P27 = land_q('Q34')), list(P31 = manniska, P27 = okant), list(P31 = manniska, P27 = land_q('Q999999')))) {
  wd_api <- function(params) {
    if (params$action == 'wbsearchentities') return(list(search = list(list(id = 'Q1', label = 'Test Testsson'))))
    if (!is.null(params$props) && params$props == 'labels') return(list(entities = list(Q999999 = list(labels = list(en = list(value = 'Old state'))))))
    ent(fall)
  }
  w <- wd_matcha('Test Testsson')
  stopifnot(!is.null(w), w$qid == 'Q1')
}
stopifnot(identical(w$land, 'Old state'), w$kategori == 'utländsk')
wd_api <- wd_api_orig

x <- tolka_kmo('https://kungligmajestatsorden.se/nyhetsarkiv/nyheter/2026-04-13-test',
               c('till Kommendör av Nordstjärneorden', 'svenske medborgaren direktör Sven Svensson', 'norske medborgaren professor Ola Nordmann',
                 'kamtjatkiske medborgaren professor Ivan Ivanov'))
stopifnot(identical(x$kategori, c('svensk', 'utländsk', NA)), identical(x$land, c('Sverige', 'Norge', NA)))

CACHE_orig <- CACHE_DIR
vanta_orig <- vanta
CACHE_DIR <- file.path(tempdir(), 'sitemap-cache')
vanta <- function(host) invisible()
dir.create(CACHE_DIR, showWarnings = FALSE)
idx <- 'https://www.kungahuset.se/sitemapindex.xml'
sm <- 'https://www.kungahuset.se/sitemap-test.xml'
writeLines(sprintf('<?xml version="1.0"?><sitemapindex xmlns="http://www.sitemaps.org/schemas/sitemap/0.9"><sitemap><loc>%s</loc></sitemap></sitemapindex>', sm), cache_fil(idx))
ny_dag <- format(Sys.Date() - 3); gammal <- format(Sys.Date() - 400)
u <- function(loc, lm = NULL) paste0('<url><loc>', loc, '</loc>', if (!is.null(lm)) paste0('<lastmod>', lm, '</lastmod>'), '</url>')
kn <- 'https://www.kungahuset.se/arkiv/nyheter/'
writeLines(paste0('<?xml version="1.0"?><urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
  paste(c(u('https://www.kungahuset.se/', ny_dag), u(paste0(kn, ny_dag, '-x-ordensforlaning'), ny_dag),
          u(paste0(kn, 'gammal-ordensforlaning-utan-lastmod')), u(paste0(kn, 'gammal-ordensforlaning'), gammal),
          u(paste0(kn, 'ny-ordensforlaning'), ny_dag)), collapse = ''), '</urlset>'), cache_fil(sm))
stopifnot(identical(kungahuset_adresser(), paste0(kn, c(paste0(ny_dag, '-x-ordensforlaning'), 'ny-ordensforlaning'))))
CACHE_DIR <- CACHE_orig
vanta <- vanta_orig

cat('Databehandling och skraparens regressionsprov godkända.\n')
