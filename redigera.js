// Validation rules shared with tests/validate.R: today is the UTC date (as in CI), text fields are trimmed
// (fread trims them), duplicates are judged on person, orden, grade rank and year, and an empty grade is
// accepted only for an existing row that already had an empty grade. Slugs follow scraper/run.R (tests/slug-fall.csv).
(function (root) {
  "use strict";

  var REPO = "LeoLevenius/kungliga-ordnar";
  var BRANCH = "main";
  var API = "https://api.github.com";
  var PATH_T = "data/tilldelningar.csv";
  var PATH_LOG = "data/andringslogg.csv";
  var PATH_G = "data/grader.csv";
  var PATH_B = "data/bilder.csv";
  var OWNER = "LeoLevenius";
  var TOKEN_KEY = "kungliga-ordnar-token";

  var ORDNAR = ["Serafimerorden", "Svärdsorden", "Nordstjärneorden", "Vasaorden"];
  var KALLTYPER = { kungahuset: "Kungahuset.se", regeringen: "Regeringen.se", officiell_publikation: "Officiell publikation", tidning: "Tidning", wikipedia: "Wikipedia" };
  var PERSON_FALT = ["namn", "kön", "kategori", "land", "kunglig", "wikidata_qid", "dödsdatum"];
  var RAD_FALT = ["orden", "grad", "yrke_titel", "datum_tilldelning", "motivering", "motivering_typ", "statsbesök", "källa_url", "källtyp", "källa_hämtad", "flagga"];
  var BILD_FALT = ["commons_sida", "licens", "licens_url", "upphov", "kalla", "anvandning", "bildtext"];
  var LICENSER = {
    "CC0": "http://creativecommons.org/publicdomain/zero/1.0/deed.en",
    "Public domain": "",
    "CC BY-SA 2.5": "https://creativecommons.org/licenses/by-sa/2.5",
    "CC BY-SA 3.0": "https://creativecommons.org/licenses/by-sa/3.0",
    "CC BY-SA 4.0": "https://creativecommons.org/licenses/by-sa/4.0",
    "CC BY 2.5": "https://creativecommons.org/licenses/by/2.5",
    "CC BY 3.0": "https://creativecommons.org/licenses/by/3.0",
    "CC BY 4.0": "https://creativecommons.org/licenses/by/4.0"
  };
  var MAX_SIDA = 1100;

  function parseCsv(text) {
    if (text.charCodeAt(0) === 0xfeff) text = text.slice(1);
    var rows = [], row = [], f = "", q = false;
    for (var i = 0; i < text.length; i++) {
      var c = text[i];
      if (q) {
        if (c === '"') {
          if (text[i + 1] === '"') { f += '"'; i++; } else q = false;
        } else f += c;
      } else if (c === '"') q = true;
      else if (c === ",") { row.push(f); f = ""; }
      else if (c === "\n") { row.push(f); rows.push(row); row = []; f = ""; }
      else if (c !== "\r") f += c;
    }
    if (q) throw new Error("CSV-filen har ett citattecken som inte avslutas.");
    if (f !== "" || row.length) { row.push(f); rows.push(row); }
    return rows;
  }

  function csvCell(v) {
    v = v == null ? "" : String(v);
    return /[",\n\r]/.test(v) ? '"' + v.replace(/"/g, '""') + '"' : v;
  }

  function csvLines(header, objs) {
    return objs.map(function (o) { return header.map(function (k) { return csvCell(o[k]); }).join(","); }).join("\n") + "\n";
  }

  function toCsv(header, objs) {
    return header.map(csvCell).join(",") + "\n" + (objs.length ? csvLines(header, objs) : "");
  }

  function csvToObjects(text) {
    var rows = parseCsv(text);
    if (rows.length > 1 && rows[rows.length - 1].length === 1 && rows[rows.length - 1][0] === "") rows.pop();
    var header = rows[0];
    if (!header) throw new Error("CSV-filen är tom.");
    rows.forEach(function (r, i) {
      if (r.length !== header.length) throw new Error("CSV-filen har fel antal fält på rad " + (i + 1) + ": " + r.length + " i stället för " + header.length + ".");
    });
    var objs = rows.slice(1).map(function (r) {
      var o = {};
      header.forEach(function (k, i) { o[k] = r[i] == null ? "" : r[i]; });
      return o;
    });
    return { header: header, rows: objs };
  }

  function idagIso() {
    return new Date().toISOString().slice(0, 10);
  }

  function precisionFor(d) {
    if (!d) return "";
    return d.length === 10 ? "dag" : d.length === 7 ? "månad" : "år";
  }

  function datumFel(d) {
    if (!d) return null;
    var idag = idagIso();
    var ar = Number(idag.slice(0, 4));
    var m;
    if ((m = /^(\d{4})-(\d{2})-(\d{2})$/.exec(d))) {
      var dt = new Date(Date.UTC(+m[1], +m[2] - 1, +m[3]));
      if (+m[1] < 1700 || dt.getUTCFullYear() !== +m[1] || dt.getUTCMonth() !== +m[2] - 1 || dt.getUTCDate() !== +m[3]) return "datumet finns inte";
      return d > idag ? "datumet ligger i framtiden" : null;
    }
    if ((m = /^(\d{4})-(\d{2})$/.exec(d))) {
      if (+m[2] < 1 || +m[2] > 12) return "månaden måste vara 01–12";
      return d > idag.slice(0, 7) ? "månaden ligger i framtiden" : (+m[1] < 1700 ? "året måste vara minst 1700" : null);
    }
    if ((m = /^(\d{4})$/.exec(d))) {
      return +m[1] < 1700 || +m[1] > ar ? "året måste vara 1700–" + ar : null;
    }
    return "använd ÅÅÅÅ-MM-DD, ÅÅÅÅ-MM eller ÅÅÅÅ";
  }

  function nyckel(s) {
    s = (s || "").toLowerCase()
      .replace(/ø/g, "o").replace(/æ/g, "ae").replace(/ß/g, "ss").replace(/đ/g, "d").replace(/ł/g, "l");
    s = s.normalize("NFD").replace(/[̀-ͯ]/g, "").replace(/[^a-z0-9 ]/g, "");
    return s.replace(/\s+/g, " ").trim();
  }

  var SLUG_SÄRSKILT = { "ø": "o", "æ": "ae", "ð": "d", "þ": "th", "ß": "ss", "ł": "l", "đ": "d", "œ": "oe", "ı": "i", "ħ": "h", "ŧ": "t" };

  function slugify(namn) {
    var s = String(namn == null ? "" : namn).toLowerCase().replace(/['’‘ʼ:]/g, "");
    s = s.replace(/[øæðþßłđœıħŧ]/g, function (c) { return SLUG_SÄRSKILT[c]; });
    s = s.normalize("NFD").replace(/[̀-ͯ]/g, "");
    return s.replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
  }

  function validera(table, grader, rörda, orig) {
    var fel = [];
    var rörd = {};
    rörda.forEach(function (id) { rörd[id] = true; });
    var gradSet = {}, rang = {}, origGrad = {};
    grader.forEach(function (g) { gradSet[g.orden + "\u0000" + g.grad] = true; rang[g.orden + "\u0000" + g.grad] = g.rang; });
    (orig || []).forEach(function (o) { origGrad[o.id] = o.grad; });
    var sedda = {}, nycklar = {}, qid = {}, rangNycklar = {};
    table.forEach(function (r) {
      if (sedda[r.id]) fel.push({ id: r.id, msg: "id används två gånger" });
      sedda[r.id] = true;
      var k = nyckel(r.namn) + "|" + r.orden + "|" + r.grad + "|" + r.datum_tilldelning;
      (nycklar[k] = nycklar[k] || []).push(r.id);
      var rk = rang[r.orden + "\u0000" + r.grad];
      if (rk !== undefined) (rangNycklar[r.person_id + "|" + r.orden + "|" + rk + "|" + r.datum_tilldelning.slice(0, 4)] = rangNycklar[r.person_id + "|" + r.orden + "|" + rk + "|" + r.datum_tilldelning.slice(0, 4)] || []).push(r.id);
      if (r.wikidata_qid) (qid[r.wikidata_qid] = qid[r.wikidata_qid] || {})[r.person_id] = true;
    });
    table.forEach(function (r) {
      if (!rörd[r.id]) return;
      var p = function (cond, msg) { if (!cond) fel.push({ id: r.id, msg: msg }); };
      p(r.namn.trim() !== "", "namn saknas");
      p(ORDNAR.indexOf(r.orden) >= 0, "orden måste vara en av de fyra ordnarna");
      p(gradSet[r.orden + "\u0000" + r.grad] || (r.grad === "" && origGrad[r.id] === ""),
        r.grad === "" ? "grad måste anges (en befintlig rad utan grad får lämnas som den är)" : "graden \"" + r.grad + "\" finns inte i " + r.orden);
      var df = datumFel(r.datum_tilldelning);
      p(!df, "datum: " + df);
      var validUrl = false;
      try { var parsed = new URL(r["källa_url"]); validUrl = /^(http:|https:)$/.test(parsed.protocol) && !/[\s"<>]/.test(r["källa_url"]); } catch (_) {}
      p(validUrl, "källa_url måste vara en giltig http(s)-adress");
      p(r.motivering_typ !== "statsbesök" || r["statsbesök"] !== "", "motivering_typ statsbesök kräver ifyllt statsbesök");
      p(r.kategori === "svensk" || r.kategori === "utländsk", "kategori måste vara svensk eller utländsk");
      var land = r.land.trim();
      p((r.kategori === "svensk" && (land === "" || land === "Sverige")) ||
        (r.kategori === "utländsk" && (land === "" || land !== "Sverige")),
        "kategori och land stämmer inte (svensk = Sverige eller okänt, utländsk = annat land eller okänt)");
      p(r.kunglig === "TRUE" || r.kunglig === "FALSE", "kunglig måste vara TRUE eller FALSE");
      p(KALLTYPER[r["källtyp"]], "ogiltig källtyp");
      p(r.motivering_typ === "" || r.motivering_typ === "officiell" || r.motivering_typ === "statsbesök", "ogiltig motivering_typ");
      p(r["kön"] === "" || r["kön"] === "man" || r["kön"] === "kvinna", "kön måste vara man, kvinna eller tomt");
      p(!datumFel(r["dödsdatum"]), "dödsdatum: " + datumFel(r["dödsdatum"]));
      var k = nyckel(r.namn) + "|" + r.orden + "|" + r.grad + "|" + r.datum_tilldelning;
      var rk2 = rang[r.orden + "\u0000" + r.grad];
      var rl = rk2 === undefined ? [r.id] : rangNycklar[r.person_id + "|" + r.orden + "|" + rk2 + "|" + r.datum_tilldelning.slice(0, 4)];
      p(rl.length === 1, "dubblett av " + rl.filter(function (x) { return x !== r.id; }).join(", ") + " (samma person, orden, grad och år; gradnamn med samma rang räknas som samma grad)");
      p(nycklar[k].length === 1, "dubblett av " + nycklar[k].filter(function (x) { return x !== r.id; }).join(", ") + " (samma namn, orden, grad och datum)");
      p(!r.wikidata_qid || Object.keys(qid[r.wikidata_qid]).length === 1, "Wikidata-id " + r.wikidata_qid + " används av flera personer");
      p(!r.wikidata_qid || /^Q\d+$/.test(r.wikidata_qid), "Wikidata-id ska vara på formen Q12345");
    });
    return fel;
  }

  function nastaId(rows) {
    var max = 0;
    rows.forEach(function (r) { var m = /^T(\d+)$/.exec(r.id); if (m) max = Math.max(max, +m[1]); });
    var n = String(max + 1);
    while (n.length < 3) n = "0" + n;
    return "T" + n;
  }

  function bildFrån(S, rows) {
    var r = rows.filter(function (x) { return x.bild_url; })[0];
    if (!r) return null;
    var fil = r.bild_url.replace(/^.*\//, "");
    var b = S.bilder.filter(function (x) { return x.fil === fil; })[0] || {};
    var o = { fil: fil, ny: null };
    BILD_FALT.forEach(function (k) { o[k] = b[k] || ""; });
    return o;
  }

  function nyBild() {
    return { fil: null, ny: null, commons_sida: "", licens: "CC BY-SA 4.0", licens_url: LICENSER["CC BY-SA 4.0"], upphov: "", kalla: "", anvandning: "sköld", bildtext: "" };
  }

  function filnamn(S, ed, origFil) {
    var bas = (ed.bild.anvandning === "sköld" ? "skold-" : "utdelning-") + ed.pid;
    var ext = ed.bild.ny.ext, n = 1, f = bas + "." + ext;
    while (S.bilder.some(function (x) { return x.fil === f && (x.fil !== origFil || S.rows.some(function (r) { return r.person_id !== ed.pid && r.bild_url === "img/" + x.fil; })); })) f = bas + "-" + (++n) + "." + ext;
    return f;
  }

  function trimma(v) { return typeof v === "string" ? v.trim() : v; }

  function beräknaÄndringar(S, ed) {
    var idag = idagIso();
    var orig = S.rows.filter(function (r) { return r.person_id === ed.pid; });
    var origRad = orig.filter(function (r) { return r.bild_url; })[0];
    var origFil = origRad ? origRad.bild_url.replace(/^.*\//, "") : "";
    var bild = ed.bild, bilder = S.bilder.slice(), filer = [], bildFält = [];
    var bildVärden = { bild_url: "", bild_licens: "", bild_upphov: "" };
    if (bild) {
      var fil = bild.ny ? filnamn(S, ed, origFil) : bild.fil;
      var gammal = S.bilder.filter(function (x) { return x.fil === origFil; })[0];
      var rad;
      if (bild.ny) {
        rad = {
          fil: fil, orden: ed.rader[0].f.orden, person_qid: ed.person.wikidata_qid, person_namn: ed.person.namn,
          bredd: String(bild.ny.bredd), hojd: String(bild.ny.hojd),
          kommentar: "Uppladdad via webben " + idag + "; original " + bild.ny.orig + " px"
        };
        filer.push({ path: "img/" + fil, base64: bild.ny.base64 });
        bildFält.push({ k: "bild.fil", från: origFil, till: fil });
      } else {
        rad = Object.assign({}, gammal || { fil: fil, orden: ed.rader[0].f.orden, person_qid: ed.person.wikidata_qid, person_namn: ed.person.namn, bredd: "", hojd: "", kommentar: "" });
      }
      BILD_FALT.forEach(function (k) {
        var från = gammal ? gammal[k] || "" : "";
        if (från !== bild[k]) bildFält.push({ k: "bild." + k, från: från, till: bild[k] });
        rad[k] = bild[k];
      });
      var i = bilder.indexOf(gammal);
      if (gammal && !bild.ny) bilder[i] = rad;
      else {
        if (gammal && !S.rows.some(function (r) { return r.person_id !== ed.pid && r.bild_url === "img/" + origFil; })) bilder.splice(i, 1);
        bilder.push(rad);
      }
      bildVärden = { bild_url: "img/" + fil, bild_licens: bild.licens, bild_upphov: bild.upphov };
    } else if (origFil) {
      var g = S.bilder.filter(function (x) { return x.fil === origFil; })[0];
      if (g && !S.rows.some(function (r) { return r.person_id !== ed.pid && r.bild_url === "img/" + origFil; })) bilder.splice(bilder.indexOf(g), 1);
      bildFält.push({ k: "bild.fil", från: origFil, till: "" });
    }
    var ändringar = [];
    var spridBild = bild && !bild.ny && origFil && bild.fil === origFil;
    var ny = S.rows.map(function (r) {
      var j = orig.indexOf(r);
      if (j < 0) {
        if (!spridBild || r.bild_url !== "img/" + origFil) return r;
        var d = Object.assign({}, r), df = [];
        [["bild_licens", bild.licens], ["bild_upphov", bild.upphov]].forEach(function (x) {
          if (d[x[0]] !== x[1]) { df.push({ k: x[0], från: d[x[0]], till: x[1] }); d[x[0]] = x[1]; }
        });
        if (df.length) ändringar.push({ id: r.id, typ: "ändrad", fält: df, person: r.person_id });
        return d;
      }
      var o = Object.assign({}, r), fält = [];
      var rad = ed.rader.filter(function (x) { return x.id === r.id; })[0];
      PERSON_FALT.forEach(function (k) {
        var v = trimma(ed.person[k]);
        if (o[k] !== v) { fält.push({ k: k, från: o[k], till: v }); o[k] = v; }
      });
      RAD_FALT.forEach(function (k) {
        var v = trimma(rad.f[k]);
        if (o[k] !== v) { fält.push({ k: k, från: o[k], till: v }); o[k] = v; }
      });
      Object.keys(bildVärden).forEach(function (k) {
        if (o[k] !== bildVärden[k]) { fält.push({ k: k, från: o[k], till: bildVärden[k] }); o[k] = bildVärden[k]; }
      });
      if (fält.some(function (x) { return x.k === "datum_tilldelning"; })) o.datum_precision = precisionFor(o.datum_tilldelning);
      if (fält.length) ändringar.push({ id: r.id, typ: "ändrad", fält: fält });
      return o;
    });
    var tillagda = [];
    ed.rader.filter(function (x) { return !x.id; }).forEach(function (x) {
      var o = {};
      S.header.forEach(function (k) { o[k] = ""; });
      o.id = nastaId(ny.concat(tillagda));
      o.person_id = ed.pid;
      PERSON_FALT.forEach(function (k) { o[k] = trimma(ed.person[k]); });
      RAD_FALT.forEach(function (k) { o[k] = trimma(x.f[k]); });
      Object.keys(bildVärden).forEach(function (k) { o[k] = bildVärden[k]; });
      o.datum_precision = precisionFor(o.datum_tilldelning);
      tillagda.push(o);
      ändringar.push({ id: o.id, typ: "tillagd", fält: [] });
    });
    if (bildFält.length && orig.length) {
      var e = ändringar.filter(function (a) { return a.id === orig[0].id; })[0];
      if (!e) { e = { id: orig[0].id, typ: "ändrad", fält: [] }; ändringar.push(e); }
      bildFält.forEach(function (x) { e.fält.push(x); });
    }
    var bilderÄndrad = JSON.stringify(bilder) !== JSON.stringify(S.bilder);
    return { tabell: ny.concat(tillagda), ändringar: ändringar, bilder: bilderÄndrad ? bilder : null, filer: filer };
  }

  function valideraBild(ed, res) {
    var fel = [], b = ed.bild;
    if (!b) return fel;
    var p = function (c, m) { if (!c) fel.push({ id: "Bild", msg: m }); };
    p(b.fil || b.ny, "ladda upp en bildfil");
    p(LICENSER.hasOwnProperty(b.licens), "licensen måste vara en fri licens");
    p(b.upphov.trim() !== "", "upphovsperson saknas");
    p(b.bildtext.trim() !== "", "bildtext saknas");
    p(b.anvandning === "sköld" || b.anvandning === "utdelning", "ogiltig användning");
    p(b.commons_sida === "" || /^https?:\/\//.test(b.commons_sida), "källsidan måste vara en http(s)-adress eller tom");
    p(b.licens_url === "" || /^https?:\/\//.test(b.licens_url), "licens-URL måste vara en http(s)-adress eller tom");
    return fel;
  }

  function b64ToText(b64) {
    var bin = atob(b64.replace(/\s/g, ""));
    var bytes = new Uint8Array(bin.length);
    for (var i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
    return new TextDecoder("utf-8").decode(bytes);
  }

  function gh(token, method, path, body) {
    return root.fetch(API + path, {
      method: method,
      cache: "no-store",
      headers: {
        "Authorization": "Bearer " + token,
        "Accept": "application/vnd.github+json",
        "X-GitHub-Api-Version": "2022-11-28",
        "Content-Type": "application/json"
      },
      body: body ? JSON.stringify(body) : undefined
    }).catch(function () {
      var e = new Error("Kunde inte nå GitHub. Kontrollera nätverket och försök igen.");
      e.nät = true;
      throw e;
    }).then(function (res) {
      return res.json().catch(function () { return {}; }).then(function (j) {
        if (!res.ok) {
          var e = new Error(res.status >= 500 ? "GitHub svarade med serverfel (HTTP " + res.status + "). Försök igen om en stund." : (j && j.message) || ("HTTP " + res.status));
          e.status = res.status;
          throw e;
        }
        return j;
      });
    });
  }

  function hämtaFil(token, path, ref) {
    return gh(token, "GET", "/repos/" + REPO + "/contents/" + path + "?ref=" + encodeURIComponent(ref || BRANCH)).then(function (j) {
      return { text: b64ToText(j.content), sha: j.sha };
    });
  }

  function läsData(token) {
    return Promise.all([hämtaFil(token, PATH_T), hämtaFil(token, PATH_LOG), hämtaFil(token, PATH_G), hämtaFil(token, PATH_B)]).then(function (r) {
      var t = csvToObjects(r[0].text), b = csvToObjects(r[3].text);
      return {
        header: t.header, rows: t.rows, shaT: r[0].sha,
        loggText: r[1].text, shaLog: r[1].sha,
        grader: csvToObjects(r[2].text).rows,
        bilderHeader: b.header, bilder: b.rows, shaB: r[3].sha
      };
    });
  }

  function spara(S, tabell, ändringar, meddelande, extra) {
    extra = extra || {};
    var token = S.token;
    var idag = idagIso();
    var nyaLoggrader = ändringar.map(function (a) { return { datum: idag, id: a.id, andring: a.typ }; });
    var tText = toCsv(S.header, tabell);
    var logg = S.loggText.endsWith("\n") ? S.loggText : S.loggText + "\n";
    var loggText = logg + csvLines(["datum", "id", "andring"], nyaLoggrader);
    var bText = extra.bilder ? toCsv(S.bilderHeader, extra.bilder) : null;
    var base = "/repos/" + REPO;
    return gh(token, "GET", base + "/git/ref/heads/" + BRANCH).then(function (ref) {
      var headSha = ref.object.sha;
      return Promise.all([hämtaFil(token, PATH_T, headSha), hämtaFil(token, PATH_LOG, headSha), hämtaFil(token, PATH_B, headSha)]).then(function (nu) {
        if (nu[0].sha !== S.shaT || nu[1].sha !== S.shaLog || nu[2].sha !== S.shaB) {
          var e = new Error("Datan har ändrats på GitHub sedan sidan lästes in. Ladda om sidan och gör om ändringen.");
          e.stale = true;
          throw e;
        }
        return headSha;
      });
    }).then(function (headSha) {
      return gh(token, "GET", base + "/git/commits/" + headSha).then(function (c) {
        var jobb = [
          { path: PATH_T, body: { content: tText, encoding: "utf-8" } },
          { path: PATH_LOG, body: { content: loggText, encoding: "utf-8" } }
        ];
        if (bText != null) jobb.push({ path: PATH_B, body: { content: bText, encoding: "utf-8" } });
        (extra.filer || []).forEach(function (f) { jobb.push({ path: f.path, body: { content: f.base64, encoding: "base64" } }); });
        return Promise.all(jobb.map(function (j) { return gh(token, "POST", base + "/git/blobs", j.body); })).then(function (b) {
          var shas = {};
          jobb.forEach(function (j, i) { shas[j.path] = b[i].sha; });
          return gh(token, "POST", base + "/git/trees", {
            base_tree: c.tree.sha,
            tree: jobb.map(function (j) { return { path: j.path, mode: "100644", type: "blob", sha: shas[j.path] }; })
          }).then(function (tree) {
            return gh(token, "POST", base + "/git/commits", { message: meddelande, tree: tree.sha, parents: [headSha] });
          }).then(function (commit) {
            return gh(token, "PATCH", base + "/git/refs/heads/" + BRANCH, { sha: commit.sha, force: false }).then(function () {
              return { commit: commit.sha, shaT: shas[PATH_T], shaLog: shas[PATH_LOG], shaB: shas[PATH_B] || S.shaB, loggText: loggText };
            });
          });
        });
      });
    });
  }

  var lib = {
    parseCsv: parseCsv, toCsv: toCsv, csvToObjects: csvToObjects, csvLines: csvLines,
    validera: validera, valideraBild: valideraBild, nyBild: nyBild, bildFrån: bildFrån, beräknaÄndringar: beräknaÄndringar, precisionFor: precisionFor,
    datumFel: datumFel, slugify: slugify, idagIso: idagIso, nastaId: nastaId, spara: spara, läsData: läsData,
    PERSON_FALT: PERSON_FALT, RAD_FALT: RAD_FALT
  };

  if (typeof module !== "undefined" && module.exports) module.exports = lib;
  root.Redigera = lib;

  if (typeof document === "undefined") return;

  function h(tag, attrs) {
    var el = document.createElement(tag);
    if (attrs) Object.keys(attrs).forEach(function (k) {
      var v = attrs[k];
      if (v == null || v === false) return;
      if (k === "class") el.className = v;
      else if (k.slice(0, 2) === "on") el.addEventListener(k.slice(2), v);
      else if (k === "value") el.value = v;
      else el.setAttribute(k, v === true ? "" : v);
    });
    for (var i = 2; i < arguments.length; i++) append(el, arguments[i]);
    return el;
  }
  function ersätt(el) {
    el.replaceChildren();
    for (var i = 1; i < arguments.length; i++) append(el, arguments[i]);
  }
  function append(el, c) {
    if (c == null || c === false) return;
    if (Array.isArray(c)) c.forEach(function (x) { append(el, x); });
    else el.appendChild(typeof c === "string" ? document.createTextNode(c) : c);
  }

  var mount = document.getElementById("redigera");
  if (!mount) return;

  var S = { token: null, user: null, ed: null, filter: "" };

  function sparaToken(t) {
    try { if (t) localStorage.setItem(TOKEN_KEY, t); else localStorage.removeItem(TOKEN_KEY); } catch (e) {}
    document.documentElement.classList.toggle("redigerare", !!t);
  }
  root.addEventListener("storage", function (e) { if (e.key === TOKEN_KEY) document.documentElement.classList.toggle("redigerare", !!e.newValue); });
  function hämtaToken() { try { return localStorage.getItem(TOKEN_KEY); } catch (e) { return null; } }

  function visaLogin(fel) {
    var inp = h("input", { type: "password", id: "rd-token", autocomplete: "off", placeholder: "github_pat_…", class: "form-control" });
    ersätt(mount, 
      h("div", { class: "rd-kort" },
        h("h2", null, "Logga in med GitHub"),
        h("p", null, "Webbplatsen ligger på GitHub Pages och saknar egen server, så inloggningen sker med en personlig åtkomsttoken. Den lagras bara i den här webbläsaren och används endast mot GitHub."),
        h("ol", null,
          h("li", null, h("a", { href: "https://github.com/settings/personal-access-tokens/new?name=Kungliga+ordnar&description=Redigera+data+via+webbplatsen&expires_in=90&contents=write", target: "_blank", rel: "noopener" }, "Skapa en fine-grained token"), " (länken fyller i namn, 90 dagar och behörigheten Contents: Read and write)."),
          h("li", null, "Välj under Repository access: ", h("strong", null, "Only select repositories"), " och därefter ", h("code", null, "kungliga-ordnar"), "."),
          h("li", null, "Klistra in token här:")),
        fel ? h("p", { class: "rd-fel" }, fel) : null,
        h("label", { for: "rd-token", class: "rd-etikett" }, "Personlig åtkomsttoken"),
        h("div", { class: "rd-rad" }, inp,
          h("button", { class: "btn btn-primary", onclick: function () { loggaIn(inp.value.trim()); } }, "Logga in"))));
  }

  function loggaIn(token) {
    if (!token) return;
    ersätt(mount, h("p", null, "Kontrollerar token…"));
    Promise.all([gh(token, "GET", "/user"), gh(token, "GET", "/repos/" + REPO)]).then(function (r) {
      if (r[0].login.toLowerCase() !== OWNER.toLowerCase()) throw Object.assign(new Error("Bara " + OWNER + " har tillgång till redigeringen."), { tokenFel: true });
      if (!r[1].permissions || !r[1].permissions.push) throw Object.assign(new Error("Token saknar skrivrätt till " + REPO + "."), { tokenFel: true });
      sparaToken(token);
      S.token = token;
      S.user = r[0].login;
      return laddaData();
    }).catch(function (e) {
      if (e.status === 401 || e.tokenFel) {
        sparaToken(null);
        visaLogin(e.status === 401 ? "GitHub avvisade token." : e.message);
      } else {
        visaFel(e, token);
      }
    });
  }

  function visaFel(e, token) {
    ersätt(mount,
      h("div", { class: "rd-kort" },
        h("h2", null, "Redigeraren kunde inte startas"),
        h("p", { class: "rd-fel", role: "alert" }, e.message),
        h("div", { class: "rd-rad" },
          h("button", { class: "btn btn-primary", onclick: function () { loggaIn(token); } }, "Försök igen"),
          h("button", { class: "btn btn-outline-secondary", onclick: function () { sparaToken(null); visaLogin(); } }, "Logga in med en annan token"))));
  }

  function personFrånHash() {
    var m = /^#person=(.+)$/.exec(location.hash);
    if (!m) return null;
    try { return decodeURIComponent(m[1]); } catch (_) { return null; }
  }

  function laddaData() {
    ersätt(mount, h("p", null, "Läser in data från GitHub…"));
    return läsData(S.token).then(function (d) {
      Object.assign(S, d);
      S.ed = null;
      var pid = personFrånHash();
      if (pid && S.rows.some(function (r) { return r.person_id === pid; })) väljPerson(pid);
      else if (location.hash === "#ny") nyPerson();
      else visaLista();
    });
  }

  function logout() {
    if (harOsparat() && !root.confirm("Du har osparade ändringar. Vill du logga ut utan att spara?")) return;
    S.ed = null;
    sparaToken(null);
    S.token = null;
    visaLogin();
  }

  function topp() {
    return h("div", { class: "rd-topp" },
      h("span", null, "Inloggad som ", h("strong", null, S.user)),
      h("button", { class: "btn btn-sm btn-outline-secondary", onclick: logout }, "Logga ut"));
  }

  function personer() {
    var m = {};
    S.rows.forEach(function (r) {
      var p = m[r.person_id] || (m[r.person_id] = { pid: r.person_id, namn: r.namn, dold: !!r["dödsdatum"], n: 0 });
      p.n++;
    });
    return Object.keys(m).map(function (k) { return m[k]; }).sort(function (a, b) { return a.namn.localeCompare(b.namn, "sv"); });
  }

  function visaLista() {
    var lista = h("ul", { class: "rd-lista" });
    var sök = h("input", { type: "search", class: "form-control", placeholder: "Sök person…", value: S.filter, autofocus: true });
    function fyll() {
      S.filter = sök.value;
      var q = nyckel(S.filter);
      var träff = personer().filter(function (p) { return !q || nyckel(p.namn).indexOf(q) >= 0; });
      var poster = träff.slice(0, 40).map(function (p) {
        return h("li", null, h("button", { class: "rd-länk", onclick: function () { väljPerson(p.pid); } },
          p.namn, p.dold ? h("span", { class: "kalltyp" }, "dold (avliden)") : null));
      });
      if (träff.length > 40) poster.push(h("li", { class: "saknas" }, "… och " + (träff.length - 40) + " till, förfina sökningen"));
      ersätt(lista, poster);
    }
    sök.addEventListener("input", fyll);
    ersätt(mount, topp(),
      h("h2", null, "Välj person"),
      h("div", { class: "rd-rad" }, sök, h("button", { class: "btn btn-primary", onclick: nyPerson }, "Ny person")),
      lista);
    fyll();
  }

  function radFrån(r) {
    var f = {};
    RAD_FALT.forEach(function (k) { f[k] = r[k]; });
    return { id: r.id, f: f };
  }

  function tomRad() {
    var f = {};
    RAD_FALT.forEach(function (k) { f[k] = ""; });
    f.orden = ORDNAR[0];
    f["källtyp"] = "kungahuset";
    f["källa_hämtad"] = idagIso();
    return { id: null, f: f };
  }

  function väljPerson(pid) {
    var rows = S.rows.filter(function (r) { return r.person_id === pid; });
    var person = {};
    PERSON_FALT.forEach(function (k) { person[k] = rows[0][k]; });
    S.ed = { pid: pid, ny: false, person: person, rader: rows.map(radFrån), bild: bildFrån(S, rows) };
    visaForm();
  }

  function nyPerson() {
    var person = {};
    PERSON_FALT.forEach(function (k) { person[k] = ""; });
    person.kategori = "utländsk";
    person.kunglig = "FALSE";
    S.ed = { pid: null, ny: true, person: person, rader: [tomRad()], bild: null };
    S.ed.start = ögonblick(S.ed);
    visaForm();
  }

  function fält(etikett, el, hjälp) {
    return h("label", { class: "rd-fält" }, h("span", { class: "rd-etikett" }, etikett), el, hjälp ? h("span", { class: "rd-hjälp" }, hjälp) : null);
  }

  function textIn(obj, k, extra) {
    return h("input", Object.assign({ type: "text", class: "form-control", value: obj[k], onInput: null }, extra || {}));
  }

  function bind(el, obj, k) {
    el.addEventListener("input", function () { obj[k] = el.value; uppdateraGranskning(); });
    return el;
  }

  function select(obj, k, val, options, label) {
    var el = h("select", { class: "form-select" }, options.map(function (o) {
      var v = Array.isArray(o) ? o[0] : o, t = Array.isArray(o) ? o[1] : o;
      return h("option", { value: v, selected: v === val }, t || "(tomt)");
    }));
    el.addEventListener("change", function () { obj[k] = el.value; if (label) label(); uppdateraGranskning(); });
    return el;
  }

  function läsBild(file) {
    return new Promise(function (resolve, reject) {
      if (!/^image\/(png|jpeg|webp)$/.test(file.type)) return reject(new Error("Välj en PNG-, JPEG- eller WebP-bild."));
      if (file.size > 15 * 1024 * 1024) return reject(new Error("Bilden är större än 15 MB."));
      var url = URL.createObjectURL(file);
      var img = new Image();
      img.onload = function () {
        var w = img.naturalWidth, hh = img.naturalHeight, k = Math.min(1, MAX_SIDA / Math.max(w, hh));
        var cw = Math.round(w * k), ch = Math.round(hh * k);
        var cv = document.createElement("canvas");
        cv.width = cw; cv.height = ch;
        cv.getContext("2d").drawImage(img, 0, 0, cw, ch);
        var png = file.type !== "image/jpeg";
        var dataUrl = cv.toDataURL(png ? "image/png" : "image/jpeg", 0.88);
        URL.revokeObjectURL(url);
        resolve({ base64: dataUrl.split(",")[1], dataUrl: dataUrl, ext: png ? "png" : "jpg", bredd: cw, hojd: ch, orig: w + "x" + hh });
      };
      img.onerror = function () { reject(new Error("Kunde inte läsa bilden.")); };
      img.src = url;
    });
  }

  function bygg_bildkort() {
    var ed = S.ed, b = ed.bild;
    if (!b) {
      return h("section", { class: "rd-kort" }, h("h3", null, "Bild"),
        h("p", { class: "saknas" }, "Ingen bild."),
        h("button", { class: "btn btn-outline-primary", onclick: function () { ed.bild = nyBild(); visaForm(); } }, "Lägg till bild"));
    }
    var förhandsvisning = h("img", { class: "rd-bild", alt: "", src: b.ny ? b.ny.dataUrl : (b.fil ? "img/" + b.fil : "") });
    var status = h("span", { class: "rd-hjälp" }, b.ny ? "Ny bild vald (" + b.ny.orig + " px, sparas som " + b.ny.bredd + "x" + b.ny.hojd + ")." : (b.fil ? b.fil : "Ingen fil vald."));
    var fil = h("input", { type: "file", accept: "image/png,image/jpeg,image/webp", class: "form-control" });
    fil.addEventListener("change", function () {
      if (!fil.files[0]) return;
      läsBild(fil.files[0]).then(function (r) {
        b.ny = r;
        förhandsvisning.src = r.dataUrl;
        status.textContent = "Ny bild vald (" + r.orig + " px, sparas som " + r.bredd + "x" + r.hojd + ").";
        uppdateraGranskning();
      }).catch(function (e) { status.textContent = e.message; });
    });
    var licensUrl = bind(textIn(b, "licens_url"), b, "licens_url");
    var licens = select(b, "licens", b.licens, Object.keys(LICENSER), function () { b.licens_url = LICENSER[b.licens]; licensUrl.value = b.licens_url; });
    return h("section", { class: "rd-kort" },
      h("h3", null, "Bild",
        h("button", { class: "btn btn-sm btn-outline-danger rd-höger", onclick: function () { ed.bild = null; visaForm(); } }, "Ta bort bild")),
      h("div", { class: "rd-bildrad" }, förhandsvisning,
        h("div", null, fält("Bildfil", fil, null), status)),
      h("div", { class: "rd-grid" },
        fält("Användning", select(b, "anvandning", b.anvandning, [["sköld", "Vapensköld"], ["utdelning", "Utdelningsfoto"]])),
        fält("Licens", licens, "Endast fria licenser."),
        fält("Upphovsperson", bind(textIn(b, "upphov"), b, "upphov")),
        fält("Källa (text)", bind(textIn(b, "kalla"), b, "kalla"), "T.ex. Own work."),
        fält("Källsida (URL)", bind(textIn(b, "commons_sida", { placeholder: "https://commons.wikimedia.org/wiki/File:…" }), b, "commons_sida")),
        fält("Licens-URL", licensUrl),
        fält("Bildtext", bind(textIn(b, "bildtext"), b, "bildtext"))));
  }

  function visaForm() {
    var ed = S.ed, p = ed.person;
    var länder = Array.from(new Set(S.rows.map(function (r) { return r.land; }).filter(Boolean))).sort(function (a, b) { return a.localeCompare(b, "sv"); });
    var landlista = h("datalist", { id: "rd-länder" }, länder.map(function (l) { return h("option", { value: l }); }));
    var kung = h("input", { type: "checkbox", checked: p.kunglig === "TRUE" });
    kung.addEventListener("change", function () { p.kunglig = kung.checked ? "TRUE" : "FALSE"; uppdateraGranskning(); });

    var personKort = h("section", { class: "rd-kort" },
      h("h3", null, ed.ny ? "Ny person" : p.namn),
      h("div", { class: "rd-grid" },
        fält("Namn", bind(textIn(p, "namn"), p, "namn")),
        fält("Kön", select(p, "kön", p["kön"], ["", "man", "kvinna"])),
        fält("Kategori", select(p, "kategori", p.kategori, ["svensk", "utländsk"])),
        fält("Land", bind(textIn(p, "land", { list: "rd-länder" }), p, "land"), "Tomt om okänt. Svensk = Sverige."),
        fält("Wikidata-id", bind(textIn(p, "wikidata_qid", { placeholder: "Q12345" }), p, "wikidata_qid")),
        fält("Dödsdatum", bind(textIn(p, "dödsdatum", { placeholder: "ÅÅÅÅ-MM-DD" }), p, "dödsdatum"), "Ifyllt döljer personen på webbplatsen. Raden raderas aldrig."),
        h("label", { class: "rd-fält rd-kryss" }, kung, h("span", null, "Kunglig"))));

    var radKort = ed.rader.map(function (rad, i) {
      var f = rad.f;
      var gradEl = h("span", null);
      function ritaGrad() {
        var opt = S.grader.filter(function (g) { return g.orden === f.orden; }).map(function (g) { return g.grad; });
        if (opt.indexOf(f.grad) < 0) f.grad = "";
        var sel = select(f, "grad", f.grad, [""].concat(opt));
        ersätt(gradEl, sel);
      }
      ritaGrad();
      var motivTyp = select(f, "motivering_typ", f.motivering_typ, [["", "(ingen)"], "officiell", "statsbesök"]);
      return h("section", { class: "rd-kort" },
        h("h3", null, rad.id ? rad.id : "Ny tilldelning",
          !rad.id && ed.rader.length > 1 ? h("button", { class: "btn btn-sm btn-outline-danger rd-höger", onclick: function () { ed.rader.splice(i, 1); visaForm(); } }, "Ta bort") : null),
        h("div", { class: "rd-grid" },
          fält("Orden", select(f, "orden", f.orden, ORDNAR, ritaGrad)),
          fält("Grad", gradEl),
          fält("Yrke/titel", bind(textIn(f, "yrke_titel"), f, "yrke_titel")),
          fält("Datum", bind(textIn(f, "datum_tilldelning", { placeholder: "ÅÅÅÅ-MM-DD, ÅÅÅÅ-MM eller ÅÅÅÅ" }), f, "datum_tilldelning"), "Tomt om okänt."),
          fält("Motivering", bind(textIn(f, "motivering"), f, "motivering")),
          fält("Motiveringstyp", motivTyp),
          fält("Statsbesök", bind(textIn(f, "statsbesök"), f, "statsbesök")),
          fält("Källa (URL)", bind(textIn(f, "källa_url", { placeholder: "https://…" }), f, "källa_url")),
          fält("Källtyp", select(f, "källtyp", f["källtyp"], Object.keys(KALLTYPER).map(function (k) { return [k, KALLTYPER[k]]; }))),
          fält("Källa hämtad", bind(textIn(f, "källa_hämtad", { placeholder: "ÅÅÅÅ-MM-DD" }), f, "källa_hämtad")),
          fält("Flagga", bind(textIn(f, "flagga"), f, "flagga"), "Intern notering, tomt tar bort flaggan.")));
    });

    var bildKort = bygg_bildkort();
    var granskning = h("section", { class: "rd-kort", id: "rd-granskning" });
    ersätt(mount, topp(),
      h("p", null, h("button", { class: "rd-länk", onclick: function () { if (harOsparat() && !root.confirm("Du har osparade ändringar. Vill du lämna utan att spara?")) return; S.ed = null; location.hash = ""; visaLista(); } }, "← Tillbaka till listan"),
        !ed.ny ? [" · ", h("a", { href: "person/" + ed.pid + ".html" }, "Visa personsidan")] : null),
      landlista, personKort, bildKort, radKort,
      h("p", null, h("button", { class: "btn btn-outline-primary", onclick: function () { ed.rader.push(tomRad()); visaForm(); } }, "Lägg till tilldelning")),
      granskning);
    uppdateraGranskning();
  }

  function ögonblick(ed) {
    return JSON.stringify({ person: ed.person, rader: ed.rader, bild: ed.bild });
  }

  function harOsparat() {
    var ed = S.ed;
    if (!ed) return false;
    if (ed.ny) return ögonblick(ed) !== ed.start;
    return !!(ed.pid && beräknaÄndringar(S, ed).ändringar.length);
  }
  root.addEventListener("beforeunload", function (e) { if (harOsparat()) { e.preventDefault(); e.returnValue = ""; } });
  var senastFel = [];
  function uppdateraGranskning() {
    var ed = S.ed, box = document.getElementById("rd-granskning");
    if (!box || !ed) return;
    if (ed.ny && ed.person.namn.trim()) {
      if (!ed.pid || ed.pidAuto) {
        var bas = slugify(ed.person.namn) || "person", id = bas, n = 2;
        while (S.rows.some(function (r) { return r.person_id === id; })) id = bas + "-" + n++;
        ed.pid = id;
        ed.pidAuto = true;
      }
    }
    if (!ed.pid) {
      ersätt(box, h("h3", null, "Ändringar"), h("p", { class: "saknas" }, "Fyll i ett namn."));
      return;
    }
    var res = beräknaÄndringar(S, ed);
    var rörda = res.ändringar.map(function (a) { return a.id; });
    var fel = validera(res.tabell, S.grader, rörda, S.rows).concat(valideraBild(ed, res));
    senastFel = fel;
    var delar = res.ändringar.map(function (a) {
      return h("li", null, h("strong", null, a.id), a.person ? " (" + a.person + ", delad bild)" : "", " ", a.typ === "tillagd" ? "tillagd" : "ändrad",
        a.fält.length ? h("ul", null, a.fält.map(function (x) {
          return h("li", null, h("code", null, x.k), ": ", h("del", null, x.från || "(tomt)"), " → ", h("ins", null, x.till || "(tomt)"));
        })) : null);
    });
    var msg = h("input", { type: "text", class: "form-control", id: "rd-msg", value: ed.meddelande == null ? (ed.ny ? "Ny person via webben: " : "Redigering via webben: ") + ed.person.namn : ed.meddelande });
    msg.addEventListener("input", function () { ed.meddelande = msg.value; });
    var knapp = h("button", { class: "btn btn-primary", disabled: !res.ändringar.length || fel.length > 0, onclick: function () { sparaNu(res, msg.value, knapp, status); } }, "Spara på GitHub");
    var status = h("p", { class: "rd-status", role: "status" });
    ersätt(box, 
      h("h3", null, "Ändringar"),
      res.ändringar.length ? h("ul", null, delar) : h("p", { class: "saknas" }, "Inga ändringar än."),
      fel.length ? h("ul", { class: "rd-fel" }, fel.map(function (x) { return h("li", null, x.id + ": " + x.msg); })) : null,
      fält("Commit-meddelande", msg),
      knapp, status);
  }

  function sparaNu(res, meddelande, knapp, status) {
    knapp.disabled = true;
    status.textContent = "Sparar på GitHub…";
    spara(S, res.tabell, res.ändringar, meddelande, { bilder: res.bilder, filer: res.filer }).then(function (r) {
      S.rows = res.tabell;
      if (res.bilder) S.bilder = res.bilder;
      S.shaB = r.shaB;
      S.shaT = r.shaT;
      S.shaLog = r.shaLog;
      S.loggText = r.loggText;
      var pid = S.ed.pid;
      väljPerson(pid);
      var box = document.getElementById("rd-granskning");
      box.insertBefore(h("p", { class: "verifierad" }, "Sparat. ",
        h("a", { href: "https://github.com/" + REPO + "/commit/" + r.commit, target: "_blank", rel: "noopener" }, "Visa commit"),
        ". Webbplatsen byggs om, vilket tar några minuter. ",
        h("a", { href: "https://github.com/" + REPO + "/actions", target: "_blank", rel: "noopener" }, "Följ bygget"), "."), box.firstChild);
    }).catch(function (e) {
      knapp.disabled = false;
      status.textContent = "Kunde inte spara: " + e.message;
      status.className = "rd-status rd-fel";
    });
  }

  var token = hämtaToken();
  if (token) loggaIn(token); else visaLogin();
})(typeof window !== "undefined" ? window : globalThis);
