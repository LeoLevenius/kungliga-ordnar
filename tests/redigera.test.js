const assert = require('node:assert/strict');
const fs = require('node:fs');
const lib = require('../redigera.js');
const read = f => lib.csvToObjects(fs.readFileSync('data/' + f, 'utf8'));
const t = read('tilldelningar.csv'), b = read('bilder.csv'), g = read('grader.csv');
const S = {header: t.header, rows: t.rows, bilder: b.rows, bilderHeader: b.header};
function editor(pid) {
  const rows = S.rows.filter(r => r.person_id === pid);
  return {pid, person: Object.fromEntries(lib.PERSON_FALT.map(k => [k, rows[0][k]])),
    rader: rows.map(r => ({id:r.id, f:Object.fromEntries(lib.RAD_FALT.map(k => [k,r[k]]))})), bild:lib.bildFrån(S, rows)};
}
assert.ok(lib.datumFel('2026-99-99'));
assert.ok(lib.datumFel('9999-12'));
assert.ok(lib.datumFel('2026-02-31'));
const ed = editor('prinsessan-margaretha');
const shared = 'img/' + ed.bild.fil;
ed.bild = null;
const result = lib.beräknaÄndringar(S, ed);
assert.ok(result.tabell.some(r => r.person_id !== ed.pid && r.bild_url === shared));
assert.ok((result.bilder || S.bilder).some(r => 'img/' + r.fil === shared));
const ed2 = editor('anne-lhuillier-wahlstrom');
ed2.person['dödsdatum'] = '2026-99-99';
let change = lib.beräknaÄndringar(S, ed2);
assert.ok(lib.validera(change.tabell, g.rows, change.ändringar.map(x=>x.id)).some(x=>x.msg.includes('dödsdatum')));
ed2.person['dödsdatum'] = '';
ed2.rader[0].f['källa_url'] = 'https://example.org/" onmouseover="x';
change = lib.beräknaÄndringar(S, ed2);
assert.ok(lib.validera(change.tabell, g.rows, change.ändringar.map(x=>x.id)).some(x=>x.msg.includes('källa_url')));
assert.throws(() => lib.csvToObjects('a,b\n1,"2\n'), /citattecken/);
assert.throws(() => lib.csvToObjects('a,b\n1,2,3\n'), /fel antal fält på rad 2/);
assert.throws(() => lib.csvToObjects('a,b\n1,2\n\n3,4\n'), /fel antal fält på rad 3/);
assert.deepEqual(lib.csvToObjects('a,b\n1,2\n\n').rows, [{a: '1', b: '2'}]);
for (const f of ['tilldelningar.csv', 'bilder.csv', 'grader.csv', 'andringslogg.csv']) {
  const x = lib.csvToObjects(fs.readFileSync('data/' + f, 'utf8'));
  if (f !== 'grader.csv' && f !== 'andringslogg.csv') assert.equal(lib.toCsv(x.header, x.rows), fs.readFileSync('data/' + f, 'utf8').replace(/^﻿/, ''), f + ' rundtur');
}
const slugfall = lib.csvToObjects(fs.readFileSync('tests/slug-fall.csv', 'utf8')).rows;
slugfall.forEach(r => assert.equal(lib.slugify(r.namn), r.slug, r.namn));
assert.equal(lib.slugify('Ax:son'), 'axson');
assert.equal(lib.idagIso(), new Date().toISOString().slice(0, 10));
assert.equal(lib.datumFel(lib.idagIso()), null);
assert.ok(lib.datumFel(new Date(Date.now() + 86400000).toISOString().slice(0, 10)));
{
  const edL = editor('anne-lhuillier-wahlstrom');
  edL.person.land = 'Sverige ';
  edL.person.kategori = 'utländsk';
  const chL = lib.beräknaÄndringar(S, edL);
  assert.ok(lib.validera(chL.tabell, g.rows, chL.ändringar.map(x=>x.id), S.rows).some(x=>x.msg.includes('kategori och land')));
  assert.ok(chL.tabell.every(r => r.land === r.land.trim()));
}
{
  const tom = S.rows.filter(r => r.grad === '');
  assert.ok(tom.length >= 1);
  const edG = editor(tom[0].person_id);
  edG.rader.forEach(r => { if (r.id === tom[0].id) r.f.yrke_titel = 'Titel ' + Date.now(); });
  let chG = lib.beräknaÄndringar(S, edG);
  assert.deepEqual(lib.validera(chG.tabell, g.rows, chG.ändringar.map(x=>x.id), S.rows), []);
  edG.rader.push({id: null, f: Object.assign({}, edG.rader[0].f, {grad: ''})});
  chG = lib.beräknaÄndringar(S, edG);
  assert.ok(lib.validera(chG.tabell, g.rows, chG.ändringar.map(x=>x.id), S.rows).some(x=>x.msg.includes('grad måste anges')));
  assert.ok(lib.validera(chG.tabell, g.rows, chG.ändringar.map(x=>x.id)).some(x=>x.msg.includes('grad måste anges')));
}
{
  const synonym = (o, gr) => g.rows.find(x => x.orden === o && x.grad !== gr && x.rang === g.rows.find(y => y.orden === o && y.grad === gr).rang);
  const rad = S.rows.find(r => r.grad && r.datum_tilldelning && synonym(r.orden, r.grad));
  const edD = editor(rad.person_id);
  edD.rader.push({id: null, f: Object.assign({}, edD.rader.find(x => x.id === rad.id).f, {grad: synonym(rad.orden, rad.grad).grad})});
  const chD = lib.beräknaÄndringar(S, edD);
  assert.ok(lib.validera(chD.tabell, g.rows, chD.ändringar.map(x=>x.id), S.rows).some(x=>x.msg.includes('dubblett')));
}
{
  const edB = editor('prinsessan-margaretha');
  const fil = edB.bild.fil;
  const andra = S.rows.filter(r => r.person_id !== edB.pid && r.bild_url === 'img/' + fil);
  assert.ok(andra.length >= 1);
  edB.bild.upphov = 'Ny upphovsperson'; edB.bild.licens = 'CC BY 4.0';
  const chB = lib.beräknaÄndringar(S, edB);
  andra.forEach(a => {
    const r = chB.tabell.find(x => x.id === a.id);
    assert.equal(r.bild_upphov, 'Ny upphovsperson'); assert.equal(r.bild_licens, 'CC BY 4.0');
    assert.ok(chB.ändringar.some(x => x.id === a.id && x.person === a.person_id && x.fält.some(f => f.k === 'bild_upphov')));
  });
  assert.ok(chB.tabell.filter(r => r.bild_url === 'img/' + fil).every(r => r.bild_upphov === 'Ny upphovsperson'));
}
// A ref change between snapshot and update must never be based on the later tree.
let calls = [], blob = 0;
global.fetch = async (url, opts) => {
 const u = new URL(url); calls.push([opts.method,u.pathname,u.search]); assert.equal(opts.cache, 'no-store');
 let response;
 if (u.pathname.endsWith('/git/ref/heads/main')) response = {object:{sha:'HEAD-A'}};
 else if (u.pathname.includes('/contents/')) response = {sha:u.pathname.endsWith('tilldelningar.csv')?'A':u.pathname.endsWith('andringslogg.csv')?'B':'C',content:Buffer.from('').toString('base64')};
 else if (u.pathname.endsWith('/git/commits/HEAD-A')) response = {tree:{sha:'TREE-A'}};
 else if (u.pathname.endsWith('/git/blobs')) response = {sha:'blob'+(++blob)};
 else if (u.pathname.endsWith('/git/trees')) response = {sha:'tree'};
 else if (u.pathname.endsWith('/git/commits')) {assert.deepEqual(JSON.parse(opts.body).parents,['HEAD-A']);response={sha:'commit'};}
 else if (u.pathname.endsWith('/git/refs/heads/main')) return {ok:false,status:422,json:async()=>({message:'non-fast-forward'})};
 else throw Error('unexpected API call ' + u.pathname);
 return {ok:true,json:async()=>response};
};
(async()=>{
 await assert.rejects(lib.spara({token:'dummy',header:['id'],shaT:'A',shaLog:'B',shaB:'C',loggText:'datum,id,andring\n'},[{id:'T'}],[{id:'T',typ:'ändrad'}],'test'), /non-fast-forward/);
 assert.ok(calls.filter(c=>c[1].includes('/contents/')).every(c=>c[2].includes('ref=HEAD-A')));
 console.log('Redigerarens säkerhets- och datatester godkända.');
})().catch(e=>{console.error(e);process.exitCode=1});
