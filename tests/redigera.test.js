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
// A ref change between snapshot and update must never be based on the later tree.
let calls = [], blob = 0;
global.fetch = async (url, opts) => {
 const u = new URL(url); calls.push([opts.method,u.pathname,u.search]);
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
