from pathlib import Path
import json,csv,collections
R=Path(__file__).resolve().parent;ROOT=R.parent
rows=[];visits=[];accepted=[];packets={p.stem:json.loads(p.read_text()) for p in (ROOT/'country').glob('*.json')}
for iso,p in sorted(packets.items()):
 for src in p['sources']:
  obs=[o for o in p['observations'] if o['source_id']==src['source_id']]
  series=[s for s in p['series'] if s['series_id'] in {o['series_id'] for o in obs}]
  years=sorted({o['year'] for o in obs if o.get('year')})
  rows.append(dict(iso2=iso,country=p['country'],source_id=src['source_id'],title=src['title'],publisher=src.get('publisher',''),url=src['url'],record_origin='original country packet',checked_date=src.get('retrieved_date','2026-09-09'),access_status=src.get('access_status',''),recorded_years=';'.join(map(str,years)),undated_observations=sum(o.get('year') is None for o in obs),series_ids=';'.join(s['series_id'] for s in series),units='; '.join(dict.fromkeys(s['unit'] for s in series)),methodology=' | '.join(dict.fromkeys(s.get('method','') for s in series)),assessment=' | '.join(dict.fromkeys(s.get('comparison_note','') for s in series)) or src.get('notes',''),trend_eligible_series=';'.join(s['series_id'] for s in series if s.get('trend_eligible')),source_note=src.get('notes',''),backward_forward_status='Original coverage imported; archive extension not yet reviewed in this pass.'))
 for v in p.get('search_log',[]):
  visits.append(dict(iso2=iso,country=p['country'],checked_date='2026-09-09',url=v.get('url',''),route=v.get('route',''),query=v.get('query',''),outcome=v.get('outcome',''),origin='original search log'))
# Later methodological reviews override eligibility without rewriting original packets.
overrides={p.parent.name:json.loads(p.read_text()) for p in (ROOT/'research-extension-2026-09-19').glob('*/review-overrides.json')}
for row in rows:
 review=overrides.get(row['iso2'])
 if review and set(row['series_ids'].split(';')) & set(review.get('suspended_series_ids',[])):
  row['assessment']=review['summary']+' Original assessment: '+row['assessment']
  row['trend_eligible_series']=''
  row['backward_forward_status']='Reviewed 2026-09-19; eligibility suspended pending source reconciliation.'

# Supplemental visits are deliberately kept separate from sources with accepted observations.
for path in sorted((ROOT/'research-extension-2026-09-19').glob('*/source-visits.json')):
 data=json.loads(path.read_text());data=data if isinstance(data,list) else data.get('visits',data.get('sources',[]))
 for v in data:
  visits.append(dict(iso2=path.parent.name,country=packets.get(path.parent.name,{}).get('country',path.parent.name),checked_date=v.get('checked_date','2026-09-19'),url=v.get('url',''),route='archive extension',query=v.get('title',''),outcome=' | '.join(str(v.get(k,'')) for k in ['years','methodology','assessment','access_status','next_step'] if v.get(k)),origin=str(path.relative_to(ROOT))))
for path in sorted((ROOT/'research-extension-2026-09-19').glob('*/accepted-observations.json')):
 for o in json.loads(path.read_text()):
  accepted.append(dict(iso2=o['iso2'],country=o['country'],year=o['year'],value=o['value'],display_value=o.get('display_value',f"{o['value']:,}"),unit=o['unit'],series_id=o['series_id'],period=o.get('period',o.get('observation_period','')),url=o['url'],locator=o.get('source_locator',o.get('locator','')),status=o['status'],chart_eligible=o.get('chart_eligible','not_assessed'),trend_eligible=o.get('trend_eligible','not_assessed'),allowed_trend_years=';'.join(map(str,o.get('allowed_trend_years',[]))),trend_review=o.get('trend_review',''),definition=o.get('definition',o.get('break_note','')),record=str(path.relative_to(ROOT))))
for name,data in [('sources.csv',rows),('visits.csv',visits),('accepted-additions.csv',accepted)]:
 with (R/name).open('w',newline='',encoding='utf-8-sig') as f:
  w=csv.DictWriter(f,fieldnames=list(data[0]));w.writeheader();w.writerows(data)
index=['# Country source catalog','','Source records and search visits are separate: a visited page is not automatically a usable employment source. The original 9 September research is imported with its recorded assessment; this catalog does not claim every old URL was rechecked today. Extension visits carry their recorded review dates.','',f'{len(packets)} countries · {len(rows)} original source records · {len(visits)} recorded search/inspection visits.','','[Employment series inventory](employment-series-inventory.md) · [Country follow-up queue](research-queue.csv) · [Machine-readable sources](sources.csv) · [Search and inspection visits](visits.csv) · [Accepted additions](accepted-additions.csv) · [Latest extension results](../research-extension-2026-09-19/README.md)','','| Country | Source records | Dated coverage in original packet | Follow-up |','| --- | ---: | --- | --- |']
index[6:6]=['[Verified Canada culture-account companions](../research-extension-2026-09-19/CA/annual-culture-followup/README.md) - 15 annual and 57 quarterly source-native rows; linked estimates, separate from the annual country-comparison ledger.','']
for iso,p in sorted(packets.items(),key=lambda x:x[1]['country']):
 sources=[s for s in rows if s['iso2']==iso];vs=[v for v in visits if v['iso2']==iso];ys=sorted({o['year'] for o in p['observations'] if o.get('year')})
 ext=ROOT/'research-extension-2026-09-19'/iso/'findings.md';follow=f'[Extension findings](../research-extension-2026-09-19/{iso}/findings.md)' if ext.exists() else 'Archive sweep pending'
 index.append(f"| [{p['country']}]({iso}.md) | {len(sources)} | {str(ys[0])+'–'+str(ys[-1]) if ys else 'Undated / no total'} | {follow} |")
 text=[f"# {p['country']}: source catalog",'','Original 9 September assessment: '+p.get('summary',''),'','Coverage ranges below include contextual and non-comparable series. A year span does not establish annual continuity.','']
 if iso in overrides:text.extend(['## Current methodological review','',overrides[iso]['summary'],'',f'[Review metadata](../research-extension-2026-09-19/{iso}/review-overrides.json)',''])
 if ext.exists():text.extend([f'[Latest deeper research](../research-extension-2026-09-19/{iso}/findings.md)',''])
 for s in sources:
  text.extend([f"## {s['title']}",'',f"[{s['publisher']}]({s['url']}) · recorded check: {s['checked_date']} · access: {s['access_status']}",'',f"- **Employment years recorded:** {s['recorded_years'].replace(';',', ') or 'None dated'}; undated observations: {s['undated_observations']}.",f"- **Units:** {s['units'] or 'No employment series accepted'}.",f"- **Method:** {s['methodology'] or 'See source note; no accepted series'}",f"- **Use / limits:** {s['assessment']}",f"- **Series eligible for a within-series trend:** {s['trend_eligible_series'] or 'None flagged in original packet'}.",f"- **Archive status:** {s['backward_forward_status']}",''])
 adds=[a for a in accepted if a['iso2']==iso]
 if adds:
  text.extend(['## Accepted extension observations','','These supplement the original snapshot. Different series or revised vintages stay separate.','','| Year | Value | Unit | Series | Source |','| --- | ---: | --- | --- | --- |'])
  for a in adds:text.append(f"| {a['year']} | {a['display_value']} | {a['unit']} | {a['series_id']} | [Original]({a['url']}) |")
  text.extend(['',f'[Full metadata](../research-extension-2026-09-19/{iso}/accepted-observations.json)',''])
 text.extend(['## Places visited and search outcomes',''])
 for v in vs:text.extend([f"- [{v['query'] or v['route']}]({v['url']}) ({v['checked_date']}, {v['route']}): {v['outcome']}"])
 (R/f'{iso}.md').write_text('\n'.join(text)+'\n')
(R/'README.md').write_text('\n'.join(index)+'\n')
assert len(packets)==31 and len(rows)==98 and len({(r['iso2'],r['source_id']) for r in rows})==98
(R/'validation.json').write_text(json.dumps({'countries':len(packets),'original_source_records':len(rows),'visits':len(visits),'accepted_additions':len(accepted),'unique_country_source_ids':True,'result':'PASS'},indent=2)+'\n')
print(f'PASS: {len(packets)} country catalogs, {len(rows)} source records, {len(visits)} visits')
