from pathlib import Path
import json,csv,collections
ROOT=Path(__file__).resolve().parents[1]
OUT=ROOT/'source-catalog'
reviews={p.parent.name:json.loads(p.read_text()) for p in (ROOT/'research-extension-2026-09-19').glob('*/review-overrides.json')}
rows=[]
for p in sorted((ROOT/'country').glob('*.json')):
 d=json.loads(p.read_text())
 for s in d['series']:
  if s.get('measure')!='employment_stock':continue
  obs=[o for o in d['observations'] if o['series_id']==s['series_id']]
  years=sorted({o['year'] for o in obs if o.get('year') is not None})
  suspend=s['series_id'] in reviews.get(d['iso2'],{}).get('suspended_series_ids',[])
  rows.append(dict(iso2=d['iso2'],country=d['country'],series_id=s['series_id'],origin='original snapshot',unit=s['unit'],years=';'.join(map(str,years)),dated_years=len(years),undated_observations=sum(o.get('year') is None for o in obs),trend_review='suspended' if suspend else ('within-series eligible' if s.get('trend_eligible') else 'not eligible'),scope=reviews[d['iso2']]['summary'] if suspend else s.get('comparison_note',''),source_record=f'country/{p.name}'))
for p in sorted((ROOT/'research-extension-2026-09-19').glob('*/accepted-observations.json')):
 grouped=collections.defaultdict(list)
 for o in json.loads(p.read_text()):grouped[o['series_id']].append(o)
 for sid,obs in grouped.items():
  years=sorted({o['year'] for o in obs if o.get('year') is not None}); states={o.get('trend_eligible','not_assessed') for o in obs}
  state='not eligible' if False in states else ('within-series eligible' if states=={True} else 'not yet assessed')
  rows.append(dict(iso2=obs[0]['iso2'],country=obs[0]['country'],series_id=sid,origin='accepted extension',unit=';'.join(sorted({o['unit'] for o in obs})),years=';'.join(map(str,years)),dated_years=len(years),undated_observations=sum(o.get('year') is None for o in obs),trend_review=state,scope=' | '.join(dict.fromkeys(o.get('definition',o.get('break_note',''))+' '+o.get('trend_review','') for o in obs)),source_record=str(p.relative_to(ROOT))))
with (OUT/'employment-series-inventory.csv').open('w',newline='',encoding='utf-8-sig') as f:
 w=csv.DictWriter(f,fieldnames=list(rows[0]));w.writeheader();w.writerows(rows)
lines=['# Employment series inventory','','One row per source-defined series and evidence layer. Original and extension rows remain separate; a verified observation does not automatically establish comparable annual coverage. Eligibility means within that series only, never cross-country equivalence. Single observations cannot establish a trend.','','| Country | Original stock series | Extension series | Eligible multi-year original series | Extension series awaiting trend review |','| --- | ---: | ---: | --- | ---: |']
countries={p.stem:json.loads(p.read_text())['country'] for p in (ROOT/'country').glob('*.json')}
for iso in sorted(countries,key=countries.get):
 rr=[x for x in rows if x['iso2']==iso];orig=[x for x in rr if x['origin']=='original snapshot'];ext=[x for x in rr if x['origin']=='accepted extension'];good=[x['series_id'] for x in orig if x['trend_review']=='within-series eligible' and x['dated_years']>=2]
 lines.append(f"| [{countries[iso]}]({iso}.md) | {len(orig)} | {len(ext)} | {', '.join(good) or 'None'} | {sum(x['trend_review']=='not yet assessed' for x in ext)} |")
lines+=['','[Full series, years, units and limitations](employment-series-inventory.csv)','','Reviewed joins and year restrictions: [comparison policy](reviewed-comparison-segments.json) and [reviewed chart-ready observations](reviewed-comparison-observations.csv). These later reviews govern joins, including the exclusion of US 2016 from the 2013-2015 segment. The regional-wrap chart applies the reviewed comparison policy; see its validation and observation-level plot decisions for current coverage. Cyprus is suspended by the later source review.']
(OUT/'employment-series-inventory.md').write_text('\n'.join(lines)+'\n')
assert len(countries)==31
print('Inventory saved:',len(rows),'series/layer records;',sum(x['trend_review']=='not yet assessed' for x in rows),'awaiting explicit trend review',flush=True)
