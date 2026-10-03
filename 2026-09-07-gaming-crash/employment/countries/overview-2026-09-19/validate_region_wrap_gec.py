from pathlib import Path
import json,csv,collections
R=Path(__file__).resolve().parent.parent;O=R/'overview-2026-09-19';E=R/'research-extension-2026-09-19'
a=list(csv.DictReader((O/'region-wrap-selected-observations.csv').open(encoding='utf-8-sig')));ids={o['observation_id']:o for o in a};keys={(o['iso2'],o['series_id'],int(o['year'])) for o in a if o['year']};links=json.loads((O/'region-wrap-links.json').read_text());pol=json.loads((R/'source-catalog/reviewed-comparison-segments.json').read_text());segs={s['segment_id']:s for s in pol['segments']}
for link in links:
 x,y=ids[link['from_id']],ids[link['to_id']]
 assert x['iso2']==y['iso2']
 if link['connection_type']=='country_identity_only':continue
 assert x.get('marker_only')!='True' and y.get('marker_only')!='True'
 if link['segment_id'] in segs:
  s=segs[link['segment_id']];assert x['series_id'] in s['series_ids'] and y['series_id'] in s['series_ids'];assert int(x['year']) in s['allowed_years'] and int(y['year']) in s['allowed_years']
 else:
  assert x['series_id']==y['series_id'] and y['method_break']!='True'
assert not any(x['iso2']=='CY' for x in a)
hidden=set(json.loads((O/'region-wrap-validation.json').read_text()).get('hidden_low_point_countries',[]))
assert all(len({o['year'] for o in a if o['iso2']==iso})>=3 for iso in {o['iso2'] for o in a})
for iso in {o['iso2'] for o in a}:
 country_ids={o['observation_id'] for o in a if o['iso2']==iso}
 edges=[e for e in links if e['iso2']==iso]
 assert len(edges)==len(country_ids)-1
 assert {e[k] for e in edges for k in ('from_id','to_id')}==country_ids
rows=[]
for p in E.glob('*/accepted-observations.json'):
 for o in json.loads(p.read_text()):
  plotted=(o['iso2'],o['series_id'],o['year']) in keys
  state='hidden country with two or fewer dated points by user request' if o['iso2'] in hidden else ('plotted' if plotted else ('reviewed alternative; omitted to avoid duplicate measure' if o.get('chart_eligible') is True else ('withheld by explicit scope decision' if o.get('chart_eligible') is False else 'plot-selection review pending')))
  rows.append(dict(iso2=o['iso2'],series_id=o['series_id'],year=o['year'],value=o['value'],decision=state,source=o['url']))
with (O/'extension-chart-decisions.csv').open('w',encoding='utf-8-sig',newline='') as f:
 w=csv.DictWriter(f,fieldnames=list(rows[0]));w.writeheader();w.writerows(rows)
counts=dict(collections.Counter(r['decision'] for r in rows));print(counts)
expected_count=sum(len(json.loads(p.read_text())) for p in E.glob('*/accepted-observations.json'))
assert len(rows)==expected_count
assert len({(o['iso2'],o['series_id'],o['year']) for o in rows})==expected_count
for p in E.glob('*/accepted-observations.json'):
 for o in json.loads(p.read_text()):
  if o.get('marker_only') and o['iso2'] not in hidden:
   assert (o['iso2'],o['series_id'],o['year']) in keys
assert not any(r['decision']=='plot-selection review pending' for r in rows)
v=json.loads((O/'region-wrap-validation.json').read_text());v['edge_policy_validation']='PASS';v['extension_decisions']=counts;(O/'region-wrap-validation.json').write_text(json.dumps(v,indent=2)+'\n')
(O/'REVIEW-NOTICE.md').write_text(f'''# Chart review status

The current country-employment-region-wrap PNG/SVG includes twelve reviewed historical comparison segments, with countries having at least three dated observations, a shared 2007-2026 calendar axis, and independent regional log-employment ranges. It applies the Cyprus suspension. All observations connect within each country. Comparable links retain their source-review checks; short dashed identity links cross scope or method changes and estimates without asserting comparability.

France2010-2018 broad ecosystem jobs remain separate from later employee FTE. Canada uses administrative ILUs; Japan and Finland are hidden because only two selected dated observations remain. US historical methods and Germany\'s revised2025/2026 pair remain separate. The alternative UK FTE series remains in the comparison export; the figure selects headcount.

See region-wrap-selected-observations.csv for exact source periods/definitions, extension-chart-decisions.csv for every accepted addition\'s plot decision, and region-wrap-links.json for every plotted connection. All {expected_count} accepted additions now have explicit plot-selection decisions. A connecting line is not evidence of consistent definitions, interpolation or a national census.

Other previously generated chart variants are historical and have not been reconciled to these later reviews. Use the regional-wrap chart for the current view.
''')
print('PASS: all accepted additions have explicit plot decisions.')
