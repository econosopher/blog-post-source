"""One region per facet, shared calendar axis and independent regional log employment ranges."""
from pathlib import Path
import importlib.util,csv,json,math
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import NullLocator

R=Path(__file__).resolve().parent
spec=importlib.util.spec_from_file_location('trends',R/'build_trend_views.py')
t=importlib.util.module_from_spec(spec);spec.loader.exec_module(t)
REGIONS={**t.REGIONS,'JP':'Asia','TW':'Asia','ID':'Asia','PH':'Asia','IN':'Asia','MY':'Asia','VN':'Asia','UA':'Europe'}
reviews={p.parent.name:json.loads(p.read_text()) for p in (R.parent/'research-extension-2026-09-19').glob('*/review-overrides.json')}
suspended={sid for review in reviews.values() for sid in review.get('suspended_series_ids',[])}
# Integrate only explicitly reviewed extension segments; original files stay unchanged.
policy=json.loads((R.parent/'source-catalog/reviewed-comparison-segments.json').read_text())
selected_segments=[g for g in policy['segments'] if g['segment_id']!='GB_tiga_fte']
allowed={g['segment_id'] for g in selected_segments}
extension_rows=[o for o in policy['observations'] if o['segment_id'] in allowed]
replace_countries={'CA','JP'}
t.data=[dict(o) for o in t.data if o['series_id'] not in suspended and o['iso2'] not in replace_countries]
by_id={o['observation_id']:o for o in t.data}
for o in extension_rows:
 item=by_id.setdefault(o['observation_id'],dict(o,source_url=o['url'],method_break='False',value_qualifier=''))
 item['segment_id']=o['segment_id'];item['comparison_note']=o['constraints']
 if o['iso2']=='FI':item['value_qualifier']='approximately'
for accepted_path in (R.parent/'research-extension-2026-09-19').glob('*/accepted-observations.json'):
 for o in json.loads(accepted_path.read_text()):
  if not o.get('marker_only'):continue
  assert o['chart_eligible'] and not o['trend_eligible'] and o['marker_only']
  oid=o.get('observation_id',f"{o['series_id']}_{o['year']}")
  by_id[oid]=dict(o,observation_id=oid,source_url=o['url'],method_break='True',marker_only=True,comparison_note=o['trend_review'],value_qualifier=o.get('display_value','approximately' if o.get('approximate') else ''))
t.data=list(by_id.values())
t.rows=[]
for iso in sorted({o['iso2'] for o in t.data if o.get('year')}):
 os=sorted([dict(o,year=int(o['year']),value=float(o['value'])) for o in t.data if o['iso2']==iso and o.get('year')],key=lambda o:(o['year'],o['series_id']))
 t.rows.append(dict(iso=iso,country='UK' if iso=='GB' else t.packets[iso]['country'],region=REGIONS.get(iso,'Europe'),observations=os))
for iso,definition in {'CA':'Individual labour units · annual T4 administrative data; provinces only','JP':'Persons engaged · responding JSIC3914 establishments','FI':'Approximate domestic FTE · entrepreneurs included'}.items():
 t.selection[iso]['display_definition']=definition
 t.selection[iso]['selected_series']=';'.join(sorted({o['series_id'] for o in t.data if o['iso2']==iso}))
for iso in {o['iso2'] for o in t.data}:
 t.selection[iso]['selected_series']=';'.join(sorted({o['series_id'] for o in t.data if o['iso2']==iso}))
for iso,definition in {'FR':'Broad ecosystem persons/jobs 2010-2018; separate employee FTE 2019-2024','US':'Separate ESA modeled history, mapping history and later developer/publisher jobs; no cross-method joins','DE':'Developer/publisher workforce; 2025-2026 revised vintage separate from original history'}.items():
 t.selection[iso]['display_definition']=definition
all_selected=list(t.data)
low_point_countries={r['iso'] for r in t.rows if len({o['year'] for o in r['observations']})<=2}
t.rows=[r for r in t.rows if r['iso'] not in low_point_countries]
visible_countries={r['iso'] for r in t.rows}
t.data=[o for o in t.data if o['iso2'] in visible_countries and o.get('year')]
export_fields=sorted(set().union(*(o.keys() for o in t.data)))
with (R/'region-wrap-selected-observations.csv').open('w',encoding='utf-8-sig',newline='') as f:
 writer=csv.DictWriter(f,fieldnames=export_fields);writer.writeheader();writer.writerows(t.data)
links=[]
def can_link(a,b):
 if a.get('marker_only') or b.get('marker_only'):return False
 if a.get('segment_id') or b.get('segment_id'):
  return bool(a.get('segment_id')) and a.get('segment_id')==b.get('segment_id')
 return not t.limited(a,b)
inventory=[]
dated={r['iso']:r for r in t.rows}
for iso,c in sorted(t.packets.items()):
 region=REGIONS.get(iso,'Europe');chosen=[o for o in all_selected if o['iso2']==iso]
 state='suspended after source review' if iso in reviews else ('two or fewer dated points; hidden by request' if iso in low_point_countries else ('dated' if iso in dated else ('undated' if chosen else 'gap / incompatible evidence')))
 inventory.append(dict(region=region,country=c['country'],iso2=iso,state=state,selected_observations=len(chosen),years=';'.join(str(o['year']) for o in chosen if o['year']),selected_series=t.selection[iso]['selected_series'],definition=reviews[iso]['summary'] if iso in reviews else t.selection[iso]['display_definition']))
with (R/'regional-country-inventory.csv').open('w',encoding='utf-8-sig',newline='') as f:
 w=csv.DictWriter(f,fieldnames=list(inventory[0]));w.writeheader();w.writerows(sorted(inventory,key=lambda x:(x['region'],x['country'])))

def labels(target,gap):
 blocks=[]
 for value in target-np.arange(len(target))*gap:
  blocks.append([float(value),1])
  while len(blocks)>1 and blocks[-2][0]>blocks[-1][0]:
   b=blocks.pop();a=blocks.pop();blocks.append([(a[0]*a[1]+b[0]*b[1])/(a[1]+b[1]),a[1]+b[1]])
 offsets=np.array([m for m,n in blocks for _ in range(n)])
 return np.clip(offsets,.035,.97-(len(target)-1)*gap)+np.arange(len(target))*gap

fig=plt.figure(figsize=(22,16))
fig.text(.045,.956,'Games employment over time, by region',fontsize=33,fontweight='bold')
fig.text(.045,.925,'Countries reviewed',fontsize=14,color=t.MUTED)
panel_positions=[('Europe',.060,.575,.29,.26),('Asia',.560,.575,.29,.26),('Americas',.060,.185,.29,.26),('Oceania',.560,.185,.29,.26)]
regional_limits={}
for region,x,y,w,h in panel_positions:
 rs=[r for r in t.rows if r['region']==region];members=[m for m in inventory if m['region']==region];color=t.COLORS[region]
 fig.text(x,y+h+.036,region,fontsize=22,fontweight='bold',color=color)
 values=[o['value'] for r in rs for o in r['observations']]
 lo=math.floor((math.log10(min(values))-.08)*5)/5;hi=math.ceil((math.log10(max(values))+.12)*5)/5
 regional_limits[region]=[10**lo,10**hi]
 ax=fig.add_axes([x,y,w,h]);ax.set_xlim(2006.5,2026.7);ax.set_yscale('log');ax.set_ylim(10**lo,10**hi)
 ax.set_xticks([2007,2011,2015,2019,2023,2026]);ticks=[m*10**e for e in range(2,7) for m in (1,2,5) if 10**lo<=m*10**e<=10**hi];ax.set_yticks(ticks,[f'{v/1000:g}k' if v>=1000 else str(v) for v in ticks])
 ax.yaxis.set_minor_locator(NullLocator());ax.tick_params(length=0,pad=7,labelsize=10.5);ax.grid(axis='y',color=t.GRID,lw=.6)
 if x<.1:ax.set_ylabel('Reported employment · log scale',fontsize=10.5,labelpad=12)
 for r in rs:
  os=r['observations']
  for a,b in zip(os,os[1:]):
   comparable=can_link(a,b) and not (r['iso']=='CN' and b['year']==2025)
   links.append(dict(iso2=r['iso'],from_id=a['observation_id'],to_id=b['observation_id'],segment_id=b.get('segment_id',b['series_id']),connection_type='comparable' if comparable else 'country_identity_only'))
   style=(0,(2,3)) if not comparable else ((0,(6,4)) if b['year']-a['year']>1 else '-')
   ax.plot([a['year'],b['year']],[a['value'],b['value']],color=color,lw=1.35,alpha=.72,ls=style,zorder=2)
  for j,o in enumerate(os):ax.scatter(o['year'],o['value'],s=o['value']/180*(math.pi/4 if r['iso']=='CN' and o['year']==2025 else 1),marker='D' if r['iso']=='CN' and o['year']==2025 else 'o',facecolor=color if j==len(os)-1 else t.BG,edgecolor=color,lw=.8,zorder=3)
 ordered=sorted(rs,key=lambda r:r['observations'][-1]['value'])
 target=np.array([(math.log10(r['observations'][-1]['value'])-lo)/(hi-lo) for r in ordered])
 pos=labels(target,.077 if len(rs)>8 else .072)
 for r,z in zip(ordered,pos):
  o=r['observations'][-1];name={'CN':'China · listed firms †','IL':'Israel · broad ecosystem †','RS':'Serbia · industry estimate †'}.get(r['iso'],r['country']);year='H1 2025E' if r['iso']=='CN' else str(o['year'])
  ax.annotate('',xy=(o['year'],o['value']),xytext=(1.022,z),textcoords=ax.transAxes,arrowprops=dict(arrowstyle='-',color='#cbd2d4',lw=.6),annotation_clip=False,zorder=1)
  ax.text(1.035,z,f"{name}\n{t.value_label(o)} · {year}",transform=ax.transAxes,fontsize=9.4,color=color,va='center',linespacing=1.2,clip_on=False)

notes=[
 'Each panel has its own log y-axis range; bubble area uses one workforce scale across panels. Countries with two or fewer dated observations omitted.',
 'All points connect within country: solid = comparable; long dashes = missing years; short dashes = definition / method change or estimate. No regional sums.',
 'Units differ: Canada = annual labour units; Denmark = FTE; France = broad jobs before 2019, then employee FTE; Sweden = annual full-time positions. Other series use people/jobs or mixed measures.',
 '† China: CNG company panel, scope / geography undisclosed; diamond = H1 2025 estimate. Not a verified domestic total.',
 'Qualified points: Netherlands 2020 ≈4,000; Romania 2020 6,000+; Serbia 2021 ≈2,200+, 2022 ≈2,500+, 2023 estimated 4,300; worker location unresolved.',
 '19 Sep review: US history = separate ESA methods; Germany 2025–26 = revised pair. Sources and periods: region-wrap-selected-observations.csv.'
]
for i,note in enumerate(notes):fig.text(.045,.088-i*.017,note,fontsize=10.5,color=t.MUTED)
for ext in ('png','svg'):fig.savefig(R/f'country-employment-region-wrap.{ext}',dpi=180,facecolor=t.BG)
plt.close(fig)
assert len(inventory)==31 and all(len({o['year'] for o in r['observations']})>=3 for r in t.rows)
assert not any(o['series_id'] in suspended for o in t.data)
assert not any(o.get('series_id')=='JP_meti_game_enterprises_development_regular' for o in t.data)
(R/'region-wrap-links.json').write_text(json.dumps(links,indent=2)+'\n')
(R/'region-wrap-validation.json').write_text(json.dumps({'countries_in_inventory':31,'dated_countries_plotted':len(dated),'suspended':list(reviews),'suspended_series_ids':sorted(suspended),'undated':3,'gaps':4,'shared_x_limits':[2006.5,2026.7],'regional_log_y_limits':regional_limits,'hidden_low_point_countries':sorted(low_point_countries),'reviewed_segments_integrated':len(selected_segments),'plotted_observations':sum(len(r['observations']) for r in t.rows),'connections':len(links),'result':'PASS'},indent=2)+'\n')
print(f'PASS: 4 regional facets, {len(dated)} countries, independent y ranges; hidden countries with two or fewer points: {sorted(low_point_countries)}')
