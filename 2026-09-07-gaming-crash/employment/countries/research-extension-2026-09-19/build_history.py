from pathlib import Path
import json,csv,importlib.util
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
R=Path(__file__).resolve().parent
spec=importlib.util.spec_from_file_location('t',R.parent/'overview-2026-09-19/build_trend_views.py');t=importlib.util.module_from_spec(spec);spec.loader.exec_module(t)
p=json.loads((R/'US/proposed-observations.json').read_text());base=t.packets['US']
new=[o for o in p['observations_additions'] if o['preferred']]
old=[o for o in base['observations'] if o['series_id']=='US_mapping_2017']
core=next(r['observations'] for r in t.rows if r['iso']=='US')
observations=[]
for o in new+old+core:
 group='Legacy ESA estimate' if 'legacy' in o['series_id'] else ('ESA Mapping Project' if 'mapping' in o['series_id'] else 'Development / publishing subtotal')
 src=next((s for s in p['source_additions']+base['sources'] if s['source_id']==o['source_id']),{})
 observations.append({'year':int(o['year']),'employment':float(o['value']),'series':group,'source_id':o['source_id'],'source_url':src.get('url',o.get('source_url','')),'source_locator':o.get('source_locator',''),'scope_break':int(o['year'])==2016,'note':'Source-reported total retained; separate estimation/reporting families must not be spliced.'})
observations.sort(key=lambda o:o['year'])
assert [(o['year'],o['employment']) for o in observations]==[(2009,33140),(2012,42975),(2013,56712),(2014,58963),(2015,60031),(2016,65678),(2019,61230),(2023,71991),(2025,60276)]
with (R/'US-accepted-history.csv').open('w',newline='') as f:
 w=csv.DictWriter(f,fieldnames=list(observations[0]));w.writeheader();w.writerows(observations)
fig=plt.figure(figsize=(16,9));ax=fig.add_axes([.085,.30,.855,.48])
fig.text(.065,.935,'US games employment: a longer history, with breaks',fontsize=25,fontweight='bold')
fig.text(.065,.89,'Exact ESA observations reach back to 2009. Different estimation and reporting families stay separate.',fontsize=12,color=t.MUTED)
colors=['#a46b3c','#577454','#376483']
for i,(group,color) in enumerate(zip(dict.fromkeys(o['series'] for o in observations),colors)):
 os=[o for o in observations if o['series']==group]
 for a,b in zip(os,os[1:]):
  if b['scope_break']:continue
  ax.plot([a['year'],b['year']],[a['employment'],b['employment']],color=color,ls=(0,(3,3)),lw=1.5,zorder=1)
 for o in os:
  ax.scatter(o['year'],o['employment'],s=o['employment']/240,facecolor=t.BG if o['scope_break'] else color,edgecolor=color,lw=1.5,zorder=3)
  dy=15 if o['year'] not in (2014,2015) else (-30 if o['year']==2014 else 20)
  ax.annotate(f"{o['employment']:,.0f}",(o['year'],o['employment']),xytext=(0,dy),textcoords='offset points',ha='center',fontsize=10,color=color)
 fig.text(.075+i*.30,.827,group,color=color,fontsize=11.5,fontweight='bold')
ax.set_xlim(2008.4,2025.6);ax.set_ylim(0,85000);ax.set_xticks([2009,2012,2013,2014,2015,2016,2019,2023,2025]);ax.tick_params(axis='x',labelrotation=45,length=0,pad=10)
ax.set_yticks([0,20000,40000,60000,80000]);ax.yaxis.set_major_formatter(FuncFormatter(lambda x,p:f'{x:,.0f}'));ax.tick_params(axis='y',length=0,pad=10);ax.grid(axis='y',color=t.GRID,lw=.7)
ax.set_ylabel('Reported direct employment',labelpad=12)
notes=[
'2009 / 2012: legacy location-based ESA estimates, including small establishments. 2013–2015: ESA mapping observations.',
'2016: wider reporting universe (open marker). 2019 / 2023 / 2025: development/publishing subtotal; later report methods also differ.',
'Dashed links identify related observations, not interpolated annual estimates or a harmonized trend. Bubble area is proportional to employment.',
'Sources: ESA / Siwek 2014 Table D-2; ESA 2017 Tables C-2 and E-2; ESA economic impact reports 2020, 2024 and 2026.',
'Research checked 19 Sep 2026. Exact source citations and values: US-accepted-history.csv. Government software-publisher totals excluded.'
]
for i,n in enumerate(notes):fig.text(.065,.19-i*.031,n,fontsize=10,color=t.MUTED)
for ext in ('png','svg'):fig.savefig(R/f'US-employment-long-history.{ext}',dpi=180,facecolor=t.BG)
plt.close(fig)
(R/'integration-validation.json').write_text(json.dumps({'accepted_new_US_observations':4,'US_companion_observations':9,'earliest_year':2009,'series_splicing':False,'source_tables_independently_viewed':['ESA2014 D-2','ESA2017 E-2'],'result':'PASS'},indent=2)+'\n')
print('PASS: four new observations; nine-point history in separate series.')
