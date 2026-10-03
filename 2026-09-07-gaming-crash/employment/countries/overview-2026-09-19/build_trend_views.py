"""Shared vertical employment axis and continent-grouped country bar facets."""
from pathlib import Path
import csv,json,math,textwrap
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.ticker import NullLocator,FuncFormatter,MaxNLocator
from PIL import Image

R=Path(__file__).resolve().parent
ROOT=R.parent
BG='#fbfbf8';INK='#23323b';MUTED='#687780';GRID='#dfe5e6'
COLORS={'Americas':'#376483','Europe':'#577454','Asia':'#a46b3c','Oceania':'#796783'}
REGIONS={'US':'Americas','CA':'Americas','BR':'Americas','KR':'Asia','TR':'Asia','SG':'Asia','CN':'Asia','IL':'Asia','AU':'Oceania','NZ':'Oceania'}
plt.rcParams.update({'font.family':'DejaVu Sans','svg.fonttype':'none','figure.facecolor':BG,'axes.facecolor':BG,'text.color':INK,'axes.labelcolor':MUTED,'xtick.color':MUTED,'ytick.color':MUTED,'axes.spines.top':False,'axes.spines.right':False,'axes.spines.left':False,'axes.spines.bottom':False})
with (R/'selected-observations.csv').open(encoding='utf-8-sig') as f:data=list(csv.DictReader(f))
with (R/'country-selection.csv').open(encoding='utf-8-sig') as f:selection={x['iso2']:x for x in csv.DictReader(f)}
packets={p.stem:json.loads(p.read_text()) for p in (ROOT/'country').glob('*.json')}
definitions={s['series_id']:s for c in packets.values() for s in c['series']}
rows=[]
for iso in sorted({o['iso2'] for o in data if o['year']}):
 obs=[dict(o,year=int(o['year']),value=float(o['value'])) for o in data if o['iso2']==iso and o['year']]
 obs.sort(key=lambda o:o['year']);assert all(o['value']>0 for o in obs)
 rows.append(dict(iso=iso,country='UK' if iso=='GB' else obs[0]['country'],observations=obs,region=REGIONS.get(iso,'Europe'),context=iso in ('CN','IL'),definition=selection[iso]['display_definition']))

def fmt(v):return f'{v/1000:g}k' if v>=1000 else f'{v:g}'
def value_label(o):
 q=o.get('value_qualifier','').replace(' ','_');return {'approximately':'≈','nearly':'nearly '}.get(q,'')+f"{o['value']:,.0f}"
def limited(a,b):
 return a['series_id']!=b['series_id'] or b['method_break']=='True' or not definitions[b['series_id']]['trend_eligible']
def save(fig,name):
 for ext in ('png','svg'):fig.savefig(R/f'{name}.{ext}',dpi=180,facecolor=BG)
 plt.close(fig)
def legend(fig,y):
 for (region,color),x in zip(COLORS.items(),[.07,.235,.37,.495]):
  fig.text(x,y,'●  '+region,color=color,fontsize=12,fontweight='bold')
def footnotes(fig,base,step=0.02,size=10.3):
 notes=[
 'FTE: Canada, Denmark, Finland, France, Turkey. Sweden: annual full-time positions. Others people/jobs, except mixed measures in Australia / NZ.',
 'Australia / Norway / New Zealand include survey samples; NZ also changes units. Canada / Romania change coverage; US mapping and Sweden 2024 have breaks.',
 'Korea excludes zero-revenue firms in the latest frame. Netherlands: scope shifts. Turkey / Serbia / NZ may include overseas workers. Cyprus: administrative mapping.',
 '† China is a listed-company panel (H1 2025 estimate at the final point). Israel is a broad company ecosystem, including gambling; neither is a verified domestic total.',
 'Japan, Taiwan, Indonesia and the Philippines lack a verified employment year. India, Malaysia, Ukraine and Vietnam have no comparable estimate plotted.',
 'Sources: national statistics and industry reports, available by 9 Sep 2026. Exact periods, definitions and citations: selected-observations.csv.'
 ]
 for i,s in enumerate(notes):fig.text(.06,base-i*step,s,fontsize=size,color=MUTED)

def shared():
 fig=plt.figure(figsize=(19,13.5))
 fig.text(.06,.953,'Games employment over time',fontsize=31,fontweight='bold')
 fig.text(.06,.920,'Year on the horizontal axis. Employment on one shared log scale; equal vertical distances mean equal proportional changes.',fontsize=13.4,color=MUTED)
 legend(fig,.882)
 fig.text(.665,.882,'Bubble area ∝ employment',fontsize=11,color=MUTED)
 ax=fig.add_axes([.08,.245,.64,.60]);ax.set_yscale('log');ax.set_ylim(350,350000);ax.set_xlim(2014.65,2025.6)
 ax.set_xticks(range(2015,2026));ax.set_yticks([500,1000,3000,10000,30000,100000,300000],['500','1,000','3,000','10,000','30,000','100,000','300,000'])
 ax.yaxis.set_minor_locator(NullLocator());ax.grid(axis='y',color=GRID,lw=.7);ax.tick_params(length=0,pad=9,labelsize=11.3)
 ax.set_ylabel('Reported employment · log scale',labelpad=14,fontsize=12)
 ax.set_xlabel('Observation year',fontsize=11.5,labelpad=12)
 for r in rows:
  os=r['observations'];color=COLORS[r['region']]
  for a,b in zip(os,os[1:]):
   # Short dashes mean continuity limits; long dashes mean missing annual observations.
   style=(0,(2,2)) if limited(a,b) else ((0,(6,4)) if b['year']-a['year']>1 else '-')
   ax.plot([a['year'],b['year']],[a['value'],b['value']],color=color,alpha=.73,lw=1.45,ls=style,zorder=2)
  for j,o in enumerate(os):
   ax.scatter(o['year'],o['value'],s=o['value']/160,facecolor=color if j==len(os)-1 else BG,edgecolor=color,lw=.9,alpha=.9,zorder=3)
 # Sort labels vertically by endpoint. Leader lines are annotations, not data.
 ordered=sorted(rows,key=lambda r:r['observations'][-1]['value'])
 lo,hi=math.log10(350),math.log10(350000)
 target=np.array([(math.log10(r['observations'][-1]['value'])-lo)/(hi-lo) for r in ordered])
 gap=.033
 blocks=[]
 for value in target-np.arange(len(target))*gap:
  blocks.append([float(value),1])
  while len(blocks)>1 and blocks[-2][0]>blocks[-1][0]:
   b=blocks.pop();a=blocks.pop();blocks.append([(a[0]*a[1]+b[0]*b[1])/(a[1]+b[1]),a[1]+b[1]])
 offsets=np.array([mean for mean,count in blocks for _ in range(count)])
 positions=np.clip(offsets,.02,.98-(len(target)-1)*gap)+np.arange(len(target))*gap
 assert min(np.diff(positions))>gap-1e-9
 for r,y in zip(ordered,positions):
  o=r['observations'][-1];v=10**(lo+y*(hi-lo));name=r['country']+(' †' if r['context'] else '')
  year='H1 2025E' if r['iso']=='CN' else str(o['year'])
  ax.annotate('',xy=(o['year'],o['value']),xytext=(1.025,y),textcoords=ax.transAxes,arrowprops=dict(arrowstyle='-',color='#c8cfd1',lw=.65),annotation_clip=False,zorder=1)
  ax.text(1.035,y,f"{name}  {value_label(o)} · {year}",transform=ax.transAxes,va='center',fontsize=10.5,color=COLORS[r['region']],clip_on=False)
 ax.text(1.035,1.02,'LAST OBSERVED VALUE / YEAR',transform=ax.transAxes,fontsize=10,color=MUTED)
 fig.text(.06,.185,'Solid links: consecutive observations. Long dashes: missing years. Short dashes: scope, sample or method limits. Links do not estimate missing values.',fontsize=10.4,color=MUTED)
 footnotes(fig,.150,.020,10.2)
 save(fig,'country-employment-shared-axis')

def facets():
 order={'Americas':0,'Europe':1,'Asia':2,'Oceania':3}
 sortedrows=sorted(rows,key=lambda r:(order[r['region']],-r['observations'][-1]['value']))
 fig=plt.figure(figsize=(18,20.5))
 fig.text(.055,.964,'Country employment trends, grouped by continent',fontsize=27,fontweight='bold')
 fig.text(.055,.943,'Each country has its own zero-based vertical scale. Compare its shape here; use the shared-axis chart to compare country sizes.',fontsize=12.5,color=MUTED)
 legend(fig,.919)
 gs=fig.add_gridspec(6,4,left=.065,right=.955,bottom=.155,top=.855,wspace=.34,hspace=.72)
 for i,r in enumerate(sortedrows):
  ax=fig.add_subplot(gs[i//4,i%4]);os=r['observations'];color=COLORS[r['region']]
  ax.set_title(r['country']+(' †' if r['context'] else ''),loc='left',fontsize=14,fontweight='bold',pad=40)
  ax.text(0,1.085,textwrap.fill(r['definition'],43),transform=ax.transAxes,fontsize=8.5,color=MUTED,va='bottom')
  heights=[o['value'] for o in os];bars=ax.bar([o['year'] for o in os],heights,width=.68,color=color,alpha=.43,zorder=2)
  bars[-1].set_alpha(.92)
  for j in range(1,len(os)):
   if os[j]['series_id']!=os[j-1]['series_id'] or os[j]['method_break']=='True':bars[j].set_hatch('///');bars[j].set_edgecolor(color)
  ax.annotate(value_label(os[-1]),(os[-1]['year'],os[-1]['value']),xytext=(0,5),textcoords='offset points',ha='center',fontsize=9.5,color=color,fontweight='bold')
  ax.set_xlim(2014.35,2026.05);ax.set_ylim(0,max(heights)*1.30)
  ax.set_xticks([2015,2020,2025]);ax.yaxis.set_major_locator(MaxNLocator(3));ax.yaxis.set_major_formatter(FuncFormatter(lambda v,p:fmt(v)))
  ax.grid(axis='y',color=GRID,lw=.55,zorder=0);ax.tick_params(length=0,pad=5,labelsize=9)
  ax.text(.98,.96,r['region'],transform=ax.transAxes,ha='right',va='top',fontsize=8.5,color=color)
 last=fig.add_subplot(gs[5,3]);last.axis('off')
 last.text(0,.82,'Blank years are missing,\nnot zero employment.',fontsize=13,fontweight='bold',linespacing=1.5)
 last.text(0,.39,'Dark bar = latest observation\nHatching = recorded definition break\nNo regional sums or stacked areas',fontsize=10.5,color=MUTED,linespacing=1.65)
 footnotes(fig,.105,.014,9.4)
 save(fig,'country-employment-faceted-bars')

if __name__=='__main__':
 shared();facets()
 used={o['observation_id'] for r in rows for o in r['observations']}
 assert len(rows)==23 and len(used)==118
 for r in rows:
  originals={o['observation_id']:o for o in packets[r['iso']]['observations']}
  for o in r['observations']:assert originals[o['observation_id']]['value']==o['value'] and originals[o['observation_id']]['year']==o['year']
 for name in ('country-employment-shared-axis','country-employment-faceted-bars'):
  with Image.open(R/f'{name}.png') as im:im.verify()
  assert '<text' in (R/f'{name}.svg').read_text()
 (R/'trend-view-validation.json').write_text(json.dumps(dict(result='PASS',countries=23,observations=118,shared_axis='employment on one log y-axis; year on x-axis',faceted_bars='zero-based independent y-scales, shared 2015-2025 calendar axis',stacking=False,interpolation=False),indent=2)+'\n')
 print('PASS: 23 countries; 118 unchanged observations; shared log axis and independent zero-based bar facets.')
