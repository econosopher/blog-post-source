"""Source-linked country bubble overview; September 9 research snapshot."""
from pathlib import Path
import json,csv,math,hashlib,textwrap
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.ticker import FixedLocator,NullLocator

OUT=Path(__file__).resolve().parent
ROOT=OUT.parent
BLUE='#285c7b'; LIGHT='#9bb7c6'; INK='#23323b'; MUTED='#687780'; RULE='#dce3e6'; BG='#fbfbf8'; ORANGE='#aa6744'
plt.rcParams.update({'font.family':'DejaVu Sans','svg.fonttype':'none','figure.facecolor':BG,'axes.facecolor':BG,'text.color':INK,'font.size':12})
# Circle area is proportional to source-native employment. No minimum-size clamp.
SIZE_DIVISOR=80
CHOICES={
 'US':(['US_core'],'Jobs · development & publishing'),
 'CA':(['CA_esac_legacy','CA_esac_revised'],'FTE · ESAC; revised coverage'),
 'KR':(['KR_core'],'People · development & publishing'),
 'GB':(['GB_tiga_people'],'People · production roles + freelancers'),
 'PL':(['PL_core_workforce'],'People · game production'),
 'BR':(['BR_developer_people'],'People · developer estimate'),
 'DE':(['DE_core'],'People · developers & publishers'),
 'TR':(['TR_toged'],'FTE · includes some staff abroad'),
 'ES':(['ES_direct'],'People · direct; freelancers separate'),
 'FR':(['FR_ey_direct_fte'],'FTE · excludes independent workers'),
 'SE':(['SE_domestic'],'Annual-average full-time positions'),
 'RO':(['RO_employees_collaborators','RO_employees2024'],'People · collaborator coverage changes'),
 'CY':(['CY_local'],'People · employed and taxed locally'),
 'NL':(['NL_domestic_broad'],'Jobs · includes self-employed specialists'),
 'RS':(['RS_national_estimate'],'People · domestic boundary uncertain'),
 'FI':(['FI_domestic_2024_fte'],'Domestic FTE · includes entrepreneurs'),
 'AU':(['AU_igea_survey','AU_igea_augmented'],'Full-time measure · includes contractors'),
 'NZ':(['NZ_fulltime','NZ_workers','NZ_fte'],'Survey · people / full-time roles / FTE'),
 'DK':(['DK_payroll_fte'],'Payroll FTE · one consistent vintage'),
 'NO':(['NO_sample_total'],'People · 30 responding employers'),
 'SG':(['SG_games2021'],'People · wider games-sector definition'),
 'CN':(['CN_listed_panel'],'Listed-company panel · location unresolved'),
 'IL':(['IL_ecosystem'],'Broad company ecosystem · location unresolved'),
}
UNDATED={'JP':('JP_core_range','People · includes console hardware'),'TW':('TW_survey2022','People · 133-company sample'),'ID':('ID_official_core_min','People · lower bound'),'PH':('PH_boi2000','People · lower bound')}
countries={p.stem:json.loads(p.read_text()) for p in (ROOT/'country').glob('*.json')}
byobs={o['observation_id']:o for c in countries.values() for o in c['observations']}
selected=[];selection=[];rows=[]
def area(v):return v/SIZE_DIVISOR
def label(o):
 q=o.get('value_qualifier','').replace(' ','_');v=o.get('value');lo=o.get('value_low');hi=o.get('value_high')
 if v is not None:return {'approximately':'≈','nearly':'nearly '}.get(q,'')+f'{v:,.0f}'
 if lo is not None and hi is not None:return ('≈' if q=='approximately' else '')+f'{lo/1000:g}–{hi/1000:g}k'
 return ('> ' if q=='more_than' else '≥ ')+f'{lo:,.0f}'
def period(iso,o):
 if iso=='CN' and o['year']==2025:return 'H1 2025 est.'
 return ('FY ' if iso in ('AU','NZ','CA') else '')+str(o['year'])
def select(iso,ids):
 c=countries[iso];ss={s['series_id']:s for s in c['series']};src={s['source_id']:s for s in c['sources']}
 result=[]
 for o in c['observations']:
  if o['series_id'] not in ids or not o['preferred'] or o['status']=='forecast':continue
  assert ss[o['series_id']]['chart_eligible']
  s=ss[o['series_id']];source=src[o['source_id']]
  out=dict(iso2=iso,country=c['country'],**o,unit=s['unit'],population_basis=s['population_basis'],geography_basis=s['geography_basis'],source_url=source['url'],source_title=source['title'],publication_date=source['publication_date'],industry_scope=s['industry_scope'],worker_status=s['worker_status'],comparison_note=s['comparison_note'])
  out['bubble_area_points2']=area(o['value']) if o['value'] is not None else None
  out['bubble_area_low_points2']=area(o['value_low']) if o['value_low'] is not None else None
  out['bubble_area_high_points2']=area(o['value_high']) if o['value_high'] is not None else None
  selected.append(out);result.append(out)
 return sorted(result,key=lambda o:o['year'] or 9999)
for iso,(ids,definition) in CHOICES.items():
 observations=select(iso,ids);assert observations and all(o['year'] is not None and o['value'] is not None for o in observations)
 rows.append(dict(iso=iso,country=countries[iso]['country'],definition=definition,observations=observations,context=iso in ('CN','IL')))
 selection.append(dict(iso2=iso,selected_series=';'.join(ids),display_definition=definition,reason='One representative employment measure; incompatible alternatives stay in original library. Changes between selected definitions use dashed links.'))
rows.sort(key=lambda r:(r['context'],-r['observations'][-1]['value']))
undated=[]
for iso,(sid,definition) in UNDATED.items():
 obs=select(iso,[sid]);assert len(obs)==1 and obs[0]['year'] is None
 undated.append((iso,definition,obs[0]))
 selection.append(dict(iso2=iso,selected_series=sid,display_definition=definition,reason='Undated panel; report edition is not treated as employment year.'))
for iso in ('IN','MY','UA','VN'):
 selection.append(dict(iso2=iso,selected_series='',display_definition='No comparable estimate plotted',reason=countries[iso]['summary']))

def csvout(name,items):
 fields=list(dict.fromkeys(k for x in items for k in x))
 with (OUT/name).open('w',newline='',encoding='utf-8-sig') as f:
  w=csv.DictWriter(f,fieldnames=fields);w.writeheader();w.writerows(items)
csvout('selected-observations.csv',selected);csvout('country-selection.csv',selection)

def connector(a,b):
 s=next(s for s in countries[a['iso2']]['series'] if s['series_id']==b['series_id'])
 uncertain=a['series_id']!=b['series_id'] or b.get('method_break') or not s['trend_eligible']
 return (0,(2.4,2.8)) if uncertain else '-'

def setup(title,subtitle):
 fig=plt.figure(figsize=(16,22))
 fig.text(.048,.963,title,fontsize=30,fontweight='bold')
 fig.text(.048,.935,subtitle,fontsize=13.8,color=MUTED)
 legend=fig.add_axes([.048,.880,.53,.040]);legend.set_xlim(0,1);legend.set_ylim(0,1);legend.axis('off')
 legend.text(0,.52,'Bubble area = employment',fontsize=11.3,color=MUTED,va='center')
 for x,v in zip([.49,.67,.87],[1000,10000,50000]):
  legend.scatter(x,.6,s=area(v),facecolor=LIGHT,edgecolor=BLUE,lw=.7,alpha=.8)
  legend.text(x,.08,f'{v:,}',ha='center',fontsize=10.8,color=MUTED)
 fig.text(.68,.900,'Selected source-native measures',fontsize=11.4,color=MUTED)
 fig.text(.68,.885,'Different units and scopes; not a country ranking.',fontsize=10.5,color=MUTED)
 return fig

def common_rows(fig,ax,log=False):
 n=len(rows);ax.set_ylim(n-.5,-.5);ax.set_yticks([])
 for spine in ax.spines.values():spine.set_visible(False)
 trans=ax.get_yaxis_transform()
 for i,r in enumerate(rows):
  latest=r['observations'][-1];color=ORANGE if r['context'] else BLUE
  country='UK' if r['iso']=='GB' else r['country']
  if r['context']:country+=' †'
  ax.text(-.45,i-.12,country,transform=trans,fontsize=13.5,fontweight='bold',va='center',clip_on=False)
  ax.text(-.45,i+.22,r['definition'],transform=trans,fontsize=9.5,color=MUTED,va='center',clip_on=False)
  ax.text(1.085,i-.1,label(latest),transform=trans,fontsize=12.4,fontweight='bold',va='center',color=color,clip_on=False)
  p=(str(r['observations'][0]['year'])+' → '+period(r['iso'],latest)) if log and len(r['observations'])>1 else period(r['iso'],latest)
  ax.text(1.085,i+.24,p,transform=trans,fontsize=10,color=MUTED,va='center',clip_on=False)
  ax.axhline(i,color=RULE,lw=.55,zorder=0)
 ax.text(-.45,1.02,'COUNTRY / MEASURE',transform=ax.transAxes,fontsize=10.8,color=MUTED)
 ax.text(1.085,1.02,'LATEST OBSERVED',transform=ax.transAxes,fontsize=10.8,color=MUTED)

def lower_panels(fig):
 fig.text(.048,.243,'Estimates without a verified employment year',fontsize=16,fontweight='bold')
 fig.text(.048,.225,'Kept off the timeline. Rings show range bounds; lower-bound bubbles use the stated minimum.',fontsize=10.5,color=MUTED)
 for (iso,definition,o),left in zip(undated,[.048,.285,.522,.759]):
  ax=fig.add_axes([left,.148,.20,.068]);ax.set_xlim(0,1);ax.set_ylim(0,1);ax.axis('off')
  ax.text(0,.95,countries[iso]['country'],fontsize=13,fontweight='bold')
  if o['value'] is not None:ax.scatter(.13,.48,s=area(o['value']),color=LIGHT,edgecolor=BLUE,lw=.8)
  else:
   if o['value_high'] is not None:ax.scatter(.13,.48,s=area(o['value_high']),facecolor='none',edgecolor=BLUE,lw=1)
   ax.scatter(.13,.48,s=area(o['value_low']),facecolor='none',edgecolor=BLUE,lw=1)
  ax.text(.32,.51,label(o),fontsize=13,fontweight='bold',va='center')
  ax.text(0,.10,definition,fontsize=9.2,color=MUTED)
 notes=[
  'People, jobs and FTE are not converted. Lines link observed years only; no missing values are estimated. Dashed links flag scope, sample or method limits.',
  'Samples: Australia through 2024, Norway, New Zealand. Australia 2025 adds estimates. Canada / Romania change series; US mapping and Sweden 2024 have breaks.',
  'Korea: latest frame excludes zero-revenue firms. Netherlands: scope shifts. Turkey / Serbia / New Zealand: some overseas coverage. Cyprus: administrative mapping.',
  '† China: listed-company panel, including H1 2025 estimate. Israel: broader company ecosystem, including gambling; location unresolved. Neither is a domestic total.',
  'No comparable estimate plotted for India, Malaysia, Ukraine or Vietnam. Additional measures and complete definitions remain in the country library.'
 ]
 for i,note in enumerate(notes):fig.text(.048,.116-i*.014,note,fontsize=9.6,color=MUTED)
 fig.text(.048,.032,'Sources: national statistics and industry reports · Research cutoff: 9 September 2026 · Exact citations: selected-observations.csv',fontsize=9.5,color=MUTED)
 fig.text(.048,.016,'GAME ECONOMIST CONSULTING',fontsize=9,fontweight='bold',color=MUTED)

def save(fig,name):
 for ext in ('png','svg'):fig.savefig(OUT/f'{name}.{ext}',dpi=180,facecolor=BG)
 plt.close(fig)

def timeline():
 fig=setup('Games employment, country by country','Each bubble is an observed year. Area shows reported employment; links keep each country’s observations together.')
 ax=fig.add_axes([.285,.280,.505,.560]);ax.set_xlim(2014.3,2025.7)
 common_rows(fig,ax)
 ax.set_xticks(range(2015,2026));ax.tick_params(axis='x',length=0,labeltop=True,labelbottom=True,pad=13,labelsize=11.2,colors=MUTED)
 for year in range(2015,2026):ax.axvline(year,color=RULE,lw=.55,zorder=0)
 for i,r in enumerate(rows):
  col=ORANGE if r['context'] else BLUE;os=r['observations']
  for a,b in zip(os,os[1:]):ax.plot([a['year'],b['year']],[i,i],color=col,lw=1.2,ls=connector(a,b),alpha=.6,zorder=1)
  for j,o in enumerate(os):
   ax.scatter(o['year'],i,s=area(o['value']),facecolor=col if j==len(os)-1 else BG,edgecolor=col,lw=.85,zorder=3,alpha=.88)
 lower_panels(fig);save(fig,'country-employment-bubble-timeline')

def logview():
 fig=setup('Games employment: scale and change','Log employment axis · Open bubble = first observation; filled bubble = latest. Bubble area remains proportional to employment.')
 ax=fig.add_axes([.285,.280,.505,.560]);ax.set_xscale('log');ax.set_xlim(300,400000)
 common_rows(fig,ax,True)
 ticks=[500,1000,3000,10000,30000,100000,300000]
 ax.set_xticks(ticks,['500','1k','3k','10k','30k','100k','300k']);ax.xaxis.set_minor_locator(NullLocator())
 ax.tick_params(axis='x',length=0,labeltop=True,labelbottom=True,pad=13,labelsize=11,colors=MUTED)
 for value in ticks:ax.axvline(value,color=RULE,lw=.6,zorder=0)
 for i,r in enumerate(rows):
  os=r['observations'];a,b=os[0],os[-1];col=ORANGE if r['context'] else BLUE
  if len(os)>1:
   dashed=any(connector(x,y)!='-' for x,y in zip(os,os[1:]))
   ax.plot([a['value'],b['value']],[i,i],lw=1.25,color=col,ls=(0,(2.4,2.8)) if dashed else '-',zorder=1)
   ax.scatter(a['value'],i,s=area(a['value']),facecolor=BG,edgecolor=col,lw=1,zorder=2)
  ax.scatter(b['value'],i,s=area(b['value']),facecolor=col,edgecolor=col,lw=.8,alpha=.75,zorder=3)
 lower_panels(fig);save(fig,'country-employment-bubbles-log-scale')

timeline();logview()
# Integrity checks on emitted rows and size encoding, independent of source selection.
assert len(rows)==23 and len(undated)==4 and len(selection)==31
assert len({o['observation_id'] for o in selected})==len(selected)
for o in selected:
 original=byobs[o['observation_id']]
 for key in ('value','value_low','value_high','year','observation_period','source_id','source_locator'):assert o[key]==original[key]
 if o['value'] is not None:assert math.isclose(o['bubble_area_points2']/o['value'],1/SIZE_DIVISOR)
from PIL import Image
for p in OUT.glob('*.png'):
 with Image.open(p) as im:assert im.size==(2880,3960);im.verify()
for p in OUT.glob('*.svg'):assert '<text' in p.read_text()
report=dict(dated_countries=len(rows),undated_countries=len(undated),countries_with_gaps=4,selected_observations=len(selected),source_snapshot_cutoff='2026-09-09',area_encoding='source-native employment / 80 in points squared; no size clamp or log size transformation',result='PASS')
(OUT/'validation.json').write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps(report,indent=2))
