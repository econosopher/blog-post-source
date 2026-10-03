"""Build source-linked CSVs and figures from separately curated country packets."""
from pathlib import Path
import csv,json,math,re,textwrap,sys
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter,MaxNLocator
from PIL import Image,ImageOps,ImageDraw
R=Path(__file__).resolve().parents[1]
EXPECTED=set('US CA GB SE FI DE FR ES PL NL RO CY CN JP KR VN DK NO RS UA TR IL IN TW SG MY ID PH BR AU NZ'.split())
BLUE='#245b80';ORANGE='#b16837';INK='#23313a';MUTED='#566670';GRID='#dce2e4';BG='#fbfbf8'
plt.rcParams.update({'font.family':['DejaVu Sans','Arial Unicode MS'],'font.size':12,'text.color':INK,'axes.labelcolor':MUTED,'xtick.color':MUTED,'ytick.color':MUTED,'axes.spines.top':False,'axes.spines.right':False,'axes.spines.left':False,'axes.edgecolor':GRID,'svg.fonttype':'none','figure.facecolor':BG,'axes.facecolor':BG})
UNIT={'people':'People','FTE':'Full-time equivalents (FTE)','jobs':'Jobs','percent':'Percent','annual_average_full_time_positions':'Annual average full-time positions','full_time_positions':'Full-time employees / roles','source_reported_full_time_employment':'Source full-time employment, incl. FTEs + contractors','ILU':'Employment units (ILU)'}
PLOTTED=[];FIGURES=[];WARN=[];CHECKS=[]
def wrap(s,n=110):return '\n'.join(textwrap.wrap(str(s),n,break_long_words=False,break_on_hyphens=False))
def fmt(v):
 if v is None:return ''
 return f'{v:,.0f}' if abs(v-round(v))<1e-8 else f'{v:,.1f}'
def sval(o):
 q=o.get('value_qualifier','').replace(' ','_')
 if o.get('value') is not None:
  prefix={'approximately':'~','nearly':'nearly ','more_than':'> ','at_least':'≥ ','up_to':'≤ '}.get(q,'')
  return prefix+fmt(o['value'])
 lo=o.get('value_low');hi=o.get('value_high')
 if lo is not None and hi is not None:return ('~' if q=='approximately' else '')+f'{fmt(lo)} to {fmt(hi)}'
 return ('> ' if q=='more_than' else '≥ ')+fmt(lo) if lo is not None else ('< ' if q in ('less_than','almost') else '≤ ')+fmt(hi)
def csvout(name,rows,fields=None):
 if fields is None:fields=list(dict.fromkeys(k for row in rows for k in row))
 with (R/name).open('w',newline='',encoding='utf-8-sig') as f:
  w=csv.DictWriter(f,fieldnames=fields,extrasaction='ignore');w.writeheader()
  for row in rows:w.writerow({k:json.dumps(v,ensure_ascii=False) if isinstance(v,(list,dict)) else v for k,v in row.items()})
def numeric(o):return any(isinstance(o.get(k),(float,int)) for k in ['value','value_low','value_high'])
def rows_for(c,s):return sorted([o for o in c['observations'] if o['series_id']==s['series_id'] and o.get('preferred',True) and o.get('status')!='forecast' and numeric(o)],key=lambda o:(o.get('year') is None,o.get('year') or 9999,o.get('subgroup','')))
def source_note(c,s,rows):
 sources={a['source_id']:a for a in c['sources']};unique=list(dict.fromkeys(o['source_id'] for o in rows))
 pubs={}
 for x in unique:
  src=sources[x];date=src.get('publication_date');vintages=list(dict.fromkeys(str(o['publication_vintage']) for o in rows if o['source_id']==x))
  if not date:
   years=re.findall(r'20\d{2}',src['title'])
   date=('edition '+years[-1]) if years else ('edition '+', '.join(vintages))
  pubs.setdefault(src['publisher'],[]).append(date)
 names='; '.join(f"{pub} ({'; '.join(dict.fromkeys(dates))})" for pub,dates in pubs.items())
 if len(names)>220:names='; '.join(unique)+'. Full citations in country notes.'
 periods='; '.join(dict.fromkeys(o['observation_period'] for o in rows))
 if len(periods)>180:periods=f"{min(o['year'] for o in rows if o.get('year'))}-{max(o['year'] for o in rows if o.get('year'))}; exact periods and vintages in plotted_values.csv."
 sample=''
 if s['measure']=='employment_stock' and s['population_basis']=='survey_sample' and s.get('sample_size') not in (None,'unknown','See observations.'):
  sample=' Sample: '+s['sample_size'].rstrip('.')+'.'
 return f"{s['population_basis'].replace('_',' ')}.{sample} {s['comparison_note'].rstrip('.')} .\nObserved: {periods.rstrip('.')}.\nSources: {names}.".replace(' .','.')
def mark(chart,c,s,o,kind='native',v=None):
 PLOTTED.append(dict(chart_id=chart,iso2=c['iso2'],country=c['country'],series_id=s['series_id'],observation_id=o['observation_id'],source_id=o['source_id'],year=o.get('year'),observation_period=o['observation_period'],value=o.get('value') if v is None else v,value_low=o.get('value_low'),value_high=o.get('value_high'),unit=s['unit'] if kind=='native' else 'index_2019_100',transform=kind,source_locator=o['source_locator']))
def plot_panel(ax,c,s,rows,chart,composition=False):
 ax.set_title(wrap(s['label'],55 if composition else 68),loc='left',fontsize=16,fontweight='bold',pad=17)
 unit=UNIT.get(s['unit'],s['unit'].replace('_',' '))
 noyears=any(o.get('year') is None for o in rows)
 cat=composition or noyears or len({o.get('year') for o in rows})<len(rows)
 if cat:
  yy=list(range(len(rows)));labels=[]
  for i,o in enumerate(rows):
   col=ORANGE if o.get('method_break') else BLUE
   lab=o.get('display_period') or str(o.get('year') or ('Year unspecified; '+str(o['publication_vintage'])))
   if o.get('subgroup') not in (None,'all'):lab+=' · '+str(o['subgroup'])
   labels.append(wrap(lab,29))
   lo=o.get('value_low');hi=o.get('value_high');v=o.get('value')
   if v is not None:
    ax.scatter(v,i,color=col,s=60,zorder=3,clip_on=False)
    if lo is not None and hi is not None:ax.hlines(i,lo,hi,color=col,lw=2);ax.plot([lo,hi],[i,i],linestyle='',marker='|',color=col,ms=10)
   elif lo is not None and hi is not None:ax.plot([lo,hi],[i,i],color=col,lw=5,solid_capstyle='butt');ax.plot([lo,hi],[i,i],linestyle='',marker='|',color=col,ms=12)
   else:ax.scatter(lo if lo is not None else hi,i,color=col,marker='>' if lo is not None else '<',s=85,clip_on=False)
   end=v if v is not None else hi if hi is not None else lo
   ax.annotate(sval(o), (end,i),xytext=(9,0),textcoords='offset points',va='center',fontsize=11)
   mark(chart,c,s,o)
  ax.set_yticks(yy,labels);ax.invert_yaxis();ax.set_ylim(len(rows)-.5,-.6)
  largest=max(max(o.get('value') or 0,o.get('value_high') or 0,o.get('value_low') or 0) for o in rows)
  ax.set_xlim(0,100 if s['unit']=='percent' else max(largest*1.3,1));ax.set_xlabel(unit);ax.xaxis.grid(True,color=GRID,lw=.7);ax.xaxis.set_major_formatter(FuncFormatter(lambda v,_:fmt(v)))
 else:
  years=[o['year'] for o in rows];vals=[o.get('value') for o in rows];mx=max(max(o.get('value') or 0,o.get('value_high') or 0,o.get('value_low') or 0) for o in rows)
  for i,o in enumerate(rows):
   y=o['year'];v=o.get('value');lo=o.get('value_low');hi=o.get('value_high')
   col=ORANGE if o.get('method_break') else BLUE
   if v is not None:
    ax.scatter(y,v,color=col,s=62,zorder=4)
    if lo is not None and hi is not None:ax.vlines(y,lo,hi,color=col,lw=1.6);ax.plot([y,y],[lo,hi],linestyle='',marker='_',color=col,ms=9)
    if i and s.get('trend_eligible') and not o.get('method_break') and years[i-1]==y-1 and vals[i-1] is not None:ax.plot(years[i-1:i+1],vals[i-1:i+1],color=BLUE,lw=2,zorder=2)
    interval=lo is not None and hi is not None
    ax.annotate(sval(o),(y,v),xytext=(9,0) if interval else (0,12 if i%2==0 or len(rows)<7 else -22),textcoords='offset points',ha='left' if interval else 'center',va='center' if interval else 'baseline',fontsize=11)
   elif lo is not None and hi is not None:ax.vlines(y,lo,hi,color=col,lw=5);ax.annotate(sval(o),(y,hi),xytext=(0,10),textcoords='offset points',ha='center',fontsize=11)
   else:ax.scatter(y,lo if lo is not None else hi,color=col,marker='^' if lo is not None else 'v',s=70);ax.annotate(sval(o),(y,lo or hi),xytext=(0,10),textcoords='offset points',ha='center')
   mark(chart,c,s,o)
  ax.set_ylim(0,max(mx*1.28,1));ax.set_xlim(min(years)-.65,max(years)+.65 if max(years)>min(years) else min(years)+.65)
  ticks=list(range(min(years),max(years)+1));display={o['year']:o.get('display_period',str(o['year'])) for o in rows};ax.set_xticks(ticks,[wrap(display.get(y,str(y)),15) for y in ticks]);ax.tick_params(axis='x',labelsize=11)
  ax.set_ylabel(wrap(unit,27),fontsize=11);ax.yaxis.grid(True,color=GRID,lw=.7);ax.yaxis.set_major_locator(MaxNLocator(5));ax.yaxis.set_major_formatter(FuncFormatter(lambda v,_:fmt(v)))
  ax.set_xlabel('Observation year; missing years remain gaps',fontsize=11)
 ax.tick_params(axis='both',length=0,pad=8);ax.set_axisbelow(True)
 note=source_note(c,s,rows)
 if any(o.get('method_break') for o in rows):note='Orange point: source or methodology break. '+note
 if any(o.get('value') is not None and o.get('value_low') is not None and o.get('value_high') is not None for o in rows):note=('Whiskers show 95% confidence intervals; points are estimates. ' if s['series_id']=='GB_dcms_jobs' else 'Whiskers show the source-reported interval; point is the central estimate. ')+note
 if composition:
  den='; '.join(dict.fromkeys(o['denominator'] for o in rows))
  note='Denominator: '+(den.rstrip('.')+'.' if len(den)<230 else den[:227]+'... See country notes.')+'\n'+note
 return note
def savefig(fig,id,country,label,series_ids):
 for ext in ['png','svg']:fig.savefig(R/'charts'/f'{id}.{ext}',dpi=180,facecolor=BG)
 plt.close(fig);FIGURES.append(dict(chart_id=id,country=country,label=label,series_ids=series_ids,png=f'charts/{id}.png',svg=f'charts/{id}.svg'))
def country_figures(c):
 stock=[];comp=[]
 for s in c['series']:
  if not s.get('chart_eligible') or not rows_for(c,s):continue
  (comp if s['measure']!='employment_stock' else stock).append(s)
 for group,items in [('employment',stock),('composition',comp)]:
  # One series per image when labels/denominators are dense; keeps exports legible.
  for i,s in enumerate(items):
   rows=rows_for(c,s);id=f"{c['iso2'].lower()}-{group}-{i+1:02d}"
   fig=plt.figure(figsize=(12,10));fig.text(.075,.942,c['country'],fontsize=27,fontweight='bold');fig.text(.075,.895,'Employment evidence' if group=='employment' else 'Workforce composition',fontsize=15,color=MUTED)
   left=.3 if group=='composition' or any(o.get('year') is None for o in rows) else .14
   ax=fig.add_axes([left,.35,.91-left,.44]);note=plot_panel(ax,c,s,rows,id,group=='composition')
   text=wrap(note,122)
   # Footnotes are separated from axes; detailed definitions remain in linked country notes.
   if len(text.splitlines())>10:text='\n'.join(text.splitlines()[:9])+'\nFull definitions, denominators and source locators: country notes + linked CSV tables.'
   fig.text(.075,.245,text,fontsize=10.4,va='top',linespacing=1.4)
   fig.text(.075,.04,'GAME EMPLOYMENT  ·  SOURCES AVAILABLE BY 9 SEP 2026',fontsize=9,color=MUTED)
   fig.text(.925,.04,id,fontsize=9,color=MUTED,ha='right')
   savefig(fig,id,c['country'],s['label'],[s['series_id']])
def index_figure(countries):
 eligible=[];indexrows=[]
 for c in countries:
  for s in c['series']:
   if not s.get('index_2019_eligible'):continue
   rows=rows_for(c,s);base=[o for o in rows if o.get('year')==2019 and o.get('value') is not None]
   if len(base)!=1 or base[0]['value']<=0 or len({o.get('year') for o in rows})<3 or any(o.get('method_break') for o in rows if o.get('year',0)>=2019) or s['geography_basis'] not in ('domestic_workplace','domestic_residence'):
    WARN.append(f"Index rejected despite packet flag: {s['series_id']}");continue
   rows=[o for o in rows if o.get('year') is not None and o['year']>=2019 and o.get('value') is not None];b=base[0]['value'];eligible.append((c,s,rows,b))
   for o in rows:indexrows.append(dict(iso2=c['iso2'],country=c['country'],series_id=s['series_id'],year=o['year'],native_value=o['value'],native_unit=s['unit'],baseline_2019=b,index_2019_100=o['value']/b*100,observation_id=o['observation_id'],source_id=o['source_id']))
 csvout('indexed_2019.csv',indexrows,['iso2','country','series_id','year','native_value','native_unit','baseline_2019','index_2019_100','observation_id','source_id'])
 for batch in range(0,len(eligible),6):
  subset=eligible[batch:batch+6];fig=plt.figure(figsize=(12,10));fig.text(.075,.94,'Games employment, indexed to 2019',fontsize=25,fontweight='bold');fig.text(.075,.895,'2019 = 100 · Within-series changes; country definitions still differ',fontsize=14,color=MUTED)
  mx=max(o['value']/b*100 for _,_,rows,b in subset for o in rows)*1.18;id=f'comparison-2019-{batch//6+1:02d}'
  gs=fig.add_gridspec(math.ceil(len(subset)/2),2,left=.1,right=.92,bottom=.23,top=.81,hspace=.55,wspace=.32)
  for i,(c,s,rows,b) in enumerate(subset):
   ax=fig.add_subplot(gs[i//2,i%2]);ax.axhline(100,color=MUTED,lw=.8,ls=':');last=None
   for o in rows:
    y=o['year'];v=o['value']/b*100;ax.scatter(y,v,color=BLUE,s=25)
    if last and last[0]==y-1:ax.plot([last[0],y],[last[1],v],color=BLUE,lw=1.7)
    last=(y,v);mark(id,c,s,o,'native_value / verified_2019_value * 100',v)
   ax.annotate(fmt(last[1]),last,xytext=(6,0),textcoords='offset points',fontsize=10)
   ax.set_ylim(0,mx);ax.set_xlim(2018.8,max(o['year'] for o in rows)+.6);ax.set_title(c['country'],loc='left',fontsize=14,fontweight='bold');ax.set_xticks([2019,max(o['year'] for o in rows)]);ax.yaxis.set_major_locator(MaxNLocator(4));ax.grid(axis='y',color=GRID,lw=.5);ax.tick_params(length=0,labelsize=10)
  labels='; '.join(f"{c['country']}: {s['unit'].replace('_',' ')}" for c,s,_,_ in subset)
  fig.text(.075,.125,wrap(labels,120)+'\nSource-native denominators; no shared country total. Full eligibility and citations in linked CSV tables.',fontsize=10,va='top');fig.text(.075,.04,'GAME EMPLOYMENT  ·  SOURCES AVAILABLE BY 9 SEP 2026',fontsize=9,color=MUTED)
  savefig(fig,id,'Comparison','Verified 2019 index',[s['series_id'] for _,s,_,_ in subset])
 return indexrows
def validate(countries,final):
 found={c['iso2'] for c in countries}
 if final:assert found==EXPECTED,f'Missing countries: {EXPECTED-found}; unexpected {found-EXPECTED}'
 ids=[]
 for c in countries:
  for field in ['sources','series','observations','search_log','limitations','review_checks']:assert isinstance(c[field],list),(c['iso2'],field)
  assert c['research_complete'],c['iso2']
  routes={s['route'] for s in c['search_log']};assert {'statistical_agency','industry_association','local_language'}<=routes,(c['iso2'],routes)
  sources={s['source_id']:s for s in c['sources']};series={s['series_id']:s for s in c['series']}
  for s in c['sources']:
   date=s.get('publication_date')
   if date and re.match(r'^\d{4}-\d{2}-\d{2}$',date):assert date<='2026-09-09',(s['source_id'],date)
   if s.get('local_file') and not (R/s['local_file']).exists():WARN.append('Missing local snapshot: '+s['source_id']+' '+s['local_file'])
  for o in c['observations']:
   assert o['series_id'] in series and o['source_id'] in sources,(c['iso2'],o)
   assert o.get('source_locator') and o.get('observation_period') and o.get('denominator'),o
   y=o.get('year');assert y is None or 2015<=y<=2026,(c['iso2'],o)
   lo=o.get('value_low');hi=o.get('value_high');v=o.get('value');assert lo is None or hi is None or lo<=hi,o
   assert all(x is None or math.isfinite(x) for x in (lo,hi,v)),o
   if series[o['series_id']]['measure']=='employment_stock' or series[o['series_id']]['unit']=='percent':assert all(x is None or x>=0 for x in (lo,hi,v)),o
   if v is not None:assert (lo is None or lo<=v) and (hi is None or v<=hi),o
   if series[o['series_id']]['unit']=='percent':assert all(x is None or 0<=x<=100 for x in (lo,hi,v)),o
   if o['status']=='derived':assert o.get('derivation'),o
   ids.append(o['observation_id'])
 assert len(ids)==len(set(ids)),'Duplicate observation IDs'
 CHECKS.extend(['Country set and three research routes per country checked.' if final else 'Partial build: country set not yet complete.','Source and series foreign keys checked.','Observation years, known publication dates, ranges, percentages and derivation fields checked.','Forecasts and non-preferred vintages excluded from chart selection.','Every plotted value written to plotted_values.csv.'])
def country_notes(c):
 lines=[f"# {c['country']}",'',c['summary'],'',f"Coverage: {c['coverage_status']}. Research cutoff: 9 September 2026.",'','## Definitions','']
 for s in c['series']:
  lines += [f"### {s['label']} ({s['series_id']})",'',f"Unit: {s['unit']}. Population: {s['population_basis']}. Geography: {s['geography_basis']}.",'']
  for key in ['industry_scope','occupation_scope','geography_note','worker_status','contractor_treatment','nationality_definition','method','sample_size','comparison_note']:
   lines.append(f"- **{key.replace('_',' ')}:** {s.get(key,'unknown')}")
  lines += ['','| Period | Value | Status / selection | Source |','| --- | ---: | --- | --- |']
  sources={x['source_id']:x for x in c['sources']}
  for o in [x for x in c['observations'] if x['series_id']==s['series_id']]:
   src=sources[o['source_id']];lines.append(f"| {o['observation_period']} | {sval(o)} | {o['status']}; {'preferred' if o.get('preferred') else 'retained only'} | [{o['source_id']}]({src['url']}), {o['source_locator']} |")
  lines += ['']
  for o in [x for x in c['observations'] if x['series_id']==s['series_id']]:
   extras=' '.join(str(o.get(k,'')) for k in ['notes','derivation','break_note']).strip()
   if extras:lines += [f"- {o['observation_id']}: {extras}"]
  lines+=['']
 lines += ['## Limits','']+[f'- {x}' for x in c['limitations']]+['','## Source checks','']
 for x in c['search_log']:lines += [f"- **{x['route']}**: {x['query']}. [Source]({x['url']}). {x['outcome']}"]
 lines += ['','## Citations','']
 for s in c['sources']:lines += [f"- **{s['source_id']}**: [{s['title']}]({s['url']}). {s['publisher']}; published {s.get('publication_date') or 'exact date unresolved'}. {s['locator']}. {s.get('notes','')}"]
 lines += ['','## Extraction review','']+[f'- {x}' for x in c['review_checks']]
 (R/'country-notes'/f"{c['iso2']}.md").write_text('\n'.join(lines)+'\n')
def contacts():
 for batch in range(0,len(FIGURES),6):
  subset=FIGURES[batch:batch+6];sheet=Image.new('RGB',(2160,2760),'#e8e8e5');d=ImageDraw.Draw(sheet)
  for i,f in enumerate(subset):
   im=Image.open(R/f['png']).convert('RGB');im.thumbnail((1080,900));x=(i%2)*1080;y=(i//2)*920;sheet.paste(im,(x,y));d.text((x+20,y+900),f['chart_id'],fill='black')
  sheet.save(R/'qa'/f'contact-{batch//6+1:02d}.jpg',quality=95)
def main():
 for d in ['charts','country-notes','qa']: (R/d).mkdir(exist_ok=True)
 countries=[json.loads(p.read_text()) for p in sorted((R/'country').glob('*.json'))];validate(countries,'--final' in sys.argv)
 defs=[];obs=[];sources=[];elig=[];coverage=[];searches=[]
 for c in countries:
  for s in c['series']:defs.append(dict(iso2=c['iso2'],country=c['country'],**s));elig.append(dict(iso2=c['iso2'],country=c['country'],series_id=s['series_id'],chart_eligible=s['chart_eligible'],trend_eligible=s['trend_eligible'],index_2019_eligible=s['index_2019_eligible'],comparison_target='Domestic development and publishing employment',geography_basis=s['geography_basis'],population_basis=s['population_basis'],unit=s['unit'],assessment=s['comparison_note']))
  for o in c['observations']:obs.append(dict(iso2=c['iso2'],country=c['country'],**o))
  for s in c['sources']:sources.append(dict(iso2=c['iso2'],**s))
  for s in c['search_log']:searches.append(dict(iso2=c['iso2'],**s))
  ys=sorted({o['year'] for o in c['observations'] if o.get('year') is not None and o.get('status')!='forecast'})
  coverage.append(dict(iso2=c['iso2'],country=c['country'],priority=c['priority'],coverage_status=c['coverage_status'],observations=len(c['observations']),first_observed_year=min(ys) if ys else None,last_observed_year=max(ys) if ys else None,years_observed=';'.join(map(str,ys)),preferred_series_ids=';'.join(c['preferred_series_ids']),summary=c['summary'],research_complete=c['research_complete']))
  country_notes(c);country_figures(c)
 indexrows=index_figure(countries)
 for name,rows in [('observations.csv',obs),('source_definitions.csv',defs),('citations.csv',sources),('comparison_eligibility.csv',elig),('country_coverage.csv',coverage),('search_log.csv',searches),('plotted_values.csv',PLOTTED),('chart_manifest.csv',FIGURES)]:csvout(name,rows)
 summary=dict(countries=len(countries),observations=len(obs),series=len(defs),sources=len(sources),charts=len(FIGURES),plotted_marks=len(PLOTTED),indexed_series=len(set(x['series_id'] for x in indexrows)),validation_checks=CHECKS,warnings=WARN)
 (R/'validation.json').write_text(json.dumps(summary,indent=2)+'\n');contacts()
 lines=['# Country games employment','',f"Evidence for {len(countries)} countries, {len(obs)} observations and {len(sources)} sources. Published by 9 September 2026; employment periods are kept separate from release dates.",'','[Findings memo](findings.md) · [Data dictionary](data-dictionary.md) · [Country coverage CSV](country_coverage.csv) · [Comparison eligibility](comparison_eligibility.csv)','','## Country coverage','','| Country | Evidence | Years with observations | Notes |','| --- | --- | --- | --- |']
 for c in coverage:lines.append(f"| {c['country']} | {c['coverage_status'].replace('_',' ')} | {c['years_observed'] or 'No dated estimate'} | [Definitions and citations](country-notes/{c['iso2']}.md) |")
 lines+=['','## Chart library','','Each PNG has an editable SVG. Separate measures are not additive. Source details for every mark are in [plotted_values.csv](plotted_values.csv).','']
 for f in sorted(FIGURES,key=lambda f:(f['country']!='Comparison',f['country'],f['chart_id'])):
  lines += [f"### {f['country']}: {f['label']}",'',f"[PNG]({f['png']}) · [Editable SVG]({f['svg']})",'',f"![{f['country']}: {f['label']}]({f['png']})",'']
 lines+=['','## Reproducing the exports','','The reviewed country/*.json packets are authoritative inputs. Use Python 3.11 with Matplotlib 3.10.8 and Pillow: `python3.11 scripts/build_country_library.py --final`, then `python3.11 scripts/verify_final_outputs.py`. Initial extraction helpers predate final manual reviews and should not overwrite curated packets.']
 if (R/'qa/final-output-validation.json').exists():
  lines+=['','## Verification','','[Final output validation](qa/final-output-validation.json) · [Europe and primary-researcher chart review](qa/chart-review-europe-root.md) · [Americas and Asia chart review](qa/chart-review-americas-asia.md)']
 (R/'README.md').write_text('\n'.join(lines)+'\n');print(json.dumps(summary,indent=2))
if __name__=='__main__':main()
