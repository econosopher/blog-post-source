from pathlib import Path
import csv,json
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
D=Path(__file__).resolve().parent
r=list(csv.DictReader((D/'interactive-media-jobs.csv').open()));a=[(int(x['REF_DATE'][:4])+(int(x['REF_DATE'][5:])-1)/12,int(x['VALUE'])) for x in r]
plt.rcParams.update({'font.family':'DejaVu Sans','axes.spines.top':False,'axes.spines.right':False})
f,ax=plt.subplots(figsize=(12,6));f.subplots_adjust(left=.09,right=.94,bottom=.25,top=.76)
f.text(.09,.91,'Canada: interactive-media jobs',fontsize=23,weight='bold');f.text(.09,.85,'Quarterly, seasonally adjusted · Games and related digital edutainment',fontsize=12,color='#52616b')
for low,high,color in [(2012,2016,'#8f9ca5'),(2016,2027,'#326b88')]:
 q=[(x,y) for x,y in a if low<=x<high];ax.plot(*zip(*q),color=color,lw=2,marker='o',markersize=3)
ax.axvline(2015.875,color='#ae7342',linestyle=':',lw=1.3);ax.text(2016.05,44500,'2016 measurement break\nNot a real employment decline',fontsize=10,color='#986239')
ax.set(xlim=(2011.8,2026.5),ylim=(0,48000),ylabel='Jobs',xticks=list(range(2012,2027,2)));ax.yaxis.set_major_formatter(FuncFormatter(lambda x,p:f'{x/1000:.0f}k'));ax.grid(axis='y',color='#dee3e7',lw=.7);ax.set_axisbelow(True)
ax.annotate(f'{a[-1][1]:,}\nQ1 2026',xy=a[-1],xytext=(-5,-35),textcoords='offset points',ha='right',fontsize=10,color='#326b88')
f.text(.09,.15,'Jobs count employees, self-employed and unpaid family workers; part-time jobs are not prorated. This is not ILU or FTE.',fontsize=9,color='#52616b');f.text(.09,.11,'Product-based estimates; no join to the annual enterprise series. Historical values may be revised.',fontsize=9,color='#52616b');f.text(.09,.07,'Statistics Canada 36-10-0652-01 · vector v1277496929 · retrieved 19 September 2026',fontsize=9,color='#52616b')
for ext in ['png','svg']:f.savefig(D/f'interactive-media-quarterly.{ext}',dpi=160,facecolor='white')
assert len(a)==57 and sum(x<2016 for x,y in a)==16 and sum(x>=2016 for x,y in a)==41
(D/'validation.json').write_text(json.dumps({'result':'PASS','observations':57,'unique_quarters':57,'segments':2,'connections_across2016_break':0,'annual_series_modified':False},indent=2)+'\n')
print('PASS:57 quarterly jobs observations; two segments, no connection across2016 break.')
