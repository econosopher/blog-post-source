from pathlib import Path
import json
import matplotlib.pyplot as plt
R=Path(__file__).resolve().parent;E=R.parent/'research-extension-2026-09-19/FI'
a=json.loads((E/'recheck-2026-09-20/historical-worldwide-observations.json').read_text())
fig,axes=plt.subplots(1,2,figsize=(13,5.7),gridspec_kw={'width_ratios':[3,1]})
fig.subplots_adjust(left=.07,right=.96,top=.74,bottom=.27,wspace=.32)
fig.text(.07,.92,'Finland has a long-running industry survey',fontsize=22,weight='bold')
fig.text(.07,.85,'Neogames reports almost every other year. Two geographic definitions are shown separately.',fontsize=11,color='#52616b')
for ax in axes:
 ax.spines[['top','right']].set_visible(False);ax.grid(axis='y',alpha=.18);ax.set_ylim(0,5000);ax.set_yticks([0,1000,2000,3000,4000,5000]);ax.tick_params(labelsize=10)
axes[0].plot([o['year'] for o in a],[o['value'] for o in a],'-o',color='#4b7055',lw=1.6,ms=5)
axes[0].set_title('Finnish studios: publisher’s retrospective history',loc='left',fontsize=12,pad=15)
axes[0].set_xticks([2004,2008,2012,2016,2020,2024]);axes[0].set_ylabel('Reported employment')
for o in a:axes[0].annotate(f"{o['value']:,}",(o['year'],o['value']),xytext=(0,10),textcoords='offset points',ha='center',fontsize=8)
axes[1].plot([2022,2024],[3700,3800],'-s',color='#a26a35',lw=1.6,ms=6);axes[1].set_xticks([2022,2024]);axes[1].set_xlim(2021.3,2024.7);axes[1].set_title('Located in Finland: FTE',loc='left',fontsize=12,pad=15)
for y,v in [(2022,3700),(2024,3800)]:axes[1].annotate(f'≈{v:,}',(y,v),xytext=(0,12),textcoords='offset points',ha='center',fontsize=10)
fig.text(.07,.16,'Left: includes overseas employees and entrepreneurs; historical units/coverage vary. Earlier-year geography is not fully documented.',fontsize=9,color='#52616b')
fig.text(.07,.115,'Right: explicit domestic FTE estimates. Do not splice the two series or infer annual growth between survey editions.',fontsize=9,color='#52616b')
fig.text(.07,.07,'Source: Neogames, The Game Industry of Finland 2024, pp. 3, 12–13; 2022 report, pp. 20–21. Underlying country notes retain earlier editions.',fontsize=9,color='#52616b')
for ext in ['png','svg']:fig.savefig(R/f'finland-survey-history.{ext}',dpi=170,facecolor='white')
assert len(a)==10 and a[0]['value']==600 and a[-1]['value']==4300
print('PASS Finland companion:10 retrospective observations;2 explicit domestic FTE estimates')
