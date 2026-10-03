"""Export explicitly reviewed comparison segments; never infer joins from country alone."""
import csv,json
from pathlib import Path
C=Path(__file__).resolve().parent
R=C.parent; E=R/'research-extension-2026-09-19'
policies=[
 ('ES','ES_direct_reviewed',['ES_direct'],list(range(2013,2025)),'people','DEV direct development employment. Historical chart matches overlapping original values; changing annual survey coverage. Excludes freelance/indirect totals and forecasts.'),
 ('RO','RO_collaborator_inclusive_reviewed',['RO_employees_collaborators'],[2021,2022,2023],'people','Employees and collaborators, legal-status split undisclosed. No join to 2020 qualified headline or 2024 employee-only series.'),
 ('SE', 'SE_domestic_gdi2020_vintage', ['SE_domestic_gdi2020_vintage'], [2012, 2013, 2014], 'reported full-time positions', 'GDI2020 Sweden-based employment historical segment only. Annual-account full-time averages with report timing qualifications, not uniform year-end counts. No splice into latest vintage; overlap2015 differs.'),
 ('DK', 'DK_core_game_producers_v2009', ['DK_core_game_producers_v2009'], [2008, 2009], 'FTE', 'Source-native2008/2009 core game-producer payroll FTE pair. Selected company population excludes freelancers/support firms. No joins to later report vintages.'),
 ('FR','FR_dge_pwc_broad_industry_persons',['FR_dge_pwc_broad_industry_persons'],list(range(2010,2019)),'persons','PwC actor-database broad ecosystem employment: studios, publishers, distributors and other actors. Source-native 2010-2018 comparison only; not FTE. No join to EY2019-2024. Exact within-year timing and independent-worker coverage unstated; retain printed totals despite component discrepancies.'),
 ('CA','CA_statcan_ilu',['CA_statcan_ilu'],list(range(2013,2023)),'ILU','Annual individual labour units; provinces only. Not FTE or headcount. Table values override contradictory narrative.'),
 ('DE','DE_core_revised2026',['DE_core_revised2026'],[2025,2026],'people','Revised 2025 comparator and 31 March 2026 only; do not join older unrevised series.'),
 ('FI','FI_domestic_fte_reviewed',['FI_domestic_explicit_fte','FI_domestic_2024_fte'],[2022,2024],'FTE','Approximate domestic FTE incl entrepreneurs; changing interview coverage. Overseas excluded.'),
 ('GB','GB_tiga_fte',['GB_tiga_fte'],[2016,2017,2018,2020,2021,2023,2024],'FTE','Development/production support; prorated freelancers. Snapshot dates; no headcount or DCMS joins.'),
 ('GB','GB_tiga_people',['GB_tiga_people'],[2017,2018,2020,2023,2024,2025],'people','Development workforce incl contractors, without FTE prorating. Snapshot dates; no FTE or DCMS joins.'),
 ('JP','JP_census_3914_required_items',['JP_census_3914_required_items'],[2012,2016],'persons engaged','JSIC3914 establishments with required responses; management-only sites excluded. 1 Feb 2012 / 1 Jun 2016. Not whole games industry.'),
 ('US','US_legacy_siwek_all_establishments',['US_legacy_siwek_all_establishments'],[2009,2012],'people','ESA2014 modeled all-worker pair only. Keep reported 42975 despite component discrepancy. No other ESA vintage joins.'),
 ('US','US_mapping_2017_2013_2015',['US_mapping_2017'],[2013,2014,2015],'people','Source mapping history with changing coverage; excludes missing employment/founding-year data. Do not connect to wider 2016 universe.')
]
rows=[]; output_policies=[]
for iso,segment,sids,years,unit,limits in policies:
 d=json.loads((R/f'country/{iso}.json').read_text()); sources={s['source_id']:s for s in d['sources']}
 accepted=json.loads((E/f'{iso}/accepted-observations.json').read_text())
 candidates=[(o,'original') for o in d['observations']]+[(o,'accepted extension') for o in accepted]
 chosen=[(o,layer) for o,layer in candidates if o['series_id'] in sids and o.get('year') in years and o.get('subgroup','all')=='all']
 assert sorted(o['year'] for o,layer in chosen)==years,(segment,chosen)
 output_policies.append(dict(segment_id=segment,iso2=iso,series_ids=sids,allowed_years=years,unit=unit,constraints=limits,review_file=str((E/f'{iso}/trend-review.json').relative_to(R))))
 for o,layer in sorted(chosen,key=lambda x:x[0]['year']):
  src=sources.get(o.get('source_id'),{})
  row=dict(segment_id=segment,country=d['country'],iso2=iso,series_id=o['series_id'],year=o['year'],observation_period=o.get('observation_period',o.get('period','')),value=o['value'],unit=unit,status=o.get('status',''),source_id=o.get('source_id',''),url=o.get('url') or src.get('url',''),source_locator=o.get('source_locator',o.get('locator','')),source_layer=layer,observation_id=o.get('observation_id',f"{o['series_id']}_{o['year']}"),constraints=limits,notes=o.get('notes',''))
  assert row['url'],row
  rows.append(row)
(C/'reviewed-comparison-segments.json').write_text(json.dumps({'scope':'Thirteen explicitly reviewed extension comparison segments, not a complete country inventory. Connect only within segment_id; no annual interpolation or regional sums.','segments':output_policies,'observations':rows},ensure_ascii=False,indent=2)+'\n')
with (C/'reviewed-comparison-observations.csv').open('w',encoding='utf-8-sig',newline='') as f:
 w=csv.DictWriter(f,fieldnames=list(rows[0]));w.writeheader();w.writerows(rows)
assert len(rows)==63 and len(output_policies)==13
assert not any(r['iso2']=='US' and r['year']==2016 for r in rows)
(C/'reviewed-segments-validation.json').write_text(json.dumps({'result':'PASS','segments':13,'observations':len(rows),'countries':11,'unreviewed_joins':0,'US_2016_excluded':True,'original_packets_modified':False},indent=2)+'\n')
print(f'PASS: {len(rows)} observations in 13 explicitly bounded comparison segments.')
