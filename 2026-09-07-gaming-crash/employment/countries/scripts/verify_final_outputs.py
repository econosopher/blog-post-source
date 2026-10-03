"""Validate the delivered CSV/PNG/SVG package and write an artifact manifest."""
from pathlib import Path
import csv, json, hashlib, math, re
from PIL import Image

R = Path(__file__).resolve().parents[1]
def table(name):
    return list(csv.DictReader((R / name).open(encoding='utf-8-sig')))
def number(value):
    return None if value in ('', None) else float(value)

observations = table('observations.csv')
definitions = table('source_definitions.csv')
sources = table('citations.csv')
charts = table('chart_manifest.csv')
marks = table('plotted_values.csv')
index = table('indexed_2019.csv')
obs = {x['observation_id']: x for x in observations}
defs = {x['series_id']: x for x in definitions}
src = {x['source_id']: x for x in sources}
assert len(obs) == len(observations)
assert len(defs) == len(definitions)
assert len(src) == len(sources)
selected = [(o['series_id'], o['observation_period'], o['subgroup']) for o in observations if o['preferred'] == 'True']
assert len(selected) == len(set(selected)), 'Duplicate preferred value within a measurement period and definition'
assert len({(m['chart_id'], m['observation_id']) for m in marks}) == len(marks)
assert {m['chart_id'] for m in marks} == {c['chart_id'] for c in charts}
for m in marks:
    o = obs[m['observation_id']]
    assert o['source_id'] == m['source_id'] and o['series_id'] == m['series_id']
    assert o['source_locator'] == m['source_locator'] and o['observation_period'] == m['observation_period']
    assert o['preferred'] == 'True' and o['status'] != 'forecast'
    if m['transform'] == 'native':
        assert m['unit'] == defs[o['series_id']]['unit']
        assert all(number(m[k]) == number(o[k]) for k in ('value', 'value_low', 'value_high'))
    else:
        ix = next(i for i in index if i['observation_id'] == m['observation_id'])
        assert math.isclose(number(m['value']), number(ix['index_2019_100']), abs_tol=1e-9)
for i in index:
    b = number(i['baseline_2019'])
    assert b > 0 and math.isclose(number(i['index_2019_100']), number(i['native_value']) / b * 100, abs_tol=1e-9)
    assert any(o['series_id'] == i['series_id'] and o['year'] == '2019' and o['preferred'] == 'True' and number(o['value']) == b for o in observations)

manifest = []
for c in charts:
    for ext in ('png', 'svg'):
        p = R / c[ext]
        assert p.is_file() and p.stat().st_size > 1000
        if ext == 'png':
            with Image.open(p) as im:
                assert im.size == (2160, 1800) and im.format == 'PNG'
                im.verify()
        else:
            text = p.read_text()
            assert '<svg' in text and '<text' in text, 'SVG must retain editable text'
        manifest.append({'chart_id': c['chart_id'], 'file': c[ext], 'bytes': p.stat().st_size, 'sha256': hashlib.sha256(p.read_bytes()).hexdigest()})
for p in [R/'README.md', R/'findings.md', R/'data-dictionary.md', *(R/'country-notes').glob('*.md')]:
    for target in re.findall(r'\]\(([^)]+)\)', p.read_text()):
        if '://' not in target and not target.startswith('#'):
            assert (p.parent / target.split('#')[0]).exists(), (p.name, target)
coverage = table('country_coverage.csv')
primary = set('US CA GB SE FI DE FR ES PL NL RO CY CN JP KR VN'.split())
assert len(coverage) == 31 and {c['iso2'] for c in coverage if c['priority'] == 'primary'} == primary
assert all(c['research_complete'] == 'True' for c in coverage)
for c in coverage:
    routes = {x['route'] for x in table('search_log.csv') if x['iso2'] == c['iso2']}
    assert {'statistical_agency', 'industry_association', 'local_language'} <= routes
result = {'result': 'PASS', 'countries': len(coverage), 'observations': len(observations), 'sources': len(sources), 'charts': len(charts), 'png_files': len(charts), 'editable_svg_files': len(charts), 'plotted_marks': len(marks), 'indexed_series': len({i['series_id'] for i in index}), 'checks': ['Unique source and definition keys; preferred observations unique within period and definition', 'Every plotted value agrees with its source observation or explicit index calculation', '2019 baselines verified; no forecasts or nonpreferred observations plotted', 'PNG dimensions and file integrity; SVG editable text; artifact links', 'All target countries and three source-search routes; requested priority groups']}
(R/'qa'/'final-output-validation.json').write_text(json.dumps(result, indent=2)+'\n')
(R/'qa'/'final-chart-manifest.json').write_text(json.dumps(manifest, indent=2)+'\n')
print(json.dumps(result, indent=2))
