from pathlib import Path
from concurrent.futures import ThreadPoolExecutor
import requests, json
from html.parser import HTMLParser
class TextParser(HTMLParser):
    def __init__(self): super().__init__(); self.text=[]; self.hidden=0
    def handle_starttag(self,t,a):
        if t in ('script','style'): self.hidden+=1
    def handle_endtag(self,t):
        if t in ('script','style'): self.hidden=max(0,self.hidden-1)
    def handle_data(self,s):
        if not self.hidden and s.strip(): self.text.append(s.strip())
ROOT=Path(__file__).resolve().parents[1]
SOURCES={
'TR/invest2025.pdf':'https://www.invest.gov.tr/tr/library/publications/lists/investpublications/turk-oyun-ekosisteminin-gorunumu-2025.pdf',
'TR/market2025.pdf':'https://investgame.net/wp-content/uploads/2026/02/2026-02-17-trkiye_game_market_report_2025-compressed_wp.pdf',
'TR/archive.html':'https://www.turkiyeoyunsektoruraporu.com/tr/turkiye-oyun-sektoru-raporlari/',
'AU/infographic2025.pdf':'https://igea.net/wp-content/uploads/2026/03/AGD-2025-Standalone-Infographic.pdf',
'AU/abs2022.html':'https://www.abs.gov.au/statistics/industry/technology-and-innovation/film-television-and-digital-games-australia/latest-release',
'AU/abs-method2022.html':'https://www.abs.gov.au/methodologies/film-television-and-digital-games-australia-methodology/2021-22-financial-year',
'NZ/mbie-review.pdf':'https://www.mbie.govt.nz/dmsdocument/31198-game-development-sector-rebate-year-two-review',
}
for y in (2015,2016,2017,2018,2019,2020,2021): SOURCES[f'NZ/survey{y}.html']=f'https://www.nzgda.com/blog/news/survey{y}'
for y in (2022,2023): SOURCES[f'NZ/survey{y}.html']=f'https://www.nzgda.com/blog/news/nz-interactive-media-industry-survey-{y}'
SOURCES['NZ/survey2024.html']='https://www.nzgda.com/blog/news/2024-industry-survey-results'
SOURCES['NZ/survey2025.html']='https://www.nzgda.com/blog/new-zealand-game-development-industry-breaks-records'
def fetch(kv):
    name,url=kv; p=ROOT/'raw'/name; p.parent.mkdir(parents=True,exist_ok=True)
    try:
        r=requests.get(url,headers={'User-Agent':'Mozilla/5.0'},timeout=90)
        valid=r.ok and (not name.endswith('.pdf') or r.content.startswith(b'%PDF'))
        if valid:
            p.write_bytes(r.content)
            if name.endswith('.html'):
                parser=TextParser(); parser.feed(r.text); p.with_suffix('.txt').write_text('\n'.join(parser.text))
        return {'file':name,'url':url,'status':r.status_code,'saved':valid,'bytes':len(r.content)}
    except Exception as e:return {'file':name,'url':url,'error':str(e)}
if __name__=='__main__':
    results=list(ThreadPoolExecutor(max_workers=5).map(fetch,SOURCES.items()))
    (ROOT/'raw/root-fetch-log.json').write_text(json.dumps(results,indent=2))
    print(json.dumps(results,indent=2))
