# Reproduce the employment archive

From the repository root, with Python 3.14:

```sh
python3 -m venv .venv
.venv/bin/python -m pip install -r 2026-09-07-gaming-crash/employment/requirements.txt
.venv/bin/python 2026-09-07-gaming-crash/employment/reproduce.py
```

Dependencies are pinned to the versions used for the 3 October 2026 rebuild (Python 3.14). After installing them, reproduction uses only the checked-in research data. No mounted volume, account, API key, network fetch or third-party report download is needed. Paths resolve from the scripts, so the runner also works from another current directory.

The runner rebuilds the 79 country charts, source catalog, reviewed comparison exports, regional overview, Finland companion, US history and Canada quarterly companion, then runs the existing observation/provenance, image-integrity and connection-policy validators. CSV data are explicitly included in Git.

Original country packets retain the September 9 research vintage. Later reviews, accepted additions, suspended series and chart eligibility remain separate. Rebuilding does not silently rewrite the original packets. Archived fetch/extraction scripts are research tools, not required reproduction steps.

The country builder reports missing local source captures because third-party downloads are intentionally excluded. These warnings concern optional archived publications; source URLs and exact numeric inputs are included. They do not indicate missing chart inputs. Historical chart review notes describe the original review; current hashes are recorded by the reproduction validator. Image bytes may vary with operating-system fonts or rendering libraries.
