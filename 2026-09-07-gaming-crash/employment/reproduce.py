"""Rebuild the public archive from checked-in inputs, without network access."""
from pathlib import Path
import subprocess
import sys

ROOT = Path(__file__).resolve().parent
STEPS = [
    "countries/scripts/build_country_library.py",
    "countries/source-catalog/build_catalog.py",
    "countries/source-catalog/build_series_inventory.py",
    "countries/source-catalog/build_reviewed_segments.py",
    "countries/overview-2026-09-19/build_region_wrap.py",
    "countries/overview-2026-09-19/validate_region_wrap.py",
    "countries/overview-2026-09-19/build_finland_companion.py",
    "countries/research-extension-2026-09-19/build_history.py",
    "countries/research-extension-2026-09-19/CA/quarterly-culture-followup/build_chart.py",
    "countries/scripts/verify_final_outputs.py",
]
for step in STEPS:
    print(f"Running {step}", flush=True)
    subprocess.run([sys.executable, str(ROOT / step)], check=True, cwd=ROOT)
print("PASS: employment archive rebuilt and validated from repository inputs.")
