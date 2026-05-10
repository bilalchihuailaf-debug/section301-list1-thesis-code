"""
Sanitised extraction-logic script for the thesis.

This file documents the main USITC DataWeb extraction workflow used in the project:
1. batching HTS8 codes,
2. querying the DataWeb API,
3. converting JSON responses into tabular form,
4. reshaping monthly values into long format,
5. saving raw, wide, long, and summary outputs.

Authentication details and API access tokens are excluded.
The variables `payload`, `headers`, `policy`, `raw_dir`, and `log_dir`
were defined in the original Colab notebook before running the extraction loop.
"""

import os
import json
import time
import requests
import pandas as pd

# Inputs assumed to be loaded before this excerpt:
# - payload: DataWeb query template
# - headers: API request headers
# - policy: coded List 1 policy universe
# - raw_dir: folder for raw and cleaned batch outputs
# - log_dir: folder for extraction logs

def run_dataweb_query(payload, headers,
                      url="https://datawebws.usitc.gov/dataweb/api/v2/report2/runReport",
                      timeout=120,
                      max_retries=6,
                      base_sleep=5):
    """
    Run a USITC DataWeb query with retry/backoff for temporary rate limits.
    """
    for attempt in range(max_retries):
        response = requests.post(url, headers=headers, json=payload, timeout=timeout)
        status = response.status_code

        if status == 200:
            return response.json()

        if status == 429:
            retry_after = response.headers.get("Retry-After")
            sleep_seconds = int(retry_after) if retry_after is not None else base_sleep * (2 ** attempt)
            time.sleep(sleep_seconds)
            continue

        response.raise_for_status()

    raise RuntimeError("Maximum retries exceeded after repeated rate-limit responses.")


def response_to_wide_df(resp_json):
    """
    Convert the nested DataWeb JSON response into a wide table.
    """
    table = resp_json["dto"]["tables"][0]
    row_group = table["row_groups"][0]
    rows = row_group["rowsNew"]

    group0_cols = table["column_groups"][0]["columns"]
    group1_cols = table["column_groups"][1]["columns"]
    all_cols = group0_cols + group1_cols
    col_names = [col["label"] for col in all_cols]

    records = []

    for row in rows:
        entries = row.get("rowEntries", [])
        values = []

        for entry in entries:
            if isinstance(entry, dict):
                if "value" in entry:
                    values.append(entry["value"])
                elif "displayValue" in entry:
                    values.append(entry["displayValue"])
                elif "formattedValue" in entry:
                    values.append(entry["formattedValue"])
                elif "label" in entry:
                    values.append(entry["label"])
                else:
                    values.append(str(entry))
            else:
                values.append(entry)

        if len(values) < len(col_names):
            values = values + [None] * (len(col_names) - len(values))
        elif len(values) > len(col_names):
            values = values[:len(col_names)]

        records.append(dict(zip(col_names, values)))

    return pd.DataFrame(records)


def wide_to_long_clean(df_wide):
    """
    Reshape monthly DataWeb output into an HTS8 × origin × month panel.
    """
    month_cols = [
        "January", "February", "March", "April", "May", "June",
        "July", "August", "September", "October", "November", "December"
    ]

    df_long = df_wide.melt(
        id_vars=["Country", "Year", "HTS Number"],
        value_vars=month_cols,
        var_name="month_name",
        value_name="import_value"
    )

    df_long = df_long.rename(columns={
        "Country": "origin",
        "Year": "year",
        "HTS Number": "hts8"
    })

    df_long["hts8"] = (
        df_long["hts8"]
        .astype(str)
        .str.replace(r"\D", "", regex=True)
        .str.zfill(8)
        .str[:8]
    )

    df_long["import_value"] = (
        df_long["import_value"]
        .astype(str)
        .str.replace(",", "", regex=False)
        .str.strip()
    )

    df_long["import_value"] = pd.to_numeric(
        df_long["import_value"],
        errors="coerce"
    ).fillna(0)

    df_long["month"] = pd.to_datetime(
        df_long["year"].astype(str) + "-" + df_long["month_name"],
        format="%Y-%B"
    )

    return df_long[["month", "hts8", "origin", "import_value"]]


def chunk_list(values, chunk_size):
    """
    Split a list into fixed-size batches.
    """
    for i in range(0, len(values), chunk_size):
        yield values[i:i + chunk_size]


all_codes = policy["hts8"].drop_duplicates().tolist()
years = ["2016", "2017", "2018", "2019"]
batch_size = 50

summary_records = []

for year in years:
    code_batches = list(chunk_list(all_codes, batch_size))

    for batch_num, batch_codes in enumerate(code_batches, start=1):

        payload_batch = json.loads(json.dumps(payload))

        payload_batch["searchOptions"]["componentSettings"]["years"] = [year]
        payload_batch["searchOptions"]["commodities"]["commodities"] = batch_codes
        payload_batch["searchOptions"]["commodities"]["commoditiesManual"] = ",".join(batch_codes)
        payload_batch["searchOptions"]["commodities"]["commoditiesExpanded"] = []

        try:
            response_json = run_dataweb_query(payload_batch, headers)

            df_wide = response_to_wide_df(response_json)
            df_long = wide_to_long_clean(df_wide)

            raw_json_path = os.path.join(raw_dir, f"batch_{year}_{batch_num:03d}_raw.json")
            wide_csv_path = os.path.join(raw_dir, f"batch_{year}_{batch_num:03d}_wide.csv")
            long_csv_path = os.path.join(raw_dir, f"batch_{year}_{batch_num:03d}_long.csv")

            with open(raw_json_path, "w") as f:
                json.dump(response_json, f, indent=2)

            df_wide.to_csv(wide_csv_path, index=False)
            df_long.to_csv(long_csv_path, index=False)

            requested_codes = sorted(set(batch_codes))
            returned_codes = sorted(set(df_long["hts8"].astype(str)))
            missing_codes = sorted(set(requested_codes) - set(returned_codes))
            extra_codes = sorted(set(returned_codes) - set(requested_codes))

            summary_records.append({
                "year": year,
                "batch_num": batch_num,
                "requested_n": len(requested_codes),
                "returned_n": len(returned_codes),
                "missing_n": len(missing_codes),
                "extra_n": len(extra_codes),
                "wide_rows": len(df_wide),
                "long_rows": len(df_long),
                "missing_codes": ";".join(missing_codes)
            })

            time.sleep(4)

        except Exception as error:
            summary_records.append({
                "year": year,
                "batch_num": batch_num,
                "requested_n": len(batch_codes),
                "returned_n": None,
                "missing_n": None,
                "extra_n": None,
                "wide_rows": None,
                "long_rows": None,
                "missing_codes": None,
                "error": str(error)
            })

summary_df = pd.DataFrame(summary_records)
summary_df.to_csv(
    os.path.join(log_dir, "dataweb_extraction_summary.csv"),
    index=False
)
