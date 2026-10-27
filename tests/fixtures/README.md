These fixtures contain real aggregate responses queried from PCDaS, not generated records.
`sim-aggregates.rds`: counts for SIM, Acre residents, deaths in 2022, by sex/race and their margins.
`paginated-null.rds`: counts by def_sexo and CAUSAMAT for the same scope, with page size 1 to exercise pagination and NULL values.
Each file includes its retrieval time. No credentials are stored. Reproduce with `Rscript scripts/record-fixtures.R` and a configured `pcdas_token`.

`municipalities.rds`: real 2022 SIM aggregates by municipal reference and sex, residence/occurrence pairs, and five distinct codes named Bom Jesus.
`municipal-stocks.rds`: real population and CNES aggregates for Acre in their latest two periods, retaining municipal codes/names and each period separately.
Reproduce these two with `Rscript scripts/record-municipal-fixtures.R`. A presentation test removes identifying values from a real aggregate to test missing data; it creates no new records or counts.
