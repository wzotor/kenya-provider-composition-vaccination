# Provider composition and childhood vaccination in Kenya

Analysis code for:
Zotor, W. Provider composition and childhood vaccination in Kenya: a
cross-sectional analysis linking the 2022 Demographic and Health Survey to
the national health facility register.

Wisdom Zotor, https://orcid.org/0009-0003-1650-2160

This repository contains the complete analysis pipeline, from raw inputs to
the tables and figures reported in the paper.

## Data availability

The 2022 Kenya Demographic and Health Survey files cannot be redistributed
under The DHS Program's terms of use. Registered users may request them at
https://dhsprogram.com. The remaining inputs are public downloads. Full
citations for every source are in the manuscript's reference list.

To rerun the analysis, obtain each input and place it at the path shown
below. The scripts use relative paths from the project root and will not run
if the layout differs.

```
data-raw/
  kenya_dhs_2022/
    children_recode/        KEKR8CFL.DTA        2022 KDHS Children's Recode
    geographic_data/        KEGE8AFL.shp        2022 KDHS GPS cluster displacements
    geospatial_covariates/  KEGC8AFL.csv        DHS Geospatial Covariate Extract
  dhs_boundaries/
    shps/                   sdr_subnational_boundaries.shp
  kmhfr/
    kenya_health_facilities_2017.xlsx           Kenya Master Health Facility Register extract
  hdx_boundaries/
    ken_admin2.shp                              OCHA constituency boundaries
    ken_admin1.shp
  gadm_ken_shp/
    gadm41_KEN_2.shp                            GADM 4.1, used for documented checks only
    gadm41_KEN_1.shp
  knbs/
    kenya_pop_density_2019.xlsx                 2019 Kenya Population and Housing Census
```

Each shapefile requires its companion .dbf, .shx, .prj and .cpg files in the
same folder. They are distributed together.

Constituency boundaries come from the OCHA layer, not from GADM. GADM's
Kenya ADM2 layer does not correspond to the 290 constituencies, which is why
it is not used for the analytic linkage.

## Running the analysis

Create an empty `data-clean/` folder in the project root, then from a clean R
session with the project root as the working directory:

```r
source("scripts/00_run_all.R")
```

This runs every script in numbered order and takes about 14 minutes. It
writes intermediate objects to `data-clean/`, tables and figures to
`output/`, and a per-script timing log and `sessionInfo.txt` to
`output/logs/`. It stops at the first error rather than continuing.

Scripts are numbered in execution order and each begins with a comment
stating its purpose. Individual scripts can be sourced on their own provided
the ones before them have run.

The script numbers contain a gap. There is no script 25, and 14b sits
between 14 and 15. Scripts run in ascending order and no analysis step
is missing.

`33_map.R`, `34_forest.R` and `38_flow_diagram.R` build the three figures and
are sourced by `42_export_figures.R`, which exports them at submission
specification. `00_run_all.R` therefore skips them in its own loop so they
are not built twice.

## Expected results

A successful run reproduces the headline estimates in the paper. The adjusted
odds ratio for co-presence of government and NGO or faith-based facilities is
1.04 (95% CI 0.73 to 1.47), and the model-standardized difference in
predicted probability of full vaccination is +0.5 percentage points
(95% CI -4.1 to +6.5). The analytic sample is 3,665 children.

## Environment

The results reported in the paper were produced under:

- R 4.5.2 on Windows 11 x64
- lme4 2.0-6, survey 4.5
- sf linked to GEOS 3.14.1, GDAL 3.12.1, PROJ 9.7.1

The complete environment is recorded in `output/logs/sessionInfo.txt`.

Simulation-based confidence intervals for the predicted probabilities use
1,000 draws under `set.seed(2026)` and reproduce exactly within this
environment. Estimates from other versions of lme4 may differ in the final
reported digit, and boundary distance statistics may shift slightly under
other versions of GEOS. The substantive conclusions do not depend on either.
