# donut-effect-pnas
Replication code for "How working from home reshapes cities" by Arjun Ramani, Joel Alcedo and Nicholas Bloom accepted for publication in the Proceedings of the National Academy of Sciences in 2024. Earlier versions of this project were NBER working paper 28876: https://www.nber.org/system/files/working_papers/w28876/w28876.pdf and Arjun's undergraduate thesis at Stanford: https://economics.stanford.edu/donut-effect-how-work-home-impacts-migration-patterns-and-real-estate-markets

## Replication file instructions
This repository contains all data and scripts necessary to replicate the figures and tables found in the paper using R.

### Creating all figures and tables from intermediate datasets

To replicate all figures and tables using intermediate data stored in the `data` folder, run:
  - `donut_figures.R` to create all main figures
  - `donut_tables` to create all regression tables
  - `net_outflow_distribution.R` to create supplementary figure S5

### Creating intermediate datasets (used to create figures and tables)

Some of the datasets used in this process must be downloaded from various locations. To ease this process, we have stored each of these datasets in the `data/external_data` folder. The locations of all datasets can be found in the files inside the `scripts` folder. To create the intermediate datasets used for the figures and tables from scratch, run
- `create_all_datasets.R` to create the Zillow and USPS datasets
- `create_all_datasets.R` has a dependency on `zip_bus_patterns.R` which sources from `scripts/census-api.R`. You must obtain a Census API key from https://www.census.gov/data/developers/guidance/api-user-guide.html and store the key in `scripts/census-api.R` by including a line as follows: `key = "INSERT_YOUR_KEY_HERE`

### Additional figures and calculations cited in text
- `persistence.R` contains code to measure persistence of the Donut Effect
- `robustness_checks.R` contains code to validate our various datasets by plotting different measures of the Donut Effect against each other

### Proprietary data
This paper relies on a handful of proprietary datasets. The repository contains all data we are permitted to share. Data where we face restrictions are the following:
1) Data Axle: contains the universe of US households with addresses from 2017Q1 to 2021Q4. This data is only accessible through a secure server hosted at the Stanford Graduate School of Business. We only report aggregated summary stats as part of a data agreement with Data Axle. See 'Zip_code_zip_code_flows_output_v3.csv' and 'Zip_code_zip_code_flows__control__output_v3.csv' for zip code-zip code flows prepandemic and postpandemic.
2) Inrix commuting data: contains the universe of GPS-connected car trips from a major US auto manufacturer. We only report aggregate summary stats as part of an agreement with Inrix. See 'inrix.xlsx' for metro level commute stats.
3) Mastercard data: contains the near universe of in-person transactions on the Mastercard card network. In the paper, we report aggregated spending indices to track how city centers perform relative to suburbs as part of a data agreement with Mastercard. See 'donut_figures.R', Figures 1, S2, S4, S9, S10, for paths to the intermediate data to recreate our Mastercard charts.


