# donut-effect-pnas
Replication code for "How working from home reshapes cities" by Arjun Ramani, Joel Alcedo and Nicholas Bloom published in the Proceedings of the National Academy of Sciences in 2024. An earlier version of this project was NBER working paper 28876: https://www.nber.org/system/files/working_papers/w28876/w28876.pdf and Arjun's undergraduate thesis at Stanford: 


## Replication file instructions
This repository contains all data and scripts necessary to replicate the figures and tables found in the paper. You may need R version 3.6.2 or later to successfully run.

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

### Global city level data (not reported in the paper but available for research use)
- `mastercard-cities.xlsx` contains data for city-level Donut Effect plots for 118 global cities. The structure of this data is same as the main Figure 1. For each city, the dataset contains monthly time series of the difference in spending between a city center spending index and an outer ring spending index. The spending indices are first normalized such that the average 2019 value = 100. Thus the difference, which we report, will have an average 2019 value of 0. Our spending data contains all in-person credit or debit card spending on the Mastercard network from Jan 2018 to Sep 2023. For category-wise spending we have data till Dec 2023



