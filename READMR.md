# Exploration: BCG Vaccination Coverage Analysis: Impact of SOML Program

## Note
This Difference-in-Differences (DiD) analysis explored the impact of SOML policy on BCG coverage using MICS data. The parallel trends assumption was not met in pre-treatment periods. The analysis is shared here to demonstrate methodological learning and encourage discussion around real-world data challenges in impact evaluation.

## Project Overview
This repository contains a difference-in-differences (DiD) analysis evaluating the effect of Nigeria's Save One Million Lives (SOML) initiative on BCG vaccination coverage using national survey data from 2007-2016.

## Key Findings
- **17.8% increase** in BCG coverage attributable to SOML (IRR=1.183, p<0.001)
- Significant **10.3% decline** in control areas during study period
- Strongest effects in **rural areas** (+5.1%, p=0.043)
- Persistent disparities for older, less educated, and poorer mothers

## Files Structure
## Files Structure
bcg_vaccine_analysis/
├── data/
│ └── processed data
├── code/
│ ├── 01_data_cleaning.
│ ├── 02_imputation.R 
│ ├── 03_did_analysis.R 
│ ├── 04_visualization.R 
├── outputs/
│ 
└── README.md


## Analysis Components

### 1. Statistical Approach
- **Quasi-Poisson regression** with survey weights
- **Multiple imputation** (m=5) for missing data
- **Event-study DiD** for dynamic effects
- **Parallel trends validation**:
  - Visual inspection (2007-2011)
  - Placebo tests (p=0.85)

### 2. Key Variables
| Variable | Description | Values |
|----------|-------------|--------|
| `bcg_equivalent` | Outcome: BCG dose indicator | 1, 2 |
| `treated` | SOML implementation state | 0/1 |
| `post` | Time period indicator | 0=Pre-2012 |
| `rural_urban` | Residence type | Binary |

## How to Replicate

### Requirements
- R ≥ 4.1.0
- Key packages: `survey`, `mice`, `ggplot2`, `kableExtra`

### Installation

install.packages(c("survey", "mice", "ggplot2", "kableExtra", "broom"))

Running Analysis
Clone repository

Run scripts sequentially:

Rscript code/01_data_cleaning.R
Rscript code/02_imputation.R
Rscript code/03_did_analysis.R

## Results Interpretation

Term	IRR (95% CI)	              p-value
SOML  Effect	1.18 (1.09-1.28)	  <0.001
Rural Advantage	1.05 (1.00-1.10)	0.043

### Limitations
- Annual data limits seasonal analysis
- State-level aggregation
- Potential residual confounding

---
License
This project is released under the MIT License. See LICENSE.md (optional) for details.


Author

Nasir Umar
Lead Researcher & Data Analyst
RaaS Institute
Email: nasir.umar@raas-institute.org


























