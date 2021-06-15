# Age-based Vaccine Prioritisation in WHO European Region
[Yang Liu](https://github.com/yangclaraliu), [Frank Sandmann](https://github.com/FGSandmann), [Rosanna Barnard](https://github.com/rosannaclairebarnard), [Carl AB Pearson](https://github.com/pearsonca), [Stefan Flasche](https://github.com/StefanFlasche), [Mark Jit](https://www.lshtm.ac.uk/aboutus/people/jit.mark)
## Code
- `0_LoadData.R`: load data for geography, epi parameters (susceptibility, clinical fractions), demographics, contacts, health economic parameters, mobility, stringency, COVID-19 mortality; establish scenario to be tested; load support functions and packages
	- `util_analysis.R`: helper functions for analysis. core functions are:
		- `gen_country_basics`: specific simulation time, natural waning, r0, country name
		- `update_vac_char`: update vaccine characteristics, the input to this function is the output from `gen_country_basics`, can specify infection- and disease- blocking ve here, can also specify vaccine immunity waning duration here.
		- `vac_policy`: details on the vaccine roll-out processes. `milestone_cov` are the target population level coverage by `milestone_date`, `priority` indicates the order of prioritisation, and `cov_max` indicates the maximum uptake rates in a given age group - once this uptake rates is reached, vaccination program will move towards the next age group following the order of prioritisation 
		- `predict_outbreak`: incorporate `gen_country_basics`, `update_vac_char` and `vac_policy` and calculate decision-making metrics
	- `util_data.R`: further data processing for heaslthcare processes, imputation of mobility, conversion between stringency and contacts
	- `util_plotting.R`: helper functions for plotting
	
```r
pacman::p_load(
  tidyverse, sf, countrycode, rnaturalearth, magrittr, data.table,
  ggsflabel, mgcv, pspline, viridis, ggsci, mgcv, imputeTS, ggpattern,
  ggpubr, gridExtra, grid
)
```
- `1_GenResults.R`
  - `1_1_fit.R`: this generates part of the input to `1_GenResults.R`
-  `2_PlotResults.R`
  - `2_1_Plot_Supplemental.R` 
- `misc_LitRev.`: code to extract literature from *medRxiv*
- covid_for_fitting: this framework was developed for the paper titled [Association of tiered restrictions and a second lockdown with COVID-19 deaths and hospital admissions in England: a modelling stud](https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30984-1/fulltext) by [Nicholas Davies](https://github.com/nicholasdavies/covid-tiers) et al. and can be found [here](https://github.com/nicholasdavies/covid-tiers/tree/main/fitting/covidm_for_fitting).
## figs
### intermediate
- DEoptim_fit_2: fitting for R0 and infection introduction dates `DEoptim`
- DEoptim_fit_3: fitting for R0, infection introduction dates, and under-reporting rates
- DEoptim_fit_3_1.5: similar to DEoptim_fit_3 but we allow over-reporting; reporting rate is not constrain to (0, 1] here, but (0, 1.5] to account for conditions where reported COVID-19 deaths is higher for actual COVID-19 deaths. 
- gs_fit_best: fitting for R0 and infection introduction dates using exhaustive search; equal distance within ranges
- gs_fit_top49: similar to gs_fit_best, but examined the top 49 maximum liklihood to see if there's cases where peaks are missed.
- `check_setting.png`: these figures are used to check the validity of projected mobility.
### suppelemental
