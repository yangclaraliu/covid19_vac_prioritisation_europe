#### load packages ####
pacman::p_load(
  tidyverse, sf, countrycode, rnaturalearth, magrittr, data.table,
  ggsflabel, mgcv, pspline, viridis, ggsci, mgcv, imputeTS, ggpattern,
  ggpubr, gridExtra, grid,   tidyverse, sf, countrycode, rnaturalearth, 
  magrittr, data.table, ggsflabel, mgcv, pspline, viridis, ggsci, mgcv, 
  imputeTS, cowplot
)

#### import new contact matrices from Prem et al. ####
load("data/contact_all.rdata")
load("data/contact_work.rdata")
load("data/contact_home.rdata")
load("data/contact_school.rdata")
load("data/contact_others.rdata")

#### load member state indformation ####
"Albania, Andorra, Armenia, Austria, Azerbaijan, Belarus, Belgium, 
Bosnia and Herzegovina, Bulgaria, Croatia, Cyprus, Czech Republic, Denmark, 
Estonia, Finland,  France, Georgia, Germany, Greece, Hungary, Iceland, Ireland, 
Israel, Italy, Kazakhstan, Kyrgyzstan, Latvia, Lithuania, Luxembourg, Malta, 
Monaco, Montenegro, Netherlands, Norway, Poland, Portugal, Republic of Moldova, 
Romania, Russian Federation, San Marino, Serbia, Slovakia, Slovenia, Spain, 
Sweden, Switzerland, Tajikistan, North Macedonia, Turkey, Turkmenistan, Ukraine, 
United Kingdom of Great Britain, Uzbekistan" %>%
  strsplit(., split = ",") %>%
  .[[1]] %>%
  trimws() %>%
  enframe(
    value = "country_name",
    name = "country_index"
  ) %>%
  mutate(wb = countrycode::countrycode(country_name,
                                       origin = "country.name",
                                       destination = "wb"
  ),
  to_replace = wb %in% names(contact_all)) -> members

##### Geography #####
suppressWarnings(
  members_shp <- st_read("data/CNTR_RG_60M_2020_4326.shp", quiet = T) %>%
    mutate(wb = countrycode(NAME_ENGL, "country.name", "wb")) %>%
    filter(wb %in% members$wb)
)

##### Geography #####
suppressWarnings(
  members_world <- st_read("data/CNTR_RG_60M_2020_4326.shp", quiet = T) %>%
    mutate(wb = countrycode(NAME_ENGL, "country.name", "wb"),
           continent = countrycode(wb, "wb","continent")) %>% 
    dplyr::filter(continent %in% c("Africa", "Asia", "Europe")) 
)


#### load epidemic parameters ####
#####  Clinical Fraction #####
# (based on Davies et al, Nature paper) 
cf <- c(
  0.2904047, 0.2904047, 0.2070468, 0.2070468, 0.2676134,
  0.2676134, 0.3284704, 0.3284704, 0.3979398, 0.3979398,
  0.4863355, 0.4863355, 0.6306967, 0.6306967, 0.6906705, 0.6906705
)

##### susceptibility #####
# (based on Davies et al, Nature paper)
sus <- c(
  0.3956736, 0.3956736, 0.3815349, 0.3815349, 0.7859512,
  0.7859512, 0.8585759, 0.8585759, 0.7981468, 0.7981468,
  0.8166960, 0.8166960, 0.8784811, 0.8784811, 0.7383189, 0.7383189
)

##### load data needed from covidm #####
cm_path <- "code/covidm_for_fitting/"
cm_force_rebuild <- F
cm_build_verbose <- T
cm_version <- 2
source(paste0(cm_path, "/R/covidm.R"))

# replace default contact matrices with new versions
tmp <- cm_parameters_SEI3R("Thailand")
ag_labels <- tmp$pop[[1]]$group_names; rm(tmp)

for(i in 1:nrow(members)){
  if(members$to_replace[i]){
    cm_matrices[[members$country_name[i]]]$home <-
      as.matrix(contact_home[[members$wb[i]]]) %>% 
      set_colnames(ag_labels) %>% 
      set_rownames(ag_labels)
    
    cm_matrices[[members$country_name[i]]]$work <-
      as.matrix(contact_work[[members$wb[i]]]) %>% 
      set_colnames(ag_labels) %>% 
      set_rownames(ag_labels)
    
    cm_matrices[[members$country_name[i]]]$school <-
      as.matrix(contact_school[[members$wb[i]]]) %>% 
      set_colnames(ag_labels) %>% 
      set_rownames(ag_labels)
    
    cm_matrices[[members$country_name[i]]]$other <-
      as.matrix(contact_others[[members$wb[i]]]) %>% 
      set_colnames(ag_labels) %>% 
      set_rownames(ag_labels)
  }
}


# fix discrepancies among country names
names(cm_matrices)[names(cm_matrices) == "TFYR of Macedonia"] <- 
  "North Macedonia"
cm_populations$name <- fct_recode(cm_populations$name,
                                  "Czech Republic" = "Czechia"
)
cm_populations$name <- fct_recode(cm_populations$name,
                                  "United Kingdom of Great Britain" = "United Kingdom"
)

# proxy for contact matrices (currently unavailable ?)
cm_matrices[["Norway"]] <- cm_matrices$Denmark
cm_matrices[["Republic of Moldova"]] <- cm_matrices$Romania
cm_matrices[["Turkmenistan"]] <- cm_matrices$Uzbekistan
cm_matrices[["San Marino"]] <- cm_matrices$`Italy | Emilia-Romagna`

# fix population demography missing for micro states (so as to be able and 
# claim modelling for all 53 Member States of WHO Europe)

# add demography of Andorra (from Eurostat, 2019; latest data: 
# https://ec.europa.eu/eurostat/databrowser/view/DEMO_PJAN__custom_145232/default/table?lang=en)

pop_Andorra <- cm_populations[name == "Italy", ] %>%
  mutate(
    country_code = 020,
    name = "Andorra",
    f = c(
      1328, 1911, 1903, 1981, 1841, 2214, 2499, 3144, 3524, 3455,
      3268, 2864, 2121, 1614, 1252, 953, 666, 512, 256, 70, 12
    ) / 1000,
    m = c(
      1391, 1912, 2124, 2096, 2164, 2356, 2563, 3125, 3511, 3628,
      3524, 3062, 2322, 1719, 1416, 832, 524, 340, 137, 38, 5
    ) / 1000
  )

# add demography of Monacco (from Monaco Statistics (IMSEE), 2016; latest data: 
# https://www.monacostatistics.mc/Population-and-employment/Population-census/Statistical-tables#eztoc4898551_1_1)
pop_Monaco <- cm_populations[name == "Italy", ] %>%
  mutate(
    country_code = 492,
    name = "Monaco",
    f = c(
      915, 915, 915, 651, 651, 889, 889, 1159, 1159, 1505,
      1505, 1335, 1335, 1277, 1277, 953, 776, 586, 277, 90, 10
    ) / 1000,
    m = c(
      938, 938, 938, 688, 688, 893, 893, 1046, 1046, 1516,
      1516, 1363, 1363, 1207, 1207, 856, 614, 364, 137, 28, 1
    ) / 1000
  )

# add demography of San Marino (from Eurostat, 2018; latest data: https://ec.europa.eu/eurostat/databrowser/view/DEMO_PJAN__custom_145232/default/table?lang=en)
pop_SanMarino <- cm_populations[name == "Italy", ] %>%
  mutate(
    country_code = 674,
    name = "San Marino",
    f = c(
      690, 860, 794, 827, 762, 818, 949, 1197, 1505, 1602,
      1646, 1312, 1104, 935, 836, 647, 527, 398, 188, 61, 7
    ) / 1000,
    m = c(
      755, 884, 900, 914, 833, 820, 892, 1071, 1395, 1496,
      1532, 1288, 1005, 888, 773, 574, 412, 244, 92, 19, 1
    ) / 1000
  )

# add to cm_populations data and afterwards declutter environment
cm_populations <- rbindlist(list(cm_populations, pop_Andorra, pop_Monaco, pop_SanMarino))
rm(pop_Andorra, pop_Monaco, pop_SanMarino)

#### load HE parameters ####
# load("data/LE_estimates.rda")
# load("data/LE_estimates_updated.rda")
load("data/LE_estimates_updated_2.rda")
LE_estimates %<>%
  mutate(wb = countrycode(country_name, "country.name", "wb")) %>%
  rename(group = AgeGroup)

load("data/WB_GDP.rda")
df_VSL <- read.delim("data/Viscusi-Masterman_2017_VSL-estimates-by-country.csv",
                     sep = ",",
                     stringsAsFactors = FALSE,
                     col.names = c(
                       "country_name",
                       "GNI_per_capita_tsds", "VSL_mlns"
                     )
)

# proxy for Monaci and San Marino based on GDP ratio
df_VSL <- rbind(
  df_VSL,
  data.frame(
    "country_name" = "Monaco",
    "GNI_per_capita_tsds" = NA,
    "VSL_mlns" = df_VSL[df_VSL$country_name == "Italy", "VSL_mlns"] *
      unlist(WB_GDP[WB_GDP$`Country Name` == "Monaco", "WTPvalue"]) /
      unlist(WB_GDP[WB_GDP$`Country Name` == "Italy", "WTPvalue"])
  )
)

df_VSL <- rbind(
  df_VSL,
  data.frame(
    "country_name" = "San Marino",
    "GNI_per_capita_tsds" = NA,
    "VSL_mlns" = df_VSL[df_VSL$country_name == "Italy", "VSL_mlns"] *
      unlist(WB_GDP[WB_GDP$`Country Name` == "San Marino", "WTPvalue"]) /
      unlist(WB_GDP[WB_GDP$`Country Name` == "Italy", "WTPvalue"])
  )
) %>%
  mutate(wb = countrycode(country_name, "country.name", "wb"))

load("data/WB_GDP_per_capita.rda") 
GDPpc <- WB_GDP_per_capita %>% 
  setNames(c("population", "wb", "indicator", "GDPpc")) %>%
  mutate(wb = countrycode(population, "country.name", "wb")) %>% 
  dplyr::select(-indicator, -population); rm(WB_GDP_per_capita)

# Google mobility data
# download the data file from Google if it doesn't exist in your directory
# if(!file.exists(paste0("data/gm.csv"))){
#   download.file("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv",
#                 paste0("data/gm.csv"))
# }

# update the data file from google if the time difference is greater than a week
# if(as.numeric(abs(as.Date(file.info(paste0(proj_path, "/data/gm.csv"))$mtime) -
#                   as.Date(Sys.time()))) > 7){
#   download.file("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv",
#                 paste0(proj_path, "/data/gm.csv"))
# }

#### Google mobility ####
gm <- fread("data/gm.csv")
gm_type <- c(
  "retail", "grocery", "parks",
  "transit", "work", "residential"
)
gm %<>%
  .[sub_region_1 == "" & sub_region_2 == "" & metro_area == ""] %>%
  .[, wb := countrycode::countrycode(
    country_region,
    "country.name",
    "wb"
  )] %>%
  .[wb %in% members$wb] %>%
  .[, !c(
    "sub_region_1", "sub_region_2", "metro_area", "iso_3166_2_code",
    "census_fips_code", "country_region_code", "place_id"
  )] %>%
  setnames(., c(
    "country_name", "date",
    gm_type, "wb"
  ))

# mobility scalers from Davies et al.
curves <- data.table(
  work_scaler = c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0.008, 0.021, 0.033, 0.046, 0.058, 0.071, 0.083, 0.096, 0.108, 0.121, 0.133,
    0.146, 0.158, 0.171, 0.183, 0.196, 0.208, 0.221, 0.233, 0.246, 0.258, 0.271,
    0.283, 0.296, 0.308, 0.321, 0.334, 0.346, 0.359, 0.371, 0.384, 0.397, 0.41,
    0.422, 0.435, 0.448, 0.461, 0.474, 0.487, 0.5, 0.513, 0.526, 0.539, 0.552,
    0.566, 0.579, 0.592, 0.606, 0.619, 0.633, 0.646, 0.66, 0.674, 0.687, 0.701,
    0.715, 0.729, 0.743, 0.757, 0.771, 0.785, 0.799, 0.813, 0.828, 0.842, 0.856,
    0.87, 0.885, 0.899, 0.914, 0.928, 0.942, 0.957, 0.971, 0.986, 1, 1.014, 1.029,
    1.043, 1.058, 1.072, 1.087, 1.101, 1.115, 1.13, 1.144, 1.159, 1.173, 1.188,
    1.202, 1.216, 1.231, 1.245, 1.26, 1.274, 1.289, 1.303, 1.317, 1.332, 1.346, 1.361
  ),
  other_scaler = c(
    0.064, 0.066, 0.067, 0.068, 0.069, 0.071, 0.072, 0.073, 0.075, 0.076, 0.077, 0.078,
    0.08, 0.081, 0.082, 0.084, 0.085, 0.086, 0.087, 0.089, 0.09, 0.091, 0.092, 0.094,
    0.095, 0.096, 0.098, 0.099, 0.1, 0.101, 0.103, 0.104, 0.105, 0.106, 0.108, 0.109,
    0.11, 0.112, 0.113, 0.114, 0.116, 0.118, 0.119, 0.121, 0.123, 0.125, 0.128, 0.13,
    0.132, 0.135, 0.137, 0.14, 0.143, 0.146, 0.15, 0.154, 0.159, 0.164, 0.169, 0.175,
    0.182, 0.19, 0.198, 0.207, 0.217, 0.228, 0.24, 0.252, 0.266, 0.28, 0.295, 0.31,
    0.327, 0.344, 0.361, 0.379, 0.398, 0.418, 0.438, 0.459, 0.48, 0.502, 0.525, 0.549,
    0.572, 0.597, 0.621, 0.647, 0.672, 0.698, 0.725, 0.751, 0.778, 0.805, 0.833, 0.86,
    0.888, 0.916, 0.944, 0.972, 1, 1.028, 1.056, 1.084, 1.112, 1.14, 1.168, 1.196, 1.224,
    1.252, 1.28, 1.308, 1.337, 1.365, 1.393, 1.421, 1.449, 1.477, 1.505, 1.533, 1.561,
    1.589, 1.617, 1.645, 1.673, 1.701
  ),
  perc = round(seq(0, 1.25, 0.01), 2)
)

#### OXCGRT ####
today <- Sys.Date()
# update stringency index file
# file_date <- format(file.info("oxcgrt.csv")$mtime, "%Y-%m-%d")
# if(abs(as.numeric(as.Date(file_date) - today)) > 7){
#   download.file("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv",
#                 paste0("data/oxcgrt.csv"))
# }
##### school related parameters #####
read_csv("data/oxcgrt.csv",
         col_types = cols(RegionName = col_character(), 
                          RegionCode = col_character())) %>%
  dplyr::filter(is.na(RegionName)) %>%
  mutate(
    Date = lubridate::ymd(Date),
    wb = countrycode::countrycode(
      CountryName,
      "country.name",
      "wb"
    )
  ) %>% 
  dplyr::filter(wb %in% members$wb,
                C1_Flag == 1 | is.na(C1_Flag)) %>%
  dplyr::select(wb, Date, `C1_School closing`) %>% 
  rename(C1 = `C1_School closing`) %>%
  distinct() %>% 
  pivot_wider(
    names_from = Date,
    values_from = C1
  ) %>%
  full_join(members[, "wb"], by = "wb") %>%
  ungroup()  %>%
  pivot_longer(
    cols = starts_with("202"),
    names_to = "Date",
    values_to = "C1"
  ) %>%
  mutate(Date = lubridate::ymd(Date)) %>% 
  pivot_wider(names_from = wb, values_from = C1) %>% 
  mutate_at(vars(members$wb %>% 
                   .[!.%in% c("ARM", "MNE", "MKD")]), na_locf) %>% 
  pivot_longer(cols = members$wb,
               names_to = "wb",
               values_to = "C1") %>% 
  mutate(C1 = if_else(is.na(C1), 0, C1)) -> oxcgrt

##### stringency index #####
si <- read_csv("data/oxcgrt.csv",
               col_types = cols(RegionName = col_character(), 
                                RegionCode = col_character())) %>%
  filter(is.na(RegionName)) %>%
  dplyr::select(CountryName, Date, StringencyIndex) %>%
  mutate(
    date = lubridate::ymd(Date),
    wb = countrycode::countrycode(
      CountryName,
      "country.name",
      "wb"
    ),
    d = as.numeric(date)
  ) %>%
  dplyr::select(
    -Date,
    -CountryName
  ) %>%
  filter(wb %in% members$wb) %>%
  distinct() %>%
  as.data.table()

si %>%
  filter(!is.na(StringencyIndex)) %>%
  group_by(date) %>%
  tally() %>%
  mutate(
    n_max = max(n),
    missing = (n_max - n) / n_max
  ) %>%
  filter(missing > 0.1) %>%
  pull(date) %>%
  min() -> si_stopdate

si %<>%
  filter(date < si_stopdate) %>%
  group_by(wb) %>%
  arrange(date) %>%
  group_split() %>%
  map(mutate, StringencyIndex = zoo::na.locf(StringencyIndex)) %>%
  bind_rows() %>%
  as.data.table()

# public health data
# download.file("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv",
#               paste0("data/owid.csv"))

#### our world in data ####
owid <- read.csv("data/owid.csv") %>%
  mutate(wb = countrycode(location, "country.name", "wb")) %>% 
  dplyr::filter(wb %in% members$wb)

epi <- data.table(owid) %>% 
  .[, list(location, date, new_deaths_smoothed, new_cases_smoothed)] %>% 
  setNames(c("loc", "date", "deaths", "cases")) %>%
  mutate_at(vars(c("deaths", "cases")), ~if_else(is.na(.), 0, .))%>%
  mutate_at(vars(c("deaths", "cases")), ~if_else(.<0, 0, .)) %>% 
  .[,date := ymd(date)] %>% 
  .[,loc := countrycode::countrycode(loc, "country.name", "wb")] 

#### model fit ####
model_selected_2 <- read_rds("data/intermediate/DEoptim2_selected_debug.rds") %>% 
  filter(!is.na(t))
model_selected_3 <- read_rds("data/intermediate/DEoptim3_selected_debug.rds") %>% 
  filter(!is.na(t))

#gs_fitted_v2_imputed
#### load supporting functions ####
source("code/util_analysis.R")
source("code/util_plotting.R")
# source("code/util_data.R")

#### create scenarios to test ####
priority_policy <- list()
priority_policy[["p1"]] <- c(rep(NA, 4), rep(1, 12))
priority_policy[["p2"]] <- c(rep(NA, 4), rep(1, 8), rep(2, 4))
priority_policy[["p3"]] <- c(rep(NA, 4), rep(2, 8), rep(1, 4))
priority_policy[["p4"]] <- c(rep(NA, 4), rep(5, 8), 4, 3, 2, 1)
# priority_policy[["p5"]] <- c(rep(NA, 4), rep(7, 6), 6, 5, 4, 3, 2, 1)
# priority_policy[["p6"]] <- c(rep(NA, 2), rep(5, 10), 4, 3, 2, 1)


priority_policy2 <- list()
priority_policy2[["p3_a"]] <- c(rep(NA, 2), rep(2, 10), rep(1, 4))
priority_policy2[["p3_b"]] <-  c(rep(NA, 2),rep(3, 2),rep(2, 8), rep(1, 4))
priority_policy2[["p4_a"]] <- c(rep(NA, 2), rep(5, 10), 4, 3, 2, 1)
priority_policy2[["p4_b"]] <- c(rep(NA, 2), rep(6, 2), rep(5, 8), 4, 3, 2, 1)
  
combo <- list()

data.frame(ms_date = seq(ymd("2021-01-01"),
                         ymd("2023-01-14"),
                         1)) %>% 
  mutate(R1 = case_when(ms_date == "2021-03-01" ~ 0,
                        ms_date == "2021-06-30" ~ 0.03,
                        ms_date == "2021-12-31" ~ 0.1,
                        ms_date == "2022-12-31" ~ 0.3,
                        TRUE ~ as.numeric(NA)),
         R2 = case_when(ms_date == "2021-03-01" ~ 0,
                        ms_date == "2021-06-30" ~ 0.03,
                        ms_date == "2021-12-31" ~ 0.2,
                        ms_date == "2022-12-31" ~ 0.5,
                        TRUE ~ as.numeric(NA)),
         R3 = case_when(ms_date == "2021-01-01" ~ 0,
                        ms_date == "2022-12-31" ~ 0.8,
                        TRUE ~ as.numeric(NA)),
         R4 = case_when(ms_date == "2021-01-01" ~ 0,
                        ms_date == "2021-12-31" ~ 0.8,
                        TRUE ~ as.numeric(NA))
  ) %>% 
  mutate_at(vars(starts_with("R", ignore.case = F)),
            imputeTS::na_interpolation) %>% 
  mutate_at(vars(starts_with("R", ignore.case = F)),
            ~lag(x = ., n = 14)) %>% 
  pivot_longer(cols = starts_with("R", ignore.case = F)) %>%
  filter(name == "R1" & ms_date %in% 
           c(ymd("2021-03-01")+14,
             ymd("2021-06-30")+14,
             ymd("2021-12-31")+14,
             ymd("2022-12-31")) |
           name == "R2" & ms_date %in% 
           c(ymd("2021-03-01")+14,
             ymd("2021-06-30")+14,
             ymd("2021-12-31")+14,
             ymd("2022-12-31")) |
           name == "R3" & ms_date %in% 
           c(ymd("2021-01-01")+14,
             ymd("2022-12-31")) |
           name == "R4" & ms_date %in% 
           c(ymd("2021-01-01")+14,
             ymd("2021-12-31")+14)
  ) %>% 
  group_by(name) %>% group_split() %>% 
  setNames(paste0("R",1:4)) %>% 
  map(select, -name) %>% 
  map(rename, ms_cov = value) %>% 
  map(mutate, ms_date = as.character(ms_date)) -> combo

# rm(
#   "fit", "pre_tab", "owid",
#   "val", "tab", "gm_forecast", "gm_scaled",
#   "si_imputed", "oxcgrt", "to_merge", "si", "gm", "schedule_pre"
# )

# save.image("data/global.RData")
file <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"
owid_vac <- read_csv(url(file)) %>% 
  mutate(wb = countrycode(location, "country.name", "wb")) %>% 
  filter(wb %in% members$wb)

cm_populations %>% 
  mutate(tot = f + m) %>% 
  filter(location_type == 4) %>% 
  group_by(name) %>% 
  summarise(tot = sum(tot) * 1000) %>% 
  mutate(wb = countrycode(name, "country.name", "wb")) -> vac_denom

##### healthcare parameters ####
critical2 <- 0

picu_cocin_func <- function(age) {
  x <- c(-0.1309118, 0, 17.2398874, 65.7016492, 100)
  y <- c(-2.1825091, -2.1407043, -1.3993552, -1.2344361, -8.8191062)
  p <- splinefun(x, y)(age)
  exp(p) / (1 + exp(p))
}
picu_cocin <- picu_cocin_func(0:85)

# Infection fatality rate (derived from Levin et al., preprint)
ifr_levin <- 100 * exp(-7.56 + 0.121 * 0:85) / (100 + exp(-7.56 + 0.121 * 0:85)) / 100
# Infection hospitalisation rate (derived from Salje et al., Science)
ihr_salje <- exp(-7.37 + 0.068 * 0:85) / (1 + exp(-7.37 + 0.068 * 0:85))
# Amalgamate probabilities
probabilities <- data.table(age = 0:85, ihr = ihr_salje, ifr = ifr_levin, picu = picu_cocin)
probabilities[, age_group := pmin(15, age %/% 5)]
probabilities <- probabilities[, lapply(.SD, mean), by = age_group, .SDcols = 2:4]

# Create model burden processes
P.critical <- probabilities[, ihr * picu]
P.severe <- probabilities[, ihr * (1 - picu)]
P.death <- probabilities[, ifr]

burden_processes <- list(
  list(
    source = "new_EEa", type = "multinomial", names = c("new_infections"), report = c("i"),
    prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
    delays = matrix(cm_delay_skip(60, 0.25)$p, nrow = 1)
  ),
  
  list(
    source = "E", type = "multinomial", names = c("death", "null"), report = c("o", ""),
    prob = matrix(c(P.death, 1 - P.death), nrow = 2, ncol = 16, byrow = T),
    delays = matrix(c(cm_delay_gamma(26, 5, 60, 0.25)$p, 
                      cm_delay_skip(60, 0.25)$p), nrow = 2, byrow = T)
  ),
  
  list(
    source = "E", type = "multinomial", names = c("to_hosp_critical", 
                                                  "to_hosp_critical2", 
                                                  "to_hosp_severe", 
                                                  "null"), 
    report = c("", "", "", ""),
    prob = matrix(c(P.critical * (1 - critical2), 
                    P.critical * critical2, P.severe, 
                    1 - P.critical - P.severe), 
                  nrow = 4, ncol = 16, byrow = T),
    delays = matrix(c(cm_delay_gamma(8.5, 5, 60, 0.25)$p, 
                      cm_delay_gamma(8.5, 5, 60, 0.25)$p, 
                      cm_delay_gamma(8.5, 5, 60, 0.25)$p, 
                      cm_delay_skip(60, 0.25)$p), 
                    nrow = 4, byrow = T)
  ),
  
  list(
    source = "to_hosp_severe", 
    type = "multinomial", 
    names = "non_icu_severe", 
    report = "pi",
    prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
    delays = matrix(cm_delay_gamma(14.6, 5, 60, 0.25)$p, nrow = 1, byrow = T)
  ),
  
  list(
    source = "to_hosp_critical2", 
    type = "multinomial", 
    names = "non_icu_critical2", 
    report = "pi",
    prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
    delays = matrix(cm_delay_gamma(15.6, 5, 60, 0.25)$p, nrow = 1, byrow = T)
  ),
  
  list(
    source = "to_hosp_critical", 
    type = "multinomial", 
    names = "non_icu_critical", 
    report = "pi",
    prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
    delays = matrix(cm_delay_gamma(6.0, 5, 60, 0.25)$p, nrow = 1, byrow = T)
  ),
  
  list(
    source = "non_icu_critical", 
    type = "multinomial", 
    names = "icu_critical", 
    report = "pi",
    prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
    delays = matrix(cm_delay_gamma(9.6, 5, 60, 0.25)$p, nrow = 1, byrow = T)
  )
)

#### impute contact ####
# simulate end dates, determine the end of mobility imputation
sim_start <- "2020-02-15"
sim_end <- "2022-12-31"
si_post <- 10 # generally should be lower than si_post after examining the data
recovery_period <- 365 # recovery_period days to recover to si_post level of stringency
asc = function(x, y0, y1, s0, s1)
{
  xx = s0 + x * (s1 - s0)
  h0 = exp(s0) / (1 + exp(s0))
  h1 = exp(s1) / (1 + exp(s1))
  h = (exp(xx) / (1 + exp(xx)) - h0) / (h1 - h0)
  y0 + (y1 - y0) * h
}

#### process SI  #### 
si_stopvalue <- si[date == as.Date(si_stopdate)-1]

# draw gradual changes
si_changes <- list()
for(i in 1:nrow(si_stopvalue)){
  asc(seq(from = 0, 
          to = 1, 
          length.out = recovery_period),
      si_stopvalue$StringencyIndex[i],
      si_post,
      -5,
      5) -> si_changes[[i]]
}

si_changes %>% 
  setNames(si_stopvalue$wb) %>%
  map(data.table) %>%
  map(~.[, date := seq(as.Date(si$date %>% range %>% .[2]),
                       as.Date(si$date %>% range %>% .[2]) + recovery_period - 1,
                       1)]) %>% 
  bind_rows(.id = "wb") %>% 
  setNames(c("wb", "StringencyIndex", "date")) %>%
  bind_rows(si[,list(wb, StringencyIndex, date)]) %>% 
  right_join(CJ(date = seq(as.Date(sim_start), 
                           as.Date(sim_end), 
                           1),
                wb = unique(si$wb)), 
             by = c("wb","date")) %>% 
  mutate(StringencyIndex = if_else(is.na(StringencyIndex),
                                   si_post,
                                   StringencyIndex)) %>% 
  mutate(status = case_when(date <= si$date %>% range %>% .[2] ~ "empirical",
                            date %in% seq(as.Date(si$date %>% range %>% .[2]),
                                          as.Date(si$date %>% range %>% .[2]) + 
                                            recovery_period - 1,
                                          1) ~ "transition",
                            TRUE ~ "post-pandemic")) %>% 
  mutate(d = as.numeric(date)) -> si_imputed

# ggplot(si_imputed, aes(x = date,
#                        color = status,
#                        y = StringencyIndex)) +
#   # geom_jitter() +
#   geom_line(aes(group = wb)) +
#   geom_point() +
#   theme_bw() +
#   scale_color_lancet() +
#   facet_wrap(~wb) +
#   labs(x = "Date", color = "",
#        title = "OXCGRT - COVID19 Stringecy Index") +
#   theme(axis.text.x = element_text(angle = 90),
#         axis.text = element_text(size = 16),
#         strip.text = element_text(size = 16),
#         legend.position = "bottom",
#         legend.text = element_text(size = 16),
#         title = element_text(size = 16))  -> p
# 
# ggsave("figs/figureSX_si.png", p, width = 20, height = 10); rm(p)

#### process mobility ####
gm %>% 
  melt(.,
       id.vars = c("country_name", "wb", "date"),
       measures.var = gm_type) %>% 
  .[, c("m",
        "dow",
        "doy",
        "d",
        "variable",
        "value") := list(factor(month(date), levels = 1:12),
                         factor(lubridate::wday(date)),
                         lubridate::yday(date),
                         as.numeric(date),
                         factor(variable),
                         (value + 100)/100)] %>% 
  left_join(., si[,list(d,wb,StringencyIndex)], 
            by = c("wb", "d")) %>% 
  filter(date < si_stopdate) %>% 
  filter(variable %in% c("retail",
                         "transit",
                         "grocery",
                         "work")) %>% 
  mutate(wb = factor(wb)) -> tab

fit <- gam(formula = value ~  dow + s(wb, bs = "re") + variable + 
             variable*dow + StringencyIndex + variable*m, 
           data = tab, 
           na.action = na.omit)

CJ(date = seq(range(tab$date)[1],
              as.Date(sim_end),
              by = 1),
   wb = unique(tab$wb),
   variable = c("retail",
                "transit",
                "grocery",
                "work")) %>% 
  .[, c("dow",
        "doy",
        "m",
        "d") := list(lubridate::wday(date) %>% factor,
                     lubridate::yday(date),
                     month(date),
                     as.numeric(date))]  %>% 
  .[, m := if_else(m == 12, 11, m)] %>%
  .[, m := if_else(m == 1, 2, m)] %>%
  .[, m := factor(m)] %>%
  left_join(si_imputed[,c("wb","d", "StringencyIndex","status")],
            by = c("wb", "d")) %>%
  split(by = "variable") %>% 
  map(arrange, date) %>% 
  bind_rows() %>% 
  mutate(d = if_else(d >= max(tab$d), max(tab$d), d),
         country_missing = if_else(is.na(status), T, F)) -> pre_tab

val <- predict(fit, pre_tab)
pre_tab %<>% mutate(predicted = val)
pre_tab %>% 
  left_join(tab[,c("wb", "date", "variable","value")],
            by = c("wb","date", "variable")) %>% 
  mutate(imputed = if_else(is.na(value),
                           predicted,
                           value)) -> gm_forecast

# imputation for countries with no data
members_neighbors <- spdep::poly2nb(members_shp)
members_missing <- which(!members$wb %in% gm_forecast$wb) %>% sort
members[members_missing,]

to_merge <- list()
for(i in 1:length(members_missing)){
  gm_forecast %>% 
    filter(wb %in% members$wb[members_neighbors[[members_missing[i]]]]) %>% 
    group_by(variable, dow, doy, m, date) %>% 
    summarise(imputed = mean(imputed, na.rm = T),
              .groups = "drop") %>% 
    mutate(predicted = NA,
           value = NA,
           wb = members$wb[members_missing[i]],
           status = "averaged",
           country_missing = T,
           StringencyIndex = NA,
           d = as.numeric(date)) %>% 
    dplyr::select(colnames(gm_forecast)) -> to_merge[[i]]
}

to_merge %>% 
  bind_rows() %>% 
  bind_rows(gm_forecast) -> gm_forecast

# check for completness 
gm_forecast %>% 
  filter(is.na(imputed)) %>% 
  ungroup %>% 
  # group_by(wb, variable) %>% 
  dplyr::select(wb) %>%
  distinct() %>% 
  left_join(members, by = "wb") %>% 
  pull(country_index) %>% 
  sort -> members_missing

members_missing <- c(members_missing, 
                     which(!(members$wb %in% unique(gm_forecast$wb)))) %>% 
  unique %>% sort

to_merge <- list()
for(i in 1:length(members_missing)){
  gm_forecast %>% 
    filter(wb %in% members$wb[members_neighbors[[members_missing[i]]]]) %>% 
    group_by(variable, dow, doy, m, date) %>% 
    summarise(imputed = mean(imputed, na.rm = T),
              .groups = "drop") %>% 
    mutate(predicted = NA,
           value = NA,
           status = "averaged",
           wb = members$wb[members_missing[i]],
           StringencyIndex = NA,
           country_missing = TRUE,
           d = as.numeric(date)) %>% 
    dplyr::select(colnames(gm_forecast)) -> to_merge[[i]]
}

to_merge %>% 
  bind_rows() %>%
  # group_by(date, wb, variable) %>% tally %>% filter(n != 1)
  bind_rows(gm_forecast) %>% 
  # dplyr::select(-value, -predicted) %>% 
  mutate(status = if_else(is.na(status), "empirical", status)) -> gm_forecast

gm_forecast$status %>% table
status_col <- ggsci::pal_futurama()(4)
status_lvl <- unique(gm_forecast$status)

gm_forecast %>% 
  filter(wb %in% c("MLT","MKD")) %>%  
  arrange(date) %>% 
  distinct() %>% 
  dplyr::select(-value, -predicted) %>% 
  pivot_wider(names_from = status, 
              values_from = imputed) %>% 
  mutate(averaged = if_else(!is.na(empirical), as.numeric(NA), averaged)) %>% 
  pivot_longer(cols = c(averaged, empirical, transition, `post-pandemic`)) %>%
  filter(!is.na(value)) %>% 
  mutate(imputed = value) %>% 
  bind_rows(gm_forecast %>% 
              filter(!wb %in% c("MLT","MKD"))) %>% 
  distinct -> gm_forecast

gm_forecast %<>% 
  data.table() %>% 
  dplyr::select(date, wb, variable, status, imputed) %>% 
  distinct() %>% 
  pivot_wider(names_from = variable,
              values_from = imputed) %>% 
  arrange(date, wb)

gm_forecast %>% 
  mutate(work = if_else(work > 1.25, 1.25, work),
         othx = 0.345*retail + 0.445*transit + 0.21*grocery,
         othx = if_else(othx > 1.25, 1.25, othx),
         work = round(work, 2),
         othx = round(othx, 2)) %>% 
  left_join(curves[,c("perc","work_scaler")], by = c("work" = "perc")) %>% 
  left_join(curves[,c("perc", "other_scaler")], by = c("othx" = "perc")) %>% 
  dplyr::select(-c(grocery, retail, transit, work, othx)) %>% 
  rename(work = work_scaler,
         other = other_scaler) -> gm_scaled

country_data_length <- 
  gm_scaled %>% group_by(wb) %>% group_split() %>% map(nrow) %>% unlist()
which(country_data_length != 1051)

schedule_raw <- gm_scaled %>% 
  mutate(home = 1,
         date = as.character(date)) %>% 
  left_join(oxcgrt %>% 
              mutate(Date = as.character(Date)) %>% 
              dplyr::select(Date, C1, wb) %>% 
              setNames(c("date", "school", "wb")), # %>% 
            # mutate(school = case_when(school == 0 ~ 1,
            #                           is.na(school) ~ 1,
            #                           school == 3 ~ 0,
            #                           TRUE ~ 0.5),
            by = c("date", "wb"))  %>%
  # filter(is.na(school)) %>% pull(date) %>% table
  # filter(is.na(school))
  # dplyr::filter(!is.na(school)) %>% 
  mutate(school = case_when(school == 0 ~ 1,
                            school == 3 ~ 0,
                            is.na(school) ~ 1,
                            TRUE ~ 0.5)) 

CJ(date = seq(as.Date("2019-12-01"), as.Date("2020-02-14"),1),
   wb = members$wb) %>% 
  .[,status := "assumed"] %>% 
  .[,c("work",
       "other",
       "home",
       "school",
       "date") := 
      list(1,1,1,1,
           as.character(date))] -> schedule_pre

# school holidays
schedule_raw %<>%  
  bind_rows(schedule_pre) %>%
  arrange(date) %>% 
  mutate(date = lubridate::ymd(date),
         month = lubridate::month(date),
         day = lubridate::day(date),
         year = lubridate::year(date)) %>% 
  mutate(holiday = if_else(
    #winter holiday, 
    (year > 2020 & month == 12 & day >=  15) |
      (year > 2020 & month == 1 & day < 5) |
      # summer holiday
      month %in% c(7,8),
    T,
    F),
    school = if_else(holiday, 0, school)) %>% 
  dplyr::select(-holiday) %>% 
  mutate(status = if_else(is.na(status), "averaged", status)) %>% 
  dplyr::select(date, wb, status, home, work, school, other, month, day, year)

# sanity check
# schedule_raw %>% 
#   # mutate(school = as.numeric(school)) %>% # filter(wb == "MLT") %>% View()
#   ggplot(., aes(x = date, y = other, group = wb, color = status)) +
#   geom_line(size = 1.2) +
#   facet_wrap(~wb) +
#   labs(title = "Other Contacts Relative Variation") +
#   theme_cowplot() +
#   theme(legend.position = "bottom",
#         strip.background = element_rect(fill = NA)) -> p
# 
# ggsave("figs/intermediate/check_other.png", p, width = 15, height = 10)

# vaccine efficacy tested
ve_tab  <- CJ(ve = c(0.5, 0.75, 0.95), ei_v = c(0, 0.25, 0.5, 0.75, 0.95)) %>% 
  dplyr::filter(ve >= ei_v) %>% 
  mutate(ed_vi = exp_ve(ve, ei_v))


load_fit_res <- function(cn){
  paste0("data/intermediate/gs_fit_top49/", cn,"_fit.rds") %>% 
    read_rds() %>% 
    mutate(rk1 = rank(desc(ll)),
           rk2 = rank(diff),
           rk = (rk1 + rk2)) %>% 
    arrange(rk) -> f
  return(f)
  
}

# countries to exclude
epi %>% group_by(loc) %>% group_split() %>% map(arrange, desc(deaths)) %>% 
  map(~.[1,]) %>% bind_rows() %>% dplyr::filter(deaths < 10) %>% 
  pull(loc) -> remove_1
c("KAZ", "KGZ") -> remove_2
c("TKM") -> remove_3

unique(c(remove_1, remove_2, remove_3)) %>% sort -> members_remove

#### covariate data #### 
covar <- list()
cm_populations %>% 
  separate(age, into = c("age_LL", "age_UL")) %>% 
  mutate(tot = f + m,
         age_LL = as.numeric(age_LL), 
         ag = case_when(age_LL >= 20 & age_LL < 60 ~ "adult",
                        age_LL >= 60 ~ "older adult")) %>% 
  filter(location_type == 4,
         !is.na(ag)) %>% 
  group_by(ag, name) %>% 
  summarise(tot = sum(tot) * 1000) %>% 
  pivot_wider(names_from = ag, values_from = tot) %>% 
  mutate(`total adult` = adult + `older adult`,
         wb = countrycode(name, "country.name", "wb")) %>% 
  filter(wb %in% members$wb) -> covar[["age"]]


tmp <- list()
for(i in 1:nrow(members)){
  Reduce("+", cm_matrices[[members$country_name[i]]]) %>% 
    reshape2::melt() %>% 
    setNames(c("Var1","Var2", "value")) %>% 
    separate(Var1, into = c("Var1_LL", "Var1_UL")) %>% 
    separate(Var2, into = c("Var2_LL", "Var2_UL")) %>% 
    mutate(ag1 = case_when(Var1_LL < 20 ~ "child",
                           Var1_LL >= 20 & Var1_LL < 60 ~ "adult",
                           Var1_LL >= 60 ~ "older adult"),
           ag2 = case_when(Var2_LL < 20 ~ "child",
                           Var2_LL >= 20 & Var2_LL < 60 ~ "adult",
                           Var2_LL >= 60 ~ "older adult")) %>% 
    group_by(ag1, ag2) %>% 
    summarise(value = sum(value)) -> tmp[[i]]
}


tmp %>% bind_rows(.id = "country_index") %>% 
  mutate(country_index = as.numeric(country_index)) %>% 
  left_join(members, by = "country_index") %>% 
  unite(ag1, ag2, col = "ag", sep = "-") %>% 
  pivot_wider(names_from = ag, values_from = value) -> covar[["contact"]]

rm(tmp)

covar[["non_S"]] <- readRDS("data/intermediate/non_S_2.rds")

res <- readRDS("data/intermediate/priority_selection_2.rds")

res[[3]] %>% 
  bind_rows(.id = "ROS") %>% 
  mutate(ROS = paste0("R",ROS),
         wb = countrycode(population, "country.name", "wb")) %>% 
  data.table::melt(., id.vars = c("ROS", "policy", "run", "w", "wb", "population")) %>% 
  dplyr::filter(variable %in% c("cases", "death_o",
                                "adjLE", 
                                # "VSLmlns_pd",
                                "QALYloss",
                                "HC"),
                w == "Before 2023",
                !wb %in% members_remove)  %>% 
  group_by(population, variable, ROS) %>% group_split() %>% 
  map(arrange, value) %>% 
  map(~.[1,]) %>% bind_rows() %>% 
  # dplyr::select(-t.x, -supply, -dir) %>%
  dplyr::select(-w, -wb) %>% 
  mutate(value = abs(value)) %>% 
  mutate(wb = countrycode(population, "country.name", "wb")) -> covar[["decisions_2"]]

rm(res)

members_pop <- data.table(name = unique(cm_populations$name)) %>% 
  mutate(wb = countrycode::countrycode(name, "country.name", "wb"))

cm_populations %>% 
  left_join(members_pop, by = "name") %>% 
  filter(wb %in% members$wb,
         location_type == 4,
         !wb %in% members_remove) %>% 
  mutate(tot = m + f) %>% 
  group_by(name, wb) %>% summarise(tot = sum(tot)) -> members_pop

