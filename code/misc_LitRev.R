require(data.table)
require(tidyverse)

# 2021/04/14
# total = 15019
#
# 2021/06/09
total = 16564
paste0("https://api.biorxiv.org/covid19/", seq(1, total, 30)) %>%
  map(jsonlite::read_json, simplifyVector = TRUE) -> find_all
#
# lapply(find_all, "[[", "messages") %>% bind_rows() %>% data.table -> find_all_records
# lapply(find_all, "[[", "collection") %>% bind_rows() %>% data.table -> find_all_collection
# write_rds(find_all_collection, "data/medrxiv_all_covid.rds")

# for term "(SARS CoV-2 OR COVID-19) AND (vaccin*) AND (prioriti*) AND (model*)"

mx <- read_rds("data/medrxiv_all_covid.rds") %>% 
  mutate(all_txt = paste(rel_title,
                         rel_abs))

index_topic2 <- grepl("vaccin", mx$all_txt, ignore.case = T)
index_topic3 <- grepl("priorit", mx$all_txt, ignore.case = T)
index_topic4 <- grepl("model", mx$all_txt, ignore.case = T)

selected <- which((index_topic2 + index_topic3 + index_topic4) == 3)

mx[selected,c("rel_doi",
              "rel_title")] %>% 
write_excel_csv(., "data/LitRev_MedRxiv_2021-06-09.csv")
