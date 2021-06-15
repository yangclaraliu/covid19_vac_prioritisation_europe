plots <- list()
# Albania
head(load_fit_res("Albania"),10)
plot_fit("Albania", 100, 59, 2) -> plots[["Albania"]]
# Andorra
head(load_fit_res("Andorra"),10)
plot_fit("Andorra", 100, 107, 2.2) -> plots[["Andorra"]]
# Armenia
head(load_fit_res("Armenia"),10)
plot_fit("Armenia", 100, 45, 1.7) -> plots[["Armenia"]]
# Austria
head(load_fit_res("Austria"),10)
plot_fit("Austria", 100, 97, 2.4) -> plots[["Austria"]]
# Azerbaijan
head(load_fit_res("Azerbaijan"),10)
plot_fit("Azerbaijan", 100, 123, 2.8) -> plots[["Azerbaijan"]]
# Belarus
head(load_fit_res("Belarus"),10)
plot_fit("Belarus", 100, 90, 1.5) -> plots[["Belarus"]]
# Belgium
head(load_fit_res("Belgium"),10)
plot_fit("Belgium", 100, 49, 2.8) -> plots[["Belgium"]]
# Bosnia and Herzegovina
head(load_fit_res("Bosnia and Herzegovina"),10)
plot_fit("Bosnia and Herzegovina", 100, 32, 1.9) -> 
  plots[["Bosnia and Herzegovina"]]
# Bulgaria
head(load_fit_res("Bulgaria"),10)
plot_fit("Bulgaria", 100, 44, 2) -> plots[["Bulgaria"]]
# Croatia
head(load_fit_res("Croatia"),10)
plot_fit("Croatia", 100, 71, 2) -> plots[["Croatia"]]
# Cyprus
head(load_fit_res("Cyprus"),10)
plot_fit("Cyprus", 100, 293, 2.9) -> plots[["Cyprus"]]
# Czech Republic
head(load_fit_res("Czech Republic"),10)
plot_fit("Czech Republic", 100, 92, 2.2) -> plots[["Czech Republic"]]
# Denmark
head(load_fit_res("Denmark"),10)
plot_fit("Denmark", 100, 23, 1.7) -> plots[["Denmark"]]
# Estonia
head(load_fit_res("Estonia"),10)
plot_fit("Estonia", 100, 104, 1.9) -> plots[["Estonia"]]
# Finland
head(load_fit_res("Finland"),10)
plot_fit("Finland", 100, 48, 2) -> plots[["Finland"]]
# France
head(load_fit_res("France"),10)
plot_fit("France", 100, 31, 2.4) -> plots[["France"]]
# Georgia
head(load_fit_res("Georgia"),10)
plot_fit("Georgia", 100, 230, 2.3) -> plots[["Georgia"]]
# Germany
head(load_fit_res("Germany"),10)
plot_fit("Germany", 100, 27, 2) -> plots[["Germany"]]
# Greece
head(load_fit_res("Greece"),10)
plot_fit("Greece", 100, 84, 2.1) -> plots[["Greece"]]
# Hungary
head(load_fit_res("Hungary"),10)
plot_fit("Hungary", 100, 54, 2.2) -> plots[["Hungary"]]
# Iceland
head(load_fit_res("Iceland"),10)
plot_fit("Iceland", 100, 329, 2.7) -> plots[["Iceland"]]
# Ireland
head(load_fit_res("Ireland"),10)
plot_fit("Ireland", 100, 55, 2.9) -> plots[["Ireland"]]
# Israel
head(load_fit_res("Israel"),10)
plot_fit("Israel", 100, 63, 2.7) -> plots[["Israel"]]
# Italy
head(load_fit_res("Italy"),10)
plot_fit("Italy", 100, 35, 3) -> plots[["Italy"]]
# Kazakhstan
head(load_fit_res("Kazakhstan"),10)
plot_fit("Kazakhstan", 100, 60, 1.9) -> plots[["Kazakhstan"]]
# Kyrgyzstan
head(load_fit_res("Kyrgyzstan"),10)
plot_fit("Kyrgyzstan", 100, 175, 2.5) -> plots[["Kyrgyzstan"]]
# Latvia
head(load_fit_res("Latvia"),10)
plot_fit("Latvia", 100, 108, 2.1) -> plots[["Latvia"]]
# Lithuania
head(load_fit_res("Lithuania"),10)
plot_fit("Lithuania", 100, 89, 2.2) -> plots[["Lithuania"]]
# Luxembourg
head(load_fit_res("Luxembourg"),10)
plot_fit("Luxembourg", 100, 60, 2.4) -> plots[["Luxembourg"]]
# Malta
head(load_fit_res("Malta"),10)
plot_fit("Malta", 100, 277, 2.2) -> plots[["Malta"]]
# Montenegro
head(load_fit_res("Montenegro"),10)
plot_fit("Montenegro", 100, 168, 1.7) -> plots[["Montenegro"]]
# Netherlands
head(load_fit_res("Netherlands"),10)
plot_fit("Netherlands", 100, 43, 2.6) -> plots[["Netherlands"]]
# North Macedonia
head(load_fit_res("North Macedonia"),10)
plot_fit("North Macedonia", 100, 60, 1.8) -> plots[["North Macedonia"]]
# Norway
head(load_fit_res("Norway"),10)
plot_fit("Norway", 100, 35, 1.8) -> plots[["Norway"]]
# Poland
head(load_fit_res("Poland"),10)
plot_fit("Poland", 100, 90, 2.5) -> plots[["Poland"]]
# Portugal
head(load_fit_res("Portugal"),10)
plot_fit("Portugal", 100, 55, 2.6) -> plots[["Portugal"]]
# Romania
head(load_fit_res("Romania"),10)
plot_fit("Romania", 100, 60, 2.5) -> plots[["Romania"]]
# Russian Federation
head(load_fit_res("Russian Federation"),10)
plot_fit("Russian Federation", 100, 35, 2) -> plots[["Russian Federation"]]
# San Marino
head(load_fit_res("San Marino"),10)
plot_fit("San Marino", 100, 94, 2.1) -> plots[["San Marino"]]
# Serbia
head(load_fit_res("Serbia"),10)
plot_fit("Serbia", 100, 59, 2) -> plots[["Serbia"]]
# Slovakia
head(load_fit_res("Slovakia"),10)
plot_fit("Slovakia", 100, 116, 2.4) -> plots[["Slovakia"]]
# Slovenia
head(load_fit_res("Slovenia"),10)
plot_fit("Slovenia", 100, 90, 2.5) -> plots[["Slovenia"]]
# Spain
head(load_fit_res("Spain"),10)
plot_fit("Spain", 100, 45, 2.9) -> plots[["Spain"]]
# Sweden
head(load_fit_res("Sweden"),10)
plot_fit("Sweden", 100, 19, 1.8) -> plots[["Sweden"]]
# Sweden
head(load_fit_res("Switzerland"),10)
plot_fit("Switzerland", 100, 21, 1.9) -> plots[["Switzerland"]]
# Turkey
head(load_fit_res("Turkey"),10)
plot_fit("Turkey", 100, 41, 2.3) -> plots[["Turkey"]]
# Ukraine
head(load_fit_res("Ukraine"),10)
plot_fit("Ukraine", 100, 53, 2.2) -> plots[["Ukraine"]]
# United Kingdom of Great Britain
head(load_fit_res("United Kingdom of Great Britain"),10)
plot_fit("United Kingdom of Great Britain", 100, 56, 3.4) -> 
  plots[["United Kingdom of Great Britain"]]

# impute from neighbors
new <- read_csv("data/gs_fitted_v2.csv")
members_missing <- data.table(country_name = c("Andorra", "Iceland", "Kazakhstan", "Kyrgyzstan",
                     "Monaco", "San Marino", "Tajikistan", "Turkmenistan",
                     "Uzbekistan")) %>% 
  left_join(members, "country_name") %>% 
  left_join(members_shp %>% rownames_to_column(), "wb")

neighbors <- spdep::poly2nb(members_shp)
neighbors[[10]] <- which(members_shp$wb %in% c("NOR", "GBR", "IRL"))
neighbors[[40]] <- 1:53

lapply(as.numeric(members_missing$rowname),
       function(x)
         neighbors[[x]]) %>% 
  map(~new[., ]) %>% 
  map(summarise, 
      t_intro = median(t_intro, na.rm = T),
      R0 = median(R0, na.rm = T)) %>% bind_rows() %>% 
  mutate(country_name = members_missing$country_name,
         t_intro = round(t_intro),
         date = as.character(as.Date("2019-12-01") + t_intro),
         wb = countrycode(country_name, "country.name", "wb"),
         Fit = FALSE) -> to_merge

new %<>% group_by(Fit) %>% group_split()

new[[1]] %<>% dplyr::select(-t_intro, -R0, -date) %>% 
  left_join(to_merge, by = c("wb", "country_name", "Fit")) %>% 
  mutate(date = ymd(date))

new %<>% bind_rows() %>% arrange(country_index)

write_csv(new, "data/gs_fitted_v2_imputed.csv")

for(i in seq_along(plots)){
  ggsave(paste0("figs/intermediate/gs_fit_best/", names(plots)[i], ".png"), plots[[i]])
}

