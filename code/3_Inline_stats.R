#### vaccination progress ####
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
            imputeTS::na_interpolation) -> combo_vac

owid_vac %>%
  left_join(vac_denom, by = "wb") %>% 
  filter(wb != "CYP") %>% 
  mutate(p = people_vaccinated/tot) %>% 
  left_join(combo_vac, by = c("date" = "ms_date")) %>% 
  .[,c("location", "wb", "date", "p", paste0("R",1:4))] %>% 
  group_by(wb) %>% group_split() %>% 
  map(filter, !is.na(p)) %>% 
  map(arrange, desc(date)) %>% 
  map(~.[1,]) %>% bind_rows() %>% 
  pivot_longer(col = starts_with("R")) %>% 
  mutate(sign = p >= value, name = parse_number(name)) %>% 
  filter(sign == T) %>% 
  group_by(wb) %>% group_split() %>% 
  map(arrange, desc(name)) %>% 
  map(~.[1,]) %>% 
  bind_rows() %>% 
  filter(name == 4) %>% 
  arrange(p)

####
res <- read_rds("data/intermediate/priority_selection_2.rds")
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
                !wb %in% members_remove) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  # left_join(res[[2]] %>% 
  #             bind_rows(.id = "ROS")  %>% 
  #             mutate(ROS = paste0("R",ROS)) %>% 
  #             group_by(ROS, population) %>% 
  #             summarise(supply = max(supply, na.rm = T)),
  #           by = c("ROS", "population")) %>% 
  # mutate(adjLE = adjLE_pd* supply,
  #        QALYloss = QALYloss_pd*supply,
  #        HC = HC_pd * supply) %>% 
  group_by(ROS, policy) %>% 
  summarise_at(vars(cases, death_o, adjLE, QALYloss, HC),
               sum) -> tab_regional

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
  # mutate(dir = if_else(variable %in% c("cases", "death_o"),
  #                      "min",
  #                      "max"),
  #        # we flip the sign for dir == "min" because we are looking to maximize
  #        # all the _pd HE measures, but minimise all the cases and death counts
  #        value = if_else(wb %in% members_remove, as.numeric(NA), value),
  #        value = if_else(dir == "min", -1*value, value)) %>% 
  group_by(population, variable, ROS) %>% group_split() %>% 
  map(arrange, value) %>% 
  map(~.[1,]) %>% bind_rows() %>% 
  # dplyr::select(-t.x, -supply, -dir) %>%
  dplyr::select(-w, -wb, -policy) %>% 
  mutate(value = abs(value)) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(policy = "tailored") %>% 
  # left_join(res[[2]] %>% 
  #             bind_rows(.id = "ROS")  %>% 
  #             mutate(ROS = paste0("R",ROS)) %>% 
  #             group_by(ROS, population) %>% 
  #             summarise(supply = max(supply, na.rm = T)),
  #           by = c("ROS", "population")) %>% 
  # mutate(QALYloss = QALYloss_pd   * supply,
  #        adjLE = adjLE_pd*supply,
  #        HC = HC_pd*supply,
  #        policy = "tailored") %>% 
  dplyr::select(colnames(tab_regional)) %>% 
  group_by(ROS, policy) %>% 
  summarise_at(vars(cases, death_o, adjLE, QALYloss, HC),
               sum) -> tab_customised

tab_regional %>% 
  bind_rows(tab_customised) %>% 
  arrange(ROS) %>% filter(policy != "0") %>% 
  pivot_longer(cols = c(cases, death_o, adjLE, QALYloss, HC)) %>% 
  # mutate(dir = if_else(name %in% c("cases", "death_o"), "min", "max"),
  #        value = if_else(dir == "min", -1*value, value)) %>% 
  group_by(name, ROS) %>% group_split() %>% 
  map(mutate, rk = rank(value, ties.method = "last")) %>% bind_rows() %>% 
  mutate(name = factor(name,
                       levels = c("death_o", "cases", 
                                  "adjLE",
                                  "QALYloss",
                                  "HC"),
                       labels = c("Deaths","Cases", 
                                  "Adj. Life Expenctancy",
                                  "Quality Adj. Life Years",
                                  "Human Capital"
                       )),
         ROS = factor(ROS, levels = paste0("R",c(1,2,3,4)),
                      labels = rollout_labels)) %>% 
  group_by(ROS, name) %>% group_split() %>% 
  map(mutate, best_perform = min(value)) %>% 
  bind_rows() %>% 
  mutate(p_increase = (value/best_perform - 1) %>% 
           round(., 2),
         p_increase = p_increase*100 %>% round) -> tab_combined

tab_combined %>% 
  dplyr::select(-rk, -value) %>% 
  pivot_wider(names_from = policy, values_from = p_increase)


#### supplemental tables
res <- read_rds("data/intermediate/priority_selection_2_debug.rds")

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
                w == "2022",
                !wb %in% members_remove) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  # left_join(res[[2]] %>% 
  #             bind_rows(.id = "ROS")  %>% 
  #             mutate(ROS = paste0("R",ROS)) %>% 
  #             group_by(ROS, population) %>% 
  #             summarise(supply = max(supply, na.rm = T)),
  #           by = c("ROS", "population")) %>% 
  # mutate(adjLE = adjLE_pd* supply,
  #        QALYloss = QALYloss_pd*supply,
  #        HC = HC_pd * supply) %>% 
  group_by(ROS, policy) %>% 
  summarise_at(vars(cases, death_o, adjLE, QALYloss, HC),
               sum) -> tab_regional

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
                w == "2022",
                !wb %in% members_remove)  %>% 
  # mutate(dir = if_else(variable %in% c("cases", "death_o"),
  #                      "min",
  #                      "max"),
  #        # we flip the sign for dir == "min" because we are looking to maximize
  #        # all the _pd HE measures, but minimise all the cases and death counts
  #        value = if_else(wb %in% members_remove, as.numeric(NA), value),
  #        value = if_else(dir == "min", -1*value, value)) %>% 
  group_by(population, variable, ROS) %>% group_split() %>% 
  map(arrange, value) %>% 
  map(~.[1,]) %>% bind_rows() %>% 
  # dplyr::select(-t.x, -supply, -dir) %>%
  dplyr::select(-w, -wb, -policy) %>% 
  mutate(value = abs(value)) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(policy = "tailored") %>% 
  # left_join(res[[2]] %>% 
  #             bind_rows(.id = "ROS")  %>% 
  #             mutate(ROS = paste0("R",ROS)) %>% 
  #             group_by(ROS, population) %>% 
  #             summarise(supply = max(supply, na.rm = T)),
  #           by = c("ROS", "population")) %>% 
  # mutate(QALYloss = QALYloss_pd   * supply,
  #        adjLE = adjLE_pd*supply,
  #        HC = HC_pd*supply,
  #        policy = "tailored") %>% 
  dplyr::select(colnames(tab_regional)) %>% 
  group_by(ROS, policy) %>% 
  summarise_at(vars(cases, death_o, adjLE, QALYloss, HC),
               sum) -> tab_customised

tab_regional %>% 
  bind_rows(tab_customised) %>% 
  arrange(ROS) %>% filter(policy != "0") %>% 
  pivot_longer(cols = c(cases, death_o, adjLE, QALYloss, HC)) %>% 
  # mutate(dir = if_else(name %in% c("cases", "death_o"), "min", "max"),
  #        value = if_else(dir == "min", -1*value, value)) %>% 
  group_by(name, ROS) %>% group_split() %>% 
  map(mutate, rk = rank(value, ties.method = "last")) %>% bind_rows() %>% 
  mutate(name = factor(name,
                       levels = c("death_o", "cases", 
                                  "adjLE",
                                  "QALYloss",
                                  "HC"),
                       labels = c("Deaths","Cases", 
                                  "Adj. Life Expenctancy",
                                  "Quality Adj. Life Years",
                                  "Human Capital"
                       )),
         ROS = factor(ROS, levels = paste0("R",c(1,2,3,4)),
                      labels = rollout_labels)) %>% 
  group_by(ROS, name) %>% group_split() %>% 
  map(mutate, best_perform = min(value)) %>% 
  bind_rows() %>% 
  mutate(p_increase = (value/best_perform - 1) %>% 
           round(., 2),
         p_increase = p_increase*100 %>% round) -> tab_combined

tab_combined %>% filter(policy %in% c(1:4)) %>% 
dplyr::select(-best_perform, -rk, -value) %>% 
  pivot_wider(names_from = policy,
              values_from = p_increase) #%>%
  mutate(tot = `1` + `2` + `3` + `4`)
  # mutate(diff = abs(`3`-`4`))

##### examine the proportion under influence ####
res[[1]] %>% 
  bind_rows(.id = "ROS") %>% 
  mutate(ROS = paste0("R",ROS),
         wb = countrycode(population, "country.name", "wb")) %>% 
  dplyr::filter(variable %in% c("cases", "death_o",
                                "adjLE_pd", 
                                # "VSLmlns_pd",
                                "QALYloss_pd",
                                "HC_pd"),
                w == "2022") %>% 
  mutate(dir = if_else(variable %in% c("cases", "death_o"),
                       "min",
                       "max"),
         # we flip the sign for dir == "min" because we are looking to maximize
         # all the _pd HE measures, but minimise all the cases and death counts
         value = if_else(dir == "min", -1*value, value),
         value = if_else(wb %in% members_remove, as.numeric(NA), value)) %>% 
  group_by(population, variable, ROS) %>% group_split() %>% 
  map(arrange, desc(value)) %>% 
  map(~.[1,]) %>% bind_rows() %>% 
  mutate(policy = paste0("P",policy)) %>% 
  group_by(ROS, variable) %>% group_split() %>% 
  map(full_join, members_world, by = "wb") %>% 
  map(st_as_sf) %>% 
  map(arrange, ROS) %>% 
  map(mutate, ROS = zoo::na.locf(ROS)) %>%
  map(mutate, variable = zoo::na.locf(variable)) %>% 
  bind_rows() %>% 
  mutate(variable = factor(variable,
                           levels = c("death_o", "cases", 
                                      "adjLE_pd",
                                      # "VSLmlns_pd",
                                      "QALYloss_pd",
                                      "HC_pd"),
                           labels = c("Deaths","Cases", 
                                      "Adj. Life Expectancy",
                                      # "Value of Statistical Life",
                                      "Quality Adj. Life Years",
                                      "Human Capital"
                           )),
         ROS = factor(ROS,
                      levels = paste0("R",c(1,2,3,4)),
                      labels = rollout_labels),
         policy = factor(policy, levels = paste0("P",1:4))) %>% 
  st_as_sf() -> tab1

tab1 %>% 
  data.table %>% 
  filter(!is.na(policy),
         !wb %in% members_remove) %>% 
  dplyr::select(ROS, wb, population, variable, policy) %>% 
  left_join(cm_populations %>% 
              filter(name %in% members$country_name) %>% 
              # pull(name) %>% unique %>% length
              group_by(name) %>% 
              summarise(pop = sum(f + m)) %>% 
              mutate(wb = countrycode(name, "country.name", "wb")),
            by = c("wb", "population" = "name")) %>% 
  group_by(ROS, policy, variable) %>% 
  summarise(pop = sum(pop),
            n = n()) -> tab2

tab2 %>% 
  mutate(pop_tot = 848407,
         n_tot = 38,
         r_pop = pop/pop_tot,
         variable = factor(variable,
                           levels = c("Deaths", "Cases", "Adj. Life Expectancy",
                                      "Quality Adj. Life Years", "Human Capital")),
         r_n = n/n_tot) %>%
  dplyr::select(-pop, -n, -pop_tot, -n_tot, -r_n) %>% 
  pivot_wider(names_from = policy,
              values_from = r_pop)%>% 
  replace(., is.na(.), 0) %>% 
  arrange(ROS, variable)-> tmp 

write_csv(tmp, "data/intermediate/r_pop.csv")
# %>% 
  # mutate(tot = P1 + P2 + P3 + P4) 

  # dplyr::select(-pop) %>% 
  # pivot_wider(names_from = policy,
  #             values_from = n) %>%
  # replace(., is.na(.), 0) %>%
#%>% pull(tot) %>% unique


tab2 %>% 
  mutate(pop_tot = 848407,
         n_tot = 38,
         variable = factor(variable,
                           levels = c("Deaths", "Cases", "Adj. Life Expectancy",
                                      "Quality Adj. Life Years", "Human Capital")),
         r_pop = pop/pop_tot,
         r_n = n/n_tot) %>%
  dplyr::select(-pop, -n, -pop_tot, -n_tot, -r_pop) %>% 
  pivot_wider(names_from = policy,
              values_from = r_n) %>% 
  replace(., is.na(.), 0) %>% 
  arrange(ROS, variable) -> tmp 
  
write_csv(tmp, "data/intermediate/r_n.csv")

# mutate(check = P1+P2+P3+P4)

read_excel("data/WHO_EUR_VAC LitRev_20210614.xlsx",
           sheet = "Selected_Updated") -> LitRev


LitRev %>% 
  group_by_at(colnames(LitRev)[c(22)]) %>% 
  tally()

LitRev %>% 
  group_by_at(colnames(LitRev)[c(18)]) %>% 
  tally()


lapply(16:22, function(x){
  LitRev %>% 
    group_by_at(colnames(LitRev)[x]) %>% 
    tally()
})

#### discussion on adolescents ####

read_rds("data/intermediate/priority_selection_2_adol.rds") %>% 
  .[[4]] %>% 
  map(rename, policy = ROS) %>% 
  bind_rows(.id = "ROS") %>% 
  mutate(ROS = paste0("R",ROS)) 

w <- read_rds("data/intermediate/priority_selection_2_adol.rds") %>% 
  .[[3]] %>% 
  bind_rows(.id = "ROS") %>% 
  mutate(ROS = paste0("R",ROS)) 

wo <- read_rds("data/intermediate/priority_selection_2.rds") %>% 
  .[[3]] %>% 
  bind_rows(.id = "ROS") %>% 
  mutate(ROS = paste0("R",ROS)) %>% 
  filter(policy %in% c(0,3,4))


vars <- c("death_o", "cases",  "adjLE", "QALYloss","HC")
tmp <- list()

for(i in 1:5){
  bind_rows(w, wo) %>% 
    distinct() %>% 
    filter(w == "Before 2023") %>% 
    arrange(population, ROS, policy) %>%
    separate(policy, into = c("policy","type")) %>% 
    mutate(policy = parse_number(policy),
           type = if_else(policy %in% c(3,4) & is.na(type), "basic", type),
           wb = countrycode(population, "country.name", "wb")) %>% 
    dplyr::select(ROS, policy, run, population, vars[i], policy, type, wb) %>% 
    filter(policy != 0,
           ROS %in% c("R3","R4"),
           !wb %in% members_remove) %>% 
    pivot_wider(names_from = type,
                values_from = vars[i]) %>% 
    mutate(p1 = (a - basic)/basic %>% round(., 2),
           p2 = (b - basic)/basic %>% round(., 2),
           compare = p1 < p2)-> tmp[[i]]
}

tmp %>% 
  bind_rows(.id = "metric") %>% 
  mutate(metric = factor(metric,
                         levels = 1:5,
                         labels = c("Deaths","Cases", 
                                    "Adj. Life Expectancy",
                                    "Quality Adj. Life Years",
                                    "Human Capital"
                         ))) %>% 
  filter(p1 > 0 | p2 > 0) %>% 
  arrange(desc(p1)) %>% View()


tmp %>% 
  bind_rows(.id = "metric") %>% 
  mutate(metric = factor(metric,
                         levels = 1:5,
                         labels = c("Deaths","Cases", 
                                    "Adj. Life Expectancy",
                                    "Quality Adj. Life Years",
                                    "Human Capital"
                         ))) %>%
  dplyr::select(-a, -b) %>% 
  pivot_longer(cols = c(p1, p2)) %>% 
  mutate(policy = factor(policy, 
                         levels = 3:4,
                         labels = c("V60", "V75")),
         name = factor(name,
                       levels = c("p1", "p2"),
                       labels = c("a", "b"))) %>% 
  ggplot(., aes(x = name, y = value, group = population)) + 
  geom_line(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red") +
  geom_point(size = 3) +
  facet_grid(ROS + policy ~ metric) +
  labs(x = "Ways to include adolescents",
       y = "%diff compared to not including adolescents") +
  theme_bw() -> p

ggsave("figs/supplemental/adol.png", p, height = 8, width = 12)

tmp %>% 
  bind_rows(.id = "metric") %>% 
  mutate(metric = factor(metric,
                         levels = 1:5,
                         labels = c("Deaths","Cases", 
                                    "Adj. Life Expectancy",
                                    "Quality Adj. Life Years",
                                    "Human Capital"
                         ))) %>% 
  filter(p1 > 0 | p2 > 0) %>% 
  arrange(p1, p2) %>% 
  group_by(population) %>% tally

# AZE can be an example
i = 3
m <- model_selected_2

lapply(1:4, function(j){
  predict_outbreak(cn = m$country_name[i],
                   date_start = as.character(as.Date("2019-12-01") +
                                               m$t[i]),
                   ve_i = 0.95,
                   ms_date = combo[[j]]$ms_date,
                   ms_cov = combo[[j]]$ms_cov,
                   ve_d = 0,
                   R = m$r[i],
                   wane = c(52*3, 1),
                   prp = priority_policy,
  )
}) -> before

lapply(1:4, function(j){
  predict_outbreak(cn = m$country_name[i],
                   date_start = as.character(as.Date("2019-12-01") +
                                               m$t[i]),
                   ve_i = 0.95,
                   ms_date = combo[[j]]$ms_date,
                   ms_cov = combo[[j]]$ms_cov,
                   ve_d = 0,
                   R = m$r[i],
                   wane = c(52*3, 1),
                   prp = priority_policy2,
  )
}) -> after

lapply(before, "[[", "dyna") %>% 
  map(bind_rows, .id = "policy") %>% 
  map(mutate, policy = paste0("p",policy)) %>% 
  bind_rows(.id = "ROS") %>% 
  mutate(ROS = paste0("R",ROS)) -> seg1

lapply(after, "[[", "dyna") %>% 
  map(bind_rows, .id = "policy") %>% 
  map(mutate, policy = factor(policy, 
                              levels = 1:4,
                              labels = names(priority_policy2))) %>% 
  bind_rows(.id = "ROS") %>% 
  mutate(ROS = paste0("R",ROS)) -> seg2

bind_rows(seg1, seg2) %>% 
  filter(ROS %in% c("R3","R4"),
         !policy %in% c("p1", "p2")) %>% 
  mutate(date = t + ymd("2019-12-01") + m$t[i])  -> tmp

tmp %>% 
  filter(compartment == "death_o") %>% 
  separate(group, into = c("LL","UL"), sep = "-") %>% 
  separate(policy, into = c("policy", "variation"), sep = "_") %>% 
  # filter(date > "2021-06-30") %>% 
  mutate(LL = parse_number(LL)) %>% 
  ggplot(aes(x = date, y = value, group = variation, color = variation)) +
  geom_line() +
  facet_grid(ROS + policy ~ LL) +
  geom_vline(xintercept = ymd("2022-02-21")) +
  geom_vline(xintercept = ymd("2021-02-21"))


before[[4]]$main %>% 
  filter(policy %in% c(0, "p3","p4")) %>% 
  bind_rows(after[[4]]$main) %>% distinct() %>% 
  # select(t.x, policy, starts_with("Y", ignore.case = T)) %>% 
  # pivot_longer(starts_with("Y")) %>% 
  arrange(policy) %>%   dplyr::select(t.x, policy, S, E,  Ip, Is, foi,
                                      Ia, R, V, cases, foi, death_o) %>%
  group_by(t.x, policy) %>% pivot_longer(cols = c(S, E,  Ip, Is, Ia, foi,
                                                  R, V, cases, foi, death_o)) %>%
  separate(policy, into = c("policy", "variation"), sep = "_") %>%
  filter(policy != 0) %>% 
  # mutate(name = parse_number(name)) %>% 
  ggplot(., aes(x = t.x, y = value, 
                # group = name, color = name
                group = variation, 
                color = variation
                )) +
  geom_line() +
  facet_wrap(name~policy, scales = "free")

bind_rows(w, wo) %>% 
  distinct() %>% 
  filter(w == "Before 2023",
         population == "Sweden") %>% 
  dplyr::select(ROS, policy, cases) %>% 
  mutate(policy = case_when(policy == 3 ~ "p3",
                            policy == 4 ~ "p4",
                            TRUE ~ policy)) %>% 
  pivot_wider(names_from = policy, values_from = cases) %>% 
  dplyr::select(c("ROS", "0", "p3","p3_a","p3_b","p4","p4_a","p4_b"))

model_selected_2 %>% 
  filter(!wb %in% members_remove) %>% 
  pull(r) %>% quantile(., c(0.25, 0.5, 0.75))

read_rds("data/intermediate/econ_by_VE_2_debug.rds") -> tmp


#### Figure S14 further exploration ####
res <- read_rds("data/intermediate/priority_selection_2_debug.rds") %>%
  .[[3]] %>%
  bind_rows(.id = "ROS") %>%
  mutate(ROS = paste0("R",ROS))  %>%
  mutate(wb = countrycode(population, "country.name", "wb")) %>%
  dplyr::select(colnames(res1)) %>%
  filter(!wb %in% members_remove)

read_rds("data/intermediate/priority_selection_2_debug.rds") %>%
  .[[2]] %>% 
  map(bind_rows) %>% 
  bind_rows(.id = "ROS") %>%
  mutate(ROS = paste0("R",ROS))  %>%
  mutate(wb = countrycode(population, "country.name", "wb")) -> main

data.table(start = c(rep("2021-03-01", 2),
                     rep("2021-01-01", 2)),
           policy = paste0("p",1:4)) %>% 
  mutate(start = ymd(start),
         marker1 = start %m+% months(6),
         marker2 = start %m+% months(12),
         marker3 = start %m+% months(18),
         marker4 = ymd("2022-12-31")) -> marker

main %>% 
  left_join(marker, by = "policy") %>% 
  ggplot(., aes(x = date, y = death_o, group = population)) +
  geom_line() +
  facet_wrap(ROS~policy) +
  geom_vline(data = marker, aes(xintercept = start)) +
  geom_vline(data = marker, aes(xintercept = marker1), color = "red") +
  geom_vline(data = marker, aes(xintercept = marker2), color = "orange") +
  geom_vline(data = marker, aes(xintercept = marker3), color = "yellow") +
  geom_vline(data = marker, aes(xintercept = marker4), color = "green") 

