# #### generate decisions ####
gen_priority <- function(m,
                         w = c(52*3, 52),
                         prp = NULL){
  
 n_param <- colnames(m) %in% c("t", 
                               "r", 
                               "rho", 
                               "par3") %>% sum
 res1 <- res2 <- res3 <- res4 <- list()
 
 for(j in seq_along(combo)){
   tmp <- list()
   for(i in seq_len(nrow(m))){
   # for(i in 1){
     suppressMessages(
       predict_outbreak(cn = m$country_name[i],
                        date_start = as.character(as.Date("2019-12-01") +
                                                    m$t[i]),
                        ve_i = 0.95,
                        ms_date = combo[[j]]$ms_date,
                        ms_cov = combo[[j]]$ms_cov,
                        reporting = if_else(n_param == 2, 1, m$par3[i]),
                        ve_d = 0,
                        R = m$r[i],
                        wane = w,
                        mobility = schedule_raw,
                        prp = prp,
                        ) -> tmp[[i]]
     )
     print(paste0("Combo ", j, ", Row ",i,", country ",
                  m$country_name[i] ," is completed!"))
   }
   
   lapply(tmp, "[[", "econ") %>%
     bind_rows() -> res1[[j]]

   lapply(tmp, "[[", "main") -> res2[[j]] #%>%
    #map(select, population, policy,  date, supply, starts_with("Y")) %>%
     #map(distinct) %>%
     #bind_rows()  
   
   lapply(tmp, "[[", "econ_w") %>%
     bind_rows()  -> res3[[j]]
   
   lapply(tmp, "[[", "vac_para") %>%
     bind_rows() -> res4[[j]]
   
 }
 
 return(list(res1, res2, res3, res4))
}

# baseline model
priority_selection_2 <- gen_priority(m = model_selected_2,
                                     prp = priority_policy)
write_rds(priority_selection_2, 
          "data/intermediate/priority_selection_2_debug.rds")

# baseline model + additional fitted parameter
priority_selection_3 <- gen_priority(m = model_selected_3,
                                     prp = priority_policy)
write_rds(priority_selection_3, 
          "data/intermediate/priority_selection_3_debug.rds")

# baseline model + extended vaccine waning
priority_selection_2_w <- gen_priority(m = model_selected_2, 
                                       w = c(52*3, 52*3),
                                       prp = priority_policy)
write_rds(priority_selection_2_w, 
          "data/intermediate/priority_selection_2_w_debug.rds")

# baseline model + additional fitted character + extended vaccine waning
priority_selection_3_w <- gen_priority(m = model_selected_3, 
                                       w = c(52*3, 52*3),
                                       prp = priority_policy)
write_rds(priority_selection_3_w, 
          "data/intermediate/priority_selection_3_w_debug.rds")

# including children
priority_selection_2_adol <- gen_priority(m = model_selected_2,
                                          prp = priority_policy2)

write_rds(priority_selection_2_adol, 
          "data/intermediate/priority_selection_2_debug_adol.rds")

#### generate fit results ####  
# select examples
fit_examples <- data.frame(wb = c("GBR", "GEO", "HUN")
                           ) %>%
  left_join(members, by = "wb") %>%
  left_join(model_selected_3, by = "wb")


if(!"par3" %in% colnames(fit_examples)){
  fit_examples[,"par3"] <- 1
}

res <- list()
for(i in seq_len(nrow(fit_examples))){
  suppressMessages(
    predict_outbreak(cn = fit_examples$`country_name.x`[i],
                     date_start = as.character(as.Date("2019-12-01") +
                                                 fit_examples$t[i]),
                     date_end = "2022-12-31",
                     ve_i = 0.95,
                     ve_d = 0,
                     reporting = fit_examples$par3[i],
                     R = fit_examples$r[i]) -> res[[i]]
  )
}

res_st <- list()
for(i in 1:nrow(fit_examples)){
  gen_country_basics(country = fit_examples$country_name.x[i],
                     date_start = as.character(as.Date("2019-12-01") +
                                                 fit_examples$t[i]),
                     date_end = "2022-12-31",
                     R0_assumed = fit_examples$r[i],
                     waning_nat = 52*7*3,
                     mobility = schedule_raw,
                     deterministic = F) -> params
  params$processes[[2]]$pro[1,] <-
    params$processes[[2]]$pro[1,] * fit_examples$par3[i]
  cm_simulate(params, n_threads = 4, n = 100) -> res_st[[i]]
  rm(params)
}

lapply(res, "[[", "main") %>%
  bind_rows() %>% dplyr::filter(policy == 0) %>%
  dplyr::select(population, date, death_o) %>%
  mutate(run = 0) -> seg1

lapply(res_st, "[[", "dynamics") %>%
  map(filter, compartment == "death_o") %>%
  bind_rows() %>%
  group_by(run, t, population) %>%
  summarise(value = sum(value)) %>%
  left_join(fit_examples %>%
              dplyr::select(country_name.x, t) %>%
              setNames(c("population", "t_intro")),
            by = "population") %>%
  mutate(date = as.Date("2019-12-01") + t_intro + t) %>%
  rename(death_o = value) -> seg2

seg2 %>% dplyr::select(colnames(seg1)) %>%
  bind_rows(seg1 %>% mutate(date = ymd(date))) %>%
  mutate(metric = if_else(run == 0, "deterministic", "stochastic")) %>%
  left_join(fit_examples %>%
              dplyr::select(country_name.x, wb) %>%
              setNames(c("population", "wb"))) %>%
  bind_rows(
    epi[loc %in% fit_examples$wb, !"cases"] %>% rename(wb = loc) %>%
      mutate(run = NA, t = NA, metric = "empirical") %>%
      left_join(fit_examples %>%
                  dplyr::select(wb, country_name.x) %>%
                  setNames(c("wb", "population"))) %>%
      rename(death_o = deaths)
    ) -> seg

write_rds(seg, "data/intermediate/fit_examples_3.rds")

#### look at predicted proportion immuned ####
gen_history <- function(m, w = c(52*3*7, 52*7)){
  n_param <- colnames(m) %in% c("t", "r", "rho", "par3") %>% sum
  tmp <- list()
  for(i in seq_len(nrow(m))){
    suppressMessages(
      gen_country_basics(country = m$country_name[i],
                       date_start = as.character(as.Date("2019-12-01") +
                                                   m$t[i]),
                       date_end = "2020-12-31",
                       waning_nat = w[1],
                       mobility = schedule_raw,
                       # prp = priority_policy,
                       # reporting = if_else(n_param == 2, 1, m$par3[i]),
                       R0_assumed = m$r[i]) %>%
        update_vac_char(para = .,
                        waning_vac = w[2],
                        ve_i = 0.95,
                        ve_d = 0) %>% 
        cm_simulate -> tmp[[i]]
    )
    print(paste0("Row ",i,", country ",
                 m$country_name[i] ," is completed!"))
  }
  return(tmp)
}

epi_history_2 <- gen_history(model_selected_2)
write_rds(epi_history_2, "data/intermediate/epi_history_2_debug.rds")
epi_history_3 <- gen_history(model_selected_3)
write_rds(epi_history_3, "data/intermediate/epi_history_3_debug.rds")
#
epi_history_2_w <- gen_history(model_selected_2, w = c(52*3*7, 52*7*3))
write_rds(epi_history_2_w, "data/intermediate/epi_history_2_w_debug.rds")
epi_history_3_w <- gen_history(model_selected_3, w = c(52*3*7, 52*7*3))
write_rds(epi_history_3_w, "data/intermediate/epi_history_3_w_debug.rds")

# epi_history_3_w <- readRDS("~/GitHub/COVID19_EUR_VAC/data/intermediate/epi_history_3_w.rds")
# epi_history_2_w <- readRDS("~/GitHub/COVID19_EUR_VAC/data/intermediate/epi_history_2_w.rds")

gen_non_S <- function(file){
  
  tmp2 <- read_rds(file)
  
  lapply(tmp2,"[[","dynamics") %>% 
    bind_rows() %>% filter(compartment == "S") %>% 
    group_by(run, t, population, compartment) %>% 
    summarise(value = sum(value)) -> tmp1
  
  tmp1[,"population"] %>% distinct() %>% 
    mutate(wb = countrycode(population, "country.name", "wb")) %>% 
    right_join(tmp1, by = "population") -> tmp1
  
  tmp1 %>% 
    left_join(model_selected_2, by = "wb") %>% 
    rename(date_start = date) %>% 
    mutate(date = date_start + t.x) %>% 
    dplyr::select(-t.x, -t.y, -country_index, -country_name) %>%
    mutate(date = as.character(date),
           date_start = as.character(date_start)) -> seg 
    
  seg %>% filter(date == ymd("2020-12-31")) %>% 
    bind_rows(seg %>% filter(date == date_start)) -> tmp1
  
  rm(seg)
  
  tmp1 %>%
    mutate(status = case_when(date == ymd("2020-12-31") ~ "end",
                              date == date_start ~ "start")) %>% 
    dplyr::select(-date_start, -date, -r, -compartment, -run) %>% 
    pivot_wider(names_from = status, values_from = value) %>% 
    mutate(diff = start - end,
           p = diff/ start) %>% 
    dplyr::select(population, start, end, diff, p, wb) -> tmp
  
  return(tmp)
}

non_S_2 <- gen_non_S("data/intermediate/epi_history_2_debug.rds")
write_rds(non_S_2, "data/intermediate/non_S_2_debug.rds")
non_S_3 <- gen_non_S("data/intermediate/epi_history_3_debug.rds")
write_rds(non_S_3, "data/intermediate/non_S_3_debug.rds")

non_S_2_w <- gen_non_S("data/intermediate/epi_history_2_w_debug.rds")
write_rds(non_S_2_w, "data/intermediate/non_S_2_w_debug.rds")
non_S_3_w <- gen_non_S("data/intermediate/epi_history_3_w_debug.rds")
write_rds(non_S_3_w, "data/intermediate/non_S_3_w_debug.rds")
# beep(8)

#### SA by vaccine characteristics #### 
check_econ_VE <- function(m, w = c(52*3, 52)){
  tmp <- list()
  n_param <- colnames(m) %in% c("t", "r", "rho", "par3") %>% sum 
  for(j in 1:4){
    tmp[[j]] <- list()
    for(i in which(!m$wb %in% members_remove)){
    # for(i in c(1,3,4)){
      suppressMessages(
        map2(ve_tab$ei_v,
             ve_tab$ed_vi,
             ~   predict_outbreak(cn = m$country_name[i],
                                  date_start = as.character(as.Date("2019-12-01") + 
                                                              m$t[i]),
                                  date_end = "2022-12-31",
                                  ve_i = .x,
                                  ve_d = .y,
                                  ms_date = combo[[j]]$ms_date,
                                  ms_cov = combo[[j]]$ms_cov,
                                  reporting = if_else(n_param == 2, 1, m$par3[i]),
                                  wane = w,
                                  mobility = schedule_raw,
                                  prp = priority_policy,
                                  R = m$r[i])) %>% 
          lapply(., "[[", "econ_w") %>% 
          bind_rows(.id = "ve_set") -> tmp[[j]][[i]]
      )
      print(paste0("ROS", j, "Row ",i,", country ", 
                   m$country_name[i] ," is completed!"))
    }
    tmp[[j]] %<>% 
      bind_rows() 
  }
  tmp %<>% 
    bind_rows(.id = "ROS") %>% 
    mutate(ROS = paste0("R",ROS))

  return(tmp)
}

econ_VE_2 <- check_econ_VE(model_selected_2)
write_rds(econ_VE_2, "data/intermediate/econ_by_VE_2_debug.rds")

# beep(8)
econ_VE_3 <- check_econ_VE(model_selected_3)
write_rds(econ_VE_3, "data/intermediate/econ_by_VE_3_debug.rds")
# beep(8)
econ_VE_2_w <- check_econ_VE(model_selected_2, w = c(52*3, 52*3))
write_rds(econ_VE_2_w, "data/intermediate/econ_by_VE_2_w_debug.rds")
# beep(8)
econ_VE_3_w <- check_econ_VE(model_selected_3, w = c(52*3, 52*3))
write_rds(econ_VE_3_w, "data/intermediate/econ_by_VE_3_w_debug.rds")
# beep(8)

# rm(tmp)
# write_rds(tmp, "data/intermediate/all_by_VE.rds")


