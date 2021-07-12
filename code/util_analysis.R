gen_country_basics <- function(country,
                               waning_nat = 52*7*3,
                               R0_assumed  = 2.7,
                               date_start = "2020-01-01",
                               date_end = "2022-12-31",
                               mobility = NULL,
                               # s_A = 0.1,
                               deterministic = TRUE){
  
  require(countrycode)
  
  wb_tmp = countrycode(country, "country.name", "wb")
  c_tmp = mobility %>% 
    filter(wb == wb_tmp,
           date >= lubridate::ymd(date_start),
           date <= lubridate::ymd(date_end)) %>% 
    arrange(date)
  
  t_run <- nrow(c_tmp)
  
  para = cm_parameters_SEI3R(dem_locations = country, 
                             date_start = date_start, 
                             date_end = date_end,
                             dE  = cm_delay_gamma(2.5, 2.5, 
                                                  t_max = 15, t_step = 0.25)$p,
                             dEa = cm_delay_gamma(2.5, 2.5, 
                                                  t_max = 15, t_step = 0.25)$p,
                             dIp = cm_delay_gamma(1.5, 4.0, 
                                                  t_max = 15, t_step = 0.25)$p,
                             dIs = cm_delay_gamma(3.5, 4.0, 
                                                  t_max = 15, t_step = 0.25)$p,
                             dIa = cm_delay_gamma(5.0, 4.0, 
                                                  t_max = 15, t_step = 0.25)$p,
                             deterministic = deterministic)
  
  n_age_groups <- length(para$pop[[1]]$size)
  
  for(i in 1:length(para$pop)){
    
    para$pop[[i]]$y <- cf
    para$pop[[i]]$u <- sus
    
    # scale u (susceptibility) to achieve desired R0
    current_R0 = cm_calc_R0(para, i); # calculate R0 in population i of params
    para$pop[[i]]$u = para$pop[[i]]$u * R0_assumed / current_R0
    
    # natural waning
    para$pop[[i]]$wn <- rep((1/waning_nat), n_age_groups)
    
    ## Set seeds to control start of outbreak
    # infections start in individuals aged 20-50
    para$pop[[i]]$dist_seed_ages = 
      cm_age_coefficients(20, 
                          80, 
                          5 * (0:length(para$pop[[i]]$size))) 
    
    # 1 new infections each day for 14 days to see the outbreak
    para$pop[[i]]$seed_times <- c(1:14)
  }
  
  para$processes = burden_processes
  
  para$schedule[["mobility"]] = list(
    parameter = "contact",
    pops = numeric(),
    mode = "assign",
    values = split(c_tmp[,4:7],
                   seq(nrow(c_tmp))) %>%
      map(unlist) %>%
      map(as.vector) %>%
      unname,
    times = 1:t_run)
  
  return(para)
}

update_vac_char <- function(para,
                            waning_vac = 52*7,
                            ve_i = NULL,
                            ve_d = NULL){

  n_age <- length(para$pop[[1]]$size)

  # currently closing two-dose setup
  para$pop[[1]]$ev2 <- rep(0, n_age)

  # key parameters on infection and disease blocking mechanisms
  para$pop[[1]]$ei_v <- rep(ve_i, n_age)
  para$pop[[1]]$ed_vi <- rep(ve_d, n_age)
  
  # waning vaccine-induced immunity
  para$pop[[1]]$wv = rep((1/waning_vac), n_age)
  
  # return results
  return(para)
}

vac_policy <- function(para,
                       milestone_date = c("2021-01-01", # start from 0
                                          "2021-06-01", # 0.03
                                          "2021-12-31", # all population; 0.2
                                          "2022-12-31"), # 0.6
                       milestone_cov = c(0,
                                         0.03,
                                         0.2,
                                         0.4),
                       # priority = c(NA, NA, NA, NA,
                       #              5,  5,  5,  5,
                       #              5,  5,  5,  5,
                       #              4,  3,  2,  1),
                       priority = c(NA, NA, NA, NA,
                                    2,  2,  2,  2,
                                    2,  2,  2,  2,
                                    1,  1,  1,  1),
                       # age-specific maximum vaccination uptake
                       cov_max = c(rep(0,2),
                                   rep(0.7, 10),
                                   rep(0.9, 4))
                       
                       
){
  date_start = para$date0

  # details of the vaccine roll-out schedules
  # beginning of each phase, and the daily number of doses available
  tmp_schedule <- data.frame(milestone_date = c(para$date0, 
                                                milestone_date, 
                                                para$time1),
                             milestone_cov = c(NA, 
                                               milestone_cov, 
                                               NA)) %>% 
    group_by(milestone_date) %>% 
    summarise(milestone_cov = mean(milestone_cov, 
                                   na.rm = T),
              .groups = "drop") %>% 
    ungroup %>% 
    mutate(milestone_date = lubridate::ymd(milestone_date),
           t = milestone_date - as.Date(para$date0),
           cov = c(NA, diff(zoo::na.locf(milestone_cov, 
                                         na.rm = F))),
           cov = if_else(cov %in% c(0, NA, NaN), 
                         as.numeric(NA), 
                         cov),
           doses = cov * sum(para$pop[[1]]$size),
           t_diff = c(NA, diff(t)) ,
           doses_daily = dplyr::lead(doses/t_diff, 1),
           milestone_marker = !is.na(milestone_cov))
  
  # details of the target population
  tmp_pop <- data.frame(n_pop = para$pop[[1]]$size) %>% 
    mutate(uptake = cov_max,
           n_tar = n_pop * uptake,
           ve = para$pop[[1]]$ev,
           wv = para$pop[[1]]$wv)
  
  # details on age-specific prioritization
  tmp_priorities <- priority %>% 
    enframe(name = "age_group") %>% 
    filter(!is.na(value)) %>% 
    arrange(value) %>% 
    group_by(value) %>% 
    group_split()
  
  # age group specific population size cap
  pop_cap <- tmp_priorities %>% 
    map(~.$age_group) %>% 
    map(~tmp_pop$n_tar[.]) 
  
  # population saturation marker
  pop_marker <- pop_cap %>% 
    map(sum) %>% 
    unlist() %>% 
    cumsum() 
  
  # create empty grid to score vaccination plans
  matrix(0, 
         ncol = length(para$pop[[1]]$size),
         nrow = max(as.numeric(tmp_schedule$t), na.rm = T)) %>% 
    as_tibble() %>% 
    setNames(paste0("Y",1:16)) %>% 
    rownames_to_column(var = "t") %>% 
    mutate(date = lubridate::ymd(date_start) + as.numeric(t)) %>% 
    left_join(    
      tmp_schedule %>% 
        mutate(doses_daily = if_else(!milestone_marker, 0, doses_daily)) %>% 
        dplyr::filter(!is.na(doses_daily)) %>% 
        dplyr::select(milestone_date, doses_daily) %>% 
        setNames(c("date", "supply")) %>% 
        mutate(date = if_else(date == date_start, date + 1, date)),
      by = "date") %>% 
    mutate(supply = imputeTS::na_locf(supply),
           supply = if_else(date < milestone_date[1], 0, supply),
           supply = if_else(date > ymd("2022-12-31"), 0, supply),
           supply_cum = cumsum(supply), phase = 0) -> daily_vac
  
  date_marker <- sapply(1:length(pop_marker), 
                        function(i) {min(which(daily_vac$supply_cum >= 
                                                 pop_marker[i]))})
  
  if(is.infinite(tail(date_marker, 1))) {exhaust = F} else {exhaust = T}
  if(all(is.infinite(date_marker)) & length(date_marker) == 2) {np = T} else {np = F}
  
  date_marker <- c(as.numeric(tmp_schedule$t[2]),
                   date_marker,
                   nrow(daily_vac)) %>% unique %>% sort %>% .[!is.infinite(.)] %>% 
    as.integer()
  
  for(i in 1:(length(date_marker)-1)) {
    if(i == 1) {
      daily_vac$phase[date_marker[i]] <- i
    }
    daily_vac$phase[(date_marker[i]+1):(date_marker[i+1])]<- i
  }
  rm(i)
  if(np) {end <- 1} else {end <- length(tmp_priorities)}
  
  for(i in 1:end){
    tmp_ag <- paste0("Y",tmp_priorities[[i]]$age_group)
    group_tot <- para$pop[[1]]$size[tmp_priorities[[i]]$age_group]
    group_prop_ag <- group_tot/sum(group_tot)
    
    if(length(tmp_priorities) > i){
      tmp_ag_next <-  paste0("Y",tmp_priorities[[i+1]]$age_group)
      group_tot_next <- para$pop[[1]]$size[tmp_priorities[[i+1]]$age_group]
      group_prop_next <- group_tot_next/sum(group_tot_next)
    }
    
    if(i == 1){
      daily_vac[((date_marker[i]):(date_marker[i+1]-1)),tmp_ag] <- 
        sapply(1:length(group_prop_ag), 
               function(x) group_prop_ag[x]*
                 daily_vac[((date_marker[i]):(date_marker[i+1]-1)),
                           "supply"]) %>% 
        bind_cols() %>% 
        setNames(tmp_ag)
    } 
    
    if(i > 1 & date_marker[i] < nrow(daily_vac)){
      daily_vac[((date_marker[i]+1):(date_marker[i+1]-1)),tmp_ag] <- 
        sapply(1:length(group_prop_ag), 
               function(x) group_prop_ag[x]*
                 daily_vac[((date_marker[i]+1):(date_marker[i+1]-1)),
                           "supply"]) %>% 
        bind_cols() %>% 
        setNames(tmp_ag)
    }
    
    if(date_marker[i] < nrow(daily_vac)){
      remainder <- sum(pop_cap[[i]]) - daily_vac[,tmp_ag] %>% 
        colSums(., na.rm = T) %>% sum
      supply <- daily_vac[date_marker[i+1],"supply"]
      daily_vac[date_marker[i+1], tmp_ag] <- as.list(remainder*group_prop_ag)
      
      if(length(tmp_priorities) > i){
        daily_vac[date_marker[i+1], tmp_ag_next] <- 
          as.list(group_prop_next*unlist((supply - remainder)))
        
      }
    }
  
    }
  
  # Highlight the exception
  # If the vaccine supply has yet to saturated the population, we will replace 
  # the last row with the row before, because the last row cannot mop up what's 
  # happening before hand
  
  if(!exhaust){
    daily_vac[nrow(daily_vac), paste0("Y",1:16)] <- 
      daily_vac[nrow(daily_vac)-1, paste0("Y",1:16)] 
  }
  
  # putting all vaccine policy related raw parameters back together
  daily_vac %>% 
    mutate(t = as.numeric(t)) %>% 
    group_by(phase) %>% group_split() %>% 
    map(group_by_at, vars(starts_with("Y"))) %>% 
    map(filter, t == min(t)) %>% 
    bind_rows() -> vac_para
  
  # then convert these parameters to a format that's friendly with `covidm`
  # allocation
  vac_para %>% 
    dplyr::select(starts_with("Y")) %>% 
    split(seq(nrow(vac_para))) %>% 
    map(unlist) %>% 
    map(as.vector) -> vacc_vals
  
  # timing
  vacc_times <- vac_para$t %>% as.numeric; vacc_times[1] <- 0
  
  para$schedule[["vaccination"]] = list(
    parameter = "v",
    pops = numeric(),
    mode = "assign",
    values = vacc_vals,
    times = vacc_times)
  
  daily_vac %<>% 
    rename(doses_daily = supply,
           supply = supply_cum) %>% 
    mutate(date = as.Date(date_start) + as.numeric(t)) %>% 
    dplyr::select(-phase)
  
  return(list(param = para, 
              supply = tmp_schedule,
              vac_para = vac_para,
              daily_vac = daily_vac))
}


predict_outbreak <- function(
  cn, # country name
  cov_tar = c(rep(0,2),
              rep(0.7, 10),
              rep(0.9, 4)), # target coverage
  ms_date = c("2021-01-01", # start from 0
              "2021-06-01", # 0.03
              "2021-12-31", # all population; 0.2
              "2022-12-31"), # milestone - dates
  ms_cov = c(0,
             0.03,
             0.2,
             0.5), # milestones - coverage levels
  # priority = NULL, # priority settings
  date_start = NULL, # starting date of uncontrollable community transmission
  date_end = "2022-12-31",
  ve_i = NULL, # VE against infections
  ve_d = NULL, # VE against disease
  reporting = 1,
  mobility = schedule_raw,
  prp = priority_policy,
  # eff = NULL,#c(rep(0.9,10),rep(0.8,6)),
  wane = c(52*3, 52), # natural waning; vaccine waning
  R = NULL) {
  
  wb <- countrycode::countrycode(cn, "country.name", "wb")
  
  gen_country_basics(
    country = cn,
    waning_nat = wane[1] * 7,
    R0_assumed = R,
    date_start = date_start,
    date_end = date_end,
    deterministic = TRUE,
    mobility = mobility
  ) %>%
    update_vac_char(
      para = .,
      waning_vac = wane[2] * 7,
      ve_i = ve_i,
      ve_d = ve_d
    ) -> params_baseline
  
  params_baseline$processes[[2]]$prob[1,] <- 
    params_baseline$processes[[2]]$prob[1,] * reporting
  
  prp %>%
    map(~ vac_policy(
      para = params_baseline,
      milestone_date = ms_date,
      milestone_cov = ms_cov,
      priority = .,
      cov_max = cov_tar
    )) -> params
  
  # identify the starting time of vaccination programs
  lapply(seq_along(params), function(x) params[[x]]$param$schedule$vaccination$times) %>%
    map(~ .[. != 0]) %>%
    map(min) %>%
    unlist() %>%
    unique() %>%
    "+"(lubridate::ymd(params[[1]]$param$date0)) %>%
    enframe(value = "date_start_vac", name = "id") %>%
    mutate(
      m1 = date_start_vac %m+% months(6),
      m2 = date_start_vac %m+% months(12),
      m3 = date_start_vac %m+% months(18)
    ) -> date_start_vac
  
  # res <- dyna <- daily_vac <- vac_para <- list()
  res <- lapply(seq_along(prp), function(x) cm_simulate(params[[x]]$param))
  res_baseline <- cm_simulate(params_baseline)
  dyna <- lapply(res, "[[", "dynamics")
  #  vac_para <- lapply(params, "[[", "vac_para")
  daily_vac <- lapply(params, "[[", "daily_vac") %>%
    bind_rows(.id = "policy") %>% 
    mutate(policy = factor(policy,
                           levels = names(prp),
                           labels = 1:4))
  
  # Aggregate health economics output
  dyna %>%
    bind_rows(.id = "policy") %>%
    bind_rows(res_baseline$dynamics %>% mutate(policy = "0")) %>%
    filter(compartment %in% c("death_o", "cases")) %>%
    dcast(., policy + run + population + group + t ~ compartment) %>%
    mutate(
      wb = countrycode::countrycode(population, "country.name", "wb"),
      baseline = if_else(policy == "0", "baseline", "current"),
      date = lubridate::ymd(params[[1]]$param$date0) + as.numeric(t)
    ) %>%
    left_join(LE_estimates, by = c("wb", "group")) %>%
    left_join(df_VSL, by = "wb") %>%
    left_join(GDPpc, by = "wb") %>% 
    mutate_at(vars(c("LE", "adjLE", "LEdisc", "adjQALEdisc", "VSL_mlns")), 
              ~ . * death_o) %>%
    rename(VSLmlns = VSL_mlns) %>%
    mutate(QALYcases = cases * 0.0307,
           HC = LEdisc * GDPpc) %>%
    # filter(date >= date_start_vac$date_start_vac) %>% 
    filter(date >= "2021-01-01") %>% 
    group_by(policy, run, population, wb, t, date) %>% 
    summarise_at(vars(c("cases", "death_o",
                        "LE", "adjLE", "QALE", "adjQALE",
                        "QALEdisc", "adjQALEdisc", "VSLmlns","QALYcases",
                        "HC")),
                 sum) %>%
    left_join(daily_vac %>% 
                dplyr::select(policy, t, doses_daily) %>% 
                mutate(t = as.numeric(t)),
              by = c("policy", "t")) %>% 
    mutate(AEFI_loss = doses_daily * 0.5 * (1/365.25),
           AEFI_loss = if_else(is.na(AEFI_loss), 0, AEFI_loss),
           QALYloss = adjQALEdisc + AEFI_loss + QALYcases) -> econ_full
  
  date_start_vac[, paste0("m",1:3)] %>%
    mutate_all(as.character) %>%
    unlist() %>%
    as.vector() %>%
    c(., date_end) %>%
    sort() %>%
    data.frame(UL = .) %>% 
    mutate(LL = min(date_start_vac$date_start_vac)) -> date_limits
  
  econ_base <- list()
  for(i in 1:nrow(date_limits)){
    econ_full %>% 
      filter(date <= date_limits$UL[i],
             date >= date_limits$LL[i]) -> econ_base[[i]]
  }
  
  econ_base %>%
    bind_rows(.id = "w") %>%
    mutate(w = factor(w,
                      levels = 1:4,
                      labels = c("6m","12m","18m","2022")
    )) %>% data.table %>% 
    .[, lapply(.SD, sum, na.rm = T),
      by = c("policy", "run", "population", "w"),
      .SDcols = c(
        "cases", "death_o",
        "LE", "adjLE", "adjQALE", "adjQALEdisc",
        "VSLmlns", "QALYcases", "doses_daily",
        "AEFI_loss", "QALYloss", "HC"
      )
    ] -> econ_w
  
  econ_w %>%
    group_by(w) %>%
    rename(doses = doses_daily) %>% 
    group_split() %>%
    map(mutate,
        LE_pd = (max(LE) - LE) / doses,
        adjLE_pd = (max(adjLE) - adjLE) / doses,
        adjQALE_pd = (max(adjQALE) - adjQALE) / doses,
        adjQALEdisc_pd = (max(adjQALEdisc) - adjQALEdisc) / doses,
        VSLmlns_pd = (max(VSLmlns) - VSLmlns) / doses,
        QALYloss_pd = (max(QALYloss) - QALYloss) / doses,
        HC_pd = (max(HC) - HC)/doses
    ) %>%
    bind_rows() %>%
    reshape2::melt(., id.var = c("policy", "run", "population", "w")) %>%
    mutate(value = if_else(is.infinite(value), as.numeric(NA), value)) %>%
    data.table() -> econ
  
  size <- params[[1]]$param$pop[[1]]$size
  
  data.table(res_baseline$dynamics) %>%
    mutate(policy = "0") %>%
    bind_rows(bind_rows(dyna, .id = "policy")) %>%
    group_by(compartment, t, policy, population, run) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    pivot_wider(
      names_from = compartment,
      values_from = value
    ) %>%
    mutate(date = lubridate::ymd(date_start) + t,
           date = as.character(date)) %>% ungroup %>% 
    dplyr::select(-t, -run) -> seg1
  
  daily_vac %>% 
    dplyr::select(-t) %>% 
    mutate(date = as.character(date)) -> seg2
  
  
  left_join(seg1, seg2,
              by = c(
                "policy",
                "date"
              )
    ) -> main; rm(seg1, seg2)
  
  r <- list(
    main = main,
    supply = params[[1]]$supply,
    econ = econ,
    econ_w = econ_w,
    date_start = date_start,
    date_start_vac = date_start_vac,
    res_baseline = res_baseline,
    size = size,
    vac_para = lapply(1:4, function(x) params[[x]]$vac_para) %>% 
      bind_rows(.id = "ROS") %>% 
      mutate(ROS = paste0("R",ROS))
  )
  
  return(r)
}

#### calculate disease/ infection blocking mechanisms #### 
exp_ve <- function(ve, ei_v#, y
                   ){
  ed_vi <- (ve - ei_v)/(1 - ei_v)
  return(ed_vi)
  
}

#### generate results based on model selected ####



