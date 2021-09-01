# functions used for plotting

ag_names <- paste(seq(0,75,5),seq(4,76,5),sep = "-")
ag_names[length(ag_names)] <- "75+"

scientific_10 <- function(x){
  scales::scientific_format()(x) %>%
    sub(pattern = "e\\+",
        x = .,
        replacement = "e") %>%
    sub(pattern = "e00", 
        x = ., 
        replacement = "") %>%
    gsub(pattern = "e",
         replacement = " %*% 10^",
         x = .) %>%f
    sub(pattern = "1 %*% ",
        replacement = "",
        x = ., fixed = T) %>%
    parse(text = .)
}

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      # plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      # panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      # legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

policy_labels <- c("V+", "V20", "V60", "V75")
rollout_labels <- paste0("R",1:4)
priority_colors <- c("#DAF0BD", "#9DCC5F", "#6EC3C1", "#0D5F8A")
# priority_colors <- c("#E69F00", "#D55E00", "#56B4E9", "#0072B2")

metric_labels <- c("Deaths", "Cases", 
                   "Adj. Life Expectancy\n(cLY) Loss",
                   "Quality Adj. Life Years\n(cQALY) Loss",
                   "Human Capital\n(HC) Loss")


plot_fit <- function(cn_tmp, n, t_intro, R0){
  # debug
  # cn_tmp = "North Macedonia"
  # n = 10
  # t_intro <- 43
  # R0 <- 2.2
  # 
  wb_tmp <- countrycode(cn_tmp, "country.name", "wb")
  
  
  gen_country_basics(country = cn_tmp,
                     R0_assumed = R0,
                     date_start = as.character(as.Date("2019-12-01") +
                                                 t_intro),
                     date_end = "2020-12-31") -> params
  
  cm_simulate(params) -> test_baseline
  
  params$deterministic <- FALSE
  
  cm_simulate(params, n, 123, n_threads = 4) -> test
  
  test$dynamics %>% 
    .[compartment == "death_o"] %>% 
    .[,list("deaths_tot" = sum(value)),by = list(t, run)] %>%
    left_join(test_baseline$dynamics %>% 
                .[compartment == "death_o"] %>% 
                .[,list("deaths_baseline" = sum(value)), by = t],
              by = "t") %>% 
    mutate(date = ymd("2019-12-01") + t_intro + t) %>% 
    left_join(epi[loc == wb_tmp], by = "date") %>%
    # filter(date >= "2020-12-15")
    ggplot(.) +
    geom_line(aes(x = date, y = deaths_tot, group = run), alpha = 0.1) +
    geom_point(aes(x = date, y = deaths), color = "red") +
    geom_point(aes(x = date, y = deaths_baseline), color = "black") +
    theme_bw() +
    labs(x = "Date", y = "Total Deaths", title = cn_tmp,
         caption = "Black dots - deterministic realisation of fitted epidemic; 
         Black line - stochastic realisation of fitted epidemics (n = 100); 
         Red - observation.") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          title = element_text(size = 14),
          plot.caption = element_text(size = 12)) -> p
  
  return(p)
}


##### Figure 3A-C: Georgia, Hungary, and UK ####
plot_fit_examples <- function(file){
  
  n_param <- parse_number(file)
  
  eg_fit <- read_rds(file) %>% 
    dplyr::filter(date < "2021-01-01") %>% 
    mutate(population = factor(population,
                               levels = c("Georgia", "Hungary",
                                          "United Kingdom of Great Britain"),
                               labels = c("Georgia", "Hungary",
                                          "United Kingdom")),
           metric = factor(metric,
                           levels = c("stochastic",
                                      "deterministic",
                                      "empirical"))) 
  
  if(n_param == 3) {
    eg_fit %<>% 
      mutate(metric = as.character(metric)) %>% 
      left_join(model_selected_3 %>% rename(date_intro = date), by = "wb") %>% 
      group_by(metric) %>% group_split()
    
    eg_fit[[4]] <- eg_fit[[1]] %>% 
      mutate(metric = "deterministic_raw",
             death_o = death_o/par3)
    eg_fit %<>% bind_rows()
  }
  
  eg_fit %>% 
    # filter(run <= 10) %>% 
    ggplot(., aes(x = as.Date(date), 
                  y = death_o, 
                  color = metric,
                  group = interaction(metric, run))) +
    geom_line(data = subset(eg_fit, metric == "stochastic"),
              alpha = 0.05, size = 1.1) +
    geom_point(data = subset(eg_fit, metric == "empirical"), size = 3) +
    geom_line(data = subset(eg_fit, metric == "deterministic"), size = 1.5) +
    facet_wrap(~population, scale = "free_y", ncol = 1) +
    geom_text(data = data.frame(date = ymd("2020-01-15"),
                                y = Inf,
                                population = c("Georgia", "Hungary",
                                               "United Kingdom"),
                                label = c("(A)","(B)","(C)"),
                                metric = NA,
                                run = NA),
              aes(x = date, 
                  y = y,
                  label = label), 
              vjust= 2, hjust= 0, size = 8,
              color = "black") +
    lims(x = c(ymd("2020-01-15", "2020-12-31"))) +
    # geom_vline(xintercept = ymd("2020-12-31"), linetype = 2) +
    labs(color = "", x = "", y = "Daily reported COVID-19 deaths") +
    theme_cowplot() +
    scale_x_date(labels = scales::date_format("%b\n%Y")) +
    theme(legend.position = "top",
          strip.text = element_text(size = 20),
          # strip.background = element_rect(colour = "black"),
          legend.text = element_text(size = 18),
          axis.text = element_text(size = 18),
          axis.title = element_text(size = 17),
          panel.border = element_rect(colour = "black", fill=NA),
          # axis.text.x = element_text(hjust = -0.2),
          plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    theme(strip.background = element_rect(fill = NA)) +
    guides(color = guide_legend(nrow = 2)) -> p1
  
  
  if(n_param == 2){
    p1 <- p1 +   scale_color_manual(values = c("black", "grey70", 
                                               # viridis(option = "inferno", 10)[5]),
                                              RColorBrewer::brewer.pal(9,"YlGnBu")[5]),
                                    breaks = c("deterministic", "stochastic", "empirical"),
                                    labels = c("Prediction:\nDeterministic",
                                               "Prediction:\nStochastic",
                                               "Observation")) 
  }
  
  if(n_param == 3){
    p1 <- p1 +   
      geom_line(data = subset(eg_fit, metric == "deterministic_raw"), size = 1.5) +
      scale_color_manual(values = c("black", "grey70", "grey40",
                                    # viridis(option = "inferno", 10)[5]),
                                    RColorBrewer::brewer.pal(9,"YlGnBu")[5]),
                                    breaks = c("deterministic", 
                                               "stochastic",
                                               "deterministic_raw",
                                               "empirical"),
                                    labels = c("Prediction:\nDeterministic",
                                               "Prediction:\nStochastic",
                                               "Prediction: \nDeterministic w/o\nUnderreporting",
                                               "Observation")) 
  }
  
  return(p1)
}

##### Figure 3D: plotting proportion non S ####
plot_non_S <- function(file){
  sero <- read_rds(file)
  
  n_param <- parse_number(file)
  if(n_param == 2) {
    selected <- model_selected_2
  } else {
    selected <- model_selected_3
  }

  sero %>%
    mutate(p = if_else(wb %in% members_remove, as.numeric(NA), p)) %>%
    left_join(selected, by = "wb") %>%
    full_join(members_world, by = "wb") %>% ungroup %>% 
    mutate(p_np = ntile(p, 9) %>% factor) %>%
    mutate(Fit = if_else(wb %in% members_remove, F, T),
           EUR = if_else(wb %in% members$wb, T, F)) %>%
    st_as_sf() -> tmp
  
  # inline statistics
  # tmp %>% 
  #   filter(!wb %in% members_remove) %>%
  #   filter(p_np == 5)
  
  tmp %>% 
    filter(!is.na(p_np)) %>% 
    group_by(p_np) %>% 
    group_split() %>% 
    map(pull, p) %>% 
    map(~.*100) %>% 
    map(round) %>% 
    map(range) %>% 
    map(~paste0(., collapse = "-")) %>% 
    map(~paste0(., "%")) %>% unlist -> bar_labels
  
  ggplot(tmp, aes(fill = p_np)) +
    geom_sf(data = tmp %>% filter(Fit == T)) +
    geom_sf_pattern(data = tmp %>% filter(Fit == F), 
                    pattern_spacing = 0.03,
                    pattern_density = 0.2,
                    color = "black",
                    fill = "grey90",
                    alpha = 0.5,
                    pattern_fill = "grey70",
                    pattern = "crosshatch") +
    geom_sf(data = tmp %>% filter(EUR == F), 
            fill = "grey50", 
            color = "black") +
    coord_sf(xlim = c(-25, 90),
             ylim = c(30, 75),
             expand = F) +
    # viridis::scale_fill_viridis(na.value = "grey90", discrete = T, 
    #                             option = "inferno",
    #                             na.translate = F, direction = -1,
    #                             labels = c(bar_labels[1], rep("",3), 
    #                                        bar_labels[5], rep("",4),
    #                                        bar_labels[10])) +
    
    scale_fill_brewer(palette = "YlGnBu",
                       na.value = "grey90", #discrete = T,
                       na.translate = F, direction = 1,
                       labels = c(bar_labels[1], rep("",3),
                                  bar_labels[5], rep("",3),
                                  bar_labels[9])) +
    theme_map() +
    labs(fill = "Proportion of population no longer\nsusceptible to SARS-CoV-2",
         title = "") +
    geom_text(data = data.frame(x = -Inf,
                                y = Inf,
                                label = c("(D)"),
                                p_np = NA),
              aes(x = x, 
                  y = y,
                  label = label), 
              vjust= 2, hjust= -0.5, size = 8,
              color = "black") +
    theme(strip.text = element_text(size = 14),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          axis.text = element_blank(),
          legend.position = "top",
          axis.ticks = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          plot.title = element_text(size = 24),
          axis.title = element_text(size = 20)) +
    guides(fill = guide_legend(nrow = 1,
                               label.position = "top",
                               override.aes = list(pattern = NA,
                                                   color = NA))) -> p4
  return(p4)
}

##### Figure 3 supplemental #####
plot_fitted_res <- function(m, var = 1){
  n_param <- colnames(m) %in% c("t", "r", "rho", "par3") %>% sum 
  
  m %>% 
    mutate(t = if_else(wb %in% members_remove, as.numeric(NA), t),
           r = if_else(wb %in% members_remove, as.numeric(NA), r),
           date = if_else(wb %in% members_remove, 
                          as.character(NA), 
                          as.character(date)),
           fit = if_else(wb %in% members_remove, F, T)) %>% 
    mutate(t_np = ntile(t, 10) %>% factor(levels = c(1:10)),
           r_np = ntile(r, 10) %>% factor(levels = c(1:10))) %>% 
    full_join(members_world, by = "wb") %>%
    dplyr::select(-NAME_ENGL, -CNTR_NAME, -CNTR_ID, 
                  -FID, -ISO3_CODE) %>%
    st_as_sf() %>% 
    sfheaders::sf_remove_holes() -> tmp
  
  if(n_param > 2){
    tmp %<>% 
      mutate(rho = if_else(wb %in% members_remove, as.numeric(NA), par3) %>% 
               round(., 2),
             rho_np = ntile(rho, 10) %>% factor(levels =c(1:10))) %>% 
      dplyr::select(-par3)
  }
  
  
  if(var == 1){
    tmp %>% 
      filter(!is.na(t_np)) %>% 
      group_by(t_np) %>% 
      group_split() %>% 
      map(pull, date) %>% 
      # map(~.*100) %>% 
      # map(round) %>% 
      map(range) %>% 
      map(~paste0(., collapse = "~\n")) %>% 
      #map(~paste0(., "%")) %>% 
      unlist -> bar_labels
  }
  if(var == 2){
    tmp %>% 
      filter(!is.na(r_np)) %>% 
      group_by(r_np) %>% 
      group_split() %>% 
      map(pull, r) %>% 
      # map(~.*100) %>% 
      map(round, 2) %>% 
      map(range) %>% 
      map(~paste0(., collapse = "-")) %>% 
      #map(~paste0(., "%")) %>% 
      unlist -> bar_labels
  }
  if(var == 3){
    tmp %>% 
      filter(!is.na(rho_np)) %>% 
      group_by(rho_np) %>% 
      group_split() %>% 
      map(pull, rho) %>% 
      map(~.*100) %>% 
      map(round) %>% 
      map(range) %>% 
      map(~paste0(., collapse = "-")) %>% 
      map(~paste0(., "%")) %>% 
      unlist -> bar_labels
  }
  
  if(var == 1){
    p <-   tmp %>% 
      ggplot(aes(fill = t_np)) +
      geom_sf(data = tmp %>% filter(fit == T))
  }
  if(var == 2){
    p <-   tmp %>% 
      ggplot(aes(fill = r_np)) +
      geom_sf(data = tmp %>% filter(fit == T))
  }
  if(var == 3){
    p <-   tmp %>% 
      ggplot(aes(fill = rho)) +
      geom_sf(data = tmp %>% filter(fit == T))
  }
  
  if(var %in% c(1,2)){
    p <- p +
      geom_sf_pattern(data = tmp %>% filter(fit == F), 
                      pattern_spacing = 0.03,
                      pattern_density = 0.2,
                      color = "black",
                      fill = "grey90",
                      alpha = 0.5,
                      pattern_fill = "grey70",
                      pattern = "crosshatch") +
      geom_sf(data = tmp %>% filter(is.na(fit)), 
              fill = "grey50", 
              color = "black") +
      scale_fill_brewer(palette = "YlGnBu", 
                        na.translate = F,
                         labels = c(bar_labels[1],
                                    rep("",8),
                                    bar_labels[10])) +
      coord_sf(xlim = c(-25, 90),
               ylim = c(30, 75),
               expand = F) +
      theme_map() +
      theme(legend.position = "bottom",
            strip.text = element_text(size = 14),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 14),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            plot.title = element_text(size = 24),
            axis.title = element_text(size = 20)) +
    guides(fill = guide_legend(nrow = 1,
                               label.position = "bottom",
                               override.aes = list(color = NA,
                                                   pattern = NA))) +
    labs(fill = "")
  }
  if(var %in% c(3)){
    p <- p +
      geom_sf_pattern(data = tmp %>% filter(fit == F), 
                      pattern_spacing = 0.03,
                      pattern_density = 0.2,
                      color = "black",
                      fill = "grey90",
                      alpha = 0.5,
                      pattern_fill = "grey70",
                      pattern = "crosshatch") +
      geom_sf(data = tmp %>% filter(is.na(fit)), 
              fill = "grey50", 
              color = "black") +
      scale_fill_viridis(breaks = c(0.25, 1)) +
      coord_sf(xlim = c(-25, 90),
               ylim = c(30, 75),
               expand = F) +
      theme_map() +
      theme(legend.position = "bottom",
            strip.text = element_text(size = 14),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 18),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            plot.title = element_text(size = 24),
            axis.title = element_text(size = 20)) +
      labs(fill = "")
  }
  return(p)
}

#### sub plots in figure 4 ####
# file should be the directory to "priority_selecion_2.rds"
plot_diff <- function(file){
  
  res <- read_rds(file)
  index <- read_rds("data/intermediate/index.rds")
  index <- index %>% 
    mutate(name = factor(name,
                         levels = as.character(unique(index$name)),
                         labels = metric_labels))
  
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
                         labels = metric_labels),
           ROS = factor(ROS, levels = paste0("R",c(1,2,3,4)),
                        labels = rollout_labels)) %>% 
    group_by(ROS, name) %>% group_split() %>% 
    map(mutate, best_perform = min(value)) %>% 
    bind_rows() %>% 
    mutate(p_increase = (value/best_perform - 1) %>% 
             round(., 2),
           p_increase = p_increase*100 %>% round) -> tab_combined
    
  set3 <- list()
  for(i in 1:nrow(index)){
    tab_combined %>% 
      filter(ROS == index$ROS[i],
             name == as.character(index$name[i])) %>%
      mutate(y_LL = p_increase %>% min,
             y_UL = p_increase %>% max,
             padding = 0.3,
             cen_LL = (y_LL + (y_UL-y_LL)/4) %>% round(., 2),
             cen_UL = (y_LL + (y_UL-y_LL)*3/4) %>% round(., 2),
             y_LL = y_LL*(1-padding),
             y_UL = y_UL*(1+padding),
             x_LL = 0.5,
             x_UL = 5.5,
             policy = factor(policy,
                             levels = c("tailored", paste0(1:4)),
                             labels = c("Local Optimal",
                                        policy_labels
                                        ))) -> p_table
      
    ggplot(p_table, aes(x = rk, y = p_increase, #log(abs(value),10), 
                        # fill = policy,
                        fill = policy,
                    group = name)) +
      geom_point(size = 4 ,
                 pch = 21) +
      scale_fill_manual(values = c("black",priority_colors)) +
      scale_color_manual(values = c("black", priority_colors)) +
      scale_x_continuous(breaks = c(1:6), limits = c(0.5, 5.5)) +
      scale_y_continuous(breaks = c(0, 
                                    if_else((round(p_table$y_UL %>% unique/5) - 1) < 1,
                                            1*5,
                                            (round(p_table$y_UL %>% unique/5) - 1)*5)),
                         labels = c("0%", 
                                    paste0(if_else((round(p_table$y_UL %>% unique/5) - 1) < 1,
                                            1, 
                                            (round(p_table$y_UL %>% unique/5) - 1))*5,
                                           "%")
                                    ),
                         limits = c(unique(p_table$y_LL),
                                    if_else(unique(p_table$y_UL) < 10, 
                                            10,
                                            unique(p_table$y_UL)))) +
      #  geom_bar(stat = "identity") +

      # geom_line(size = 2) +
      # ggh4x::facet_nested_wrap(ROS~name,scale = "free") +
      labs(#x = "Ranking", 
        x = "", y = "",
        # y = "Values (log-scaled)", 
        fill = "", color = "") +
      theme_cowplot() +
      theme(strip.background = element_rect(fill = NA),
            legend.position = "none",
            # legend.text = element_text(size = 16),
            # axis.text.y = element_text(size = 16),
            # aspect.ratio = 1,
            plot.background = element_rect(fill = "white", color = "black"),
            plot.margin = unit(c(0, 0, -0.5, -0.5), "cm"),
            strip.text = element_text(size = 18)) -> set3[[i]]
  }
  
  
  
  # coord_sf(xlim = c(-25, 90),
  #          ylim = c(30, 75),
  #          expand = F) +
  # 
  
  # set1[[1]] + annotation_custom(ggplotGrob(set3[[1]]),
  #                               xmin = 50, xmax = 85,
  #                               ymin = 57, ymax = 72)
  
  return(set3)
}

#### Figure 4: decision map ####
plot_decisions <- function(file){
  
  res <- read_rds(file)

  res[[3]] %>% 
    bind_rows(.id = "ROS") %>% 
    mutate(ROS = paste0("R",ROS),
           wb = countrycode(population, "country.name", "wb"),
           policy = if_else(wb %in% members_remove, 
                            as.character(NA), policy)) %>% 
    dplyr::filter(
      # variable %in% c("cases", "death_o",
      #                 "adjLE_pd", 
      #                 # "VSLmlns_pd",
      #                 "QALYloss_pd",
      #                 "HC_pd"),
    w == "2022",
    policy != 0
    ) %>% 
    dplyr::select(ROS, policy, population, w, cases, death_o,
                  adjLE, QALYloss, HC) %>% 
    pivot_longer(cols = c(cases, death_o, adjLE, QALYloss, HC)) %>% 
    # mutate(dir = if_else(variable %in% c("cases", "death_o"),
    #                      "min",
    #                      "max"),
    #        # we flip the sign for dir == "min" because we are looking to maximize
    #        # all the _pd HE measures, but minimise all the cases and death counts
    #        value = if_else(dir == "min", -1*value, value),
    #        value = if_else(wb %in% members_remove, as.numeric(NA), value)) %>% 
    group_by(population, name, ROS) %>% group_split() %>% 
    map(mutate, rk = rank(value)) %>% 
    map(arrange, desc(value)) %>% bind_rows() %>% 
    filter(rk == 1) %>% mutate(wb = countrycode(population, "country.name", "wb")) %>% 
    group_by(ROS, name) %>% group_split() %>% 
    map(full_join, members_world, by = "wb") %>% 
    map(st_as_sf) %>% 
    map(arrange, ROS) %>% 
    map(mutate, ROS = zoo::na.locf(ROS)) %>%
    map(mutate, name = zoo::na.locf(name)) %>% 
    bind_rows() %>% 
    mutate(name = factor(name,
                         levels = c("death_o", "cases", 
                                    "adjLE",
                                    # "VSLmlns_pd",
                                    "QALYloss",
                                    "HC"),
                         labels = metric_labels),
           ROS = factor(ROS,
                        levels = rollout_labels),
           policy = factor(policy)) %>% 
    st_as_sf() -> tab1
  
  tab1 %>% 
    data.table %>% 
    filter(!is.na(policy),
           !wb %in% members_remove) %>% 
    dplyr::select(ROS, wb, population, name, policy) %>% 
    left_join(cm_populations %>% 
                filter(name %in% members$country_name) %>% 
                # pull(name) %>% unique %>% length
                group_by(name) %>% 
                summarise(pop = sum(f + m)) %>% 
                mutate(wb = countrycode(name, "country.name", "wb")),
              by = c("wb", "population" = "name")) %>% 
    group_by(ROS, policy, name) %>% 
    summarise(pop = sum(pop)) -> tab2
  
  index <- read_rds("data/intermediate/index.rds")
  index <- index %>% 
    mutate(name = factor(name,
                         levels = as.character(unique(index$name)),
                         labels = metric_labels))
  

  # index <- tab2 %>% group_by(name, ROS) %>% tally()
  set3 <- plot_diff(file)
  set1 <- list()
  no_outbreak <- as.logical(0 %in% tab1$policy %>% unique %>% as.character())
  
  for(i in 1:nrow(index)){
  
    
    if(no_outbreak == T){
      colors_tmp <- c("black", priority_colors,"grey")
      labels_tmp <- c("No Vac", "V+", "V20", "V60", "V75", 
                      "Outside of WHO/Europe")
      breaks_tmp <- c(0:4, NA)
    }
    
    if(no_outbreak == F){
      colors_tmp <- c(priority_colors, "grey")
      labels_tmp <- c("V+", "V20", "V60", "V75", 
                      "Outside of WHO/Europe")
      breaks_tmp <- c(1:4, NA)
    }

    tab1 %>% 
      filter(name == index$name[i],
             ROS == index$ROS[i],
             !wb %in% members_remove) %>% 
      ggplot(., aes(fill = policy)) +
      geom_sf(color = "black") +
      scale_fill_manual(values = colors_tmp, 
                        na.value = "grey",
                        breaks = breaks_tmp,
                        labels = labels_tmp) +
      geom_sf_pattern(data =  tab1 %>% 
                        filter(name == index$name[i],
                               ROS == index$ROS[i],
                               wb %in% members_remove),
                      pattern_spacing = 0.03,
                      pattern_density = 0.2,
                      # aes(color = policy),
                      # fill = "grey90",
                      color = "black",
                      fill = "white",
                      # alpha = 0.5,
                      pattern_fill = "grey70",
                      pattern = "crosshatch",
                      show.legend = F) +
      # coord_sf(xlim = c(-20, 87),
      #          ylim = c(30, 73),
      #          expand = F) +
      coord_sf(xlim = c(-25, 90),
               ylim = c(30, 75),
               expand = F) +
      theme_nothing() +
      labs(fill = "Prioritisation Strategies: ") +
      theme(legend.position = "none",
            plot.margin = unit(c(0, 0, 0, 0), "cm")) -> set1[[i]]
    
    if(i == 5 &
       file %in% c("data/intermediate/priority_selection_2_debug.rds",
                   "data/intermediate/priority_selection_2_w_debug.rds")){
      set1[[i]] <- set1[[i]] + scale_fill_manual(breaks = 2, values = colors_tmp[2])
    }
    
    if(i == 6 &
       file %in% c("data/intermediate/priority_selection_2_w_debug.rds")){
      set1[[i]] <- set1[[i]] + scale_fill_manual(breaks = 2, values = colors_tmp[2])
    }
    

    set1[[i]] + annotation_custom(ggplotGrob(set3[[i]]),
                                  xmin = 45, xmax = 90,
                                  ymin = 60, ymax = 75) -> set1[[i]]
  
    
  }
  
  set2 <- list()
  for(i in 1:nrow(index)){
    tab2 %>% 
      filter(name == index$name[i],
             ROS == index$ROS[i]) %>% 
      ggplot(., aes(x = ROS, y = pop, color = policy, fill = policy)) +
      geom_bar(stat = "identity") +
      scale_color_manual(breaks = c(1:4, NA),
                         values = priority_colors,
                         na.value = "grey",
                         labels = c(paste0("P",1:4), "Outside of WHO/Europe")) +
      scale_fill_manual(breaks = c(1:4, NA),
                        values = priority_colors,
                        na.value = "grey",
                        labels = c(paste0("P",1:4), "Outside of WHO/Europe")) +
      theme_nothing() +
      theme(legend.position = "none",
            plot.margin = unit(c(0, 0, 0, 0), "cm")) -> set2[[i]]
    
  }
  
  # column labels
  top_titles <- list()
  top_label <- metric_labels
  
  for(i in 1:6){
    # if((i%%2) != 0) {
    if(i <= 5){
      top_titles[[i]] <- ggdraw() + 
        draw_label(
          # top_label[(i+1)/2],
          top_label[i],
          fontface = 'bold',
          x = 0.5,
          size = 20
        ) +
        theme(
          plot.margin = margin(0, 0, 0, 7)
        )
    }
    # if((i%%2) == 0){
    if(i == 6){
      top_titles[[i]] <- ggdraw() +
        draw_label(
          "",
          fontface = 'bold',
          x = 0,
          hjust = 1,
        ) +
        theme(
          plot.margin = margin(0, 0, 0, 0)
        )
    }
  }
  
  # row labels
  side_titles <- list()
  side_labels <- rollout_labels
  
  for(i in 1:4){
    side_titles[[i]] <- ggdraw() + 
      draw_label(
        side_labels[i],
        fontface = 'bold',
        angle = 90,
        y = 0.5,
        size = 20
      ) 
    theme(
      plot.margin = margin(0, 0, 0, 0)
    )
  }
  
  set1[[3]] +
    scale_fill_manual(values = colors_tmp, 
                      na.value = "white",
                      breaks = breaks_tmp,
                      labels = c(labels_tmp[1:4],"")) +
    geom_sf(color = "white") -> eg_legend
  
  legend_all <- get_legend(eg_legend +
                             theme(legend.position = "top",
                                   legend.title = element_text(size = 20),
                                   legend.text = element_text(size = 20)))

  
  # put all elements together
  plot_grid(top_titles[[6]], top_titles[[1]], top_titles[[2]], top_titles[[3]], top_titles[[4]],
            top_titles[[5]], # top_titles[[6]], top_titles[[7]], top_titles[[8]],
            # top_titles[[9]], top_titles[[10]],
            # top_titles[[2]], 
            
            # first row 
            side_titles[[1]],
            plot_grid(set1[[1]]) + draw_figure_label("(A)", size = 14), # set2[[1]], 
            plot_grid(set1[[5]]) + draw_figure_label("(B)", size = 14), # set2[[5]],
            plot_grid(set1[[9]]) + draw_figure_label("(C)", size = 14), # set2[[9]], 
            plot_grid(set1[[13]]) + draw_figure_label("(D)", size = 14), # (set2[[13]]), 
            plot_grid(set1[[17]]) + draw_figure_label("(E)", size = 14), # (set2[[17]]), 

            
            # second row 
            side_titles[[2]],
            plot_grid(set1[[2]]) + draw_figure_label("(F)", size = 14), # set2[[2]], 
            plot_grid(set1[[6]]) + draw_figure_label("(G)", size = 14), # set2[[6]],
            plot_grid(set1[[10]]) + draw_figure_label("(H)", size = 14), # set2[[10]], 
            plot_grid(set1[[14]]) + draw_figure_label("(I)", size = 14), # set2[[14]], 
            plot_grid(set1[[18]]) + draw_figure_label("(J)", size = 14), # set2[[18]], 

            
            # third row
            side_titles[[3]],
            plot_grid(set1[[3]]) + draw_figure_label("(K)", size = 14), # set2[[3]], 
            plot_grid(set1[[7]]) + draw_figure_label("(L)", size = 14), # set2[[7]],
            plot_grid(set1[[11]]) + draw_figure_label("(M)", size = 14), # set2[[11]],
            plot_grid(set1[[15]]) + draw_figure_label("(N)", size = 14), # set2[[15]], 
            plot_grid(set1[[19]]) + draw_figure_label("(O)", size = 14), # set2[[19]], 
  
            
            # fourth row
            side_titles[[4]],
            plot_grid(set1[[4]]) + draw_figure_label("(P)", size = 14), # set2[[4]], 
            plot_grid(set1[[8]]) + draw_figure_label("(Q)", size = 14), # set2[[8]],
            plot_grid(set1[[12]]) + draw_figure_label("(R)", size = 14), # set2[[12]], 
            plot_grid(set1[[16]]) + draw_figure_label("(S)", size = 14), # set2[[16]], 
            plot_grid(set1[[20]]) + draw_figure_label("(T)", size = 14), # set2[[20]], 
            
            top_titles[[6]], top_titles[[6]], top_titles[[6]], legend_all,
            ncol = 6, nrow = 6, 
            align = "hv", rel_widths = c(2,rep(c(15),5)),
            rel_heights = c(0.4, 1, 1, 1, 1, 0.3),
            greedy = T, axis = "rlbt", vjust = 1) -> p
  
}


