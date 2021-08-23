# list.files("~/GitHub/COVID19_EUR_VAC/figs/intermediate/gs_fit_top49") %>%
#   gsub(".png","",.) -> fit_done
# 
# members %>%
#   dplyr::filter(! wb %in% countrycode(fit_done, "country.name", "wb")) %>%
#   pull(country_name) -> fit_yet

# data.table(todo = setdiff(fit_yet, fit_done)) %>% 
#   mutate(wb = countrycode(todo, "country.name", "wb")) %>% 
#   left_join(members, by = "wb") %>% 
#   pull(country_name) -> fit_yet

members  -> fit_yet

library(furrr)
plan(multisession, workers = 4)
rm(i)

# r <- 2:53; r <- r[!r %in% c(31, 50)]
# r <- 32:53; r <- r[!r %in% c(31, 50)]

r <- fit_yet$country_index; r <- r[!r %in% c(31, 50)]

out <- list()
cal_start <- "2019-12-01"
# input 1 = introduction time
# input 2 = basic reproduction number
# input 3 = death reporting rate

fit_func_3 <- function(input){
  gen_country_basics(country = cn,
                     date_start = as.character(ymd("2019-12-01") + input[1]),
                     waning_nat = 52*7*3,
                     date_end = "2020-12-31",
                     R0_assumed = input[2]) -> params
  
  params$processes[[2]]$prob[1,] <- params$processes[[2]]$prob[1,]*input[3] 
  
  cm_simulate(params) %>%
    .[["dynamics"]] %>% 
    .[compartment == "death_o", sum(value), by = t] %>%
    rename(t_internal = t,
           deaths_predicted = V1) %>%
    mutate(date = ymd("2019-12-01") + floor(input[1] + as.numeric(t_internal))) %>% 
    full_join(., data_tmp, by = "date") %>% 
    mutate(deaths = if_else(is.na(deaths), 
                            0, 
                            deaths) %>% round,
           deaths_predicted = if_else(is.na(deaths_predicted), 
                                      0, 
                                      deaths_predicted),
           ll = dpois(deaths, deaths_predicted, log = T),
           ll = if_else(is.infinite(ll), -12, ll)) %>% 
    pull(ll) %>% sum  -> a
  
  return(-a)
}

fit_func_2 <- function(input){
  gen_country_basics(country = cn,
                     date_start = as.character(ymd("2019-12-01") + input[1]),
                     waning_nat = 52*7*3,
                     date_end = "2020-12-31",
                     R0_assumed = input[2]) -> params
  
  # params$processes[[2]]$prob[1,] <- params$processes[[2]]$prob[1,]*input[3] 
  
  cm_simulate(params) %>%
    .[["dynamics"]] %>% 
    .[compartment == "death_o", sum(value), by = t] %>%
    rename(t_internal = t,
           deaths_predicted = V1) %>%
    mutate(date = ymd("2019-12-01") + floor(input[1] + as.numeric(t_internal))) %>% 
    full_join(., data_tmp, by = "date") %>% 
    mutate(deaths = if_else(is.na(deaths), 
                            0, 
                            deaths) %>% round,
           deaths_predicted = if_else(is.na(deaths_predicted), 
                                      0, 
                                      deaths_predicted),
           ll = dpois(deaths, deaths_predicted, log = T),
           ll = if_else(is.infinite(ll), -12, ll)) %>% 
    pull(ll) %>% sum  -> a
  
  return(-a)
}

out_3 <- out_2 <- list()
for(i in r){
  cn <- "Albania" #fit_yet$country_name[i]
  wb_tmp <- countrycode(cn, "country.name","wb")
  data_tmp <- owid_epi[wb ==wb_tmp] %>% .[order(date)] %>% .[date <= "2020-12-31"]
  
  owid_epi[wb == wb_tmp & deaths > 1] %>% 
    .[order(date)] %>% 
    pull(date) %>% .[1] - 90 -> intro_LL
  
  intro_LL + 90 -> intro_UL
  t_LL <- as.numeric(ymd(intro_LL) - ymd(cal_start))
  t_UL <- as.numeric(ymd(intro_UL) - ymd(cal_start))
  
  controlDE <- list(reltol=.0001, steptol=20, itermax = 100, trace = 10)
  
  # two parameter fitting
  # DEoptim(fn = fit_func_2,
  #         lower = c(t_LL, 1.5),
  #         upper = c(t_UL, 5),
  #         control = controlDE) -> out_2[[i]]
  # res_2 <- out_2[[i]]$optim$bestmem# out_2[[i]]$member$bestmemit %>% tail(1)
  # res_2[1] <- round(res_2[1])
  
  # three parameter fitting
  DEoptim(fn = fit_func_3,
          lower = c(t_LL, 1.5, 0.01),
          upper = c(t_UL, 5, 1),
          control = controlDE) -> out_3[[i]]
  res_3 <- out_3[[i]]$optim$bestmem# out_3[[i]]$member$bestmemit %>% tail(1)
  res_3[1] <- round(res_3[1])

  # plot results
  gen_country_basics(country = cn,
                     date_start = as.character(ymd("2019-12-01") + res_2[1]),
                     date_end = "2020-12-31",
                     waning_nat = 52*7*3,
                     R0_assumed = res_2[2]) %>% 
    cm_simulate() %>% 
    .[["dynamics"]] %>% 
    .[compartment == "death_o", sum(value), by = list(t)] %>% 
    mutate(n_param = 2,
           t_external = res_2[1]) -> dyna_2
    
  gen_country_basics(country = cn,
                     date_start = as.character(ymd("2019-12-01") + res_3[1]),
                     date_end = "2020-12-31",
                     waning_nat = 52*7*3,
                     R0_assumed = res_3[2]) %>% 
    cm_simulate() %>% 
    .[["dynamics"]] %>% 
    .[compartment == "death_o", sum(value), by = list(t)] %>% 
    mutate(n_param = 3,
           t_external = res_3[1])-> dyna_3
  
  dyna_3 %>% 
    mutate(status = "scaled",
           V1 = V1 * res_3[3]) %>% 
    bind_rows(dyna_3) %>% 
    mutate(status = if_else(is.na(status), "raw", status)) %>% 
    rename(t_internal = t,
           deaths_predicted = V1) %>%
    mutate(date = ymd("2019-12-01") + t_external + as.numeric(t_internal)) %>% 
    full_join(., data_tmp, by = "date") %>% 
    mutate(deaths = if_else(is.na(deaths),
                            0,
                            deaths) %>% round,
           n_param = paste0("# of varying parameters = ", n_param)) %>%
    ggplot(., aes(x = date, color = status)) +
    geom_point((aes(y = deaths)), color = "black") +
    geom_point(aes(y = deaths_predicted)) +
    scale_color_manual(values = c("red", "orange")) +
    facet_wrap(~n_param, ncol = 1) +
    labs(title = cn, x = "") +
    # geom_point(aes(y = deaths_predicted*res[3]), color = "blue") +
    theme_bw() ->  p_tmp
    
  ggsave(paste0("figs/intermediate/DEoptim_fit_combined_debug/", cn, ".png"),
           plot  = p_tmp,
           width = 10, height = 10)
  
  print(paste0(cn, " completed!"))
  save(out_2, out_3, file = "data/intermediate/out_combined_debug.rdata")
  rm(cn)
}

# out_2[[1]]$optim$


# lapply(out_2, "[[", "optim") %>% lapply(., "[[", "bestmem") -> out_2_list
# out_2_list %>% map(is.null) %>% unlist %>% which -> out_2_null
# for(i in out_2_null) out_2_list[[i]] <- data.frame(par1 = NA, par2 = NA)
# 
# out_2_list %>% bind_rows() %>%
#   setNames(c("t","r")) %>%
#   bind_cols(members) %>%
#   mutate(date = ymd("2019-12-01") + t) -> tmp
# write_rds(tmp, "data/intermediate/DEoptim2_selected_debug.rds")
# 
# lapply(out_3, "[[", "optim") %>% lapply(., "[[", "bestmem") -> out_3_list
# out_3_list %>% map(is.null) %>% unlist %>% which -> out_3_null
# for(i in out_3_null) out_3_list[[i]] <- data.frame(par1 = NA, par2 = NA, par3 = NA)
# 
# out_3_list %>% bind_rows() %>%
#   setNames(c("t","r","par3")) %>%
#   bind_cols(members) %>%
#   mutate(date = ymd("2019-12-01") + t) -> tmp
# write_rds(tmp, "data/intermediate/DEoptim3_selected_debug.rds")


# optim(par = c(80, 2), 
  #       fn = fit_func, 
  #       # gr = next_fun,
  #       lower = c(t_LL, 1.5),
  #       upper = c(t_UL, 5),
  #       # method = "SANN",
  #       method = "L-BFGS-B",
  #       control = list(trace = 5,
  #                      fnscale = -1))

  # parameter table 
  # tab <- CJ(R0 = R0_range, t = t_LL:t_UL) %>% 
  #   mutate(date = as.Date("2019-12-01") + t,
  #          date = as.character(date))
  
  # generate search
  # future_pmap(.l = list(country = cn,
  #                       date_start = tab$date,
  #                       date_end = "2020-12-31",
  #                       R0_assumed = tab$R0),
  #             .f = gen_country_basics,
  #             .progress = T) %>% 
  #   map(cm_simulate) %>% 
  #   future_map(~.[["dynamics"]] %>% 
  #                .[compartment == "death_o", sum(value), by = t])%>% 
  #   bind_rows(.id = "set") %>% 
  #   left_join( tab %>% rownames_to_column(var = "set"), by = "set") %>% 
  #   mutate(date = t.x + t.y + ymd(cal_start)) %>% 
  #   rename(deaths_predicted = V1, t = t.x, t_intro = t.y) %>% 
  #   merge(data_tmp, by = "date", all = T) %>% 
  #   mutate(deaths = if_else(is.na(deaths), 0, deaths)) %>% 
  #   mutate(ll = dnorm(deaths_predicted, deaths, 1, log = T))  -> sim_res 
  # 
  # sim_res %>% 
  #   .[,sum(ll), by = set] %>% 
  #   .[order(-V1)] %>% 
  #   head(., 49) %>% 
  #   setNames(c("set","ll")) -> sim_set
  
  # sim_res %>% 
  #   filter(set %in% sim_set$set) %>% 
  #   group_by(set) %>% 
  #   summarise(deaths_predicted = sum(deaths_predicted),
  #             deaths = sum(deaths)) %>% 
  #   mutate(diff = abs(deaths - deaths_predicted)) -> diff
  # 
  # sim_set %>% 
  #   left_join(tab %>% rownames_to_column(var = "set"),
  #             by = "set") %>% 
  #   left_join(diff, by = "set") -> fit
  # 
  # write_rds(fit, 
  #           paste0("~/GitHub/COVID19_EUR_VAC/data/intermediate/gs_fit_top49/",
  #                  cn,
  #                  "_fit.rds"))
  # 
  # sim_res %>% 
  #   filter(set %in% sim_set$set) %>% 
  #   ggplot() +
  #   geom_line(aes(x = date, y = deaths_predicted, group = set), color = "red") +
  #   geom_line(aes(x = date, y = deaths)) +
  #   facet_wrap(~set) +
  #   labs(title = cn) -> p
  # 
  # ggsave(paste0("~/GitHub/COVID19_EUR_VAC/figs/intermediate/gs_fit_top49/",
  #               cn,".png"), p)
  # print(paste0("Task completed for: ", cn)) 
  
#}
# out_2 <- readRDS("data/intermediate/out_3.rds")

# lapply(lapply(out, "[[", "member"), "[[", "bestvalit") %>%
#   map(enframe) %>%
#   bind_rows(.id = "country_index") %>%
#   mutate(country_index = as.numeric(country_index)) %>%
#   left_join(members, by = "country_index") %>%
#   filter(value > 0.1) %>%
#   ggplot(., aes(x = name, y = -value, group = country_name)) +
#   geom_point() +
#   facet_wrap(~wb, scales = "free") +
#   cowplot::theme_cowplot() -> p_tmp
# 
# ggsave(paste0("figs/intermediate/DEoptim_diag_2.png"),
#        plot  = p_tmp,
#        width = 20, height = 10)

# lapply(lapply(out_3, "[[", "member"), "[[", "bestmemit") %>%
#   map(tail, 1) %>%
#   map(data.frame) %>%
#   bind_rows(., .id = "country_index") %>%
#   mutate(country_index = as.numeric(country_index)) %>%
#   left_join(members, by = "country_index") %>%
#   mutate(fit = if_else(wb %in% members_remove, "fail", "pass")) %>%
#   rename(t = par1, r = par2) %>%
#   mutate(t = round(t),
#          date = ymd("2020-12-01") + t) %>%
#   write_rds(., "data/intermediate/DEoptim3_selected.rds")

# DEoptim_model_selected <- readRDS("data/intermediate/DEoptim_model2_selected.rds")

#### model output using new info ####
#  
# DEoptim_model_selected %>% 
#   full_join(members_world, by = "wb") %>%
#   dplyr::select(-NAME_ENGL, -CNTR_NAME, -CNTR_ID, 
#                 -FID, -ISO3_CODE) %>%
#   st_as_sf() %>% 
#   sfheaders::sf_remove_holes() -> tmp
# 
# # tmp %>% filter(t_intro_np %in% c(1))
# # tmp %>% filter(R0_np %in% c(10))
# 
# tmp %>% 
#   ggplot(aes(fill = par3)) +
#   geom_sf(data = tmp) +
#   # geom_sf_pattern(data = tmp %>% filter(fit == "fail"), 
#   #                 pattern_spacing = 0.03,
#   #                 pattern_density = 0.2,
#   #                 color = "black",
#   #                 # fill = "grey90",
#   #                 alpha = 0.5,
#   #                 pattern_fill = "grey70",
#   #                 pattern = "crosshatch") +
#   geom_sf(data = tmp %>% filter(is.na(fit)), 
#           fill = "grey50", 
#           color = "black") +
#   scale_fill_viridis() +
#   # scale_fill_viridis(discrete = T, na.translate = F, option = "inferno", 
#   #                    labels = c("Earliest", rep("",8), "Latest Introduction")) +
#   # scale_fill_manual(breaks = 1:10,
#   #                   values = colorRampPalette(c("firebrick", 
#   #                                               "white"))(11)[1:10], 
#   #                   na.translate = F,
#   #                   labels = c("Earliest", rep("",8), "Latest Introduction")) +
#   coord_sf(xlim = c(-25, 90),
#            ylim = c(30, 75),
#            expand = F) +
#   theme_map() +
#   theme(strip.text = element_text(size = 14),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 14),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         plot.margin = unit(c(0, 0, 0, 0), "cm"),
#         plot.title = element_text(size = 24),
#         axis.title = element_text(size = 20)) +
#   labs(fill = "Reporting Rate") -> p_tmp
#   
# ggsave(paste0("figs/intermediate/DEoptim_fit_3/", "DEoptim3_reporting", ".png"),
#        plot  = p_tmp,
#        width = 15, height = 10)
# 
#   # guides(fill = guide_legend(nrow = 1,
#   #                            label.position = "bottom",
#   #                            override.aes = list(color = NA,
#   #                                                pattern = NA))) +
# 
