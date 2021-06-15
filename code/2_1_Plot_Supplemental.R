#### supplemental figures ####
##### co morbidity adjusted life expectancy #####
LE_estimates %>% 
  filter(wb %in% members$wb) %>% 
  mutate(wb = factor(wb)) %>% 
  ggplot(., aes(x = group, y = wb, fill = adjLE)) +
  geom_tile() +
  viridis::scale_fill_viridis(option = "inferno") +
  labs(x = "Age Group", y = "", fill = "Comorodbity Adjusted Life Expectancy") +
  cowplot::theme_cowplot() +
  scale_y_discrete(limits = rev) +
  theme(legend.position = "top",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 16)) -> p_tmp 

ggsave("figs/figS1_adjLE.png", p_tmp, width = 10, height = 18)

LE_estimates %>% 
  filter(wb %in% members$wb) %>% 
  mutate(wb = factor(wb),
         group = factor(group, ordered = T)) %>% 
  group_by(wb, group, LE) %>% 
  pivot_longer(cols = c(adjLE, LEdisc, adjQALEdisc)) %>% 
  ggplot(., aes(x = LE, y = value, color = group)) +
  geom_point(size = 4) +
  geom_abline(intercept = 0, slope = 1) +
  geom_label(x = 60, y = 55, 
             label = "adjLE", 
             color = "black", size = 6) +
  geom_label(x = 60, y = 30, 
             label = "LEdisc", 
             color = "black", size = 6) +
  geom_label(x = 60, y = 20, 
             label = "adjQALEdisc", 
             color = "black", size = 6) +
  viridis::scale_fill_viridis(option = "inferno") +
  labs(x = "Crude Life Expectancy", 
       y = "", color = "Age Group") +
  cowplot::theme_cowplot() +
  theme(  legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16)) -> p_tmp 

ggsave("figs/figS2_compare.png", p_tmp, width = 16, height = 12)

GDPpc %>% 
  full_join(members_world, by = "wb") %>% 
  mutate(Fit = if_else(wb %in% members_remove, F, T),
         Fit = if_else(wb %in% members$wb, Fit, as.logical(NA))) %>% 
  st_as_sf() -> tmp

tmp %>% 
  ggplot(aes(fill = log(GDPpc, 10))) +
  geom_sf() +
  scale_fill_viridis(option = "inferno") +
  coord_sf(xlim = c(-25, 90),
           ylim = c(30, 75),
           expand = F) +
  theme_map() +
  theme(strip.text = element_text(size = 14),
        legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        plot.title = element_text(size = 24),
        axis.title = element_text(size = 20)) +
  labs(fill = "Log10(GDPpc)")  -> p_tmp 

ggsave("figs/figS3_GDPpc.png", p_tmp, width = 15, height = 10)


gm_country <- unique(gm$wb) %>% sort
si_country <- unique(si$wb) %>% sort
fitted_country <- members$wb %>% .[!. %in% members_remove]

fitted_country[which(!fitted_country %in% gm_country)]


#### 1.1 population pyramid ####
cm_populations %>% 
  data.table() %>% 
  separate(age, into = c("LL", "UL"), sep = "-") #%>% 
  filter(location_type == 4) %>% 
  mutate(LL = parse_number(LL),
         LL = if_else(LL > 75, 75, LL),
         UL = as.numeric(UL),
         UL = if_else(UL>75|is.na(UL), "+", as.character(UL))) %>% 
  group_by(name, LL, UL) %>% 
  summarise(f = sum(f),
            m = sum(m)) %>% 
  unite(LL, UL, col = "age", sep = "-") %>% 
  mutate(age = if_else(age == "75-+", "75+", age),
         wb = countrycode(name, "country.name", "wb")) %>% 
  filter(wb %in% members$wb) -> all_pop


all_pop  %>% 
  mutate(age = factor(age,
                      levels = unique(all_pop$age))) %>% 
  rename(country_name = name) %>% 
  pivot_longer(cols = c("f", "m"),
               names_to = "gender",
               values_to = "pop") %>% 
  mutate(pop = if_else(gender == "m", -pop, pop),
         gender = factor(gender, levels = c("m", "f"))) %>% 
  filter(wb %in% members$wb[27:53]) %>% 
  ggplot(., aes(x = age, y = pop, fill = gender)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +
  scale_fill_manual(
    values = c("#6233a0", "#964ef5"),
    labels = c("Male", "Female")
  ) +
  scale_y_continuous(
    labels = function(x) abs(x)
  ) +
  facet_wrap(~wb, scale = "free", ncol = 5) +
  labs(x = "", y = "", fill = "") +
  cowplot::theme_cowplot() +
  theme(legend.position = "top",
        strip.background = element_blank(),
        plot.title = element_text(size = 24),
        axis.title = element_text(size = 20)) -> p1

ggsave("figs/figSX_pyramid_2.pdf", p1, width = 20, height = 20)

#### CLM ####
##### Panel 2: Illustration on Vaccine Characteristics #####
priority_selection_2 <- readRDS("~/GitHub/COVID19_EUR_VAC/data/intermediate/priority_selection_2.rds")
priority_selection_2[[3]] %>% 
  bind_rows(.id = "ROS") %>% 
  filter(w == "Before 2023") %>% 
  left_join(members, c("population" = 
                         "country_name")) %>% 
  left_join(covar$age, by = "wb") %>% 
  ggplot(., aes(x = adult, y = adjLE)) +
  geom_point(aes(color = policy)) +
  geom_line(aes(group = wb)) +
  # geom_boxplot() +
  facet_wrap(~ROS, ncol = 1) +
  scale_y_log10()

priority_selection_2[[3]] %>% 
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
  mutate(value = abs(value)) -> tmp 

tmp %>% 
  left_join(members, by = c("population" = "country_name")) %>% 
  left_join(covar$age, by = "wb") %>% 
  left_join(covar$contact, by = c("wb", "country_index")) %>% 
  left_join(covar$non_S, by = c("wb", "population")) %>% 
  mutate(policy = factor(policy, ordered = F),
         adult = as.numeric(adult))-> reg_tab

require(arm)
reg_tab %>% 
  mutate(adult = arm::rescale(adult),
         `older adult` = rescale(`older adult`),
         `total adult` = rescale(`total adult`),
         p_older = rescale(`older adult`/`total adult`),
         p = rescale(p),
         `adult-adult` = rescale(`adult-adult`),
         `adult-older adult` = rescale(`adult-older adult`),
         `adult-child` = rescale( `adult-child`),
         `older adult-older adult` = rescale(`older adult-older adult`),
         `child-child` = rescale(`child-child`),
         `child-older adult` = rescale(`child-older adult`)
  ) -> tmp

params <- CJ(variable = unique(tmp$variable) %>% as.character)

models <- list()
for(i in 1:nrow(params)){
  MASS::polr(policy ~ `total adult` + p_older +
               p + 
               `adult-adult` +
               # `older adult-older adult` + 
               `adult-older adult` +
               # 
               # `adult-child` + 
               `child-child` + ROS, #+
             # `child-older adult`, #collinearity 
             data = tmp %>% filter(variable == params$variable[i]),
             control = glm.control(maxit = 1000)) -> models[[i]]
}


tmp[,c("older adult", "total adult", "adult",
       "p", "p_older",
       "adult-adult","adult-older adult", "older adult-older adult",
       "child-child","adult-child","child-older adult")] %>% 
  distinct() %>% 
  PerformanceAnalytics::chart.Correlation() -> p

ggsave("figs/supplemental/pair_panels.png", p, height = 10, width = 10)

summaries <- models %>% map(summary)

cen <- models %>% map(coef) %>% 
  map(enframe) %>% map(rename, cen = value)
names <- cen[[1]] %>% names()
cis <- models %>% map(confint)
cis2 <- models %>% map(confint, level = 0.9)
var_labels = c("Total population",
               "Proportion of population\nwho are older adults",
               "Proportion of population no longer\n susceptible on 2021/01/01",
               "Adult-adult contacts",
               "Adult-older adult contacts",
               "Child-child contacts",
               "R2",
               "R3",
               "R4")

cis %>% 
  map(data.table) %>% 
  map2(.x = .,
       .y = cen,
       ~bind_cols(.x, .y)) %>% 
  map2(.x = .,
       .y = cis2 %>% 
         map(data.table),
       ~bind_cols(.x, .y)) %>% 
  setNames(unique(tmp$variable)) %>% 
  map(mutate, name = var_labels) %>% 
  bind_rows(.id = "variable") %>% 
  mutate(sign = `2.5 %`/`97.5 %`,
         sign2 =  `5 %`/`95 %`,
         sig = case_when(sign > 0 ~ "Significant @ 5%",
                         sign < 0 & sign2 > 0 ~ "Significant @ 10%",
                         sign < 0 & sign2 < 0 ~ "null",
                         TRUE~ as.character(NA)),
         variable = factor(variable,
                           levels = c("death_o", "cases", 
                                      "adjLE",
                                      "QALYloss",
                                      "HC"),
                           labels = c("Deaths","Cases", 
                                      "Adj. Life Expenctancy",
                                      "Quality Adj. Life Years",
                                      "Human Capital"))
  ) %>% 
  ggplot(., aes(x = name, y = cen, color = sig)) +
  geom_point(size = 3) +
  geom_segment(aes(x = name, xend = name,
                   y = `2.5 %`, yend = `97.5 %`)) +
  geom_segment(aes(x = name, xend = name,
                   y = `5 %`, yend = `95 %`),
               size = 1.2) +
  scale_color_manual(values = c("black", "orange", "red")) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~variable, ncol = 1) +
  labs(color = "", x = "", y = "Coefficients") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
    legend.position = "top") -> p
  # coord_flip() +
  # scale_x_discrete(limits = rev)  -> p #+

ggsave("figs/supplemental/clm.png", p, height = 10, width = 5)

# tmp %>%
#   ggplot(., aes(x = adult, color = policy)) + 
#   geom_density() +
#   facet_wrap(~ROS, scales = "free")


#### Figure S4: how decision time-frame affect strategies ####
res <- read_rds("data/intermediate/econ_by_VE_2.rds")
res %>%  
  filter(policy != 0) %>% 
  # bind_rows(.id = "ROS") %>% 
  mutate(wb = countrycode(population, "country.name", "wb")) %>% 
  dplyr::filter(variable %in% c("cases", "death_o",
                                "adjLE_pd",
                                "QALYloss_pd", 
                                #"VSLmlns_pd",
                                "HC_pd")) %>% 
  mutate(dir = if_else(variable %in% c("cases", "death_o"),
                       "min",
                       "max"),
         # we flip the sign for dir == "min" because we are looking to maximize
         # all the _pd HE measures, but minimise all the cases and death counts
         value = if_else(dir == "min", -1*value, value)) %>% 
  group_by(population, variable, ROS, w) %>% group_split() %>% 
  map(arrange, desc(value)) %>% 
  map(~.[1,]) %>% bind_rows() %>% 
  mutate(policy = paste0("P",policy)) %>% 
  group_by(w, variable, policy, ROS) %>% tally %>% 
  mutate(variable = factor(variable,
                           levels = c("death_o", "cases", 
                                      "adjLE_pd",
                                      "QALYloss_pd",
                                      "HC_pd"),
                           labels = c("Deaths","Cases", 
                                      "Adj. Life Expectancy",
                                      "Quality Adj. Life Years",
                                      "Human Capital"
                           )),
         policy = factor(policy,
                         levels = paste0("P", 1:4),
                         labels = c("V+", "V20", "V60","V75")),
         w = factor(w,
                    levels = c("1 month", "4 months",
                               "7 months", "Before 2023"),
                    labels = c("1m", "4m", "7m", "by '23")),
         ROS = factor(ROS,
                      levels = paste0("R",c(1,2,4,3)),
                      labels = rollout_labels)) -> tab2

tab2 %>% 
  ggplot(., aes(x = w,
                y = n,
                color = policy,
                fill = policy)) +
  geom_bar(stat = "identity",
           color = "black") +
  facet_grid(ROS ~ variable) +
  labs(x = "Decision Time Frame",
       y = "Number of Countries by Optimal Priorisation Strategy",
       fill = "Priority Strategies:",
       color = "Priority Strategies:") +
  scale_color_manual(values = priority_colors) +
  scale_fill_manual(values = priority_colors) +  
  theme_cowplot() +
  theme(legend.position = "top",
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        strip.background = element_rect(color = "white", fill = "white"),
        panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) -> figS4

ggsave(filename = "figs/supplemental/timing.png", figS4,
       width = 10, height = 10)

#### country name table ####
members %>% 
  mutate(country_name = countrycode(wb, "wb","country.name")) -> tmp

write_csv(tmp, "data/country_names.csv")
