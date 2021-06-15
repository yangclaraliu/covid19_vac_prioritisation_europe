#### figure 2: assumptions #### 
##### Panel 1: pyramid #####
cm_populations %>%
  filter(name == "United Kingdom of Great Britain") %>%
  mutate(
    age_LL = parse_number(str_sub(age, 1, 3)),
    age = if_else(age_LL >= 75, "75+", as.character(age))
  ) %>%
  .[, list(
    "f" = sum(f),
    "m" = sum(m)
  ), by = age] %>%
  mutate(index = 1:16) %>%
  melt(.,
       id.vars = c("age", "index"),
       variable.name = "gender",
       value.name = "pop",
       measure.vars = c("m", "f")
  ) %>%
  mutate(pop = if_else(gender == "m", -1 * pop, pop)) -> pyramid


age_lvls <- unique(pyramid$age)

pyramid %>%
  ggplot(., aes(x = index, y = pop, fill = gender)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +
  scale_fill_manual(
    values = c("#6233a0", "#964ef5"),
    labels = c("Male", "Female")
  ) +
  scale_y_continuous(
    breaks = c(-2000, 0, 2000),
    labels = c("2", "0", "2"),
    limits = c(-3000, 3500),
  ) +
  scale_x_continuous(
    breaks = 1:16,
    labels = age_lvls,
    limits = c(0.45, 16.5)
  ) +
  labs(
    y = "Population Size (Unit = Million)",
    x = "Age Group",
    title = "A: Age structure",
    fill = ""#,
    # subtitle = "UK as example."
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(
    xintercept = c(0:16) + 0.5,
    color = "grey",
    linetype = 2
  ) +
  theme_cowplot() +
  theme(legend.position = "none",
        plot.title = element_text(size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16)) +
  geom_text(aes(x = 10, 
                y = -2800, 
                label = "Male"), 
            color = "#6233a0", size = 8) +
  geom_text(aes(x = 10, 
                y = 3000, 
                label = "Female"), 
            color = "#964ef5", size = 8) -> p1

##### Panel 2: prioritisation #####
data.table(index = 1, priority = "V0", LL = 0, UL = 16, label = NA) %>%
  add_row(
    index = rep(2, 2),
    priority = rep("V+", 2),
    LL = c(0, 4),
    UL = c(4, 16),
    label = c(NA, "1^{st}")
  ) %>%
  # P3
  add_row(
    index = rep(3, 3),
    priority = rep("V20", 3),
    LL = c(0, 4, 12),
    UL = c(4, 12, 16),
    label = c(NA, "1^{st}", "2^{nd}")
  ) %>%
  # P4
  add_row(
    index = rep(4, 3),
    priority = rep("V60", 3),
    LL = c(0, 4, 12),
    UL = c(4, 12, 16),
    label = c(NA, "2^{nd}", "1^{st}")
  ) %>%
  # P5
  add_row(
    index = rep(5, 6),
    priority = rep("V75", 6),
    LL = c(0, 4, 12, 13, 14, 15),
    UL = c(4, 12, 13, 14, 15, 16),
    label = c(NA, "5^{th}", "4^{th}", "3^{rd}", "2^{nd}", "1^{st}")
  ) %>%
  mutate(
    label = factor(label),
    pattern = if_else(is.na(label), 1, 2) %>% factor()
  ) -> tmp

tmp %>%
  ggplot(., aes(
    xmin = index - 0.4,
    xmax = index + 0.4,
    ymin = LL,
    ymax = UL,
    fill = interaction(priority, pattern)
  )) +
  geom_hline(yintercept = 0:16, color = "grey", linetype = 2) +
  scale_fill_manual(values = c(NA, 
                               NA,
                               NA,
                               NA,
                               NA,
                               priority_colors[1],
                               priority_colors[2],
                               priority_colors[3],
                               priority_colors[4])) +
  geom_rect_pattern(aes(pattern = pattern)) +
  geom_rect(color = "black", fill = NA) +
  scale_pattern_manual(breaks = c(1, 2), values = c("stripe", NA)) +
  geom_text(aes(x = index, y = (UL + LL) / 2, label = label),
            size = 6, parse = T, color = c(rep("black",4),
                                           rep("white",2),
                                           rep("black",4),
                                           rep("white",5))
  ) +
  scale_y_continuous(
    breaks = c(1:16) - 0.5,
    labels = age_lvls
  ) +
  scale_x_continuous(
    breaks = c(1:5),
    labels = c("V0", "V+", "V20", "V60", "V75")
  ) +
  labs(
    x = "Strategy ID",
    title = "D: Vaccine prioritisation",
    y = "Age"
  ) +
  theme_cowplot() +
  theme(legend.position = "none",
        plot.title = element_text(size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16)) -> p2

##### Panel 3: roll out ##### 
data.table(date = c("2021-01-01",
                    "2021-03-01",
                    "2021-06-30", 
                    "2021-12-31", 
                    "2022-12-31"),
           R1 = c(NA, 0, 0.03, 0.1, 0.3),
           R2 = c(NA, 0, 0.03, 0.2, 0.5),
           R4 = c(0, NA, NA, 0.8, NA),
           R3 = c(0, NA, NA, NA, 0.8)) %>% 
  melt(id.vars = "date",
       measure.vars = c("R1","R2","R3", "R4"),
       variable.name = "R",
       value.name = "cov") %>% 
  mutate(date = ymd(date)) %>% 
  drop_na() %>% 
  mutate(R = factor(R, 
                    levels = c("R1", "R2", "R3", "R4"),
                    labels = rollout_labels)) -> ROS 

ROS %>% 
  ggplot(., aes(x = date, y = cov, group = R)) +
  geom_line(data = owid_vac %>%
               left_join(vac_denom, by = "wb") %>% 
               filter(wb != "CYP") %>% 
               mutate(p = people_vaccinated/tot), 
             aes(date, y = p, group = wb),
             size = 1, color = "grey", alpha = 0.5
  ) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = seq(0,0.8,0.1), color = "grey", linetype = 2) +
  geom_vline(xintercept = ymd(c("2021-01-01",
                                "2022-01-01",
                                "2023-01-01")), color = "grey", linetype = 2) +
  geom_label(aes(x = date,
                 y = cov,
                 label = R),
            data = ROS %>% filter(date == "2021-12-31") %>% 
              add_row(date = ymd("2021-12-31"), 
                      R = rollout_labels[3], cov = 0.4),
             size = 6) +
  theme_cowplot() +
  theme(
    plot.title = element_text(size = 24),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16)) +
  labs(x = "Date", y = "Vaccine Coverage",
       title = "C: Roll-out scenarios") +
  geom_text(aes(x = ymd("2021-01-01"), y = 0.75, label = "Year 1"), 
            vjust = 1, hjust = -0.05, size = 8) +
  geom_text(aes(x = ymd("2022-01-01"), y = 0.75, label = "Year 2"), 
            vjust = 1, hjust = -0.05, size = 8) -> p3

##### Panel 4: contact matrices #####
cm_matrices$`United Kingdom of Great Britain`$home + 
  cm_matrices$`United Kingdom of Great Britain`$work  + 
  cm_matrices$`United Kingdom of Great Britain`$school + 
  cm_matrices$`United Kingdom of Great Britain`$other -> matrices_all

matrices_all %>% 
  melt(., varnames = c("age1", "age2"), value.name = "contacts") %>% 
  ggplot(., aes(x = age2, y = age1, fill = contacts)) + 
  theme(legend.position = "bottom") + 
  geom_tile() +
  labs(x = "Age of Individual", y = "Age of Contacts", fill = "Contacts\n(/day)",
       title = "B: Contact patterns"#, 
       #subtitle = "UK as example."
       ) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(size = 24),
        axis.title = element_text(size = 20),
        # legend.position = "top",
        axis.text = element_text(size = 16)) +
  # viridis::scale_fill_viridis() -> p4
  ggsci::scale_fill_material("blue") -> p4

##### Panel 5: vaccine profiles#####
data.table(ve = c(0.5, 0.5, 1, 1),
           ei_v = c(0, 0.5, 1, 0)) %>% 
  ggplot(.) +
  geom_polygon(aes(x = ve, y = ei_v), fill = "grey90", color = "black", size = 1.2) +
  geom_vline(xintercept = c(0.5, 0.75, 1),
             linetype = 2,
             color = "grey50") +
  geom_hline(yintercept = c(0, 0.5, 0.75, 1),
             linetype = 2,
             color = "grey50") +
  geom_label(aes(x = x,
                 y = y,
                 label = l),
             color = "black", size =6,
             data = data.frame(x = c(0.95, 0.95, 0.75, 0.75, 0.5, 0.5),
                               y = c(0.95, 0.75, 0.75, 0.5, 0.5, 0),
                               l = paste("Profile", 1:6))) +
  geom_segment(data =   data.frame(x = c(0.95, 0.88, 0.75, 0.68, 0.48),
                                   xend = c(0.95, 0.82, 0.75, 0.57, 0.48),
                                   y = c(0.9, 0.75, 0.7, 0.5, 0.45),
                                   yend = c(0.8, 0.75, 0.54, 0.5, 0.05)),
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "black",
               arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  
  geom_text(aes(x = 0.75, y = 0.25, 
                label = "Possible \nVaccine Characteristics"),
            size = 8) +
  
  labs(x = "VE against symptomatic infection",
       y = "VE against overall infection",
       title = "E: Vaccine Characteristics") +
  scale_x_continuous(breaks = c(0.5, 0.75, 1),
                     limits = c(0.43, 1)) +
  theme_cowplot() +
  theme(plot.title = element_text(size = 24),
        axis.title = element_text(size = 20),
        # legend.position = "top",
        axis.text = element_text(size = 16)) -> p5


row_1 <- plot_grid(p1, p4, align = "hv",
                   axis = "l")
row_2 <- plot_grid(p3, p2, p5,
                   align = "hv",
                   axis = "l", ncol = 3)
# 
# plot_grid(p1, p4, NULL, p3, p2, p5,
#           align = "hv",
#           axis = "l",
#           ncol = 3) -> p

plot_grid(row_1, row_2, 
          align = "hv",
          axis = "l", ncol = 1,
          rel_heights = c(1.2, 1)) -> p

ggsave("figs/fig2.png", p, width = 20, height = 14) 

#### Figure 3: quality of fitting ####
##### Panel A-C: fitting time-series examples #####
#### put together results that shows fitting quality
p1_2 <- plot_fit_examples("data/intermediate/fit_examples_2.rds")
p1_3 <- plot_fit_examples("data/intermediate/fit_examples_3.rds")
  

 ##### Panel D: predicted proportion of immune population #####
p4_2 <- plot_non_S("data/intermediate/non_S_2.rds")
p4_3 <- plot_non_S("data/intermediate/non_S_3.rds")

plot_grid(p1_2, NULL, p4_2, ncol = 3, rel_widths = c(1, 0.1,4), align = "h", 
          axis = "bt") -> fig3_2

plot_grid(p1_3, NULL, p4_3, ncol = 3, rel_widths = c(1, 0.1,4), align = "h", 
          axis = "bt") -> fig3_3

ggsave("figs/Fig3_2.png",fig3_2, width = 25, height = 15)
ggsave("figs/Fig3_3.png",fig3_3, width = 25, height = 15)

p_t_2 <- plot_fitted_res(model_selected_2)
p_t_3 <- plot_fitted_res(model_selected_3)

p_r_2 <- plot_fitted_res(model_selected_2, var = 2)
p_r_3 <- plot_fitted_res(model_selected_3, var = 2)

p_rho_3 <- plot_fitted_res(model_selected_3, var = 3)

ggsave("figs/supplemental/p_t_2.png",p_t_2, width = 25, height = 15)
ggsave("figs/supplemental/p_t_3.png",p_t_3, width = 25, height = 15)
ggsave("figs/supplemental/p_r_2.png",p_r_2, width = 25, height = 15)
ggsave("figs/supplemental/p_r_3.png",p_r_3, width = 25, height = 15)
ggsave("figs/supplemental/p_rho_3.png",p_rho_3, width = 25, height = 15)

#### Figure 4: vaccination strategy selected by decision criteria ####
decisions_2 <- plot_decisions("data/intermediate/priority_selection_2.rds")
ggsave(filename = "figs/Fig4_2.png", decisions_2, width = 24, height = 13.5, dpi = 500)

decisions_2_w <- plot_decisions("data/intermediate/priority_selection_2_w.rds")
ggsave(filename = "figs/Fig4_2_w.png", decisions_2_w, width = 24, height = 13.5)

decisions_3 <- plot_decisions("data/intermediate/priority_selection_3.rds")
ggsave(filename = "figs/Fig4_3.png", decisions_3, width = 24, height = 13.5)

decisions_3_w <- plot_decisions("data/intermediate/priority_selection_3_w.rds")
ggsave(filename = "figs/Fig4_3_w.png", decisions_3_w, width = 24, height = 13.5)
#ggsave(filename = "figs/Fig4_2.png", p, width = 24, height = 13.5)

#### figure 5: decision by VE ####
##### panel 1: SA by VE by country####
require(ggh4x)
file_sero <- "data/intermediate/non_S_2.rds"
file_VE <- "data/intermediate/econ_by_VE_2.rds"
file_select <- "data/intermediate/priority_selection_2.rds"

tmp_sero <- read_rds(file_sero)
tmp_ve <- read_rds(file_VE)
tmp_select <- read_rds(file_select)

tmp <- tmp_ve %>% 
  dplyr::filter(w == "Before 2023",
                variable %in% c("cases", "death_o", "adjLE_pd",
                                "QALYloss_pd", "HC_pd")) %>% 
  left_join(ve_tab %>% rownames_to_column(var = "ve_set"),
            by = "ve_set")  %>% 
  left_join(tmp_sero, by = "population") %>% 
  mutate(dir = if_else(variable %in% c("cases", "death_o"),
                       "min", "max"),
         value = if_else(dir == "min", -1*value, value)) %>% 
  group_by(ve_set, run, population, w, variable, ROS) %>% group_split() %>% 
  map(arrange, desc(value)) %>% 
  map(~.[1,]) %>% bind_rows() %>% 
  arrange(p) %>% 
  filter(!wb %in% members_remove)

tmp %>% 
  mutate(profile = case_when(ve ==0.95 & ei_v == 0.95 ~ 1,
                             ve ==0.95 & ei_v == 0.75 ~ 2,
                             ve ==0.75 & ei_v == 0.75 ~ 3,
                             ve ==0.75 & ei_v == 0.5  ~ 4,
                             ve ==0.5  & ei_v == 0.5  ~ 5,
                             ve ==0.5  & ei_v == 0    ~ 6) %>% factor,
         ROS = factor(ROS, levels = paste0("R",c(1,2,3,4)),
                      labels = rollout_labels),
         variable = factor(variable,
                           levels = c("death_o", "cases", 
                                      "adjLE_pd",
                                      "QALYloss_pd",
                                      "HC_pd"),
                           labels = c("Deaths","Cases", 
                                      "Adj. Life Expectancy",
                                      "Quality Adj. Life Years",
                                      "Human Capital"
                           )),
         wb = factor(wb,
                     levels =  epi %>% 
                       filter(!loc %in% members_remove) %>% 
                       group_by(loc) %>%
                       summarise(tot = sum(deaths)) %>% 
                       arrange(tot) %>% pull(loc))) %>%  
  filter(!is.na(profile),
         !wb %in% members_remove,
         ROS %in% rollout_labels[c(1, 4)]) -> tmp_fp

# tmp_fp %>% 
#   #filter(ROS == rollout_labels[1]) %>% 
#   group_by(ROS, profile, variable, policy) %>% 
#   tally() %>% 
#   ggplot(., aes(x = profile,
#                 y = n,
#                 group = interaction(policy), 
#                 color = policy)) +
#   geom_line(size = 2) +
#   # geom_point() +
#   facet_wrap(ROS~variable, ncol = 5, scale = "free") +
#   scale_color_manual(breaks = c(0:4),
#                      values = c("grey",priority_colors),
#                      labels = c("V0", "V+", "V20", "V60","V75")) +
#   theme_bw() +
#   theme() +
#   labs(x = "Vaccine Profile")
# 
# tmp_fp %>% 
#   filter(ROS == rollout_labels[3]) %>% 
#   ggplot(., aes(x = ve, y = ei_v, color = policy)) +
#   geom_point(size = 2) + 
#   scale_color_manual(values = c("grey", priority_colors),
#                      labels = c("V0", "V+", "V20", "V60","V75")) +
#   facet_wrap(~wb)

ggplot(tmp_fp, aes(y = wb, 
                x = profile, 
                fill = policy, 
                color = policy)) +
  # geom_bar(stat = "identity", width = 1) +
  # geom_tile(color = "black") +
  geom_tile() +
  facet_nested(ROS ~ variable , scales = "free") +
  scale_y_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_fill_manual(values = c("grey",priority_colors), labels = c("V0", "V+", "V20", "V60", "V75")) +
  scale_color_manual(values = c("grey", priority_colors), labels = c("V0", "V+", "V20", "V60", "V75")) +
  labs(x = "Vaccine Profile", y = "", fill = "", color = "") +
  theme_cowplot() +
  theme(strip.background = element_rect(fill = NA),
        legend.position = "bottom",
        strip.text = element_text(size = 18),
        # strip.background = element_rect(colour = "black"),
        legend.text = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        # legend.text = element_text(size = 16),
        # axis.text.y = element_text(size = 16),
        # aspect.ratio = 1,
        plot.margin = unit(c(0, 0, 0, 0), "cm")#,
        # strip.text = element_text(size = 16)
        ) -> p

ggsave("figs/fig5_extended.png", p, width = 18,height = 24, dpi = 500)

