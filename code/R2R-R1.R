#R2R
#### SA: uptake ####
baseline <- read_rds("data/intermediate/priority_selection_2_debug.rds")
SA_uptake <- read_rds("data/R1/SA_uptake.rds")  

data_baseline <- baseline[[3]] %>% bind_rows(.id = "ROS") %>% 
  mutate(param_set = "baseline") %>% 
  left_join(members[,c("country_name", "wb")],
            by = c("population" = "country_name")) %>% 
  mutate(remove = if_else(wb %in% members_remove, T, F))

data <- map(SA_uptake, function(x) x[[3]]) %>% 
  map(bind_rows, .id = "ROS") %>% 
  bind_rows(.id = "param_set") %>% 
  left_join(members[,c("country_name", "wb")],
            by = c("population" = "country_name")) %>% 
  mutate(remove = if_else(wb %in% members_remove, T, F))
  
bind_rows(data_baseline, data) %>% 
  filter(w == "2022") %>% 
  dplyr::select(ROS, policy, population, cases, death_o, 
                adjLE, QALYloss, HC, param_set, wb, remove) %>% 
  pivot_longer(cols = c(cases, death_o, 
                        adjLE, QALYloss, HC)) %>% 
  arrange(name) %>% 
  group_by(ROS, population, param_set, wb, remove, name) %>% 
  filter(policy != "0") %>% 
  mutate(rkv = rank(value)) %>% filter(rkv %in% c(1)) %>% 
  dplyr::select(-value) %>% 
  pivot_wider(names_from = param_set, values_from = policy) %>% 
  mutate_at(vars(c("baseline",`1`,`2`)),as.numeric) %>% 
  mutate(diff1 = baseline - `1`,
         diff2 = baseline - `2`, 
         diff3 = `1` - `2`) %>% 
  filter(remove == F) -> tmp

tmp %>% 
  # filter(diff1 != 0) %>%
  group_by(ROS, name, `baseline`, `1`) %>% tally %>% 
  pivot_wider(names_from = name, values_from = n) %>% 
  arrange(`baseline`,`1`) %>% 
  pivot_longer(cols = c(cases, death_o, adjLE, QALYloss, HC)) %>% 
  mutate(value = if_else(is.na(value), as.integer(0), value),
         name = factor(name, levels = c("death_o", "cases", "adjLE", 
                                        "QALYloss", "HC"),
                       labels = metric_labels),
         ROS = factor(ROS, levels = 1:4, labels = rollout_labels)) %>% 
  ggplot(., aes(x = `baseline`, y = `1`)) +
  geom_tile(aes(fill = value), color = "black") +
  geom_tile(aes(x = x, y = y), data = CJ(x = 1:4, y = 1:4), 
            fill = NA, color = "black") +
  facet_grid(ROS~name) +
  scale_fill_material("purple", limits = c(0, 38)) +
  theme_cowplot() +
  theme(legend.position = "top") +
  labs(x = "Original", fill = "",
       y = "Outcome with slightly lower uptake ") -> p1

tmp %>% 
  # filter(diff2 != 0) %>%
  group_by(ROS, name, `baseline`, `2`) %>% tally %>% 
  pivot_wider(names_from = name, values_from = n) %>% 
  mutate(change = paste0(`baseline`,"-",`2`)) %>% 
  arrange(`baseline`,`2`) %>% 
  pivot_longer(cols = c(cases, death_o, adjLE, QALYloss, HC)) %>% 
    mutate(value = if_else(is.na(value), as.integer(0), value),
           name = factor(name, levels = c("death_o", "cases", "adjLE", 
                                          "QALYloss", "HC"),
                         labels = metric_labels),
           ROS = factor(ROS, levels = 1:4,
                        labels = rollout_labels)) %>% # pull(value) %>% range
  ggplot(., aes(x = `baseline`, y = `2`)) +
  geom_tile(aes(fill = value), color = "black") +
  geom_tile(aes(x = x, y = y), data = CJ(x = 1:4, y = 1:4), 
            fill = NA, color = "black") +
  facet_grid(ROS~name) +
  scale_fill_material("purple", limits = c(0,38)) + 
  theme_cowplot() +
  theme(legend.position = "top") +
  labs(x = "Original", fill = "",
       y = "Outcome with significantly lower uptake") -> p2

p2 +
  theme(strip.text = element_text(size = 12),
        legend.position = "right",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  scale_x_continuous(breaks = 1:4, labels = policy_labels)  +
  scale_y_continuous(breaks = 1:4, labels = policy_labels) +
  theme(axis.text.x = element_text(angle = 90)) -> tmp

ggsave("figs/R2R-R1/lowest_uptake.pdf", 
       tmp,
       height = 8, width = 11)

#### SA: VOC ####
SA_VOC <- read_rds("data/R1/SA_VOC.rds")  
SA_VOC[[1]][[3]] %>% bind_rows(.id = "ROS") %>% mutate(VOC = T) %>% 
  bind_rows(baseline[[3]] %>% bind_rows(.id = "ROS") %>% mutate(VOC = F)) %>% 
  filter(w == "2022", policy != "0") %>% 
  left_join(members[,c("country_name", "wb")],
            by = c("population" = "country_name"))  %>% 
  dplyr::select(ROS, policy, population, cases, death_o, 
                adjLE, QALYloss, HC, VOC, wb) %>% 
  mutate(remove = if_else(wb %in% members_remove, T, F)) %>% 
  pivot_longer(cols = c(cases, death_o, 
                        adjLE, QALYloss, HC)) %>% 
  group_by(ROS, population, VOC, wb, remove, name) %>% 
  arrange(name) %>% 
  mutate(rkv = rank(value)) %>% 
  filter(rkv == 1) %>% 
  dplyr::select(-value) %>% 
  pivot_wider(names_from = VOC, values_from = policy) %>% 
  filter(remove == F) %>% 
  arrange(`TRUE`, `FALSE`) %>%
  group_by(`TRUE`, `FALSE`, ROS, name) %>% tally %>%
  mutate(name = factor(name,
                       levels = c("death_o", "cases",
                                  "adjLE", "QALYloss","HC"),
                       labels = metric_labels),
         ROS = factor(ROS, 1:4, rollout_labels)) %>% 
  ggplot(.,aes(x = `FALSE`, y = `TRUE`, fill = n)) +
  geom_tile(color = "black") +
  geom_tile(aes(x = x, y = y), data = CJ(x = 1:4, y = 1:4), 
            fill = NA, color = "black") +
  scale_fill_material("purple", limits = c(0,38)) + 
  facet_grid(ROS~name) +
  theme_cowplot() +
  theme(legend.position = "top") +
  labs(x = "w/o VOC", fill = "",
       y = "w/ VOC") -> p3
  
p3  +
  theme(strip.text = element_text(size = 12),
        legend.position = "right",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  scale_x_discrete(breaks = 1:4, labels = policy_labels)  +
  scale_y_discrete(breaks = 1:4, labels = policy_labels) +
  theme(axis.text.x = element_text(angle = 90))  -> tmp

ggsave("figs/R2R-R1/increased_beta.pdf", 
       tmp,
       height = 8, width = 11)

