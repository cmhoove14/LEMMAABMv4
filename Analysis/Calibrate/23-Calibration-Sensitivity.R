# ---------------------------------------
# Sensitivity of model fits to different parameters
# Chris Hoover Feb 2021
# ---------------------------------------
library(tidyverse)
library(sensitivity)

fits <- readRDS(here::here("data", "processed", "LHS_Fits1_summary.rds"))
LHS  <- as_tibble(readRDS(here::here("data", "processed", "Calibration_LHS_Wynton.rds"))) %>% 
  mutate(sim = row_number())

lhs_fits <- fits %>% left_join(LHS, by = "sim") %>% 
  filter(hosp_fit > 0)

lhs_fits_long <- lhs_fits %>% 
  dplyr::select(-status) %>% 
  pivot_longer(hosp_fit:hosp_dths_race_fit_norm, 
               names_to = "outcomes", 
               values_to = "outcome_value") %>% 
  pivot_longer(bta_base:E0,
               names_to = "vars",
               values_to = "vars_value")

# Scatter plots of vars to outcomes ----------------
# Hospitalizations fit
lhs_fits_long %>% 
  filter(outcomes == "hosp_fit") %>% 
  ggplot(aes(x = vars_value, y = outcome_value)) +
    geom_point(pch = 16, size = 0.5) +
    facet_wrap("vars", ncol = 4, nrow = 7,
               scales = "free") +
    geom_smooth() +
    labs(y = "Hospitalization fit score",
         x = "Var value",
         title = "Hospitalizations nll to par vals")
  
ggsave(here::here("Plots", "LHS_Calibration", "Fits1", "hosp_fit_scatter.jpg"),
       width = 8, height = 8, units = "in")




# dths z score
lhs_fits_long %>% 
  filter(outcomes == "dths_fit") %>% 
  ggplot(aes(x = vars_value, y = outcome_value)) +
  geom_point(pch = 16, size = 0.5) +
  facet_wrap("vars", ncol = 4, nrow = 7,
             scales = "free") +
  geom_smooth() +
  labs(y = "Deaths fit score",
       x = "Var value",
       title = "Deaths fit score to par vals")

ggsave(here::here("Plots", "LHS_Calibration", "Fits1", "dths_fit_scatter.jpg"),
       width = 8, height = 8, units = "in")





# dths by race fit score
lhs_fits_long %>% 
  filter(outcomes == "dths_race_fit") %>% 
  ggplot(aes(x = vars_value, y = outcome_value)) +
  geom_point(pch = 16, size = 0.5) +
  facet_wrap("vars", ncol = 4, nrow = 7,
             scales = "free") +
  geom_smooth() +
  labs(y = "Deaths by race fit score",
       x = "Var value",
       title = "Deaths by race fit score to par vals")

ggsave(here::here("Plots", "LHS_Calibration", "Fits1", "dths_race_fit_scatter.jpg"),
       width = 8, height = 8, units = "in")





# cases by race fit score
lhs_fits_long %>% 
  filter(outcomes == "case_race_fit") %>% 
  ggplot(aes(x = vars_value, y = outcome_value)) +
  geom_point(pch = 16, size = 0.5) +
  facet_wrap("vars", ncol = 4, nrow = 7,
             scales = "free") +
  geom_smooth() +
  labs(y = "Cases by race fit score",
       x = "Var value",
       title = "Cases by race fit score to par vals")

ggsave(here::here("Plots", "LHS_Calibration", "Fits1", "case_race_fit_scatter.jpg"),
       width = 8, height = 8, units = "in")





# cases by ct fit score
lhs_fits_long %>% 
  filter(outcomes == "ct_cases_fit") %>% 
  ggplot(aes(x = vars_value, y = outcome_value)) +
  geom_point(pch = 16, size = 0.5) +
  facet_wrap("vars", ncol = 4, nrow = 7,
             scales = "free") +
  geom_smooth() +
  labs(y = "Cases by ct fit score",
       x = "Var value",
       title = "Cases by ct fit score to par vals")

ggsave(here::here("Plots", "LHS_Calibration", "Fits1", "ct_case_fit_scatter.jpg"),
       width = 8, height = 8, units = "in")




# Partial rank correlation coefficients ---------------
hosp_pcc <- pcc(X = lhs_fits %>% dplyr::select(bta_base:E0),
                y = lhs_fits$hosp_fit,
                rank = T,
                nboot = 100,
                conf = 0.95)$PRCC %>% 
  mutate(Vars = row.names(.),
         Outcome = "Hosp")

dths_pcc <- pcc(X = lhs_fits %>% dplyr::select(bta_base:E0),
                y = lhs_fits$dths_fit,
                rank = T,
                nboot = 100,
                conf = 0.95)$PRCC %>% 
  mutate(Vars = row.names(.),
         Outcome = "Deaths")

dths_race_pcc <- pcc(X = lhs_fits %>% dplyr::select(bta_base:E0),
                y = lhs_fits$dths_race_fit,
                rank = T,
                nboot = 100,
                conf = 0.95)$PRCC %>% 
  mutate(Vars = row.names(.),
         Outcome = "Deaths by race")

case_race_pcc <- pcc(X = lhs_fits %>% dplyr::select(bta_base:E0),
                     y = lhs_fits$case_race_fit,
                     rank = T,
                     nboot = 100,
                     conf = 0.95)$PRCC %>% 
  mutate(Vars = row.names(.),
         Outcome = "Cases by race")

case_ct_pcc <- pcc(X = lhs_fits %>% dplyr::select(bta_base:E0),
                     y = lhs_fits$ct_cases_fit,
                     rank = T,
                     nboot = 100,
                     conf = 0.95)$PRCC %>% 
  mutate(Vars = row.names(.),
         Outcome = "Cases by ct")


overall_pcc <- pcc(X = lhs_fits %>% dplyr::select(bta_base:E0),
                   y = lhs_fits$overall_fit_norm,
                   rank = T,
                   nboot = 100,
                   conf = 0.95)$PRCC %>% 
  mutate(Vars = row.names(.),
         Outcome = "Overall normalized")

all_pccs <- bind_rows(hosp_pcc, dths_pcc, dths_race_pcc, case_race_pcc, case_ct_pcc, overall_pcc)

all_pccs %>% 
  ggplot(aes(x = Vars, y = original, fill = Outcome)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    labs(y = "PRCC")

ggsave(here::here("Plots", "LHS_Calibration", "Fits1", "Outcome_fits_PRCC.jpg"),
       width = 8, height = 5, units = "in")
