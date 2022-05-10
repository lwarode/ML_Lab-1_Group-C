library(tidyverse)
library(fuzzyjoin)
library(sf)
library(tmap)
library(cowplot)
library(corrr)


# data --------------------------------------------------------------------
test_forecast_xgb <- read_csv("data/test_forecast_xgb_df.csv")
test_forecast_svm <- read_csv("data/test_forecast_svm_df.csv")
most_important_features_xgb <- read_csv("data/df_most_important_xgb.csv")

# validate forecasts

# xgb
test_forecast_xgb %>% 
  filter(year == 2021) %>% 
  arrange(desc(final_pred_prob)) %>% 
  top_n(10) %>% 
  select(country_name, final_pred_prob)

# svm
test_forecast_svm %>% 
  filter(year == 2021) %>% 
  arrange(desc(final_pred_prob)) %>% 
  top_n(10) %>% 
  select(country_name, final_pred_prob)

# map
# tmap_mode("view")
tmap_mode("plot")
data("World")

# fuzzy join function
ci_str_detect <- function(x, y){str_detect(x, regex(y, ignore_case = TRUE))}

# test forecasts 2021 -----------------------------------------------------
# xgb
xgb_forecasts_2021 <- World %>% 
  fuzzy_left_join(test_forecast_xgb, by = c("sovereignt" = "country_name"), match_fun = ci_str_detect) %>% 
  filter(year == 2021) %>% 
  st_as_sf()

class(xgb_forecasts_2021)

plot_2021_xgb <- tm_shape(xgb_forecasts_2021) +
  tm_polygons("final_pred_prob",
              title = "Predicted Prob. of ART",
              breaks = seq(0, 1, 0.2)) +
  tm_layout(main.title = "XGBoost",
            main.title.position = "center")

plot_2021_xgb

# svm
svm_forecasts_2021 <- World %>% 
  fuzzy_left_join(test_forecast_svm, by = c("sovereignt" = "country_name"), match_fun = ci_str_detect) %>% 
  filter(year == 2021) %>% 
  st_as_sf()

class(svm_forecasts_2021)

plot_2021_svm <- tm_shape(svm_forecasts_2021) +
  tm_polygons("final_pred_prob",
              title = "Predicted Prob. of ART",
              breaks = seq(0, 1, 0.2)) +
  tm_layout(main.title = "SVM",
            main.title.position = "center") 

plot_2021_svm

forecasts_2021_pl <- plot_grid(
  title = ggdraw() + draw_label("Test Forecasts, 2021", size = 20),
  tmap_grob(plot_2021_xgb),
  tmap_grob(plot_2021_svm),
  ncol = 1,
  scale = 1.7,
  rel_heights = c(0.2, 1, 1)
) 

forecasts_2021_pl

# test set forecasts ------------------------------------------------------
core_vars_xgb_test <- test_forecast_xgb %>% 
  select(1:6, final_pred, final_pred_prob) %>% 
  mutate(model = "xgb") 

core_vars_svm_test <- test_forecast_svm %>% 
  select(1:6, final_pred, final_pred_prob) %>% 
  mutate(model = "svm") 

test_set_forecasts <- core_vars_xgb_test %>% 
  full_join(core_vars_svm_test)

# facet plots xgb
xgb_test_forecasts <- World %>% 
  fuzzy_left_join(core_vars_xgb_test, by = c("sovereignt" = "country_name"), match_fun = ci_str_detect) %>% 
  # filter(year < 2021) %>%
  filter(!is.na(year)) %>% 
  st_as_sf()

plot_xgb_test_forecasts <- tm_shape(xgb_test_forecasts) +
  tm_polygons(col = "final_pred_prob",
              title = "Predicted Prob. of ART",
              breaks = seq(0, 1, 0.2)) +
  tm_facets(by = "year", ncol = 2) +
  tm_layout(main.title = "XGB",
            main.title.position = "center") 

plot_xgb_test_forecasts  

# facet plots SVM
svm_test_forecasts <- World %>% 
  fuzzy_left_join(core_vars_svm_test, by = c("sovereignt" = "country_name"), match_fun = ci_str_detect) %>% 
  # filter(year < 2021) %>% 
  filter(!is.na(year)) %>% 
  st_as_sf()

plot_svm_test_forecasts <- tm_shape(svm_test_forecasts) +
  tm_polygons(col = "final_pred_prob",
              title = "Predicted Prob. of ART",
              breaks = seq(0, 1, 0.2)) +
  tm_facets(by = "year", ncol = 2) +
  tm_layout(main.title = "SVM",
            main.title.position = "center") 

plot_svm_test_forecasts  

# aggregated plots --------------------------------------------------------
# by model (one observation per coutnry and per year)
test_forecasts_agg_model <- test_set_forecasts %>% 
  group_by(year, country_name) %>% 
  summarise(final_pred_prob = mean(final_pred_prob, na.rm = T))

test_forecasts_agg_model_2 <- test_forecasts_agg_model %>% 
  filter(year < 2021) %>% 
  group_by(country_name) %>% 
  summarise(final_pred_prob = mean(final_pred_prob, na.rm = T))

ensemble_test_forecasts <- World %>% 
  fuzzy_left_join(test_forecasts_agg_model, by = c("sovereignt" = "country_name"), match_fun = ci_str_detect) %>% 
  # filter(year < 2021) %>%
  filter(!is.na(year)) %>% 
  st_as_sf() 
  
ensemble_plot <- tm_shape(ensemble_test_forecasts) +
  tm_polygons(col = "final_pred_prob",
              title = "Predicted Prob. of ART",
              breaks = seq(0, 1, 0.2)) +
  tm_facets(by = "year", ncol = 3) +
  tm_layout(main.title = "Ensemble Model",
            main.title.position = "center") 

# plots needs to be saved manually (ggsave won't work)
ensemble_plot

# by model and year (one observation per country)
test_forecasts_year_agg_model <- World %>% 
  fuzzy_left_join(test_forecasts_agg_model_2, by = c("sovereignt" = "country_name"), match_fun = ci_str_detect) %>% 
  st_as_sf() 

agg_plot <- tm_shape(test_forecasts_year_agg_model) +
  tm_polygons(col = "final_pred_prob",
              title = "Predicted Prob. of ART",
              breaks = seq(0, 0.5, 0.05)) +
  tm_layout(main.title = "Aggregated Results, 2016-2020",
            main.title.position = "center") 

agg_plot

# feature analysis xgboost ------------------------------------------------


  
