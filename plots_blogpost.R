library(tidyverse)
library(fuzzyjoin)
library(sf)
library(tmap)
library(cowplot)
library(corrr)
library(ggdendro)


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

tmap_save(
  plot_2021_xgb,
  "figures/plot_2021_xgb.png",
  width = 2500,
  height = 1400
)

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

tmap_save(
  plot_2021_svm,
  "figures/plot_2021_svm.png",
  width = 2500,
  height = 1400
)

forecasts_2021_pl <- plot_grid(
  title = ggdraw() + draw_label("Test Forecasts, 2021", size = 20),
  tmap_grob(plot_2021_xgb),
  tmap_grob(plot_2021_svm),
  ncol = 1,
  scale = 1.7,
  rel_heights = c(0.2, 1, 1)
) 

forecasts_2021_pl

ggsave(
  "figures/forecasts_2021_pl.png",
  forecasts_2021_pl
)

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
# correlation plot
most_important_features_xgb_df <- most_important_features_xgb %>% 
  correlate() %>% 
  mutate(
    var_name = case_when(
      term == "lagged_diff_year_prior_v2x_elecoff" ~ "Elected officials index",
      term == "lagged_diff_year_prior_v2elembaut" ~ "EMB autonomy",
      term == "lagged_v2ellocons" ~ "Lower chamber election consecutive",
      term == "lagged_v2xlg_leginter" ~ "Legislature closed down or aborted",
      term == "lagged_v2clslavem" ~ "Freedom from forced labor for men",
      term == "lagged_v2lginvstp" ~ "Legislature investigates in practice",
      term == "lagged_diff_year_prior_v2x_neopat" ~ "Neopatrimonial Rule Index",
      term == "lagged_v2dlconslt" ~ "Range of consultation",
      term == "lagged_is_closed_autocracy" ~ "Closed autocracy",
      term == "lagged_v2x_clpriv" ~ "Private civil liberties index"
    )
  )

col_names <- most_important_features_xgb_df %>% 
  select(-term, -var_name) %>% 
  names

var_labels <- most_important_features_xgb_df %>% 
  pull(var_name)

colnames(most_important_features_xgb_df)[which(colnames(most_important_features_xgb_df) %in% col_names)] <- var_labels

feat_importance_corr_pl <- most_important_features_xgb_df %>% 
  select(-term) %>% 
  rename(term = var_name) %>% 
  # rearrange() %>% 
  rplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(colour = "Correlation\n")

feat_importance_corr_pl

ggsave(
  "figures/feat_importance_corr_pl.png",
  plot = feat_importance_corr_pl,
  width = (16/9) * 5,
  height =  5
)

# correlation network plot
feat_importance_network_pl <- most_important_features_xgb_df %>% 
  select(-term) %>% 
  rename(term = var_name) %>% 
  # rearrange() %>% 
  network_plot() 

feat_importance_network_pl

ggsave(
  "figures/feat_importance_network_pl.png",
  plot = feat_importance_network_pl,
  width = (16/9) * 6,
  height =  6
)


feat_importance_network_pl

# dendrogram
feat_importance_dendrogram_pl <- most_important_features_xgb_df %>% 
  select(-term) %>% 
  rename(term = var_name) %>% 
  as_tibble() %>%
  column_to_rownames("term") %>% 
  dist() %>% 
  hclust %>% 
  ggdendrogram(rotate = T)

feat_importance_dendrogram_pl

ggsave(
  "figures/feat_importance_dendrogram_pl.png",
  plot = feat_importance_dendrogram_pl,
  width = (16/9) * 6,
  height =  6
)

