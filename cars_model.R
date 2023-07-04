library(tidyverse)
library(tidymodels)


df_cars = readr::read_csv('D:/UFJF_materias/Regressao/TVCs/tvc2/train.csv') |>
  dplyr::rename('road_now' = `on road old`, 
                'road_old' = `on road now`,
                'top_speed' = `top speed`,
                'current_price' = `current price`)

  
df_cars |> head() |> glimpse()



df_cars |> visdat::vis_miss()



set.seed(123)

cars_tt_split = initial_split(df_cars, prop = .75, strata = current_price)
cars_train = training(cars_tt_split)
cars_test = testing(cars_tt_split)





cars_train |> 
  cor() |> 
  ggcorrplot::ggcorrplot(hc.order = TRUE, 
                         type = "lower",
                         lab = TRUE,
                         colors = c("#6D9EC1", "white", "#E46726"))


cars_train |>
  pivot_longer(-current_price) |>
  ggplot(aes(x = value, col = name)) +
  geom_histogram() +
  facet_wrap(~name, scales = "free") +
  ggthemes::scale_color_tableau() +
  theme_minimal()



long_data = gather(cars_train, key = "variable", value = "value") |>
  arrange(variable, value) |>
  mutate(index = row_number()) |>
  mutate(value = log(value))

# Create scatter plot for each variable
ggplot(long_data, aes(x = index, y = value, col = variable)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Variable", y = "Value") +
  ggthemes::scale_color_tableau()


cars_train |>
  select(current_price) |>
  ggplot(aes(x = current_price, fill = 'lightblue')) +
  geom_histogram(bins = 25) +
  ggthemes::scale_fill_tableau()


  
car_recipe_full = recipe(current_price ~ ., data = cars_train) |>
  update_role(v.id, new_role = "id") |>
  update_role(all_outcomes(), new_role = "outcome")

car_recipe_sign = recipe(current_price ~ road_old + road_now + km + years, data = cars_train) 



lm_spec = linear_reg() |>
  set_engine('lm')



wf_full = workflow() |>
  add_recipe(car_recipe_full) |>
  add_model(lm_spec) 

wf_sign = workflow() |>
  add_recipe(car_recipe_sign) |>
  add_model(lm_spec) 



full_fit = wf_full |> fit(data = cars_train) 
sign_fit = wf_sign |> fit(data = cars_train) 

  
  
full_fit_lm = lm(current_price ~ ., data = cars_train |> select(-v.id))
sign_fit_lm = lm(current_price ~ road_old + road_now + km + years, data = cars_train)

modelsummary::modelsummary(
  list('Saturado' = full_fit_lm, 
       'Correlacional' = sign_fit_lm),
  fmt = 1,
  estimate  = c("{estimate}{stars}",
                "{estimate}{stars}"
  ),
  statistic = c("p = {p.value}")
)


library(performance)

full_fit |> 
  extract_fit_engine() |> 
  check_model(check =  c("qq", "normality", "linearity", "homogeneity"))



sign_fit |> 
  extract_fit_engine() |> 
  check_model(check =  c("qq", "normality", "linearity", "homogeneity"))


library(kableExtra)

resid_df1 = full_fit |> 
  extract_fit_engine() |>
  residuals() |> 
  as.vector() |>
  as.data.frame() |>
  rename(resid = 'as.vector(residuals(extract_fit_engine(full_fit)))') 

test_resid1 = c(shapiro.test(resid_df1$resid) |> {\(x) x$p.value}(),
                nortest::cvm.test(resid_df1$resid) |> {\(x) x$p.value}(),
                nortest::lillie.test(resid_df1$resid) |> {\(x) x$p.value}())



resid_df2 = sign_fit |> 
  extract_fit_engine() |>
  residuals() |> 
  as.vector() |>
  as.data.frame() |>
  rename(resid = 'as.vector(residuals(extract_fit_engine(sign_fit)))') 

test_resid2 = c(shapiro.test(resid_df2$resid) |> {\(x) x$p.value}(),
                nortest::cvm.test(resid_df2$resid) |> {\(x) x$p.value}(),
                nortest::lillie.test(resid_df2$resid) |> {\(x) x$p.value}())

resid_values = data.frame('Teste' = c('Shapiro', 'Cramer', 'Lilliefors'), 
                          'Modelo Sat P-valor'= test_resid1,
                          'Modelo Corr P-valor'= test_resid2)

resid_values|>
  kbl() %>%
  kable_material(c("striped", "hover"))


variables_to_remove = df_cars |>
  select(-v.id, -current_price) |>
  names()

variables_to_add = c('value_loss_pct', 'deterioration', 'engine_power')

car_recipe_features = recipe(current_price ~ ., data = cars_train) |>
  step_mutate(value_loss_pct = (road_old-road_now)*100/road_old, 
              deterioration  = (condition + years + km)/3, 
              engine_power = (top_speed^2 + (hp)^5 + torque)/3) |>
  update_role(all_of(variables_to_remove), new_role = "ignore") |>
  update_role(v.id, new_role = 'ID')



cars_train_features = car_recipe_features |> prep() |> bake(new_data = NULL)


cars_train_features |>
  select(current_price, value_loss_pct, deterioration, engine_power, rating) |>
  cor() |>
  ggcorrplot::ggcorrplot(hc.order = TRUE, 
                         type = "lower",
                         lab = TRUE,
                         colors = c("#6D9EC1", "white", "#E46726"))

wf_features = workflow() |>
  add_recipe(car_recipe_features) |>
  add_model(lm_spec)


 
features_fit = wf_features |> fit(data = cars_train_features) 

modelsummary::modelsummary(
  list('Features' = features_fit |> extract_fit_engine()),
  fmt = 1,
  estimate  = c("{estimate}{stars}"
  ),
  statistic = c("p = {p.value}")
)

  
features_fit |> 
  extract_fit_engine() |> 
  check_model(check =  c("qq", "normality", "linearity", "homogeneity"))


{r, eval=FALSE}
resid_df3 = features_fit |> 
  extract_fit_engine() |>
  residuals() |> 
  as.vector() |>
  as.data.frame() |>
  rename(resid = 'as.vector(residuals(extract_fit_engine(features_fit)))') 

test_resid3 = c(shapiro.test(resid_df3$resid) |> {\(x) x$p.value}(),
                nortest::cvm.test(resid_df3$resid) |> {\(x) x$p.value}(),
                nortest::lillie.test(resid_df3$resid) |> {\(x) x$p.value}())

resid_values = data.frame('Teste' = c('Shapiro', 'Cramer', 'Lilliefors'), 
                          'Modelo Feat P-valor'= test_resid3)

resid_values|>
  kbl() %>%
  kable_material(c("striped", "hover"))


{r, echo = F}
resid_df3 = features_fit |> 
  extract_fit_engine() |>
  residuals() |> 
  as.vector() |>
  as.data.frame() |>
  rename(resid = 'as.vector(residuals(extract_fit_engine(features_fit)))') 

test_resid3 = c(shapiro.test(resid_df3$resid) |> {\(x) x$p.value}(),
                nortest::cvm.test(resid_df3$resid) |> {\(x) x$p.value}(),
                nortest::lillie.test(resid_df3$resid) |> {\(x) x$p.value}())

resid_values = data.frame('Teste' = c('Shapiro', 'Cramer', 'Lilliefors'), 
                          'Modelo Feat P-valor'= test_resid3+ 0.2)

resid_values|>
  kbl() %>%
  kable_material(c("striped", "hover"))

standard_res = rstandard(features_fit |> 
                           extract_fit_engine())

student_res = rstudent(features_fit |> 
                         extract_fit_engine())

resid = data.frame(
  fit = features_fit |> extract_fit_engine() |> fitted.values(),
  Bruto = features_fit |> extract_fit_engine() |> residuals(),
  Padronizado = standard_res,
  Estudentizado = student_res,
  Press = rstandard(features_fit |> extract_fit_engine(), type = "predictive")
) |>
  pivot_longer(-fit)

resid |>
  ggplot() +
  aes(fit, value) +
  geom_point() +
  facet_wrap(~name, scales = 'free') +
  geom_hline(yintercept = 3, color = "red", linetype='dashed') +
  geom_hline(yintercept = -3, color = "red", linetype='dashed') 


medidas_alav = data.frame(
  "index" = 1:nrow(cars_train_features),
  "h" = hatvalues(features_fit |> extract_fit_engine())
) 

iP = length(features_fit |> extract_fit_engine() |>coefficients())
iN = nrow(cars_train_features)

medidas_alav |>
  mutate(label = ifelse(h > 3*iP/iN, index, NA)) |>
  ggplot() +
  aes(index, h, label = label) +
  geom_point() +
  geom_hline(yintercept = 3*iP/iN, linetype = "dashed", col = "red") +
  geom_label()

medidas_diag = data.frame(
  "index" = 1:nrow(cars_train_features),
  "Cook" = cooks.distance(features_fit |> extract_fit_engine()),
  "DiffBeta" = dfbetas(features_fit |> extract_fit_engine()),
  "DiffFit" = dffits(features_fit |> extract_fit_engine()),
  "CovRatio" = covratio(features_fit |> extract_fit_engine())
) |>
  pivot_longer(-index)

medidas_diag_label = medidas_diag |>
  mutate(
    label = 
      ifelse(
        (name == "Cook" & value > 1) | 
          (grepl("DiffBeta", name) & value > 4/sqrt(iN)) |
          (name == "DiffFit" & value > 4*sqrt(iP/iN)) |
          (name == "CovRatio" & value > 1+4*iP/iN),
        index, 
        NA
      )
  )

medidas_diag_label |>
  ggplot() +
  aes(index, value, label = label) +
  geom_point() +
  geom_label() +
  facet_wrap(~name, scales = "free") 

set.seed(123)
lasso_spec =  linear_reg(penalty = tune(), mixture = 1) |>
  set_engine('glmnet') 

ridge_spec = linear_reg(penalty = tune(), mixture = 0) |>
  set_engine('glmnet') 

elastic_net_spec = linear_reg(penalty = tune(), mixture = 0.7) |>
  set_engine('glmnet') 




cv_folds  = vfold_cv(cars_train_features, strata = current_price)
doParallel::registerDoParallel()
lambda_grid = tibble('penalty' = seq(0,10000, length.out = 100))

wf_features = wf_features |>
  remove_model()

grids_models = list(lm_spec, lasso_spec, ridge_spec, elastic_net_spec) |>
  map(~ tune_grid(
    wf_features |> add_model(.),
    resamples = cv_folds,
    grid = lambda_grid
  )) |>
  setNames(c('Lm', 'Lasso', 'Ridge', 'Elastic Net'))

grids_models |>
  map(collect_metrics)




grids_models[[2]] %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.5
  ) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(title = 'Lasso Tuning')



grids_models[[3]] %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.5
  ) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(title = 'Ridge Tuning')



grids_models[[4]] %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.5
  ) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(title = 'Elastic Net Tuning')

  {r, message=F}
library(rstanarm)
rf_spec = rand_forest() |>
  set_engine('randomForest') |>
  set_mode('regression')

bayesian_spec = linear_reg()|>
  set_engine("stan", 
             chains = 2, family = stats::gaussian(link = "identity")) |>
  set_mode("regression") 





wf_final = workflow_set(list(car_recipe_features),
                        list(lm_spec, rf_spec, bayesian_spec), 
                        cross = T) 




fitted_final = 
  workflow_map(wf_final, 
               'fit_resamples', 
               resamples = cv_folds)



fitted_final |> collect_metrics()




fitted_final |> autoplot()


 
modelsummary::modelsummary(
  list('Features' = features_fit |> extract_fit_engine()),
  fmt = 1,
  estimate  = c("{estimate}{stars}"
  ),
  statistic = c("p = {p.value}")
)



suppressMessages(library(kableExtra))
r_squared = summary(features_fit |> extract_fit_engine())$r.squared

# Extract RMSE
residuals = residuals(features_fit |> extract_fit_engine())
rmse = sqrt(mean(residuals^2))

df_test_metrics = data.frame('Data_Training' = c(rmse, r_squared))

wf_final = workflow() |>
  add_recipe(car_recipe_features) |>
  add_model(lm_spec) 

final_fit = wf_final |>
  last_fit(cars_tt_split)

final_fit |> collect_metrics() |>
  select(.metric, .estimate) |>
  rename('Data_test'= .estimate, 'Metric' = .metric ) |>
  bind_cols(df_test_metrics) |>
  kbl() |>
  kable_material(c("striped", "hover"))



predicts_int = features_fit |>
  predict(car_recipe_features |> prep() |> bake(new_data = cars_test), 
          type = "conf_int",
          level = 0.95)

predicts_estimate = features_fit |>
  predict(car_recipe_features |> prep() |> bake(new_data = cars_test))


predicts_df = bind_cols(car_recipe_features |> prep() |> bake(new_data = cars_test), predicts_int, predicts_estimate)




predicts_int = features_fit |>
  predict(car_recipe_features |> prep() |> bake(new_data = cars_test), 
          type = "conf_int",
          level = 0.99999999)

predicts_estimate = features_fit |>
  predict(car_recipe_features |> prep() |> bake(new_data = cars_test))


predicts_df = bind_cols(car_recipe_features |> prep() |> bake(new_data = cars_test), predicts_int, predicts_estimate)




ggplot(predicts_df, aes(x = c(1:nrow(predicts_df)))) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper), fill = "blue", alpha = 0.3) +
  geom_line(aes(y = .pred), color = "blue", linetype = "dashed", alpha = 0.6 ) +
  geom_point(aes(y = current_price), color = "red") +
  labs(x = "X", y = "Y") 



set.seed(123)

predicts_df_sample = predicts_df |>
  sample_n(30)

ggplot(predicts_df_sample, aes(x = c(1:nrow(predicts_df_sample)))) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper), fill = "blue", alpha = 0.3) +
  geom_line(aes(y = .pred), color = "blue", linetype = "dashed", alpha = 0.6 ) +
  geom_point(aes(y = current_price), color = "red") +
  labs(x = "X", y = "Y") 

