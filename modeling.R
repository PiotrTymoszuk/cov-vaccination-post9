# This script models daily incidence and mortality data based on the counts from previous days (infection effect) 
# and date (seasonal effect). I'm tinkering with 3 Swiss army knifes of machine learning: neural networks
# random forests and support vector machines

# tools ----

  library(doParallel)
  library(caret)
  library(furrr)

# container list ----

  mod <- list()
  
# modeling tables: the training (2020) and test sets (2021) ----
  
  mod$data <- cov_data$data %>% 
    dlply(.(location), function(x) dlply(x, .(year))) %>% 
    transpose %>% 
    set_names(c('train', 'test')) %>% 
    map(~map(.x, as_tibble))
  
# train control object and modeling formula -----
  
  mod$control$rf <- trainControl(method = 'cv', 
                                 number = 5, 
                                 seeds = list(rep(1234, 3), 
                                              rep(1234, 3), 
                                              rep(1234, 3), 
                                              rep(1234, 3), 
                                              rep(1234, 3), 
                                              7))
  
  mod$control$svmPoly <- trainControl(method = 'cv', 
                                 number = 5, 
                                 seeds = list(rep(1234, 27), 
                                              rep(1234, 27), 
                                              rep(1234, 27), 
                                              rep(1234, 27), 
                                              rep(1234, 27), 
                                              7))
  
  mod$formula <- cov_data$responses %>% 
    map(~paste(.x, paste(cov_data$indep_vars , collapse = '+'), sep = '~')) %>% 
    map(as.formula)
  
# model types -----
  
  mod$methods <- c('rf', 'svmPoly')
  
  mod$method_colors <- c(outcome = 'coral4', 
                         rf = 'steelblue', 
                         svmPoly = 'gray50')
  
  mod$method_labels <- c(outcome = 'Outcome', 
                         rf = 'RF', 
                         svmPoly = 'SVM')
  
# training the case and mortality models -----

  plan('multisession')
  
  mod[paste(mod$methods, 'cases', sep = '_')] <- mod$methods %>% 
    map(function(alg)  mod$data$train %>% 
          future_map(function(country) train(mod$formula$cases,
                                             data = country, 
                                             method = alg, 
                                             trControl = mod$control[[alg]], 
                                             trace = FALSE), 
                     .options = furrr_options(seed = 1234)))
  
  mod[paste(mod$methods, 'fatal', sep = '_')] <- mod$methods %>% 
    map(function(alg)  mod$data$train %>% 
          future_map(function(country) train(mod$formula$fatal,
                                             data = country, 
                                             method = alg, 
                                             trControl = mod$control[[alg]], 
                                             trace = FALSE), 
                     .options = furrr_options(seed = 1234)))

  plan('sequential')
  
# getting the redistribution (2020) residuals, compacting into tibbles -----
  
  mod$pred_train_cases <- mod[paste(mod$methods, 'cases', sep = '_')] %>% 
    map(function(alg) list(caret_model = alg, 
                           new_data = mod$data$train) %>% 
          pmap(get_predictions)) %>% 
    transpose %>% 
    map(compact_predictions)
  
  mod$pred_train_fatal <- mod[paste(mod$methods, 'fatal', sep = '_')] %>% 
    map(function(alg) list(caret_model = alg, 
                           new_data = mod$data$train) %>% 
          pmap(get_predictions)) %>% 
    transpose %>% 
    map(compact_predictions)
  
# getting the test set (2021) predictions and residuals, compacting into tibbles -----
  
  mod$pred_test_cases <- mod[paste(mod$methods, 'cases', sep = '_')] %>% 
    map(function(alg) list(caret_model = alg, 
                           new_data = mod$data$test) %>% 
          pmap(get_predictions)) %>% 
    transpose %>% 
    map(compact_predictions)
  
  mod$pred_test_fatal <- mod[paste(mod$methods, 'fatal', sep = '_')] %>% 
    map(function(alg) list(caret_model = alg, 
                           new_data = mod$data$test) %>% 
          pmap(get_predictions)) %>% 
    transpose %>% 
    map(compact_predictions)

# obtaining the redistribution and validation MAE and sum error, needed for calculation of the cases and lives saved -----

  mod$errors <- mod[c('pred_train_cases', 
                      'pred_train_fatal', 
                      'pred_test_cases', 
                      'pred_test_fatal')] %>% 
    map(~map(.x, get_errors, algorithms = 'rf') %>% 
          map2_dfr(., names(.), ~mutate(.x, country = .y)) %>% 
          mutate(plot_label = paste0('MAE = ', signif(mae, 2), ', CumE = ', signif(cue, 2))))
  
# obtaining the redistribution and validation MAE and sum error, needed for calculation of the cases and lives saved from day 180 -----
  
  mod$errors_delta <- mod[c('pred_test_cases', 
                            'pred_test_fatal')] %>% 
    map(~map(.x, filter, day > 180)) %>% 
    map(~map(.x, get_errors, algorithms = 'rf') %>% 
          map2_dfr(., names(.), ~mutate(.x, country = .y)) %>% 
          mutate(plot_label = paste0('MAE = ', signif(mae, 2), ', CumE = ', signif(cue, 2))))
  
# END ----