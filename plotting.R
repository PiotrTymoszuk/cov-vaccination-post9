# This script generates specific plots with the summary modeling stats

# container list -----

  plots <- list()
  
# Plots of cases and fatalities for single countries ------
    
  plots$country_plots <- list(case_plots_train = list(prediction_table = mod$pred_train_cases, 
                                                      plot_title = paste(names(mod$pred_train_cases), '2020'), 
                                                      plot_subtitle =  mod$errors$pred_train_cases$plot_label, 
                                                      bar_fill = 'indianred3', 
                                                      y_lab = 'Cases per million'), 
                              fatal_plots_train = list(prediction_table = mod$pred_train_fatal, 
                                                       plot_title = paste(names(mod$pred_train_fatal), '2020'), 
                                                       plot_subtitle =  mod$errors$pred_train_fatal$plot_label, 
                                                       bar_fill = 'gray70', 
                                                       y_lab = 'Deaths per million'), 
                              case_plots_test = list(prediction_table = mod$pred_test_cases, 
                                                     plot_title = paste(names(mod$pred_test_cases), '2021'), 
                                                     plot_subtitle =  mod$errors$pred_test_cases$plot_label, 
                                                     bar_fill = 'indianred3', 
                                                     y_lab = 'Cases per million'), 
                              fatal_plots_test = list(prediction_table = mod$pred_test_fatal, 
                                                      plot_title = paste(names(mod$pred_test_fatal), '2021'), 
                                                      plot_subtitle =  mod$errors$pred_test_fatal$plot_label, 
                                                      bar_fill = 'gray70', 
                                                      y_lab = 'Deaths per million')) %>% 
    map(~pmap(.x, plot_predictions, bar_alpha = 0.5, 
              algorithms = 'rf') %>% 
          map(~.x + scale_color_manual(values = mod$method_colors, 
                                       labels = mod$method_labels, 
                                       name = '7-day average')))
  
# Plotting residuals for each country -----
  
  plots$country_resid <- list(case_plots_test = list(prediction_table = mod$pred_test_cases, 
                                                     plot_title = paste(names(mod$pred_test_cases), '2021'), 
                                                     plot_subtitle = 'Cases rescued per day', 
                                                     y_lab = 'Cases per million'), 
                              fatal_plots_test = list(prediction_table = mod$pred_test_fatal, 
                                                      plot_title = paste(names(mod$pred_test_fatal), '2021'), 
                                                      plot_subtitle =  mod$errors$pred_test_fatal$plot_label, 
                                                      y_lab = 'Deaths rescued per day')) %>% 
    map(~pmap(.x, plot_resids, 
              bar_alpha = 0.5, 
              algorithms = 'rf') %>% 
          map(~.x + 
                scale_color_manual(values = unname(mod$method_colors['rf']), 
                                   labels = unname(mod$method_labels['rf']), 
                                   name = '7-day average'))) %>% 
    map2(., c('indianred3', 'gray70'), 
         function(response, color) map(response, ~.x + 
                                         scale_fill_manual(values = color) + 
                                         guides(fill = FALSE)))
  
# plotting tables with percent vaccinated and errors -----
  
  plots$vacc <- cov_data$data %>% 
    mutate(country = location) %>% 
    group_by(country) %>% 
    summarise(perc_vacc = max(people_fully_vaccinated_per_hundred, na.rm = TRUE), 
              iso_code = iso_code[1])
  
  plots$errors <- mod$errors[c('pred_test_cases', 
                               'pred_test_fatal')] %>% 
    map(~left_join(.x, plots$vacc, by = 'country'))
  
  plots$errors_delta <- mod$errors_delta[c('pred_test_cases', 
                                           'pred_test_fatal')] %>% 
    map(~left_join(.x, plots$vacc, by = 'country'))
  
# plotting the validation errors for the countries in 2021 -----

  plots$saved[c('cases_cume', 'fatal_cume')] <- list(error_table = plots$errors, 
                                                     plot_title = c('SARS-CoV-2 cases rescued in 2021', 
                                                                    'COVID-19 deaths rescued in 2021')) %>% 
    pmap(plot_error, 
         err_var = 'cue', 
         x_lab = 'Cummulative residuals', 
         plot_subtitle = 'per million inhabitants') %>% 
    map(~.x + scale_fill_manual(values = c('steelblue', 'indianred4')))
  
  plots$saved[c('cases_cume_delta', 'fatal_cume_delta')] <- list(error_table = plots$errors_delta, 
                                                                 plot_title = c('SARS-CoV-2 cases rescued from 07.2021', 
                                                                                'COVID-19 deaths rescued from 07.2021')) %>% 
    pmap(plot_error, 
         err_var = 'cue', 
         x_lab = 'Cummulative residuals', 
         plot_subtitle = 'per million inhabitants') %>% 
    map(~.x + scale_fill_manual(values = c('steelblue', 'indianred4')))
  
# Plotting correlations of percent vaccinated with the cases and lives saved from day 180 on ----
  
  plots$saved_vacc[c('cases_cume_delta', 'fatal_cume_delta')]  <- list(data = plots$errors_delta, 
                                                                       x_lab = c('Cases per million rescued', 
                                                                                 'Cases per million rescued'), 
                                                                       plot_title = c('SARS-CoV-2 cases rescued from 07.2021', 
                                                                                      'COVID-19 deaths rescued from 07.2021')) %>% 
    pmap(plot_point, 
         x_var = 'cue', 
         y_var = 'perc_vacc', 
         y_lab = '% fully vaccinated population', 
         plot_subtitle = 'Second half 2021') %>% 
    map(~.x + guides(fill = FALSE))
  
# Choropleth maps with the percents of vaccinated, cases and lives saved from day 180 on ------
  
  ## plotting tables
  
  plots$europe_map <- map_data('world') %>% 
    mutate(country = stri_replace(region, fixed = 'UK', replacement = 'United Kingdom'), 
           country = stri_replace(country, fixed = 'Czech Republic', replacement = 'Czechia'), 
           country = stri_replace(country, fixed = 'Macedonia', replacement = 'North Macedonia')) %>% 
    filter(country %in% cov_data$countries, 
           long < 45, 
           lat < 72)
  
  plots$europe_map <- plots$errors_delta %>% 
    map(~left_join(plots$europe_map, .x, by = 'country')) %>% 
    map(mutate, 
        vacc_fct = cut(perc_vacc, 
                       c(-Inf, 30, 50, 60, 70, 80, 90, 100), 
                       c('0 - 30', '31 - 50', '51 - 60', '61 - 70', '71 - 80', '81 - 90', '91 - 100'))) %>% 
    map(as_tibble)
  
  ## plots

  plots$saved_choro <- list(fill_var = c('vacc_fct', 'cue', 'cue'), 
                                  plot_title = c('Vaccination rate', 
                                                 'SARS-CoV-2 cases rescued from 07.2021', 
                                                 'COVID-19 deaths rescued from 07.2021'), 
                            map_table = plots$europe_map[c('pred_test_cases', 
                                                           'pred_test_cases', 
                                                           'pred_test_fatal')]) %>% 
    pmap(plot_choro) %>% 
    map(~.x + coord_map('gilbert')) %>% 
    set_names(c('vacc', 'cases', 'fatal'))
  
  ## adjustments
  
  plots$saved_choro$vacc <- plots$saved_choro$vacc + 
    scale_fill_manual(values = c('steelblue4', 
                                 'lightblue2', 
                                 'white', 
                                 'cornsilk', 
                                 'indianred2', 
                                 'indianred4'), 
                      name = '% population')
  
  plots$saved_choro[c('cases', 'fatal')] <- plots$saved_choro[c('cases', 'fatal')] %>% 
    map(~.x + 
          scale_fill_gradient2(low = 'steelblue4', 
                               mid = 'white', 
                               high = 'indianred3', 
                               midpoint = 0, 
                               name = 'rescued per million'))
  
# END ----