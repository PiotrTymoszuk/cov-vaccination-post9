# Functions for the blog post project

# tools ----

  require(plyr)
  require(tidyverse)
  require(rlang)
  require(forecast)
  require(ggrepel)
  require(maps)

  map <- purrr::map

# globals -----

  globals <- list()

  globals$common_text <- element_text(size = 8, face = 'plain', color = 'black')
  
  globals$common_margin <- ggplot2::margin(t = 4, l = 3, r = 2, unit = 'mm')
  
  globals$common_theme <- theme_classic() + theme(axis.text = globals$common_text, 
                                                  axis.title = globals$common_text, 
                                                  plot.title = element_text(size = 8, 
                                                                            face = 'bold', 
                                                                            color = 'black', 
                                                                            hjust = 0), 
                                                  plot.subtitle = globals$common_text, 
                                                  plot.tag = element_text(size = 8, 
                                                                          face = 'plain', 
                                                                          color = 'black', 
                                                                          hjust = 0), 
                                                  plot.tag.position = 'bottom', 
                                                  legend.text = globals$common_text, 
                                                  legend.title = globals$common_text, 
                                                  strip.text = globals$common_text,
                                                  strip.background = element_rect(fill = 'gray95', color = 'gray80'), 
                                                  plot.margin = globals$common_margin, 
                                                  panel.grid.major = element_line(color = 'gray90'))
  
# functions ----

  add_lag <- function(vector, lag = 1) {
    
    ## adds a lag to a numeric vector
    
    stopifnot(is.numeric(vector))
    
    lags <- length(vector):(length(vector) - lag)
    
    c(rep(NA, lag + 1), vector[-lags])
    
  }
  
  add_lag_var <- function(data, src_variable, lags, complete = TRUE) {
    
    ## adds lag variables to the table
    ## can filter out incomplete scr_variable and the derived lag variables
    
    stopifnot(is.integer(lags))

    lag_names <- paste(src_variable, 'lag', lags, sep = '_')
    
    lag_tbl <- lags %>% 
      map(add_lag, 
              vector = data[[src_variable]]) %>% 
      set_names(lag_names)
    
    new_tbl <- cbind(data, lag_tbl) %>% 
      as_tibble
    
    if(complete) {
      
      for(i in lag_names) {
        
        new_tbl <- new_tbl %>% 
          filter(!is.na(.data[[i]]))
        
      }

    }
    
    new_tbl
    
  }
  
  get_predictions <- function(caret_model, new_data = NULL) {
    
    ## calculates the model predictions and residuals
    
    if(is.null(new_data)) {
      
      predict(caret_model)
      
    } else {
      
      predictions <- predict(caret_model, newdata = new_data)
      
      mod_response <- as.character(eval(caret_model$call$form))[2]
      
      tibble(day = new_data[['day']], 
             outcome = new_data[[mod_response]], 
             predicted = predictions) %>% 
        mutate(resid = predicted - outcome)
      
    }
    
  }
  
  compact_predictions <- function(pred_list) {
    
    cbind(pred_list[[1]], 
          pred_list[[2]][3:4])%>% 
      set_names(c('day', 
                  'outcome', 
                  'rf', 
                  'rf_resid', 
                  'svmPoly', 
                  'svmPoly_resid')) %>% 
      as_tibble
    
  }
  
  get_errors <- function(prediction_table, 
                         algorithms = mod$methods) {
    
    ## calculated mean absolute and cumulative error
    
    vars <- paste(algorithms, 'resid', sep = '_')
    
    vars %>% 
      map_dfr(~tibble(mae = mean(abs(prediction_table[[.x]]), na.rm = TRUE), 
                      cue = sum(prediction_table[[.x]], na.rm = TRUE))) %>% 
      mutate(algorithm = algorithms)
    
  }
  
  plot_predictions <- function(prediction_table, 
                               algorithms = mod$methods, 
                               y_lab = 'outcome', 
                               plot_title = NULL, 
                               plot_subtitle = NULL, 
                               plot_tag = NULL, 
                               bar_fill = 'cornsilk2', 
                               bar_alpha = 0.75, 
                               order = 7, 
                               line_size = 0.75) {
    
    ## plots predictions and the true outcome
    
    prediction_table <- prediction_table %>% 
      filter(outcome >= 0) ## For Spain there was a correction of the count
    
    smooth_tbl <- prediction_table %>% ## moving average
      gather(key = 'response', 
             value = 'value', 
             outcome, 
             all_of(algorithms)) %>% 
      group_by(response) %>% 
      mutate(m_avg = ma(value, order = order, centre = FALSE)) %>% 
      ungroup
    
    prediction_table %>% 
      ggplot(aes(x = day, 
                 y = outcome)) + 
      geom_bar(stat = 'identity', 
               fill = bar_fill, 
               alpha = bar_alpha) + 
      geom_line(data = smooth_tbl, 
                aes(x = day, 
                    y = m_avg, 
                    color = response), 
                size = line_size) + 
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = 'Day of the year', 
           y = y_lab)
    
  }
  
  plot_error <- function(error_table, 
                         err_var = 'mae', 
                         x_lab = 'MAE', 
                         plot_title = NULL, 
                         plot_subtitle = NULL, 
                         plot_tag = NULL) {
    
    
    error_table %>% 
      mutate(country = stri_replace(country, 
                                    fixed = 'Bosnia and Herzegovina', 
                                    replacement = 'BiH'), 
             country = stri_replace(country, 
                                    fixed = 'United Kingdom', 
                                    replacement = 'UK')) %>% 
      ggplot(aes(x = .data[[err_var]], 
                 y = reorder(country, .data[[err_var]]), 
                 fill = factor(sign(.data[[err_var]])))) + 
      geom_bar(stat = 'identity', 
               color = 'black') + 
      guides (fill = FALSE) + 
      globals$common_theme + 
      theme(axis.title.y = element_blank()) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = x_lab)
    
  }
  
  plot_point <- function(data, 
                         x_var, 
                         y_var, 
                         fill_var = x_var, 
                         land_label = 'iso_code', 
                         smooth = TRUE, 
                         plot_title = NULL, 
                         plot_subtitle = NULL, 
                         plot_tag = NULL, 
                         x_lab = x_var, 
                         y_lab = y_var, ...) {
    pplot <- data %>% 
      ggplot(aes(x = .data[[x_var]], 
                 y = .data[[y_var]], 
                 fill = .data[[fill_var]])) + 
      geom_vline(xintercept = 0, 
                 linetype = 'dashed') + 
      geom_point(shape = 21, 
                 size = 2) + 
      geom_text_repel(aes(label = iso_code), 
                      size = 2.75) +  
      scale_fill_gradient2(low = 'steelblue', 
                           high = 'indianred4', 
                           mid = 'white', 
                           midpoint = 0) +
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = x_lab, 
           y = y_lab)
    
    if(smooth) {
      
      pplot <- pplot + 
        geom_smooth(method = 'loess', se = FALSE)
      
    }
    
    return(pplot)
  }
  
  plot_resids <-  function(prediction_table, 
                           algorithms = mod$methods, 
                           y_lab = 'outcome', 
                           plot_title = NULL, 
                           plot_subtitle = NULL, 
                           plot_tag = NULL, 
                           bar_alpha = 0.75, 
                           order = 7, 
                           line_size = 0.75) {
    
    ## plots residuals in time
    
    prediction_table <- prediction_table %>% 
      filter(outcome >= 0) ## For Spain there was a correction of the count
    
    smooth_tbl <- prediction_table %>% ## moving average
      gather(key = 'response', 
             value = 'value', 
             all_of(paste(algorithms, 'resid', sep = '_'))) %>% 
      group_by(response) %>% 
      mutate(m_avg = ma(value, order = order, centre = FALSE)) %>% 
      ungroup
    
    smooth_tbl %>% 
      ggplot(aes(x = day, 
                 y = value, 
                 fill = response)) + 
      geom_bar(stat = 'identity', 
               alpha = bar_alpha) + 
      geom_line(aes(x = day, 
                    y = m_avg, 
                    color = response), 
                size = line_size) + 
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = 'Day of the year', 
           y = y_lab)
    
  }
  
  plot_choro <- function(map_table, 
                         fill_var, 
                         plot_title = NULL, 
                         plot_subtitle = NULL, 
                         plot_tag = NULL, 
                         x_lab = 'Longitude', 
                         y_lab = 'Latitude') {
    
    map_table %>% 
      ggplot(aes(x = long, 
                 y = lat, 
                 group = group, 
                 fill = .data[[fill_var]])) + 
      geom_polygon(color = 'gray40') + 
      globals$common_theme + 
      theme(axis.line = element_blank()) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = x_lab, 
           y = y_lab)
    
  }
  
