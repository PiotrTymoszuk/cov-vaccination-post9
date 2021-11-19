# Figures for the blog post

# container list ----

  figures <- list()

# Figure 1: model training and predictions in Austria -----
  
  figures$figure_austria <- plots$country_plots[c('case_plots_train', 
                                                  'case_plots_test', 
                                                  'fatal_plots_train', 
                                                  'fatal_plots_test')] %>% 
    map(~.x$Austria + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              labels = c('A', '', 'B'), 
              label_size = 10) %>% 
    plot_grid(get_legend(plots$country_plots[[1]][[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.92, 0.08)) %>% 
    as_figure_object(figure_label = 'figure_1_austria', 
                     w = 180, 
                     h = 160)
  
# Figure 2 model training and prediction in Poland -----
  
  figures$figure_poland <- plots$country_plots[c('case_plots_train', 
                                                 'case_plots_test', 
                                                 'fatal_plots_train', 
                                                 'fatal_plots_test')] %>% 
    map(~.x$Poland + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              labels = c('A', '', 'B'), 
              label_size = 10) %>% 
    plot_grid(get_legend(plots$country_plots[[1]][[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.92, 0.08)) %>% 
    as_figure_object(figure_label = 'figure_2_poland', 
                     w = 180, 
                     h = 160)
  
# Figure 3: cases and deaths rescued -----
  
  figures$rescue_2021 <- plot_grid(plots$saved$cases_cume, 
                                   plots$saved$fatal_cume, 
                                   ncol = 2, 
                                   align = 'hv', 
                                   labels = LETTERS, 
                                   label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_3_rescue_2021', 
                     w = 180, 
                     h = 180)
  
# Figure 4: cases and deaths rescued from day 180 on -----
  
  figures$rescue_2021 <- plot_grid(plots$saved$cases_cume_delta, 
                                   plots$saved$fatal_cume_delta, 
                                   ncol = 2, 
                                   align = 'hv', 
                                   labels = LETTERS, 
                                   label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_4_rescue_delta', 
                     w = 180, 
                     h = 180)
  
# Figure 5: vaccination, cases and fatalities rescued in a choropleth map ----
  
  figures$rescue_map <- plots$saved_choro %>% 
    plot_grid(plotlist = ., 
              nrow = 3, 
              align = 'hv', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_5_rescue_maps', 
                     h = 280, 
                     w = 200)
  
# Figure 6: vaccination and cases, deaths rescued ----
  
  figures$corr <- plots$saved_vacc %>% 
    plot_grid(plotlist = ., 
              nrow = 2, 
              align = 'hv', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_6_rescue_points', 
                     w = 180, 
                     h = 180)
  
# Saving -----
  
  figures %>%
    walk(save_figure_object, 
         format = 'png')

