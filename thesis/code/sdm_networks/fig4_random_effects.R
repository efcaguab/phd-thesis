plot_ranf <- function(random_species_draws,
                      random_correlation_posterior,
                      random_sp_names,
                      random_slope_intercepts,
                      mean_suitability){

  suppressPackageStartupMessages({
    require(ggplot2)
    require(tidybayes)
    require(ggforce)
  })

  pal <- cgm()$pal_el_green[c(8,6)]

  conditional_effects_plot <- random_species_draws %>%
    dplyr::ungroup() %>%
    dplyr::mutate(guild = translate_guild(guild)) %>%
    dplyr::left_join(random_sp_names, by = "org_id") %>%
    ggplot(aes(x = var, y = .value, colour = guild)) +
    geom_line(aes(group = org_id, alpha = highlight, size = highlight), stat = "summary",fun.y = "mean") +
    geom_vline(xintercept = mean_suitability, size = 0.25, linetype = 2) +
    geom_mark_circle(aes(group = org_id,
                         filter = highlight & mark,
                         label = sp_name),
                     expand = unit(0, "mm"),
                     alpha = 0.1,
                     # concavity = 0,
                     colour = "transparent",
                     fill = "grey",
                     label.fontsize = 7,
                     label.buffer = unit(5, "mm"),
                     label.minwidth = unit(10, "mm"),
                     label.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
                     label.fontface = "italic",
                     con.size = 0.25,
                     con.cap = unit(0.1, "mm"),
                     con.type = "straight") +
    facet_wrap(~guild, ncol = 1) +
    scale_alpha_manual(values = c(0.5, 1)) +
    scale_size_manual(values = c(0.25, 0.5)) +
    scale_fill_manual(values = pal, aesthetics = c("fill", "colour"),
                      labels = c(" env. space based on all spp. occurrences",
                                 " env. space based on each spp. occurrences")) +
    pub_theme() +
    coord_cartesian(expand = F) +
    theme(legend.position = "none") +
    labs(y = "# partners",
         x = "environmental stress",
         title = "(a) effect of stress on individual species")

  mean_correlation <- random_correlation_posterior %>%
    dplyr::summarise_all(mean) %$%
    correlation



  plot_left <- conditional_effects_plot



  p <- cowplot::plot_grid(plot_left,
                          plot_right,
                          ncol = 2)
  #  ggsave("plot.pdf", p, width = unit(width("double"), "in"), height = unit(2.2*2, "in"))

  p
}


plot_correlations <- function(){


  correlation_plot <- random_correlation_posterior %>%
    ggplot(aes(x = correlation)) +
    geom_density(fill = cgm()$pal_el_green[1], colour = NA) +
    stat_density(geom = "line", colour = cgm()$pal_el_green[9], size = 0.25) +
    geom_vline(xintercept = mean_correlation, size = 0.25, linetype = 2) +
    coord_cartesian(expand = FALSE) +
    pub_theme() +
    theme(panel.border = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.x.bottom = element_line(),
          plot.title = element_text(margin = margin(b = 4))) +
    labs(title = "(c) correlation coefficient")


  slope_intercept_plot <- random_slope_intercepts %>%
    dplyr::left_join(random_sp_names, by = "org_id") %>%
    dplyr::mutate(mark = !is.na(sp_name)) %>%
    ggplot(mapping = aes(colour = guild,
                         #fill = guild,
                         x = Intercept_Estimate,
                         y = scaled_suitability_Estimate)) +
    geom_hline(yintercept = 0, linetype = 2, size = 0.25) +
    geom_vline(xintercept = 0, linetype = 2, size = 0.25) +
    geom_point(shape = 21, stroke = 0.25, size = 1) +
    #     geom_mark_circle(aes(group = org_id,
    #                          filter = mark,
    #                           label = sp_name),
    #                      expand = unit(0, "mm"),
    #                      alpha = 0.1,
    #     concavity = 0,
    #                       colour = "transparent",
    #                       fill = "grey",
    #                       label.fontsize = 7,
    #                       label.buffer = unit(5, "mm"),
    #                       label.minwidth = unit(10, "mm"),
  #                       label.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
  #                       label.fontface = "italic",
  #                       con.size = 0.25,
  #                       con.cap = unit(0.1, "mm"),
  #                       con.type = "straight") +
  #     geom_errorbar(mapping = aes(ymin = scaled_suitability_Q25,
  #                                 ymax = scaled_suitability_Q75),
  #                   size = 0.15, alpha = 0.25) +
  #     geom_errorbarh(mapping = aes(xmin = Intercept_Q25,
  #                              xmax = Intercept_Q75),
  #                size = 0.15, alpha = 0.25) +
  scale_fill_manual(values = pal, aesthetics = c("fill", "colour")) +
    coord_cartesian(expand = TRUE) +
    pub_theme() +
    theme(legend.position = "none") +
    labs(x = "species' intercept",
         y = "species' slope",
         title = "(b) species' specific intercept and slope",
         subtitle = "all values in parameter space")

  plot_right <- cowplot::plot_grid(slope_intercept_plot,
                                   correlation_plot,
                                   ncol = 1,
                                   rel_heights = c(3, 1),
                                   axis = "lr",
                                   align = "v")

}
