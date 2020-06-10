plot_ranf_species_presentation <- function(fig_random_effects_data,
                                           p1_file,
                                           p2_file,
                                           p3_file){
  attach(fig_random_effects_data)

  suppressPackageStartupMessages({
    require(ggplot2)
    # require(tidybayes)
    require(ggforce)
    require(showtext)
  })

  showtext_auto()

  pal <- fig_metric("red_shade")[c(8,6)]

  data <- random_species_draws %>%
    dplyr::ungroup() %>%
    dplyr::mutate(guild = translate_guild(guild)) %>%
    dplyr::left_join(random_sp_names, by = "org_id") %>%
    dplyr::filter(guild == "animals") %>%
    dplyr::filter(!is.na(sp_name))

  p <- data %>%
    ggplot(aes(x = var, y = .value, colour = guild)) +
    geom_line(aes(group = org_id, alpha = sp_name),
              stat = "summary",
              fun.y = "mean",
              colour = "#9F2B35",
              size = 1) +
    coord_cartesian(expand = T) +
    defense_theme() +
    theme(axis.ticks = element_blank()) +
    labs(y = "# partners →",
         x = "environmental stress →",
         title = "",
         subtitle = "",
         caption = "")

  p1 <- p +
    scale_alpha_manual(values = c(1,1))

  p2 <- p +
    scale_alpha_manual(values = c(1,0.3))

  p3 <- p +
    scale_alpha_manual(values = c(0.3,1))

  plot_width <- 10
  width <- unit(plot_width, "cm")
  height <- unit(plot_width * 10 / 16, "cm")
  ggsave(p1_file, p1, width = width, height = height)
  ggsave(p2_file, p2, width = width, height = height)
  ggsave(p3_file, p3, width = width, height = height)

  showtext_auto(FALSE)
}

plot_ranf_correlation_presentation <- function(fig_random_effects_data,
                                               p1_file,
                                               p2_file){

  attach(fig_random_effects_data)

  suppressPackageStartupMessages({
    require(ggplot2)
    # require(tidybayes)
    require(ggforce)
    require(showtext)
  })

  showtext_auto()
  pal <- fig_metric("red_shade")[c(8,6)]

  mean_correlation <- random_correlation_posterior %>%
    dplyr::summarise_all(mean) %$%
    correlation

  correlation_plot <- random_correlation_posterior %>%
    ggplot(aes(x = correlation)) +
    # geom_density(fill = fig_metric("red_shade")[1], colour = NA) +
    stat_density(geom = "line", colour = "#9F2B35", size = 1) +
    geom_vline(xintercept = mean_correlation, size = 0.25, linetype = 2) +
    # coord_cartesian(expand = F) +
    base_ggplot_theme() +
    theme(text = element_text(family = "iwona"),
          axis.title = element_text(family = "iwonalight", size = 20),
          plot.title = element_text(size = 25, hjust = 0.5),
          plot.subtitle = element_text(size = 20, hjust = 0.5),
          plot.caption = element_text(size = 14, family = "iwonalight"),
          # panel.border = element_blank(),
          # axis.text = element_blank(),
          # axis.ticks = element_blank(),
          legend.position = "none",
          plot.margin = unit(rep(0.5, 4), "cm"), panel.border = element_blank(),
          # axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.x.bottom = element_line()) +
    labs(title = "",
         subtitle = "",
         caption = "",
         y = "",
         x= "correlation coefficient")

    plot_width <- 10
  width <- unit(plot_width, "cm")
  height <- unit(plot_width * 10 / 16, "cm")
  ggsave(p2_file, correlation_plot, width = width, height = height)

  slope_intercept_plot <- random_slope_intercepts %>%
    dplyr::left_join(random_sp_names, by = "org_id") %>%
    dplyr::mutate(mark = !is.na(sp_name)) %>%
    ggplot(mapping = aes(#colour = guild,
                         # fill = guild,
                         x = Intercept_Estimate,
                         y = scaled_suitability_Estimate)) +
    geom_hline(yintercept = 0, linetype = 2, size = 0.25) +
    geom_vline(xintercept = 0, linetype = 2, size = 0.25) +
    geom_point(shape = 21, stroke = 0.25, size = 2, colour = "black", fill = "#9F2B35") +
    scale_fill_manual(values = pal, aesthetics = c("fill", "colour")) +
    coord_cartesian(expand = TRUE) +
    base_ggplot_theme() +
    theme(text = element_text(family = "iwona"),
          axis.title = element_text(family = "iwonalight", size = 20),
          plot.title = element_text(size = 25, hjust = 0.5),
          plot.subtitle = element_text(size = 20, hjust = 0.5),
          plot.caption = element_text(size = 14, family = "iwonalight"),
          # panel.border = element_blank(),
          # axis.text = element_blank(),
          # axis.ticks = element_blank(),
          legend.position = "none",
          plot.margin = unit(rep(0.5, 4), "cm")) +
    theme(legend.position = "none") +
    labs(x = "species' intercept",
         y = "species' slope",
         title = "",
         subtitle = "",
         caption = "")

  ggsave(p1_file, slope_intercept_plot, width = width, height = height)

  showtext_auto(FALSE)
}
