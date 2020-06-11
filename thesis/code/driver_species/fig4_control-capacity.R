make_fig_control_capacity <- function(sl_characteristics, metadata){
  require(ggplot2)


  p5 <- sl_characteristics %>%
    filter_networks_df(metadata) %>%
    dplyr::filter(guild == "pla") %>%
    ggplot(aes(x = control_capacity, colour = guild)) +
    stat_density(geom = "line",
                 bw = "SJ",
                 show.legend = FALSE) +
    geom_point(aes(x = 1, y = 0),
               fill = fig_metric("red_shade")[4],
               colour = "black",
               shape = 21,
               size = 1) +
    scale_color_manual(values = fig_metric("red_shade")[4]) +
    base_ggplot_theme() +
    labs(title = "(a) control capacity of plants",
         subtitle = "probability density",
         x = latex2exp::TeX("control capacity ($\\phi$)"),
         y = "Probability density") +
    scale_y_continuous(limits = c(0,9)) +
    theme(legend.position = c(1, 0),
          legend.justification = c(1,0),
          legend.background = element_rect(fill = "NA"),
          legend.key.size = unit(0.15, "in"),
          panel.border = element_blank(),
          axis.line.x = element_line(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank())

  p6 <- sl_characteristics %>%
    filter_networks_df(metadata) %>%
    dplyr::filter(guild == "pol") %>%
    ggplot(aes(x = control_capacity, colour = guild)) +
    stat_density(geom = "line",
                 bw = "SJ",
                 show.legend = FALSE) +
    scale_color_manual(values = fig_metric("red_shade")[6]) +
    base_ggplot_theme() +
    labs(title = "(b) control capacity of pollinators",
         subtitle = "probability density",
         x = latex2exp::TeX("control capacity ($\\phi$)"),
         y = "Probability density") +
    scale_y_continuous(limits = c(0,9)) +
    theme(legend.position = c(1, 0),
          legend.justification = c(1,0),
          legend.background = element_rect(fill = "NA"),
          legend.key.size = unit(0.15, "in"),
          panel.border = element_blank(),
          axis.line.x = element_line(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank())

  list(p5, p6)
}

filter_networks_df <- function(x, metadata, desired_ntw = c("bartomeus", "lopezaraiza")){
  m <- dplyr::select(metadata, net_name, study)
  x %>%
    dplyr::inner_join(m, by = "net_name") %>%
    dplyr::filter(study %in% desired_ntw) %>%
    dplyr::select(-study)
}
