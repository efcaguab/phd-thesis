make_fig_structural_stability <- function(critical_sp_df){
  require(ggplot2)

  st_df <- critical_sp_df

  means <- st_df %>%
    dplyr::group_by(critical) %>%
    dplyr::summarise_if(is.numeric, mean, na.rm = T)

  value <- 1
  my.labs <- list(bquote(paste("critical species (", phi ==.(value), ")")),
                  bquote(paste("redundant (", phi<.(value), ")")))

  p1 <- st_df %>%
    ggplot(aes(x = value, colour = critical)) +
    stat_density(geom = "line", position = "identity",
                 size = 0.5) +
    geom_vline(data = means,
               aes(xintercept = value, colour = critical),
               linetype = 2,
               size = 0.25,
               show.legend = F) +
    scale_color_manual(values = c(my_pallete()$dark_grey,
                                  my_pallete()$light_grey),
                       name = "",
                       labels = my.labs) +
    base_ggplot_theme() +
    theme(legend.position = c(0, 1.15),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = "NA"),
          legend.key.size = unit(0.15, "in")) +
          panel.border = element_blank(),
          axis.line.x = element_line(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank()) +
    labs(title = "structural stability of species removal",
         subtitle = "probability density",
         y = "probability density",
         x = "scaled structural stability")

  list(p1)

}
