make_fig_structural_stability <- function(critical_sp_df){
  require(ggplot2)

  st_df <- critical_sp_df

  means <- st_df %>%
    dplyr::group_by(critical) %>%
    dplyr::summarise_if(is.numeric, mean, na.rm = T)

  value <- 1
  my.labs <- list(bquote(paste("Critical species (", phi ==.(value), ")")),
                  bquote(paste("Redundant (", phi<.(value), ")")))

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
    labs(y = "Probability density",
         x = "Scaled structural stability")

  list(p1)

}
