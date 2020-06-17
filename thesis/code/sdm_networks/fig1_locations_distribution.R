# plot distribution of number of species per number of locations
plot_species_location_distribution <- function(dist_species_multiple_locations_data){

  suppressPackageStartupMessages({
    require(ggplot2)
    require(ggforce)
  })

  pal <- fig_metric("red_shade")[c(7,4)]

  p <- dist_species_multiple_locations_data %>%
    ggplot(aes(x = n_locations, y = n_species, fill = guild)) +
    geom_line(aes(colour = guild), linetype = 3, size = 0.5) +
    geom_point(shape = 21, size = 1) +
    # geom_text_repel(data = distribution_data_highlight, aes(label = sp_name)) +
    geom_mark_circle(aes(label = sp_name, filter = !is.na(sp_name), group = sp_name),
                     expand = unit(0.1, "mm"),
                     alpha = 0.1,
                     colour = NA,
                     label.fontsize = 7,
                     label.buffer = unit(5, "mm"),
                     label.minwidth = unit(10, "mm"),
                     label.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
                     label.fontface = "italic",
                     label.family = "iwonaitalic",
                     con.size = 0.25,
                     con.cap = unit(1, "mm"),
                     con.type = "straight") +
    scale_fill_manual(values = pal, aesthetics = c("fill", "colour"),
                      labels = c("animals", "plants")) +
    scale_y_log10(labels = scales::number_format(big.mark = ",")) +
    scale_x_log10(breaks = c(1,2, 4, 8, 16, 32)) +
    base_ggplot_theme() +
    theme(legend.position = c(0.95,0.95),
          legend.justification = c(1,1),
          legend.title = element_blank()) +
    labs(x = "number of locations",
         y = "frequency (# species)",
         title = "number of species at multiple locations",
         subtitle = "frequency distribution")

  # ggsave("plot.pdf")
  # ggsave("plot.pdf", p,  width = unit(width("single"), "in"), height = unit(2.2, "in"))
  p
}
