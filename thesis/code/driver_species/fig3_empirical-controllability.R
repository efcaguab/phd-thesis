make_fig_emp_contollability <- function(fig_data){

  attach(fig_data)
  p1 <- d %>%
    ggplot(aes(x = n_D, colour = inv)) +
    stat_density(geom= "line", size = 0.5, position = position_identity()) +
    geom_vline(data = medians_d, aes(xintercept = n_D, colour = inv),  linetype = 2, size = 0.25, show.legend = FALSE) +
    scale_color_manual(values = c(my_pallete()$light_grey, my_pallete()$dark_grey),
                       name = "",
                       guide = guide_legend(reverse = F), labels = c("Invaded", "Uninvaded")) +
    xlab(expression(paste(n[D]))) +
    base_ggplot_theme() +
    theme(legend.position = c(1, 1.15),
          legend.justification = c(1,1),
          legend.background = element_rect(fill = "NA"), legend.key.size = unit(0.15, "in")) +
    labs(title = "(a)",
         y = "Probability density")

  p3 <- df_cc %>%
    ggplot(aes(x = web_asymmetry_o, y = n_D)) +
    geom_point(shape = 21, size = 1, colour = "grey50", alpha = 0.75) +
    geom_smooth(method = "glm",
                se = T,
                colour = my_pallete()$dark_grey,
                fill = my_pallete()$light_grey,
                alpha = 0.25,
                size = 0.5) +
    scale_y_continuous(labels = function(x) round(plogis(x), 2)) +
    labs(title = latex2exp::TeX("(b)"),
         x = "Network asymmetry",
         y = latex2exp::TeX("$n_D$")) +
    base_ggplot_theme()

  p2 <- r %>%
    ggplot(aes(x = delta_n_D, color = randomisation)) +
    stat_density(geom = "line", position = position_identity(), size = 0.5) +
    geom_vline(data = means, aes(xintercept = delta_n_D, color = randomisation), size = 0.25, linetype = 2, show.legend = F) +
    scale_color_manual(values = c(my_pallete()$dark_grey,  my_pallete()$light_grey),
                       name = "",
                       guide = guide_legend(reverse = T), labels = c("Directions", "Interactions")) +
    base_ggplot_theme() +
    theme(legend.position = c(1,1.15),
          legend.justification = c(1,1),
          legend.background = element_rect(fill = "NA"), legend.key.size = unit(0.15, "in")) +
    labs(title = "(c)",
         x = expression(paste(Delta, n[D])),
         y = "Probability density")

  list(p1, p3, p2)
}
