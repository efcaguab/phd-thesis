make_fig_emp_contollability <- function(fig_data){

  attach(fig_data)
  p1 <- d %>%
    ggplot(aes(x = n_D, colour = inv)) +
    stat_density(geom= "line", size = 0.5, position = position_identity()) +
    geom_vline(data = medians_d, aes(xintercept = n_D, colour = inv),  linetype = 2, size = 0.5, show.legend = FALSE) +
    scale_color_manual(values = c(fig_metric("red_shade")[6], fig_metric("red_shade")[4]),
                       name = "",
                       guide = guide_legend(reverse = F), labels = c("Invaded", "Uninvaded")) +
    xlab(expression(paste(n[D]))) +
    base_ggplot_theme() +
    theme(legend.position = c(0.98, 1.1),
          legend.justification = c(1,1),
          legend.background = element_rect(fill = "NA"), legend.key.size = unit(0.15, "in")) +
    labs(title = "(a) invaded and uninvaded networks",
         subtitle = "probability density of controllability vs. invasion status",
         y = "probability density")

  p3 <- df_cc %>%
    ggplot(aes(x = web_asymmetry_o, y = n_D)) +
    geom_point(shape = 21, size = 1, colour = fig_metric("red_shade")[9]) +
    geom_smooth(method = "glm",
                se = T,
                colour = fig_metric("red_shade")[5],
                fill = fig_metric("red_shade")[5],
                alpha = 0.25,
                size = 0.5) +
    scale_y_continuous(labels = function(x) round(plogis(x), 2)) +
    labs(title = "(b) controllability vs. network asymmetry",
         subtitle = "asymmetry of plant-pollinator richness",
         x = "network asymmetry",
         y = latex2exp::TeX("$n_D$")) +
    base_ggplot_theme()

  p2 <- r %>%
    ggplot(aes(x = delta_n_D, color = randomisation)) +
    stat_density(geom = "line", position = position_identity(), size = 0.5) +
    geom_vline(data = means, aes(xintercept = delta_n_D, color = randomisation), size = 0.5, linetype = 2, show.legend = F) +
    scale_color_manual(values = c(fig_metric("red_shade")[6], fig_metric("red_shade")[4]),
                       name = "",
                       guide = guide_legend(reverse = T), labels = c("Directions", "Interactions")) +
    base_ggplot_theme() +
    theme(legend.position = c(0.98,1.1),
          legend.justification = c(1,1),
          legend.background = element_rect(fill = "NA"), legend.key.size = unit(0.15, "in")) +
    labs(title = "(c) difference between random and empirical networks",
         subtitle = "probability density of controllability difference",
         x = expression(paste(Delta, n[D])),
         y = "probability density")

  list(p1, p3, p2)
}
