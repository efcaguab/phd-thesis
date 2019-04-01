make_fig_species_partial <- function(df_cc){
  require(ggplot2)

  add_res <- function(x, res){
    x + res
  }

  ylims <- c(-11, 20)
  p1 <- partial_plot(df_cc, "pushpull_o", "I(pushpull_comp + res)", "lm") +
    labs(title = "(c) species dependence asymmetry",
         subtitle = "partial residual plot",
         x = "asymmetry",
         y = latex2exp::TeX("partial residuals")) +
    coord_cartesian(ylim = ylims)

  p2 <- partial_plot(df_cc, "strength_o", "I(strength_comp + res)", "lm") +
    labs(title = "(b) species visitation strength",
         subttle = "partial residual plot",
         x = "strength",
         y = latex2exp::TeX("partial residuals")) +
    scale_x_continuous(breaks = log(c(1,10,100)), labels = exp) +
    coord_cartesian(ylim = ylims)


  p3 <- partial_plot(df_cc, "nested_o", "I(nested_comp + res)", "lm") +
    labs(title = "(a) species contribution to network nestedness",
         subtitle = "partial residual plot",
         x = "contribution",
         y = latex2exp::TeX("partial residuals")) +
    coord_cartesian(ylim = ylims)

  p4 <- partial_plot(df_cc, "degree_o", "I(degree_comp + res)", "lm") +
    labs(title = "(d) species degree",
         subtitle = "partial residual plot",
         x = "number of interacting species",
         y = latex2exp::TeX("partiall residuals")) +
    scale_x_continuous(breaks = log(c(1,10,100)), labels = exp) +
    coord_cartesian(ylim = ylims)

  # cowplot::plot_grid(p1,p2,p3,p4)
  list(p3, p2, p1, p4)
}

partial_plot <- function(df, x, y, smooth_method = "lm"){
  p <-  df %>%
    ggplot(aes_string(x = x, y = y)) +
    geom_hline(yintercept = 0, size = 0.25, linetype = 2) +
    geom_point(aes(colour = guild), shape = 21, size = 1, alpha = 0.15) +
    geom_smooth(aes(color = guild, fill = guild),
                method = smooth_method,
                method.args = list(family = "binomial"),
                se = T,
                size = 0.5,
                alpha = 0.2) +
    scale_color_manual(values = c(fig_metric("red_shade")[4],
                                  fig_metric("red_shade")[6]),
                       name = "",
                       labels = c("plants", "pollinators")) +
    scale_fill_manual(values = c(fig_metric("red_shade")[4],
                                 fig_metric("red_shade")[6]),
                      name = "",
                      labels = c("plants", "pollinators")) +
    base_ggplot_theme() +
    theme(legend.position = c(0.01, 1.125),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = "NA"),
          legend.key.size = unit(0.15, "in"),
          plot.subtitle = element_blank(),
          plot.title = element_text(margin = margin(b = "5.5")))
  p
}
