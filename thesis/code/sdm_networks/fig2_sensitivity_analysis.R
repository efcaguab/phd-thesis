plot_sensitivity_analysis <- function(fig_sensitivity_analysis_data){

  attach(fig_sensitivity_analysis_data)
  suppressPackageStartupMessages({
    library(ggplot2)
  })

  thresholds <- min_occurrences_factor %>%
    as.data.frame() %>%
    tidyr::gather(key = "niche_space") %>%
    dplyr::filter(niche_space == chosen_niche_space)

  pal <- fig_metric("shade_red")[c(4,7)]

  n_nets <- suitability_subsamples$n_net_occurrences[1]

  e <- error_subsamples %>%
    dplyr::group_by(niche_space) %>%
    dplyr::mutate(error = mae) %>%
    dplyr::filter(niche_space == "all_species")

  p <- e %>%
    ggplot(aes(x = n_occ, y = error)) +
    geom_point(data = dplyr::sample_n(e, 1000),
               shape = 21, alpha = 1, size = 1, colour = "grey30", stroke = 0.25) +
    # geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    geom_smooth(method = "gam" ,
                method.args = list(family = "binomial"),
                formula = y ~ s(x), se = F,
                size = 0.5, colour = "black") +
    geom_hline(yintercept = min_suitability_error, size = 0.25, linetype = 2) +
    geom_vline(data = thresholds,
               aes(xintercept = value*n_nets), size = 0.25, linetype = 2) +
    geom_text(data = thresholds,
              aes(label = paste("~",  value, "occurrences\nper community"),
                  x = value*n_nets),
              y = max(e$error), angle = 90, hjust = 1, vjust = 1.2,
              size = 2.3, lineheight = 0.8) +
    annotate("text", label = "10% error", x = max(e$n_occ), y = 0.1, hjust = 1,
             vjust = -1, size = 2.3) +
    # scale_x_continuous(limits = c(2,35)) +
    scale_x_log10() +
    # scale_fill_manual(values = pal, aesthetics = c("fill", "colour"),
    # labels = c(" env. space based on all spp. occurrences",
    # " env. space based on each spp. occurrences")) +
    base_ggplot_theme() +
    theme(legend.position = "none",
          legend.justification = c(1,1),
          legend.title = element_blank()) +
    labs(x = "number of GBIF occurrences",
         y = "mean absolute error",
         title = "error of bioclimatic suitability",
         subtitle = "for a species present in two plant-pollinator communities ")

  # ggsave("plot.pdf", p,  width = unit(width("single"), "in"), height = unit(2.2, "in"))

  p
}
