make_fig_module_cophylogeny <- function(modules_p_value, alpha_level = 0.05){

  pal <- c(thesis_palette[1],
           thesis_palette_light[1])

  mod_pvalue <- read.table(modules_p_value, header = T, sep=',', row.names=1) %>%
    dplyr::mutate(mod = mod_pla) %>%
    dplyr::select(-mod_pla, -mod_pol) %>%
    dplyr::group_by(net) %>%
    dplyr::summarise(significant = sum(p_value <= alpha_level),
                     no_significant = - 1 * (dplyr::n() - significant)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(net = forcats::fct_reorder(net, significant/no_significant, .desc = F)) %>%
    tidyr::gather("significant", "n", significant, no_significant) %>%
    dplyr::mutate(significant = factor(significant, levels = c("significant", "no_significant")),
                  network = factor(net, labels = 1:54),
                  group = cut(as.numeric(network), 2,
                              labels = c("network 1-27",
                                         # "network 19-36",
                                         "network 28-54")))

  mod_pvalue %>%
    ggplot() +
    geom_col(aes(x = net, y = n, fill = significant),
             size = 0.3,
             width = 0.8,
             # colour = "black",
             position = "stack") +
    geom_hline(yintercept = 0, colour = "white", size = 1) +
    geom_hline(yintercept = 0,
               colour = thesis_palette_dark[1],
               linetype = 2,
               size = 0.25) +
    scale_fill_manual(values = pal,
                      name = "",
                      labels = c("p < 0.05  ", "p > 0.05  ")) +
    scale_y_continuous(breaks = seq(-100, 100, by = 2),
                       labels = abs(seq(-100, 100, by = 2))) +
    facet_wrap("group", ncol = 1, scales = "free_x") +
    base_ggplot_theme() +
    theme(panel.border = element_blank(),
          legend.position = "bottom",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.y = element_line(),
          axis.ticks.x = element_blank()) +
    labs(title = "cophylogenetic signal of individual modules",
         subtitle = "across 54 networks",
         y = "number of modules")

  # ggsave("plot1.pdf", width = fig_sizes()$two_column_width,
  #        height = fig_sizes()$two_column_width*0.9)
}
