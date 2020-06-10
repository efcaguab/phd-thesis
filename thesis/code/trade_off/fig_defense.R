plot_coefficient_averages_presentation <- function(coefficient_averages, variable_importance, file_name){
  # drake::loadd(coefficient_averages)
  require(ggplot2)
  require(showtext)

  var_imp <- variable_importance %>%
    tidyr::gather("key", "value", `conspecific (absolute)`:heterospecific) %>%
    dplyr::filter(key %in% c("conspecific (absolute)", "heterospecific")) %>%
    dplyr::mutate(term = forcats::fct_reorder(term, value))

  plot_metric_qual_quan <- function(x, this_metric){

    abs_smaller <- function(x,y) {if (abs(x) < abs(y)) return(x) ; y}
    abs_larger <-  function(x,y) {if (abs(x) < abs(y)) return(y) ; x}

    showtext_auto()

    line_offset <- 0.25
    p <- x %>%
      dplyr::filter(metric == this_metric) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(label_y = dplyr::if_else(abs(estimate_mid) < 0.25,
                                             abs_larger(mean_estimate_quantile_025, mean_estimate_quantile_975),
                                             abs_smaller(mean_estimate_quantile_025, mean_estimate_quantile_975)),
                    label_hjust = dplyr::if_else(abs(estimate_mid) > 0.25,
                                                 "inward",
                                                 "outward")) %>%
      ggplot(aes(y = estimate_mid, x = term, group = metric)) +
      # geom_hline(yintercept = 0, linetype = 2, size = 0.25, colour = "gray30") +
      geom_point(aes(x = term),
                 colour = "#9F2B35",
                 size = 3,
                 position = position_dodge(width = dodge_width)) +
      geom_segment(aes(yend = 0, xend = term), size = 1, colour = "#9F2B35") +
      defense_theme() +
      labs(y = "",
           caption = "") +
      coord_flip(clip = "off") +
      theme(legend.position = "none",
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            panel.border = element_blank(),
            axis.line.x = element_line(size = 0.25)) +
      scale_alpha_manual(values = c(0,1))

    if (this_metric == "quality") {
      p <- p + scale_y_continuous(limits = c(-0.24, 0.35))
    }
    p
  }

  dodge_width <- 0
  qua_qua_data <- coefficient_averages  %>%
    dplyr::group_by(scale, pollen_category, term, sample_n) %>%
    dplyr::mutate(sample_n2 = 1:dplyr::n()) %>%
    dplyr::select(pollen_category, term, sample_n, sample_n2, estimate) %>%
    tidyr::spread(pollen_category, estimate) %>%
    dplyr::mutate(quantity_abs = conspecific_abs,
                  quality_abs = conspecific_abs - heterospecific_abs) %>%
    dplyr::select(scale, term, sample_n, dplyr::contains("qua")) %>%
    tidyr::gather("var", "estimate", dplyr::contains("qua")) %>%
    dplyr::group_by(scale, term, sample_n, var) %>%
    dplyr::mutate(mean_estimate = mean(estimate)) %>%
    dplyr::group_by(scale, term, var) %>%
    # dplyr::sample_n(9, replace = TRUE) %>%
    dplyr::summarise_at(c("estimate", "mean_estimate"), dplyr::funs(mid, lower, upper, quantile_025, quantile_975)) %>%
    tidyr::separate(col = var, into = c("metric", "type"), sep = "_", remove = FALSE) %>%
    dplyr::mutate(type = paste0(type, ".")) %>%
    dplyr::group_by() %>%
    humanize() %>%
    dplyr::mutate(term = factor(term, levels(var_imp$term)))

  plots <- purrr::map(c("quantity", "quality"),
                      ~ plot_metric_qual_quan(qua_qua_data, .)) %>%
    purrr::map2(c("", ""),
                function(x,y) {x + labs(subtitle = y)})

  plots[[1]] <- plots[[1]] +
    theme(axis.text.y = element_blank(),
          plot.margin = margin(t = 0.5, r = 0, b = 0.5, l = 0.5, unit = "cm")) +
          # plot.title = element_text(margin = margin(b = 5.5, unit = "pt")),
          # axis.line.x = element_line(size = 0.25)) +
    labs(title = "")
  plots[[2]] <- plots[[2]] + theme(plot.subtitle = element_text(hjust = 1),
                                   axis.line.x = element_blank(),
                                   plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0, unit = "cm"))

  plots[[3]] <- qua_qua_data %>%
    ggplot(aes(y = term)) +
    geom_text(aes(label = term), x = 0.5, stat = "unique", size = 7, family = "iwona")+
    # expansion to match the 90% width of the columns
    scale_y_discrete(expand = c(0,0.5)) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title.y = element_blank(),
          panel.grid = element_blank(),
          plot.margin = margin(r = 0, t = 5.5, b = 5.5, l = 0))

    p <- cowplot::plot_grid(
      plots[[1]],
      plots[[3]],
      plots[[2]],
      ncol = 3,
      align = "h",
      rel_widths = c(1, 0.75, 1) # for two thirds figure its 1:1.4
    )
    plot_width <- 10
    width <- unit(plot_width, "cm")
    height <- unit(plot_width * 10 / 16, "cm")
    ggsave(file_name, p, width = width, height = height)

    showtext_auto(FALSE)


}
