plot_variable_importance <- function(variable_importance) {
  require(ggplot2)

  plot_importance <- function(x){
    middle_space <- -0.4  # for 1/3 is -0.525
    x %>% ggplot(aes(x = term, y = value)) +
      # geom_segment(aes(xend = term, yend = 0)) +
      # geom_point(shape = 21, fill = cgm()$pal_rb3[2], size = 6) +

      geom_col(fill = fig_metric("red_shade")[2]) +
      # geom_hline(yintercept = 0, linetype = 2, size = 0.25, colour = "grey30") +

      # geom_point(aes(x = 1, y = middle_space), alpha = 0) +
      geom_text(aes(label = paste0(" ", format(round(value, digits = 2)), " "),
                    hjust = text_align),
                size = 2) +
      # geom_text(aes(label = term, y = middle_space),
      #           # fontface = "bold",
      #           size = 2.5,
      #           colour = "black") +
      scale_x_discrete(expand = c(0,0)) +
      coord_flip() +
      base_ggplot_theme() +
      theme(axis.title.y = element_blank(),
            legend.position = "none",
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank())
  }

  plots <- variable_importance %>%
    tidyr::gather("key", "value", `conspecific (absolute)`:heterospecific) %>%
    dplyr::filter(key %in% c("conspecific (absolute)", "heterospecific")) %>%
    dplyr::mutate(term = forcats::fct_reorder(term, value),
                  # text_align = dplyr::if_else(value < 0.1,
                  # "outward", "inward")) %>%
                  text_align = dplyr::if_else(key == "heterospecific",
                                              "right", "left")) %>%
    split(.$key) %>%
    purrr::map(plot_importance)

  plots <- plots %>%
    # subtitles
    purrr::map2(c("conspecific pollen", "heterospecific pollen"),
                function(x,y) {x + labs(subtitle = y)}) %>%
    purrr::map2(list(margin(r = 0, t = 5.5, b = 5.5, l = 5.5), margin(l = 0, r = 5.5, t = 5.5, b = 5.5)),
                function(x,y) {x + theme(plot.margin = y)})

  plots[[3]] <- variable_importance %>%
    ggplot(aes(y = term)) +
    geom_text(aes(label = term), x = 0.5, size = 2.3) +
    # expansion to match the 90% width of the columns
    scale_y_discrete(expand = c(0,0.45)) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = margin(r = 0, t = 5.5, b = 5.5, l = 0))

  plots[[1]] <- plots[[1]] + labs(title = "relative variable importance")
  # plots[[2]] <- plots[[2]] + scale_x_discrete(position = "top", expand = c(0,0))
  plots[[1]] <- plots[[1]] + scale_y_reverse(expand = c(0,0))
  plots[[2]] <- plots[[2]] + scale_y_continuous(expand = c(0,0))
  plots[[2]] <- plots[[2]] + theme(plot.subtitle = element_text(hjust = 1))
  plots[[1]] <- plots[[1]] + theme(plot.title = element_text(margin = margin(b = 5.5, unit = "pt")))

  # pdf(width = 3.25 , height = 1.25)
  # cowplot::plot_grid(
  #   plots[[1]],
  #   plots[[3]],
  #   plots[[2]],
  #   align = "h",
  #   rel_widths = c(1,0.75,1),
  #   nrow = 1
  # )
  # dev.off()
  plots
}


plot_coefficient_averages <- function(coefficient_averages, variable_importance){
  # drake::loadd(coefficient_averages)
  require(ggplot2)

  var_imp <- variable_importance %>%
    tidyr::gather("key", "value", `conspecific (absolute)`:heterospecific) %>%
    dplyr::filter(key %in% c("conspecific (absolute)", "heterospecific")) %>%
    dplyr::mutate(term = forcats::fct_reorder(term, value))

  plot_metric_qual_quan <- function(x, this_metric){

    abs_smaller <- function(x,y) {if (abs(x) < abs(y)) return(x) ; y}
    abs_larger <-  function(x,y) {if (abs(x) < abs(y)) return(y) ; x}

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
      # dplyr::filter(metric == this_metric) %>%
      ggplot(aes(y = estimate_mid, x = term, group = metric)) +
      geom_tile(aes(height = Inf, width = 1, alpha = as.numeric(term) %% 2 == 0),
                fill = fig_metric("fill_rows")) +
      geom_hline(yintercept = 0, linetype = 2, size = 0.25, colour = "gray30") +
      geom_text(aes(#label = "..",
        label = paste0(" ", format(round(estimate_mid, 2)), " "),
        y = label_y,
        hjust = label_hjust,
        alpha = metric == this_metric),
        size = 2,
        # nudge_x = line_offset,
        vjust = 0.5) +
      geom_errorbar(aes(x = term, ymin = mean_estimate_quantile_025,
                        ymax = mean_estimate_quantile_975,
                        alpha = metric == this_metric),
                    colour = fig_metric("color_errorbars"),
                    size = fig_metric("size_errorbars"),
                    width = 0,
                    position = position_dodge(width = dodge_width)) +
      geom_point(aes(x = term, alpha = metric == this_metric),
                 colour = fig_metric("red_shade")[9], shape = 21,
                 fill = "white",
                 size = 1,
                 position = position_dodge(width = dodge_width)) +
      # geom_text(data = annotations, aes(x = x, y = y, label = label, hjust = hjust),
      # size = 2) +
      base_ggplot_theme() +
      labs(y = "effect size") +
      scale_x_discrete(expand = c(0,0)) +
      coord_flip(clip = "off") +
      theme(legend.position = "none",
            axis.title = element_blank(),
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
    purrr::map2(c("quantity\n(conspecific)", "purity\n(consp. / heterosp.)"),
                function(x,y) {x + labs(subtitle = y)})

  plots[[1]] <- plots[[1]] +
    theme(axis.text.y = element_blank(),
          plot.margin = margin(t = 5.5, r = 0, b = 5.5, l = 5.5, unit = "pt"),
          plot.title = element_text(margin = margin(b = 5.5, unit = "pt")),
          axis.line.x = element_line(size = 0.25)) +
    labs(title = "(b) mean effect on pollination service")
  plots[[2]] <- plots[[2]] + theme(plot.subtitle = element_text(hjust = 1),
                                   axis.line.x = element_line(size = 0.25),
                                   plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 0, unit = "pt"))

  plots[[3]] <- qua_qua_data %>%
    ggplot(aes(y = term)) +
    geom_text(aes(label = term), x = 0.5, stat = "unique", size = 2.3)+
    # expansion to match the 90% width of the columns
    scale_y_discrete(expand = c(0,0.5)) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = margin(r = 0, t = 5.5, b = 5.5, l = 0))
  #
  #   pdf(width = 3.25, height = 1.5)
  #   cowplot::plot_grid(
  #     plots[[1]],
  #     plots[[3]],
  #     plots[[2]],
  #     ncol = 3,
  #     align = "h",
  #     rel_widths = c(1, 0.75, 1) # for two thirds figure its 1:1.4
  #   )
  # dev.off()
  plots
}

make_fig_coefficient_avarages <- function(coefficient_averages, variable_importance){
  require(ggplot2)
  require(ggridges)

  pal <- fig_metric("red_shade")[c(6,8)]

  dist <- coefficient_averages %>%
    dplyr::filter(scale == "community",
                  pollen_category %in% c("conspecific_abs", "heterospecific_abs")) %>%
    dplyr::group_by(pollen_category, scale, term, sample_n) %>%
    # dplyr::summarise(estimate = mean(estimate)) %>%
    dplyr::group_by() %>%
    humanize() %>%
    # dplyr::mutate(term = forcats::fct_relevel(term, c("func. originality", "abundance", "degree"), after = 2)) %>%
    # add fake point lower so that I can include the importance labels
    dplyr::group_by(pollen_category) %>%
    # dplyr::mutate(estimate = dplyr::if_else(max(estimate) == estimate,
    # estimate + 0.15,
    # estimate)) %>%
    dplyr::filter(pollen_category != "heterospecific_abs") %>%
    dplyr::group_by()

  # to make sure lines extend to the limits
  borders_rows <- dist %>%
    dplyr::mutate(max_max = max(estimate),
                  min_min = min(estimate)) %>%
    dplyr::group_by(pollen_category) %>%
    dplyr::mutate(max = max(estimate),
                  min = min(estimate)) %>%
    tidyr::gather(key = "min_max", "min_max_value", min, max) %>%
    dplyr::group_by(pollen_category, min_max) %>%
    dplyr::filter(estimate == min_max_value) %>%
    dplyr::slice(1) %>%
    dplyr::filter(estimate != min_min & min_max == "min" |
                    estimate != max_max & min_max == "max") %>%
    dplyr::mutate(estimate = dplyr::if_else(min_max == "min",
                                            min_min, estimate),
                  estimate = dplyr::if_else(min_max == "max",
                                            max_max, estimate)) %>%
    dplyr::group_by() %>%
    dplyr::select(-min_max, -min_min, -max_max, -min_max_value)

  dist <- dplyr::bind_rows(dist, borders_rows)

  dist %<>%
    dplyr::group_by() %>%
    dplyr::mutate(pollen_category = dplyr::if_else(pollen_category == "conspecific (absolute)",
                                                   "conspecific",
                                                   pollen_category))

  labels <- dist %>%
    dplyr::filter(estimate != 0) %>%
    dplyr::mutate(x = estimate, categ1 = term, categ2 = pollen_category) %>%
    dplyr::mutate(min_min = min(x),
                  max_max = max(x)) %>%
    dplyr::group_by(categ1) %>%
    dplyr::mutate(min_x = min(x),
                  max_x = max(x),
                  inner = min(abs(min_x), abs(max_x)),
                  outer = max(abs(min_x), abs(max_x)),
                  mean_x = mean(unique(c(min_x, max_x)))) %>%
    dplyr::select(categ1, categ2, min_min, max_max, min_x, max_x, inner, outer, mean_x) %>%
    dplyr::distinct() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      sign_outer = dplyr::if_else(
        which.max(c(abs(min_x), abs(max_x))) == 1,
        sign(min_x), sign(max_x)),
      sign_inner = dplyr::if_else(
        which.min(c(abs(min_x), abs(max_x))) == 1,
        sign(min_x), sign(max_x)),
      x_coord = inner * sign_inner,
      x_coord_labs = mean_x,
      x_coord_title = mean_x,
      x_align = dplyr::if_else(x_coord == max_x,
                               "left", "right"),
      x_align_labs = dplyr::if_else(x_coord == min_x, "left", "right"),
      sign_title = dplyr::if_else(x_align == "right",
                                  -1, 1),
      x_coord = dplyr::if_else(categ2 == "func. originality",
                               outer * sign_outer, x_coord)) %>%
    dplyr::rename(term = categ1, pollen_category = categ2) %>%
    dplyr::group_by() %>%
    dplyr::mutate(term_n = dplyr::case_when(
      term == "# shared pol." ~ 4,
      term == "func. originality" ~ 3,
      term == "abundance" ~ 2,
      TRUE ~ 1
    ))

  p <- dist  %>%
    dplyr::filter(estimate != 0) %>%
    dplyr::mutate(term_n = dplyr::case_when(
      term == "# shared pol." ~ 4,
      term == "func. originality" ~ 3,
      term == "abundance" ~ 2,
      TRUE ~ 1
    ),
    pollen_category = forcats::fct_relevel(pollen_category, c("conspecific", "heterospecific")),
    pollen_category = forcats::fct_rev(pollen_category)) %>%
    ggplot() +
    geom_vline(xintercept = 0, linetype = 2, size = 0.25, color = "grey30") +
    geom_density_ridges(aes(x = estimate,
                            y = pollen_category,
                            colour = pollen_category),
                        alpha = 0.8,
                        panel_scaling = F,
                        scale = 4,
                        quantile_lines = F,
                        quantiles = 2,
                        fill = "white",
                        vline_linetype = 1,
                        size = 0.25,
                        rel_min_height = 0,
                        bandwidth = 0.015) +
    geom_text(data = labels,
              aes(x = x_coord + sign_title * 0.075,
                  colour = pollen_category,
                  y = pollen_category,
                  label = abb_col(pollen_category),
                  hjust = x_align),
              nudge_x = 0.01,
              stat = "unique", show.legend = F,
              size = 2) +
    geom_text(data = labels %>% dplyr::select(-pollen_category) %>% dplyr::distinct(),
              aes(x = x_coord_title,
                  y = 0.25,
                  label = term,
                  hjust = "center"),
              fontface = "plain",
              stat = "unique", show.legend = F,
              size = 2.5,
              colour = fig_metric("red_shade")[9]) +
    facet_grid(term_n ~ .) +
    scale_color_manual(values = pal) +
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(-2,2, by = 0.4)) +
    # scale_y_discrete(expand = c(1,0)) +
    # scale_discrete_manual("vline_color", values = c_scale()) +
    coord_cartesian(clip = "off", ylim = c(-0.5,2.25)) +
    base_ggplot_theme() +
    theme(legend.position = "none",
          # legend.direction = "horizontal",
          panel.border = element_blank(),
          axis.line.x = element_line(size = 0.25),
          axis.ticks.y = element_blank(),
          strip.placement = "outside",
          panel.spacing.y = unit(0, "pt"),
          # panel.grid.major.y = element_line(size = 0.25, colour = "grey70"),
          # axis.text.y = element_text(size = 7, vjust = 0, colour = "black"),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          plot.subtitle = element_text(margin = margin(b = 15)),
          # axis.title.x = element_text(size = 8, colour = "grey20"),
          strip.text = element_blank(
            #size = 7, face = "plain", hjust = 1, margin = margin(t = - 5)
          )) +
    labs(title = "(a) distribution of effects",
         subtitle = "based on 100 bootstrap repllicates",
         colour = "", fill = "", x = "estimated effect on (log) pollen deposition", y = "")

  # pdf(width = 6.5/5*2, height = 2.29)
  p
  # dev.off()
}

abb_col <- function(x){
  x %>%
    stringr::str_replace("conspecific", "consp.") %>%
    stringr::str_replace("heterospecific", "hetsp.")
}


c_scale_discrete <- function(x){
  c_scale()[x]
}


mid <- function(x){
  mean(x, na.rm  = TRUE)
}

upper <- function(x){
  mid(x) + se(x)
}

lower <- function(x){
  mid(x) - se(x)
}

se <- function(x) sqrt(var(x, na.rm = T)/length(x))


quantile_025 <- function(x, na.rm = FALSE){
  quantile(x, 0.025, na.rm = na.rm)
}

quantile_975 <- function(x, na.rm = FALSE){
  quantile(x, 0.975, na.rm = na.rm)
}


humanize <- function(x, sites = NA, random_effects = NA, formula_long = FALSE, term_long = TRUE){
  if ('site_name' %in% names(x) & !is.na(sites)) {
    x <- dplyr::inner_join(x, sites, by = 'site_name')
  }
  if ('term' %in% names(x)) {
    if (term_long) {
      x <- x %>%
        dplyr::mutate(term = dplyr::case_when(
          grepl('org', term) ~ 'func. originality',
          grepl('originality', term) ~ 'func. originality',
          grepl('abn', term) ~ 'abundance',
          grepl('abu', term) ~ 'abundance',
          grepl('rab', term) ~ 'abundance',
          grepl('deg', term) ~ '# shared pol.',
          grepl('k', term) ~ '# shared pol.',
          grepl('poc', term) ~ 'visit potential',
          grepl('pollen_cont', term) ~ 'visit potential',
          TRUE ~ term
        ))
    } else {
      x <- x %>%
        dplyr::mutate(term = dplyr::case_when(
          grepl('org', term) ~ 'func. originality',
          grepl('originality', term) ~ 'func. originality',
          grepl('abn', term) ~ 'abundance',
          grepl('abu', term) ~ 'abundance',
          grepl('rab', term) ~ 'abundance',
          grepl('deg', term) ~ '# shared pol.',
          grepl('k', term) ~ '# shared pol.',
          grepl('poc', term) ~ 'visit potential',
          grepl('pollen_cont', term) ~ 'visit potential',
          TRUE ~ term
        ))
    }

  }
  if ('fixed_formula' %in% names(x)) {
    if (formula_long == TRUE) {
      x <- x %>%
        dplyr::mutate(
          fixed_formula = stringr::str_replace(fixed_formula, "pollen_gain ~", ""),
          fixed_formula = stringr::str_replace(fixed_formula, "abn", "abundance"),
          fixed_formula = stringr::str_replace(fixed_formula, "poc", "visit potential"),
          fixed_formula = stringr::str_replace(fixed_formula, "org", "func. originality"),
          fixed_formula = stringr::str_replace(fixed_formula, "deg", "# shared pol."))
    } else {
      x <- x %>%
        dplyr::mutate(
          fixed_formula = stringr::str_replace(fixed_formula, "pollen_gain ~", ""),
          fixed_formula = stringr::str_replace(fixed_formula, "abn", "a"),
          fixed_formula = stringr::str_replace(fixed_formula, "poc", "o"),
          fixed_formula = stringr::str_replace(fixed_formula, "org", "t"),
          fixed_formula = stringr::str_replace(fixed_formula, "deg", "k"))
    }
  }
  if ('random_effect' %in% names(x)) {
    x <- dplyr::inner_join(x, random_effects, by = "random_effect")
  }
  if ('pollen_category' %in% names(x)) {
    x <- x %>%
      dplyr::filter(pollen_category != "heterospecific") %>%
      dplyr::mutate(pollen_category = dplyr::case_when(
        grepl('conspecific_abs', pollen_category) ~ 'conspecific (absolute)',
        grepl('conspecific_ctr', pollen_category) ~ 'conspecific (control)',
        grepl('conspecific', pollen_category) ~ 'conspecific (relative)',
        TRUE ~ 'heterospecific'
      ))
  }
  x
}
