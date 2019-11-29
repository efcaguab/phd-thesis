plot_pca_variances_and_contributions <- function(pcas, chosen_threshold){
  require(ggplot2)

  this_pca <- pcas %>%
    purrr::keep(~ .$call$X$pca_type[1] == "across") %>%
    purrr::keep(~ .$call$X$threshold[1] == chosen_threshold) %>%
    extract2(1)

  variances_data <- this_pca$eig %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(rowname = stringr::str_remove(rowname, "comp ")) %>%
    dplyr::rename(dim = rowname) %>%
    dplyr::mutate_if(is.numeric, function(x) x/100) %>%
    dplyr::mutate(group = "components' cumulative percentage of variance")

  contributions_data <- this_pca$var$contrib %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    tidyr::gather(key = "dim", "contribution", dplyr::contains("Dim")) %>%
    dplyr::mutate(dim = stringr::str_remove(dim, "Dim."),
                  contribution = contribution/100) %>%
    dplyr::rename(term = rowname)

  contributions_data %>%
    humanize(term_long = F) %>%
    dplyr::mutate(term = forcats::fct_relevel(term, "visit potential", "abundance", "func. originality")) %>%
    ggplot(aes(x = as.numeric(dim), y = contribution)) +
    geom_col(aes(fill = term), alpha = 0.75,
             position = position_stack(reverse = TRUE)) +
    geom_line(data = variances_data,
              aes(y = `cumulative percentage of variance`,
                  linetype = group),
              size = 0.5,
              colour = fig_metric("red_shade")[c(9)]) +
    geom_point(data = variances_data,
               aes(y = `cumulative percentage of variance`,
                   shape = group),
               size = 1,
               # shape = 21,
               fill = fig_metric("red_shade")[2],
               colour = fig_metric("red_shade")[c(9)]) +
    scale_x_continuous(breaks =1:4, trans = "reverse", labels = scales::ordinal(1:4)) +
    scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
    scale_fill_manual(values = fig_metric("red_shade")[c(8,6,4,2)]) +
    scale_shape_manual(values = 21) +
    scale_linetype_manual(values = 1) +
    guides(fill = guide_legend(order = 2, title = ""),
           shape = guide_legend(order = 1, title = ""),
           linetype = guide_legend(order = 1, title = "")) +
    base_ggplot_theme() +
    coord_flip(clip = "off") +
    labs(x = "component",
         title = "(a) components' variance and variable contributions",
         subtitle = "principal component analysis of ecological variables") +
    theme(panel.border = element_blank(),
          axis.line.x = element_line(size = 0.25),
          legend.position = "top",
          axis.ticks.y = element_blank(),
          legend.box = "vertical",
          legend.spacing = unit(1, "pt"),
          plot.margin = margin(t = 5.5, r = 8, b = 5.5, l = 5.5, unit = "pt"),
          legend.text = element_text(margin = margin(r = 0, l = -1, unit = "pt")),
          legend.box.just = "right",
          legend.margin = margin(),
          legend.box.margin = margin(b = -10))

}


# Plot the PCA, scaled globally (across communities)
plot_pca <- function(pcas, chosen_threshold){

  require(ggplot2)
  require(ggforce)

  this_pca <- pcas %>%
    purrr::keep(~ .$call$X$pca_type[1] == "across") %>%
    purrr::keep(~ .$call$X$threshold[1] == chosen_threshold) %>%
    extract2(1)

  this_pca_data <- this_pca$call$X %>%
    dplyr::bind_cols(tibble::as_data_frame(this_pca$ind$coord)) %>%
    dplyr::group_by(plant_name) %>%
    dplyr::mutate(n_sites = dplyr::n_distinct(site_name))

  variances_data <- this_pca$eig %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(rowname = stringr::str_remove(rowname, "comp ")) %>%
    dplyr::rename(dim = rowname)

  hulls <- this_pca_data %>%
    # hulls on;y for plants in more than one site
    dplyr::filter(n_sites > 1) %>%
    dplyr::group_by(plant_name) %>%
    dplyr::select_at(dplyr::vars(dplyr::contains("Dim"))) %>%
    dplyr::do(.[grDevices::chull(.[, -1L]), ]) %>%
    dplyr::filter(plant_name %in% unique(this_pca_data$plant_name))

  # plot only points that are in a hull or belong to single community
  hulls_and_single_points <- hulls %>%
    dplyr::mutate(hull_point = T) %>%
    dplyr::full_join(this_pca_data) %>%
    dplyr::filter(!is.na(hull_point) | n_sites == 1)

  this_pca_data %>%
    ggplot(aes(x = Dim.1, y = Dim.2)) +
    geom_hline(yintercept = 0,
               linetype = 2,
               colour = fig_metric("color_references"),
               size = fig_metric("size_references")) +
    geom_vline(xintercept = 0,
               linetype = 2,
               colour = fig_metric("color_references"),
               size = fig_metric("size_references")) +
    geom_polygon(data = hulls,
                 aes(group = plant_name),
                 colour = fig_metric("color_errorbars"),
                 fill = fig_metric("red_shade")[1],
                 size = fig_metric("size_errorbars"),
                 alpha = 0.25) +
    geom_point(data = hulls_and_single_points,
               aes(fill = n_sites > 1),
               shape = 21,
               colour = fig_metric("red_shade")[9],
               size = 1) +
    geom_mark_hull(data = hulls, aes(group = plant_name,
                                     label = shorten_sp_name(as.character(plant_name)),
                                     filter = plant_name %in% c("Verbena intermedia",
                                                                "Sphaeralcea crispa",
                                                                # "Mentha pulegium",
                                                                "Nothoscordum euosimum",
                                                                # "Thelesperma megapotamicum",
                                                                "Hirschfeldia incana")
    ),
    color = NA,
    expand = unit(0.1, "mm"),
    radius = unit(0, "mm"),
    label.fontsize = 6,
    label.fontface = "italic",
    label.colour = fig_metric("color_errorbars"),
    label.fill = NA,
    label.margin = margin(2,1,2,1,"pt"),
    label.buffer = unit(2, "mm"),
    con.size = 0.25,
    con.linetype = 1,
    con.colour = fig_metric("color_errorbars_light"),
    con.type = "straight",
    # con.arrow = arrow(type = "closed",
    # length = unit(5, "pt"),
    # angle = 7),
    con.cap = unit(0.1, "mm")) +
    base_ggplot_theme() +
    scale_fill_manual(values = c("white", fig_metric("red_shade")[1])) +
    theme(legend.position = "none") +
    # coord_equal() +
    labs(x = paste0("1st component (", round(variances_data$`percentage of variance`[1]), "%)"),
         y = paste0("2nd component (", round(variances_data$`percentage of variance`[2]), "%)"),
         title = "(b) plant realised niches in PCA space",
         subtitle = "convex hulls of species niches across communities")

}


#' Shorten species names
#'
#' From Genus species to G. species. Ignores Genus sp.
#'
#' @param x a character vector
#'
#' @return a character vector
#'
shorten_sp_name <- function(x){
  y <- x
  for(i in 1:length(x)){
    if(!grepl('sp\\.', x[i])) {
      y[i] <- mini_dot(x[i])
    }
  }
  y
}


#' Shorten species names
#'
#' #' From Genus species to G. species. Ignores Genus sp.
#'
#' @param x a character string
#'
#' @return a character string
#'
mini_dot <- function(x){
  require(stringr)
  space_location <- stringr::str_locate(x, ' ')
  if (!any(is.na(space_location))) {
    str_sub(x, 2, space_location[1]) <- ". "
    x
  } else{
    x
  }
}

plot_permanova_dist <- function(permanova_plant_distances,
                                permanova_site_distances){
  require(ggplot2)

  perma_data <- permanova_plant_distances %>%
    dplyr::filter(na_threshold %in% c(0,1)) %>%
    dplyr::group_by(plant_name) %>%
    dplyr::mutate(mean_value = mean(value, na.rm = T)) %>%
    dplyr::group_by() %>%
    dplyr::filter(!is.na(mean_value),
                  metric == "median_dist",
                  na_threshold == 0,
                  !is.na(value)) %>%
    # dplyr::mutate(plant_name = paste0("  ", plant_name, "  ")) %>%
    dplyr::mutate(plant_name = shorten_sp_name(plant_name)) %>%
    dplyr::mutate(plant_name = forcats::fct_reorder(plant_name, value, .desc = F),
                  value_adj = p.adjust(value, method = "BH"))

  nudge <- 0
  perma_data %>%
    dplyr::mutate(x_label = dplyr::if_else(value < 0.05,
                                           max(value) + nudge, min(value) - nudge)) %>%
    ggplot(aes(x = value, y = plant_name)) +
    geom_tile(aes(width = Inf, height = 1,
                  alpha = as.numeric(plant_name) %% 2 == 0),
              fill = fig_metric("fill_rows")) +
    geom_vline(xintercept = 0.05, linetype = 2,
               size = fig_metric("size_references"),
               color = fig_metric("color_references")) +
    geom_segment(aes(xend = 0.05, yend = plant_name), size = 0.25,
                 color = fig_metric("red_shade")[9]) +
    geom_point(shape = 21,
               size = 1,
               fill = "white",
               color = fig_metric("red_shade")[9]) +
    geom_text(aes(label = plant_name, x = x_label),
              fontface = "italic",
              size = 2,
              hjust = "inward") +
    geom_point(aes(y = 13.5, x = 0.05), alpha = 0) +
    annotate(geom = "text", x = 0.05, y = 13,
             label = expression(" " %->% plain("more flexible") %->% ""),
             parse = F,
             hjust = "left",
             size = 2.25,
             colour = "grey20") +
    geom_point(aes(y = -0.5, x = 0.05), alpha = 0) +
    annotate(geom = "text", x = 0.05, y = 0,
             label = expression(" " %<-% plain("less flexible") %<-% ""),
             parse = F,
             hjust = "right",
             size = 2.25,
             colour = "grey20") +
    base_ggplot_theme() +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.border = element_blank(),
          axis.line.x = element_line(size = 0.25)) +
    labs(x = expression(italic(p) - plain("value")),
         title = "flexibility of plant's strategies",
         subtitle = "median distance between plant niches vs. randomisations") +
    coord_cartesian(clip = "off") +
    scale_x_continuous(expand = c(0,0.1),
                       breaks = c(0.01, 0.05, 0.5, 0.99),
                       trans = "log") +
    scale_y_discrete(expand = c(0,0)) +
    scale_alpha_manual(values = c(0,1))

}

