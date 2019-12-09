plot_all_conditional_effect <- function(fig_conditional_effects_data){

  attach(fig_conditional_effects_data)

  suppressPackageStartupMessages({
    require(ggplot2)
  })

  pal <- fig_metric("red_shade")[c(7,6)]

  # grinell_niche_size_plot <- cond_draws$grinell_niche_size %>%
  #   plot_conditional_effect_guild(pal, mean_parameter_values$niche_size) +
  #   labs(title = "(a) environmental niche size",
  #        x = "environmental niche size (scaled)")

  suitability_plot <- cond_draws$suitability %>%
    plot_conditional_effect_guild(pal, mean_parameter_values$suitability) +
    labs(title = "(a) environmental stress",
         x = "environmental stress")

  # generality_plot <- cond_draws$generality %>%
  #   plot_conditional_effect_guild(pal, mean_parameter_values$generality, TRUE) +
  #   labs(title = "(c) generality",
  #        x = "# partners across communities")

  possible_plot <- cond_draws$possible %>%
    plot_conditional_effect_guild(pal, mean_parameter_values$possible) +
    labs(title = "(b) possible number of partners",
         x = "# possible partners")

  p <- cowplot::plot_grid(#grinell_niche_size_plot,
    suitability_plot,
    # generality_plot,
    possible_plot,
    ncol = 1,
    align = "hv", axis = "lt")
  p
  # ggsave("plot.pdf", p,  width = unit(width("single"), "in"), height = unit(2.2*3, "in"))

}

plot_conditional_effect_guild <- function(data, pal, mean_val, log_transformed = FALSE){

  if(log_transformed) mean_val <- exp(mean_val)

  data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(guild = translate_guild(guild, "effect")) %>%
    ggplot(aes(x = var, y = .value, group = interaction(.draw, guild), colour = guild)) +
    geom_vline(xintercept = mean_val, size = 0.25, linetype = 2) +
    geom_line(alpha = 0.15, size = 0.25) +
    geom_line(aes(group = guild), stat = "summary",fun.y = "mean", size = 1) +
    facet_wrap(~guild) +
    scale_fill_manual(values = pal, aesthetics = c("fill", "colour"),
                      labels = c(" env. space based on all spp. occurrences",
                                 " env. space based on each spp. occurrences")) +
    base_ggplot_theme() +
    coord_cartesian(expand = F) +
    theme(legend.position = "none") +
    labs(y = "# partners")
}

translate_guild <- function(x, type = "normal"){
  if(type == "effect"){
    dplyr::case_when(
      x == "pla_id" ~ "effect on plants",
      x == "ani_id" ~ "effect on animals",
      TRUE ~ "unknown"
    )
  } else if(type == "normal"){
    dplyr::case_when(
      x == "pla_id" ~ "plants",
      x == "ani_id" ~ "animals",
      TRUE ~ "unknown"
    )
  }

}

translate_model_formula <- function(x, type = "short"){

  if(type == "long-initial"){
    dplyr::case_when(
      x == "formula_base" ~ "baseline: S + G + P + E",
      x == "formula_no_grinell_niche_size" ~ "S + G + P",
      x == "formula_no_generalism" ~ "S + P + E",
      x == "formula_no_suitability" ~ "G + P + E",
      x == "formula_no_possible_partners_generalism" ~ "S + E",
      x == "formula_no_possible_partners" ~ "S + G + E",
      TRUE ~ "unknown"
    )
  } else if(type == "long-abv"){
    dplyr::case_when(
      x == "formula_base" ~ "stress x guild + # possible partners",
      # x == "formula_no_grinell_niche_size" ~ "Suit. + Gen. + Pot.",
      # x == "formula_no_generalism" ~ "Suit. + Pot. + Env.",
      x == "formula_no_suitability" ~ "guild + # possible partners",
      # x == "formula_no_possible_partners_generalism" ~ "Suit. + Env.",
      x == "formula_no_possible_partners" ~ "stress x guild",
      x == "formula_full" ~ "FULL",
      x == "formula_no_guild" ~ "stress + # possible partners",
      TRUE ~ "unknown"
    )
  }
}

