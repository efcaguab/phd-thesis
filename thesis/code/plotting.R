# Info about the plotting sizes
fig_sizes <- function(){
  list(
    one_column_width = 3.4,
    two_column_width = 4.65
  )
}


base_ggplot_theme <- function(){
  require(ggplot2)
  theme_bw() +
    theme(text = element_text(family = "Helvetica"),
          title = element_text(size = 7, hjust = 0),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 7),
          axis.text = element_text(size = 7),
          axis.title = element_text(size = 8, hjust = 0.5),
          strip.text = element_text(size = 8, hjust = 0),
          strip.background = element_blank(),
          plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5), "mm"),
          panel.grid = element_blank(),
          legend.key = element_rect(fill = NA))
}

add_internal_margins <- function(x, i = 1){
  require(ggplot2)
  x[[i]] <- x[[i]]  +
    theme(plot.margin = margin(0.5, 2, 0.5, 0.5))
  x[[i + 1]] <- x[[i + 1]]  +
    theme(plot.margin = margin(0.5, 0.5, 0.5, 2))
  x
}

# Base palette, only first two colours are color blind safe
thesis_palette <- c(
  "#C23724", # Latex BrickRed
  "#084C61", # Bluish
  "#E3B505", # Yellowish
  "#4F6D7A", # Greysh
  "#56A3A6" # Greenish
  )

# Lighter shade for main palette (only first three colours)
thesis_palette_light <- c(
  "#fbece9",
  "#e7f8fd",
  "#fef9e6"
)

# Darker shade for main palette (only first three colours)
thesis_palette_dark <- c(
  "#160604",
  "#021218",
  "#191401"
)

fig_metric <- function(metric = NULL){
  metrics <- list(
    size_errorbars =  0.20,
    color_errorbars = "grey30",
    color_errorbars_light = "grey75",
    log1p_axis_breaks_10 = c(0, 10, 100, 1000, 10000),
    point_size = 1,
    color_references = "grey50",
    size_references = 0.25,
    fill_rows = "grey95",
    red_shade = colorRampPalette(
      c(thesis_palette_light[1], thesis_palette[1], thesis_palette_dark[1]),
      bias = 1, space = "rgb", interpolate = "linear", alpha = FALSE)(9),
    blue_shade = colorRampPalette(
      c(thesis_palette_light[2], thesis_palette[2], thesis_palette_dark[2]),
      bias = 1, space = "rgb", interpolate = "linear", alpha = FALSE)(9),
    yellow_shade = colorRampPalette(
      c(thesis_palette_light[3], thesis_palette[3], thesis_palette_dark[3]),
      bias = 1, space = "rgb", interpolate = "linear", alpha = FALSE)(9),
    thesis_palette = thesis_palette,
    thesis_palette_dark = thesis_palette_dark,
    thesis_palette_light = thesis_palette_light
  )

  if(is.null(metric)) return(metrics)
  metrics[[metric]]
}
