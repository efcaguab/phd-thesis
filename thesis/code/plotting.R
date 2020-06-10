# Info about the plotting sizes
fig_sizes <- function(){
  list(
    one_column_width = 3.4,
    two_column_width = 4.28
  )
}


base_ggplot_theme <- function(){
  require(ggplot2)
  theme_bw() +
    theme(text = element_text(size = 10, family = "Helvetica"),
          axis.title = element_text(size = 9),
          plot.title = element_text(size = 9, face = "bold", margin = margin(b = 0)),
          plot.subtitle = element_text(size = 9, hjust = 0),
          plot.tag = element_text(size = 9),
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0),
          legend.title = element_text(hjust = 0.5),
          legend.key.size = unit(3, "mm"),
          legend.margin = margin(),
          axis.ticks.x = element_line(colour = "grey30", size = 0.05),
          axis.ticks.y = element_line(colour = "grey30", size = 0.25),
          # panel.border = element_blank(),
          # axis.line.y = element_line(),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          legend.key = element_rect(fill = NA))
}

defense_theme <- function(){
  require(ggplot2)
  base_ggplot_theme() +
    theme(text = element_text(family = "iwona"),
          axis.title = element_text(family = "iwonalight", size = 20),
          plot.title = element_text(size = 25, hjust = 0.5),
          plot.subtitle = element_text(size = 20, hjust = 0.5),
          plot.caption = element_text(size = 14, family = "iwonalight"),
          panel.border = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          plot.margin = unit(rep(0.5, 4), "cm"))
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
