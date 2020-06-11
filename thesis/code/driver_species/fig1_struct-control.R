make_fig_structural_control <- function(en_direction, pdf_out = NULL){

  # graph attributes
  formatted_en_direction <- en_direction  %>%
    purrr::map(ntw_format_theme)

  formatted_en_direction[[1]] %<>%
    igraph::set_edge_attr("width", value = igraph::E(.)$weight)
  formatted_en_direction[[2]] %<>%
    igraph::set_edge_attr("width", value = 1)

  # adjust_darker_bg <- . %>%
  #   add_property("vertex", "color", "control_type", "type == 'b' ~ get_color('bg_dark')", "TRUE ~ get_color('bg')") %>%
  #   add_property("vertex", "frame.color", "control_type", "type == 'b' ~ get_color('bg_dark')", "TRUE ~ 'black'")
  #
  # formatted_en_structural[[3]] %<>% adjust_darker_bg()
  # formatted_en_structural[[5]] %<>% adjust_darker_bg()

  box_col <- get_color("bg")
  box_col_2 <- get_color("bg_dark")

  heading_height <- 0.20
  # heading_thin_height <- 0.15
  # div_height <- 0.05
  # inner_margin_height <- 0.05
  plot_1_height <- 0.75
  # plot_2_height <- 0.7
  # legend_height <- 0.25
  heights <- c(heading_height, plot_1_height)
  heights <- heights * fig_sizes()$two_column_width/fig_sizes()$one_column_width
  d <- 30
  x <- fig_sizes()$two_column_width/1.096774
  margin_h <- (fig_sizes()$two_column_width-x)
  widths <- c(margin_h, x/3-x/2/d, x/d, x/2-x/3-x/d, x/d, x/2-x/3-x/d/2, x/3,0.001)

  # pdf(pdf_out, width = sum(widths), height = sum(heights), paper='special')
  # if(is.null(pdf_out)) dev.control("enable")

  c(  01,10,10,10,12,11,11,91,
      90,13,13,13,12,14,14,91) %>%
    dplyr::dense_rank() %>%
    matrix(ncol = 8, byrow = T) %>%
    layout(heights = heights,
           widths = widths)
  par(mar = rep(0,4), bg = "white", xpd = NA)

  plot.new()

  # rect(grconvertX(margin_h, "inches", "user"),
  #      grconvertY(sum(heights) - plot_1_height - heading_height, "inches", "user"),
  #      grconvertX( fig_sizes()$two_column_width, "inches", "user"),
  #      grconvertY(sum(heights) , "inches", "user"),
  #      col = box_col,
  #      border = NA)

  # rect(grconvertX(margin_h, "inches", "user"),
  #      grconvertY(legend_height, "inches", "user"),
  #      grconvertX( fig_sizes()$two_column_width, "inches", "user"),
  #      grconvertY(plot_2_height * 2 + div_height + heading_thin_height + legend_height, "inches", "user"),
  #      col = box_col,
  #      border = NA)

  # rect(grconvertX(margin_h, "inches", "user"),
  #      grconvertY(plot_2_height  + legend_height +  heading_thin_height, "inches", "user"),
  #      grconvertX(3.5, "inches", "user"),
  #      grconvertY(plot_2_height * 2 + div_height + heading_thin_height + legend_height, "inches", "user"),
  #      col = box_col_2,
  #      border = NA)

  # rect(grconvertX(margin_h, "inches", "user"),
  #      grconvertY(legend_height, "inches", "user"),
  #      grconvertX( fig_sizes()$two_column_width, "inches", "user"),
  #      grconvertY(plot_2_height + div_height/2+ legend_height, "inches", "user"),
  #      col = box_col_2,
  #      border = NA)


  # 02
  # standalone_text("(b)", y = 0.75)
  # plot.new()
  ## FIGURE A
  # 10
  standalone_text("visitation network", y = 0.25, adj = c(0.5,0), family = "iwonaheavy")
  # text(0, 1, "(a)", adj = c(0.15,1.5), family = "iwona")

  # 11
  standalone_text("direction of control", y = 0.25, adj = c(0.5,0), family = "iwonaheavy")

  # 12
  # standalone_vline(lty = 2)
  plot.new()
  # 13-14
  for(i in 1:length(formatted_en_direction)){
    l <- rescale_layout(direction_layout(), xlim = c(-1, 1) * 1.6, ylim = c(-0.55, 0.7))
    plot_example_ntw(formatted_en_direction[[i]], layout = l)
  }
  plot.new()
}


make_fig_controllability_conditions <- function(en_structural){

  # format networks for plotting
  formatted_en_structural <- en_structural %>%
    purrr::map(ntw_format_theme) %>%
    purrr::map(ntw_control_network_theme)

  heading_thin_height <- 0.15
  div_height <- 0.05
  plot_2_height <- 0.7
  legend_height <- 0.25
  heights <- c(heading_thin_height,
               plot_2_height,
               div_height, plot_2_height,
               legend_height)
  heights <- heights * fig_sizes()$two_column_width/fig_sizes()$one_column_width
  d <- 30
  x <- fig_sizes()$two_column_width/1.096774
  margin_h <- (fig_sizes()$two_column_width-x)
  widths <- c(margin_h, x/3-x/2/d, x/d, x/2-x/3-x/d, x/d, x/2-x/3-x/d/2, x/3,0.001)

  c(#  01,10,10,10,12,11,11,91,
     # 90,13,13,13,12,14,14,91,
      #80,80,80,80,80,80,80,80,
      19,20,20,20,28,21,21,93,
      22,24,24,24,28,25,25,94,
      94,81,81,81,81,81,81,81,
      23,26,26,26,28,27,27,93,
      30,30,30,30,30,30,30,30) %>%
    dplyr::dense_rank() %>%
    matrix(ncol = 8, byrow = T) %>%
    layout(heights = heights,
           widths = widths)
  par(mar = rep(0,4), bg = "white", xpd = NA)

  plot.new()

  ## FIGURE B
  # 20
  standalone_text("dilation", y = 0.5, adj = c(0.5,1), family = "iwona")
  # text(0, 1, "(b)", adj = c(0.15,1.5), family = "iwona")
  # 21
  standalone_text("inaccessible node", y = 0.5, adj = c(0.5,1), family = "iwona")
  # 22
  standalone_text("not\ncontrollable", srt = 90, family = "iwonaheavy")
  # 23
  standalone_text("controllable", srt = 90, family = "iwonaheavy")
  # 24-27
  for (i in c(4,2,5,3)){
    # if(i == 3)   text(-2.26, 1, "(b)", adj = c(0,1.3), family = "iwona")

    types <- igraph::V(formatted_en_structural[[i]])$control_type == dplyr::first(igraph::V(formatted_en_structural[[i]])$control_type)
    l <- igraph::layout_as_bipartite(formatted_en_structural[[i]], types) %>%
      rescale_layout(xlim = c(-1, 1) * 1.55, ylim = c(-0.6, 0.65))
    plot_example_ntw(formatted_en_structural[[i]], layout = l)
  }
  # 28
  # standalone_vline(lty = 2)
  plot.new()

  ## DIV LINES
  # 80
  # standalone_hline()

  # 82
  # standalone_hline(lty = 2)
  # 83
  # standalone_hline(lty = 2)
  plot.new()
  op <- par(family = "iwona")
  legend(0.5, 0.5,legend = c("control input"), horiz = TRUE,
         lty = 1,
         col = get_color("control"),
         lwd = 1.5, cex = 1, xjust=0.5, yjust=0.5, bty = "n")
  plot.new()
  par(op)
}

add_vertex_edge <- function(x, vertex_name, vertex_type, edges_from, edges_to, edges_type){
  x$edges <- x$edges %>%
    rbind(dplyr::data_frame(from = edges_from, to = edges_to, control_type = edges_type))
  x$vertex <- x$vertex %>%
    rbind(dplyr::data_frame(name = vertex_name, control_type = vertex_type))
  x
}


#' Title
#'
#' @param x an incidence matrix or an igraph network. If an igraph network the higher level argument must be specified
#' @param type type of directed links desired. Asymmetry produces a single
#' @param higher_level
#'
#' @return
#' @export
#'
#' @examples
as_directed_network <- function(x,
                                direction = c("asymmetry", "dependence", "top-down", "bottom-up"),
                                ties = c("both", "none", "top-down", "bottom-up"), higher_level){
  # if is an igraph object construct an incidence matrix with the weights
  if(class(x) == "igraph"){
    types <- dplyr::data_frame(name = igraph::V(x)$name, type = igraph::V(x)$type)
    x <- x %>% igraph::as_incidence_matrix(types = igraph::V(.)$type == higher_level, attr = "weight")
  } else {
    types <- dplyr::data_frame(name = c(rownames(x), colnames(x)),
                               type = c(rep(F, nrow(x)), rep(T, ncol(x))))
  }
  if(direction[1] == "top-down"){
    y <- igraph::graph_from_incidence_matrix(x, directed = T, "in", weighted = T)
  } else if(direction[1] == "bottom-up"){
    y <- igraph::graph_from_incidence_matrix(x, directed = T, "out", weighted = T)
  } else if(direction[1] %in% c("asymmetry", "dependence")){
    y <- x %>%
      bipartite::linklevel("dependence")
    if(direction[1] == "asymmetry"){
      y <- y %>% get_assymetry(ties = ties)
    }
    y <- two_incidence_to_adjacency(y)
    y <- igraph::graph_from_adjacency_matrix(y, mode = "directed", weighted = T)
  }
  y %>%
    assign_vertex_types(types)
}

assign_vertex_types <- function(x, types){
  net_names <- dplyr::data_frame(name = igraph::V(x)$name)
  net_name_type <- net_names %>% dplyr::inner_join(types, by = "name")
  igraph::set_vertex_attr(x, "type", value = net_name_type$type)
}

two_incidence_to_adjacency <- function(a){
  x <- a[[1]]
  y <- a[[2]]
  # fill "inverse" diagonal
  adj_matrix <- rbind(
    cbind(matrix(0,nrow(x), nrow(x)), x),
    cbind(t(y), matrix(0, ncol(x), ncol(x))))
  colnames(adj_matrix) <- rownames(adj_matrix)
  adj_matrix
}

get_assymetry <- function(a, ties){
  if(ties[1]=="both"){
    r <- c(1,1)
  } else if(ties[1]=="none"){
    r <- c(0,0)
  } else if(ties[1]=="bottom-up"){
    r <- c(1,0)
  } else if(ties[1]=="top-down"){
    r <- c(0,1)
  }
  list(
    (a[[1]]-a[[2]])/pmax(a[[1]], a[[2]]),
    (a[[2]]-a[[1]])/pmax(a[[1]], a[[2]])
  ) %>%
    purrr::map2(r, replace_zeros) %>%
    purrr::map(replace_negatives) %>%
    purrr::map(replace_nas)

}

replace_negatives <- function(x, y = 0){
  x[x<0] <- y
  x
}

replace_nas <- function(x, y = 0){
  x[is.na(x)] <- y
  x
}

replace_zeros <- function(x,y = 0){
  x[x==0] <- y
  x
}

# common formatting
ntw_format_theme <- function(x, named = T){
  x %<>%
    fancify_vertex_name() %>%
    add_property(element = "edge", attr_name = "color", attr_base = "type", 'TRUE ~ "black"') %>%
    add_property(element = "edge", attr_name = "label.color", attr_base = "type",'TRUE ~ "black"') %>%
    add_property(element = "edge", attr_name = "label.font", attr_base = "type",'TRUE ~ 2') %>%
    add_property(element = "vertex", attr_name = "color",attr_base = "type", "TRUE ~ get_color('base')") %>%
    add_property(element = "vertex", attr_name = "frame.color", attr_base = "type","TRUE ~ 'black'") %>%
    add_property(element = "vertex", attr_name = "size",attr_base = "type", "TRUE ~ 55") %>%
    add_property(element = "vertex", attr_name = "label.cex", attr_base = "type","TRUE ~ 1") %>%
    add_property(element = "vertex", attr_name = "label.color", attr_base = "type","TRUE ~ 'black'") %>%
    add_property(element = "edge",attr_name = "arrow.size", attr_base = "control_type", "TRUE ~ 0.4") %>%
    add_property(element = "edge",attr_name = "arrow.width", attr_base = "control_type", "TRUE ~ 0.7")

  if(!named) x %<>% add_property(element = "vertex", attr_name = "name_fancy", attr_base = "type", 'TRUE ~ NA')
  return(x)

}

#' Make a vertex name a mathematica expression
#'
#' @param x an igraph network with vertex names
#'
#' @return an igraph network
#'
fancify_vertex_name <- function(x){

  n <- igraph::vertex_attr(x, "name") %>%
    purrr::array_branch() %>%
    purrr::map_chr(fancify_vertex_ind_name)

  igraph::V(x)$name_fancy <- n %>%
    latex2exp::TeX()
  x
}

fancify_vertex_ind_name <- function(y){
  if(length(stringr::str_match_all(y, "_")[[1]]) == 1){
    yy <- stringr::str_split(y, "_") %>%
      extract2(1) %>% {
        paste0(.[1], "_{", .[2], "}")
      }
  } else {
    yy <- y
  }
  paste0("$", yy, "$")
}

#' Add a property for vertex or graphs
#'
#' @param x an igraph network
#' @param element either "vertex" or "edge"
#' @param ... named argument. The name of the argument correspond to the new attribute and
#'
#' @return
#'
add_property <- function(x, element, attr_name = "type", attr_base, ...){
  if(element == "vertex"){
    sel <- igraph::set_vertex_attr
    elements <- igraph::V(x)
    get_attr <- igraph::vertex_attr
  } else {
    sel <- igraph::set_edge_attr
    elements <- igraph::E(x)
    get_attr <- igraph::edge_attr
  }
  type <- get_attr(x, attr_base)
  if(is.null(type)) type <- 1:length(elements)
  e <- environment()
  pattern <- list(...) %>% purrr::map(as.formula, e)
  df <- dplyr::data_frame(type = type) %>%
    dplyr::mutate(attr = dplyr::case_when(!!! pattern))
  sel(x, name = attr_name, value = df$attr)

}

get_color <- function(x = T){
  dplyr::case_when(
    x == "matched" ~ fig_metric("red_shade")[4],
    x == "unmatched" ~ fig_metric("red_shade")[6],
    x == "control" ~ fig_metric("blue_shade")[5],
    x == "base" ~ my_pallete()$light,
    x == "dark" ~ "grey10",
    x == "bg" ~ "white",
    x == "bg_dark" ~ "white",
    TRUE ~ "white"
  )
}

my_pallete <- function(){
  list(dark_orange = "#e66101",
       light_orange = "#fdb863",
       light_purple = "#b2abd2",
       extra_light_purple = "#d1cde4",
       dark_purple = "#5e3c99",
       dark_grey = "#636363",
       light_grey = "#bdbdbd",
       light = "#f7f7f7")
}


ntw_control_network_theme <- . %>%
  add_property(element = "edge", attr_name = "color", attr_base ="control_type", 'type== "a" ~ "black"', 'TRUE ~ get_color("control")') %>%
  add_property(element = "edge",attr_name = "lty", attr_base = "control_type", "type == 'a' ~ 1", "TRUE ~ 1") %>%
  add_property(element = "vertex", attr_name = "frame.color", attr_base = "control_type", "type == 'a' ~ 'black'", "TRUE ~ get_color('bg')") %>%
  add_property(element = "vertex", attr_name = "color", attr_base = "control_type", "type == 'b' ~ get_color('bg')", "TRUE ~ get_color('base')") %>%
  add_property(element = "vertex", attr_name = "size",attr_base = "control_type",  "type == 'a' ~ 55", "TRUE ~ 45")

standalone_text <- function(text, x = 0.5, y = 0.5, adj = c(0.5,0.5), font = 1, cex = 1, srt = 0, family = "iwona"){
  plot.new()
  text(x, y, text, adj = adj, font = font, cex = cex, srt = srt, family = family)
}


#' Rescale igraph layout
#'
#' Use with rescale=FALSE in igraph::plot
#'
#' @param layout the layout -a matrix-
#' @param xlim a vector of length two with the minimum and maximum x value
#' @param ylim a vector of length two with the minimum and maximum y value
#'
#' @return the updated layout
#'
rescale_layout <- function(layout, xlim, ylim){
  layout[, 1] <- range02(layout[, 1], xlim[1], xlim[2])
  layout[, 2] <- range02(layout[, 2], ylim[1], ylim[2])
  layout
}


#' Rescale range of a vector
#'
#'
#' @param x vector to be rescaled
#' @param newMin new minimum
#' @param newMax new maximum
#'
#' @return the updated layout
#'
range02 <- function(x, newMin, newMax){
  if(min(x) == max(x)) return(x)
  (x - min(x))/(max(x)-min(x)) * (newMax - newMin) + newMin
}

direction_layout <- function(){
  matrix(c(1,0,
           2,0,
           3,0,
           1.5, 1,
           2.5, 1), nrow = 5, ncol = 2, byrow = T)
}


plot_example_ntw <- function(x, ...){
  plot_examples(x, rescale = F,
                frame = F,
                margin = rep(0, 4),
                edge.arrow.size = 0.4,
                edge.label.family = "sans",
                vertex.label.family = "sans", ...)
}

#' Plot a network using several aesthetic parameters
#'
#' @param x network
#' @param ... parameters for plot.igraph
#'
#'
plot_examples <- function(x, ...){
  igraph::plot.igraph(x,
                      vertex.label = igraph::V(x)$name_fancy,
                      vertex.label.cex = igraph::V(x)$label.cex,
                      vertex.label.color = igraph::V(x)$label.color,
                      vertex.size = igraph::V(x)$size,
                      vertex.color = igraph::V(x)$color,
                      vertex.frame.color = igraph::V(x)$frame.color,
                      vertex.shape = igraph::V(x)$shape,
                      vertex.label.family = "iwona",
                      # vertex.label.color = l_c,
                      edge.width = igraph::E(x)$width,
                      edge.lty = igraph::E(x)$lty,
                      edge.color = igraph::E(x)$color,
                      edge.label = igraph::E(x)$label,
                      edge.label.x = igraph::E(x)$label.x,
                      edge.label.color = igraph::E(x)$label.color,
                      edge.label.font = igraph::E(x)$label.font,
                      edge.arrow.size = igraph::E(x)$arrow.size,
                      edge.arrow.width = igraph::E(x)$arrow.width,
                      # edge.label.family = fam,
                      ...)
}
