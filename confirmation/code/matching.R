library(magrittr)
library(ggplot2)
library(Hmisc)

source("../proposal/code/multiplot.R")

my_theme <- theme_bw() +
	#scale_color_brewer(palette = "Paired") +
	theme(legend.position = "none",
				text = element_text(family = "Times"),
				plot.margin=unit(c(4,0,-4,0),"mm"),
				panel.margin=unit(c(0,0,0,0),"mm"),
				# panel.border = element_rect(colour = "grey10", size = 0.5),
				panel.border = element_blank(),
				axis.text = element_blank(),
				axis.ticks.x = element_blank(),
				axis.ticks.y = element_blank(),
				axis.title = element_text(size = 9),
				plot.title = element_text(hjust = 0.05, vjust = -1.5, size = 9),
				panel.grid = element_blank())

# Function to generate paths between each connected node
edgeMaker <- function(whichRow, len = 100, curved = TRUE){
	fromC <- layoutCoordinates[adj_list[whichRow, 1], ]  # Origin
	toC <- layoutCoordinates[adj_list[whichRow, 2], ]  # Terminus
	
	# Add curve:
	graphCenter <- colMeans(layoutCoordinates)  # Center of the overall graph
	bezierMid <- c(fromC[1], toC[2])  # A midpoint, for bended edges
	distance1 <- sum((graphCenter - bezierMid)^2)
	if(distance1 < sum((graphCenter - c(toC[1], fromC[2]))^2)){
		bezierMid <- c(toC[1], fromC[2])
	}  # To select the best Bezier midpoint
	bezierMid <- (fromC + toC + bezierMid) / 3  # Moderate the Bezier midpoint
	if(curved == FALSE){bezierMid <- (fromC + toC) / 2}  # Remove the curve
	
	edge <- data.frame(bezier(c(fromC[1], bezierMid[1], toC[1]),  # Generate
														c(fromC[2], bezierMid[2], toC[2]),  # X & y
														evaluation = len))  # Bezier path coordinates
	edge$Sequence <- 1:len  # For size and colour weighting in plot
	edge$Group <- paste(adj_list[whichRow, 1:2], collapse = ">")
	return(edge)
}


sp_names <- c("a[1]", "a[2]", "a[3]", "p[1]", "p[2]")

adj_matrix <- c(0,0,0,1,0,
								0,0,0,1,1,
								0,0,0,0,0,
								0,0,1,0,0,
								0,0,1,0,0) %>%
	matrix(nrow = 5, byrow = T) %>%
	`rownames<-`(sp_names) %>%
	`colnames<-`(sp_names)

net <- adj_matrix %>% 
	igraph::graph_from_adjacency_matrix(mode = "directed") 
igraph::V(net)$type <- c(F, F, F, T, T)
igraph::E(net)$weight <- 1

layoutCoordinates <- cbind(c(0,1,2,0.5,1.5), 
													 c(1,1,1,0,0)) %>%
	`colnames<-`(c("x", "y"))

adj_list <- adj_matrix %>% 
	reshape2::melt() %>% 
	dplyr::filter(value > 0)

# Generate an edge path for each pair of connected nodes
n <- 17
allEdges <- lapply(1:nrow(adj_list), edgeMaker, len = n, curved = FALSE)
allEdges <- do.call(rbind, allEdges) %>%
	dplyr::filter(Sequence != n) %>%
	dplyr::mutate(Group = factor(Group),
				 type = Group)

new_lev <- c("m", "u", "m", "m", "u")
levels(allEdges$type) <- new_lev

nodes <- data.frame(layoutCoordinates) %>%
	dplyr::mutate(spe = sp_names,
								spe_c = c("u", "u", "m", "m", "m"), 
								pos = rep(c(0.25, -0.25), c(3,2)))

p1 <- ggplot(allEdges) +  # Pretty simple plot code
	geom_path(aes(x = x, y = y, group = Group, colour = type, size = type), 
						arrow = arrow(type = "closed", angle = 15, length = grid::unit(0.1, "inches"))) + # and taper
	geom_point(data = nodes,  # Add nodes
						 aes(x = x, y = y , fill = spe_c), size = 5, pch = 21,
						 colour = "black") + 
	geom_text(data = nodes, aes(x = x, y = y + pos, label = spe), size = 3, parse = T) +
	scale_fill_manual(values = c("#bdbdbd", "white")) + 
	scale_colour_manual(values = c("#bdbdbd", "black")) +
	my_theme + xlab("") + ylab("") + ylim(c(-0.27, 1.26)) +
 theme(plot.margin=unit(c(4,4,-4,-4),"mm")) +
	scale_size_manual(values = c(0.7, 0.4))

			
source("~/github/driver-species/code/functions/digraph_bipartite.R")
	
net <- digraph_bipartite(net)
adj_matrix <- net %>% igraph::as_adjacency_matrix(sparse = F, type = "upper")
layoutCoordinates <- cbind(rep(0:4, 2), rep(c(1,0), each = 5)) %>%
	`colnames<-`(c("x", "y"))

adj_list <- adj_matrix %>% 
	reshape2::melt() %>% 
	dplyr::filter(value > 0)

# Generate an edge path for each pair of connected nodes
n <- 100
allEdges <- lapply(1:nrow(adj_list), edgeMaker, len = n, curved = FALSE)
allEdges <- do.call(rbind, allEdges) %>%
	dplyr::filter(Sequence != n,
								Sequence != 1) %>%
	dplyr::mutate(Group = factor(Group),
				 type = Group)

new_lev <- c("m", "m", "u", "m", "u")
levels(allEdges$type) <- new_lev

nodes <- data.frame(layoutCoordinates) %>%
	dplyr::mutate(spe = rep(sp_names, 2),
								spe_l = c(paste0(sp_names, " %->% phantom(0)"), paste0(sp_names, " %<-% phantom(0)")),
								spe_c = c(rep("u",5), c("u", "u", "m", "m", "m")), 
								pos = rep(c(0.25, -0.25), each = 5))

p2 <- ggplot(allEdges) +  # Pretty simple plot code
	geom_path(aes(x = x, y = y, group = Group, colour = type, size = type)) + # and taper
	geom_point(data = nodes,  # Add nodes
						 aes(x = x, y = y, fill = spe_c), size = 5, pch = 21,
						 colour = "black") + 
	geom_text(data = nodes, aes(x = x + 0.08, y = y + pos, label = spe_l), parse = T, size = 3) +
	# coord_flip()  + scale_x_reverse() + scale_y_reverse() +
	scale_fill_manual(values = c("#bdbdbd", "white")) + 
	scale_colour_manual(values = c("#bdbdbd", "black")) +
	xlim(c(-0.12, 4.12)) + ylim(c(-0.27, 1.26)) +
	my_theme + xlab("") + ylab("") +
	scale_size_manual(values = c(0.7, 0.4))

pdf("./figures/control_net.pdf", width = 5.75 ,height = 1.35)
multiplot(p1, p2, layout = matrix(nrow = 1, c(1,1,1,2,2,2,2,2)))
dev.off()

