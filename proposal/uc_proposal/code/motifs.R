library(ggplot2)
library(sna)
library(Hmisc)
library(reshape2)
library(dplyr)
library(magrittr)
library(igraph)

source("../code/multiplot.R")

################## Functions ##########################


get_net <- function (adjacencyMatrix) {
	
	rownames(adjacencyMatrix) <- 1:n_pla
	colnames(adjacencyMatrix) <- (n_pla + 1):(n_pla + n_pol)
	layoutCoordinates <- layout.bipartite(graph.incidence(adjacencyMatrix))  # Get graph layout coordinates
	colnames(layoutCoordinates) <- c("x", "y")
	# 
	# data(coleman)  # Load a high school friendship network
	# adjacencyMatrix <- coleman[1, , ]  # Fall semester
	# layoutCoordinates <- gplot(adjacencyMatrix)  # Get graph layout coordinates
	
	# Function to generate paths between each connected node
	edgeMaker <- function(whichRow, len = 100, curved = TRUE){
		fromC <- layoutCoordinates[adjacencyList[whichRow, 1], ]  # Origin
		toC <- layoutCoordinates[adjacencyList[whichRow, 2], ]  # Terminus
		
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
		edge$Group <- paste(adjacencyList[whichRow, 1:2], collapse = ">")
		return(edge)
	}
	
	adjacencyList <- melt(adjacencyMatrix)  # Convert to list of ties only
	adjacencyList <- adjacencyList[adjacencyList$value > 0, ]
	
	# Generate a (curved) edge path for each pair of connected nodes
	allEdges <- lapply(1:nrow(adjacencyList), edgeMaker, len = 500, curved = FALSE)
	allEdges <- do.call(rbind, allEdges)  # a fine-grained path ^, with bend ^
	
	nodes <- data.frame(layoutCoordinates) 
	
	return(list(edges = allEdges, nodes = nodes))
}


plot_motif <- function (m, tit = "") {
	
	ggplot(m$edges) +  # Pretty simple plot code
		geom_path(aes(x = x, y = y, group = Group), size = 0.7) + # and taper
		geom_point(data = m$nodes,  # Add nodes
							 aes(x = x, y = y, fill = as.factor(y)), size = 5, pch = 21,
							 colour = "black") +  # Customize gradient v														)) +
		scale_fill_manual(values = c("#a6cee3", "#1f78b4")) +
		scale_x_continuous(expand = c(0.1,0.1)) +
		scale_y_continuous(expand = c(0.1,0.1)) +
		ggtitle(tit) +
		# 	coord_flip() +
		#scale_size(range = c(1/10, 1), guide = "none")  + # Customize taper
		my_theme + # Clean up plot 
		coord_fixed() +
		xlab("") + ylab("")
	
}

######################## theme ##################################

my_theme <- theme_bw() +
	#scale_color_brewer(palette = "Paired") +
	theme(legend.position = "none",
				text = element_text(family = "Times"),
				plot.margin=unit(c(0,0,0,0),"mm"),
				#panel.margin=unit(c(0,0,0,0),"mm"),
				panel.border = element_blank(),
				axis.text = element_blank(),
				axis.ticks.x = element_blank(),
				axis.ticks.y = element_blank(),
				axis.title = element_text(size = 9),
				plot.title = element_text(hjust = 0.05, vjust = -1.5, size = 9),
				panel.grid = element_blank())


################# motivs ################################

n_pla <- 2
n_pol <- 2

adjacencyMatrix <-   # Fall semester

m1 <- get_net(matrix(c(1,0,0,1), n_pla, n_pol))
m2 <- get_net(matrix(c(1,1,0,1), n_pla, n_pol))
m3 <- get_net(matrix(c(1,1,1,1), n_pla, n_pol))

################### plots #########################

p1 <- plot_motif(m1)
p2 <- plot_motif(m2)
p3 <- plot_motif(m3)

pdf("./figures/motifs.pdf", width = 5.75 ,height = 1)
multiplot(p3, p2, p1, cols = 3)
dev.off()

