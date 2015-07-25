library(sna)
library(ggplot2)
library(Hmisc)
library(reshape2)
library(dplyr)
library(magrittr)
library(igraph)
source("../code/multiplot.R")

#################### NETWORK ###########################

# Empty ggplot2 theme
new_theme_empty <- theme_bw()
new_theme_empty$line <- element_blank()
new_theme_empty$rect <- element_blank()
new_theme_empty$strip.text <- element_blank()
new_theme_empty$axis.text <- element_blank()
new_theme_empty$plot.title <- element_blank()
new_theme_empty$axis.title <- element_blank()
new_theme_empty$legend.position <- "none"
new_theme_empty$plot.margin <- unit(c(0,0,0,0),"mm")

n_pla <- 4
n_pol <- 5

set.seed(1)
adjacencyMatrix <- matrix(sample(0:1, n_pla * n_pol, replace=TRUE, prob=c(1,1)), n_pla, n_pol)  # Fall semester
rownames(adjacencyMatrix) <- 1:n_pla
colnames(adjacencyMatrix) <- (n_pla + 1):(n_pla + n_pol)
layoutCoordinates <- layout.bipartite(graph.incidence(adjacencyMatrix))  # Get graph layout coordinates
colnames(layoutCoordinates) <- c("x", "y")
# 
# data(coleman)  # Load a high school friendship network
# adjacencyMatrix <- coleman[1, , ]  # Fall semester
# layoutCoordinates <- gplot(adjacencyMatrix)  # Get graph layout coordinates


adjacencyList <- melt(adjacencyMatrix)  # Convert to list of ties only
adjacencyList <- adjacencyList[adjacencyList$value > 0, ]

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

# Generate a (curved) edge path for each pair of connected nodes
allEdges <- lapply(1:nrow(adjacencyList), edgeMaker, len = 500, curved = FALSE)
allEdges <- do.call(rbind, allEdges)  # a fine-grained path ^, with bend ^

allEdges %<>%
	mutate(Group = factor(Group),
				 type = Group)

new_lev <- c("n", "c", "n", "c", "n", "c", "n", "c", "n")
levels(allEdges$type) <- new_lev

nodes <- data.frame(layoutCoordinates) %>%
	mutate(cont = rep(c("n", "c", "n", "n"), c(5,2,1,1)),
				 spe = 1:n())


################################### DYNAMICS #####################################

sigmoidal <- function (A, D, C, B, t) {
	D + ( (A - D) / (1 + exp( B * ( t - C) )))
}

normal <- function(A, D, C, B, t, mod = 1, sda = 50) {
	if (D == 0) return(0)
	if (mod==1){
		if (D > A & runif(1) < 0.3) return(0)
		if (D < A & runif(1) < 0.7) return(0)
	}
	mod <- mod * runif(1, 0.8, 1.2)
	sda <- sda * runif(1, 0.8, 1.2)
	10 * (D-A) * mod * dnorm(t, mean = C + C * B, sd = C / B / sda)
}

dynamics <- function (param, t, mod = 1, sda = 50) {
	plyr::ddply(param, "species", function(x){
		data.frame(p = sigmoidal(x$lower[1], 
														 x$upper[1],
														 x$inflection[1],
														 x$slope[1], 
														 t), 
							 n = normal(x$lower[1], 
							 					 x$upper[1],
							 					 x$inflection[1],
							 					 x$slope[1], 
							 					 t, mod, sda),
							 t = t)
	})
}
set.seed(3)
n_species <- n_pol + n_pla

lower <- runif(n_species, 0.2, 0.8)

set.seed(5)
t <- seq(0,800, 1)

shift_param <- dplyr::data_frame(species = 1:n_species,
																 lower = lower,
																 upper = c(runif(n_pla-1, 0.7, 1), runif(n_pol-1, 0,0.3), runif(2, 0.3, 0.7)), 
																 inflection = runif(n_species, 400,500),
																 slope = runif(n_species, 0.065,0.095)) %>%
	dplyr::rowwise() %>%
	dplyr::mutate(lower = max(0, lower),
								upper = max(0, upper))
# 

shi_nat <- dynamics(shift_param, t) %>%
	dplyr::mutate(type = "native")


# shi_inv <- data.frame(species = n_species + 1,
# 											lower = 0.025, 
# 											upper = runif(1, 0.8, 1),
# 											inflection = 400,
# 											slope = 0.075) %>% 
# 	dynamics(t = 250:1000) %>%
# 	dplyr::mutate(type = "z")
t <- seq(801,2000, 1)
set.seed(3)
shift_param2 <- shift_param %>%
	mutate(prev = lower,
				 lower = upper,
				 upper = prev + runif(n(), -0.15, 0.15), 
				 inflection = runif(n(), 1400,1500),
				 slope = slope-0.02)

shift_param2$slope[shift_param2$species == 1 | shift_param2$species == 6] <- runif(2, 0.07, 0.15)
shift_param2$inflection[shift_param2$species == 1 | shift_param2$species == 6] <-
	shift_param2$inflection[shift_param2$species == 1 | shift_param2$species == 6] - 200


shi_nat2 <- dynamics(shift_param2, t) %>%
	dplyr::mutate(type = "native")

shi_nat <- rbind(shi_nat, shi_nat2) %>%
	arrange(species, t)
#shi <- rbind(shi_nat, shi_inv)

#shi_nat %<>% mutate(pp = "normal")
shi_nat$pp[shi_nat$species == 1 & shi_nat$t >= 1200 & shi_nat$t <= 1360] <- "intervention"
shi_nat$pp[shi_nat$species == 6 & shi_nat$t >= 1200 & shi_nat$t <= 1360] <- "intervention"


################################################### PLOTS ########

my_theme <- theme_bw() +
	#scale_color_brewer(palette = "Paired") +
	theme(legend.position = "none",
				text = element_text(family = "Times"),
				plot.margin=unit(c(-4,0,0,0),"mm"),
				panel.margin=unit(c(0,0,0,0),"mm"),
				panel.border = element_rect(colour = "grey10", size = 0.5),
				axis.text = element_blank(),
				axis.ticks.x = element_blank(),
				axis.ticks.y = element_blank(),
				axis.title = element_text(size = 9),
				plot.title = element_text(hjust = 0.05, vjust = -1.5, size = 9),
				panel.grid = element_blank())


p1 <- ggplot(allEdges) +  # Pretty simple plot code
	geom_path(aes(x = x, y = y, group = Group, alpha = type), size = 0.7) + # and taper
	geom_point(data = nodes,  # Add nodes
						 aes(x = x, y = y, fill = as.factor(spe)), size = 5, pch = 21,
						 colour = "black") +  # Customize gradient v
	#scale_colour_gradient(low = gray(0), high = gray(9/10), guide = "none") +
	#scale_fill_manual(values = c("#33a02c", "#1f78b4")) +
#   scale_fill_manual(values = c(
# 																"#084594", 
# 																"#2171b5", "#4292c6", "#6baed6", "#9ecae1", 
# 																"#238b45",  #green
# 																"#74c476",  # green
# 																"#c6dbef", "#deebf7"
# 																)) +
	scale_fill_manual(values = c(
		"#a6cee3", 
		"#a6cee3", "#a6cee3", "#a6cee3", "#a6cee3", 
		"#1f78b4",  #green
		"#1f78b4",  # green
		"#a6cee3", "#a6cee3"
	)) +
	scale_x_continuous(expand = c(0.3,0.3)) +
	scale_y_continuous(expand = c(0.4,0.4)) +
	scale_alpha_manual(values = c(0.4, 1)) +
	ggtitle("(a)") +
	# 	coord_flip() +
	#scale_size(range = c(1/10, 1), guide = "none")  + # Customize taper
	my_theme + # Clean up plot 
	theme(plot.margin=unit(c(-4,4,0,0),"mm")
				# panel.border = element_blank()
				) +
	xlab("") + ylab("")

p2 <- shi_nat %>%
	ggplot(aes(x = t, y = p + n)) +
	annotate("rect", xmin = 520, xmax = 1360, ymin = -Inf, ymax = Inf, fill = "grey90") +
	geom_line(aes(colour = pp, group = as.factor(species), size = as.factor(species))) +
	geom_segment(aes(x = 1300, xend = 1300, y = 1.05, yend = 0.7), 
							 arrow = arrow(length = unit(0.1, "cm"), type = "closed", angle = 20),
							 size = 0.3, colour = "grey10")+
	annotate("text", x = 1300, y = 1.12, label = "targeted intervention", family = "Times", size = 3, colour = "grey10") +
# 	geom_segment(aes(x = 400, xend = 400, y = 1.05, yend = 0.7), 
# 							 arrow = arrow(length = unit(0.1, "cm"), type = "closed", angle = 20),
# 							 size = 0.3, colour = "grey50")+
# 	annotate("text", x = 400, y = 1.12, label = "perturbation", family = "Times", size = 2.5, colour = "grey30") +
	my_theme +
# 	scale_color_manual(values = c("#74c476",  # green
# 																"#084594", "#2171b5", "#4292c6", "#6baed6", 
# 																"#238b45",  #green
# 																"#9ecae1", "#c6dbef", "#deebf7"
# 	)) +
# 	scale_color_manual(values = c("#1f78b4",  # green
# 																"#a6cee3", "#a6cee3", "#a6cee3", "#a6cee3", 
# 																"#1f78b4",  #green
# 																"#a6cee3", "#a6cee3", "#a6cee3"
# 	)) +
	scale_colour_manual(values = c("#1f78b4"), na.value =  "#a6cee3") +
	scale_size_manual(values = rep(c(0.7, 0.4, 0.7, 0.4), c(1, 4, 1, 3))) +
	xlab("time") +
	ylab("abbundance") +
	ylim(0, 1.2) + 
	ggtitle("(b)")

pdf("./figures/control_net.pdf", width = 5.75 ,height = 1.92)
multiplot(p1, p2, cols = 2)
dev.off()
