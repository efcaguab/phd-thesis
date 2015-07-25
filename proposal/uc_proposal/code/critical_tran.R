library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)
source("../code/multiplot.R")

basis <- dplyr::data_frame(x = seq(-pi/3*2, pi *(5/2), 0.01), 
													 y = sin(x) * x/4)
arrows <- dplyr::mutate(basis, y = y + 0.29)
mini <- 0.35
maxi <- 0.75

arrows_f <- rbind(
	filter(arrows, x < -mini & x > -maxi) %>% mutate(g = "a", col = "s"),
	filter(arrows, x > 2.0256049 + mini & x < 2.0256049 + maxi) %>% mutate(g = "b", col = "u"),
	filter(arrows, x < 4.915605 - mini & x > 4.915605 - maxi) %>% mutate(g = "c", col = "s")
)

arrows_l <- rbind(
	filter(arrows, x > mini & x < maxi) %>% mutate(g = "a", col = "s"),
	filter(arrows, x < 2.0256049 - mini & x > 2.0256049 - maxi) %>% mutate(g = "b", col = "u"),
	filter(arrows, x > 4.915605 + mini & x < 4.915605 + maxi) %>% mutate(g = "c", col = "s")
)

dia <- 0.25
circle <- dplyr::data_frame(x = dia * cos(seq(0,2*pi,length.out=100)), 
														y = dia * sin(seq(0,2*pi,length.out=100)))


sigmoidal <- function (A, D, C, B, t) {
	D + ( (A - D) / (1 + exp( B * ( t - C) )))
}

dia <- 0.25
circle <- dplyr::data_frame(x = dia * cos(seq(0,2*pi,length.out=100)), 
														y = dia * sin(seq(0,2*pi,length.out=100)))

basis2 <- dplyr::data_frame(x = seq(-pi/3*2, pi *(5/2), 0.01), 
														y = sin(x) * x/4) %>%
	mutate(z = sigmoidal(0,1,3.470605,4, x),
				 w = sigmoidal(0,-1,3.470605,1, x),
				 y = y + z * y + w)
basis2$y[basis2$x > basis2$x[basis2$y == min(basis2$y)]] <- min(basis2$y)

arrows2 <- dplyr::mutate(basis2, y = y + 0.29)
mini <- 0.35
maxi <- 0.75

arrows_l2 <- rbind(
	filter(arrows2, x < 0 - 0.4 & x > 0 - 1.8) %>% mutate(g = "b", col = "u"),
	filter(arrows2, x > 0 + 0.25 & x < 0 + 4) %>% mutate(g = "c", col = "s", x = x + 0.2)
)

basis3 <- dplyr::data_frame(x = seq(-pi/3*2, pi *(5/2), 0.01), 
														y = sin(x) * x/4) %>%
	mutate(z = sigmoidal(0,-0.9,3.470605,0.5, x),
				 y = y + z * y)

arrows3 <- dplyr::mutate(basis3, y = y + 0.29)
mini <- 0.35
maxi <- 0.75

arrows_l3 <- rbind(
	filter(arrows3, x < basis3$x[basis3$y == min(basis3$y)] - 0.5 & x > basis3$x[basis3$y == min(basis3$y)] - 3) %>% mutate(g = "b", col = "u", x = x + 0.1),
	filter(arrows3, x > basis3$x[basis3$y == min(basis3$y)] + 0.4 & x < basis3$x[basis3$y == min(basis3$y)] + 2) %>% mutate(g = "c", col = "s")
)

theme_mine <- theme_bw() +
	theme(legend.position = "none",
				text = element_text(family = "Times"),
				plot.margin=unit(c(0,0,0,0),"mm"),
				#panel.margin=unit(c(0,0,0,0),"mm"),
				panel.border = element_blank(),
				axis.text = element_blank(),
				axis.ticks.x = element_blank(),
				axis.ticks.y = element_blank(),
				axis.title = element_blank(),
				plot.title = element_text(hjust = 0.03, vjust = -1.5, size = 9),
				legend.text = element_text(size = 8),
				legend.background = element_blank(),
				legend.key = element_blank(),
				legend.text.align = 0,
				panel.grid = element_blank())

y_lim <- c(min(c(basis$y, basis2$y, basis3$y)),
					 max(c(basis$y, basis2$y, basis3$y)))

p1 <- ggplot(basis) +
	annotate(x = circle$x, y = 0.25 + circle$y, geom = "polygon", fill = "#1f78b4", colour = "grey10") +
	annotate(x = circle$x + 2.0256049, y = 0.4549231 + dia + circle$y, geom = "polygon", fill = "#a6cee3", colour = "grey50", linetype = 1, size = 0.3) +
	annotate(x = circle$x + 4.915605, y = -1.2036136 + dia + circle$y, geom = "polygon", fill = "#1f78b4", colour = "grey10") +
	geom_line(aes(x = x, y = y), size = 0.7) +
	geom_path(data = arrows_f, aes(x = x, y = y, group = g, colour = col), arrow = arrow(length = unit(0.1, "cm"))) +
	geom_path(data = arrows_l, aes(x = x, y = y, group = g, colour = col), arrow = arrow(length = unit(0.1, "cm"), ends = "first")) +
	scale_colour_manual(values = c("gray10", "gray50")) +
  theme_minimal() +
	ylim(y_lim) +
	theme_mine +
	ggtitle("(a)") +
	theme(plot.margin=unit(c(0,0,-15,-5),"mm")) +
	coord_fixed()



p2 <- ggplot(basis2, aes(x = x)) +
	geom_line(data = basis, aes(x = x, y = y), size = 0.5, alpha = 0.5, linetype = 2) +
	annotate(x = circle$x, y = 0.25 + circle$y, geom = "polygon", fill = "#1f78b4", colour = "grey10") +
	annotate(x = circle$x + 5, y = min(basis2$y) + dia + circle$y, geom = "polygon", fill = "#1f78b4", colour = "grey10") +
	geom_line(aes(y = y), size = 0.7) +
	ggtitle("(b)") + 
	annotate("text", x = basis3$x[basis3$y == min(basis3$y)], y = y_lim[2], label = "Business as usual", family = "Times", size = 3, vjust = 1) +
	geom_path(data = arrows_l2, aes(x = x, y = y, group = g, colour = col, size = col), arrow = arrow(length = unit(0.1, "cm"))) +
	scale_colour_manual(values = c("#fb9a99", "#e31a1c")) +
	scale_size_manual(values = c(0.4, 0.7)) +
	theme_minimal() +
	ylim(y_lim) +
	theme_mine +
	coord_fixed()



p3 <- ggplot(basis3, aes(x = x)) +
	geom_line(data = basis, aes(x = x, y = y), size = 0.5, alpha = 0.5, linetype = 2) +
	annotate(x = circle$x, y = 0.25 + circle$y, geom = "polygon", fill = "#1f78b4", colour = "grey10") +
	annotate(x = circle$x + basis3$x[basis3$y == min(basis3$y)], y = min(basis3$y) + dia + circle$y, geom = "polygon", fill = "#1f78b4", colour = "grey10") +
	geom_line(aes(y = y), size = 0.7) +
	ggtitle("(c)") + 
	annotate("text", x = basis3$x[basis3$y == min(basis3$y)], y = y_lim[1], label = "Improved policies\n+ targeted interventions", family = "Times", size = 3, vjust = 0) +
	geom_path(data = arrows_l3, aes(x = x, y = y, group = g, colour = col, size = col), arrow = arrow(length = unit(0.1, "cm"),ends = "first")) +
	scale_colour_manual(values = c("#33a02c", "#b2df8a")) +
	scale_size_manual(values = c(0.7, 0.4)) +
	
	theme_minimal() +
	ylim(y_lim) +
	theme_mine +
	coord_fixed()

pdf("./figures/critical_tran.pdf", width = 5.75 ,height = 4)
multiplot(p1,p2,p3, layout = matrix(c(1,1,2,3),ncol = 2, byrow = T))
dev.off()