library(ggplot2)
library(grid)
library(magrittr)
source("../code/multiplot.R")

sigmoidal <- function (A, D, C, B, t) {
	D + ( (A - D) / (1 + exp( B * ( t - C) )))
}

len <- 100

theme_mine <- theme_bw() +
	theme(legend.position = "none",
				text = element_text(family = "Times"),
				plot.margin=unit(c(0,0,0,0),"mm"),
				panel.margin=unit(c(0,0,0,0),"mm"),
				panel.border = element_rect(colour = "grey10", size = 0.5),
				axis.text = element_blank(),
				axis.ticks.x = element_blank(),
				axis.ticks.y = element_blank(),
				axis.title = element_text(size = 9),
				plot.title = element_text(hjust = 0.03, vjust = -1.5, size = 9),
				legend.text = element_text(size = 8),
				legend.background = element_blank(),
				legend.key = element_blank(),
				legend.text.align = 0,
				panel.grid = element_blank())

p1 <- dplyr::data_frame(x = rep(1:len, 2),
									type = rep(c("pool", "a_no_pool"), each = len),
									inv = c(log((x[1:len]*1.5)^2.5)*(x[1:len])^0.01 + 10, log(x[1:len]*0.5) + 10)) %>%
	dplyr::filter(type == "pool") %>%
	ggplot() +
	geom_line(aes(x = x, y = inv, colour = type), size = 0.7) +
	ylim(5,25) +
	scale_color_manual(values = c("#1f78b4", "#a6cee3")) +
	theme_mine +
# 	theme(plot.margin=unit(c(0,4,0,0),"mm")) +
	xlab("nestedness") +
	ylab("invasibility") +
	ggtitle("(a)")

p2 <- dplyr::data_frame(x = rep(1:len, 2),
									type = rep(c("pool", "no_pool"), each = len),
									inv = c(sigmoidal(1,0,-20,0.04,x[1:len]),sigmoidal(1,0,-20,0.08,x[1:len]))) %>%
	dplyr::filter(type == "pool") %>%
	ggplot() +
	geom_line(aes(x = x, y = inv, colour = type), size = 0.7) +
	ylim(-0.03,0.37) +
	scale_color_manual(values = c("#1f78b4", "#a6cee3"), name = "", labels = c("facilitation + competition", "facilitation")) +
	theme_mine +
	# theme(legend.position = c(0.67, 0.85)) +
	xlab("compartmentalization") +
	ylab("") +
	ggtitle("(b)")

pdf("./figures/hypo_c1.pdf", width = 5.75 ,height = 1.6)
multiplot(p1, p2, cols = 2)
dev.off()