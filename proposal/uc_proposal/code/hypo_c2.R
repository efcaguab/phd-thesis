library(ggplot2)
library(grid)
library(dplyr)
library(tidyr)
source("../code/multiplot.R")

sigmoidal <- function (A, D, C, B, t) {
	D + ( (A - D) / (1 + exp( B * ( t - C) )))
}

theme_mine <- theme_bw() +
	theme(legend.position = "top",
				legend.direction = "horizontal", 
				text = element_text(family = "Times"),
				plot.margin=unit(c(0,0,0,0),"mm"),
				#panel.margin=unit(c(0,0,0,0),"mm"),
				#panel.border = element_blank(),
				axis.text = element_blank(),
				axis.ticks.x = element_blank(),
				axis.ticks.y = element_blank(),
				axis.title = element_text(size = 9),
				plot.title = element_text(hjust = -0.1, vjust = -1.5, size = 9),
				legend.text = element_text(size = 8),
				legend.title = element_blank(),
				legend.background = element_blank(),
				legend.key = element_blank(),
				legend.text.align = 0,
				panel.grid = element_blank())

################### hystheresis ############################

C <- 20
B <- 10

fc <- data_frame(x = seq(-25,25, 0.01),
					 y1 = sigmoidal(1,-1,C,B, x),
					 y2 = sigmoidal(1,-1,-C,B, x)) %>%
	gather(l, y, -x) %>%
	mutate(type = "m. r.   ")

ne <- data_frame(x = seq(-25,25, 0.01),
								 y1 = sigmoidal(1,-1,C/2,B/exp(2)/3, x),
								 y2 = sigmoidal(1,-1,-C/2,B/exp(2)/3, x)) %>%
	gather(l, y, -x) %>%
	mutate(type = "n.   ")

co <- data_frame(x = seq(-25,25, 0.01),
								 y1 = sigmoidal(1,-1,C/2/6,B/exp(2)/6, x),
								 y2 = sigmoidal(1,-1,-C/2/6,B/exp(2)/6, x)) %>%
	gather(l, y, -x) %>%
	mutate(type = "n + c. p.   ")

hyst <- rbind(fc, ne, co)

arr <- hyst %>%
	filter(y > -0.05, y < 0.05)


################### coexistence ########################

no <- data_frame(x = seq(0,1, 0.01),
								 yp = dnorm(x, 0.5, 1/4),
								 yo = x + 0.5 ) %>%
	gather(l, y, -x) %>%
	mutate(l = as.character(l)) %>%
	mutate(l = replace(l, l =="yp", "comp. for pollination    ")) %>%
	mutate(l = replace(l, l =="yo", "standard models    "))

##################### plots #########################

p1 <- ggplot(data = no, aes(x = x, y = y)) +
	geom_line(aes(colour = l), size = 0.7) +
	scale_color_manual(values = c("#1f78b4", "#a6cee3")) +
	xlab("connectance / global redundancy") +
	ylab("species on stable coexistence") +
	geom_segment(x = 0.5, xend = 0.5, y = 0.5, yend = 0.8, arrow = arrow(length = unit(0.1, "cm")), size = 0.4) +
	annotate(geom = "text", label = "maximally nested", x = 0.5, y = 0.35, family = "Times", size = 3) +
	ggtitle("(a)") +
	theme_mine +
	theme(plot.margin=unit(c(0,3,0,0),"mm"))
	

p2 <- ggplot(data = hyst, aes(x = x, y = y)) +
	geom_line(aes(colour = type, group = interaction(type, l)), size = 0.7) +
	scale_color_manual(values = c("#e31a1c", "#33a02c", "#1f78b4")) +
	ylab("ecosystem state") +
	xlab("driver of decline") +
	geom_path(data = arr %>% filter(l == "y1"), aes(x = x, y = y, group = interaction(type, l), colour = type), arrow = arrow(length = unit(0.2, "cm"))) +
	geom_path(data = arr %>% filter(l == "y2"), aes(x = x, y = y, group = interaction(type, l), colour = type), arrow = arrow(length = unit(0.2, "cm"), ends = "first")) +
	ggtitle("(b)") +
	theme_mine +
	theme(plot.margin=unit(c(0,0,0,3),"mm"))

pdf("./figures/hypo_c2.pdf", width = 5.75 ,height = 2.53)
multiplot(p1, p2, cols =2)
dev.off()