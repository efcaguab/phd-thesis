library(ggplot2)
library(dplyr)
library(magrittr)
library(readr)
library(grid)

chart <- read_csv("uc_proposal/code/schedule.csv")
ids <- chart %>% distinct(name) %>%
	mutate(id = 1:nrow(.)) %>%
	select(name, id)

chart <- inner_join(chart, ids) %>%
	mutate(name = factor(name, rev(unique(name))),
				 category = factor(category, unique(category)))

categ <- chart %>%
	group_by(category) %>%
	summarise(min = min(as.numeric(name)),
						max = max(as.numeric(name))) %>%
	mutate(even = 1:nrow(.) %% 2, 
				 even = plyr::mapvalues(even, c(0,1), c("white", "grey"))) %>%
	rowwise() %>%
	mutate(m = (min + max)/2)

# chart %<>%
# 	mutate(start = as.Date(start, format = "%Y-%m-%d"), 
# 				 end = as.Date(end))
	
mytheme <- theme_bw() + 
	theme(legend.position = "none",
				text = element_text(family = "Times", size = 9),
				plot.margin=unit(c(0,0,0,0),"mm"),
				panel.margin=unit(c(0,0,0,0),"mm"),
				axis.title = element_blank(),
				panel.border = element_blank(),
				panel.grid.major.y = element_line(colour = "grey80", linetype = 2, size = 0.2),
				panel.grid.major.x = element_line(colour = "grey80", linetype = 2, size = 0.2),
				panel.grid.minor.x = element_line(colour = "grey90", linetype = 2, size = 0.1),
				axis.ticks = element_blank(),
				# axis.text = element_blank(),
				# axis.ticks.x = element_blank(),
				# axis.ticks.y = element_blank(),
				# axis.title = element_text(size = 9),
				plot.title = element_text(hjust = 0.05, vjust = -1.5, size = 9)
				)

pdf("./uc_proposal/figures/schedule.pdf", width = 9.3, height = 5.89)
ggplot() +
	geom_point(data = chart,
					 aes(x = mean, y = name), shape = 23, fill = "#1f78b4", size = 2.5) +
	annotate(geom = "rect", xmin = as.Date("2015-01-01"), 
					 xmax =as.Date("2018-05-15"), ymin = categ$min-0.5, 
					 ymax = categ$max+0.5, fill = categ$even, alpha = 0.2) + 
	annotate(geom = "text", x = as.Date ("2018-04-15"), y = categ$m, label = categ$category, 
					 family = "Times", angle = 90, size = 3) + 
	geom_rect(data = filter(chart, type == "line"),
						 aes(xmin = start, xmax = end, ymin = as.numeric(name) - 0.15,
						 		ymax = as.numeric(name) + 0.15),
						fill = "#1f78b4", colour = "black") +
	geom_point(data = filter(chart, type == "dot"),
						 aes(x = mean, y = name), shape = 23, fill = "#1f78b4", size = 2.5) +
	scale_x_date(expand = c(0,0)) + 

	mytheme
dev.off()
