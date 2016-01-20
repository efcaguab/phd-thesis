library(ggplot2)
library(dplyr)
library(magrittr)
library(readr)
library(grid)
library(scales)

# read imput file
chart <- read_csv("./code/schedule.csv")

ids <- chart %>% distinct(name) %>%
	mutate(id = 1:nrow(.)) %>%
	select(name, id)

chart <- inner_join(chart, ids) %>%
	mutate(name = factor(name, rev(unique(name))),
				 category = factor(category, unique(category)),
				 completed = replace(completed, completed == "", NA))

categ <- chart %>%
	group_by(category) %>%
	summarise(min = min(as.numeric(name)),
						max = max(as.numeric(name))) %>%
	mutate(even = 1:nrow(.) %% 2, 
				 even = plyr::mapvalues(even, c(0,1), c("white", "grey"))) %>%
	rowwise() %>%
	mutate(m = (min + max)/2)
	
mytheme <- theme_bw() + 
	theme(
		legend.position = "none",
		legend.text.align = 0,
		legend.margin = unit(-1.1, "cm"),
		legend.direction = "vertical",
		legend.box.just = "top", 
		legend.background = element_blank(),
		legend.key = element_rect(colour = "white"),
		# Change font
				text = element_text(family = "Times", size = 11),
				plot.margin=unit(c(0,0,0,-2),"mm"),
				axis.title = element_blank(),
				panel.border = element_blank(),
				panel.grid.major.y = element_line(colour = "grey80", linetype = 2, size = 0.2),
				panel.grid.major.x = element_line(colour = "grey80", linetype = 2, size = 0.2),
				panel.grid.minor.x = element_line(colour = "grey90", linetype = 2, size = 0.1),
				axis.ticks = element_blank(),
				plot.title = element_text(hjust = 0.05, vjust = -1.5, size = 9)
				)

p <- ggplot() +
	geom_point(data = chart,
						 aes(x = mean, y = name), size = 0.5) +
	annotate(geom = "rect", xmin = as.Date("2014-12-31"), 
					 xmax =as.Date("2019-07-15"), ymin = categ$min-0.5, 
					 ymax = categ$max+0.5, fill = categ$even, alpha = 0.15) + 
	annotate(geom = "text", x = as.Date ("2019-06-15"), y = categ$m, label = categ$category, 
					 family = "Times", angle = -90, size = 3.5) + 
	geom_rect(data = filter(chart, type == "line"),
						aes(xmin = start, xmax = end, ymin = as.numeric(name) - 0.25,
								ymax = as.numeric(name) + 0.25, fill = completed),
						colour = "black", show.legend = FALSE) +
	geom_point(data = filter(chart, type == "dot"),
						 aes(x = mean, y = name, fill = completed), shape = 23, size = 3) +
	# Change fill colours
	scale_fill_manual("", values = c("#a6cee3"), na.value = "#1f78b4") + 
	guides(fill = guide_legend(label.position = "right")) +
	scale_x_date(expand = c(0,0), labels = date_format("%b. %Y"), minor_breaks =  date_breaks("months")) + 
	mytheme

# Save as pdf
# pdf("./figures/schedule.pdf", width = 9.3, height = 5.3)
pdf("./figures/schedule.pdf", width = 5.85, height = 5.85)
p
dev.off()
