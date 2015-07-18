library(ggplot2)
library(dplyr)
library(magrittr)
library(knitr)
library(paco)
library(reshape2)
library(grid)

source("../code/multiplot.R")

size <- 6
mod_size <- 3
my_seed <- 8
cscale <- "Dark2"

# modular network
dect_mod <- function(x, y) {
	if(as.character(x) == as.character(y)) return(x)
	return(NA_integer_)
} 

modu <- expand.grid(pla = 1:size, 
										pol = 1:size) %>%
	mutate(pla_mod = cut(pla, mod_size),
				 pol_mod = cut(pol, mod_size), 
				 int = pla_mod == pol_mod) %>%
	rowwise() %>%
	mutate(mod = dect_mod(pla_mod, pol_mod)) %>%
	rowwise() %>%
	mutate(pla_name = LETTERS[pla],
				 pol_name = letters[pol])


# set.seed(my_seed)
# modu$int[sample(nrow(modu), (size/mod_size)^(2))] <- TRUE
# modu$int[sample(nrow(modu), (size/mod_size)^(2))] <- FALSE

# nested network
size <- 8
nest <- expand.grid(pla = 1:size, 
										pol = 1:size) %>%
	mutate(pla_name = LETTERS[pla],
				 pol_name = letters[pol]) %>%
	rowwise() %>%
# 	mutate(ppla = plogis(runif(1, - size, pla^2)),
# 				 ppol = plogis(runif(1, - size, pol^2)))
	mutate(nest = round(sqrt(2*size^2) * dnorm(sqrt(pla^2 + pol^2), sqrt(2*size^2)/2, size/5)),
				 int = pla < pol - nest) 

# set.seed(4)
# nest$int[sample(nrow(nest), (size)*1.2)] <- TRUE
# nest$int[sample(nrow(nest), (size)*1.5)] <- FALSE

# full network
size <- 9
toda <- expand.grid(pla = 1:size, 
										pol = 1:size) %>%
	mutate(pla_name = LETTERS[pla],
				 pol_name = letters[pol], 
				 int = TRUE)

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
				plot.title = element_text(hjust = 0, size = 9),
				panel.grid = element_blank())

pm <- modu %>%
	filter(int) %>%
	ggplot(aes(x = pla_name, y = pol_name)) +
	geom_tile(aes(fill = mod > 0), height = 0.8, width = 0.8, fill = "#1f78b4") +
	scale_fill_brewer(palette = "Set1", na.value = "grey70") + 
#	coord_fixed() + 
	xlab("") + 	ylab("") + 
	theme_mine 
	

pn <- nest %>%
	filter(int) %>%
	ggplot(aes(x = pla_name)) +
	geom_tile(aes(y = pol_name, height = 0.8, width = 0.8), fill = "#1f78b4") +
#	coord_fixed() + 
	xlab("") + 	ylab("") + 
	theme_mine
	
pt <- toda %>%
	filter(int) %>%
	ggplot(aes(x = pla_name)) +
	geom_tile(aes(y = pol_name, height = 0.8, width = 0.8), fill = "#1f78b4") +
#	coord_fixed() + 
	xlab("plants") + 	ylab("polinators") + 
	theme_mine

space <- 9
pdf("./figures/networks.pdf", width = 5.75 ,height = 2.1)
multiplot(pt + ggtitle("fully connected"),
					pn + ggtitle("nested") + theme(plot.margin =unit(c(0,space,space,0),"mm")),
					pm + ggtitle("compartmentalised")+ theme(plot.margin =unit(c(0,space*2,space*2,0),"mm")), 
					cols = 3)
dev.off()