library(ggplot2)
library(magrittr)
library(grid)
source("../proposal/code/multiplot.R")


# n_driver figure ---------------------------------------------------------

my_theme <- theme_bw() +
	#scale_color_brewer(palette = "Paired") +
	theme(legend.position = "right",
				# text = element_text(family = "Times"),
				plot.margin=unit(c(4,0,0,0),"mm"),
				panel.margin=unit(c(0,0,0,0),"mm"),
				panel.border = element_rect(colour = "grey10", size = 0.5),
				# panel.border = element_blank(),
				axis.text = element_text(size = 7),
				# axis.ticks.x = element_blank(),
				# axis.ticks.y = element_blank(),
				axis.title = element_text(size = 8),
				plot.title = element_text(hjust = 0.05, vjust = -1.5, size = 9),
				panel.grid = element_blank(),
				legend.text = element_text(size = 7),
				legend.key = element_blank(), 
				legend.title = element_text(size = 8))

n_driver <- read.csv("../../driver-species/data/n_driver_species.dat")
ntw_info <- read.csv("../../driver-species/data/ntw_info.csv")

p1 <- 
	n_driver %>%
	dplyr::filter(type == "weight.B") %>%
	dplyr::inner_join(ntw_info) %>%
	ggplot(aes(x = n_driver/n_species)) +
# 	geom_boxplot(aes(x = 0.15, fill = inv), 
# 							 width = 0.3, size = 0.25, colour = "grey10",
# 							 outlier.shape = 21, outlier.size = 1) +
	stat_density(aes(y = ..scaled..), colour = "grey10", geom = "line") +
	# geom_point(aes(y = 0), shape = 21) +
	xlab("") + 
	ylab("scaled density") + 
	# scale_x_continuous(limits = c(0,1.01)) + 
	scale_fill_manual(name = "", 
										values  = c("#ffffff", "#bdbdbd"),
										labels = c("uninvaded", "invaded")) +
	my_theme +
	theme(axis.text.x = element_blank()) +
	expand_limits(x = c(0,1))

p2 <- n_driver %>%
	dplyr::filter(type == "weight.B") %>%
	dplyr::inner_join(ntw_info) %>%
	ggplot(aes(y = n_driver/n_species)) +
	geom_boxplot(aes(x = inv, fill = inv), 
							 size = 0.25, colour = "grey10",
							 outlier.shape = 21, outlier.size = 1) +
	# stat_ydensity(aes(x = ..scaled..), colour = "black", geom = "violin") +
	# geom_point(aes(y = 0), shape = 21) +
	xlab("") + 
	ylab("proportion of driver species") + 
	# scale_x_continuous(limits = c(0,0.3)) + 
	scale_fill_manual(name = "", 
										values  = c("#ffffff", "#bdbdbd"),
										labels = c("uninvaded", "invaded")) +
	my_theme + coord_flip() + 
	theme(legend.position = "none",
				panel.border = element_blank(),
				axis.line = element_line(colour = "grey10", size = 0.3),
				axis.line.y = element_blank(),
				axis.text.y = element_blank(),
				axis.ticks.y = element_blank(),
				plot.margin =  unit(c(-3,0,2,5.5),"mm"),
				legend.margin = unit(c(-3.5,0,0,0),"mm")
				# strip.background = element_rect(colour = "grey10",size = 0.5)
				)  +
	expand_limits(y = c(0,1))

pdf("./figures/n_driver.pdf", width = 3.14 ,height = 2.8)
multiplot(p1, p2, layout = matrix(c(1,1,2)))
dev.off()


# random_degree figure ----------------------------------------------------

random_n <- read.csv("../../driver-species/data/random_n_driver.csv")
n_control <- n_driver %>%
	dplyr::select(-keep) %>%
	tidyr::separate(type, c("type", "keep"), "\\.", remove = T)

random_n_z <- random_n %>% 
	dplyr::group_by(net, type, keep, method) %>%
	dplyr::summarise(n_driver_u = mean(n_driver, na.rm = T),
									 n_driver_sd = sd(n_driver, na.rm = T)) %>%
	dplyr::full_join(n_control) %>% 
	dplyr::group_by() %>%
	dplyr::mutate(n_driver_z_score = (n_driver - n_driver_u)/n_driver_sd) 

p3 <- random_n_z %>% 
	dplyr::filter(type == "weight", 
								keep == "B") %>%
	dplyr::inner_join(ntw_info) %>%
	ggplot(aes(x = n_driver_z_score)) +
	geom_rect(ymin = -2, ymax = 2, xmin = -2, xmax = 2, fill = "#f8f8f8") +
	# geom_hline(yintercept = 0, size = 0.4) +
	geom_vline(xintercept = 0, linetype = 2, size = 0.25) +
	stat_density(aes(colour = method, y = ..scaled..), geom = "line", position = position_identity()) +
	scale_color_manual(name = "randomisations\nthat mantain\ndegree of", 
										 labels= c("plants", "pollinators", "both"),
										 values = c("#fdcc8a", "#fc8d59", "#d7301f")) + 
	my_theme + 
	xlab("") +
	ylab("scaled density") +
	theme(legend.position = "top",
				axis.line.x = element_blank(),
				axis.text.x = element_blank())

p4 <- random_n_z %>% 
	dplyr::filter(type == "weight", 
								keep == "B") %>%
	dplyr::inner_join(ntw_info) %>%
	ggplot(aes(y = n_driver_z_score)) +
	geom_rect(xmin = -2, xmax = 20, ymin = -2, ymax = 2, fill = "#f8f8f8") +
	coord_flip() +
	# geom_hline(yintercept = 0, size = 0.4) +
	geom_hline(yintercept = 0, linetype = 2, size = 0.25) +
	geom_boxplot(aes(x = method, fill = method), size = 0.25,
							 outlier.shape = 21, outlier.size = 1) +
	# stat_density(aes(colour = method, y = ..scaled..), geom = "line", position = position_identity()) +
	scale_fill_manual(name = "randomisations\nthat mantain\ndegree of", 
										 labels= c("plants", "pollinators", "both"),
										values = c("#fdcc8a", "#fc8d59", "#d7301f")) +
	my_theme + 
	ylab("standard score (z)") +
	xlab("") + 
	theme(legend.position = "none",
				panel.border = element_blank(),
				axis.line = element_line(colour = "grey10", size = 0.3),
				axis.line.y = element_blank(),
				axis.text.y = element_blank(),
				axis.ticks.y = element_blank(),
				plot.margin =  unit(c(-3,0,2,6),"mm"),
				legend.margin = unit(c(-3.5,0,0,0),"mm")
				# strip.background = element_rect(colour = "grey10",size = 0.5)
	) +
	expand_limits(x = c(0.6,1.4))


pdf("./figures/random_degree.pdf", width = 3.14 ,height = 3.5)
multiplot(p3, p4, layout = matrix(c(1,1,1,1,2,2)))
dev.off()


# random_direction figure -------------------------------------------------

random_dir_n <- read.csv("../../driver-species/data/random_dir_n_driver.csv") 

d <- random_dir_n %>% 
	dplyr::mutate(type = "weight") %>%
	dplyr::group_by(net, type, keep) %>%
	dplyr::summarise(n_driver_u = mean(n_driver, na.rm = T),
									 n_driver_sd = sd(n_driver, na.rm = T)) %>%
	dplyr::inner_join(n_control) %>% 
	dplyr::group_by() %>%
	dplyr::mutate(n_driver_z_score = (n_driver - n_driver_u)/n_driver_sd) %>%
	dplyr::filter(type == "weight",
								keep == "B")
	
p5 <-	d %>%
	ggplot(aes(x = n_driver_z_score)) +
	geom_rect(ymin = -2, ymax = 2, xmin = -2, xmax = 2, fill = "#f8f8f8") +
	geom_vline(xintercept = 0, linetype = 2, size = 0.25) +
	stat_density(aes(y = ..scaled..), geom = "line", colour = "grey10") +
	my_theme + 
	xlab("") +
	ylab("scaled density") +
	theme(axis.text.x = element_blank()
				)

p6 <- d %>%
		ggplot(aes(x  = as.character(interaction(type, keep)), y = n_driver_z_score)) +
		geom_rect(ymin = -2, ymax = 2, xmin = -1, xmax = 10, fill = "#f8f8f8") +
		geom_hline(yintercept = 0, linetype = 2, size = 0.25) +
		# geom_jitter(position = position_jitter(height = 0), shape = 21) +
		geom_boxplot(fill = "white", size = 0.25,
								 outlier.shape = 21, outlier.size = 1, colour = "grey10") +
		scale_fill_discrete(name = "randomisations\nmantain\ndegree of", labels= c("plants", "pollinators", "both")) + 
		my_theme + 
		xlab("") +
		ylab("standard score (z)")  + 
	theme(legend.position = "none",
				panel.border = element_blank(),
				axis.line = element_line(colour = "grey10", size = 0.3),
				axis.line.y = element_blank(),
				axis.text.y = element_blank(),
				axis.ticks.y = element_blank(),
				plot.margin =  unit(c(-3,0,2,5.5),"mm"),
				legend.margin = unit(c(-3.5,0,0,0),"mm")
				# strip.background = element_rect(colour = "grey10",size = 0.5)
	) +
	coord_flip()

pdf("./figures/random_direction.pdf", width = 3.14 ,height = 2.5)
multiplot(p5, p6, layout = matrix(c(1,1,1,2)))
dev.off()


# by_species --------------------------------------------------------------
folder <- "../../driver-species/data/maximum_matchings_summary/" 
files <- list.files(folder)
node_mm <- plyr::adply(files, 1, function(x){
	read.csv(file.path(folder, x)) %>%
		dplyr::mutate(net = unlist(stringr::str_split(x, "_"))[1],
									type = unlist(stringr::str_split(x, "_"))[2], 
									keep = unlist(stringr::str_split(x, "_"))[3],
									batch = unlist(stringr::str_split(x, "_"))[4])
}) %>%
	dplyr::select(-X1) %>%
	dplyr::group_by(net, type, keep, node) %>%
	dplyr::summarise(freq = sum(freq))

ranked_mm <- node_mm %>%
	dplyr::filter(!is.na(freq)) %>%
	dplyr::group_by(net, type, keep) %>%
	dplyr::mutate(r = rank(freq, ties.method = "random"),
								f_f = freq/max(freq), 
								type.keep = as.character(interaction(type, keep)))  %>%
	dplyr::rename(net_name = net)

invasive_sp <- data.frame(node = c("p_4", "p_25"), invs.species = TRUE)

ranked_with_inv <- 
	ranked_mm %>% 
	dplyr::group_by() %>%
	dplyr::full_join(invasive_sp) %>% 
	dplyr::full_join(ntw_info)

p7 <- ranked_with_inv %>%
	dplyr::filter(type == "weight", 
								keep == "B") %>%
	tidyr::separate(node, into = c("categ", "number"), sep = "_", remove = F) %>%
	ggplot(aes(x = f_f)) + 
	stat_density(aes(y = ..scaled.., colour = categ), adjust = 0.5, geom = "line", position = position_identity()) +
	geom_point(data = ranked_with_inv %>%
						 	dplyr::filter(invs.species, type == "weight", keep == "B"), 
						 aes(y = scale(net)/3 + 0.5), 
						 shape = 21, fill = "#bdbdbd") +
	scale_color_manual(name = "", labels = c("pollinators", "plants"),
										 values = c("#fc8d59", "#fdcc8a")) +
	my_theme +	
	xlab("species frequency in control sets") +
	ylab("scaled density") +
	theme(legend.position = c(0.05,1),
				legend.justification = c(0,0.7),
				legend.background = element_blank(),
				legend.direction = "horizontal",
				plot.margin=unit(c(0,0,0,0),"mm"))
	
pdf("./figures/per_guild.pdf", width = 3.14 ,height = 2)
p7
dev.off()