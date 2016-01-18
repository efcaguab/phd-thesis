library(ggplot2)
library(magrittr)


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

p <- 
	n_driver %>%
	dplyr::filter(type == "weight.B") %>%
	dplyr::inner_join(ntw_info) %>%
	ggplot(aes(y = n_driver/n_species)) +
	geom_boxplot(aes(x = 0.15, fill = inv), 
							 width = 0.3, size = 0.25, colour = "grey10",
							 outlier.shape = 21, outlier.size = 1) +
	stat_ydensity(aes(x = ..scaled..), colour = "black", geom = "violin") +
	# geom_point(aes(y = 0), shape = 21) +
	xlab("scaled density") + ylab("proportion of driver species") + 
	scale_x_continuous(limits = c(0,1.01)) + 
	scale_fill_manual(name = "", 
										values  = c("#ffffff", "#bdbdbd"),
										labels = c("uninvaded", "invaded")) +
	my_theme + coord_flip()

pdf("./figures/n_driver.pdf", width = 4 ,height = 2.0329)
p
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

p <- random_n_z %>% 
	dplyr::filter(type == "weight", 
								keep == "B") %>%
	dplyr::inner_join(ntw_info) %>%
	ggplot(aes(x = n_driver_z_score)) +
	geom_rect(ymin = -2, ymax = 2, xmin = -2, xmax = 2, fill = "#f0f0f0") +
	# geom_hline(yintercept = 0, size = 0.4) +
	geom_vline(xintercept = 0, linetype = 2, size = 0.25) +
	stat_density(aes(colour = method, y = ..scaled..), geom = "line", position = position_identity()) +
	scale_color_manual(name = "randomisations\nthat mantain\ndegree of", 
										 labels= c("plants", "pollinators", "both"),
										 values = c("#636363", "#bdbdbd", "black")) + 
	my_theme + 
	xlab("standard score (z)") +
	ylab("scaled density") +
	theme(legend.position = "right",
				axis.line.x = element_blank(), axis.ticks.x = element_blank())

pdf("./figures/random_degree.pdf", width = 4 ,height = 2.0329)
p
dev.off()