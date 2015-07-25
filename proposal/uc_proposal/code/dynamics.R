library(ggplot2)
library(magrittr)
library(grid)
source("../code/multiplot.R")

t <- seq(0,1000, 1)
n_species <- 5

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
lower <- runif(n_species, 0.2, 0.8)

# invasor causes a shift

set.seed(5)

shift_param <- dplyr::data_frame(species = 1:n_species,
													 lower = lower,
													 upper = rep(c(0,runif(1, 0.8, 1)), c(n_species-1, 1)), 
													 inflection = runif(n_species, 400,500),
													 slope = runif(n_species, 0.065,0.095)) %>%
	dplyr::rowwise() %>%
	dplyr::mutate(lower = max(0, lower),
								upper = max(0, upper))

shi_inv <- data.frame(species = n_species + 1,
												lower = 0.025, 
												upper = runif(1, 0.8, 1),
												inflection = 400,
												slope = 0.075) %>% 
	dynamics(t = 250:1000) %>%
	dplyr::mutate(type = "z")

shi_nat <- dynamics(shift_param, t) %>%
	dplyr::mutate(type = "native")

shi <- rbind(shi_nat, shi_inv)

# invasor doesn't cause a shift

set.seed(1)

no_shi_param <- dplyr::data_frame(species = 1:n_species,
																		lower = lower,
																		upper = lower + runif(n_species, -0.25, 0.25), 
																		inflection = runif(n_species, 400,500),
																		slope = runif(n_species, 0.01,0.05)) %>%
	dplyr::rowwise() %>%
	dplyr::mutate(lower = max(0, lower),
								upper = max(0, upper))

no_shi_inv <- data.frame(species = n_species + 1,
												 lower = 0.025, 
												 upper = runif(1, 0.2, 0.5),
												 inflection = runif(1, 400,500),
												 slope = runif(1, 0.065,0.095)) %>% 
	dynamics(t = 250:1000) %>%
	dplyr::mutate(type = "z")

no_shi_nat <- dynamics(no_shi_param, t, mod = 8) %>%
	dplyr::mutate(type = "native")

no_shi <- rbind(no_shi_nat, no_shi_inv)

# invasor doesn't suceed


set.seed(4)
no_suc_param <- dplyr::data_frame(species = 1:n_species,
																		lower = lower,
																		upper = lower + runif(n_species, -0.05, 0.05), 
																		inflection = runif(n_species, 400,500),
																		slope = runif(n_species, 0.01,0.05)) %>%
	dplyr::rowwise() %>%
	dplyr::mutate(lower = max(0, lower),
								upper = max(0, upper))

no_suc_inv <- data.frame(species = n_species + 1,
												 lower = 0, 
												 upper = 0,
												 inflection = runif(1, 400,500),
												 slope = 0.05) %>% {
												 	dplyr::data_frame (species = n_species + 1,
												 										 t = 250:1000,
												 										 p = sigmoidal(0.025,0,runif(1, 400,500),0.025, t),
												 										 n = 15 * 
												 										 	dnorm(t, 
												 										 				mean = runif(1, 400,500) + 0.05 * runif(1, 400,500), 
												 										 				sd = 50))
												 } %>%
	dplyr::mutate(type = "z")

no_suc_nat <- dynamics(no_suc_param, t, mod = 20, sda = 400) %>%
	dplyr::mutate(type = "native")

no_suc <- rbind(no_suc_nat, no_suc_inv)


ijole <- . %>% {
	ggplot(data = ., aes(x = t, y = p + n)) +
		geom_line(aes(colour = type, group = as.factor(species)), size = 0.7) +
		geom_segment(aes(x = 250, xend = 250, y = 0.15, yend = 0.06), 
								 arrow = arrow(length = unit(0.1, "cm"), type = "closed", angle = 20),
								 size = 0.3, colour = "grey50")+
		ylim(0,1) +
		xlab("")+
		ylab("")+
		theme_bw() +
		scale_color_brewer(palette = "Paired") +
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
}

pdf("./figures/dynamics.pdf", width = 5.75 ,height = 1.92)
multiplot(ijole(no_suc) + ggtitle("(a)") + ylab("abbundance"),
					ijole(no_shi) + ggtitle("(b)") + xlab("time"),
					ijole(shi) + ggtitle("(c)"), 
					cols = 3)
dev.off()
