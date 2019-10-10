pre_process_coev_data <- function(barber, random, paco, phylo_signal, aic_obs, aic_same, comm){

  #modules
  barber <- readRDS(barber)
  random <- readRDS(random) %>%
    dplyr::select(network, permutation, modularity)

  mod <- dplyr::bind_rows(barber, random)
  mod <- plyr::ddply(mod, "network", function(x){
    x %<>%
      dplyr::mutate(rank = dplyr::min_rank(modularity)/dplyr::n())
  })

  mod %<>%
    dplyr::filter(is.na(permutation)) %>%
    dplyr::mutate(mod = 1 - rank,
                  modularity = replace(mod, mod < 1/1000, 1/1000)) %>%
    dplyr::select(network, modularity)

  # comm cophylogeny
  paco <- read.csv(paco, row.names=1)
  paco <- subset(paco, paco$type == 'HP_S')
  paco <- paco[,c(1,4)]
  colnames(paco) <- c('network', 'coevolution')

  # phylogenetic signal pollinators
  phy <- read.csv(phylo_signal, row.names=1)
  phy <- phy %>%
    dplyr::group_by(network, type) %>%
    dplyr::group_by() %>%
    dplyr::select(network, type, pval) %>%
    tidyr::spread(type, pval) %>%
    dplyr::rename(phy_pol = pol) %>%
    dplyr::rename(phy_pla = pla) %>%
    dplyr::select(network, phy_pol, phy_pla)

  # module aic
  aic_obs <- read.csv(aic_obs, sep=',', row.names=1, header=TRUE)
  aic_obs <- subset(aic_obs, aic_obs$type=='HP_S')
  aic_obs$perm <- as.character(aic_obs$perm)

  aic_same <- read.csv(aic_same, sep=',', header=TRUE, row.names=1)
  aic_same <- subset(aic_same, aic_same$type=='HP_S')

  aic <- rbind(aic_same, aic_obs)
  aic <- aic %>%
    dplyr::group_by(network, type) %>%
    dplyr::mutate(aic = dplyr::percent_rank(aic)) %>%
    dplyr::filter(perm == "rds") %>%
    dplyr::select(-X1, -perm) %>%
    dplyr::group_by() %>%
    dplyr::mutate(type = paste("aic", type, sep = "_")) %>%
    tidyr::spread(type, aic)

  # shuffled assemblage paco
  comm <- read.csv(comm, row.names=1)
  colnames(comm) <- c('network', 'comm_just')
  comm[,1] <- unlist(strsplit(as.character(comm[,1]), '.csv'))


  all_results_org <- dplyr::full_join(comm, paco) %>%
    #dplyr::full_join(mod) %>%
    dplyr::full_join(phy) %>%
    dplyr::full_join(dplyr::select(aic, network, aic_HP_S)) %>%
    tidyr::gather(property, p_value, -network) %>%
    #   mutate(p_value = replace(p_value, p_value < 1/1000, 1/1000),
    #          p_value = replace(p_value, p_value > 999/1000, 999/1000))
    dplyr::filter(!is.na(p_value)) %>%
    dplyr::distinct()

  all_results <- all_results_org %>%
    dplyr::mutate(property = plyr::mapvalues(property,
                                from = c("comm_just",
                                         "coevolution",
                                         #"modularity",
                                         "phy_pol",
                                         "phy_pla",
                                         "aic_HP_S"),

                                to = c("Community CS (shuffled assemblage)",
                                       "Community CS (shuffled network)",
                                       #"Community modularity",
                                       "Phylogenetic signal (pollinator modules)",
                                       "Phylogenetic signal (plant modules)",
                                       "Modular network CS")))


  prop_matrix <- all_results %>%
    dplyr::mutate(signif = p_value <= 0.05,
           signif = plyr::mapvalues(signif,
                                    c(TRUE, FALSE),
                                    c(1, 0))) %>%
    dplyr::select(network, property, signif) %>%
    reshape2::acast(network ~ property, value.var = "signif")

  commi <- prop_matrix[,3]
  prop_matrix <- prop_matrix[,-3]
  prop_matrix <- cbind(prop_matrix, commi)
  colnames(prop_matrix)[5] <- "Modular network CS"

  comco <- prop_matrix[,2]
  prop_matrix <- prop_matrix[,-2]
  prop_matrix <- cbind(comco, prop_matrix)
  colnames(prop_matrix)[1] <- "Community CS (shuffled network)"


  prop_matrix[is.na(prop_matrix)] <- 0

  all_results %>%
    dplyr::mutate(network = factor(network, levels = rownames(prop_matrix)),
           property = factor(property, levels = rev(colnames(prop_matrix))))

}

make_fig_empirical_cophylogeny <- function(data_coevolution_res){

  pal <- c(#thesis_palette_dark[1],
    thesis_palette[1],
    thesis_palette_light[1],
    "grey90")

  data_coevolution_res %>%
    dplyr::mutate(
      network = forcats::fct_reorder(network, p_value, .fun = min),
      network = factor(network, labels = 1:54),
      group = cut(as.numeric(network), 3,
                  labels = c("network 1-18",
                             "network 19-36",
                             "network 37-54")),
      scale = dplyr::case_when(property == "Community CS (shuffled assemblage)" ~ "community",
                               property == "Community CS (shuffled network)" ~ "community",
                               TRUE ~ "module"),
      p_value = cut(p_value, breaks = c(0, 0.05, 1), include.lowest = T)) %>%
    ggplot() +
    geom_tile(aes(x = network, y = property, fill = p_value, width = 0.8, height = 0.8)) +
    scale_fill_manual(values = pal,
                      labels = c("p < 0.05  ", "p > 0.05  "),
                      name = "") +
    scale_x_discrete(position = "top") +
    scale_y_discrete(labels = rev(c("community (assemblage)",
                                    "community (network)",
                                    "pollinator modules",
                                    "plant modules",
                                    "modular network"))) +
    # coord_fixed() +
    facet_wrap(~ group, scales = "free", ncol = 1, strip.position = "top") +
    # scale_x_discrete(labels = c(1:54))+
    xlab('Pollination networks') +
    base_ggplot_theme() +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          strip.text = element_text(debug = F, margin = margin(b = 1, l = 1)),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = margin(),
          panel.border = element_blank()) +
    labs(x = "pollination networks",
         title = "significancy of (co)phylogenetic signal",
         subtitle = "across 54 pollination networks")

  # ggsave("plot1.pdf", width = fig_sizes()$two_column_width,
         # height = fig_sizes()$two_column_width*0.9)

  }
