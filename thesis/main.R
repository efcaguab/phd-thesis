library(magrittr)
library(drake)
pkgconfig::set_config("drake::strings_in_dots" = "literals")

f <- lapply(list.files("code",
                       full.names = T,
                       pattern = ".R$",
                       recursive = TRUE), source)

# Create bibliography folder if it doesn't exist
dir.create("bibliography", showWarnings = FALSE)
biblio_plan <- drake_plan (
target(command = get_bibliography("https://raw.githubusercontent.com/efcaguab/phd-bibliography/master/phd-literature.bib",
                                  file_out("bibliography/phd-literature.bib"))))

pdf_plan <- drake_plan(
    data_coevolution_res = pre_process_coev_data(
      barber = file_in("data/coevolution/barber_modularity_values.rds"),
      random = file_in("data/coevolution/significancy_modularity_V2.rds"),
      paco = file_in("data/coevolution/4_paco_shuf_net_res.csv"),
      phylo_signal = file_in("data/coevolution/phylogenetic_signal.csv"),
      aic_obs = file_in("data/coevolution/aic_obs.csv"),
      aic_same = file_in("data/coevolution/aic_same_size.csv"),
      comm = file_in("data/coevolution/5_paco_shuf_ass_res.csv")
    ),
    thesis = bookdown::render_book(knitr_in("index.Rmd"),
                                   config_file = file_in("_bookdown.yml")))


full_plan <- rbind(biblio_plan,
                   pdf_plan)

# plan_config <- drake_config(full_plan)
# vis_drake_graph(plan_config)

make(full_plan)
