library(magrittr)
library(drake)
library(showtext)

pkgconfig::set_config("drake::strings_in_dots" = "literals")

# setup fonts
sysfonts::font_paths("fonts/iwona/")
sysfonts::font_add("iwona", "fonts/iwona/Iwona-Regular.ttf")
sysfonts::font_add("iwonaitalic", "fonts/iwona/Iwona-Italic.ttf")
sysfonts::font_add("iwonalight", "fonts/iwona/IwonaLight-Regular.ttf")
sysfonts::font_add("iwonamedium", "fonts/iwona/IwonaMedium-Regular.ttf")
sysfonts::font_add("iwonaheavy", "fonts/iwona/IwonaHeavy-Regular.ttf")

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
                                   config_file = file_in("_bookdown.yml")),
    cover_letter = knitr::knit2pdf(knitr_in("cover-letter.Rnw"), output = file_out("cover-letter.tex")))


defense_figures_plan <- drake_plan(
  fig_random_effects_data = readRDS(file_in("data/sdm_networks/fig_random_effects_data.rds")),
  two_sp_random = target(plot_ranf_species_presentation(fig_random_effects_data,
                                                        file_out("defense/figures/two_sp_random1.pdf"),
                                                        file_out("defense/figures/two_sp_random2.pdf"),
                                                        file_out("defense/figures/two_sp_random3.pdf"))),
  correlation_plot_presentation = plot_ranf_correlation_presentation(
    fig_random_effects_data,
    file_out("defense/figures/all_sp_random_scatter.pdf"),
    file_out("defense/figures/all_sp_random_correlation.pdf")),
  coefficient_averages = readRDS(file_in("data/trade_off/coefficient_averages.rds")),
  variable_importance = readRDS(file_in("data/trade_off/variable_importance.rds")),
  trade_off_summary_plot = target(
    plot_coefficient_averages_presentation(
      coefficient_averages,
      variable_importance,
      file_out("defense/figures/trade_off_summary.pdf")))
)


full_plan <- rbind(biblio_plan,
                   pdf_plan,
                   defense_figures_plan)

# plan_config <- drake_config(full_plan)
# vis_drake_graph(plan_config)

make(full_plan)
