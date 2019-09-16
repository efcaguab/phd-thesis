library(magrittr)
library(drake)
pkgconfig::set_config("drake::strings_in_dots" = "literals")

f <- lapply(list.files("code",
                       full.names = T,
                       pattern = ".R$",
                       recursive = TRUE), source)

# Create bibliography folder if it doesn't exist
dir.create("biblio", showWarnings = FALSE)
biblio_plan <- drake_plan (
pollen_competition = get_bibliography("https://raw.githubusercontent.com/efcaguab/phd-bibliography/master/pollen-competition.bib",
                                      file_out("biblio/pollen-competition.bib")),
network_control = get_bibliography("https://raw.githubusercontent.com/efcaguab/phd-bibliography/master/network-control.bib",
                                   file_out("biblio/network-control.bib")),
interactions_sdm = get_bibliography("https://raw.githubusercontent.com/efcaguab/phd-bibliography/master/interactions%2Bsdm.bib",
                                    file_out("biblio/interactions_sdm.bib")),
ownpubs = get_bibliography("https://raw.githubusercontent.com/efcaguab/phd-bibliography/master/ownpubs.bib",
                           file_out("biblio/ownpubs.bib"))
)

pdf_plan <-
  drake_plan(chapters = c(knitr_in("introduction.Rmd"),
                          knitr_in("driver-species.Rmd"),
                          knitr_in("conclusion.Rmd")),
             biblio = c(pollen_competition, network_control, interactions_sdm, ownpubs),
             thesis = bookdown::render_book(knitr_in("index.Rmd"),
                                            config_file = file_in("_bookdown.yml")))


full_plan <- rbind(biblio_plan,
                   pdf_plan)


make(full_plan)
