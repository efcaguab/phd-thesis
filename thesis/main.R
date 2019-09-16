library(magrittr)
library(drake)
pkgconfig::set_config("drake::strings_in_dots" = "literals")

f <- lapply(list.files("code", full.names = T, pattern = ".R$"), source)


biblio_plan <- drake_plan (
pollen_competition = get_bibliography("https://raw.githubusercontent.com/efcaguab/phd-bibliography/master/pollen-competition.bib",
                                      file_out("phd-bibliography/pollen-competition.bib")),
network_control = get_bibliography("https://raw.githubusercontent.com/efcaguab/phd-bibliography/master/network-control.bib",
                                   file_out("phd-bibliography/network-control.bib")),
interactions_sdm = get_bibliography("https://raw.githubusercontent.com/efcaguab/phd-bibliography/master/interactions%2Bsdm.bib",
                                    file_out("phd-bibliography/interactions_sdm.bib")),
ownpubs = get_bibliography("https://raw.githubusercontent.com/efcaguab/phd-bibliography/master/ownpubs.bib",
                           file_out("phd-bibliography/ownpubs.bib"))
)

full_plan <- rbind(biblio_plan)

make(full_plan)
