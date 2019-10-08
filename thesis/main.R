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
target(command = get_bibliography("https://raw.githubusercontent.com/efcaguab/phd-bibliography/master/phd-literature.bib",
                                  file_out("bibliography/phd-literature.bib"))))

pdf_plan <-
  drake_plan(thesis = bookdown::render_book(knitr_in("index.Rmd"),
                                            config_file = file_in("_bookdown.yml")))


full_plan <- rbind(biblio_plan,
                   pdf_plan)

# plan_config <- drake_config(full_plan)
# vis_drake_graph(plan_config)

make(full_plan)
