# download bibliography from a remote file
get_bibliography <- function(filename_in, filename_out){
  readLines(filename_in) %>%
    writeLines(filename_out)
}
