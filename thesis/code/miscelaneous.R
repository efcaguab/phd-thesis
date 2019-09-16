# download bibliography from a remote file
get_bibliography <- function(filename_in, filename_out){

  biblio_items <- readLines(filename_in)
  writeLines(biblio_items, filename_out)

  biblio_items %>%
    paste(collapse = " ") %>%
    openssl::sha1()
}
