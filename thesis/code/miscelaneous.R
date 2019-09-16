# download bibliography from a remote file
get_bibliography <- function(filename_in, filename_out){

  biblio_items <- readLines(filename_in)

  url_lines <- stringr::str_detect(biblio_items, "url = ") |
    stringr::str_detect(biblio_items, "urldate = ")

  writeLines(biblio_items[!url_lines],
             filename_out)

  biblio_items[!url_lines] %>%
    paste(collapse = " ") %>%
    openssl::sha1()

}
