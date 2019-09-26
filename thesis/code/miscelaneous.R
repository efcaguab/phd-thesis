# download bibliography from a remote file
get_bibliography <- function(filename_in, filename_out){

  biblio_items <- readLines(filename_in)

  lines_to_remove <- stringr::str_detect(biblio_items, "url = ") |
    stringr::str_detect(biblio_items, "urldate = ") |
    stringr::str_detect(biblio_items, "abstract = ")

  writeLines(biblio_items[!lines_to_remove],
             filename_out)

  biblio_items[!url_lines] %>%
    paste(collapse = " ") %>%
    openssl::sha1()

}
