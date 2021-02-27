pacman::p_load(Rcrawler,XML, dplyr, purrr)





Rcrawler(
  Website = "https://www.oxfordcheese.co.uk/our-cheeses/",
  no_cores = 2,
  no_conn = 6,
  ExtractCSSPat = c("h1", "p", ".detailslist table"),
  PatternsNames = c("Title", "Description", "Table"),
  MaxDepth = 6,
  saveOnDisk = FALSE,
  ExtractAsText = FALSE
)


pacman::p_load(readr)

readr::write_rds(list(data = DATA, index = INDEX), file = "cheese.rds")

parse_html_table <- function(table) {
  out <- tibble(table=FALSE)
  if(table!="NA"){
  df <- XML::readHTMLTable(table)[[1]]
  
  
  if (is.data.frame(df)) {
    out <- t(df) %>%
      as_tibble() %>%
      set_names(.[1, ]) %>%
      .[-1, ]
  }
  }
  return(out)
}

tables<-map(DATA, ~.x$Table%>%
      parse_html_table())

tidy_output<-map2(tables,DATA, ~mutate(.x,
                               title = .y$Title,
                               description = .y$description
                               ))%>%
  map2_df(INDEX$Url, ~mutate(.x, url = .y))

write_rds(tidy_output, "oxford_cheese.rds")
