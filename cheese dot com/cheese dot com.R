pacman::p_load(Rcrawler,XML,tidyverse)


Rcrawler(Website = "https://www.cheese.com/babybel/",
         no_cores = 2,
         no_conn = 4,
         ExtractCSSPat = c(".summary-points",".description",".cheese-image-border"),
         PatternsNames = c("table","description","image"),
         ExtractAsText = FALSE,
         saveOnDisk = FALSE)


empty_vector <-function(x){
  x==""
}

equals_na<-function(x){
  x=="NA"
}

split_string_to_tibble<-function(x){
  
  split_position<-(str_locate(x,"Made from|:"))[2]
  
  col_1<-str_sub(x, 1, split_position)%>%
    str_remove(":")
  col_2<-str_sub(x, split_position+2, str_length(x))
  
  list(col_2)%>%
    set_names(col_1)%>%
    as_tibble()
}


parse_cheese_table<-function(cheese_table){
  cheese_table%>%
    xml2::read_html()%>%
    rvest::html_text(trim = TRUE)%>%
    str_replace_all("\t|<p>|</p>|<li>|</li>|</a>", "") %>%
    str_split("\n")%>%
    unlist()%>%
    stringi::stri_subset(regex="[:alpha:]")%>%
    map(split_string_to_tibble)%>%
    do.call(cbind,.)%>%
    map_df(trimws)%>%
    set_names(tolower(names(.)))%>%
    as_tibble(.name_repair = "universal")
}





write_rds(list(DATA=DATA, INDEX = INDEX), "cheese dot com/scraped_cheese.rds")


parse_cheese_page<-function(cheese_page){


description<-cheese_page$description%>%
xml2::read_html()%>%
  rvest::html_text(trim = T)

image_data<-cheese_page$image%>%
  xml2::read_html()%>%
  rvest::html_children()%>%
  rvest::html_children()%>%
  rvest::html_children()%>%
  rvest::html_children()%>%
  rvest::html_attrs()

output<-parse_cheese_table(cheese_page$table)%>%
  mutate(img_url = paste0("https://www.cheese.com", image_data[[1]]["src"]),
         cheese = image_data[[1]]["alt"],
         description =  description)
return(output)

}

safe_cheese<-safely(parse_cheese_page)

cheese_out<-map(DATA%>%
                  set_names(INDEX$Url), safe_cheese)


errors<-DATA[map_lgl(cheese_out,~!is.null(.x$error))]%>%
  .[map_lgl(.,~!equals_na(.x$table))]


tidy_cheese<-cheese_out[map_lgl(cheese_out, ~!is.null(.x$result))]%>%
  map2_df(names(.), ~.x$result%>%
            mutate(url = .y))%>%
  unique()%>%
  mutate(date_collected = Sys.Date())

write_rds(tidy_cheese, "cheese dot com/cheese_data.rds")
