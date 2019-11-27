################
# Web Scraping Conference Proceedings of the GDCP.de website ####
# by: David Buschhüter and Peter Wulff
# 14.11.2019
#################

# loadinging the rvest package and dplyr 
library('rvest')
library('dplyr')

# paths
machine_path <- getwd()
text_path <- paste(machine_path, "/text_data_raw/", sep ="")

#****************************
# Website specifications ####
#****************************

# specifying the url
url_2018 <- 'http://www.gdcp.de/index.php/tagungsbaende/tagungsband-uebersicht/169-2019'
url_2017 <- 'http://www.gdcp.de/index.php/tagungsbaende/tagungsband-uebersicht/167-2018'
url_2016 <- 'http://www.gdcp.de/index.php/tagungsbaende/tagungsband-uebersicht/165-2017'
url_2015 <- 'http://www.gdcp.de/index.php/tagungsbaende/tagungsband-uebersicht/164-2016'
url_2014 <- 'http://www.gdcp.de/index.php/tagungsbaende/tagungsband-uebersicht/161-2015'
url_2013 <- 'http://www.gdcp.de/index.php/tagungsbaende/tagungsband-uebersicht/157-2014'
url_2012 <- 'http://www.gdcp.de/index.php/tagungsbaende/tagungsband-uebersicht/145-2013'

url_list <- list(url_2012, url_2013, url_2014, url_2015, url_2016, url_2017, 
                 url_2018)
conf_years <- list(2012, 2013, 2014, 2015, 2016, 2017, 2018)

#***************
# functions ####
#***************
#Extraction functions ####
# function extracting the abstract author, title text from one document
extr_text <- function(url_x, conf_year){
  
  # reading webpage
  webpage_x <- read_html(url_x)
  
  # using CSS selectors to get text, title, authors
  abs_text <- html_nodes(webpage_x,'#content p:nth-child(3)')
  abs_title <- html_nodes(webpage_x,'h2')
  abs_author <- html_nodes(webpage_x,'p:nth-child(1)')
  
  # converting data to text
  abs_text <- html_text(abs_text)
  abs_title <- html_text(abs_title)
  abs_title <- gsub( "\n", "",x=abs_title)
  abs_title <- gsub( "\t", "",x=abs_title)
  abs_author <- html_text(abs_author)
  
  data.frame(abs_author , abs_title , abs_text, conf_year, 
             stringsAsFactors = FALSE)
}

# Extracts all these information for all abstracts of
# one year (using the function "extr_text")
extr_all <- function(url_year = url_list[[1]], conf_year = conf_years[[1]]){
  
  # reading the HTML code from the website
  webpage <- read_html(url_year)
  
  # using CSS selectors to scrape the abstract links section
  # e.g. http://tiny.cc/ijl9fz
  abstr_links <- html_nodes(webpage,'.list-title a')
  abstr_links <-paste("http://www.gdcp.de",
                      html_attr(abstr_links, "href"), sep ="")
  
  # extracting all abstracts from year x
  text_list <- lapply(abstr_links[c(1:length(abstr_links))], FUN = extr_text, 
                      conf_year = conf_year)
  text_df <- bind_rows(text_list, .id = "column_label")
  text_df 
}

#******************************
# execution  over all years####
#******************************
# This may take up to 20 minutes
all_abs <- data.frame()
for(i in 1:length(url_list)){
  all_abs <- rbind(all_abs, extr_all(url_year = url_list[[i]], 
                                     conf_year = conf_years[[i]]))
}

# creating folder and writing to output file
dir.create(text_path, showWarnings = FALSE)
write.csv(x = all_abs, paste0(text_path,"all_abs_utf-8.csv"), 
          fileEncoding = "UTF-8")

