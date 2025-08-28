#*****************************************
#*** BOOK CLUB DASHBOARD 
#*** Author: Vigile Fabella
#*** Date: Aug 2025
#*****************************************





#*****************************************
#----- preliminaries -----
#*****************************************
# deactivate scientific notation
options(scipen=999)


# set wd
setwd("C:/Users/user/OneDrive/Personal/Kada/bookclubdash")


# setup libraries
library(lubridate)
library(dplyr)
library(magrittr)
library(data.table)
library(googlesheets4)
library(tidyr)
library(stopwords)
library(ggwordcloud)
library(tidytext)
library(stringr)
library(rvest)
library(polite)




# clear workspace
rm(list = ls())










#*****************************************
#----- import masterlists -----
#*****************************************

# 2021 Masterlist
masterlist2021 <- read_sheet("https://docs.google.com/spreadsheets/d/1hBcY0ZTgPz7ZquAcFyp8WqlYipBuIsJ7lOGotMCf4_s/edit?usp=sharing",
                             sheet = "2021 Masterlist",
                             range = "C5:T28",
                             col_types = "ccccnnnnnnnnnnnccc"
                             ) %>% 
  filter(!is.na(Author)) %>%
  select(c("Title", "Author", "Date", "G", "V", "B", "F", "Recommended\n by", "Genre")) %>%
  rename("Recommender" = "Recommended\n by")


# 2022 Masterlist
masterlist2022 <- read_sheet("https://docs.google.com/spreadsheets/d/1hBcY0ZTgPz7ZquAcFyp8WqlYipBuIsJ7lOGotMCf4_s/edit?usp=sharing",
                             sheet = "2022 Masterlist",
                             range = "C5:T28",
                             col_types = "ccccnnnnnnnnnnnccc") %>% 
  filter(!is.na(Author)) %>%
  select(c("Title", "Author", "Date", "G", "V", "B", "F", "Recommended\n by", "Genre"))  %>%
  rename("Recommender" = "Recommended\n by")


# 2023 Masterlist
masterlist2023 <- read_sheet("https://docs.google.com/spreadsheets/d/1hBcY0ZTgPz7ZquAcFyp8WqlYipBuIsJ7lOGotMCf4_s/edit?usp=sharing",
                             sheet = "2023 Masterlist",
                             range = "C5:T28",
                             col_types = "ccccnnnnnnnnnnnccc") %>% 
  filter(!is.na(Author)) %>%
  select(c("Title", "Author", "Date", "G", "V", "B", "F", "Recommended\n by", "Genre")) %>%
  rename("Recommender" = "Recommended\n by") 




# VGR Masterlist
masterlist_vgr <- read_sheet("https://docs.google.com/spreadsheets/d/1hBcY0ZTgPz7ZquAcFyp8WqlYipBuIsJ7lOGotMCf4_s/edit?usp=sharing",
                             sheet = "VGR Bookclub",
                             range = "C5:V28",
                             col_types = "ccccnnnnnnnnnnnnnccc") %>% 
  filter(!is.na(Author)) %>%
  select(c("Title", "Author", "Date", "G", "V", "R", "Recommended\n by", "Genre")) %>%
  rename("Recommender" = "Recommended\n by")













#*****************************************
#----- year, start, end dates -----
#*****************************************


# 2021
masterlist2021 <- masterlist2021 %>%
  mutate(Date = if_else(Date == "mixed dates", true = "2021", false = Date)) %>%
  mutate(Date = if_else(Date == "2018-2019", true = "Oct 2020", false = Date)) %>%
  mutate(Year = str_extract(Date, "20[0-9]+"))

  # separate 2021 for easier date processing
  m2021a <- filter(masterlist2021, Date == "2021") %>%
    mutate(start_date = NA, 
           end_date = NA)
  
  m2021b <- filter(masterlist2021, Date != "2021") %>%
    group_by(Date) %>%
    mutate(row = row_number()-1, # zero if only/first book in that month, 1 if second book, 2 if third ...
           n = n()) %>% # total books that month
    mutate(start_date = dmy(paste("1", Date)) + row*(floor(30/n))) %>% 
    ungroup %>% arrange(start_date) %>% 
    mutate(end_date = if_else(month(start_date) != 12, true = lead(start_date), false = ymd(paste(Year, 12, 31)))) %>%
    select(!c(row, n))
  
  # recombine two datasets
  masterlist2021 <- bind_rows(m2021b, m2021a)
  




# 2022
masterlist2022 <- masterlist2022 %>%
  mutate(Year = str_extract(Date, "20[0-9]+"))

  # separate 2022 for easier date processing
  m2022a <- filter(masterlist2022, grepl(x = Date, pattern = "-", fixed = TRUE)) %>%
    separate(col = Date, into = c("d1", "d2"), sep = "\\-", remove = FALSE)  %>%
    mutate(start_date = mdy(paste(d1, "1", Year)), 
           end_date = dmy(paste("1", d2))+29) %>%
    select(!c("d1", "d2"))

  
  m2022b <- filter(masterlist2022, !grepl(x = Date, pattern = "-", fixed = TRUE)) %>%
    group_by(Date) %>%
    mutate(row = row_number()-1, # zero if only/first book in that month, 1 if second book, 2 if third ...
           n = n()) %>% # total books that month
    mutate(start_date = dmy(paste("1", Date)) + row*(floor(30/n))) %>% 
    ungroup %>% arrange(start_date) %>% 
    mutate(end_date = if_else(month(start_date) != 12, true = lead(start_date), false = ymd(paste(Year, 12, 31)))) %>%
    select(!c(row, n))
  
  
  # recombine two datasets
  masterlist2022 <- bind_rows(m2022b, m2022a)
  
  
  
  
  
  # 2023
  masterlist2023 <- masterlist2023 %>%
    mutate(Year = str_extract(Date, "20[0-9]+")) %>%
    group_by(Date) %>%
    mutate(row = row_number()-1, # zero if only/first book in that month, 1 if second book, 2 if third ...
           n = n()) %>% # total books that month
    mutate(start_date = dmy(paste("1", Date)) + row*(floor(30/n))) %>% 
    ungroup %>% arrange(start_date) %>% 
    mutate(end_date = if_else(month(start_date) != 5, true = lead(start_date), false = ymd(paste(Year, 5, 31)))) %>%
    select(!c(row, n))
  
  
  
  
  
  # vgr
  masterlist_vgr <- masterlist_vgr %>%
    mutate(Year = str_extract(Date, "20[0-9]+")) %>%
    group_by(Date) %>%
    mutate(row = row_number()-1, # zero if only/first book in that month, 1 if second book, 2 if third ...
           n = n()) %>% # total books that month
    mutate(start_date = dmy(paste("1", Date)) + row*(floor(30/n))) %>% 
    ungroup %>% arrange(start_date) %>% 
    mutate(end_date = if_else(month(start_date) != 5, true = lead(start_date), false = ymd(paste(Year, 5, 31)))) %>%
    select(!c(row, n))
  
  












#*****************************************
#----- masterlist combined -----
#*****************************************

masterlist <- bind_rows(masterlist2021, masterlist2022, masterlist2023, masterlist_vgr) %>%
    mutate(Year = if_else(Year == "2020", true = "2021", false = Year))  %>%
    mutate(Recommender = gsub(x = Recommender, pattern = "Genina", replacement = "Gen")) 













  










#*****************************************
#----- vigile's reads -----
#*****************************************

v <- read_sheet("https://docs.google.com/spreadsheets/d/1hBcY0ZTgPz7ZquAcFyp8WqlYipBuIsJ7lOGotMCf4_s/edit?usp=sharing",
                sheet = "Vig Notes",
                col_names = FALSE, 
                col_types = "c")

# save column names
colnames <- v[2,2:ncol(v)] %>% 
  gsub(pattern = " ", replacement = "_") %>%
  gsub(pattern = "\\?", replacement = "")

# add year and clean rows
v <- v %>% 
  mutate(Year = str_extract(...1, "20[0-9]+")) %>%
  fill(Year, .direction = "down") %>%
  filter(!is.na(...1) & !is.na(...6)) %>%
  select(!c(...1))

# column names
colnames(v) <- c("Title", "Author", "Notes", "Reread", "Rating", "Form", "Date_finished", "Genre", "Year")


# create start and end dates (separate for easier date processing)
va <- filter(v, grepl(x = Date_finished, pattern = "/", fixed = TRUE)) %>%
  separate(col = Date_finished, into = c("d1", "d2"), sep = "/", remove = FALSE)  %>%
  mutate(start_date = mdy(paste(d1, "1", Year)), 
         end_date = dmy(paste("1", d2, Year))+29) %>%
  select(!c("d1", "d2"))

vb <- filter(v, !grepl(x = Date_finished, pattern = "/", fixed = TRUE)) %>%
  group_by(Year, Date_finished) %>%
  mutate(row = row_number()-1, # zero if only/first book in that month, 1 if second book, 2 if third ...
         n = n()) %>% # total books that month
  mutate(start_date = if_else(is.na(Date_finished), true = NA, false = dmy(paste("1", Date_finished, Year)) + row*(floor(30/n)))) %>% 
  ungroup %>% arrange(start_date) %>% 
  mutate(end_date = if_else(month(start_date) != 12, true = lead(start_date), false = ymd(paste(Year, 12, 31)))) %>%
  mutate(end_date = if_else(n-1 == row & !is.na(Date_finished), true = ymd(paste(Year, Date_finished, 1)) + 30, false = end_date)) %>%
  select(!c(row, n))


# get vigile's book club reads
v_bc <- masterlist %>% select(c(Title, Author, V, Date, Genre, Year, start_date, end_date))  %>%
  filter(!is.na(V)) %>%
  mutate(Rating = as.character(V),
         Date_finished = Date) %>% select(!c(V, Date))


# combine both datasets
v <- bind_rows(va, vb, v_bc) %>% arrange(start_date) %>%
  mutate(Title = str_to_title(Title)) %>%
  group_by(Title) %>%
  mutate(n = n()) %>%
  filter(!(n>1 & is.na(Reread))) %>%
  select(!n) %>%
  mutate(Reread = if_else(is.na(Reread), true = "N", false = Reread),
         Form = if_else(is.na(Form), true = "Audio", false = Form),
         Title = str_to_title(Title), 
         Author = gsub(x = Author, pattern = "\\.", replacement = ""),
         Reader = "Vigile")


# remove auxillary datasets
rm(va, vb, v_bc)








#*****************************************
#----- gen's reads -----
#*****************************************

g <- read_sheet("https://docs.google.com/spreadsheets/d/1hBcY0ZTgPz7ZquAcFyp8WqlYipBuIsJ7lOGotMCf4_s/edit?usp=sharing",
                sheet = "Gen Notes",
                col_names = FALSE, 
                col_types = "c")

# save column names
colnames <- g[2,2:ncol(g)] %>% 
  gsub(pattern = " ", replacement = "_") %>%
  gsub(pattern = "\\?", replacement = "") %>%#
  replace_na(replace = "misc")


# add year and clean rows
g <- g %>% 
  mutate(Year = str_extract(...1, "20[0-9]+")) %>%
  fill(Year, .direction = "down") %>%
  filter(!is.na(...1) & !is.na(...6)) %>%
  select(!c(...1))


# column names
colnames(g) <- c(colnames, "Year")


# create end dates (gen has very specific end-dates)
g <- g %>%
  mutate(end_date = mdy(paste(Date_finished, Year))) %>%
  arrange(Year, end_date) %>%
  mutate(start_date = lag(end_date))



# get gen's book club reads
g_bc <- masterlist %>% select(c(Title, Author, G, Date, Genre, Year, start_date, end_date))  %>%
  filter(!is.na(G)) %>%
  mutate(Rating = as.character(G),
         Date_finished = Date) %>% select(!c(G, Date))


# combine both datasets
g <- bind_rows(g, g_bc) %>% arrange(start_date) %>%
  mutate(Title = str_to_title(Title)) %>%
  group_by(Title) %>%
  mutate(n = n()) %>%
  filter(!(n>1 & is.na(Reread))) %>%
  select(!c(n)) %>%
  ungroup %>%
  mutate(Form = gsub(x = Form, pattern = "-", replacement = "") %>% str_to_title(),
         Title = str_to_title(Title), 
         Author = gsub(x = Author, pattern = "\\.", replacement = ""),
         Reader = "Gen") 
g <- g %>%
  mutate(Form = case_when(Form == "A" ~ "Audio",
                          Form == "E" ~ "Ebook",
                          Form == "P" | Form == "G" ~ "Physical",
                          .default = Form))

# remove auxillary datasets
rm(g_bc)













#*****************************************
#----- rach's reads -----
#*****************************************

r <- read_sheet("https://docs.google.com/spreadsheets/d/1hBcY0ZTgPz7ZquAcFyp8WqlYipBuIsJ7lOGotMCf4_s/edit?usp=sharing",
                sheet = "Rach Notes",
                col_names = FALSE, 
                col_types = "c")

# save column names
colnames <- r[2,2:ncol(r)] %>% 
  gsub(pattern = " ", replacement = "_") %>%
  gsub(pattern = "\\?", replacement = "")
  

# add year and clean rows
r <- r %>% 
  mutate(Year = str_extract(...8, "20[0-9]+")) %>% # extract year from "Date finished"
  fill(Year, .direction = "down") %>%
  filter(!is.na(...1) & !is.na(...8)) %>%
  select(!c(...1)) 



# column names
colnames(r) <- c(colnames, "Year")




# create start and end dates (separate for easier date processing)
ra <- filter(r, grepl(x = Date_finished, pattern = "/", fixed = TRUE)) %>%
  separate(col = Date_finished, into = c("d1", "d2"), sep = "/", remove = FALSE)  %>%
  mutate(start_date = mdy(paste(d1, "1", Year)), 
         end_date = dmy(paste("1", d2))+29) %>%
  select(!c("d1", "d2"))

rb <- filter(r, !grepl(x = Date_finished, pattern = "/", fixed = TRUE)) %>%
  group_by(Year, Date_finished) %>%
  mutate(row = row_number()-1, # zero if only/first book in that month, 1 if second book, 2 if third ...
         n = n()) %>% # total books that month
  mutate(end_date = if_else(is.na(Date_finished), true = NA, false = dmy(paste("1", Date_finished)) + 29 - row*(floor(30/n)))) %>% 
  ungroup %>% arrange(end_date) %>% 
  mutate(start_date = lag(end_date)) %>%
  select(!c(row, n))


# get rach's book club reads
r_bc <- masterlist %>% select(c(Title, Author, R, Date, Genre, Year, start_date, end_date)) %>%
  filter(!is.na(R)) %>%
  mutate(Rating = as.character(R),
         Date_finished = Date) %>% select(!c(R, Date))




# combine both datasets
r <- bind_rows(ra, rb, r_bc) %>% arrange(end_date) %>%
  mutate(Title = str_to_title(Title), 
         Author = gsub(x = Author, pattern = "\\.", replacement = ""),
         Reader = "Rachel") %>%
  group_by(Title) %>%
  mutate(n = n()) %>%
  filter(!(n>1 & is.na(Reread))) %>%
  select(!c(n)) %>%
  ungroup %>%
  mutate(Form = gsub(x = Form, pattern = "-", replacement = "") %>% str_to_title()) 


# remove auxillary datasets
rm(ra, rb)
















#*****************************************
#----- biancas's reads -----
#*****************************************


b <- read_sheet("https://docs.google.com/spreadsheets/d/1hBcY0ZTgPz7ZquAcFyp8WqlYipBuIsJ7lOGotMCf4_s/edit?usp=sharing",
                sheet = "Bianca Notes",
                col_names = FALSE, 
                col_types = "c")

# save column names
colnames <- b[2,2:ncol(b)] %>% 
  gsub(pattern = " ", replacement = "_") %>%
  gsub(pattern = "\\?", replacement = "")


# add year and clean rows
b <- b %>% 
  mutate(Year = str_extract(...1, "20[0-9]+")) %>%
  fill(Year, .direction = "down") %>%
  filter(!is.na(...1) & !is.na(...6)) %>%
  select(!c(...1))




# column names
colnames(b) <- c(colnames, "Year")


# create end dates (gen has very specific end-dates)
b <- b %>%
  group_by(Year, Date_finished) %>%
  mutate(row = row_number()-1, # zero if only/first book in that month, 1 if second book, 2 if third ...
         n = n()) %>% # total books that month
  mutate(end_date = if_else(is.na(Date_finished), true = NA, false = dmy(paste("1", Date_finished, Year)) + 29 - row*(floor(30/n)))) %>% 
  ungroup %>% arrange(end_date) %>% 
  mutate(start_date = if_else(is.na(Date_finished), true = NA, false = lag(end_date))) %>%
  mutate(start_date = if_else(is.na(start_date) & !is.na(end_date), true = dmy(paste("1", Date_finished, Year)), false = start_date)) %>%
  select(!c(row, n))




# get bianca's book club reads
b_bc <- masterlist %>% select(c(Title, Author, B, Date, Genre, Year, start_date, end_date))  %>%
  filter(!is.na(B)) %>%
  mutate(Rating = as.character(B),
         Date_finished = Date) %>% select(!c(B, Date))


# combine both datasets
b <- bind_rows(b, b_bc) %>% arrange(start_date) %>%
  mutate(Title = str_to_title(Title)) %>%
  group_by(Title, Year) %>%
  mutate(n = n())  %>%
  filter(!(n==2 & is.na(Reread))) %>%
  select(!n) %>%
  mutate(Form = gsub(x = Form, pattern = "-", replacement = "") %>% str_to_title(), 
         Author = gsub(x = Author, pattern = "\\.", replacement = ""),
         Reader = "Bianca") %>%
  mutate(Author = gsub(x = Author, pattern = "LeGuin", replacement = "Le Guin"))


# remove auxillary datasets
rm(b_bc)


















#*****************************************
#----- france's reads -----
#*****************************************

f <- masterlist %>% select(c(Title, Author, F, Date, Genre, Year, start_date, end_date))  %>%
  filter(!is.na(F)) %>%
  mutate(Rating = as.character(F),
         Date_finished = Date) %>% 
  select(!c(F, Date)) %>% 
  filter(!is.na(Rating)) %>%
  arrange(start_date) %>%
  mutate(Title = str_to_title(Title), 
         Author = gsub(x = Author, pattern = "\\.", replacement = ""),
         Notes = "", 
         Reread = "",
         Form = "",
         Reader = "France")


















#*****************************************
#----- book metadata -----
#*****************************************

books <- bind_rows(v,g,b,f,r) %>%
  mutate(Genre = gsub(x = Genre, pattern = "-", replacement = "")) %>% 
  select(Title, Author, Genre) %>% 
  
  # fix genre
  ungroup %>% group_by(Title, Author) %>%
  unnest_tokens(output = Genre, input = Genre) %>% 
  distinct %>%
  mutate(n = row_number()) %>%
  pivot_wider(names_from = n, values_from = Genre, names_prefix ="v") %>%
  mutate(Genre = paste(v1, v2, v3, v4) %>% gsub(pattern = "NA", replacement = "")) %>%
  select(!starts_with("v")) %>%
  mutate(Genre = gsub(x = Genre, pattern = "dark academia", replacement = "dark_academia")) %>%
  mutate(Genre = gsub(x = Genre, pattern = "graphic novel", replacement = "dark_academia")) %>%
  mutate(Genre = gsub(x = Genre, pattern = "short stor", replacement = "short_stor"))

















#*****************************************
#----- book covers -----
#*****************************************

# # create scrape function
scrape_bookcover <- function(title, author) {
  # fix title to match goodreads search format
  t <- title %>%
    gsub(pattern = "'", replacement = "") %>%
    gsub(pattern = "[[:punct:] ]+", replacement = " ") %>%
    gsub(pattern = " ", replacement = "+")

  # get last name of author
  a <- author %>%
    gsub(pattern = "[[:punct:] ]+", replacement = " ") %>%
    word(-1)

  # create url
  url <- paste0("https://www.goodreads.com/search?q=", t)

  # scrape img url and image alt
  dat <- tibble(src = read_html(url) %>% html_elements(".bookCover") %>% html_attr("src"),
                alt = read_html(url) %>% html_elements(".bookTitle") %>% as.vector %>% tolower,
                authorName = read_html(url) %>% html_nodes('[itemprop=author]') %>%
                  html_text() %>% as.vector %>% tolower %>%
                  gsub(pattern = "\\\n", replacement = "") %>%
                  gsub(pattern = "[[:punct:]]+", replacement = ""))

  # keep only urls where the title does not include the author (to filter out summary books etc.)
  filter(dat, !grepl(x = alt, pattern = tolower(a)) & grepl(x = authorName, pattern = tolower(a)))[1,1] %>% return

}

# run function on books dataset
img_url <- mapply(FUN = scrape_bookcover, title = books$Title, author = books$Author)

img_url1 <- img_url %>%
  stack %>%
  rename("Title" = "ind", "url" = "values") %>%
  mutate(url = gsub(x= url,  pattern = "._SX50_SY75_", replacement = "", fixed = TRUE)) %>%
  mutate(url = gsub(x= url,  pattern = "._SY75_", replacement = "", fixed = TRUE)) %>%
  mutate(url = gsub(x= url,  pattern = "._SX50_", replacement = "", fixed = TRUE)) %>%
  mutate(Title = gsub(x = Title, pattern = ".src", replacement = "", fixed = TRUE)) %>%
  distinct %>% na.omit



# save
save(img_url, img_url1, file = "RData/img_url.RData")
#load(file = "RData/img_url.RData")

# add to books
books <- left_join(x = distinct(books), y = img_url1, by = "Title")












#*****************************************
#----- save book metadata -----
#*****************************************

# save to google sheets
sheet <- as_sheets_id("https://docs.google.com/spreadsheets/d/1hBcY0ZTgPz7ZquAcFyp8WqlYipBuIsJ7lOGotMCf4_s/edit?gid=1383162945#gid=1383162945")
sheet_delete(sheet, list("fordash_bookmetadata"))
sheet_write(data = books, ss = sheet, sheet = "fordash_bookmetadata")

















#*******************************************
#----- concatinate all individual data -----
#*******************************************

all <- bind_rows(v,g,b,f,r) %>%
  select(!c(Genre)) %>%
  left_join(y = books, by = c("Title", "Author")) %>%
  mutate(Year = if_else(Year == "2020", true = "2021", false = Year))















#*****************************************
#----- save -----
#*****************************************
save(masterlist, all, books, file = "RData/individual_reads.RData")
# load(file = "RData/individual_reads.RData")


# save to google sheets
sheet <- as_sheets_id("https://docs.google.com/spreadsheets/d/1hBcY0ZTgPz7ZquAcFyp8WqlYipBuIsJ7lOGotMCf4_s/edit?gid=1383162945#gid=1383162945")
sheet_delete(sheet, list("fordash"))
sheet_write(data = all, ss = sheet, sheet = "fordash")























