
# pkgs
#devtools::install_github("JohnCoene/echarts4r")
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(magrittr)
library(tidytext)
library(googlesheets4)
library(echarts4r)
library(gfonts)
library(waiter)
library(DT)
library(shinyWidgets)


# set background urls -- for ease, I'm calling them here
top1 <- "https://images-na.ssl-images-amazon.com/images/S/compressed.photo.goodreads.com/books/1600789291i/40864002.jpg"
top2 <- "https://images-na.ssl-images-amazon.com/images/S/compressed.photo.goodreads.com/books/1587128688i/46041199.jpg"
top3 <- "https://images-na.ssl-images-amazon.com/images/S/compressed.photo.goodreads.com/books/1525132805i/38633526.jpg"
top4 <- "https://images-na.ssl-images-amazon.com/images/S/compressed.photo.goodreads.com/books/1565909496i/35959740.jpg"
bottom1 <- "https://images-na.ssl-images-amazon.com/images/S/compressed.photo.goodreads.com/books/1573764213l/52219386.jpg"
bottom2 <- "https://images-na.ssl-images-amazon.com/images/S/compressed.photo.goodreads.com/books/1415428227i/20518872.jpg"
bottom3 <- "https://images-na.ssl-images-amazon.com/images/S/compressed.photo.goodreads.com/books/1550135418i/22544764.jpg"
bottom4 <- "https://images-na.ssl-images-amazon.com/images/S/compressed.photo.goodreads.com/books/1614273529i/43884209.jpg"


# open datasets 
load(file = "RData/individual_reads.RData")
