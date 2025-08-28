bootstrapPage(
  theme  = bs_theme(version = 5, "bslib-value-box-horizontal-break-point" = "1px") %>% bs_add_rules(sass::sass_file("custom.scss")),
  use_font("poppins", "www/css/poppins.css"),
  use_waiter(),
  # background ----
  layout_column_wrap(width = 1, heights_equal = "row", gap = "0px", style = "position:fixed;top:4%;z-index:1",
                     layout_column_wrap(width = 1/2, gap = "0px", 
                                        div(class="background-content",
                                            img(src=top1),
                                            img(src=top2)
                                        ),
                                        div(class="background-content",
                                            img(src=top3),
                                            img(src=top4)
                                        )
                     ),
                     layout_column_wrap(width = 1/2, gap = "0px", 
                                        # background ----
                                        div(class="background-content",
                                            img(src=bottom1),
                                            img(src=bottom2)
                                        ),
                                        div(class="background-content",
                                            img(src=bottom3),
                                            img(src=bottom4)
                                        )
                     )
  ),
  # navbar ----
  page_navbar(title = "Bookclub Dashboard",
              navbar_options = navbar_options(position = "fixed-top", bg = "black", fg = "white"),
              nav_panel(title = "About",
                        # about ----
                        div(class="foreground-content",
                            div(class="foreground-text",
                                h1("Welcome to the Bookclub Dashboard"),
                                p("This is just a little page that tracks the reading progress of a bunch of bookies
                                  (Vigile, Genina, Bianca, France and Rachel) since they started documenting 
                                  their book club reads in 2021. Here, both bookclub stats and individual stats are tracked."),
                                p("The Bookclub Stats compare ratings across readers for books that have been read by at 
                                least two people. Individual Stats show the book statistics of each individual 
                                  reader, including solo reads, that is, books outside of the bookclub (so far as they have been entered in the Bookclub Database). "),
                                h1("Data Overview"),
                                p(paste0("Last data refresh occured on ", format(Sys.Date(), "%d %B %Y"), ".")),
                                layout_column_wrap(width = "200px",
                                                   value_box(title = "# Books", 
                                                             value = textOutput("num_books"),
                                                             showcase = bsicons::bs_icon("book", size = "3rem"),
                                                             theme = value_box_theme(bg = "white", fg = "black")
                                                             ),
                                                   value_box(title = "# Authors", 
                                                             value = textOutput("num_authors"),
                                                             showcase = bsicons::bs_icon("pencil-square", size = "3rem"),
                                                             theme = value_box_theme(bg = "white", fg = "black")
                                                   ),
                                                   value_box(title = "# Genres", 
                                                             value = textOutput("num_genres"),
                                                             showcase = bsicons::bs_icon("heart", size = "3rem"),
                                                             theme = value_box_theme(bg = "white", fg = "black")
                                                   ),
                                                   value_box(title = "# Readers", 
                                                             value = textOutput("num_readers"),
                                                             showcase = bsicons::bs_icon("sunglasses", size = "3rem"),
                                                             theme = value_box_theme(bg = "white", fg = "black")
                                                             )
                                                   ),
                                # recommendations ----
                                h1("Book Recommendations"),
                                p("Here are a few top-rated books based on the ratings they were given. Select a name to see this person's personal favorites."),
                                radioGroupButtons(inputId = "recom", width = "100%",
                                                  choices = list("Bookclub", "Vigile", "Gen", "Rachel", "Bianca", "France"),
                                                  status = 'info', size = 'xs',
                                                  individual = TRUE,
                                                  selected = "Bookclub"),
                                
                                layout_column_wrap(width = "800px",
                                                   uiOutput("book_recos", width = "auto")
                                ), 
                                
                            )
                        )
              ),
              nav_panel(title = "Bookclub Stats",
                        # bookclub stats ----
                        div(class="foreground-content",
                            div(class="foreground-text",
                                h1("Bookclub Summary Stats"),
                                p("All the books on this tab (N =", 
                                  textOutput("club_books", inline = TRUE), 
                                  "books) were read by at least two people. A book is included 
                                  in a year if at least one person read or reread the 
                                book in that year. Also, books can have multiple genres. 
                                Click on a year or genre to filter the ratings, unclick to reset."),
                                #p(textOutput("indiv_name")),
                                layout_column_wrap(
                                                   # bookclub ratings ----
                                                   echarts4rOutput("ratings", height = "250px")
                                                   ),      
                                layout_column_wrap(echarts4rOutput("club_years", height = "250px"), 
                                                   echarts4rOutput("club_genre", height = "250px")
                                  ), 
                                layout_column_wrap(
                                                   echarts4rOutput("club_reco")
                                ), 
                                h4("Bookclub Ratings Table"),
                                p(textOutput("club_yr_text", inline=TRUE), " | ",
                                  textOutput("club_genre_text", inline=TRUE)),
                                layout_column_wrap(style = "font-size: 80%;",
                                                   DTOutput("club_table"))
                            )
                        )
                        ),
              nav_panel(title = "Individual Stats",
                        # individual stats ----
                        div(class="foreground-content",
                            div(class="foreground-text",
                                h1("Individual Statistics - ", textOutput("indiv_name", inline = TRUE)),
                                p("This tab shows individual reading statistics for each member of the bookclub.
                                Stats include all books and information documented by this member in the Bookclub Database. 
                                  Ratings are averages of all reads and re-reads. Select a name to see someone's reading statistics."),
                                div(class = "row",
                                    div(class = "col-lg-2", 
                                        radioGroupButtons(inputId = "reader", width = "100%",
                                                          choices = list("Vigile", "Gen", "Rachel", "Bianca", "France"),
                                                          status = 'info', size = 'xs',
                                                          justified = FALSE, 
                                                          selected = "Vigile", 
                                                          direction = "vertical"),
                                        div(style = "font-size: 70%;",
                                            value_box(title = "# Books", 
                                                      value = textOutput("reader_numbooks"),
                                                      showcase = bsicons::bs_icon("book", size = "2rem"),
                                                      theme = value_box_theme(bg = "white", fg = "black")
                                                      ),
                                            value_box(title = "# Authors", 
                                                      value = textOutput("reader_numauthor"),
                                                      showcase = bsicons::bs_icon("pencil-square", size = "2rem"),
                                                      theme = value_box_theme(bg = "white", fg = "black")
                                                      )
                                            ),
                                        div(style = "font-size: 70%;",
                                            value_box(title = "# Genres", 
                                                  value = textOutput("reader_numgenres"),
                                                  showcase = bsicons::bs_icon("heart", size = "2rem"),
                                                  theme = value_box_theme(bg = "white", fg = "black")
                                                  ),
                                            value_box(title = "Preferred Medium", 
                                                  value = textOutput("reader_prefmed"),
                                                  showcase = bsicons::bs_icon("person", size = "2rem"),
                                                  theme = value_box_theme(bg = "white", fg = "black")
                                                  )
                                        )
                                        
                                                       ), # end sidebar
                                    div(class = "col-lg-10",
                                        layout_column_wrap(width = "200px",
                                                           # individual ratings ----
                                                           echarts4rOutput("reader_ratings", height = "150px")
                                                           ),
                                        layout_column_wrap(width = "200px", 
                                                           echarts4rOutput("indiv_years", height = "250px"),
                                                           echarts4rOutput("indiv_genre", height = "290px")
                                        ),
                                        layout_column_wrap(width = "200px",
                                                           echarts4rOutput("reader_mostauth"),
                                                           echarts4rOutput("book_medium")
                                                           
                                                           ),
                                        h4("Book-Ratings Table -", textOutput("reader_name", inline = TRUE)),
                                        p(textOutput("indiv_yr_text", inline=TRUE), " | ",
                                          textOutput("indiv_genre_text", inline=TRUE)),
                                        layout_column_wrap(style = "font-size: 80%;",
                                                           DTOutput("indiv_table")
                                                           )
                                        ) 
                                
                                )
                            )
                        )
              )
  )
)

