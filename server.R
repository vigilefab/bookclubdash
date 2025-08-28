shinyServer(function(input, output){
  
  custom_theme <- bs_theme(version = 5)

  e_common(
    font_family = "Poppins",
    theme = "walden"
  )
  
  # ----------------------- ABOUT ----------------------------
  
  # about: # books ----
  output$num_books <- renderText({
    length(unique(all$Title))
  })
  
  
  
  # about: # authors ----
  output$num_authors <- renderText({
    length(unique(all$Author))
  })
  
  
  # about: # readers ----
  output$num_readers <- renderText({
    length(unique(all$Reader))
  })
  
  # about: # genres ----
  output$num_genres <- renderText({
    genre <- all %>% ungroup %>%
      unnest_tokens(output = Genre, input = Genre) %>% 
      # count words
      count(Genre, sort = TRUE)
    nrow(genre)
  })
  
  # recom: reader subset -----
  recom_dat <- reactive({
    if (input$recom == "Bookclub") bookclub_books else filter(all, Reader == input$recom) 
  })
  
  # recommended books ----
     output$book_recos <- renderUI({
       # generate data based on input
       if (input$recom == "Bookclub") {
         dat <- bookclub_books %>% ungroup %>%
           mutate(Rating = round(Ave_Rating, digits = 2)) %>%
           select(!Year) %>% distinct
       } else {
           dat <- recom_dat() %>% ungroup %>%
             mutate(Rating = round(as.numeric(Rating), digits = 2)) %>%
             select(Title, Author, Rating)
           }
       
       dat <- dat %>% ungroup %>%
         arrange(desc(Rating)) %>%
         
         # get the top 10
         head(10) %>%
         
         # randomly pick three 
         mutate(random = runif(10)) %>% arrange(random) %>% head(4)
       
       title <- dat$Title
       author <- dat$Author
       cover <- dat %>% left_join(x = dat, y = select(books, c(Title, Author, url)), by = c("Title", "Author")) 
       cover <- cover$url
                
       # ui
       
       reco_list <- lapply(seq_along(title), function(i) {
         div(class = "col-lg",
              img(src = cover[i], width = "50%", style = "align: center;"), 
              h5(title[i]),
              p("by", author[i])
             #)
         )
         })
       
       # Convert the list to a flowlayout - this is necessary for the list of items
       # to display properly.
       do.call(flowLayout, reco_list)

       })
  
  
  
  
  
  
  # ----------------------- BOOKCLUB ----------------------------
  
  
  # club dat: book match ----
  # all books with at least two readers
  bookclub_books <- all %>% select(Title, Author, Genre, Year, Reader, Rating) %>% 
    
    # take only most recent reread (if any)
    group_by(Title, Reader) %>%
    arrange(Title, Reader, desc(Year)) %>%
    mutate(n = row_number(),
           Rating = as.numeric(Rating)) %>%
    filter(n == 1) %>% select(!n) %>%
    
    # reshape wide to get person ratings in columns
    pivot_wider(names_from = Reader, values_from = c(Rating, Year)) %>%
    filter(rowSums(is.na(across(starts_with("Rating")))) < 4) %>%
    
    # average ratings
    mutate(Ave_Rating = rowMeans(across(starts_with("Rating")), na.rm = TRUE)) %>%
    
    # reshape long to get books that were read in separate years
    pivot_longer(cols = starts_with("Year"), names_to = "var", values_to = "Year") %>%
    select(!var) %>% filter(!is.na(Year)) %>% distinct
  
  
  
  
  
  # club: # bookclub books ----
  output$club_books <- renderText({
    paste0(length(unique(bookclub_books$Title)))
    })

  
  
  
  
  
  
  # club: ratings chart ----
  output$ratings <- renderEcharts4r({
    # generate data
    dat <- bookclub_books %>% ungroup %>%
      arrange(desc(Ave_Rating)) %>%
      mutate(Ave_Rating = round(Ave_Rating, digits = 2),
             Average_Rating = Ave_Rating*4,
             Gen = -Rating_Gen,
             Vigile = -Rating_Vigile,
             Bianca = -Rating_Bianca,
             France = -Rating_France,
             Rachel = -Rating_Rachel
      )

   # filter if year is selected
   if (isTruthy(selected_yr())) dat <- dat %>% filter(Year == selected_yr())

   # filter if genre is selected
   if (isTruthy(selected_genre())) {
     dat <- dat %>% filter(grepl(gsub(x = as.character(selected_genre()),
                                                pattern = " ",
                                                replacement = "_"), Genre))
   }

   # remove year and keep only distinct rows (re-reads cause double rows with different years)
   dat <- dat %>% select(!Year) %>% distinct

   # generate chart
   dat %>%
      e_charts(x = Title) %>%
      e_title("Book Ratings", left = "center") %>%
      e_scatter(serie = Ave_Rating,
                size = Average_Rating,
             legend = list(show = FALSE),
            #label = list(show = FALSE),
            itemStyle = list(color = "gray")
      ) %>%
     e_bar(Gen, stack = "grp") %>%
     e_bar(Vigile, stack = "grp") %>%
     e_bar(Bianca, stack = "grp") %>%
     e_bar(France, stack = "grp") %>%
     e_bar(Rachel, stack = "grp") %>%
     e_y_axis(show = FALSE) %>%
     e_x_axis(position = "top",
              axisTick = list(show = FALSE),
              axisLine =list(show = TRUE),
              axisLabel = list(show = TRUE, alignMinLabel = "left")
     ) %>%
     e_tooltip(textStyle = list(fontSize = 12),
               trigger = "axis", formatter = htmlwidgets::JS('
                                                          function (params) {
            let tooltip = `<div  style="width: 130px;float:left;white-space: normal;"><p><strong>${params[0].axisValue}</strong></p>`;
            let total = 0
            params.forEach(({ seriesName, marker, value }) => {
              value = value || [0, 0];
              let neg_val  = value[1] == null ? "-" : value[1] < 0 ? -value[1]: value[1];
              tooltip += `<p style="width: 130px;float: left;margin-bottom:0;"><span style="float: left;">${marker} ${seriesName}</span><span style="float: right;"><strong>${neg_val}</strong></span></p>`;
            });

            return tooltip;
          }')) %>%
      e_legend(show = TRUE, bottom =0) %>%
      e_hide_grid_lines(which = c("x", "y")) %>%
      e_grid(top = 60, bottom = 20, left = "5%", right = "0%")
  })
  

  
  
  
  
  
  
    
  # club: years chart ----
  output$club_years <- renderEcharts4r({
    # generate data
    dat <- bookclub_books %>% group_by(Year) %>%
      summarize(Books = n()) 
    
    # highlight selected year
    if (isTruthy(selected_yr())) {
      dat <- dat %>%
        mutate(color = if_else(Year == selected_yr(), true = "black", false = "gray"))
    } else {
      dat <- dat %>% mutate(selected = "unselected", color = "gray")
    }

    # create chart
    dat %>% e_charts(x = Year) %>%
      e_title("Books per Year", left = "center") %>% #, subtext = "Click on a bar to filter by year."
      e_bar(serie = Books, name = "Books",
            label = list(show = TRUE, 
                         position = "insideRight",
                         color = "white")) %>%
      e_add_nested("itemStyle", color) %>%
      e_tooltip(trigger = "axis") %>%
      e_y_axis(show = FALSE) %>%
      e_x_axis(
        axisTick = list(show = FALSE),
        axisLine = list(show = FALSE),
        inverse =  TRUE) %>%
      e_flip_coords() %>%
      e_legend(show = FALSE) %>%
      e_hide_grid_lines(which = c("x", "y")) %>%
      e_grid(top = 30, bottom = 0, left = "20%")

    })

  
  # club: year filter ----
  # Track the filter year in a reactive value so it can be
  # update independently of the chart
  selected_yr <- reactiveVal(value = NULL)

  # Update the filter year on bar click
  observeEvent(input$club_years_clicked_data, {
    if (isTruthy(selected_yr())) # if year is selected, unselect
      {
      if (selected_yr() == input$club_years_clicked_data$value[2]) {
        selected_yr(NULL)
      } else {
        selected_yr(input$club_years_clicked_data$value[2]) # if selected year is diff from clicked year, select the clicked year
        }
    } else {
      selected_yr(input$club_years_clicked_data$value[2]) # otherwise, select the clicked year
     }
  })
  
  
  
  
  # club: year text ----
  output$club_yr_text <- renderText({
    if (isTruthy(selected_yr())) paste("Year:", as.character(selected_yr())) else "Year: all years"
    })

  
  
  
  
  
  
  # club: genre subset ----
  genre <- reactive({
  if (isTruthy(selected_yr())) {
    genre <- bookclub_books %>% ungroup %>% 
      filter(Year == selected_yr()) %>%
      unnest_tokens(output = Genre, input = Genre) %>%
      count(Genre, sort = TRUE) %>%
      mutate(Books = n, 
             #color = "black", 
             Genre = gsub(x = Genre, pattern = "_", replacement = " ")) 
  } else {
    genre <- bookclub_books %>% ungroup %>% 
      unnest_tokens(output = Genre, input = Genre) %>%
      count(Genre, sort = TRUE) %>%
      mutate(Books = n, 
             #color = "black", 
             Genre = gsub(x = Genre, pattern = "_", replacement = " ")) 
  }
  
  })

  
  
  
  
  # club: genre chart ----
  output$club_genre <- renderEcharts4r({
    if (isTruthy(selected_genre())) {
      dat <- genre() %>% 
        mutate(color = if_else(Genre == as.character(selected_genre()), true = "#3fb1e3", false = "black")) 
    } else {
      dat <- genre() %>% 
        mutate(color = "black")
    }
      
    dat <- dat %>%
      e_charts() %>%
      e_title("Genre Frequency", left = "center") %>%
      e_cloud(word = Genre, freq = Books, color = color, shape = "circle", 
              sizeRange = c(10,50), shrinkToFit = TRUE, gridSize = 2, rotationRange = c(0,0),
              drawOutOfBound = FALSE) %>%
      e_tooltip(trigger = "item") 
  })
  
  
  # club: genre filter ----
  # Track the filter genre in a reactive value so it can be
  # update independently of the chart
  selected_genre <- reactiveVal(value = "")
  
  # Update the filter year on bar click
  observeEvent(input$club_genre_clicked_row, {
    genre_text <- genre()[input$club_genre_clicked_row, "Genre"]
    if (isTruthy(selected_genre())) # if year is selected, unselect
    {
      if (selected_genre() == genre_text) {
        selected_genre("")
      } else {
        selected_genre(genre_text) # if selected year is diff from clicked year, select the clicked year
      }
    } else {
      selected_genre(genre_text) # otherwise, select the clicked year
    }
  })
  
  
  
  
  # club: genre text ----
  output$club_genre_text <- renderText({
    if (isTruthy(selected_genre())) paste("Genre:", as.character(selected_genre())) else "Genre: all genres"
    })
  
  
  
  
  

  
  
  
  # club: recommenders ----
  output$club_reco <- renderEcharts4r({
    # get data 
    dat <- masterlist %>% 
      group_by(Recommender) %>% 
      summarize(Books = n()) %>% arrange(desc(Books)) %>% na.omit %>%
      filter(Recommender != "") %>%
      mutate(Recommender = gsub(x = Recommender, pattern = "Vigileile", replacement = "Vigile"))
    
    
    # create chart
    dat %>% e_charts(x = Recommender) %>%
      e_title("Book Recommenders", left = "center")  %>%
      e_pie(serie = Books, name = "Books", roseType = "radius") %>%
      e_legend(show = FALSE) %>%
      e_tooltip(trigger = "item") %>%
      #e_theme("walde") %>%
      e_grid(top = 60, bottom = 0) #, left = "20%")
    
  })
  
  
  
  
  
  # club table ----
  output$club_table <- renderDT({
    # get data
    dat <- bookclub_books %>%
      mutate(Ave_Rating = round(Ave_Rating, digits = 2))
    
    # clean column names
    colnames(dat) <- gsub(x = colnames(dat), pattern = "Rating_", replacement = "")
    
    # filter year
    if (isTruthy(selected_yr()))  dat <- dat %>% filter(Year == selected_yr())
    
    # filter genre
    if (isTruthy(selected_genre())) {
      dat <- dat %>% filter(grepl(gsub(x = as.character(selected_genre()), 
                                       pattern = " ",
                                       replacement = "_"), Genre))
    }
    dat %>% select(!c(Genre, Year)) %>% distinct %>% arrange(desc(Ave_Rating))
    
  })
  
  
  
  
  
  
  
  
  
  # ----------------------- INDIVIDUAL ----------------------------
  
  # indiv: reader subset -----
  reader_dat <- reactive({
    filter(all, Reader == input$reader) %>%
      mutate(Form = gsub(x = Form, pattern = "Audio", replacement = "Audiobook")) %>%
      mutate(Form = gsub(x = Form, pattern = "-", replacement = "")) %>%
      mutate(Form = gsub(x = Form, pattern = "Physical", replacement = "Paperback"))
  })
  
  # indiv: name ----
  output$reader_name <- renderText({
    input$reader
  })
  
  # indiv: name ----
  output$indiv_name <- renderText({
    as.character(input$reader)
  })
  
  # indiv stat: # books ----
  output$reader_numbooks <- renderText({
    length(unique(reader_dat()$Title))
  })
  
  
  
  # indiv stat: # authors ----
  output$reader_numauthor <- renderText({
    length(unique(reader_dat()$Author))
  })
  
  
  
  # indiv stat: # genres ----
  output$reader_numgenres <- renderText({
    genre <- reader_dat() %>% ungroup %>%
      unnest_tokens(output = Genre, input = Genre) %>% 
      # count words
      count(Genre, sort = TRUE)
    nrow(genre)
  })
  
  
  
  # indiv stat: pref medium ----
  output$reader_prefmed <- renderText({
    dat <- reader_dat() %>% group_by(Form) %>% 
      summarize(n = n()) %>% arrange(desc(n)) %>% na.omit %>%
      filter(Form != "") 
    
    as.character(dat[1,1])
  })
  
  
  
  # indiv: ratings chart ----
  output$reader_ratings <- renderEcharts4r({
    # get data
    dat <- reader_dat() %>%
      select(Title, Rating, Year, Genre) 
    
    
    # filter if year is selected
    if (isTruthy(selected_yr_indiv())) dat <- dat %>% filter(Year == selected_yr_indiv())
    
    # filter if genre is selected
    if (isTruthy(selected_genre_indiv())) {
      dat <- dat %>% filter(grepl(gsub(x = as.character(selected_genre_indiv()),
                                       pattern = " ",
                                       replacement = "_"), Genre))
    }
    
    # remove year and keep only distinct rows (re-reads cause double rows with different years)
    dat <- dat %>% group_by(Title) %>%
      summarize(Rating = mean(as.numeric(Rating))) %>% 
      arrange(desc(Rating)) %>%
      filter(!is.na(Rating))
    
    # generate chart
    dat %>% e_charts(x = Title) %>%
      e_title(paste0(input$reader, "'s Book Ratings"), left = "center") %>% #, subtext = "Click on a bar to filter by year."
      e_bar(serie = Rating, name = "Rating") %>%
      e_tooltip(trigger = "axis") %>%
      e_y_axis(show = FALSE)  %>%
    #   e_x_axis(position = "bottom",
    #            axisTick = list(show = FALSE),
    #            axisLine =list(show = TRUE),
    #            axisLabel = list(show = TRUE, alignMinLabel = "left"),
    #   ) %>%
      e_legend(show = FALSE) %>%
      e_color("#96dee8")   %>%
      # e_hide_grid_lines(which = c("x", "y")) %>%
      e_grid(top = 30, bottom = 30, left = "7%", right = "0%")
  })
  
  
  
  
  
  
  
  # indiv: year chart ----
  output$indiv_years <- renderEcharts4r({
    # generate data
    dat <- reader_dat() %>% group_by(Year) %>%
      summarize(Books = n()) 
    
    # highlight selected year
    if (isTruthy(selected_yr_indiv())) {
      dat <- dat %>%
        mutate(color = if_else(Year == selected_yr_indiv(), true = "black", false = "gray"))
    } else {
      dat <- dat %>% mutate(selected = "unselected", color = "gray")
    }
    
    # create chart
    dat %>% e_charts(x = Year) %>%
      e_title("Books per Year", subtext = "Click on a bar to filter by year.", left = "center") %>% 
      e_bar(serie = Books, name = "Books",
            label = list(show = TRUE, 
                         position = "insideRight",
                         color = "white")) %>%
      e_add_nested("itemStyle", color) %>%
      e_tooltip(trigger = "axis") %>%
      e_y_axis(show = FALSE) %>%
      e_x_axis(
        axisTick = list(show = FALSE),
        axisLine = list(show = FALSE),
        inverse =  TRUE) %>%
      e_flip_coords() %>%
      e_legend(show = FALSE) %>%
      e_hide_grid_lines(which = c("x", "y")) %>%
      e_grid(top = 50, bottom = 0, left = "20%")
    
  })
  
  
  # indiv: year filter ----
  # Track the filter year in a reactive value so it can be
  # update independently of the chart
  selected_yr_indiv <- reactiveVal(value = NULL)
  
  # Update the filter year on bar click
  observeEvent(input$indiv_years_clicked_data, {
    if (isTruthy(selected_yr_indiv())) # if year is selected, unselect
    {
      if (selected_yr_indiv() == input$indiv_years_clicked_data$value[2]) {
        selected_yr_indiv(NULL)
      } else {
        selected_yr_indiv(input$indiv_years_clicked_data$value[2]) # if selected year is diff from clicked year, select the clicked year
      }
    } else {
      selected_yr_indiv(input$indiv_years_clicked_data$value[2]) # otherwise, select the clicked year
    }
  })
  
  
  
  
  # indiv: year text ----
  output$club_yr_text <- renderText({
    if (isTruthy(selected_yr_indiv())) paste("Year:", as.character(selected_yr_indiv())) else "Year: all years"
  })
  
  
  
  
  
  
  
  
  # indiv: genre subset ----
  genre_ind <- reactive({
      if (isTruthy(selected_yr_indiv())) {
        genre <- reader_dat() %>% ungroup %>%
          filter(Year == selected_yr_indiv()) %>%
          unnest_tokens(output = Genre, input = Genre) %>%
          count(Genre, sort = TRUE) %>%
          mutate(Books = n,
                 #color = "black",
                 Genre = gsub(x = Genre, pattern = "_", replacement = " "))
      } else {
        genre <- reader_dat() %>% ungroup %>%
          unnest_tokens(output = Genre, input = Genre) %>%
          count(Genre, sort = TRUE) %>%
          mutate(Books = n,
                 #color = "black",
                 Genre = gsub(x = Genre, pattern = "_", replacement = " "))
      }

  })
  
  
  
  
  
  # indiv: genre chart ----
  output$indiv_genre <- renderEcharts4r({
    if (isTruthy(selected_genre_indiv())) {
      dat <- genre_ind() %>% 
        mutate(color = if_else(Genre == as.character(selected_genre_indiv()), true = "#3fb1e3", false = "black")) 
    } else {
      dat <- genre_ind() %>% 
        mutate(color = "black")
    }
    
    dat <- dat %>%
      e_charts() %>%
      e_title("Genre Frequency", subtext = "Click on a word to filter by genre.", left = "center") %>%
      e_cloud(word = Genre, freq = Books, color = color, shape = "circle", 
              sizeRange = c(10,50), shrinkToFit = TRUE, gridSize = 2, rotationRange = c(0,0),
              drawOutOfBound = FALSE) %>%
      e_tooltip(trigger = "item") %>%
      e_grid(height = "70%")
    
  })
  
  
  # indiv: genre filter ----
  # Track the filter genre in a reactive value so it can be
  # update independently of the chart
  selected_genre_indiv <- reactiveVal(value = "")
  
  # Update the filter year on bar click
  observeEvent(input$indiv_genre_clicked_row, {
    genre_text <- genre_ind()[input$indiv_genre_clicked_row, "Genre"]
    if (isTruthy(selected_genre_indiv())) # if year is selected, unselect
    {
      if (selected_genre_indiv() == genre_text) {
        selected_genre_indiv("")
      } else {
        selected_genre_indiv(genre_text) # if selected year is diff from clicked year, select the clicked year
      }
    } else {
      selected_genre_indiv(genre_text) # otherwise, select the clicked year
    }
  })
  
  
  
  
  # indiv: genre text ----
  output$indiv_genre_text <- renderText({
    if (isTruthy(selected_genre_indiv())) paste("Genre:", as.character(selected_genre_indiv())) else "Genre: all genres"
  })
  
  
  
  
  # indiv: author chart ----
  output$reader_mostauth <- renderEcharts4r({
    # get data 
    dat <- reader_dat() %>% 
      group_by(Author) %>% 
      summarize(Books = n()) %>% arrange(desc(Books)) %>% na.omit %>%
      filter(Author != "")  %>% head(dat, n = 8)
    
    
    # create chart
    dat %>% e_charts(x = Author) %>%
      e_title("Most Read Authors", left = "center")  %>%
      e_pie(serie = Books, name = "Books", roseType = "radius", radius = "60%") %>%
      e_legend(show = FALSE) %>%
      e_tooltip(trigger = "item") %>%
      e_grid(top = 50, bottom = 0, left = "20%")
    
  })
  
  
  
  # indiv: medium chart ----
  output$book_medium <- renderEcharts4r({
    # get data
    dat <- reader_dat() %>%
      group_by(Form) %>%
      summarize(Books = n()) %>% na.omit %>%
      filter(Form != "") 

    if (nrow(dat) > 0) {
      dat %>% e_charts(x = Form) %>%
        e_pie(serie = Books, name = "Books", radius = c("30%", "60%")) %>%
        e_title("Book Medium", left = "center") %>%
        e_tooltip(trigger = "item") %>%
        e_legend(show = FALSE) %>%
        e_grid(top = 10, bottom = 0, left = "0%", right = "0%")
    }

  })
  
  
  
  
  
  
  # indiv table ----
  output$indiv_table <- renderDT({
    # get data
    dat <- reader_dat() %>%
      select(!c(Notes, Date_finished, start_date))
    
    # clean column names
    colnames(dat) <- gsub(x = colnames(dat), pattern = "end_date", replacement = "Date Finished")
    
    # filter year
    if (isTruthy(selected_yr_indiv()))  dat <- dat %>% filter(Year == selected_yr_indiv())
    
    # filter genre
    if (isTruthy(selected_genre_indiv())) {
      dat <- dat %>% filter(grepl(gsub(x = as.character(selected_genre_indiv()), 
                                       pattern = " ",
                                       replacement = "_"), Genre))
    }
    dat %>% select(c(Title, Author, Reread, Form, `Date Finished`, Rating)) %>% distinct %>% arrange(desc(Rating))
    
  })
  
  
  
  
  
  
  
  
  
  
  
   
  # themes ----
  # bs_themer() 
  
})
