library(tidyverse)
library(tidytext)
library(shiny)


function(input, output) {
  
  netflix <- read_csv('netflix_titles.csv')
  genres <- read_csv('genres.csv')
  directors <- read_csv('directors.csv')
  desc_all <- read_csv('desc_all.csv')
  main_characters <- read_csv('main_characters.csv')
  countries <- read_csv('countries.csv')
  cast_members <- read_csv('cast_members.csv')
  
  #define functions
  spread_netflix <- function(show_id, var) {
    a <- data.frame(show_id = show_id, 
                    var = str_split(as.character(var), pattern = ', '))
    names(a) <- c('show_id','var')
    a[is.na(a$var),2] <- ''
    spread_df <<- rbind(spread_df, a)
  }
  
  desc <- data.frame()
  spread_desc <- function(show_id, var) {
    a <- data.frame(show_id = show_id, 
                    desc = str_split(as.character(var), pattern = ' '))
    names(a) <- c('show_id','word')
    a$word <- gsub('[\\,.;:!?"]','',a$word)
    a$word <- a$word %>% str_to_lower()
    a <- anti_join(a, stop_words, by= 'word')
    a$len <- str_length(a$word)
    desc <<- rbind(desc, a)
  }
  
  get_matches <- function(show_id, df, boost) {
    my_selection <- df[df$show_id == show_id,]
    my_selection_chr <- my_selection$var
    matching_titles <- subset(df, var %in% my_selection_chr)
    matching_titles <- matching_titles[matching_titles$show_id != show_id,]
    matching_titles <- matching_titles[matching_titles$var != '',]
    matching_titles <- matching_titles %>%
      group_by(show_id, var, type) %>%
      summarise(count = n() + boost, .groups = 'keep')
  }
  
  suggest_titles <- function(title) {
    show_id <- netflix[netflix$title == title,1] %>% as.character()
    match_countries <- get_matches(show_id, df = countries, boost = 0)
    match_countries$count <- ifelse(match_countries$var == 'United States', yes = match_countries$count, no = match_countries$count + 2)
    
    match_desc <- get_matches(show_id, df = desc_all, boost = 2)
    match_genres <- get_matches(show_id, df = genres, boost = 0)
    match_directors <- get_matches(show_id, df = directors, boost = 5)
    match_main_characters <- get_matches(show_id, df = main_characters, boost = 5)
    
    match_cast <- get_matches(show_id, df = cast_members, boost = 2)
    match_cast <- anti_join(match_cast,match_main_characters, by = 'show_id')
    
    all_titles <- rbind(match_countries, 
                        match_genres, 
                        match_directors, 
                        match_main_characters, 
                        match_cast,
                        match_desc)

    all_titles <- merge(all_titles, netflix, by = 'show_id')
    
    
    suggested_titles <- all_titles %>%
      group_by(show_id) %>%
      summarise(tally = sum(count), .groups = 'keep')
    
    suggested_titles <- arrange(suggested_titles, desc(tally))
    top_picks <- suggested_titles %>% slice_max(tally, n = 5)
    
    top_picks <- merge(top_picks,netflix, by.x = 'show_id') %>% arrange(desc(tally))
    top_picks <- top_picks[1:5,]
    top_picks_df <- data.frame(title = paste0(top_picks$title,'\n'), 
                               rank = paste0(seq(1,nrow(top_picks),1),'.'))
    top_picks_df$both <- paste(top_picks_df$rank, top_picks_df$title, sep = ' ')
   
    
  }
  
  output$result <- renderText({
    
    
    results <- suggest_titles(input$show)
    results <- append(paste0("You chose '",input$show,"'. Based on your choice, you might like:\n"), results)
    
    if(input$show != 'Select a show' && input$show != '')  {
      print(results)
    }
  })
}





