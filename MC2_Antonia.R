
################## Funktion Cosinus-Similarity ##################
# Parameter: 2 Matrizen für die die Cosine-Ähnlichkeit bestummen werden soll
# Output: Matrix mit den Cosine-Similarities 

calc_cos_similarity_twomx <- function(m1, m2){

  numerator <- (m1 %*% t(m2))
  denominator <- sqrt(rowSums(m1^2) %*% t(rowSums(m2^2)))
  cos_sim <- numerator / denominator
  return(cos_sim)
} 


################## 5-Punkt-Statistik Funktion ##################
stats <- function(df){
  quants <- as.data.frame(t(quantile(df,na.rm = TRUE))) %>% 
  mutate(Mean = mean(df, na.rm=TRUE), 
         NAs = sum(is.na(df)))
  colnames(quants) <- c("Min", "25%", "Median", "75%", "Max", "Mean", "NAs")
return(quants)
}

##################Funktion für TopN Empfehlungen ##################
get_topn_recos <- function(df,N=20){
  df_topN <- rownames_to_column(as.data.frame(df), "user") %>% 
    pivot_longer(!user, names_to = "movie", values_to = "value") %>% 
    arrange(user,desc(value)) %>% 
    mutate(rank = rep(1:ncol(df), nrow(df))) %>% 
    filter(rank <= N)
  
  return(df_topN)
}


################## Funktion zur Visualisierung der Minimalen Ähnlichkeits-Scores ##################

# Parameter: df: Datensatz mit Cosine-Ähnlichkeitm Ns: Liste von N für TopN Empfehlungen  
# Output: Histogram der minimalen Ähnlichkeitsscores   

analyze_topn_recos <- function(df, Ns){
  ##### Datensatz erstellen mit den TopN Empfehlungen pro User ##### 
  min_scores <- data.frame()
  for (n in Ns){
    recs <- get_topn_recos(df, n)
    recs <- recs %>% 
      group_by(user) %>% 
      summarize(min_score = min(value),
                N = n)
    min_scores <- rbind(min_scores, recs)}
  
  min_scores <- min_scores %>% arrange(user, N)
  
  ##### Plot erstellen   ##### 
  plot1 <- ggplot(min_scores, aes(min_score))+
    geom_histogram(bins = 100)+
    facet_wrap(~paste0("Top", N))+
    labs(title = "Verteilung der minimalen Scores in den Toplisten",
         x = "minimale Cosine-Similarity",
         y = "Anzahl")
  
  plot2 <- min_scores %>%
    mutate(N = as.factor(N)) %>% 
    ggplot(aes(x = N, y = min_score, fill = N))+
    geom_boxplot()+
    labs(title = "Verteilung der minimalen Scores",
         x = "TopN",
         y = "Minimale Aehnlichkeit")+
    theme_minimal()
  
  return(list(min_scores, plot1, plot2))
  
}


################## Funktion Cleveland-Plot ##################
#### Dokumentation ####
#Kreiert einen Clevelandplot für beliebige Anzahl User und Ns

##### Parameter #####
#usermatrix: Matrix mit Nutzern & Genres
#moviematrix: Matrix mit Filmen und Genres
#cosim Matrix: Matrix mit der Cosine-Similarity
#users: liste von Nutzern
#Ns: Liste von Ns für TopN-Empfehlungen

##### Output: Plot-Objekt, welches nachher weiterverarbeitet werden kann (z.B. mit Facet-Wrap) #####


create_cleveland_plot <- function(usermatrix = m_user_genre, moviematrix = df_movie_genre, cosim_matrix = m_neg_csim, users, Ns){

  #### Empfehlungen erstellen & mit Genres ergänzen  #### 
  df_recos <- data.frame()
  for(N in Ns){
    df_topN <- get_topn_recos(m_neg_csim, N)
    df_topN['N'] <- N
    df_recos <- rbind(df_recos, df_topN)
  }
  df_topN_movies <- rownames_to_column(moviematrix, "movie") %>% filter(movie %in% df_recos$movie)
  df_topN_genre <- merge(df_recos, df_topN_movies, by="movie")
  
  ####   Empfehlungen umformen für Plot  #### 
  df_topN_genres_cleveland <- df_topN_genre %>% 
    arrange(user, rank) %>% 
    filter(user %in% users) %>% 
    select(-c(movie, value, rank, movie_profile)) %>% 
    pivot_longer(!c(user, N), names_to = "genre", values_to = "n") %>% 
    group_by(user, N, genre) %>% 
    summarize(n = sum(n)) %>% 
    ungroup %>% 
    mutate(tot = sum(n),
           pct = n / tot,
           source = "Empfehlungen", 
           N = N)  %>% 
    select(!tot) 
  
  df_user_genre_cleveland <- data.frame()
  
  #### Nutzerprofile umformen für Plot   #### 
  for(N in Ns){
    df_user_N <- rownames_to_column(as.data.frame((m_user_genre)), "user")  %>%
      filter(user %in% users) %>% 
      pivot_longer(!user, names_to = "genre", values_to = "n") %>% 
      mutate(tot = sum(n),
             pct = n / tot,
             source = "Nutzerprofil",
             N = N)  %>% 
      select(!c(tot)) 
    df_user_genre_cleveland <- rbind(df_user_genre_cleveland, df_user_N)
  }
  
  #### Datensätze kombinieren & Plot erstellen   #### 
  clp <- rbind(df_user_genre_cleveland, df_topN_genres_cleveland) %>% 
    ggplot(aes(x = pct, genre, color = source))+
    geom_point(alpha = 0.5) +
    geom_line(aes(group = genre), color = "black") +
    labs(title = paste("Genre-Profil für ausgewählte Nutzer:innen"),
         x = "Prozentualer Anteil", 
         y = "Genre",
         color = "Art") +
    scale_x_continuous(limits = c(0,0.3))
  
  
  return(clp)
  
}



