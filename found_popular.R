library(jsonlite)
library(tidyverse)
pl1.JSON <- fromJSON("./spotify_million_playlist_dataset/data/mpd.slice.0-999.json",
                      flatten = TRUE)
pl2.JSON <- fromJSON("spotify_million_playlist_dataset/data/mpd.slice.10000-10999.json",
                     flatten = TRUE)

pl3.JSON <- fromJSON("./spotify_million_playlist_dataset/data/mpd.slice.4000-4999.json",
                     flatten = TRUE)['playlists']



df2 <- pl3.JSON$playlists %>% filter(num_followers > 50)
ppp<- rbind(popular_pl, df2)
  


found_popular <- function(start, end, threshold){
  # found popular lists to be added to df from playlist file of "*-[start]-[end].json"

  file <- paste(getwd(), "/spotify_million_playlist_dataset/data/mpd.slice.", 
                format(start, scientific = FALSE), "-", end, ".json", sep="")
  print(file)
  temp_json <- fromJSON(file, flatten = TRUE)
  temp_pl <- temp_json$playlists %>% filter(num_followers > threshold)
  temp_pl %>% count()
  return(temp_pl)
}


popular_pl <- pl1.JSON$playlists %>% filter(num_followers > 50)

start <- 1000
end <- 1999
final_end <- 1000000

while (end < final_end){
  popular_pl <- bind_rows(popular_pl, found_popular(start, end, 50))
  start <- end + 1
  end <- end + 1000
}

save(popular_pl, file = "popularPLs.RData")

# found 121 popular playlists from the the first 100000 files
# found the rest

start <- 100000
end <- 100999
final_end <- 1000000

while (end < final_end){
  popular_pl <- bind_rows(popular_pl, found_popular(start, end, 50))
  start <- end + 1
  end <- end + 1000
}



playlist_fields <- names(pl1.JSON[["playlists"]]) 
track_fields <- names(pl1.JSON[["playlists"]][["tracks"]][[1]])

plot_num_followers <- function(js){
  num_fol = js[["playlists"]][["num_followers"]]
  num_fol
  ggplot(num_fol) + geom_bar()
}
plot_num_followers(pl1.JSON)
check_any_missing <- function(js){
  for (f in playlist_fields){
    if (f=="tracks") {
      map(js[["playlists"]][["tracks"]], function(x){
        for (tf in track_fields){
          if(any(is.na(x[tf])==TRUE)){
            cat(sprintf("%s of trakcs has na\n", tf))}
        }
      })
      
      
    }else{
      temp <- is.na(js[["playlists"]][f])
      if (any(temp)==TRUE){
        cat(sprintf("%s has na\n how many: %d", f, sum(temp)))
      }
    }
  }
}

# get popular ones
pl2.JSON[["playlists"]] %>%filter(num_followers > 1)%>% arrange(desc(num_followers) )%>% 
  ggplot(aes(x = num_followers)) + geom_bar(width=1, alpha=0.7) + 
  scale_x_continuous(limits = c(0, 12), breaks = seq(2, 12, by = 1))
