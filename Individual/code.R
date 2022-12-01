library(tidyverse)
library(scales)
library(patchwork)
library(httpgd)

animeData <- read_csv(file="c:\\Users\\pavel\\Downloads\\Software for Data Science\\Individual\\anime_data.csv")
# View(animeData)
print("Data description:")
print("animeID - id of the anime")
print("name - name of the anime")
print("title_english - english title")
print("title_japanese - japanese title")
print("title_synonysm - alternate titles in english")
print("type - type of the anime (e.g. TV, Movie, OVA, ONA)")
print("source - source of the anime (e.g. original, manga, ranobe)")
print("producers - production studio(s)")
print("genre - genre")
print("studio - executive studio(s)")
print("episodes - number of episodes")
print("status - status of the show (airing, finished, on pause")
print("airing - boolean of the airing")
print("start_date - date when the airing started")
print("end_date - date when the airing finished")
print("duration - duration of the anime")
print("ratint - age rating of the show")
print("score - score of the show")
print("scored_by - number of people who voted")
print("rank - overall rank of the show")
print("popularity - popularity index of the show")
print("members - those who added the show to 'watched' list")
print("favorites - those who added the show to 'favorites' list")
print("synopsis - synopsis of the show")
print("background - background of the show")
print("premiered - date of the premiere")
print("broadcast - days of the broadcast")
print("related - related shows")

print("Data tyding:")
print("Removing unneccessary data")
animeData <- animeData %>% select(-c(title_synonyms,synopsis,background,related,genre,studio,producers))
# View(animeData)
print("Removing duplicates (they exist, because anime categorized by genres, and it can have multiple)")
uniqueAnime <- animeData %>% distinct() %>% filter(!is.na(title_english))
# View(uniqueAnime)
print("Let's select only shows with type 'TV'")
tvAnime <- uniqueAnime%>%filter(type=="TV")
# View(tvAnime)

print("Data visualisation:")
print("Most popular shows")
tvPlot <-tvAnime%>%
  mutate(graph_name=paste0(title_english," (",premiered,")"))%>%
  top_n(-20,wt=popularity) %>%  
  ggplot(aes(reorder(graph_name,desc(popularity)),popularity,colour=title_english))+
  geom_point(show.legend = FALSE)+
  geom_segment(aes(x=graph_name,xend=graph_name,y=0,yend=popularity),show.legend = FALSE)+
  coord_flip()+
  theme_classic()+
  labs(x="",y="Popularity",title = "Top 20 Most Popular Anime TV Shows")

print("Most popular shows which are currently airing")
tvaAnimeAiring <- uniqueAnime%>%filter(type=="TV",airing=="TRUE")
tvPlotAiring <-tvaAnimeAiring%>%
  mutate(graph_name=paste0(title_english," (",premiered,")"))%>%
  top_n(-20,wt=popularity) %>% 
  ggplot(aes(reorder(graph_name,desc(popularity)),popularity,colour=title_english))+
  geom_point(show.legend = FALSE)+
  geom_segment(aes(x=graph_name,xend=graph_name,y=0,yend=popularity),show.legend = FALSE)+
  coord_flip()+
  theme_classic()+
  labs(x="",y="Popularity",title = "Top 20 Most Popular Anime TV Shows", subtitle = "Currently airing")+
  theme(axis.text.y.left = element_text(size = 12))
print("It seems, dataset is pretty old, because the latest anime is for 2019")

print("Correlation between the number of views and highest score of popular shows")

tvCorrelation <- tvAnime %>% 
  filter(popularity <= 50) %>%
  mutate(title_english = str_replace(title_english, "Code Geass: Lelouch of the Rebellion", "Code Geass")) %>%
  ggplot(aes(score, scored_by)) + 
  geom_point(shape=21,aes(fill=title_english,size=members)) + 
  geom_text(aes(label = title_english ), check_overlap = T, show.legend = F, size = 3, hjust = 1) + 
  xlim(c(6, 10)) +
  scale_y_log10()+
  labs(title = "Which popular anime also score high?", 
       subtitle = "Top 50 anime shown based on popularity",
       y = "Number of users that scored",
       x = "Score (1-10)") +
  theme_classic()+
  theme(legend.position = 'none',aspect.ratio = 0.5)

print("Comparison of twenty most favorite vs most members vs most scored")

mostMembers <- tvAnime%>%
  top_n(20,wt=members) %>%
  mutate(graph_name=paste0(title_english," (",premiered,")"),graph_name=fct_reorder(graph_name,members))%>%
  ggplot(aes(graph_name,members,fill=graph_name))+
  geom_bar(stat = 'identity',width=0.5,show.legend = FALSE,color='black')+
  coord_flip()+
  theme_classic()+
  scale_y_continuous(limits = c(0,1700000),labels = comma)+
  geom_text(aes(label=comma(members)),size=3)+
  labs(x="",y="Rank",title = "Top 20 Most Members")
mostFavorite<-
  tvAnime%>%
  top_n(20,wt=favorites) %>% 
  mutate(title_english=fct_reorder(title_english,favorites))%>%
  ggplot(aes(title_english,favorites,fill=title_english))+
  geom_bar(stat = 'identity',width=0.5,show.legend = FALSE,color='black')+
  coord_flip()+
  theme_classic()+
  scale_y_continuous(limits = c(0,150000),labels = comma)+
  geom_text(aes(label=comma(favorites)),size=3,hjust=1,fontface='bold')+
  labs(x="",y="Favourites",title = "Top 20 Most Favorite")
mostScored<-
  tvAnime%>%
  top_n(20,wt=scored_by) %>% 
  mutate(title_english=fct_reorder(title_english,scored_by))%>%
  ggplot(aes(title_english,scored_by,fill= title_english))+
  geom_bar(stat = 'identity',width=0.5,color='black',show.legend = FALSE)+
  coord_flip()+
  theme_classic()+
  scale_y_continuous(limits = c(0,1500000),labels = comma)+
  geom_text(aes(label=comma(scored_by)),size=3,hjust=1,fontface='bold')+
  labs(x="",y="Favourites",title = "Top 20 Most Scored by")

compariosn <- mostFavorite + mostMembers+ mostScored + plot_layout(widths = 20)