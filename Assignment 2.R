## MED SIDER 10, 20, 30. 

library("stringr")
library("rvest")
library("plyr")
library("dplyr")

##### STEP 1:                                                       #####
##### 1) Laver en liste af generiske sider (én for hvert sideskift) #####
##### 2) Scraper links for hvert enkelt post                        #####

link = "http://www.ipaidabribe.com/reports/paid"      ## Definerer base-hjemmesiden ##

loop <- vector("list", 3)                             ## Genererer liste af pages (skal være 100)
for(i in seq(from = 10, to = 30, by = 10)){           ## Looper. Tager kun hver 10'ende, som ipaidbribe.com
  loop[[i]] = print(paste("http://www.ipaidabribe.com/reports/paid?page=",i, sep=""))
}

g.sider = ldply(loop)                                 ## Fra vector til data frame
names(g.sider) = c("links")                           ## Navngiver variablen

## SCRAPE-FUNKTION: Scraper hvert link, på en given side, der har "read-more"-markøren ##
link.scraper = function(link) {
  my.link = read_html(link, encoding = "UTF-8")
  my.link.text = my.link %>%                          ## Definér, then 
    html_nodes(".read-more") %>%                      ## Registrer hvert "Read more", then
    html_attr('href')                                 ## Giv egenskab som link og træk link
    return(cbind(my.link.text))                       ## Returnér det og tving det(/dem) til søjlebinding
}

## Opretter en liste og bruger funktionen over et loop af generiske bribe-sider. Funktionens udtræk gemmes i listen ##
links.posts = list() # initialize empty list
for (i in g.sider$links[1:3]){                        ## Loop over den genererede liste ##
  print(paste("processing", i, sep = " "))            ## Vis mig løbende processen  ##
  links.posts[[i]] = link.scraper(i)
  # waiting 10 seconds between hits
  Sys.sleep(10)
  cat(" done!\n")
} 

dflinks=ldply(links.posts)                           ## Laver den om til et data frame

#### DONE ####

###### STEP 2: Scrape dataelementer ##################################################
data.scraper = function(dflinks) {
  my.link = read_html(dflinks, encoding = "UTF-8")
  kategori = my.link %>%                            ## Henter post'ets kategori
    html_nodes(".details .name a") %>% 
    html_text()
  location = my.link %>%                            ## Henter byområde
    html_nodes(".location") %>% 
    html_text()
  my.body = my.link %>%                             ## Henter hovedtekst          ##
    html_nodes(".body-copy-lg") %>% html_text() %>%
    paste(collapse = "")
  my.titel = my.link %>%                            ## Henter overskrift          ##
    html_nodes(".heading-3 a") %>% html_text()
  my.date = my.link %>%                             ## Henter dato                ##
    html_nodes(".date") %>% html_text()
  postnr = my.link %>%                              ## Henter nummer på post'et
    html_nodes(".unique-reference") %>% 
    html_text()
  views = my.link %>%                               ## Henter antal views
    html_nodes(".overview .views") %>% 
    html_text()
  return(cbind(kategori, location, my.body, my.titel, my.date, postnr, views))
}

data.liste = list()
for (i in dflinks$my.link.text[1:20]){
  print(paste("processing", i, sep = " "))
  data.liste[[i]] = data.scraper(i)
  # waiting 10 seconds between hits
  Sys.sleep(10)
  cat(" done!\n")
}
data.frame=ldply(data.liste)

########################################################### Betaling
betaling.scraper= function(dflinks) {
  my.link = read_html(dflinks, encoding = "UTF-8")
  link.betaling = my.link %>%                         ## Henter bestikkelsessum     ##
    html_nodes(".paid-amount span") %>% 
    html_text()
return(cbind(link.betaling))
}

betaling.liste = list()
for (i in g.sider$links[1:2]){
  print(paste("processing", i, sep = " "))
  betaling.liste[[i]] = betaling.scraper(i)
  # waiting 10 seconds between hits
  Sys.sleep(10)
  cat(" done!\n")
}

df.betaling=ldply(betaling.liste)
df.betaling = df.betaling[-1,]
df.betaling1 = df.betaling
df.betaling1$.id = NULL
newrow = ldply(c(1:1))
names(newrow) = c("link.betaling")
df.betaling1 = rbind(df.betaling1,newrow)
data.frame1 = cbind(data.frame, df.betaling1)
