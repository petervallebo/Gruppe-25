## MED SIDER 10, 20, 30. 

library("stringr")
library("rvest")
library("plyr")
library("dplyr")

##### STEP 1:                                                       #####
##### 1) Laver en liste af generiske sider (én for hvert sideskift) #####
##### 2) Scraper links for hvert enkelt post                        #####

link = "http://www.ipaidabribe.com/reports/paid"      ## Definerer base-hjemmesiden ##

loop <- list()                                        ## Genererer liste af pages (skal være 100)
for(i in seq(from = 10, to = 1050, by = 10)){           ## Looper. Tager kun hver 10'ende, som ipaidbribe.com
  loop[[i/10]] = print(paste("http://www.ipaidabribe.com/reports/paid?page=",i-10, sep="")) ## Skal starte på 0, som er side 1
}

g.sider = ldply(loop)                                 ## Fra vector til data frame
names(g.sider) = c("links")                           ## Navngiver variablen

## SCRAPE-FUNKTION: Scraper hvert link, på en given side, der har "read-more"-markøren ##
link.scraper = function(link) {
  my.link = read_html(link, encoding = "UTF-8")
  my.link.text = my.link %>%                          ## Definér, then 
    html_nodes("a.read-more") %>%                     ## Registrer hvert "Read more", then
    html_attr('href')                                 ## Giv egenskab som link og træk link
    return(cbind(my.link.text))                       ## Returnér det og tving det(/dem) til søjlebinding
}

## Opretter en liste og bruger funktionen over et loop af generiske bribe-sider. Funktionens udtræk gemmes i listen ##

start = Sys.time()                                    ## Køres samtidigt med nedenfor. Dokumentation af tidspunktet på hvornår de 1000 seneste findes
links.posts = list() # initialize empty list
for (i in g.sider$links[1:nrow(g.sider)]){            ## Loop over den genererede liste ##
  print(paste("processing", i, sep = " "))            ## Vis mig løbende processen  ##
  links.posts[[i]] = link.scraper(i)
  # waiting 10 seconds between hits - jf. deres robots.txt
  #Sys.sleep(10) -> Giver dubletter hvis den er her. Derfor bruges kun til scrape for data
  cat(" done!\n")
} 

dflinks=ldply(links.posts)                           ## Laver den om til et data frame

## Gemmer liste med links og tidspunkt

save(dflinks, file="dflinks.RData")
save(start,file="start.RData")

#### DONE ####

###### STEP 2: Scrape dataelementer ##################################################

data.scraper = function(dflinks) {
  my.link = read_html(dflinks, encoding = "UTF-8")
  kategori = my.link %>%                            ## Henter post'ets kategori
    html_nodes(".details .name a") %>% 
    html_text()
  kategori2 = my.link %>%                           ## Henter post'ets underkategori
    html_nodes("div.report-listing.details li.transaction a") %>% 
    html_text()
  location = my.link %>%                            ## Henter byområde
    html_nodes(".location") %>% 
    html_text()
  my.body = my.link %>%                             ## Henter hovedtekst          ##
    html_nodes(".body-copy-lg") %>% 
    html_text() %>%
    paste(collapse = "")
  my.titel = my.link %>%                            ## Henter overskrift          ##
    html_nodes(".heading-3 a") %>% 
    html_text()
  my.date = my.link %>%                             ## Henter dato                ##
    html_nodes(".date") %>%
    html_text()
  postnr = my.link %>%                              ## Henter nummer på post'et
    html_nodes(".unique-reference") %>% 
    html_text()
  views = my.link %>%                               ## Henter antal views
    html_nodes(".overview .views") %>% 
    html_text()
  betaling = my.link %>%                            ## Henter bestikkelsessum     ##
    html_nodes("div.report-listing.details li.paid-amount span") %>% 
    html_text()
  return(cbind(kategori, kategori2, location, my.body, my.titel, my.date, postnr, views, betaling))
}

data.liste = list()
for (i in dflinks$my.link.text[1:nrow(dflinks)]){
  print(paste("processing", i, sep = " "))
  data.liste[[i]] = data.scraper(i)
  # waiting 10 seconds between hits
  #Sys.sleep(10)
  cat(" done!\n")
}
data.frame=ldply(data.liste)

save(data.frame,file="data.frama.RData")

###### STEP 3: Bearbejd og gem data ##################################################

# 1 Omdanner variable

data.frame.endelig = data.frame %>%
                     mutate(
                       betaling=as.numeric(str_replace_all(betaling,"[^0-9]","")), # Fjerner alle ikke numeriske tegn og omdanner til tal
                       views=as.numeric(str_replace_all(views,"[^0-9]","")),
                       my.date=as.Date(my.date,"%B %d, %Y"), # Om danner til datovariabel ved at oversætter opbygningen den angivede dato - se: https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html
                       start_scrape=start # Start tidspunkt for scrape til dokumentation
                       ) %>%
                     filter(.id != "http://www.ipaidabribe.com/" ) # Fjerner obs der ikke er endelige

# 2 Gemmer data

save(data.frame.endelig,file = "data.frame.endelig.RData")

## Test for duplicates - I alt er der 10 dubletter, hvorfor det endelige datasæt har 990 obs.

dubletter =  dflinks %>%
        group_by(my.link.text) %>%
        filter(n() != 1)

dubletter2 =  data.frame.endelig %>%
  group_by(.id) %>%
  filter(n() != 1)



