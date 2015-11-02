library("dplyr")
library("stringr")
library("rvest")
library("plyr")

## Definerer base-hjemmesiden ##
link = "http://www.ipaidabribe.com/reports/paid"

## Genererer en prøveliste af pages (skal være 100) ##
gsider <- vector("list", 2) 
for(i in 1:2){
  gsider[[i]] = print(paste("http://www.ipaidabribe.com/reports/paid?page=",i, sep=""))
}

## Definerer en scraping-funktion ##
scrape.1 = function(link) {
  my.link = read_html(link, encoding = "UTF-8")
  my.link.text = my.link %>%                          ## Henter hovedtekst          ##
    html_nodes(".body-copy-lg") %>% html_text() %>%
    paste(collapse = "")                              ## Samler teksten på én linje ##
  my.link.titel = my.link %>%                         ## Henter overskrift          ##
    html_nodes(".heading-3 a") %>% html_text()
  link.betaling = my.link %>%                         ## Henter bestikkelsessum     ##
    html_nodes(".paid-amount span") %>% 
    html_text()
  return(cbind(my.link.titel, my.link.text, link.betaling))
}


## Scraper hvert link ##
my.data.1 = list() # initialize empty list
for (i in gsider[1:2]){                               ## Loop over den genererede liste ##
  print(paste("processing", i, sep = " "))            ## Vis mig løbende processen  ##
  my.data.1[[i]] = scrape.1(i)
  # waiting 10 seconds between hits
  Sys.sleep(10)
  cat(" done!\n")
} 

## Laver den om til et data frame ##
df=ldply(my.data.1)                                   ## Transformerer listen til data frame ##

### FØLGENDE VIRKER IKKE. Samme type funktion som den første, den virker bare ikke ###
scrape.2 = function(link) {
  my.link = read_html(link, encoding = "UTF-8")
link.kategori = my.link %>%                       #
    html_nodes(".details .name a") %>% 
    html_text() 
link.views = my.link %>% 
    html_nodes(".overview .views") %>% 
    html_text() %>% 
link.city = my.link %>% 
    html_nodes(".location") %>% 
    html_text() %>% 
link.reportnr = my.link %>% 
    html_nodes(".unique-reference") %>% 
    html_text() %>% 
  return(cbind(link.kategori, link.views, link.city, link.reportnr))
}

my.data.2 = list() # initialize empty list
for (i in gsider[1:2]){
  print(paste("processing", i, sep = " "))
  my.data.2[[i]] = scrape.2(i)
  # waiting 10 seconds between hits
  Sys.sleep(10)
  cat(" done!\n")
}


df=ldply(my.data.2)




