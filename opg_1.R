#
# Opgave 1 – Webscrape
# Bilerne som I henter i denne opgave skal bruges i opgave 2 når de skal gemmes i en database.
#
# Opgave 1.1 – Hente data fra Bilbasen
# I skal hente udvalgte biler fra bilbasen.dk. Hent så mange som muligt i en ”lukket” serie.
# I kan f.eks. vælge VW, model Up, eldrevne.
# Når I henter data, skal I ud over de oplagte data også huske at gemme linket så man kan hente flere data på hver bil.
# I skal også hente forhandleren – både hans id samt firmanavn, adresse samt cvr-nummer. Det kan involvere manuelt arbejde.

# Trin 0 - Hent relevante pakker:
library(httr)
library(rvest)
library(dplyr)
library(stringr)

# Trin 1 - Fortæl hvilket startlink den skal hente info fra:
startlink <- "https://www.bilbasen.dk/brugt/bil/mercedes?includeengroscvr=true&includeleasing=false&pagesize=100"

# Trin 2 - Hent siden ned og tjek statuskoden:
rawres <- GET(
  url = startlink,
  add_headers(
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/121.0.0.0 Safari/537.36"))

rawres$status_code  # tjek at koden er 200 = OK

# Trin 2.1 Gem cookies i et objekt:
  mycookies <- cookies(rawres)

# Trin 2.2 Nyt kald hvor cookies sendes med
rawres <- GET(
  url = startlink,
  add_headers(
    "User-Agent" = "Mozilla/5.0"
  ),
  set_cookies(bbtracker = "id=3b1662ac-e8e8-4895-a8ed-310e8cf6faf4")
)

# Trin 3 - Konverter HTML-indholdet til tekst og lav tekstindholdet om til HTML, som kan scrapes:
rawcontent <- httr::content(rawres, as = "text")   # Pakker HTML’en ud af HTTP-svaret og gør den til en ren tekststreng

page <- read_html(rawcontent)     # Laver tekststrengen om til en HTML-struktur, der kan scrapes

# Trin 4 - Hent alle bilopslagene på startlinket:
carlist <- page %>% html_elements("article")   # Find alle 'article'-elementer på siden

# Trin 5 - Lav alle de tags/informationer vi gerne vil have på hver bil, så de er lettere at bruge fremover:
model_t <- "[class^='Listing_makeModel']"
price_t <- "[class^='Listing_price']"
det_t <- "[class^='ListingDetails_listItem']"
loc_t <- "[class^='Listing_location']"
description_t <- "[class^='Listing_descriptionription']"
prop_t <- "[class^='Listing_properties']"
link_t <- "[class^='Listing_link']"


# Trin 6 - Test det af ved at hente alle dataene ned for første bil, med et for-loop:
# Modelnavn
car_test <- carlist[[1]]   # vælger den første bil i listen

model_test <- car_test %>%   # Gemmer i en vektor
  html_element(model_t) %>%  # Finder modelnavn-elementet.
  html_text2()     # Udtrækker selve teksten.

model_test

# Bilens pris
car_test <- carlist[[1]]

price_test <- car_test %>%
  html_element(price_t) %>%
  html_text2()

price_test

# Årgang (details)
car_test <- carlist[[1]]

year_test <- car_test %>%
  html_element(det_t) %>%
  html_text2()

year_test

# Location
car_test <- carlist[[1]]

loc_test <- car_test %>%
  html_element(loc_t) %>%
  html_text2()

loc_test

# descriptionription
car_test <- carlist[[1]]

description_test <- car_test %>%
  html_element(description_t) %>%
  html_text2()

description_test

# Antal km (details)
car_test <- carlist[[1]]

km_test <- car_test %>%
  html_elements(det_t) %>%
  html_text2()

# Udvælg kun kilometer-linjen
km_test <- km_test[grepl("km$", km_test)]   # Hent kun det der slutter på 'km'
km_test

# Link
car_test <- carlist[[1]]

link_test <- car_test %>%
  html_element(link_t) %>%
  html_attr("href")        # Henter selve linkadressen fra elementets href-attribut.

link_test

# Carid
carid <- link_test %>%
  str_extract("[0-9]{7}")

carid

# Trin 7 - Hent alle 100 biler ned med et for-loop:
# Trin 7.1 - Lav et tomt dataframe til alle bilerne:
car_df <- data.frame(
  titel = character(),
  pris = character(),
  aar = character(),
  km = character(),
  loc = character(),
  description = character(),
  link = character(),
  carid = character(),
  scrapedate = as.Date(character() ) )

# Trin 7.2 - Hent dagsdato (scrapedate):
scrapedate <- Sys.Date()

# Trin 7.3 - Lav et loop der hente alle oplysningerne på bilerne:
for(i in seq_along(carlist)) {
  
  car <- carlist[[i]]   # ← hent én bilannonce
  
  # --- MODEL NAVN ---
  model <- car %>% 
    html_element(model_t) %>% 
    html_text2()
  
  # --- PRIS ---
  price <- car %>%
    html_element(price_t) %>%
    html_text2()
  
  # --- DETALJER: ÅR OG KM---
  details <- car %>%
    html_elements(det_t) %>%
    html_text2()
  
  # Udtræk år (første tal i første detalje-linje)
  year <- details[1]
  
  # Udtræk km-linjen (slutter på km)
  km <- details[grepl("km$", details)]
  
  # --- LOKATION ---
  loc <- car %>%
    html_element(loc_t) %>%
    html_text2()
  
  # --- BESKRIVELSE ---
  description <- car %>%
    html_element(description_t) %>%
    html_text2()
  
  # --- LINK TIL BILEN ---
  link <- car %>%
    html_element(link_t) %>%
    html_attr("href")
  
  # --- BIL-ID (sidste 7 tal i linket) ---
  carid <- link %>%
    str_extract("[0-9]{7}")
  
  # --- TILFØJ DET TIL EN RÆKKE I DATAFRAMEN ---
  car_df <- rbind(
    car_df,
    data.frame(
      model = model,
      price = price,
      year = year,
      km = km,
      loc = loc,
      description = description,
      link = link,
      carid = carid,
      scrapedate = scrapedate) )
  }

# Trin 8 - Hent oplysninger på forhandlerne (id, firmanavn, adresse og cvr-nr)
# Da vi ikke kan finde forhandlerens id før vi klikker ind på forhandlerens link,
# starter vi med at finde navn, adresse og cvr.

# Trin 8.1 - Start med at lave en vektor med alle de 100 annoncelinks:
linklist <- car_df$link     # Tom vektor

# Trin 8.2 - Lav tags for hver information der skal hentes:
name_t <- "[class^='bas-MuiTypography-root bas-MuiTypography-h3']"
adr_t <- "[class^='bas-MuiSellerInfoComponent-sellerLink bas-MuiSellerInfoComponent-addressWrapper']"
cvr_t <- "[class^='bas-MuiSellerInfoComponent-cvr']"
deal_link_t <- "[class^='bas-MuiTypography-root bas-MuiLink-root bas-MuiLink-underlineHover bas-MuiTypography-colorPrimary']"

# Trin 8.3 - Start small, og test det af på én bil først:
# Lav en vektor med det første link.
testlink <- linklist[1]

# Hent indholdet på siden ned og tjek statuskode:
testbil <- GET(url = testlink ,
               add_headers("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.6 Safari/605.1.15"))
testbil$status_code  # tjek at den er 200 = OK

# Konverter HTML-indholdet til tekst og lav tekstindholdet om til HTML, som kan scrapes:
testbil_con <- httr::content(testbil, as = "text")

test_page <- read_html(testbil_con)

# Firmanavn
name_test <- test_page %>%
  html_element(name_t) %>%
  html_text2()

name_test

# Adresse
adr_test <- test_page %>%
  html_element(adr_t) %>%
  html_text2()

adr_test

# CVR-nummer
cvr_text <- test_page %>%
  html_element(cvr_t) %>%
  html_text2()

cvr_text

# Forhandler id
link <- test_page %>%
  html_element(deal_link_t) %>%
  html_attr("href")

link

dealer_id <- sub(".*id", "", link)    # tag id og alt før id, og erstat det med ingenting (fjern det)

dealer_id

# Carid - så vi kan merge de to datasæt på carid-kolonnen senere
carid <- car_df$carid[1]

carid

# Trin 9 - Lav et loop, der henter alle forhandlernes informationer ned ud fra alle 100 links.
# Trin 9.1 - Lav et tomt dataframe til alle informationerne:
dealer_df <- data.frame(
  carid = character(),
  dealer_name = character(),
  dealer_address = character(),
  dealer_cvr = character(),
  dealer_id = character() ) 

# Trin 9.2 - Loop igennem alle annonce-links i linklist og hent forhandler data:
for (i in seq_along(linklist)) {
  
  link <- linklist[i]        # bilens annonce-link
  carid <- car_df$carid[i]   # bilens ID (samme rækkefølge som linksene)
  
  # Hent bilens-side
  res <- GET(
    url = link,
    add_headers("User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.6 Safari/605.1.15"))
  
  # Sæt den til at holde pauser undervejs i loopet
  Sys.sleep(runif(1, 1, 4))
  
  page_txt <- httr::content(res, as = "text")
  page <- read_html(page_txt)
  
  # Forhandlernavn
  dealer_name <- page %>%
    html_element(name_t) %>%
    html_text2()
  
  # Adresse
  dealer_address <- page %>%
    html_element(adr_t) %>%
    html_text2()
  
  # CVR-tekst
  cvr_text <- page %>%
    html_element(cvr_t) %>%
    html_text2()
  # Træk 8 cifre ud af tekststrengen
  dealer_cvr <- str_extract(cvr_text, "[0-9]{8}")
  
  # Link til forhandlerens side
  link <- page %>%
    html_element(deal_link_t) %>%
    html_attr("href")
  
  # Forhandler id
  dealer_id <- sub(".*id", "", link)    # tag id og alt før id, og erstat det med ingenting (fjern det)
  
  dealer_df <- rbind(
    dealer_df,
    data.frame(
      carid = carid,
      dealer_name = dealer_name,
      dealer_address = dealer_address,
      dealer_cvr = dealer_cvr,
      dealer_id = dealer_id))
  }

# Trin 10 - Merge de to dataframs (car_df og dealer_df):
bilbasen <- left_join(car_df, dealer_df, by = "carid")

#
# Opgave 1.2 – Rense data
#
# Salgsteksten fra bilen indeholder en masse overflødige tegn.
# Sørg for at få renset teksten så der kun er almindelige karakterer og tegn (punktum, komma) tilbage.
# Sørg også for at ”newline” erstattes af ”. ” og at mange mellemrum erstattes med ét mellemrum.
#

# Trin 1 - Lav et nyt identisk dataframe, så vi ikke kommer til at ødelægge noget i det originale:
bilbasen_df <- bilbasen

# Trin 2 - Erstat newline (afsnit) med '. ':
bilbasen_df$description <- gsub("\n", ". ", bilbasen_df$description)

# Trin 3 - Fjern alle “mærkelige” tegn (alt undtagen almindelige danske bogstaver, tal og standard-tegn):
bilbasen_df$description <- gsub(
  "[^A-Za-z0-9æøåÆØÅ.,:; ]", "", bilbasen_df$description)

# Trin 4 - Lav flere mellemrum om til ét mellemrum:
bilbasen_df$description <- gsub(" +", " ", bilbasen_df$description)

# Trin 5 - Indsæt scrapedate som den sidste kolonne i datasætter:
bilbasen_df <- bilbasen_df %>%
  select(-scrapedate, scrapedate)

# Trin 6 - Gem denne dataframe som en .rds-fil:
saveRDS(bilbasen_df, "bilbasen_df.rds")

#
# Opgave 1.3 – Hente nye data - simuleret
#
# I skal nu lave en simuleret hentning af biler, hvor I skal tage udgangspunkt i de biler I hentede i 1.1.
# Den simulerede kørsel skal have en scrapedate som ligger én dag senere end 1.1
# og den skal have 2 rækker med nye biler, 3 rækker med ændrede priser
# og så skal der mangle 5 rækker fra den oprindelige testkørsel så I kan simulere, at bilerne er blevet solgt.
#

# Trin 1 – Lav en kopi til den simulerede kørsel, så vi ikke ødelægger originalen:
bilbasen_df2 <- bilbasen_df

# Trin 2 - Lav scrapedate til at være en dag senere end det andet dataframe:
bilbasen_df2$scrapedate <- bilbasen_df2$scrapedate + 1

# Trin 3 - Fjern 5 tilfældige biler, der simulerer de er “solgt”.
# Trin 3.1 - Først laver vi vektor der indeholder 5 tilfældige tal, som er de rækker der skal fjernes:
sold <- sample(1:nrow(bilbasen_df2), 5)

# Trin 3.2 - Fjern de 5 tilfældige rækker:
bilbasen_df2 <- bilbasen_df2[-sold, ]

# Trin 4 - Tilføj 2 nye rækker, der simulerer to nye biler:
# Trin 4.1 - Først kopieres to rækker fra originalen så strukturen er det samme:
new1 <- bilbasen_df2[1, ]
new2 <- bilbasen_df2[2, ]

# Trin 4.2 - Så ændres alle oplysninger til tilfældige informationer:

# model
new1$model <- "(Sim) Mercedes ABC GT 4,0 Road"
new2$model <- "(Sim) Mercedes QLM GT 5,0 Ride"

# Price
new1$price <- "649.000 kr."
new2$price <- "395.000 kr."

# Year
new2$year <- new1$year
new1$year <- new2$year

# km
new1$km <- "15.000 km"
new2$km <- "25.000 km"

# Location
new1$loc <- bilbasen_df2[5,"loc"]
new2$loc <- bilbasen_df2[12,"loc"]

# descriptionription
new1$description <- "Lækker Mercedes. Klar til afhentning."
new2$description <- "Kom og køb. Lækker Mercedes. Klar til afhentning."

# Link
new1$link <- "https://www.bilbasen.dk/nybil1"
new2$link <- "https://www.bilbasen.dk/nybil2"

# Carid
new1$carid <- "1234567"
new2$carid <- "7654321"

# Dealer name
new1$dealer_name <- bilbasen_df2[5,"dealer_name"]
new2$dealer_name <- bilbasen_df2[12,"dealer_name"]

# Dealer address
new1$dealer_address <- bilbasen_df2[5,"dealer_address"]
new2$dealer_address <- bilbasen_df2[12,"dealer_address"]

# Dealer cvr
new1$dealer_cvr <- bilbasen_df2[5,"dealer_cvr"]
new2$dealer_cvr <- bilbasen_df2[12,"dealer_cvr"]

# Dealer id
new1$dealer_id <- bilbasen_df2[5,"dealer_id"]
new2$dealer_id <- bilbasen_df2[12,"dealer_id"]

# Trin 4.3 - Kombinér nu de nye rækker i datasættet (bilbasen_df2):
bilbasen_df2 <- rbind(bilbasen_df2, new1, new2)

# Trin 5 - Lav prisændringer på 3 tilfældige rækker.
# Trin 5.1 - Ændre price-kolonnen til numerisk og fjern 'kr.':
bilbasen_df2$price <- gsub("[^0-9]", "", bilbasen_df2$price)
bilbasen_df2$price <- as.numeric(bilbasen_df2$price)

# Trin 5.2 - Lav en vektor der indeholder 3 tilfældige tal, som er de rækker der skal ændres pris på:
price_change <- sample(1:nrow(bilbasen_df2), 3)

# Trin 5.3 - Sænk prisen på de 3 tilfældige rækker:
bilbasen_df2$price[price_change] <- bilbasen_df2$price[price_change] * 0.95

# Trin 6 - Renser begge dataframe så kolonnerne med tal kun indeholder tal:
bilbasen_df$price <- gsub("[^0-9]", "", bilbasen_df$price)
bilbasen_df$price <- as.numeric(bilbasen_df$price)

bilbasen_df$km <- gsub("[^0-9]", "", bilbasen_df$km)
bilbasen_df$km <- as.numeric(bilbasen_df$km)

bilbasen_df2$km <- gsub("[^0-9]", "", bilbasen_df2$km)
bilbasen_df2$km <- as.numeric(bilbasen_df2$km)

# Lav year om til en dato:
library(lubridate)

bilbasen_df$year <- dmy(paste0("01/", bilbasen_df$year))
bilbasen_df2$year <- dmy(paste0("01/", bilbasen_df2$year))

# Trin 7 - Gemmer begge tabeller som .rds-filer der senere hen kan bruges:
saveRDS(bilbasen_df2, "bilbasen_df2.rds")
saveRDS(bilbasen_df, "bilbasen_df.rds")




