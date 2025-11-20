#
# Opgave 2.1 – Oprette skemaet for bilbasen
#
# For at kunne gemme bilerne i databasen er det vigtigt at oprette et skema,
# hvor man har invariante entiteter organiseret i tabeller med fremmed-nøgler så de kan linkes sammen,
# samt variende data i tidsserieagtige tabeller, hvor man linker til den sidst-forekommende observation.
# Jeres design skal beskrives i et ER-diagram og jeres DDL-statements skal ligge på github med filextension .sql.

## MySQL Workbench ##

# Trin 1 - Først laves en database hvor alle tabellerne kan indsætter:
#CREATE DATABASE bilbasen;

# Trin 2 - Fortæl at vi vil bruge bilbasen-databasen:
#USE bilbasen;

# Trin 3 - Lav en tabel for forhandler (invariant):
#CREATE TABLE dealer (
#  dealer_id   VARCHAR(20) PRIMARY KEY,
#  dealer_name        VARCHAR(255),
#  dealer_address     VARCHAR(255),
#  dealer_cvr         VARCHAR(20) );

# dealer_id = kolonnenavn
# VARCHAR(20) = tekstfelt, der kan være op til 20 tegn langt
# PRIMARY KEY = den unikke nøgle i tabellen
# Hvad er en PRIMARY KEY?
# •	En kolonne, der identificerer hver række unikt
# •	Ingen to rækker må have samme værdi
# •	Den må ikke være NULL
# VARCHAR(255) = tekstfelt, der kan være op til 255 tegn langt
# CVR skal være tekst og ikke integer fordi:
# •	CVR kan starte med 0 (fx “01234567”)
# •	Hvis du brugte tal/INT, ville 0 i starten forsvinde
# •	Tekst (VARCHAR) bevarer præcis det, du skriver

# Trin 4 - Lav en tabel for bilerne (grundtdata, invariant):
#CREATE TABLE car (
#  carid       VARCHAR(20) PRIMARY KEY,
#  model       VARCHAR(100),
#  year        DATE,
#  loc    		VARCHAR(255),
#  link        VARCHAR(255),
#  dealer_id   VARCHAR(20),
#  FOREIGN KEY (dealer_id)
#  REFERENCES dealer(dealer_id) );

# Fremmednøglen (FK) skaber relationen mellem to tabeller.
# CONSTRAINT fk_car_dealer = giver fremmednøglen et navn.
#  FOREIGN KEY (dealer_id) = Dvs. den peger på en række i en anden tabel. “kolonnen dealer_id i car-tabellen er en fremmed nøgle”.
# REFERENCES dealer(dealer_id) = “dealer_id i car refererer til dealer_id i dealer-tabellen”.

# Trin 5 - Lav en tabel med alle detaljerne/observationer for hver bil (varierende data):
#CREATE TABLE car_observation (
#  observation_id INT AUTO_INCREMENT PRIMARY KEY,
#  carid          VARCHAR(20),
#  scrapedate     DATE,
#  price          INT,
#  km             INT,
#  description    TEXT,
#  sold           BOOLEAN DEFAULT FALSE,
#  FOREIGN KEY (carid) REFERENCES car(carid) );

# AUTO_INCREMENT betyder at den selv laver en tal/id til hver række der er. Så de alle for hver sit unikke id.
# BOOLEAN er det samme som TINYINT(1). Det betyder, at kolonnen er en sand/falsk-værdi.
# DEFAULT FALSE betyder at hvis ikke jeg fortlæller hvad der skal stå i denne kolonne, så får den værdien FALSE.

#
# Opgave 2.2 – Gemme bilerne i database
#
# I skal nu gemme jeres første scrape-resultat i jeres database.
# I kan lade R-driveren gøre arbejdet med at oprette skemaet men I skal sørge for at der er plads til det,
# som R-driveren sætter på data-typerne. I skal desuden definere carid som primær nøgle.
# Jeres INSERT-statements skal også ligge på github.

# Trin 1 - Indlæs relevante pakker:
library(DBI)
library(RMariaDB)

# Trin 2 - Lav en forbindelse mellem R og MySQL:
con <- dbConnect(
  MariaDB(),
  user = "root",
  password = "din_kode",
  dbname = "bilbasen",
  host = "localhost",
  port = 3306)

# Test om forbindelsen er okay:
dbListTables(con)

# Trin 3 - Indsæt bilbasen_df-tabellen fra R til MySQL:
dbWriteTable(con, "scrape1", bilbasen_df, overwrite = TRUE, row.names = FALSE)

#
# Opgave 2.3 – Opdatere databasen ud fra den simulerede kørsel
#
# Hvis I har fået lavet et korrekt skema – altså at I kan versionere prisen vha en pris-tabel
# – burde I kunne opdatere databasen med jeres simulerede nye scraping,
# hvor I altså opretter/ændrer en record med pris, dato og carid så man joine de to tabeller på carid.
# Det gøres bedst ved at I laver et script eller en funktion som henter den forrige kørsel fra databasen
# (altså jeres data fra opgave 2.2) og sammenligner med den simulerede kørsel (altså kørsel-II).
# I skal finde:
# a) Nye records
# b) Ændrede records (på prisen)
# c) Missing records (solgte biler)
# Og opdatere databasen efterfølgende. De solgte biler skal ikke slette men markeres som TRUE i sold.
#

# Trin 1 - Hent alle de nye biler, solgte biler og prisændringer og sæt det ind i dataframes i R. Opdater databasen i MySQL:

library(DBI)
library(RMariaDB)
library(dplyr)

# 0) Forbind til databasen ------------------------------------------
con <- dbConnect(
  MariaDB(),
  user = "root",
  password = "Din_kode",
  dbname = "bilbasen",
  host = "localhost",
  port = 3306
)


# 1) NYE FORHANDLERE (dealer) ---------------------------------------
new_dealers <- anti_join(bilbasen_df2, bilbasen_df, by = "dealer_id") %>%
  distinct(dealer_id, dealer_name, dealer_address, dealer_cvr)

if (nrow(new_dealers) > 0) {
  dbWriteTable(
    con,
    name      = "dealer",
    value     = new_dealers,
    append    = TRUE,
    row.names = FALSE
  )
}


# 2) NYE BILER (car) -------------------------------------------------
new_cars <- anti_join(bilbasen_df2, bilbasen_df, by = "carid") %>%
  distinct(carid, model, year, loc, link, dealer_id)

if (nrow(new_cars) > 0) {
  dbWriteTable(
    con,
    name      = "car",
    value     = new_cars,
    append    = TRUE,
    row.names = FALSE
  )
}


# 3) OBSERVATIONER for nye biler ------------------------------------
new_car_obs <- anti_join(bilbasen_df2, bilbasen_df, by = "carid") %>%
  select(carid, scrapedate, price, km, description)

if (nrow(new_car_obs) > 0) {
  dbWriteTable(
    con,
    name      = "car_observation",
    value     = new_car_obs,
    append    = TRUE,
    row.names = FALSE
  )
}


# 4) PRISÆNDREDE BILER (nye observationer) ---------------------------
price_cars <- inner_join(bilbasen_df2, bilbasen_df, by = "carid") %>%
  filter(price.x != price.y) %>%
  pull(carid) %>%
  unique()

new_price <- bilbasen_df2 %>%
  filter(carid %in% price_cars) %>%
  select(carid, scrapedate, price, km, description)

if (nrow(new_price) > 0) {
  dbWriteTable(
    con,
    name      = "car_observation",
    value     = new_price,
    append    = TRUE,
    row.names = FALSE
  )
}


# 5) SOLGTE BILER ----------------------------------------------------
sold_cars <- anti_join(bilbasen_df, bilbasen_df2, by = "carid")

if (nrow(sold_cars) > 0) {
  # lav en kommasepareret liste af carid'er i korrekt SQL-format
  idlist <- paste0("'", sold_cars$carid, "'", collapse = ",")
  dbExecute(
    con,
    paste0(
      "UPDATE car_observation
       SET sold = TRUE
       WHERE carid IN (", idlist, ");"
    )
  )
}

# Luk forbindelsen
dbDisconnect(con)

# Scriptet opdeler opdateringen i fem blokke: nye forhandlere, nye biler, nye observationer, prisændringer og solgte biler.
# For hver blok indsættes eller opdateres kun de relevante rækker i databasen, og foreign keys sikrer korrekt relation mellem tabellerne.




#
# Opgave 2.4 – Scrape & SQL med Miljødata.
#
# I skal kigge på https://envs.au.dk/om-instituttet-1/faglige-omraader/luftforurening-udledninger-og-effekter/data-om-luftkvalitet/aktuelle-målinger/tabeller
# og hente links fra H.C.Andersens Boulevard, Anholt, Banegårdsgade i Århus og Risø.
# I skal lave et R-script som kan hente data fra de fire lokationer og gemme dem i fire tabeller.
# I skal derpå vente en dag og så hente data igen og opdatere tabellerne med nye data.

## WULFS KODE ##
library(httr)
library(rvest)

####Opgave 2.4 - Scrape & SQL med Miljødata ####

#Headers
headers <- add_headers(
  `accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
  `accept-encoding` = "gzip, deflate, br, zstd",
  `accept-language` = "da-DK,da;q=0.9,en-US;q=0.8,en;q=0.7",
  `cache-control` = "max-age=0",
  `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36"
)

#Liste med URL'er til de forskellige sider
urls <- list(
  "HCAB" = "https://envs2.au.dk/Luftdata/Presentation/table/Copenhagen/HCAB",
  "ANHO" = "https://envs2.au.dk/Luftdata/Presentation/table/Rural/ANHO",
  "AARH3" = "https://envs2.au.dk/Luftdata/Presentation/table/Aarhus/AARH3",
  "RISOE" = "https://envs2.au.dk/Luftdata/Presentation/table/Rural/RISOE"
)

#Funktion til at hente data fra en URL
url="https://envs2.au.dk/Luftdata/Presentation/table/Copenhagen/HCAB"
# Send GET-anmodning for at hente CSRF-token

fetch_data <- function(url) {
  get_response <- GET(url, headers)
  
  # Kontroller om GET-anmodningen var succesfuld
  print(get_response$status_code)
  page_content <- content(get_response, as = "text")
  #writeLines(page_content,"out.txt")
  page <- read_html(page_content)
  # Udtræk CSRF-token
  csrf_token <- page %>%
    html_element("input[name='__RequestVerificationToken']") %>%
    html_attr("value")
  
  # Fejlhåndtering hvis CSRF-token ikke er udtrukket
  if (is.na(csrf_token)) {
    stop("Failed to extract CSRF-token for URL: ", url)
  } else {
    print(paste("CSRF-token extracted for", url, ":", csrf_token))
  }
  
  # Brug URL-encode til at sikre korrekt formatering af tokenet
  encoded_csrf_token <- URLencode(csrf_token)
  
  # Forbered POST-anmodningen (payload)
  payload <- list(
    `__RequestVerificationToken` = encoded_csrf_token  # Den korrekte CSRF-token
  )
  
  # Cookies 
  cookies <- set_cookies(
    CookieScriptConsent = '{"bannershown":1,"action":"accept","consenttime":1718103120,"categories":"[\"targeting\",\"functionality\",\"performance\",\"unclassified\"]","key":"c414c3ce-0f5d-45f7-9258-0e3eb062b385"}',
    `__RequestVerificationToken_L0x1ZnRkYXRhL1ByZXNlbnRhdGlvbg2` = "vGZmXU8znQRoPge8zmnonE-UF08FjDOMu2TY_sQ3Zvdz98-8n2j2yrCvva-pnPoBm262cJMWEq85s9eEuvObNwJM5SGksjnmZOfY8Yo8tFE1"
  )
  
  # Send POST-anmodning
  post_url <- paste0("https://envs2.au.dk/Luftdata/Presentation/table/MainTable/", gsub(".*table/", "", url))
  post_response <- POST(
    url = post_url,
    body = payload,
    encode = "form",
    headers = headers,
    cookies = cookies
  )
  
  # Hent data efter POST-anmodning
  if (post_response$status_code == 200) {
    print(paste("POST request successful for", url))
    
    # Parse HTML-indholdet fra POST-anmodningen
    post_page <- read_html(content(post_response, as = "text"))
    
    # Ekstrahér tabellen
    table <- post_page %>%
      html_element("table.table-bordered") %>%
      html_table(header = TRUE)
    
  } else {
    stop("Failed to fetch data with POST request for URL:", url, "Status code: ", post_response$status_code)
  }
}

#Hent data fra alle URL'er og gem i separate dataframes
data_hcab <- fetch_data(urls$HCAB)
data_anho <- fetch_data(urls$ANHO)
data_aarh3 <- fetch_data(urls$AARH3)
data_risoe <- fetch_data(urls$RISOE)


