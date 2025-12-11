#
# Opgave 3 – OpenSky
#
#
# Opgave 3.1 – Hent fly og plot
# I tager udgangspunkt i den udleverede kode - getCircl.R fra https://github.com/cphstud/DALE25W43 samt rds-filerne med cirklende fly.
# Koden henter en liste af fly ned fra min nordsø-bounding-box og putter icao-numrene i en vektor.
# Hent nu en ”frisk” liste af fly over Nordsøen. Lav et barplot af antal fly fordelt på lande.
#

# Trin 0 - Lav en safeGET-funktion, så vi kan hente ned fra API’et uden at blive lukket ude:
safeGET <- function(url, token, max_tries = 5) {
  for (i in 1:max_tries) {
    res <- httr::GET(
      url,
      add_headers(Authorization = paste("Bearer", token))
    )
    code <- res$status_code
    if (code == 200) {
      return(res)                 # Succes
    }
    if (code == 429) {
      message("429 Too Many Requests – venter 5 sekunder (forsøg ", i, "/", max_tries, ")")
      Sys.sleep(5)                # Vent før næste forsøg
      next                        # prøv igen
    }
    if (code >= 500) {
      message("Serverfejl ", code, " – venter 3 sekunder og prøver igen.")
      Sys.sleep(3)
      next
    }
    stop("API-kald fejlede med statuskode: ", code)
  }
  stop("safeGET opgav efter ", max_tries, " forsøg.")
}


options(scipen = 999)

#
# Vi har hentet fly d. 11. dec. 2025.
#
# Trin 1 - Henter relevante pakker:
library(jsonlite)
library(httr)
library(stringr)
library(ggplot2)
library(dplyr)

# Trin 2 - Henvis til min anden R-fil som indeholder min adgang til OpenSky API:
source("util.r")

# Trin 3 - Definér OpenSky-endpoint og Nordsø-bounding-box:
baseurl  <- "https://opensky-network.org/api"   # Forsiden til API'et. Alle kald starter med denne adresse.
endpoint_s <- "/states/all"                      # Det er dette endpoint som url-adressen skal ende på.

# NORTH SEA bounding box (samme som underviser)
lamin <- 52.055129
lamax <- 56.196869
lomin <- -6.065140
lomax <- 4.305954

# Step 4 - Byg URL til at hente “frisk” liste af fly over Nordsøen.
# Der defineres en URL for de fly vi vil hente, som her har bestemte bredde-/længdegrader, for Nordsøen:
fullurl <- paste0(baseurl, endpoint_s,
  "?lamin=", lamin,
  "&lomin=", lomin,
  "&lamax=", lamax,
  "&lomax=", lomax)

# Step 5 - Hent data fra OpenSky med token:
# Henter min "adgangsbillet" til API'en:
token <- getToken()
# Giver API'en min adgangsnøgle og 'spørger' om tilladelse:
res <- safeGET(fullurl, token)

res$status_code   # Statuskoden er 200, hvilket betyder det er accepteret og den har hentet dataen.

# Trin 6 - Omdan JSON til et dataframe (snapshot over Nordsøen)
# Giv mig kun selve JSON teksten:
rescontent <- content(res, as = "text")
# Oversæt JSON formatet til R’s datastruktur:
resretval  <- fromJSON(rescontent)

# Dataframe med selve flyene. "resretval$states" er et array af alle fly.
statedf <- as.data.frame(resretval$states)

# Step 7 - Giv kolonnerne meningsfulde navne:
colnames(statedf)[1:17] <- c(
  "icao24",
  "callsign",
  "origin_country",
  "time_position",
  "last_contact",
  "lng",
  "lat",
  "baro_altitude",
  "on_ground",
  "velocity",
  "crs",                  # her kalder vi true_track for crs (kurs) – ligesom underviser
  "vertical_rate",
  "sensors",
  "geo_altitude",
  "squawk",
  "spi",
  "position_source")

# Trin 8 - Tæl antal fly fordelt på lande:
country_counts <- statedf %>%
  filter(!is.na(origin_country)) %>%             # Filtrerer eventuelle rækker fra, hvor landet er en NA
  count(origin_country, name = "antal_fly") %>%  # Count er en funktion der grupperer landene og tæller dem samtidig. Den hører til pakken "dplyr".
  arrange(desc(antal_fly))

# Trin 9 - Lav barplot af antal fly fordelt på lande:
ggplot(country_counts,
       aes(x = reorder(origin_country, antal_fly),
           y = antal_fly)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Storbritannien står for størstedelen af flytrafikken over Nordsøen",
    subtitle = "Snapshot hentet d. 11. december 2025",
    x = "Land",
    y = "Antal fly",
    caption = "Kilde: OpenSky API")+
  theme_minimal() +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0))

#
# Opgave 3.2 – Start small - ét fly
#
# I tager udgangspunkt i ét fly og henter nu tracks for det givne fly.
# Gør det samme for ét af de cirklende fly. Plot nu begge og kommenter.
#

#
# Trin 1-4 genbruges fra opgave 3.1.
#

# Trin 1 - Lav en vektor med icao24/flynumre for “normale” fly:
icaov <- statedf$icao24

# Trin 2 - Vælg ét fly fra vektoren (her nummer 8):
icao_ny <- icaov[[8]]
icao_ny   # så vi kan se hvilket fly (icao24) vi arbejder med

# Flynummer/icao = 406753

# Trin 3 - Definér endpoint til tracks (historiske spor for ét fly)
endpoint_t <- "/tracks/all"

# Trin 4 - Byg URL og hent tracks for det valgte fly
token <- getToken()

turl <- paste0(baseurl, endpoint_t,
               "?icao24=", icao_ny,
               "&time=0")   # time = 0 betyder "seneste tilgængelige track"

res_track <- GET(turl, add_headers(Authorization = 
                                     paste("Bearer", token)))

res_track$status_code   # statuskoden viser 200 = OK

# Trin 5 - Omdan track-data til dataframe.
# Trin 5.1 - Giv mig kun selve JSON teksten:
rescontent_track <- content(res_track, as = "text")

# Trin 5.2 - Oversæt JSON formatet til R’s datastruktur:
resretval_track  <- fromJSON(rescontent_track)

# Trin 5.3 - Lav det til et dataframe:
trackdf <- as.data.frame(resretval_track)

# Trin 6 - Giv kolonnerne meningsfulde navne som underviser
colnv <- c("icao24",
           "callsign",
           "startTime",
           "endTime",
           "time",
           "lat",
           "lng",
           "alt",
           "crs",
           "grd")

colnames(trackdf) <- colnv

# Trin 7 - Hent ét af de cirklende fly fra træningsdata (.rds-fil)
# Vi bruger her filen "circMT.rds" som et eksempel på et cirklende fly.
circdf <- readRDS("circMT.rds")

# Trin 8 - Plot det normale flys rute og det cirklende flys rute.
# Trin 8.1 - Plot ruten (lat/lng) for det normale fly:
ggplot(trackdf, aes(x = lng, y = lat)) +
  geom_path() +
  labs(
    title = "Rute for normalt fly",
    subtitle = paste("icao24:", icao_ny),
    x = "Længdegrad",
    y = "Breddegrad",
    caption = "Kilde: OpenSky API") +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0))


# Trin 8.2 - Plot ruten (lat/lng) for det cirklende fly:
ggplot(circdf, aes(x = lng, y = lat)) +
  geom_path() +
  labs(
    title = "Rute for cirklende fly (træningsdata)",
    subtitle = "Eksempel: circMT.rds",
    x = "Længdegrad",
    y = "Breddegrad",
    caption = "Kilde: OpenSky API og cirklende fly fra træningsdata (.rds)") +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0))

# Trin 8.3 - Plot ruten for begge fly ved siden af hinanden:
# Lav en ny kolonne i hver dataframe der beskriver typen af fly:
trackdf$flytype <- "Normalt fly"
circdf$flytype <- "Cirkulerende fly"

# Kombinér de to dataframes:
combined <- rbind(trackdf, circdf)

# Lav to plot der står ved siden af hinanden:
ggplot(combined, aes(x = lng, y = lat, color = flytype)) +
  geom_path() +
  facet_wrap(~ flytype, scales = "free") +   # Lav flere små plots (paneler) – ét for hver kategori i variablen (flytype = 2).
  labs(
    title = "Flyruter for cirklende fly vs. normalt fly",
    x = "Længdegrad",
    y = "Breddegrad",
    color = "Flytype",
    caption = "Kilde: OpenSky API og cirklende fly fra træningsdata (.rds)") +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0))

#
# Opgave 3.3 – Move fast - alle fly
#
# I looper nu igennem alle jeres friske fly-icaoer og henter deres respective tracks.
# I skal nu udfylde standardafvigelsen på kursen samt R-squared fra lm(lat ~ lng).
# Gør det samme for træningsflyene.

# Først gør vi det for alle de nye 'normale' fly vi har hentet!

# Trin 1 - Genrbug vektoren "icaov" med flynumrene fra opg. 3.2:
icaov

# Trin 2 - Lav en funktion, der henter track for ét fly:
get_track <- function(icao) {
 
   # Hent token
  token <- getToken()
  
  # Byg URL til tracks-endpointet for dette fly
  turl <- paste0(baseurl, endpoint_t,
                 "?icao24=", icao,
                 "&time=0")               # seneste tilgængelige track
  
  # Kald OpenSky API
  res <- httr::GET(turl,
                   add_headers(Authorization = paste("Bearer", token)))
  
  # Hvis vi ikke får kode 200, så returnér NULL
  if (res$status_code != 200) {
    return(NULL)
  }
  # Lav svar om til tekst.
  res_txt  <- httr::content(res, as = "text")
  # Lav svar om til R-objekt.
  res_json <- jsonlite::fromJSON(res_txt)
 
   # Lav det til et dataframe.
  df_track <- as.data.frame(res_json)
  
  # Hvis der ingen rækker er, returner null.
  if (nrow(df_track) == 0) {
    return(NULL)
  }
  
  # Giv kolonnerne relevante navne.
  colnv <- c("icao24",
             "callsign",
             "startTime",
             "endTime",
             "time",
             "lat",
             "lng",
             "alt",
             "crs",
             "grd")
  colnames(df_track) <- colnv
  
  # Returnér dataframen for dette fly
  return(df_track)
}

# Trin 3 - Lav en tom liste til at gemme alle tracks i:
all_tracks <- list()

# Trin 4 - Lav et loop over alle fly og hent deres tracks:
for (icao in icaov) {
  # Hent track for ét fly
  track_df <- get_track(icao)
  # Hvis der kom noget data (ikke er null), gem det i listen
  if (!is.null(track_df)) {
    all_tracks[[icao]] <- track_df
  }
  # Pause for at undgå at hente for meget for hurtigt:
  Sys.sleep(0.3)
}

# Trin 5 - Lav et tomt dataframe til standardafvigelse og r-squared for alle de trackede fly:
resultater <- data.frame(
  icao24 = character(),
  sd_crs = numeric(),
  r2     = numeric() )

# Trin 6 - Loop igennem alle flys tracks og beregn standardafvigelse og R²:
# Hent pakken circular
library(circular)

for (icao in names(all_tracks)) {
  # Hent track-data for dette fly
  df <- all_tracks[[icao]]
  
  # Fjern eventuelle rækker med manglende værdier i lat/lng/crs
  df <- df %>%
    dplyr::filter(!is.na(lat),
                  !is.na(lng),
                  !is.na(crs))
 
  # Lav kursen om til cirkulære data (i grader)
  crs_circ <- circular(df$crs,
                       units = "degrees",
                       template = "geographics")
  
  # Beregn cirkulær standardafvigelse på kursen (i grader)
  sd_crs <- sd.circular(crs_circ, units = "degrees")
  
  # Beregn R² fra en lineær regression for lat ~ lng
  model <- lm(lat ~ lng, data = df)
  r2_fly <- summary(model)$r.squared
  
  # Tilføj en række til resultat-tabellen med resultaterne for det ene fly (det loopes over alle fly)
  resultater <- rbind(
    resultater,
    data.frame(
      icao24 = icao,
      sd_crs = sd_crs,
      r2     = r2_fly))
  }

# Nu gør vi det samme for de 8 cirklende fly vi har fået som træningsfly!

# Trin 7 – Saml de 8 cirklende fly i en liste:
circ_list <- list(
  circjet = circjet,
  circjet2 = circjet2,
  circjet3 = circjet3,
  circjet4 = circjet4,
  circjet5 = circjet5,
  circjet6 = circjet6,
  circjet7 = circjet7,
  circMT = circMT)

# Trin 8 – Lav et tomt resultat-dataframe til træningsflyene, ligesom vi gjorde med de normale fly:
resultater_circ <- data.frame(
  icao24 = character(),
  sd_crs = numeric(),
  r2     = numeric() )

# Trin 9 – Loop igennem alle de 8 cirklende fly.
# Vi laver præcis den samme beregning som for de friske fly, bare på circ_list i stedet:
for (icao in names(circ_list)) {
  
  df <- circ_list[[icao]]   # hent data for ét cirkelfly
  
  # Ryd evt. NA'er (hvis nogen)
  df <- df %>%
    dplyr::filter(!is.na(lat),
                  !is.na(lng),
                  !is.na(crs))
  
  # Lav kursen om til cirkulære data (i grader)
  crs_circ <- circular(df$crs,
                       units = "degrees",
                       template = "geographics")
  
  # Beregn cirkulær standardafvigelse på kursen (i grader)
  sd_crs <- sd.circular(crs_circ, units = "degrees")
  
  # R² fra regression lat ~ lng
  model <- lm(lat ~ lng, data = df)
  r2_circ <- summary(model)$r.squared
  
  # Tilføj række til resultattabellen
  resultater_circ <- rbind(
    resultater_circ,
    data.frame(
      icao24 = icao,   # fx "circ1", "circMT" osv.
      sd_crs = sd_crs,
      r2     = r2_circ))
  }

# Trin 10 - Visualisering af forskellene i de to typer af fly, i et plot.

# Trin 10.1 - Indsæt en 'type'-kolonne i hver af de to dataframes:
resultater$type <- "Normalt fly"
resultater_circ$type  <- "Cirkulerende fly"

# Trin 10.2 - Kombiner de to dataframes:
resultater_comb <- rbind(resultater, resultater_circ)

# Trin 10.3 - Lav et boxplot der viser standardafvigelsen og forklaringsgraden for hver flytype:

ggplot(resultater_comb, aes(x = sd_crs, y = r2, color = type)) +
  geom_point(size = 3) +
  labs(
    title = "Cirkulerende fly identificeres tydeligt ved højere standardafvigelse i kurs",
    x = "Standardafvigelse i kurs (crs)",
    y = "R² fra lm(lat ~ lng)",
    caption = "Kilde: OpenSky API og træningsdata (.rds)",
    color = "Flytype") +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0))

# Trin 11 - Lav et gns af standardafvigelsen og R2 for begge flytyper:
samlet_gns <- resultater_comb %>%
  group_by(type) %>%
  summarise(
    mean_sd  = mean(sd_crs, na.rm = TRUE),
    mean_r2  = mean(r2, na.rm = TRUE),
    n = n()
  )


#
# Opgave 3.4 – Aim high - jeres egen algoritme
#
# I skal nu designe jeres egen algoritme som kan spotte cirklende fly.
# Man kan f.eks overveje om flyveturen skal deles op i faser - f.eks lette, march, landing.
# Eller om man på lat og/eller long kan spotte, hvornår et fly ”krydser” sit eget spor - altså cirkler.
# Jeres resultater skal samles i en dataframe.

# Trin 1 - Genbrug datasættet "resultater_comb" fra opgave 3.3:
resultater_comb

# Trin 2 - Definér en simpel regel (algoritme):
sd_grænse <- 1.1
# Vi vælger at tage udgangspunkt i standardafvigelsen for normale fly og cirklende fly.
# Vi tager midten af gennemsnittet er hvar type fly. Det svarer til en grænse på cirka 1.1.
# Hvis standardafvigelsen er større end 1.1, kategoriseres den som et cirklende fly.

# Trin 3 - Lav en tom dataframe til resultaterne:
final_df <- resultater_comb

# Trin 4 - (Kør algoritmen) Lav en ny kolonne "MyAlg" hvor der udregnes om det er et cirklende fly eller ej, ud sd_crs:
final_df$MyAlg <- ifelse(final_df$sd_crs > sd_grænse, 1, 0)

# Trin 5 - Lav en kolonne med den faktiske facit (om det er normalt eller cirklende):
final_df$facit <- ifelse(final_df$type == "Cirkulerende fly", 1, 0)

#
# Nu vil vi prøve i stedet for at tage hele flyruten, kun at kigge på den midterste del (50%) af flyruten, og se om, vi kan lave en bedre algoritme.
# Samtidig vil vi bruge en funktion der udregner hvilke sd_grænse der har den bedste accuracy.

#
# Trin 6 - Lav en funktion der udtrækker rutens midt-sektion:
middle_part <- function(df, mid_part = 0.5) {
  # Sortér på tid:
  df <- df[order(df$time), ]
  # Antal rækker:
  n <- nrow(df)
  # Hvor meget der skal skæres fra hver ende:
  trim <- (1 - mid_part) / 2     # 1-0.5 = 0.5. & 0.5/2 = er 0.25 der skal trimmes i hver ende.
  # Brug tid i stedet for index:
  t_min <- quantile(df$time, trim)      # det tidspunkt hvor midterdelen af flyvningen starter (25%-percentilen)
  t_max <- quantile(df$time, 1 - trim)  # det tidspunkt hvor midterdelen af flyvningen slutter (75%-percentilen)
  # Udvælg midten:
  df_mid <- df[df$time >= t_min & df$time <= t_max, ]
  return(df_mid)
}

# Trin 7 - Lav et nyt resultat-dataframe, der bruger kun midten af flyvningen for normale fly:
resultater_mid <- data.frame(
  icao24 = character(),
  sd_crs = numeric(),
  r2     = numeric(),
  type   = character() )

# Trin 8 - Lav et loop der indsamler alle flyenes tracks og beregn standardafvigelse og R² (kun midten af flyruten):
for (icao in names(all_tracks)) {
  # Hent track-data for dette fly
  df <- all_tracks[[icao]]
  
  # Fjern eventuelle rækker med manglende værdier i lat/lng/crs
  df <- df %>%
    dplyr::filter(!is.na(lat),
                  !is.na(lng),
                  !is.na(crs))
  
  # Tag kun midterste del af flyvningen
  df_mid <- middle_part(df, mid_part = 0.5)
 
   # Spring flyet over, hvis der næsten ikke er noget data i midten
  if (nrow(df_mid) < 6) {
    next
  }
  # Lav kursen om til cirkulære data (i grader)
  crs_circ <- circular(df_mid$crs,
                       units = "degrees",
                       template = "geographics")
  
  # Beregn cirkulær standardafvigelse på kursen (i grader)
  sd_crs <- sd.circular(crs_circ, units = "degrees")
  
  # Beregn R² fra en lineær regression for lat ~ lng
  model <- lm(lat ~ lng, data = df_mid)
  r2_fly <- summary(model)$r.squared
  
  # Tilføj en række til resultat-tabellen med resultaterne for det ene fly (det loopes over alle fly)
  resultater_mid <- rbind(
    resultater_mid,
    data.frame(
    icao24 = icao,
    sd_crs = sd_crs,
    r2     = r2_fly,
    type   = "Normalt fly"))
  }

# Så gør vi det for cirklende fly.

# Trin 9 - Lav resultat-dataframe til de cirklende fly:

resultater_circ_mid <- data.frame(
  icao24 = character(),
  sd_crs = numeric(),
  r2     = numeric(),
  type   = character() )

#Trin 10 - Loop over de 8 cirklende fly og brug middle_part:

for (icao in names(circ_list)) {
  # Hent track-data for dette fly
  df <- circ_list[[icao]]
  # Fjern eventuelle rækker med manglende værdier i lat/lng/crs
  df <- df %>%
    dplyr::filter(!is.na(lat),
                  !is.na(lng),
                  !is.na(crs))
  # Tag kun midterste del af flyvningen
  df_mid <- middle_part(df, mid_part = 0.5)
  # Spring flyet over, hvis der næsten ikke er noget data i midten
  if (nrow(df_mid) < 6) {
    next
  }
  # Lav kursen om til cirkulære data (i grader)
  crs_circ <- circular(df_mid$crs,
                       units = "degrees",
                       template = "geographics")
  # Beregn cirkulær standardafvigelse på kursen (i grader)
  sd_crs <- sd.circular(crs_circ, units = "degrees")
  # Beregn R² fra en lineær regression for lat ~ lng
  model <- lm(lat ~ lng, data = df_mid)
  r2_fly <- summary(model)$r.squared
  # Tilføj en række til resultat-tabellen med resultaterne for det ene fly (det loopes over alle fly)
  resultater_circ_mid <- rbind(
    resultater_circ_mid,
    data.frame(
      icao24 = icao,
      sd_crs = sd_crs,
      r2     = r2_fly,
      type   = "Cirklende" ))
  }

# Trin 11 - Kombinér de to dataframes (normale fly og cirklende fly):

resultater_mid_comb <- rbind(resultater_mid, resultater_circ_mid)

# Trin 12 - Vælg grænse for standardafvigelsen ud fra de kendte cirklende fly:
# Vi bruger kun sd_crs for træningsflyene til at sætte grænsen
sd_circ   <- resultater_circ_mid$sd_crs
sd_grænse <- quantile(sd_circ, 0.25)  # f.eks. 25% af deres sd.

sd_grænse   # så vi kan se, hvad grænsen bliver

#Resultat:
# grænsen for at vurdere om det er et cirklende fly ligger på 1.4.

# Trin 13 - Lav et final dataframe der kører algoritmen over alle flyene:
final_mid_df <- resultater_mid_comb

# 1 hvis algoritmen mener det er cirklende fly, ellers 0
final_mid_df$MyAlg <- ifelse(final_mid_df$sd_crs > sd_grænse, 1, 0)

# facit: 1 for cirklende fly, 0 for normale
final_mid_df$facit <- ifelse(final_mid_df$type == "Cirklende", 1, 0)

# Trin 14 - Fjerne 'type'-kolonne da den ikke er relevnt mere da vi har 'facit':
final_mid_df$type <- NULL

# Trin 15 - Vis hvor meget i procent, den tager fejl og har ret
# Trin 15.1 - Beregn accuracy og error rate
accuracy <- mean(final_mid_df$MyAlg == final_mid_df$facit)
error_rate <- 1 - accuracy

# Trin 15.2 - Lav dataframe til at plotte:
plot_df <- data.frame(
  kategori = c("Rigtige", "Fejl"),
  procent = c(accuracy * 100, -error_rate * 100)
)

# Trin 15.3 - Lav søjlediagram:

ggplot(plot_df, aes(x = kategori, y = procent, fill = kategori)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, color = "black") +
  scale_fill_manual(values = c("Rigtige" = "#2E8B57", "Fejl" = "#B22222")) +
  labs(
    title = "Algoritmen rammer rigtigt i langt de fleste tilfælde",
    subtitle ="Negativ søjle = fejlprocent",
    x = "",
    y = "Procent",
    caption = "Kilde: OpenSky API og træningsdata (.rds)"
  ) +
  scale_y_continuous(labels = function(x) paste0(abs(x), "%")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.caption = element_text(hjust = 0)
  )
