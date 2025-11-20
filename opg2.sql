#
# Opgave 2.1 – Oprette skemaet for bilbasen
#
# For at kunne gemme bilerne i databasen er det vigtigt at oprette et skema,
# hvor man har invariante entiteter organiseret i tabeller med fremmed-nøgler så de kan linkes sammen,
# samt variende data i tidsserieagtige tabeller, hvor man linker til den sidst-forekommende observation.
# Jeres design skal beskrives i et ER-diagram og jeres DDL-statements skal ligge på github med filextension .sql.

# Trin 1 - Først laves en database hvor alle tabellerne kan indsætter:
CREATE DATABASE bilbasen;

# Trin 2 - Fortæl at vi vil bruge bilbasen-databasen:
USE bilbasen;

# Trin 3 - Lav en tabel for forhandler (invariant):
CREATE TABLE dealer (
    dealer_id   VARCHAR(20) PRIMARY KEY,
    dealer_name        VARCHAR(255),
    dealer_address     VARCHAR(255),
    dealer_cvr         VARCHAR(20)
);
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
CREATE TABLE car (
    carid       VARCHAR(20) PRIMARY KEY,
    model       VARCHAR(100),
    year        DATE,
    loc    		VARCHAR(255),
    link        VARCHAR(255),
    dealer_id   VARCHAR(20),
	FOREIGN KEY (dealer_id)
	REFERENCES dealer(dealer_id)
);
	# Fremmednøglen (FK) skaber relationen mellem to tabeller.
    # CONSTRAINT fk_car_dealer = giver fremmednøglen et navn.
    #  FOREIGN KEY (dealer_id) = Dvs. den peger på en række i en anden tabel. “kolonnen dealer_id i car-tabellen er en fremmed nøgle”.
    # REFERENCES dealer(dealer_id) = “dealer_id i car refererer til dealer_id i dealer-tabellen”.
    
# Trin 5 - Lav en tabel med alle detaljerne/observationer for hver bil (varierende data):
CREATE TABLE car_observation (
    observation_id INT AUTO_INCREMENT PRIMARY KEY,
    carid          VARCHAR(20),
    scrapedate     DATE,
    price          INT,
    km             INT,
    description    TEXT,
    sold           BOOLEAN DEFAULT FALSE,
    FOREIGN KEY (carid)
    REFERENCES car(carid)
);
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
# I R-studio
# Trin 2 - Lav en forbindelse mellem R og MySQL:
# I R-studio
# Trin 3 - Indsæt bilbasen_df-tabellen fra R til MySQL:
# I R-studio


# Trin 4 - Fortæl hvilken tabel vi arbejder i:
USE bilbasen;

# Trin 5 - Indsæt forhandlere i dealer-tabellen:
INSERT INTO dealer (dealer_id, dealer_name, dealer_address, dealer_cvr)
SELECT DISTINCT
    dealer_id,
    dealer_name,
    dealer_address,
    dealer_cvr
FROM scrape1;

# SELECT DISTINCT = Bruger SELECT DISTINCT, når du vil sikre dig, at hver række kun optræder én gang i resultatet.

# Trin 6 - Indsæt biler car:
INSERT INTO car (carid, model, year, loc, link, dealer_id)
SELECT DISTINCT
	carid,
    model,
    year,
    loc,
    link,
    dealer_id
FROM scrape1;

# Trin 7 - Indsæt observationer i car_observation:
INSERT INTO car_observation (carid, scrapedate, price, km, description, sold)
SELECT
	carid,
    scrapedate,
    price,
    km,
    description,
    FALSE
FROM scrape1;
    
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

# Trin 1 - Lav en forbindelse mellem R og MySQL
# I R-studio

# Trin 2 - Indsæt bilbasen_df2-tabellen fra R til MySQL
# I R-studio

# Trin 3 - 
