
#Trin 4 - SQL-opdateringer af databasen
#Trin 4.1 - Find og indsæt nye dealers:

INSERT INTO dealer (dealer_id, dealer_name, dealer_address, dealer_cvr)
SELECT s2.dealer_id, s2.dealer_name, s2.dealer_address, s2.dealer_cvr
FROM scrape2 s2
LEFT JOIN scrape1 s1 ON s1.dealer_id = s2.dealer_id
WHERE s1.dealer_id IS NULL;
 
#Trin 4.2 - Find og indsæt nye biler:

INSERT INTO car (carid, model, year, loc, link, dealer_id)
SELECT s2.carid, s2.model, s2.year, s2.loc, s2.link, s2.dealer_id
FROM scrape2 s2
LEFT JOIN car c ON c.carid = s2.carid
WHERE c.carid IS NULL;
 
#Trin 4.3 - Indsæt første observation for nye biler:

INSERT INTO car_observation (carid, scrapedate, price, km, description)
SELECT carid, scrapedate, price, km, description
FROM scrape2
WHERE carid NOT IN (SELECT carid FROM scrape1);
 
#Trin 4.4 - Indsæt nye prisændringer:
INSERT INTO car_observation (carid, scrapedate, price, km, description)
SELECT carid, scrapedate, price, km, description
FROM scrape2
WHERE carid IN (SELECT carid FROM scrape1)
  AND price <> (SELECT price FROM scrape1 WHERE scrape1.carid = scrape2.carid);
 
#Trin 4.5 - Markér solgte biler:
UPDATE car_observation
SET sold = TRUE
WHERE carid IN (SELECT carid FROM scrape1 WHERE carid NOT IN (SELECT carid FROM scrape2));
 
#Trin 5 - Tjek om databasen er opdateret korrekt:

SELECT co.*
FROM car_observation co
WHERE co.carid NOT IN (SELECT carid FROM scrape2);
 
#Prisændringer:
SELECT *
FROM car_observation
WHERE carid IN (
    SELECT s1.carid
    FROM scrape1 s1
    JOIN scrape2 s2 USING (carid)
    WHERE s1.price <> s2.price)
ORDER BY carid, scrapedate;
