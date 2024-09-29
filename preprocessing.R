install.packages(c('DBI','RPostgres','sf','geojsonsf',
                   'leaflet','wkb','leaflet.extras'))

library(sf)
library(geojsonsf)
library(leaflet)
library(leaflet.extras)
library(wkb)
library(DBI)
library(RPostgres)

csv_files <- c(
  "C:/Users/nikola.pujaz/source/repos/npdrums/TrafficAccidents/DataSeedingTool/SeedData/nez-opendata-2015.csv",
  "C:/Users/nikola.pujaz/source/repos/npdrums/TrafficAccidents/DataSeedingTool/SeedData/nez-opendata-2016.csv",
  "C:/Users/nikola.pujaz/source/repos/npdrums/TrafficAccidents/DataSeedingTool/SeedData/nez-opendata-2017.csv",
  "C:/Users/nikola.pujaz/source/repos/npdrums/TrafficAccidents/DataSeedingTool/SeedData/nez-opendata-2018.csv",
  "C:/Users/nikola.pujaz/source/repos/npdrums/TrafficAccidents/DataSeedingTool/SeedData/nez-opendata-2019-20200125.csv",
  "C:/Users/nikola.pujaz/source/repos/npdrums/TrafficAccidents/DataSeedingTool/SeedData/nez-opendata-2020-20210125.csv",
  "C:/Users/nikola.pujaz/source/repos/npdrums/TrafficAccidents/DataSeedingTool/SeedData/nez-opendata-2021-20220125.csv",
  "C:/Users/nikola.pujaz/source/repos/npdrums/TrafficAccidents/DataSeedingTool/SeedData/nez-opendata-2022-20230125.csv",
  "C:/Users/nikola.pujaz/source/repos/npdrums/TrafficAccidents/DataSeedingTool/SeedData/nez-opendata-2023-20230425.csv"
)

read_and_format_csv <- function(file_path) {

  csv_data <- read.csv(file_path, header = FALSE, stringsAsFactors = FALSE)
  
  colnames(csv_data) <- c("case_number", 
                          "police_department",
                          "municipality_name", 
                          "reported_on_raw", 
                          "longitude", 
                          "latitude", 
                          "participants_status",
                          "participants_nominal_count", 
                          "description")
  
    return(csv_data)
}

all_csv_data <- lapply(csv_files, read_and_format_csv)

traffic_accidents <- do.call(rbind, all_csv_data)

# Dodeljivanje accident_type na osnovu description kolone
classify_accident_type <- function(accident_description) {
  
  accident_type <- ifelse(
    # Angle
    accident_description %in% c(
      'Najmanje dva vozila koja se kreću istim putem u istom smeru uz skretanje, skretanje ulevo ispred drugog vozila',
      'Najmanje dva vozila koja se kreću u istom smeru – uključivanje u saobraćaj',
      'Najmanje dva vozila koja se kreću različitim putevima uz skretanje ulevo i uz nailazak vozila sleva',
      'Najmanje dva vozila koja se kreću istim putem u istom smeru uz skretanje, skretanje udesno ispred drugog vozila',
      'Najmanje dva vozila koja se kreću istim putem u istom smeru uz skretanje, polukružno okretanje ispred drugog vozila',
      'Najmanje dva vozila koja se kreću različitim putevima uz prolazak kroz raskrsnicu, ili od kojih jedno prelazi preko kolovoza, bez skretanja',
      'Najmanje dva vozila koja se kreću različitim putevima uz skretanje ulevo i uz nailazak vozila zdesna',
      'Najmanje dva vozila koja se kreću različitim putevima uz skretanje oba vozila',
      'Najmanje dva vozila koja se kreću istim putem u suprotnim smerovima uz polukružno okretanje ispred drugog vozila'
    ), 1, ifelse(
      
      # Fall Of Road
      accident_description %in% c(
        'Nezgoda sa jednim vozilom – silazak sa kolovoza u krivini',
        'Nezgoda sa jednim vozilom – silazak udesno sa kolovoza na pravcu',
        'Nezgoda sa jednim vozilom – silazak ulevo sa kolovoza na pravcu'
      ), 2, ifelse(
        
        # Head On Collision
        accident_description %in% c(
          'Najmanje dva vozila koja se kreću različitim putevima uz skretanje udesno – čeoni sudar sa vozilom koje nailazi zdesna',
          'Najmanje dva vozila – čeoni sudar',
          'Najmanje dva vozila koja se kreću istim putem u suprotnim smerovima i vrše skretanje na isti put',
          'Najmanje dva vozila koja se kreću istim putem u suprotnim smerovima uz skretanje ulevo ispred drugog vozila',
          'Najmanje dva vozila koja se kreću različitim putevima uz skretanje udesno ispred vozila koje nailazi sleva',
          'Najmanje dva vozila koja se kreću istim putem u suprotnim smerovima i vrše skretanje na naspramne puteve'
        ), 3, ifelse(
          
          # Rear End Collision
          accident_description %in% c(
            'Najmanje dva vozila koja se kreću u istom smeru – sustizanje',
            'Najmanje dva vozila koja se kreću istim putem u istom smeru uz skretanje, sudar u sustizanju'
          ), 4, ifelse(
            
            # Rear To Rear Collision
            accident_description %in% c(
              'Najmanje dva vozila – suprotni smerovi bez skretanja – kretanje unazad'
            ), 5, ifelse(
              
              # Side Swipe
              accident_description %in% c(
                'Najmanje dva vozila koja se kreću u istom smeru – preticanje',
                'Najmanje dva vozila koja se kreću u istom smeru – sudar pri uporednoj vožnji'
              ), 6, ifelse(
                
                # Turn Over
                accident_description %in% c(
                  'Nezgoda sa jednim vozilom i prevrtanjem'
                ), 7, ifelse(
                  
                  # With Animals
                  accident_description %in% c(
                    'Nezgode sa učešćem jednog vozila i životinja'
                  ), 8, ifelse(
                    
                    # With Obstacles
                    accident_description %in% c(
                      'Nezgode sa učešćem jednog vozila i preprekama na ili iznad kolovoza',
                      'Nezgode sa učešćem jednog vozila na mestu na kome se izvode radovi na putu'
                    ), 9, ifelse(
                      
                      # With Parked Vehicle
                      accident_description %in% c(
                        'Sudar sa parkiranim vozilom sa desne strane kolovoza',
                        'Ostali sudari sa parkiranim vozilom',
                        'Sudar sa parkiranim vozilom sa leve strane kolovoza',
                        'Sudar sa parkiranim vozilom pri otvaranju vrata',
                        'Sudar sa parkiranim vozilom – bilo sa leve ili sa desne strane kolovoza'
                      ), 10, ifelse(
                        
                        # With Pedestrians
                        accident_description %in% c(
                          'Prelazak pešaka sleva, van raskrsnice , bez skretanja vozila',
                          'Prelazak pešaka sleva, sa skretanjem vozila ulevo, u raskrsnici',
                          'Prelazak pešaka zdesna, sa skretanjem vozila ulevo, u raskrsnici',
                          'Pešak – ostale situacije',
                          'Prelazak pešaka zdesna, van raskrsnice, bez skretanja vozila',
                          'Prelazak pešaka sleva, u raskrsnici, bez skretanja vozila',
                          'Pešak se kreće duž kolovoza',
                          'Prelazak pešaka preko kolovoza, van raskrsnice, bez skretanja vozila',
                          'Prelazak pešaka preko kolovoza sa skretanjem vozila ulevo, u raskrsnici',
                          'Pešak se kreće duž kolovoza u smeru kretanja vozila',
                          'Prelazak pešaka zdesna , u raskrsnici, bez skretanja vozila',
                          'Prelazak pešaka preko kolovoza, u raskrsnici, bez skretanja vozila',
                          'Prelazak pešaka preko kolovoza, u ili van raskrsnice, bez skretanja vozila',
                          'Pešak stoji na kolovozu',
                          'Pešak se kreće na trotoaru ili biciklističkoj stazi',
                          'Prelazak pešaka sleva, sa skretanjem vozila udesno, u raskrsnici',
                          'Pešak se kreće duž kolovoza suprotno od smera kretanja vozila',
                          'Pešak zaustavljen na trotoaru ili biciklističkoj stazi',
                          'Prelazak pešaka zdesna, sa skretanjem vozila udesno, u raskrsnici',
                          'Pešak se na trotoaru ili biciklističkoj stazi kreće u smeru kretanja vozila',
                          'Prelazak pešaka preko kolovoza sa skretanjem vozila udesno, u raskrsnici',
                          'Prelazak pešaka preko kolovoza (sleva ili zdesna), sa skretanjem vozila (levo ili desno), u raskrsnici',
                          'Pešak se kreće duž kolovoza ili stoji na kolovozu',
                          'Pešak se na trotoaru ili biciklističkoj stazi kreće suprotno od smera kretanja vozila',
                          'Pešak stoji ili se kreće, sa kretanjem vozila unazad'
                        ), 11, ifelse(
                          
                          # With Rail Vehicle
                          accident_description %in% c(
                            'Nezgode sa učešćem šinskog i drumskog vozila'
                          ), 12, 13)))))))))))) # Default: Unknown
  
  return(accident_type)
}

traffic_accidents$accident_type <- classify_accident_type(traffic_accidents$description)

# Kategorizacija participants_nominal_count
classify_participants_nominal_count <- function(participants_description) {
  
  participants_nominal_count <- ifelse(
    participants_description %in% c(
      'SN SA NAJMANjE DVA VOZILA – BEZ SKRETANjA',
      'SN SA NAJMANjE DVA VOZILA – SKRETANjE ILI PRELAZAK'
    ), 2, 
    ifelse(
      participants_description == 'SN SA PEŠACIMA', 4, 
      ifelse(
        participants_description == 'SN SA JEDNIM VOZILOM', 1, 
        ifelse(
          participants_description == 'SN SA PARKIRANIM VOZILIMA', 3, 
          5))))
  
  return(participants_nominal_count)
}

traffic_accidents$participants_nominal_count <- classify_participants_nominal_count(traffic_accidents$participants_nominal_count)

# Kategorizacija participants_status
classify_participants_status <- function(participants_description) {
  
  participants_status <- ifelse(
    participants_description == 'Sa mat.stetom', 3, 
    ifelse(
      participants_description == 'Sa povredjenim', 1, 
      ifelse(
        participants_description == 'Sa poginulim', 2, 
        4)))
  
  return(participants_status)
}

traffic_accidents$participants_status <- classify_participants_status(traffic_accidents$participants_status)

cities_file <- "C:/Users/nikola.pujaz/source/repos/npdrums/TrafficAccidents/DataSeedingTool/SeedData/gradovi.csv"
cities_data <- read.csv(cities_file, header = TRUE, stringsAsFactors = FALSE)

db_password <- "password" # dodati svoju lozinku

Sys.setenv(POSTGRES_KEY=db_password);

# Kreiranje DB konekcije
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'postgres',
  host = 'localhost',
  port = 5432,
  user = 'postgres',
  password = Sys.getenv('POSTGRES_KEY') 
)
dbExecute(con, "DROP TABLE IF EXISTS temp_cities;")

dbExecute(con, "
  CREATE TABLE temp_cities (
    id SERIAL PRIMARY KEY,
    grad_imel VARCHAR(255),
    geom geometry(Geometry,4326) NOT NULL
  );
")

for (i in 1:nrow(cities_data)) {
  query <- sprintf(
    "INSERT INTO temp_cities (grad_imel, geom) VALUES ('%s', ST_Transform(ST_GeomFromText('%s', 32634), 4326));",
    cities_data$grad_imel[i],
    cities_data$wkt[i]
  )

  dbExecute(con, query)
}

temp <- dbGetQuery(con, "select grad_imel from temp_cities;")
print(temp)

get_city_name <- function(latitude, longitude, con) {
  query <- sprintf("
    SELECT grad_imel 
    FROM temp_cities 
    WHERE ST_Covers(geom, ST_SetSRID(ST_MakePoint(%f, %f), 4326)) 
    LIMIT 1;", longitude, latitude)
  
  result <- dbGetQuery(con, query)
  
  if (nrow(result) > 0) {
    return(result$grad_imel[1])
  } else {
    return(NA)
  }
}

traffic_accidents$city_name <- mapply(get_city_name, 
                                      traffic_accidents$latitude, 
                                      traffic_accidents$longitude, 
                                      MoreArgs = list(con = con))

names(traffic_accidents)[names(traffic_accidents) == "reported_on_raw"] <- "reported_on"
traffic_accidents$reported_on <- as.POSIXct(traffic_accidents$reported_on, format = "%d.%m.%Y,%H:%M")

# Zatvaranje konekcije sa bazom
dbDisconnect(con)

# Filtriranje df - kolone koje ce biti koriscene
traffic_accidents <- traffic_accidents[, c("case_number","police_department", "reported_on", "longitude", "latitude",
                                         "accident_type", "participants_status", "participants_nominal_count",
                                         "municipality_name", "city_name")]
