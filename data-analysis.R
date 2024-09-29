# Za ucitavanje biblioteka i df izvrsiti skriptu preprocessing.R
install.packages('ggplot2')
install.packages("arules")

library(ggplot2)
library(arules)

# Osnovne informacije o DF
summary(traffic_accidents)

# Prvih nekoliko redova tabele
head(traffic_accidents)

# Pregled nedostajucih vrednosti
missing_values <- colSums(is.na(traffic_accidents))
print(missing_values)

traffic_accidents <- traffic_accidents[!is.na(traffic_accidents$reported_on),]

# Uklanjanje duplikata?
# traffic_accidents$case_number <- trimws(traffic_accidents$case_number)
# traffic_accidents <- traffic_accidents[!duplicated(traffic_accidents$case_number),]
# print(traffic_accidents[duplicated(traffic_accidents$case_number),])

# Broj saobracajnih nezgoda
length(traffic_accidents$case_number)

# Konverzija kolone reported_on u Date tip
traffic_accidents_reported_on_date <- as.Date(traffic_accidents$reported_on)

# Najraniji datum prijavljivanja nezgode
earliest_date <- min(traffic_accidents_reported_on_date, na.rm = TRUE)
print(format(earliest_date, "%d/%m/%Y"))

# Poslednji datum prijavljivanja nezgode
latest_date <- max(traffic_accidents_reported_on_date, na.rm = TRUE)
print(format(latest_date, "%d/%m/%Y"))

# Pregled prijavljenih nesreca po godinama
traffic_accidents_year <- format(traffic_accidents_reported_on_date, "%Y")
traffic_accidents$year <- traffic_accidents_year
accidents_per_year <- as.data.frame(table(traffic_accidents$year))
colnames(accidents_per_year) <- c("year", "count")

ggplot(accidents_per_year, aes(x = year, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Broj saobraćajnih nezgoda po godinama", x = "Godina", y = "Broj nezgoda") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pregled broja nesreca u periodima dana
traffic_accidents$hour <- as.numeric(format(as.POSIXct(traffic_accidents$reported_on), "%H"))
traffic_accidents$time_interval <- cut(traffic_accidents$hour,
                                       breaks = c(1, 7, 12, 19, 24),
                                       labels = c("01-07", "07-12", "12-19", "19-01"),
                                       include.lowest = TRUE, right = FALSE)

traffic_accidents$time_interval[traffic_accidents$hour == 0] <- "19-01"

time_summary <- aggregate(case_number ~ time_interval, data = traffic_accidents, FUN = length)

colnames(time_summary)[2] <- "count"

ggplot(time_summary, aes(x = time_interval, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.7) +
  labs(title = "Broj saobraćajnih nezgoda u periodima dana",
       x = "Period dana", y = "Broj nezgoda") +
  theme_minimal()

time_summary$percentage <- (time_summary$count / sum(time_summary$count)) * 100

ggplot(time_summary, aes(x = "", y = count, fill = time_interval)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Procenat saobraćajnih nezgoda u periodima dana",
       fill = "Period dana") +
  scale_fill_discrete(labels = paste0(time_summary$time_interval, " (", round(time_summary$percentage, 1), "%)")) +
  theme_void() +
  theme(legend.position = "right")

# Konvertovanje u kategoricki tip, tabelarni, procentualni i grafički prikaz
# accident_type
accident_type_category <- factor(traffic_accidents$accident_type, levels = c(1:13), 
                                 labels = c("Angle", "Fall Of Road", "Head On Collision", "Rear End Collision",
                                            "Rear To Rear Collision", "Side-swipe", "Turnover", "With Animals",
                                            "With Obstacles", "With Parked Vehicle", "With Pedestrians", "With Rail Vehicle",
                                            "Unknown"))

accident_type_table <- table(accident_type_category)
accident_type_percent <- 
  accident_type_table / length(traffic_accidents$accident_type) * 100

accident_type_df <- as.data.frame(accident_type_table)
accident_type_df$Percentage <- accident_type_percent
accident_type_df$LabelWithPercentage <- paste0(
  accident_type_df$accident_type_category, 
  " (", round(accident_type_df$Percentage, 1), "%)")

ggplot(accident_type_df, aes(x = "", y = Freq, fill = LabelWithPercentage)) +
  geom_bar(width = 1, stat = "identity", color = "white") + 
  coord_polar(theta = "y") +
  labs(title = "Procenat tipova saobraćajnih nezgoda", fill = "Tip nezgode") +
  theme_void() +
  theme(legend.position = "right",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 16))

# participants_status
participants_status_category <- factor(traffic_accidents$participants_status, levels = c(1, 2, 3, 4), 
                                       labels = c("Injured", "Killed", "Material Damage", "Unknown"))

participants_status_table <- table(participants_status_category)
participants_status_percent <- 
  participants_status_table / length(traffic_accidents$participants_status) * 100

participants_status_df <- as.data.frame(participants_status_table)
participants_status_df$Percentage <- participants_status_percent
participants_status_df$LabelWithPercentage <- paste0(
  participants_status_df$participants_status_category, 
  " (", round(participants_status_df$Percentage, 1), "%)")

ggplot(participants_status_df, aes(x = "", y = Freq, fill = LabelWithPercentage)) +
  geom_bar(width = 1, stat = "identity", color = "white") + 
  coord_polar(theta = "y") +
  labs(title = "Procenat različitih ishoda saobraćajnih nezgoda", fill = "Ishodi nezgoda") +
  theme_void() +
  theme(legend.position = "right",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 16))

# participants_nominal_count
participants_nominal_count_category <- factor(traffic_accidents$participants_nominal_count, levels = c(1:5), 
                                              labels = c("Single Vehicle", "At Least Two Vehicles", 
                                                         "Parked Vehicle", "With Pedestrians", "Unknown"))

participants_nominal_count_table <- table(participants_nominal_count_category)
participants_nominal_count_percent <- 
  participants_nominal_count_table / length(traffic_accidents$participants_status) * 100

participants_nominal_count_df <- as.data.frame(participants_nominal_count_table)
participants_nominal_count_df$Percentage <- participants_nominal_count_percent
participants_nominal_count_df$LabelWithPercentage <- paste0(
  participants_nominal_count_df$participants_nominal_count_category, 
  " (", round(participants_nominal_count_df$Percentage, 1), "%)")

ggplot(participants_nominal_count_df, aes(x = "", y = Freq, fill = LabelWithPercentage)) +
  geom_bar(width = 1, stat = "identity", color = "white") + 
  coord_polar(theta = "y") +
  labs(title = "Procenat broja učesnika u saobraćajnim nezgodama", fill = "Učesnici") +
  theme_void() +
  theme(legend.position = "right",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 16))

# Opstina sa najvise povredjenih u saobracajnim nezgodama
injured_data <- traffic_accidents[factor(traffic_accidents$participants_status, levels = c(1, 2, 3), 
                                          labels = c("Injured", "Killed", "Material Damage")) == "Injured", ]
injured_summary <- aggregate(case_number ~ municipality_name, data = injured_data, FUN = length)
colnames(injured_summary)[2] <- "count_injured"
injured_summary <- injured_summary[order(-injured_summary$count_injured), ]
head(injured_summary, 1)

# Najzastupleniji tip nezgode na izdvojenoj opstini
top_municipality <- injured_summary$municipality_name[1]
municipality_data_md <- traffic_accidents[traffic_accidents$municipality_name == top_municipality & 
                                            factor(traffic_accidents$participants_status, levels = c(1, 2, 3), 
                                                   labels = c("Injured", "Killed", "Material Damage")) == "Injured", ]
accident_type_category_md <- factor(municipality_data_md$accident_type, levels = c(1:13), 
                                 labels = c("Angle", "Fall Of Road", "Head On Collision", "Rear End Collision",
                                            "Rear To Rear Collision", "Side-swipe", "Turnover", "With Animals",
                                            "With Obstacles", "With Parked Vehicle", "With Pedestrians", "With Rail Vehicle",
                                            "Unknown"))
accident_type_summary <- aggregate(case_number ~ accident_type_category_md, data = municipality_data_md, FUN = length)
colnames(accident_type_summary)[2] <- "count_accident_type"
accident_type_summary <- accident_type_summary[order(-accident_type_summary$count_accident_type), ]
head(accident_type_summary, 1)

# Interaktivni prikaz podataka na mapi najvise povredjenih
map_data <- traffic_accidents[
  traffic_accidents$municipality_name == top_municipality & 
    factor(traffic_accidents$participants_status, levels = c(1, 2, 3),
           labels = c("Injured", "Killed", "Material Damage")) == "Injured", 
]

map_data_sf <- st_as_sf(map_data, coords = c("longitude", "latitude"), crs = 4326)

leaflet(data = map_data_sf) %>%
  addTiles() %>%
  addHeatmap(lng = ~st_coordinates(geometry)[,1], 
             lat = ~st_coordinates(geometry)[,2], 
             blur = 20, max = 0.05, radius = 15)

# Opstina sa najvise nastradalih u saobracajnim nezgodama
killed_data <- traffic_accidents[ factor(traffic_accidents$participants_status, levels = c(1, 2, 3), 
                                          labels = c("Injured", "Killed", "Material Damage")) == "Killed", ]
killed_summary <- aggregate(case_number ~ municipality_name, data = killed_data, FUN = length)
colnames(killed_summary)[2] <- "count_killed"
killed_summary <- killed_summary[order(-killed_summary$count_killed), ]
head(killed_summary, 1)

# Najzastupleniji tip nezgode na izdvojenoj opstini
top_municipality <- killed_summary$municipality_name[1]
municipality_data_md <- traffic_accidents[traffic_accidents$municipality_name == top_municipality & 
                                            factor(traffic_accidents$participants_status, levels = c(1, 2, 3), 
                                                   labels = c("Injured", "Killed", "Material Damage")) == "Killed", ]
accident_type_category_md <- factor(municipality_data_md$accident_type, levels = c(1:13), 
                                    labels = c("Angle", "Fall Of Road", "Head On Collision", "Rear End Collision",
                                               "Rear To Rear Collision", "Side-swipe", "Turnover", "With Animals",
                                               "With Obstacles", "With Parked Vehicle", "With Pedestrians", "With Rail Vehicle",
                                               "Unknown"))
accident_type_summary <- aggregate(case_number ~ accident_type_category_md, data = municipality_data_md, FUN = length)
colnames(accident_type_summary)[2] <- "count_accident_type"
accident_type_summary <- accident_type_summary[order(-accident_type_summary$count_accident_type), ]
head(accident_type_summary, 1)

# Interaktivni prikaz podataka na mapi najvise povredjenih
map_data <- traffic_accidents[
  traffic_accidents$municipality_name == top_municipality & 
    factor(traffic_accidents$participants_status, levels = c(1, 2, 3),
           labels = c("Injured", "Killed", "Material Damage")) == "Killed", 
]

map_data_sf <- st_as_sf(map_data, coords = c("longitude", "latitude"), crs = 4326)

leaflet(data = map_data_sf) %>%
  addTiles() %>%
  addHeatmap(lng = ~st_coordinates(geometry)[,1], 
             lat = ~st_coordinates(geometry)[,2], 
             blur = 20, max = 0.05, radius = 15)

# Opstina sa najvise materijalne stete u saobracajnim nezgodama
material_damage_data <- traffic_accidents[ factor(traffic_accidents$participants_status, levels = c(1, 2, 3), 
                                         labels = c("Injured", "Killed", "Material Damage")) == "Material Damage", ]
material_damage_summary <- aggregate(case_number ~ municipality_name, data = material_damage_data, FUN = length)
colnames(material_damage_summary)[2] <- "count_material_damage"
material_damage_summary <- material_damage_summary[order(-material_damage_summary$count_material_damage), ]
head(material_damage_summary, 1)

# Najzastupleniji tip nezgode na izdvojenoj opstini
top_municipality <- material_damage_summary$municipality_name[1]
municipality_data_md <- traffic_accidents[traffic_accidents$municipality_name == top_municipality & 
                                            factor(traffic_accidents$participants_status, levels = c(1, 2, 3), 
                                                   labels = c("Injured", "Killed", "Material Damage")) == "Material Damage", ]
accident_type_category_md <- factor(municipality_data_md$accident_type, levels = c(1:13), 
                                    labels = c("Angle", "Fall Of Road", "Head On Collision", "Rear End Collision",
                                               "Rear To Rear Collision", "Side-swipe", "Turnover", "With Animals",
                                               "With Obstacles", "With Parked Vehicle", "With Pedestrians", 
                                               "With Rail Vehicle", "Unknown"))

accident_type_summary <- aggregate(case_number ~ accident_type_category_md, data = municipality_data_md, FUN = length)
colnames(accident_type_summary)[2] <- "count_accident_type"
accident_type_summary <- accident_type_summary[order(-accident_type_summary$count_accident_type), ]
head(accident_type_summary, 1)

# Interaktivni prikaz podataka na mapi sa najvise materijalne stete
map_data <- traffic_accidents[
  traffic_accidents$municipality_name == top_municipality & 
    factor(traffic_accidents$participants_status, levels = c(1, 2, 3),
           labels = c("Injured", "Killed", "Material Damage")) == "Material Damage", 
]

map_data_sf <- st_as_sf(map_data, coords = c("longitude", "latitude"), crs = 4326)

leaflet(data = map_data_sf) %>%
  addTiles() %>%
  addHeatmap(lng = ~st_coordinates(geometry)[,1], 
             lat = ~st_coordinates(geometry)[,2], 
             blur = 20, max = 0.05, radius = 15)

# Inferencijska statistika:
# municipality_name vs accident_type
traffic_accidents$accident_type_factor <- factor(
  traffic_accidents$accident_type, levels = c(1:13), 
  labels = c("Angle", "Fall Of Road", "Head On Collision", "Rear End Collision",
             "Rear To Rear Collision", "Side-swipe", "Turnover", "With Animals",
             "With Obstacles", "With Parked Vehicle", "With Pedestrians", 
             "With Rail Vehicle","Unknown"))

contingency_table <- table(traffic_accidents$municipality_name, traffic_accidents$accident_type_factor)

chisq_test_sim <- chisq.test(contingency_table, simulate.p.value = TRUE, B = 5000)

chisq_test_sim

# city_name vs accident_type
traffic_accidents_cities <- traffic_accidents[!is.na(traffic_accidents$city_name), ]

traffic_accidents_cities$accident_type_factor <- factor(
  traffic_accidents_cities$accident_type, 
  levels = c(1:13), 
  labels = c("Angle", "Fall Of Road", "Head On Collision", "Rear End Collision",
             "Rear To Rear Collision", "Side-swipe", "Turnover", "With Animals",
             "With Obstacles", "With Parked Vehicle", "With Pedestrians", 
             "With Rail Vehicle", "Unknown"))

contingency_table <- table(traffic_accidents_cities$city_name, traffic_accidents_cities$accident_type_factor)

chisq_test_sim <- chisq.test(contingency_table, simulate.p.value = TRUE, B = 5000)

chisq_test_sim

contingency_df <- as.data.frame(as.table(contingency_table))

ggplot(contingency_df, aes(Var2, Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradientn(
    colors = c("white", "lightpink", "red"),
    values = scales::rescale(c(0, 3000, 6000, max(contingency_df$Freq))),
    breaks = seq(0, max(contingency_df$Freq), by = 3000),
    limits = c(0, max(contingency_df$Freq))
  ) +
  labs(x = "Tip nezgode", y = "Naziv grada", fill = "Broj nezgoda") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Heatmap prikaz tipova nezgoda po gradovima")

# Pravila asocijacije
traffic_accidents$municipality_name <- as.factor(traffic_accidents$municipality_name)
traffic_accidents$participants_status <- factor(traffic_accidents$participants_status, levels = c(1, 2, 3), 
                                                labels = c("Injured", "Killed", "Material Damage"))
traffic_accidents$participants_nominal_count <- factor(traffic_accidents$participants_nominal_count, levels = c(1:5), 
                                                          labels = c("Single Vehicle", "At Least Two Vehicles", 
                                                                     "Parked Vehicle", "With Pedestrians", "Unknown"))

accident_transactions <- traffic_accidents[, 
              c("municipality_name", "participants_status", "participants_nominal_count")]

rules <- apriori(accident_transactions, parameter = list(support = 0.01, confidence = 0.8))

inspect(rules)

rules_sorted <- sort(rules, by="lift")

inspect(head(rules_sorted, n=10))
