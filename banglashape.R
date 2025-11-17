library(ggplot2)
library(sp)
library(sf)
library(tidyverse)
library(ncdf4)
bangladish = st_read("mygeodata/gadm41_BGD_2-polygon.shp", quiet = TRUE)
bangladish
plot(bangladish, max.plot = 13)
plot(bangladish["NAME_1"])
plot(bangladish["NAME_2"])
ggplot(bangladish) + geom_sf(aes(fill = NAME_1))
colnames(bangladish)
bangladish$VARNAME_2 = runif(64, 0.3, 0.9)


ggplot() + 
  geom_sf(data = bangladish, size = 1.5, color = "black", fill = "cyan1") + 
  ggtitle("Bangladish") + 
  coord_sf(label_axes = waiver())

ggplot(bangladish) + geom_sf(aes(fill = VARNAME_2))

###################################################################################
# Assuming `bangladish` is the problematic sf object
# Check validity
invalid_geoms <- which(!st_is_valid(bangladish))

# Print the rows of invalid geometries
if (length(invalid_geoms) > 0) {
  print(invalid_geoms)
  
  # Attempt to fix invalid geometries
  bangladish <- st_make_valid(bangladish)
  
  # Verify that the geometries have been fixed
  invalid_geoms_after_fix <- which(!st_is_valid(bangladish))
  if (length(invalid_geoms_after_fix) == 0) {
    print("All geometries are now valid.")
  } else {
    print("There are still invalid geometries:")
    print(invalid_geoms_after_fix)
  }
} else {
  print("All geometries are valid.")
}

district_abbreviations <- c(
  "Barguna" = "BG", "Barisal" = "BS", "Bhola" = "BH", "Jhalokati" = "JK",
  "Patuakhali" = "PA", "Pirojpur" = "PR", "Bandarban" = "BD", "Brahamanbaria" = "BB",
  "Chandpur" = "CP", "Chittagong" = "CT", "Comilla" = "CO", "Cox'SBazar" = "CB",
  "Feni" = "FN", "Khagrachhari" = "KH", "Lakshmipur" = "LP", "Noakhali" = "NO",
  "Rangamati" = "RM", "Dhaka" = "DK", "Faridpur" = "FD", "Gazipur" = "GZ",
  "Gopalganj" = "GP", "Kishoreganj" = "KG", "Madaripur" = "MD", "Manikganj" = "MK",
  "Munshiganj" = "MS", "Narayanganj" = "NY", "Narsingdi" = "NR", "Rajbari" = "RB",
  "Shariatpur" = "SP", "Tangail" = "TL", "Bagerhat" = "BR", "Chuadanga" = "CD",
  "Jessore" = "JS", "Jhenaidah" = "JD", "Khulna" = "KL", "Kushtia" = "KS",
  "Magura" = "MG", "Meherpur" = "MP", "Narail" = "NL", "Satkhira" = "SK",
  "Jamalpur" = "JM", "Mymensingh" = "MY", "Netrakona" = "NK", "Sherpur" = "SR",
  "Bogra" = "BO", "Joypurhat" = "JP", "Naogaon" = "NG", "Natore" = "NT",
  "Nawabganj" = "NB", "Pabna" = "PB", "Rajshahi" = "RJ", "Sirajganj" = "SG",
  "Dinajpur" = "DJ", "Gaibandha" = "GB", "Kurigram" = "KR", "Lalmonirhat" = "LH",
  "Nilphamari" = "NP", "Panchagarh" = "PG", "Rangpur" = "RP", "Thakurgaon" = "TG",
  "Habiganj" = "HG", "Maulvibazar" = "MB", "Sunamganj" = "SN", "Sylhet" = "SL"
)

# Assign the abbreviations to the 'bangladish' data frame
bangladish$Abbreviation <- district_abbreviations[bangladish$NAME_2]

#bangladish = data.frame(bangladish)

labels <- cbind(bangladish, st_coordinates(st_centroid(bangladish$geometry)))
ggplot(bangladish) + 
  geom_sf(show.legend = FALSE) +
  geom_text(data = labels, aes(label = Abbreviation, x = X, y = Y), colour = "black", size = 4) +
  labs(title = "District Abbreviations in Bangladesh") +
  theme_minimal()+
  theme(
    axis.text.x = element_blank(),   # Remove x-axis text
    axis.text.y = element_blank(),   # Remove y-axis text
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.ticks = element_blank(),    # Remove axis ticks
    panel.grid.major = element_blank(),  # Remove grid lines
    panel.grid.minor = element_blank()
  )

##########################################################################################
bangla_frame = data.frame(bangladish)

summary = read.csv("results.csv")

# Number of missed case in years
num_years <- 10

districts_per_year <- 64  # Each year contains 64 districts
for (i in 1:num_years) {
  missed_cases = list()
  for (j in 1:districts_per_year){
    index <- (j-1) * num_years + i - 1 
    column_name <- paste("missed_cases_per100k[", index, "]", sep = "")
    missed_case = summary[summary$X == column_name, ]$mean
    missed_cases = append(missed_cases, missed_case)
  }
  output_name <- paste("Missed_cases", i, sep = "")
  bangla_frame[[output_name]] = missed_cases
}
#To plot overall missed cases in all districts over the years, we have the following code
average_missed_cases_per_district <- numeric(districts_per_year)


# Calculate the average for each district across all years
for (j in 1:districts_per_year) {
  # Extract the missed cases for the j-th district across all years
  district_cases <- sapply(1:num_years, function(x) bangla_frame[j, paste("Missed_cases", x, sep = "")])
  # Calculate the average of these cases
  average_missed_cases_per_district[j] <- mean(unlist(district_cases), na.rm = TRUE)
}
# Optionally, add these averages to bangla_frame or handle as needed
bangla_frame$Average_Missed_Cases <- average_missed_cases_per_district
print(average_missed_cases_per_district)

# To plot the missed cases per year
bangla_frame_long <- bangla_frame %>%
  pivot_longer(
    cols = starts_with("Missed_cases"), 
    names_to = "Year", 
    values_to = "Cases",
    names_prefix = "Missed_cases"
  )
bangla_frame_long$Cases = unlist(bangla_frame_long$Cases)
# Convert the Year into a more readable format if necessary
bangla_frame_long$Year <- factor(as.integer(bangla_frame_long$Year), labels = paste(2008:2017))
sammy = ggplot(bangla_frame_long, aes(fill= Cases, geometry = geometry)) + 
  geom_sf() + 
  facet_wrap(~Year) +  # Facet by year
  scale_fill_gradientn(colors = c("pink", "red", "darkred")) +
  labs(title = "Missed Cases by Year", fill = "Cases") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),   # Remove x-axis text
    axis.text.y = element_blank(),   # Remove y-axis text
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.ticks = element_blank(),    # Remove axis ticks
    panel.grid.major = element_blank(),  # Remove grid lines
    panel.grid.minor = element_blank()
  )
print(sammy)
ggsave("missed_cases_faceted.png", width = 20, height = 15, units = "cm")

ggplot(bangla_frame) + 
  geom_sf(aes(fill= Average_Missed_Cases, geometry = geometry)) + 
  geom_text(data = labels, aes(label = Abbreviation, x = X, y = Y), colour = "black", size = 4) +
  ggtitle("Overall missed cases") +
  scale_fill_gradientn(colors = c("pink", "red", "darkred")) +
  theme(
    axis.text.x = element_blank(),   # Remove x-axis text
    axis.text.y = element_blank(),   # Remove y-axis text
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.ticks = element_blank(),    # Remove axis ticks
    panel.grid.major = element_blank(),  # Remove grid lines
    panel.grid.minor = element_blank()
  )
ggsave("overall_missed_cases_faceted.png", width = 20, height = 15, units = "cm")

####################################################


# Number of incidence in years
num_years1 <- 10
districts_per_year1 <- 64  # Each year contains 64 districts

for (i in 1:num_years) {
  incidences = list()
  for (j in 1:districts_per_year){
    index <- (j-1) * num_years + i - 1 
    column_name <- paste("output_inc[0, ", index, "]", sep = "")
    inc = summary[summary$X == column_name, ]$mean
    incidences = append(incidences, inc)
  }
  output_name <- paste("Incidence", i, sep = "")
  bangla_frame[[output_name]] = incidences
}

#To plot overall incidence in all districts over the years, we have the following code
average_incidence_per_district <- numeric(districts_per_year1)

# Calculate the average for each district across all years
for (j in 1:districts_per_year1) {
  # Extract the incidence for the j-th district across all years
  district_inc <- sapply(1:num_years1, function(x) bangla_frame[j, paste("Incidence", x, sep = "")])
  # Calculate the average of these cases
  average_incidence_per_district[j] <- mean(unlist(district_inc), na.rm = TRUE)
}
# Optionally, add these averages to bangla_frame or handle as needed
bangla_frame$Average_Incidence <- average_incidence_per_district
print(average_incidence_per_district)

bangla_frame_long1 <- bangla_frame %>%
  pivot_longer(
    cols = starts_with("Incidence"), 
    names_to = "Year", 
    values_to = "Cases",
    names_prefix = "Incidence"
  )

bangla_frame_long1$Cases = unlist(bangla_frame_long1$Cases)

# Convert the Year into a more readable format if necessary
bangla_frame_long1$Year <- factor(as.integer(bangla_frame_long1$Year), labels = paste(2008:2017))
sammy1 = ggplot(bangla_frame_long1, aes(fill= Cases, geometry = geometry)) + 
  geom_sf() + 
  facet_wrap(~Year) +  # Facet by year
  scale_fill_gradientn(colors = c("pink", "red", "darkred")) +
  labs(title = "Incidence by Year", fill = "Cases") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),   # Remove x-axis text
    axis.text.y = element_blank(),   # Remove y-axis text
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.ticks = element_blank(),    # Remove axis ticks
    panel.grid.major = element_blank(),  # Remove grid lines
    panel.grid.minor = element_blank()
  )
print(sammy1)
ggsave("incidence_faceted.png", width = 20, height = 15, units = "cm")

ggplot(bangla_frame) + 
  geom_sf(aes(fill= Average_Incidence, geometry = geometry)) + 
  geom_text(data = labels, aes(label = Abbreviation, x = X, y = Y), colour = "black", size = 4) +
  ggtitle("Overall Incidence") +
  scale_fill_gradientn(colors = c("pink", "red", "darkred")) +
  theme(
    axis.text.x = element_blank(),   # Remove x-axis text
    axis.text.y = element_blank(),   # Remove y-axis text
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.ticks = element_blank(),    # Remove axis ticks
    panel.grid.major = element_blank(),  # Remove grid lines
    panel.grid.minor = element_blank()
  )
ggsave("overall_incidence_faceted.png", width = 20, height = 15, units = "cm")

####################################################


# Number of fractional treated cases in years
num_years2 <- 10
districts_per_year2 <- 64  # Each year contains 64 districts

for (i in 1:num_years) {
  pns = list()
  for (j in 1:districts_per_year){
    index <- (j-1) * num_years + i - 1 
    column_name <- paste("output_pn[0, ", index, "]", sep = "")
    pn = summary[summary$X == column_name, ]$mean
    pns = append(pns, pn)
  }
  output_name <- paste("Fractional Treated", i, sep = "")
  bangla_frame[[output_name]] = pns
}

#To plot overall fractional treated in all districts over the years, we have the following code
average_pn_per_district <- numeric(districts_per_year2)

# Calculate the average for each district across all years
for (j in 1:districts_per_year2) {
  # Extract the incidence for the j-th district across all years
  district_pn <- sapply(1:num_years2, function(x) bangla_frame[j, paste("Fractional Treated", x, sep = "")])
  # Calculate the average of these cases
  average_pn_per_district[j] <- mean(unlist(district_pn), na.rm = TRUE)
}
# Optionally, add these averages to bangla_frame or handle as needed
bangla_frame$Average_Pn <- average_pn_per_district
print(average_pn_per_district)

# To plot the fractional treated in years...
bangla_frame_long2 <- bangla_frame %>%
  pivot_longer(
    cols = starts_with("Fractional Treated"), 
    names_to = "Year", 
    values_to = "Fraction",
    names_prefix = "Fractional Treated"
  )
bangla_frame_long2$Fraction = unlist(bangla_frame_long2$Fraction)
# Convert the Year into a more readable format if necessary
bangla_frame_long2$Year <- factor(as.integer(bangla_frame_long2$Year), labels = paste(2008:2017))
sammy2 = ggplot(bangla_frame_long2, aes(fill= Fraction, geometry = geometry)) + 
  geom_sf() + 
  facet_wrap(~Year) +  # Facet by year
  scale_fill_gradientn(colors = c("pink", "red", "darkred")) +
  labs(title = "Fractional Treated by Year", fill = "Fraction") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),   # Remove x-axis text
    axis.text.y = element_blank(),   # Remove y-axis text
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.ticks = element_blank(),    # Remove axis ticks
    panel.grid.major = element_blank(),  # Remove grid lines
    panel.grid.minor = element_blank()
  )
print(sammy2)
ggsave("Fractional_Treated_faceted.png", width = 20, height = 15, units = "cm")


ggplot(bangla_frame) + 
  geom_sf(aes(fill= Average_Pn, geometry = geometry)) + 
  geom_text(data = labels, aes(label = Abbreviation, x = X, y = Y), colour = "black", size = 4) +
  ggtitle("Overall Fractional Treated") +
  scale_fill_gradientn(colors = c("pink", "red", "darkred")) +
  theme(
    axis.text.x = element_blank(),   # Remove x-axis text
    axis.text.y = element_blank(),   # Remove y-axis text
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.ticks = element_blank(),    # Remove axis ticks
    panel.grid.major = element_blank(),  # Remove grid lines
    panel.grid.minor = element_blank()
  )
ggsave("overall_Fractional_Treated_faceted.png", width = 20, height = 15, units = "cm")

ggplot(bangla_frame, aes(x = Abbreviation, y = Average_Missed_Cases)) +
  geom_bar(stat = "identity", fill = "red", width = 0.7, show.legend = FALSE) +
  labs(title = "", y = "Missed cases", x = "Districts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("missed_cases_bar.png", width = 20, height = 15, units = "cm")


for (i in 1:num_years) {
  deaths = list()
  for (j in 1:districts_per_year){
    index <- (j-1) * num_years + i - 1 
    column_name <- paste("deaths[", index, "]", sep = "")
    death = summary[summary$X == column_name, ]$mean
    deaths = append(deaths, death)
  }
  output_name <- paste("deaths", i, sep = "")
  bangla_frame[[output_name]] = deaths
}
average_deaths_per_district = numeric(64)
for (j in 1:districts_per_year) {
  # Extract the missed cases for the j-th district across all years
  district_deaths <- sapply(1:num_years, function(x) bangla_frame[j, paste("deaths", x, sep = "")])
  # Calculate the average of these cases
  average_deaths_per_district[j] <- mean(unlist(district_deaths), na.rm = TRUE)
}
# Optionally, add these averages to bangla_frame or handle as needed
bangla_frame$average_death <- average_deaths_per_district



ggplot(bangla_frame, aes(x = reorder(Abbreviation, -average_death), y = average_death)) +
  geom_bar(stat = "identity", fill = "red", width = 0.7, show.legend = FALSE) +
  labs(title = "", y = "Number of Deaths", x = "Districts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("deaths.png", width = 20, height = 15, units = "cm")
