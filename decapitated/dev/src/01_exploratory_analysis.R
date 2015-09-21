library(data.table)
library(ggplot2)
library(maps)
library(RColorBrewer)
library(ggmap)
library(RgoogleMaps)

# STEP 01: Import data --------------------------------------------------------
dat <- data.table(read.csv("data/base/animals.csv", stringsAsFactors = F))

# STEP 02: Clean data ---------------------------------------------------------

# clean dates
dat$date_started <- as.Date(dat$date_started, "%m/%d/%Y")
dat$date_closed  <- as.Date(dat$date_closed, "%m/%d/%Y")

dat$resolution_action_updated[1] <- paste0(dat$resolution_action_updated[1], " 00:00:00")

dat$resolution_action_updated <- as.POSIXct(strptime(dat$resolution_action_updated,
                                                     "%m/%d/%Y %H:%M:%S"))

# give an unique key to each report
dat$key <- 1:nrow(dat)
colorder <- c(names(dat)[ncol(dat)], names(dat)[1:(ncol(dat) - 1)])
setcolorder(dat, colorder)

# each animal species should have it's own entry
dat_tmp1 <- dat[grep("&", dat$animal), ]
dat_tmp1 <- rbind(dat_tmp1, dat[grep(",", dat$animal), ])
dat_tmp2 <- dat[setdiff(dat$key, dat_tmp1$key), ]

dat_tmp1$animal <- gsub(", ", " & ", dat_tmp1$animal)
dat_tmp1$animal <- gsub("and", " & ", dat_tmp1$animal)
parenthesis_tmp <- gsub("[\\(\\)]", "", regmatches(dat_tmp1$animal, gregexpr("\\(.*?\\)", dat_tmp1$animal)))
dat_tmp1$animal[which((parenthesis_tmp != "character0") == TRUE)] <- parenthesis_tmp[parenthesis_tmp != "character0"]

# rearranging
dat_tmp2 <- rbind(dat_tmp2, dat_tmp1[dat_tmp1$key == 33, ], dat_tmp1[dat_tmp1$key == 44, ])
dat_tmp1 <- dat_tmp1[!dat_tmp1$key %in% dat_tmp2$key, ]
dat_tmp1 <- rbind(dat_tmp1, dat_tmp2[grep("[0-9]", dat_tmp2$body_part_found)])
dat_tmp2 <- dat_tmp2[!dat_tmp2$key %in% dat_tmp1$key, ]

dat_tmp1$animal[grep("[0-9]", dat_tmp1$body_part_found)] <- "Squirrels & Squirrels"

rep_num <- c()
animals <- c()
for(i in 1:nrow(dat_tmp1)) {
  tmp_animal <- strsplit(dat_tmp1$animal[i], " & ")
  animals <- c(animals, tmp_animal[[1]])
  rep_num <- c(rep_num, rep(i, times = length(tmp_animal[[1]])))
}

dat_tmp1 <- dat_tmp1[c(rep_num)]
dat_tmp1$animal <- animals

rm(rep_num, animals, parenthesis_tmp)

# manually review complains for quantities & body parts
animal_quant <- c(1,1,3,1,2,3,1,1,1,1,1,6,1)
animal_body_parts <- c("Head and Body",
                       "Head and Body",
                       "Body Without Head",
                       "Head",
                       "Body Without Head",
                       "Body Without Head",
                       "Head",
                       "Whole Body",
                       "Head & Rib Cage",
                       "Whole Body",
                       "Not Specified",
                       "Whole Body",
                       "Body Without Head")

dat_tmp1$quantity <- animal_quant
dat_tmp1$body_part_found <- animal_body_parts

rm(animal_quant, animal_body_parts)

# combine the cleaned sets into one
dat_v1 <- rbind(dat_tmp1, dat_tmp2)

dat_v1$animal[dat_v1$animal == " Other animals"] <- "Unknown"
dat_v1$animal[dat_v1$animal == "Not specified"]  <- "Unknown"
dat_v1$animal[dat_v1$animal == "Goat (Unsure)"]  <- "Unknown"

rm(dat_tmp1, dat_tmp2)

# recode animal types
dat_v1$animal_type <- ifelse(dat_v1$animal %in% c("Dove", "Chicken", "Pigeon",
                                                  "Hens", "Pigeons", "Rooster",
                                                  "Roosters", "Rooster ",
                                                  "Birds", "Bird", "Goose", "Duck",
                                                  "Turkey or Chicken", "Pigeon & Rooster",
                                                  "Eagle & Hawk & Pigeon"), "Bird", dat_v1$animal)
dat_v1$animal_type <- ifelse(dat_v1$animal_type %in% c("Goat", "Pig", "Squirrels", "Cat",
                                                  "Lamb", "Dog", "Cow"), "Mammal", dat_v1$animal_type)
dat_v1$animal_type <- ifelse(dat_v1$animal_type %in% c("Turtle"), "Reptile", dat_v1$animal_type)

dat_v1$report_year <- substr(dat_v1$date_started, 1, 4)

# recode body parts
dat_v1$body_part_type <- ifelse(dat_v1$body_part_found %in% c("Head & Rib Cage",
                                                              "Head, Legs, Body parts separated",
                                                              "Head & Intestines"),
                                "Head & Others", dat_v1$body_part_found)
dat_v1$body_part_type <- tolower(dat_v1$body_part_type)

dat_v1$animal_type <- as.factor(dat_v1$animal_type)
dat_v1$body_part_type <- as.factor(dat_v1$body_part_type)

write.csv(dat_v1, "data/processed/dat_v1.csv", row.names = F)

# STEP 03: Visualize ----------------------------------------------------------
# Graph animal types over time
tmp <- dat_v1[, .(animal_count = sum(quantity)),
              by = .(report_year, animal_type)]
tmp <- tmp[order(tmp$animal_type, tmp$report_year), ]

write.csv(tmp, "data/processed/01_(animal counts by year and type).csv", row.names = F)

ggplot(data = tmp, aes(x = report_year,  y = animal_count, fill = animal_type)) +
  geom_bar(stat = 'identity') + ggtitle("decapitation of animal types over the years")

# Graph animal types vs. body_part types
tmp <- dat_v1[, .(animal_count = sum(quantity)),
              by = .(animal_type, body_part_type)]
tmp <- tmp[order(tmp$animal_type, tmp$body_part_type), ]

write.csv(tmp, "data/processed/02_(animal counts by body parts and animal type).csv", row.names = F)

ggplot(data = tmp, aes(x = body_part_type,  y = animal_count, fill = animal_type)) +
  geom_bar(stat = 'identity') + ggtitle("body parts of animals") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Body part types over the years
tmp <- dat_v1[, .(animal_count = sum(quantity)),
              by = .(report_year, body_part_type)]
tmp <- tmp[order(tmp$body_part_type, tmp$report_year), ]

write.csv(tmp, "data/processed/03_(animal counts by year and body parts).csv", row.names = F)

ggplot(data = tmp, aes(x = report_year,  y = animal_count, fill = body_part_type)) +
  geom_bar(stat = 'identity') + ggtitle("body parts of animals over the years") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Map lat long
map <- get_map(location = c(lon = mean(dat_v1$lng), lat = mean(dat_v1$lat)), zoom = 10,
               maptype = "roadmap", scale = 2, color = 'bw')
ggmap(map) + geom_point(data = dat_v1, aes(x = lng, y = lat, colour = animal_type, size = quantity)) +
  scale_size_continuous(range = c(3, 10), breaks = c(1, 3, 6, 12))

map <- get_map(location = c(lon = mean(dat_v1$lng), lat = mean(dat_v1$lat)), zoom = 10,
                      maptype = "roadmap", scale = 2, color = 'bw')
ggmap(map) + geom_point(data = dat_v1, aes(x = lng, y = lat, colour = body_part_type, size = quantity)) +
  scale_size_continuous(range = c(3, 10), breaks = c(1, 3, 6, 12))

# Map count by city
tmp <- dat_v1[, .(animal_count = sum(quantity)),
              by = .(site_borough)]
tmp1 <- dat_v1[, .(animal_count = sum(quantity)),
               by = .(site_borough, report_year)]
tmp1 <- tmp1[order(tmp1$report_year, tmp1$site_borough), ]

write.csv(tmp1, "data/processed/05_(animal count by year and city).csv", row.names = F)

ggplot(data = tmp1, aes(x = report_year,  y = animal_count, fill = site_borough)) +
  geom_bar(stat = 'identity') + ggtitle("body parts of animals over the years") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tmp <- dat_v1[, .(case_count = .N),
              by = .(site_borough)]
tmp1 <- dat_v1[, .(case_count = .N),
               by = .(site_borough, report_year)]
tmp1 <- tmp1[order(tmp1$report_year, tmp1$site_borough), ]

write.csv(tmp1, "data/processed/05_(case count by year and city).csv", row.names = F)

# STEP 04: Analysis -----------------------------------------------------------

