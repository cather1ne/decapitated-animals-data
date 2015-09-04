library(data.table)

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

dat_tmp1 <- dat_tmp1[c(rep(1:nrow(dat_tmp1), times = 2))]
dat_tmp1 <- dat_tmp1[order(dat_tmp1$key), ]

for(i in 1:nrow(dat_tmp1)) {
  tmp_animal <- strsplit(dat_tmp1$animal[7], " & ")
}


