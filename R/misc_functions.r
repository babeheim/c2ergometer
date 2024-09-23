id_maker <- function(n, reserved='', seed=NA, nchars=NA){
    my_let <- letters 
    my_num <- 0:9 
    if(is.na(seed) | !is.numeric(seed)) set.seed(as.numeric(as.POSIXlt(Sys.time())))
    if(!is.na(seed) & is.numeric(seed)) set.seed(seed)
    output <- replicate(n, paste(sample(c(my_let, my_num), nchars, replace=TRUE), 
        collapse=''))
    rejected <- duplicated(output) | output %in% reserved | substr(output, 1, 1) %in% my_num
    while(any(rejected)){
        output <- output[-which(rejected)]
        remaining <- n-length(output)
        output <- c(output, replicate(remaining, paste(sample(c(my_let, my_num), nchars, 
            replace=TRUE), collapse='')))
        rejected <- duplicated(output) | output %in% reserved | substr(output, 1, 1) %in% my_num
    }
    output
}

time_to_seconds <- function(time) {
  if(length(time)>1){
    seconds <- sapply(time, time_to_seconds)
  } else {
    t <- strsplit(as.character(time), " |:")[[1]]
    seconds <- NaN
    if (length(t) == 1 )
      seconds <- as.numeric(t[1])
    else if (length(t) == 2)
      seconds <- as.numeric(t[1]) * 60 + as.numeric(t[2])
    else if (length(t) == 3)
      seconds <- (as.numeric(t[1]) * 60 * 60 
        + as.numeric(t[2]) * 60 + as.numeric(t[3]))   
    else if (length(t) == 4)
      seconds <- (as.numeric(t[1]) * 24 * 60 * 60 +
        as.numeric(t[2]) * 60 * 60  + as.numeric(t[3]) * 60 +
        as.numeric(t[4]))
  }
  return(seconds)
}

read_pm5 <- function(data) {

  c2u_version <- readLines(data)[1]
  c2u_version <- gsub(",", "", c2u_version)
  c2u_version <- gsub("Concept2 Utility - Version ", "", c2u_version)

  if (c2u_version == "6.97") {
    raw_data <- read.csv(data, skip = 5, stringsAsFactors = FALSE, header = TRUE, sep = ",")
  } else if (c2u_version == "7.05.3") {
    raw_data <- read.csv(data, skip = 4, stringsAsFactors = FALSE, header = TRUE, sep = ",")
  } else {
    raw_data <- read.csv(data, skip = 3, stringsAsFactors = FALSE, header = TRUE, sep = ",")
  }

  drop <- which(raw_data$Date == "")
  if (length(drop) > 0) raw_data <- raw_data[-drop, ]

  year_code <- nchar(strsplit(raw_data$Date[1], "/")[[1]][3])
  if (!is.na(year_code) & year_code == 2) {
    raw_data$Date <- as.Date(raw_data$Date, "%m/%d/%y")
  } else if (!is.na(year_code) & year_code == 4) {
    raw_data$Date <- as.Date(raw_data$Date, "%m/%d/%Y")
  } else {
    raw_data$Date <- as.Date(raw_data$Date)
  }

  raw_data$Time.of.Day %>% strptime("%H:%M") %>% format("%H:%M") -> raw_data$Time.of.Day

  name_present <- colnames(raw_data)[1] == "Name"

  if (name_present) {
    header_cols <- c("Name", "Date", "Time.of.Day", "Workout.Name")
  } else {
    header_cols <- c("Date", "Time.of.Day", "Workout.Name")
  }

  workout_cols <- c("Time", "Meters", "Avg.SPM", "Avg.Heart.Rate")
  split_cols <- c("Time.1", "Meters.1", "SPM", "Heart.Rate")

  workouts <- raw_data[, c(header_cols, workout_cols)]
  drop <- which(workouts$Time == "")
  if (length(drop) > 0) workouts <- workouts[-drop,]

  splits <- raw_data[, c(header_cols, split_cols)]
  drop <- which(splits$Time.1 == "")
  splits <- splits[-drop,]

  workouts$Time <- time_to_seconds(workouts$Time)
  splits$Time.1 <- time_to_seconds(splits$Time.1)

  workouts$workout_type <- "Fixed Time"
  workouts$workout_type[grep("m", workouts$`Workout.Name`)] <- "Fixed Distance"
  workouts$workout_type[grep("v", workouts$`Workout.Name`)] <- "Complex"
  workouts$workout_type[which(workouts$workout_type == "Fixed Time" & workouts$Time < 6 * 60)] <- "Warmup"

  workouts <- dplyr::select(workouts,
    date = Date,
    time_of_day = `Time.of.Day`,
    workout_type = workout_type,
    time = Time,
    distance = Meters,
    stroke_rate = `Avg.SPM`,
    heart_rate = Avg.Heart.Rate
  )

  splits <- dplyr::select(splits,
    date = Date,
    time_of_day = Time.of.Day,
    time = Time.1,
    distance = Meters.1,
    stroke_rate = `SPM`,
    heart_rate = Heart.Rate
  )

  # there wasn't enough time in this split to calculate an spm
  splits$stroke_rate[splits$stroke_rate == "0"] <- NA
  splits$heart_rate[splits$heart_rate == "0"] <- NA
  workouts$heart_rate[workouts$heart_rate == "0"] <- NA

  workouts$stroke_rate <- as.numeric(workouts$stroke_rate)
  splits$stroke_rate <- as.numeric(splits$stroke_rate)

  workouts$distance <- as.numeric(workouts$distance)
  splits$distance <- as.numeric(splits$distance)

  for (i in 1:nrow(workouts)) {

    workout_key <- paste(workouts$date, workouts$time_of_day)
    splits_key <- paste(splits$date, splits$time_of_day)

    tar <- which(splits_key == workout_key[i])

    time_summed <- abs(max(splits$time[tar]) - workouts$time[i]) < 0.3
    if (time_summed & length(tar) > 1) {
      splits$time[tar] <- diff(c(0, splits$time[tar]))
    }

    dist_summed <- abs(max(splits$distance[tar]) - workouts$distance[i]) < 1
    if (dist_summed & length(tar) > 1) {
      splits$distance[tar] <- diff(c(0, splits$distance[tar]))
    }

  }

  out <- list(
    workouts = workouts,
    splits = splits
  )

  return(out)

}

create_pm5_database <- function(files) {
  # load a set of csvs and combine
  for (i in 1:length(files)) {
    d <- read_pm5(files[i])
    if (i == 1) {
      workouts <- d$workouts
      splits <- d$splits
    } else {
      existing_entries <- paste(workouts$date, workouts$time_of_day)
      workout_candidates <- paste(d$workouts$date, d$workouts$time_of_day)
      drop <- which(workout_candidates %in% existing_entries)
      if (length(drop) > 0) d$workouts <- d$workouts[-drop,]
      split_candidates <-  paste(d$split$date, d$split$time_of_day)
      drop <- which(split_candidates %in% existing_entries)
      if (length(drop) > 0) d$splits <- d$splits[-drop,]
      if (nrow(d$workouts) > 0) workouts <- bind_rows(workouts, d$workouts)
      if (nrow(d$splits) > 0) splits <- bind_rows(splits, d$splits)
    }
  }
  out <- list(workouts = workouts, splits = splits)
  return(out)
}
