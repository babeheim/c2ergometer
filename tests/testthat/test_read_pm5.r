
test_that("one file loads", {

  expect_silent(d <- read_pm5("raw-data/LogBook 2015-04-27.csv"))
  expect_true(length(d) == 2)
  expect_identical(names(d), c("workouts", "splits"))

})

test_that("every file loads", {

  my_files <- list.files("raw-data", full.names = TRUE)
  for (i in 1:length(my_files)) {
    expect_silent(d <- read_pm5(my_files[i]))
    expect_true(length(d) == 2)
    expect_identical(names(d), c("workouts", "splits"))
  }

})


test_that("can create a database", {

  my_files <- list.files("raw-data", full.names = TRUE)
  for (i in 1:length(my_files)) {
    d <- read_pm5(my_files[i])
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
      if (length(drop) > 0) d$workouts <- d$workouts[-drop,]

      if (nrow(d$workouts) > 0) workouts <- bind_rows(workouts, d$workouts)
      if (nrow(d$splits) > 0)splits <- bind_rows(splits, d$splits)
    }
  }

  workout_key <- paste(workouts$date, workouts$time_of_day)
  expect_true(!any(duplicated(workout_key)))

  splits_key <- paste(splits$date, splits$time_of_day)
  expect_true(all(splits_key %in% workout_key))

})