
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

  db <- create_pm5_database(my_files)

  workout_key <- paste(db$workouts$date, db$workouts$time_of_day)
  expect_true(!any(duplicated(workout_key)))

  splits_key <- paste(db$splits$date, db$splits$time_of_day)
  expect_true(all(splits_key %in% workout_key))

})