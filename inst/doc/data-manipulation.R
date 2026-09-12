## -----------------------------------------------------------------------------
library(basetable)

## -----------------------------------------------------------------------------
mtcars |>
  subset(cyl >= 6, select = c("mpg", "hp", "wt", "cyl")) |>
  transform(power = hp / wt) |>
  orderrows(by = c("cyl", "mpg"), decreasing = c(FALSE, TRUE))

## -----------------------------------------------------------------------------
orderrows(
  mtcars,
  by = c("cyl", "mpg"),
  decreasing = c(FALSE, TRUE)
) |>
  firstrows(6)

## -----------------------------------------------------------------------------
scorecars <- function(data, horsepowerweight = 0.7) {
  data |>
    transform(
      weightedhp = hp * horsepowerweight,
      score = weightedhp / wt
    ) |>
    orderrows("score", decreasing = TRUE)
}

scorecars(mtcars) |>
  pick(c("mpg", "hp", "wt", "score")) |>
  firstrows(5)

## -----------------------------------------------------------------------------
aggregate(airquality, by = "Month", value = c("Ozone", "Temp"), fun = mean, na.rm = TRUE)

## -----------------------------------------------------------------------------
summaries(
  airquality,
  ozone = mean(Ozone, na.rm = TRUE),
  temperature = mean(Temp, na.rm = TRUE),
  days = length(Temp),
  by = "Month"
)

## -----------------------------------------------------------------------------
merge(
  data.frame(id = 1:3, x = letters[1:3]),
  data.frame(id = c(2, 3, 4), y = LETTERS[2:4]),
  by = "id",
  all = TRUE
)

## -----------------------------------------------------------------------------
cars <- mtcars |>
  transform(car = rownames(mtcars)) |>
  firstcols("car")

assertcomplete(cars, c("car", "mpg", "cyl"))
assertunique(cars, "car")
assertrows(cars, mpg > 0)

## -----------------------------------------------------------------------------
mtcars |>
  basetable::subset(cyl == 6) |>
  basetable::pick(c("mpg", "hp", "wt")) |>
  basetable::transform(power = hp / wt)

