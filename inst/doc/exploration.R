## -----------------------------------------------------------------------------
library(basetable)

## -----------------------------------------------------------------------------
dims(iris)
types(iris)
describe(iris)
missingness(airquality)
freq(iris, "Species", prop = TRUE)

## -----------------------------------------------------------------------------
summarytab(
  transform(mtcars, am = factor(am, labels = c("Automatic", "Manual"))),
  vars = c("mpg", "hp"),
  by = "am",
  p_value = TRUE
)

## -----------------------------------------------------------------------------
compare(mtcars, transform(mtcars, mpg = mpg * 1.1))

