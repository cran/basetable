## ----setup------------------------------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE)
library(basetable)
.old_opts <- options(width = 90, digits = 4)

## ---------------------------------------------------------------------------------------
subset(mtcars, cyl == 6, select = c("mpg", "hp", "wt"))

## ---------------------------------------------------------------------------------------
subset(mtcars, mpg > mean(mpg), by = "cyl")

## ---------------------------------------------------------------------------------------
pick(mtcars, c("mpg", "hp"))
drop(mtcars, c("vs", "am"))

## ---------------------------------------------------------------------------------------
transform(mtcars, power = hp / wt, power_sq = power^2)

## ---------------------------------------------------------------------------------------
within(mtcars, {
  power <- hp / wt
  heavy <- wt > 3.5
})

## ---------------------------------------------------------------------------------------
mtcars |>
  subset(cyl == 6 & mpg > 18, select = c("mpg", "cyl", "hp")) |>
  transform(ratio = hp / mpg) |>
  orderrows(by = "ratio", decreasing = TRUE)

## ---------------------------------------------------------------------------------------
transform(mtcars, ratio = hp / mpg, .keep = FALSE)
renamecols(mtcars, horsepower = hp) |> pick(c("horsepower", "mpg"))

## ---------------------------------------------------------------------------------------
aggregate(mtcars, by = "cyl", value = c("mpg", "hp"), fun = mean)

## ---------------------------------------------------------------------------------------
count(iris, by = "Species")
propcount(mtcars, by = c("cyl", "am"), margin = "cyl")

## ---------------------------------------------------------------------------------------
summaries(mtcars, "cyl", mean_hp = mean(hp), sd_hp = sd(hp))

## ---------------------------------------------------------------------------------------
pieces <- split(iris, by = "Species")
names(pieces)
pieces$setosa |> headtail(2)

## ---------------------------------------------------------------------------------------
applyby(mtcars, by = "cyl", fun = function(d) data.frame(mean_mpg = mean(d$mpg)), bind = TRUE, id = "cyl_group")

## ---------------------------------------------------------------------------------------
df <- data.frame(id = c(1, 1, 2, 2, 2), visit = c(1, 2, 1, 2, 3), value = c(10, 12, 5, 6, 7))
firstby(df, by = "id")
lastby(df, by = "id")

## ---------------------------------------------------------------------------------------
orders <- data.frame(id = c(1, 2, 3), customer = c("a", "b", "c"))
payments <- data.frame(id = c(2, 3, 4), amount = c(50, 75, 20))

merge(orders, payments, by = "id")
merge(orders, payments, by = "id", all.x = TRUE)

## ---------------------------------------------------------------------------------------
semimerge(orders, payments, by = "id")
antimerge(orders, payments, by = "id")

## ---------------------------------------------------------------------------------------
matchedkeys(orders, payments, by = "id")
unmatchedkeys(orders, payments, by = "id")
joinrelationship(orders, payments, by = "id")

## ---------------------------------------------------------------------------------------
crossmerge(data.frame(size = c("S", "M")), data.frame(color = c("red", "blue")))

## ---------------------------------------------------------------------------------------
trades <- data.frame(id = 1L, time = c(5, 10, 15))
quotes <- data.frame(id = 1L, time = c(3, 8, 14), price = c(100, 101, 99))

rollingmerge(trades, quotes, by = c("id", "time"), direction = "backward")
nearestmerge(trades, quotes, by = c("id", "time"))

## ---------------------------------------------------------------------------------------
events <- data.frame(id = 1L, start = c(1, 10), end = c(5, 15))
windows <- data.frame(id = 1L, start = c(0, 8), end = c(6, 20), label = c("early", "late"))

overlapmerge(events, windows, startx = "start", endx = "end", starty = "start", endy = "end", by = "id")

## ---------------------------------------------------------------------------------------
current <- data.frame(id = c(1, 2, 3), status = c("pending", "pending", "pending"))
updates <- data.frame(id = c(2, 3), status = c("shipped", "cancelled"))

updatemerge(current, updates, by = "id")

## ---------------------------------------------------------------------------------------
events <- data.frame(id = 1, date = as.Date("2024-01-15"))
periods <- data.frame(
  id = 1,
  start_date = as.Date(c("2024-01-01", "2024-02-01")),
  end_date = as.Date(c("2024-01-31", "2024-02-28")),
  period = c("Jan", "Feb")
)
nonequimerge(events, periods, by = c("id", "date>=start_date", "date<=end_date"))

## ---------------------------------------------------------------------------------------
ranges <- data.frame(id = c(1, 1, 2), lower = c(0, 0, 0), upper = c(10, 10, 20))
points <- data.frame(id = c(1, 1, 2), val = c(5, 15, 5), label = c("a", "b", "c"))
rangemerge(ranges, points, by = "id", lower = "lower", upper = "upper", value = "val")

## ---------------------------------------------------------------------------------------
a <- data.frame(id = c(1, 2, 3))
b <- data.frame(id = c(2, 3, 4))

unionrows(a, b)
intersectrows(a, b, by = "id")
diffrows(a, b, by = "id")

## ---------------------------------------------------------------------------------------
equalrows(a, data.frame(id = c(3, 2, 1)), by = "id")
equaldata(a, a[nrow(a):1, , drop = FALSE], ignoreorder = TRUE)
sameschema(mtcars, mtcars)
compareschema(mtcars, iris)

## ---------------------------------------------------------------------------------------
old <- data.frame(id = c(1, 2, 3), status = c("a", "b", "c"))
new <- data.frame(id = c(2, 3, 4), status = c("b", "c2", "d"))

addedrows(old, new, by = "id")
removedrows(old, new, by = "id")
changedrows(old, new, by = "id")

## ---------------------------------------------------------------------------------------
rbindfill(data.frame(x = 1), data.frame(x = 2, y = 3))
cbind(data.frame(a = 1:2), data.frame(b = 3:4))

## ---------------------------------------------------------------------------------------
wide <- data.frame(id = 1:2, jan = c(10, 20), feb = c(11, 22))
long <- tolong(wide, cols = c("jan", "feb"), names = "month", values = "sales")
long
towide(long, names = "month", values = "sales", idcols = "id", fun = sum)

## ---------------------------------------------------------------------------------------
wide2 <- data.frame(id = 1:2, t1 = c(10, 20), t2 = c(11, 22))
reshape(wide2, varying = c("t1", "t2"), v.names = "value", timevar = "time", idvar = "id", direction = "long")

stack(data.frame(a = 1:2, b = 3:4))

## ---------------------------------------------------------------------------------------
codes <- data.frame(code = c("A-1-red", "B-2-blue"))
parts <- separate(codes, column = "code", into = c("letter", "num", "color"), sep = "-")
parts
unite(parts, column = "code", cols = c("letter", "num", "color"), sep = "_")

## ---------------------------------------------------------------------------------------
transpose(data.frame(a = 1:2, b = 3:4))

## ---------------------------------------------------------------------------------------
dims(iris)
types(iris)

## ---------------------------------------------------------------------------------------
headtail(iris, 2)
preview(iris)

## ---------------------------------------------------------------------------------------
describe(iris)

## ---------------------------------------------------------------------------------------
freq(iris, column = "Species")
freq(mtcars, column = "cyl", by = "am", prop = TRUE)

## ---------------------------------------------------------------------------------------
dat <- transform(mtcars, am = factor(am, labels = c("Automatic", "Manual")))
summarytab(dat, vars = c("mpg", "cyl"), by = "am", p_value = TRUE)

## ---------------------------------------------------------------------------------------
with_na <- transform(iris, Sepal.Length = ifelse(Sepal.Length > 7, NA, Sepal.Length))
missingness(with_na)

## ---------------------------------------------------------------------------------------
compare(iris, with_na, by = "Species")

## ---------------------------------------------------------------------------------------
mtcars |>
  assertnames(c("mpg", "cyl")) |>
  assertrows(mpg > 0) |>
  assertrange("mpg", lower = 0, upper = 60) |>
  asserttype("cyl", "numeric") |>
  assertcomplete() |>
  headtail(2)

## ---------------------------------------------------------------------------------------
tryCatch(assertkey(mtcars, "cyl"), error = function(e) conditionMessage(e))
tryCatch(assertunique(mtcars, "cyl"), error = function(e) conditionMessage(e))
tryCatch(assertvalues(mtcars, "am", allowed = c(0, 1)), error = function(e) "passes")

## ---------------------------------------------------------------------------------------
assert_cols(mtcars, c("mpg", "cyl")) |> invisible()
tryCatch(assert_key(mtcars, "cyl"), error = function(e) conditionMessage(e))

## ---------------------------------------------------------------------------------------
invalidrows(mtcars, mpg > 15)
outofrange(mtcars, "mpg", lower = 15, upper = 30)

## ---------------------------------------------------------------------------------------
d <- data.frame(a = c(1, NA, 3), b = c(NA, NA, 3))
missingrows(d, mode = "any")
keepmissing(d)
omitmissing(d, mode = "any")
missingindicator(d)

## ---------------------------------------------------------------------------------------
sparse <- data.frame(site = c("A", "A", "B"), year = c(2020, 2021, 2020), value = c(1, 2, 3))
completegrid(sparse, cols = c("site", "year"), fill = list(value = 0))

expandrows(data.frame(x = c(1, 2)), times = c(2, 1))

## ---------------------------------------------------------------------------------------
naif(c(1, 99, 2), 99)
nato(c(1, NA, 2), 0)
blanktona(c("a", "", "b"))
replacevalues(c("a", "b", "c"), old = c("a", "b"), new = c("A", "B"))
replacewhere(mtcars, cyl == 4, cols = "mpg", value = NA) |> headtail(2)

## ---------------------------------------------------------------------------------------
dup_df <- data.frame(id = c(1, 1, 2), v = c("a", "b", "c"))
uniquerows(dup_df, cols = "id")
removeduplicates(dup_df, by = "id", keep = "first")
duplicaterows(data.frame(x = c(1, 1, 2)))
duplicatekeys(dup_df, "id")

## ---------------------------------------------------------------------------------------
duplicated_keys(dup_df, "id")

## ---------------------------------------------------------------------------------------
messy <- data.frame(`First Name` = 1, `2nd_col` = 2, check.names = FALSE)
cleannames(messy)
commonnames(mtcars, iris)

## ---------------------------------------------------------------------------------------
common_names(mtcars, iris)

## ---------------------------------------------------------------------------------------
visits <- data.frame(
  id = c(1, 1, 1, 2, 2),
  visit = c(1, 2, 3, 1, 2),
  treatment = c("A", NA, NA, NA, "B")
)
filldown(visits, cols = "treatment", by = "id")
fillup(visits, cols = "treatment", by = "id")
fillboth(visits, cols = "treatment", by = "id")

## ---------------------------------------------------------------------------------------
colnames(iris)
classes(iris)
uniques(iris)
cardinality(iris, cols = "Species")
constants(data.frame(a = 1, b = 1:2))
emptycols(data.frame(a = c(NA, NA), b = 1:2))

## ---------------------------------------------------------------------------------------
emptyrows(data.frame(a = c(NA, "x", ""), b = c(NA, "y", NA)))

## ---------------------------------------------------------------------------------------
move(mtcars, "hp", before = "mpg") |> colnames()
firstcols(mtcars, "wt") |> colnames()
lastcols(mtcars, "mpg") |> colnames()

## ---------------------------------------------------------------------------------------
firstrows(mtcars, 3)
lastrows(mtcars, 2)
set.seed(1)
samplerows(mtcars, 2)

## ---------------------------------------------------------------------------------------
set.seed(1)
samplerows(mtcars, 2, by = "cyl")

## ---------------------------------------------------------------------------------------
firstrows(mtcars, c(1, 3, 5))

## ---------------------------------------------------------------------------------------
orderrows(mtcars, by = "mpg", decreasing = TRUE) |> headtail(2)

## ---------------------------------------------------------------------------------------
rownumber(c("a", "b", "c"))

## ---------------------------------------------------------------------------------------
d <- data.frame(x = c(1, NA, 3), y = c(4, 5, NA), z = c(TRUE, FALSE, TRUE))
rowmin(d, cols = c("x", "y"), na.rm = TRUE)
rowmax(d, cols = c("x", "y"), na.rm = TRUE)
rowfirst(d, cols = c("x", "y"), na.rm = TRUE)
rowlast(d, cols = c("x", "y"), na.rm = TRUE)
rowapply(d, cols = c("x", "y"), fun = function(row) sum(row, na.rm = TRUE))

## ---------------------------------------------------------------------------------------
applycols(mtcars, cols = "mpg", fun = round) |> headtail(2)
convertcols(mtcars, cols = "cyl", fun = as.character) |> types()

## ---------------------------------------------------------------------------------------
trim("  hello  ")
squish("  too   many   spaces ")
lower("HELLO"); upper("hello")
titlecase("hello world"); sentencecase("HELLO WORLD")
textlen(c("abc", NA))

## ---------------------------------------------------------------------------------------
left("hello", 3); right("hello", 3); middle("hello", 2, 4)
truncate("a long sentence", 10)
padleft("7", 3, pad = "0"); padright("7", 3, pad = "0"); padcenter("hi", 6, pad = "*")

## ---------------------------------------------------------------------------------------
containstext(c("apple", "banana"), "an")
matchestext(c("cat", "cats"), "cat")
startswith(c("apple", "banana"), "a")
endswith(c("apple", "banana"), "a")
countmatch("banana", "a")
locate("banana", "an")

## ---------------------------------------------------------------------------------------
extract("order #4231", "[0-9]+")
extractall("a1b2c3", "[0-9]")
extractnum("total: -12.5 kg")
extractbetween("[important]", "\\[", "\\]")

## ---------------------------------------------------------------------------------------
replacetext("a-b-c", "-", "_")
removetext("a-b-c", "-")
replaceall(c("a", "b"), old = "a", new = "A")

## ---------------------------------------------------------------------------------------
splittext("a-b-c", "-")
splitfirst("a-b-c", "-"); splitlast("a-b-c", "-")
jointext("a", "b", "c")
collapsetext(c("a", "b", "c"), sep = ", ")

## ---------------------------------------------------------------------------------------
isblank(c("", "  ", NA, "x"))
removeaccents("café")

## ---------------------------------------------------------------------------------------
textdist("cat", c("cat", "bat", "dog"))
nearesttext("kat", c("cat", "dog", "bird"))

## ---------------------------------------------------------------------------------------
isalpha(c("abc", "a1c"))
isnumerictext(c("12.5", "abc"))
isemail(c("a@b.com", "not-an-email"))
isurl(c("https://example.com", "example.com"))

## ---------------------------------------------------------------------------------------
recode(c("a", "b", "c"), old = "a", new = "A")
collapsevalues(c("cat", "dog", "bird"), groups = list(pet = c("cat", "dog")))

x <- rep(c("a", "b", "c", "d"), c(10, 5, 3, 1))
lump(x, n = 2, other = "Other") |> table()

f <- factor(c("low", "high", "mid"), levels = c("low", "mid", "high"))
reorderlevels(f, by = c("high", "mid", "low"))
expandlevels(f, "extra")

## ---------------------------------------------------------------------------------------
transform(mtcars, size = casewhen(
  list(light = wt < 2.5, mid = wt < 3.5),
  default = "heavy"
)) |> pick(c("wt", "size")) |> headtail(2)

## ---------------------------------------------------------------------------------------
parseint(c("1", "2", "NA"))
parsenum("1.234,56", decimal = ",", grouping = ".")
parselogical(c("TRUE", "false", "T", "0"))
parsedate(c("2024-01-15", "15/01/2024"), formats = c("%Y-%m-%d", "%d/%m/%Y"))
parsedatetime("2024-01-15 08:30:00", formats = "%Y-%m-%d %H:%M:%S")
parsepercent("42%")
parsecurrency("$1,234.50")
parsefailures(c("1", "x", "3"), parseint)

## ---------------------------------------------------------------------------------------
d <- as.Date("2024-03-15")
year(d); month(d); day(d); weekday(d); yearday(d); week(d); quarter(d)

t <- as.POSIXct("2024-03-15 08:30:45", tz = "UTC")
hour(t); minute(t); second(t)

## ---------------------------------------------------------------------------------------
adddays(d, 10); addweeks(d, 2)
addmonths(as.Date("2024-01-31"), 1, invalid = "previous")
addmonths(as.Date("2024-01-31"), 1, invalid = "next")
datediff(as.Date("2024-01-10"), as.Date("2024-01-01"), units = "days")
dateseq(as.Date("2024-01-01"), as.Date("2024-01-05"))
betweendates(d, "2024-01-01", "2024-12-31")

## ---------------------------------------------------------------------------------------
floordate(d, "month"); ceilingdate(d, "month"); rounddate(d, "month")
floordate(d, "week")

## ---------------------------------------------------------------------------------------
x <- c(1, 2, 3, 4, 100)
rescale(x); standardize(x); center(x)
winsorize(x, probs = c(0.1, 0.9))

## ---------------------------------------------------------------------------------------
quantilegroup(1:100, n = 4) |> table()

## ---------------------------------------------------------------------------------------
percentchange(c(100, 110, 99))
percentrank(c(10, 20, 20, 30))
denserank(c(30, 10, 20, 10, 30))

## ---------------------------------------------------------------------------------------
cumcount(c("a", "b", "c"))
cumedist(c("a", "a", "b"))
cumavg(c(1, 2, 3))

## ---------------------------------------------------------------------------------------
difference(c(1, 3, 6, 10))
lagvalue(1:5, n = 1)
leadvalue(1:5, n = 1)

## ---------------------------------------------------------------------------------------
v <- c(1, 2, NA, 4, 5)
rollmean(v, width = 3, na.rm = TRUE)
rollsum(v, width = 3, na.rm = TRUE, partial = TRUE)
rollmax(v, width = 2, na.rm = TRUE)
rollapply(1:5, width = 3, FUN = sum, partial = TRUE)

## ---------------------------------------------------------------------------------------
map(1:3, function(x) x + 0.5)
traverse(list(a = 1:2, b = 10:11), function(a, b) a + b)
foldr(1:4, `+`)

## ----teardown-----------------------------------------------------------------
options(.old_opts)

