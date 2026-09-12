## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, message = FALSE, warning = FALSE,
  fig.width = 8, fig.height = 6, dpi = 120
)
library(basetable)
library(bench)
library(ggplot2)

have <- function(pkg) requireNamespace(pkg, quietly = TRUE)
HAVE_DT <- have("data.table")
HAVE_DP <- have("dplyr")

# Keep the vignette build quick; inst/benchmarks/benchmark-scale.R runs the
# full size x cardinality matrix, and inst/benchmarks/make-readme-figures.R
# regenerates the figures shown in the README.
N     <- as.integer(Sys.getenv("BT_VIGNETTE_N", "3e5"))
REPS  <- as.integer(Sys.getenv("BT_VIGNETTE_REPS", "9"))

.old_threads <- getOption("basetable.threads")
basetable::setthreads(percent = 100)

## ----data---------------------------------------------------------------------
set.seed(1)
n <- N
d <- data.frame(
  g  = sprintf("k%07d", sample(2000L, n, replace = TRUE)),               # ~2000 groups
  gh = sprintf("k%08d", sample(max(n %/% 10L, 1L), n, replace = TRUE)),  # ~n/10 groups
  x  = rnorm(n),
  y  = rnorm(n),
  id = seq_len(n),
  stringsAsFactors = FALSE
)
dim_tbl <- d[!duplicated(d$g), c("g", "y")]

if (HAVE_DT) {
  dt  <- data.table::as.data.table(d)
  dmt <- data.table::as.data.table(dim_tbl)
}

## ----run----------------------------------------------------------------------
bench_one <- function(label, exprs) {
  keep <- c(TRUE, HAVE_DT, HAVE_DP)[seq_along(exprs)]
  exprs <- exprs[keep]
  m <- bench::mark(exprs = exprs, iterations = REPS, check = FALSE, memory = TRUE)
  m$operation <- label
  m$engine    <- names(exprs)
  m
}

marks <- list(
  bench_one("filter", list(
    basetable  = quote(basetable::subset(d, x > 0.5)),
    data.table = quote(dt[x > 0.5]),
    dplyr      = quote(dplyr::filter(d, x > 0.5)))),
  bench_one("sort (string key)", list(
    basetable  = quote(basetable::orderrows(d, by = c("g", "x"))),
    data.table = quote(data.table::setorder(data.table::copy(dt), g, x)),
    dplyr      = quote(dplyr::arrange(d, g, x)))),
  bench_one("distinct", list(
    basetable  = quote(basetable::uniquerows(d, cols = "g")),
    data.table = quote(unique(dt[, list(g)])),
    dplyr      = quote(dplyr::distinct(d, g)))),
  bench_one("count by group", list(
    basetable  = quote(basetable::count(d, by = "gh", sort = FALSE)),
    data.table = quote(dt[, .N, by = gh]),
    dplyr      = quote(dplyr::count(d, gh)))),
  bench_one("sd by group", list(
    basetable  = quote(basetable::aggregate(d, by = "g", value = "x", fun = sd, sort = FALSE)),
    data.table = quote(dt[, list(x = sd(x)), by = g]),
    dplyr      = quote(dplyr::summarise(dplyr::group_by(d, g), x = sd(x), .groups = "drop")))),
  bench_one("equi join", list(
    # basetable::merge() keeps input order; pin data.table to sort = FALSE so
    # neither side also sorts the joined result.
    basetable  = quote(basetable::merge(d, dim_tbl, by = "g")),
    data.table = quote(merge(dt, dmt, by = "g", sort = FALSE)),
    dplyr      = quote(dplyr::inner_join(d, dim_tbl, by = "g")))),
  bench_one("semi join", list(
    basetable  = quote(basetable::semimerge(d, dim_tbl, by = "g")),
    data.table = quote(dt[dmt, on = "g", nomatch = NULL]),
    dplyr      = quote(dplyr::semi_join(d, dim_tbl, by = "g"))))
)

ops <- c("filter", "sort (string key)", "distinct", "count by group",
         "sd by group", "equi join", "semi join")

res <- do.call(rbind, lapply(marks, function(m) {
  data.frame(
    operation = m$operation,
    engine    = m$engine,
    median_ms = as.numeric(m$median) * 1000,
    mem_mb    = as.numeric(m$mem_alloc) / 1024^2,
    itr_sec   = as.numeric(m$`itr/sec`),
    stringsAsFactors = FALSE
  )
}))
res$operation <- factor(res$operation, levels = ops)
res$engine    <- factor(res$engine, levels = c("basetable", "data.table", "dplyr"))

## ----raw----------------------------------------------------------------------
marks[[which(ops == "sd by group")]][, c("engine", "min", "median", "itr/sec", "mem_alloc", "n_gc")]

## ----table, results='asis'----------------------------------------------------
bt <- res[res$engine == "basetable", c("operation", "median_ms", "mem_mb")]
names(bt)[2:3] <- c("bt_ms", "bt_mb")
tab <- merge(res, bt, by = "operation")
tab$vs_time <- tab$median_ms / tab$bt_ms
tab$vs_mem  <- tab$mem_mb / tab$bt_mb
tab <- tab[order(tab$operation, tab$engine),
           c("operation", "engine", "median_ms", "mem_mb", "vs_time")]
for (col in c("median_ms", "mem_mb", "vs_time"))
  tab[[col]] <- format(round(tab[[col]], 2), nsmall = 2)
knitr::kable(
  tab, row.names = FALSE,
  col.names = c("Operation", "Engine", "Median (ms)", "Mem (MB)", "vs basetable"),
  align = c("l", "l", "r", "r", "r")
)

## ----plot-time, fig.cap="Median runtime by engine (lower is better). Each panel has its own scale."----
ggplot(res, aes(engine, median_ms, fill = engine)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = round(median_ms)), hjust = -0.15, size = 3) +
  facet_wrap(~operation, ncol = 2, scales = "free_x") +
  coord_flip() +
  scale_fill_manual(values = c(basetable = "#1b7837", data.table = "#762a83",
                               dplyr = "#c2a5cf")) +
  labs(x = NULL, y = "Median time (ms)", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

## ----plot-mem, fig.cap="Memory allocated by each expression, as reported by bench (lower is better)."----
ggplot(res, aes(engine, mem_mb, fill = engine)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = ifelse(mem_mb < 1, sprintf("%.2f", mem_mb),
                               sprintf("%.0f", mem_mb))), hjust = -0.15, size = 3) +
  facet_wrap(~operation, ncol = 2, scales = "free_x") +
  coord_flip() +
  scale_fill_manual(values = c(basetable = "#1b7837", data.table = "#762a83",
                               dplyr = "#c2a5cf")) +
  labs(x = NULL, y = "Memory allocated (MB)", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

## ----teardown, include=FALSE--------------------------------------------------
options(basetable.threads = .old_threads)

