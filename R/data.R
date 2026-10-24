# Access to the package's data (R/sysdata.rda, built from the PediTools parameter tables) and checks of chart and
# measure ids.

# Session cache: the merged data and the tables' parameters (reset when charts are added or removed)
cache <- new.env(parent = emptyenv())

# The data: the built-in charts, and the charts added in the session (growth_add_chart()) in place of any built-in
# chart of the same id
pt_data <- function() {
  if (is.null(cache$data)) cache$data <- merge_data()
  cache$data
}

merge_data <- function() {
  data <- builtin_data
  data$tables$added <- FALSE
  if (is.null(session$tables) || !nrow(session$tables)) {
    return(data)
  }
  added <- unique(session$tables$chart)
  for (part in session_parts) {
    keep <- data[[part]][!data[[part]]$chart %in% added, , drop = FALSE]
    new <- session[[part]]
    if (part == "tables") new$added <- TRUE
    data[[part]] <- if (!is.null(new) && nrow(new)) rbind(keep, new[names(keep)]) else keep
    rownames(data[[part]]) <- NULL
  }
  data
}

# "chart|measure|sex" (sex NA: "")
table_key <- function(chart, measure, sex = NA_character_) {
  if (!length(chart)) {
    return(character(0))
  }
  paste(chart, measure, ifelse(is.na(sex), "", sex), sep = "|")
}

# Stops on chart ids that do not exist.
check_charts <- function(chart, tables = pt_data()$tables) {
  if (!is.character(chart)) {
    stop("`chart` must be a character vector of chart ids.", call. = FALSE)
  }
  unknown <- setdiff(unique(chart[!is.na(chart)]), tables$chart)
  if (length(unknown)) {
    stop("Unknown chart", if (length(unknown) > 1) "s", ": ", paste(unknown, collapse = ", "),
      "\nSee growth_charts() for the available ids.",
      call. = FALSE
    )
  }
  invisible(chart)
}

# The tables (rows of pt_data()$tables) of the charts, crossed with the measures (NULL: every measure) and the
# sexes (NULL: both; a table for both sexes is always included). Stops on pairs of chart and measure that do not
# exist.
select_tables <- function(chart, measure = NULL, sex = NULL) {
  tables <- pt_data()$tables
  check_charts(chart, tables)
  chart <- unique(chart[!is.na(chart)])
  rows <- tables[tables$chart %in% chart, , drop = FALSE]
  if (!is.null(measure)) {
    if (!is.character(measure)) {
      stop("`measure` must be a character vector of measure ids.", call. = FALSE)
    }
    measure <- unique(measure[!is.na(measure)])
    wanted <- expand.grid(chart = chart, measure = measure, stringsAsFactors = FALSE)
    missing <- wanted[!table_key(wanted$chart, wanted$measure) %in% table_key(tables$chart, tables$measure), ]
    if (nrow(missing)) {
      stop("Unknown chart/measure (", nrow(missing), "):\n",
        paste0("  ", missing$chart, " / ", missing$measure, collapse = "\n"),
        "\nSee growth_charts() for the available ids.",
        call. = FALSE
      )
    }
    rows <- rows[rows$measure %in% measure, , drop = FALSE]
  }
  if (!is.null(sex)) {
    sex <- parse_sex(sex, strict = TRUE)
    rows <- rows[is.na(rows$sex) | rows$sex %in% sex, , drop = FALSE]
  }
  rows
}

# Sex codes: "m", "f", "male", "female" in any case (and surrounding spaces) to "m" or "f". Anything else is NA
# with a warning, or an error when strict.
parse_sex <- function(sex, strict = FALSE) {
  if (is.factor(sex)) {
    sex <- as.character(sex)
  }
  if (!is.character(sex) && !all(is.na(sex))) {
    stop("`sex` must be a character vector: \"m\", \"f\", \"male\" or \"female\".", call. = FALSE)
  }
  codes <- c(m = "m", male = "m", f = "f", female = "f")
  out <- unname(codes[tolower(trimws(as.character(sex)))])
  bad <- unique(sex[is.na(out) & !is.na(sex)])
  if (length(bad)) {
    message <- paste0(
      "Unknown sex code", if (length(bad) > 1) "s", ": ", paste0("\"", bad, "\"", collapse = ", "),
      " (use \"m\", \"f\", \"male\" or \"female\")"
    )
    if (strict) {
      stop(message, call. = FALSE)
    }
    warning(message, "; set to NA.", call. = FALSE)
  }
  out
}

# Rows of a data frame of parameter rows (columns chart, measure, sex) belonging to the selected tables.
rows_of <- function(d, tables) {
  rownames(d) <- NULL
  d[table_key(d$chart, d$measure, d$sex) %in% table_key(tables$chart, tables$measure, tables$sex), , drop = FALSE]
}
