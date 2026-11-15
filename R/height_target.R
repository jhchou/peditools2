# Mid-parental (target) height: the PediTools engine's Calc\TargetHeight (phase 25 A), method by method.

target_methods <- c("tanner", "hermanussen_cole", "wright_cheetham", "jgz", "luo")
# The adult references: the height table at the adult age, in months
target_refs <- list(
  cdc = list(chart = "cdc_2000_pedi", measure = "height", months = 240),
  who = list(chart = "who_2007_5_19", measure = "height", months = 228)
)
# The child's height, on the reference's family, in this order: chart, measure, x units
target_child_tables <- list(
  cdc = list(c("cdc_2000_pedi", "height", "months")),
  who = list(c("who_2007_5_19", "height", "months"), c("who_expanded", "length_height", "days"))
)
# The child's height is compared from this age (standing height)
target_child_from_days <- 731
tanner_sex_cm <- 13
tanner_range_cm <- 8.5
aap_range_cm <- 10.2
hc_factor <- 0.72
hc_range_sds <- 1.64
wc_sex_cm <- 7
wc_factor <- 0.5
wc_range_sds <- 1.4
jgz_coef <- list(m = c(44.5, 0.376, 0.411, 11), f = c(47.1, 0.334, 0.364, 10))
luo_coef <- list(m = c(45.99, 0.78), f = c(37.85, 0.75))
luo_range_cm <- 10

#' Mid-parental (target) height
#'
#' A child's target (adult) height from both parents' heights, by the published methods, as on the PediTools
#' mid-parental height calculator; optionally compared with the child's own height. Vectorised over `sex`, the
#' heights, `reference` and the child's age and height.
#'
#' Methods (heights in cm; F the father's, M the mother's):
#' - `"tanner"` (Tanner, Goldstein and Whitehouse 1970): \eqn{(F + M + 13)/2} for boys, \eqn{(F + M - 13)/2} for
#'   girls. Range +-8.5 cm (Tanner; `low`, `high`) and +-10.2 cm (AAP, 95%; `low_aap`, `high_aap`).
#' - `"hermanussen_cole"` (Hermanussen and Cole 2003): the target SDS is \eqn{0.72 (SDS_F + SDS_M)/2}, the parents'
#'   SDS on the adult reference (the father's on the male table, the mother's on the female); 95% range +-1.64 SDS.
#' - `"wright_cheetham"` (Wright and Cheetham 1999): the mid-parental height \eqn{(F + M)/2 + 7} cm for boys,
#'   \eqn{- 7} cm for girls, as an SDS on the adult reference (UK90 in the paper; CDC or WHO here); the child's
#'   expected SDS is half of it; 90% range +-1.4 SDS.
#' - `"jgz"` (Dutch JGZ guideline Lengtegroei 2019, children of Dutch origin): boys \eqn{44.5 + 0.376 F + 0.411 M},
#'   girls \eqn{47.1 + 0.334 F + 0.364 M}; 95% range +-11 cm (boys), +-10 cm (girls).
#' - `"luo"` (Luo, Albertsson-Wikland and Karlberg 1998, Swedish children): boys \eqn{45.99 + 0.78 x}, girls
#'   \eqn{37.85 + 0.75 x}, x the mid-parental height \eqn{(F + M)/2}; 95% range +-10 cm.
#'
#' The target's SDS and probability are on the adult reference: `"cdc"`, `cdc_2000_pedi` height at 240 months (20
#' years), or `"who"`, `who_2007_5_19` height at 228 months (19 years). Ranges given in SDS are converted to cm on
#' the same reference.
#'
#' The child's height is compared from 731 days (2 years; standing height), on the reference's family:
#' `cdc_2000_pedi` (to 20 years); or `who_2007_5_19` from 5 years and `who_expanded` `length_height` from 2 to 5
#' years (to 19 years). `diff_z` is the child's SDS minus the target SDS.
#'
#' @param sex `"m"`, `"f"`, `"male"` or `"female"`, in any case: the child's sex. Other values are `NA` (with a
#'   warning).
#' @param father,mother The parents' heights, in cm.
#' @param reference The adult reference: `"cdc"` (default) or `"who"`.
#' @param methods The methods, any of `"tanner"`, `"hermanussen_cole"`, `"wright_cheetham"`, `"jgz"` and `"luo"`.
#'   Default: Tanner and Hermanussen-Cole, as on the website.
#' @param age_days,height Optional: the child's age in days (to convert: [growth_age_days()], [growth_convert()])
#'   and height in cm. Give both or neither.
#' @return A data frame, one row per element (after recycling) and method, in the order of `methods` above:
#'   \describe{
#'     \item{row}{The element's position in the input.}
#'     \item{sex, father, mother, reference, age_days, height}{The input (`sex` as `"m"` or `"f"`; `age_days` and
#'       `height` `NA` when not given).}
#'     \item{method}{The method.}
#'     \item{target, target_z, target_p}{The target height (cm), its SDS and probability on the adult reference.}
#'     \item{low, high}{The method's range (cm): Tanner's +-8.5 cm, or the range given above.}
#'     \item{low_aap, high_aap}{Tanner's method only: the AAP range, +-10.2 cm.}
#'     \item{mph_z, mph_cm}{The mid-parental SDS of the SDS methods (Hermanussen-Cole, Wright-Cheetham), and
#'       Wright-Cheetham's mid-parental height in cm; `NA` for the others.}
#'     \item{child_z, child_p, child_chart}{The child's height SDS, probability and chart.}
#'     \item{child_status}{`"ok"`; `"under_2_years"` (before 731 days, not compared); `"outside"` (after the end
#'       of the reference's charts); `"invalid"` (a height that is not positive); `NA` when no child was given.}
#'     \item{diff_z}{`child_z - target_z`.}
#'   }
#' @references
#' Tanner JM, Goldstein H, Whitehouse RH. Standards for children's height at ages 2-9 years allowing for height of
#' parents. Arch Dis Child 1970;45:755-762.
#'
#' Hermanussen M, Cole TJ. The calculation of target height reconsidered. Horm Res 2003;59:180-183.
#'
#' Wright CM, Cheetham TD. The strengths and limitations of parental heights as a predictor of attained height.
#' Arch Dis Child 1999;81:257-260.
#'
#' Luo ZC, Albertsson-Wikland K, Karlberg J. Target height as predicted by parental heights in a population-based
#' study. Pediatr Res 1998;44:563-571.
#'
#' JGZ-richtlijn Lengtegroei (Dutch youth health care guideline, height growth), 2019, appendix 1.
#' @seealso The PediTools mid-parental height calculator, <https://peditools.org/>, and its section of the Sources
#'   and methods page.
#' @export
#' @examples
#' # A girl, father 180 cm, mother 165 cm: Tanner and Hermanussen-Cole on CDC 2000
#' growth_height_target("f", 180, 165)[c("method", "target", "target_z", "low", "high")]
#'
#' # Every method, on WHO, and the child's height at 10 years
#' growth_height_target("m", 175, 160,
#'   reference = "who", methods = c("tanner", "hermanussen_cole", "wright_cheetham", "jgz", "luo"),
#'   age_days = growth_convert(10, "years", "days"), height = 135
#' )[c("method", "target", "target_z", "child_z", "diff_z")]
#'
#' # Vectorised: one call for a data frame of families
#' d <- data.frame(sex = c("m", "f"), father = c(182, 170), mother = c(168, 158))
#' growth_height_target(d$sex, d$father, d$mother)[c("row", "method", "target")]
growth_height_target <- function(sex, father, mother, reference = "cdc", methods = c("tanner", "hermanussen_cole"),
                                 age_days = NULL, height = NULL) {
  if (!is.character(methods) || !length(methods) || anyNA(methods) || !all(methods %in% target_methods)) {
    stop("`methods` must be among ", paste0("\"", target_methods, "\"", collapse = ", "), ".", call. = FALSE)
  }
  methods <- target_methods[target_methods %in% methods]
  if (is.null(age_days) != is.null(height)) {
    stop("Give both `age_days` and `height` (the child's), or neither.", call. = FALSE)
  }
  child <- !is.null(age_days)
  if (!child) age_days <- height <- NA_real_
  a <- recycle(
    sex = sex, father = father, mother = mother, reference = reference, age_days = age_days,
    height = height
  )
  for (v in c("father", "mother", "age_days", "height")) {
    if (!is.numeric(a[[v]]) && !all(is.na(a[[v]]))) stop("`", v, "` must be numeric.", call. = FALSE)
    a[[v]] <- as.numeric(a[[v]])
  }
  if (!is.character(a$reference) || anyNA(a$reference) || !all(a$reference %in% names(target_refs))) {
    stop("`reference` must be \"cdc\" or \"who\".", call. = FALSE)
  }
  n <- length(a$sex)
  sex <- parse_sex(a$sex)
  is_m <- sex == "m"
  chart <- vapply(target_refs[a$reference], `[[`, "", "chart", USE.NAMES = FALSE)
  measure <- vapply(target_refs[a$reference], `[[`, "", "measure", USE.NAMES = FALSE)
  months <- vapply(target_refs[a$reference], `[[`, 0, "months", USE.NAMES = FALSE)
  z_of <- function(y, s) evaluate(months, s, chart, measure, y = y)$z
  y_of <- function(z) evaluate(months, sex, chart, measure, z = z)$y
  na <- rep(NA_real_, n)

  cm_method <- function(cm, half) {
    list(target = cm, target_z = z_of(cm, sex), low = cm - half, high = cm + half, mph_z = na, mph_cm = na)
  }
  sds_method <- function(sds, half, mph_z, mph_cm = na) {
    list(
      target = y_of(sds), target_z = sds, low = y_of(sds - half), high = y_of(sds + half), mph_z = mph_z,
      mph_cm = mph_cm
    )
  }
  coef <- function(table, k) ifelse(is_m, table$m[k], table$f[k])
  results <- list()
  for (id in methods) {
    r <- switch(id,
      tanner = {
        out <- cm_method((a$father + a$mother + ifelse(is_m, tanner_sex_cm, -tanner_sex_cm)) / 2, tanner_range_cm)
        out$low_aap <- out$target - aap_range_cm
        out$high_aap <- out$target + aap_range_cm
        out
      },
      hermanussen_cole = {
        mph <- (z_of(a$father, "m") + z_of(a$mother, "f")) / 2
        sds_method(hc_factor * mph, hc_range_sds, mph)
      },
      wright_cheetham = {
        mph_cm <- (a$father + a$mother) / 2 + ifelse(is_m, wc_sex_cm, -wc_sex_cm)
        mph <- z_of(mph_cm, sex)
        sds_method(wc_factor * mph, wc_range_sds, mph, mph_cm)
      },
      jgz = cm_method(
        coef(jgz_coef, 1) + coef(jgz_coef, 2) * a$father + coef(jgz_coef, 3) * a$mother,
        coef(jgz_coef, 4)
      ),
      luo = cm_method(coef(luo_coef, 1) + coef(luo_coef, 2) * (a$father + a$mother) / 2, luo_range_cm)
    )
    if (is.null(r$low_aap)) r$low_aap <- r$high_aap <- na
    r$target_p <- stats::pnorm(r$target_z)
    results[[id]] <- r
  }

  # The child's height SDS, from 2 years, on the first of the reference's tables that covers the age
  child_z <- child_p <- na
  child_chart <- status <- rep(NA_character_, n)
  if (child) {
    given <- !is.na(sex) & !is.na(a$age_days) & !is.na(a$height)
    invalid <- given & !(is.finite(a$height) & a$height > 0)
    status[invalid] <- "invalid"
    under <- given & !invalid & a$age_days < target_child_from_days - 1e-9
    status[under] <- "under_2_years"
    todo <- given & !invalid & !under
    for (ref in names(target_child_tables)) {
      for (t in target_child_tables[[ref]]) {
        i <- which(todo & is.na(child_chart) & a$reference == ref)
        if (!length(i)) next
        x <- growth_convert(a$age_days[i], "days", t[3])
        e <- evaluate(x, sex[i], t[1], t[2], y = a$height[i])
        ok <- e$status == "ok"
        child_z[i[ok]] <- e$z[ok]
        child_p[i[ok]] <- e$p[ok]
        child_chart[i[ok]] <- t[1]
      }
    }
    status[todo & !is.na(child_chart)] <- "ok"
    status[todo & is.na(child_chart)] <- "outside"
    if (!all(given | (is.na(a$age_days) & is.na(a$height)) | is.na(sex))) {
      warning("Rows with only one of the child's age and height: not compared.", call. = FALSE)
    }
  }

  rows <- rep(seq_len(n), each = length(methods))
  pick <- function(field) {
    m <- vapply(results, `[[`, numeric(n), field)
    as.vector(t(matrix(m, nrow = n)))
  }
  out <- data.frame(
    row = rows, sex = sex[rows], father = a$father[rows], mother = a$mother[rows], reference = a$reference[rows],
    age_days = a$age_days[rows], height = a$height[rows], method = rep(methods, n), stringsAsFactors = FALSE
  )
  for (v in c("target", "target_z", "target_p", "low", "high", "low_aap", "high_aap", "mph_z", "mph_cm")) {
    out[[v]] <- pick(v)
  }
  out$child_z <- child_z[rows]
  out$child_p <- child_p[rows]
  out$child_chart <- child_chart[rows]
  out$child_status <- status[rows]
  out$diff_z <- out$child_z - out$target_z
  out
}
