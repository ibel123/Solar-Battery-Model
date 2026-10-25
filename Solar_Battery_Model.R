# ==================================================================
# Solar battery savings model for Naomi
# Author: Ibaale Eric
#
# Purpose: work out hour by hour how much of Naomi's 2020 electricity
# use a 12.5 kWh battery would have covered, then project the saving
# over 20 years and calculate the NPV and IRR for two price scenarios.
#
# How to run:
#  1. Put this script and "Data for candidate 2026.xlsx" in the same folder.
#  2. Set R's working directory to that folder
#     (RStudio: Session > Set Working Directory > To Source File Location).
#  3. Run the whole script. It needs the readxl package.
#
# Output: tables printed in the console, seven figures saved as png files
# and a file called hourly_model_results.csv, all in the same folder.
# The explanation, assumptions and results are in the Word document.
# ==================================================================


# ------------------------------------------------------------------
# Settings, inputs from the brief and small helper functions
# ------------------------------------------------------------------
options(scipen = 999)   # no scientific notation in the output
library(readxl)

# Inputs taken from the brief
file_name    <- "Data for candidate 2026.xlsx"  # sits in the same folder as this file
battery_cost <- 7000    # $ paid on 1 January 2022
battery_cap  <- 12.5    # kWh, the most the battery can hold
price_2022   <- 0.17    # $ per kWh from 1 January 2022
disc_rate    <- 0.06    # discount rate
n_years      <- 20      # battery life: 2022 to 2041
years        <- 2021 + 1:n_years
spike_level  <- 20      # kWh in one hour. Above this I flagged the reading but kept it

# Helpers that format numbers for the output
nf  <- function(x, d = 1) formatC(x, format = "f", digits = d, big.mark = ",")
pct <- function(x, d = 1) paste0(formatC(100 * x, format = "f", digits = d), "%")
pct_or_na <- function(x) ifelse(is.na(x), "not defined", pct(x, 2))
usd <- function(x, d = 2) {
  s <- formatC(abs(x), format = "f", digits = d, big.mark = ",")
  ifelse(x < 0, paste0("-$", s), paste0("$", s))
}

# Prints a table to the console with a title above it
show_table <- function(tab, caption) {
  cat("\n", caption, "\n", sep = "")
  print(tab, row.names = FALSE, right = FALSE)
}


# ------------------------------------------------------------------
# Step 1: read the Excel file
# ------------------------------------------------------------------
# The column names are not in row 1, so I read everything as text first
# and look for the row that starts with the word "Hour"
raw <- read_excel(file_name, sheet = "Raw Data", range = cell_cols("A:D"),
                  col_names = FALSE, col_types = "text")
header_row <- which(trimws(raw[[1]]) == "Hour")[1]
body <- raw[(header_row + 1):nrow(raw), ]
sheet_names <- excel_sheets(file_name)

# Excel stores the date and hour as one number (days since 30 Dec 1899).
# If a cell came through as a date in text form, this turns it back into that number.
to_serial <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  need <- is.na(out) & !is.na(x)
  if (any(need)) {
    as_time <- tryCatch(as.numeric(as.POSIXct(x[need], tz = "UTC")) / 86400 + 25569,
                        error = function(e) rep(NA_real_, sum(need)))
    out[need] <- as_time
  }
  out
}

d <- data.frame(hour   = suppressWarnings(as.numeric(body[[1]])),
                serial = to_serial(body[[2]]),
                solar  = suppressWarnings(as.numeric(body[[3]])),
                usage  = suppressWarnings(as.numeric(body[[4]])))

# Remove rows that are completely empty
all_blank <- is.na(d$hour) & is.na(d$serial) & is.na(d$solar) & is.na(d$usage)
d <- d[!all_blank, ]
n_rows_read <- nrow(d)

# Duplicated time stamps: keep the first one, then sort by time
dup_time <- duplicated(d$serial) & !is.na(d$serial)
n_dup_time <- sum(dup_time)
d <- d[!dup_time, ]
d <- d[order(d$serial), ]
d$row <- seq_len(nrow(d))

# Date, month and the hour worked out from the time stamp
d$date  <- as.Date(floor(d$serial + 1e-6), origin = "1899-12-30")
d$month <- as.integer(format(d$date, "%m"))
d$hour_from_time <- round((d$serial - floor(d$serial + 1e-6)) * 24)

# Keep the original values so I can compare before and after
d$solar_raw <- d$solar
d$usage_raw <- d$usage


# ------------------------------------------------------------------
# Step 2: checks on the raw data (nothing is changed here)
# ------------------------------------------------------------------
expected_rows   <- 365 * 24
n_days          <- length(unique(d$date))
n_feb29         <- sum(format(d$date, "%m-%d") == "02-29", na.rm = TRUE)
n_odd_days      <- sum(table(d$date) != 24)
n_jumps         <- sum(diff(round(d$serial * 24)) != 1, na.rm = TRUE)
n_hour_clash    <- sum(d$hour != d$hour_from_time, na.rm = TRUE)
n_missing_solar <- sum(is.na(d$solar))
n_missing_usage <- sum(is.na(d$usage))

# Flags for each problem I looked for
d$neg_usage  <- !is.na(d$usage) & d$usage < 0
d$neg_solar  <- !is.na(d$solar) & d$solar < 0
d$huge_usage <- !is.na(d$usage) & d$usage > 1000
d$dropout    <- !is.na(d$solar) & !is.na(d$usage) & d$solar == 0 & d$usage == 0 &
                d$hour %in% 9:15
d$spike      <- !is.na(d$usage) & d$usage > spike_level & d$usage <= 1000

n_neg_solar  <- sum(d$neg_solar)
n_neg_usage  <- sum(d$neg_usage)
n_huge       <- sum(d$huge_usage)
n_dropout    <- sum(d$dropout)
n_spike      <- sum(d$spike)
spike_max    <- max(d$usage[d$spike], 0)
n_spike_day  <- sum(d$spike & d$hour %in% 9:12)

dark_hours       <- d$hour %in% c(22, 23, 0:4)
n_night_solar    <- sum(d$solar > 0 & dark_hours, na.rm = TRUE)
max_night_solar  <- max(d$solar[which(d$solar > 0 & dark_hours)], 0)
n_long_dec <- sum(!is.na(d$usage) & abs(d$usage * 1000 - round(d$usage * 1000)) > 1e-6)

checks_tab <- data.frame(
  Check = c("Data rows read from the sheet",
            "Rows expected (365 days x 24 hours)",
            "Different dates in the file",
            "Rows dated 29 February",
            "Days that did not have exactly 24 rows",
            "Places where the hourly sequence jumped",
            "Rows where the Hour column disagreed with the time stamp",
            "Missing solar values",
            "Missing usage values",
            "Duplicated time stamps",
            "Negative solar values",
            "Negative usage values",
            "Usage above 1,000 kWh in one hour",
            "Daylight hours (9 am to 4 pm) with solar and usage both zero",
            "Usage above 20 kWh in one hour (flagged, kept)",
            "Non-zero solar between 10 pm and 4 am",
            "Usage values with more than 3 decimal places"),
  Result = c(nf(n_rows_read, 0), nf(expected_rows, 0), nf(n_days, 0), nf(n_feb29, 0),
             nf(n_odd_days, 0), nf(n_jumps, 0), nf(n_hour_clash, 0),
             nf(n_missing_solar, 0), nf(n_missing_usage, 0), nf(n_dup_time, 0),
             nf(n_neg_solar, 0), nf(n_neg_usage, 0), nf(n_huge, 0), nf(n_dropout, 0),
             nf(n_spike, 0), nf(n_night_solar, 0), nf(n_long_dec, 0)),
  stringsAsFactors = FALSE)


# ------------------------------------------------------------------
# Step 3: corrections
# ------------------------------------------------------------------
# Which values need replacing
fix_usage <- d$neg_usage | d$huge_usage | d$dropout | is.na(d$usage)
fix_solar <- d$neg_solar | d$dropout | is.na(d$solar)
n_fixed_rows <- sum(fix_usage | fix_solar)

# Replacement = median for the same hour of the day, using only the good rows
good <- !(fix_usage | fix_solar)
med_usage <- tapply(d$usage[good], d$hour[good], median)
med_solar <- tapply(d$solar[good], d$hour[good], median)
d$usage[fix_usage] <- as.numeric(med_usage[as.character(d$hour[fix_usage])])
d$solar[fix_solar] <- as.numeric(med_solar[as.character(d$hour[fix_solar])])

# Table of the rows I changed (first 15 only, to keep the table short)
why <- ifelse(d$neg_usage, "Negative usage",
       ifelse(d$huge_usage, "Usage above 1,000 kWh",
       ifelse(d$dropout, "Solar and usage both zero in daylight",
       ifelse(d$neg_solar, "Negative solar", "Missing value"))))
shown <- head(which(fix_usage | fix_solar), 15)
fixed_tab <- data.frame(Date = format(d$date[shown], "%d %b"),
                        Hour = d$hour[shown],
                        Problem = why[shown],
                        `Usage before` = nf(d$usage_raw[shown], 3),
                        `Usage after` = nf(d$usage[shown], 3),
                        `Solar before` = nf(d$solar_raw[shown], 3),
                        `Solar after` = nf(d$solar[shown], 3),
                        check.names = FALSE, stringsAsFactors = FALSE)

usage_total_raw   <- sum(d$usage_raw, na.rm = TRUE)
usage_total_clean <- sum(d$usage)
solar_total_raw   <- sum(d$solar_raw, na.rm = TRUE)
solar_total_clean <- sum(d$solar)

# Average by hour of the day, before and after
avg_by_hour <- function(x) as.numeric(tapply(x, d$hour, mean, na.rm = TRUE))
raw_solar_avg   <- avg_by_hour(d$solar_raw)
raw_usage_avg   <- avg_by_hour(d$usage_raw)
clean_solar_avg <- avg_by_hour(d$solar)
clean_usage_avg <- avg_by_hour(d$usage)
h_chg <- which.max(abs(clean_usage_avg - raw_usage_avg)) - 1
peak_use_hour   <- which.max(clean_usage_avg) - 1
peak_solar_hour <- which.max(clean_solar_avg) - 1

# Things I quote when I explain why I kept the spikes
typical_usage  <- median(d$usage[!fix_usage])
n_evening      <- sum(d$hour %in% 18:20)
n_evening_big  <- sum(d$hour %in% 18:20 & d$usage_raw > 12 & !fix_usage, na.rm = TRUE)


# ------------------------------------------------------------------
# Step 4: the battery model
# ------------------------------------------------------------------
# Goes through the hours in order. Solar is used first, then the battery,
# then the electricity provider.
run_battery <- function(solar, usage, cap) {
  n <- length(solar)
  bought_nb <- pmax(usage - solar, 0)   # (ii) bought, no battery
  excess    <- pmax(solar - usage, 0)   # (iii) solar left over
  charge <- discharge <- spilled <- level <- numeric(n)
  prev <- 0                             # battery starts empty
  for (i in 1:n) {
    charge[i]    <- max(0, min(excess[i], cap - prev))  # what fits in the battery
    spilled[i]   <- excess[i] - charge[i]               # what is lost
    discharge[i] <- min(bought_nb[i], prev)             # what the battery gives back
    level[i]     <- prev + charge[i] - discharge[i]     # (iv) battery level
    prev <- level[i]
  }
  bought_b <- bought_nb - discharge                     # (v) bought, with battery
  data.frame(bought_nb, excess, charge, spilled, discharge, level, bought_b)
}

d <- cbind(d, run_battery(d$solar, d$usage, battery_cap))

extra_kwh   <- sum(d$bought_nb) - sum(d$bought_b)   # extra kWh met by the battery
saving_2020 <- extra_kwh * price_2022               # (vi) saving at 2022 prices

# (vii) monthly totals
monthly <- aggregate(d[, c("solar", "usage", "bought_nb", "bought_b")],
                     by = list(month = d$month), FUN = sum)

# Save the full hourly results next to this file for anyone who wants to look
invisible(try(write.csv(d[, c("date", "hour", "solar_raw", "usage_raw", "solar", "usage",
                    "bought_nb", "excess", "level", "bought_b")],
              "hourly_model_results.csv", row.names = FALSE), silent = TRUE))


# ------------------------------------------------------------------
# Step 5: 20 year projection, NPV and IRR
# ------------------------------------------------------------------
# Price in each year as a multiple of the 2022 price
price_factors <- function(scenario, n = n_years) {
  f <- numeric(n)
  f[1] <- 1                                   # 2022
  for (k in 2:n) {
    if (scenario == 1) {
      inflation <- 0.04                       # 4% every year
    } else {
      inflation <- 0.04 + 0.0025 * (k - 2)    # 4%, 4.25%, 4.5% ... from 2023
    }
    f[k] <- f[k - 1] * (1 + inflation)
  }
  f
}

# Present value of a list of yearly amounts, received at the end of each year
pv <- function(cf, rate) sum(cf / (1 + rate)^seq_along(cf))

# IRR: the rate at which the present value of the savings equals the cost
find_irr <- function(cf, cost) {
  f <- function(r) pv(cf, r) - cost
  if (f(-0.5) * f(5) > 0) return(NA_real_)
  uniroot(f, lower = -0.5, upper = 5, tol = 1e-10)$root
}

project <- function(extra, scenario, rate = disc_rate) {
  price  <- price_2022 * price_factors(scenario)
  saving <- extra * price
  disc   <- saving / (1 + rate)^(1:n_years)
  list(price = price, saving = saving, disc = disc,
       pv = sum(disc), npv = sum(disc) - battery_cost,
       irr = find_irr(saving, battery_cost))
}

s1 <- project(extra_kwh, 1)
s2 <- project(extra_kwh, 2)

# Year in which the running total of discounted savings passes the cost
payback_year <- function(disc) {
  k <- which(cumsum(disc) >= battery_cost)[1]
  if (is.na(k)) "Not within 20 years" else as.character(2021 + k)
}

# Saving needed in the first year for the NPV to be zero
need1 <- battery_cost / sum(price_factors(1) / (1 + disc_rate)^(1:n_years))
need2 <- battery_cost / sum(price_factors(2) / (1 + disc_rate)^(1:n_years))

# Sensitivity 1: spikes above 20 kWh replaced by the hourly median
usage_alt <- d$usage
usage_alt[d$spike] <- as.numeric(med_usage[as.character(d$hour[d$spike])])
m_alt     <- run_battery(d$solar, usage_alt, battery_cap)
extra_alt <- sum(m_alt$bought_nb) - sum(m_alt$bought_b)
a1 <- project(extra_alt, 1)
a2 <- project(extra_alt, 2)

# Sensitivity 2: savings received in the middle of each year instead of the end
pv_mid <- function(cf, rate) sum(cf / (1 + rate)^(seq_along(cf) - 0.5))
mid1 <- pv_mid(s1$saving, disc_rate) - battery_cost
mid2 <- pv_mid(s2$saving, disc_rate) - battery_cost

conclusion <- if (s1$npv > 0 && s2$npv > 0) {
  "In both scenarios the NPV was positive and the IRR was above 6%, so on these figures the battery paid for itself within its 20 years."
} else if (s1$npv < 0 && s2$npv < 0) {
  "In both scenarios the NPV was negative and the IRR was below 6%, so on these figures the battery did not pay for itself within its 20 years."
} else {
  "The two scenarios gave different answers: the NPV was positive in one and negative in the other, so the decision depends on which price path Naomi believes."
}


# ------------------------------------------------------------------
# Step 6: checks on my own calculations
# ------------------------------------------------------------------
tol <- 1e-8
# A second, independent way of getting the battery level:
# new level = old level + solar - usage, held between 0 and the cap
ref_level <- Reduce(function(l, net) min(battery_cap, max(0, l + net)),
                    d$solar - d$usage, 0, accumulate = TRUE)[-1]

check_list <- c(
  "No negative values in the bought, excess and battery columns" =
    all(d[, c("bought_nb", "excess", "charge", "spilled", "discharge", "level", "bought_b")] >= -tol),
  "Battery level always between 0 and 12.5 kWh" =
    all(d$level >= -tol & d$level <= battery_cap + tol),
  "Battery started empty (first level equals first charge less first discharge)" =
    abs(d$level[1] - (d$charge[1] - d$discharge[1])) < tol,
  "Each hour: usage = solar used + battery + bought (with battery)" =
    all(abs(d$usage - ((d$solar - d$excess) + d$discharge + d$bought_b)) < tol),
  "Each hour: usage = solar used + bought (no battery)" =
    all(abs(d$usage - ((d$solar - d$excess) + d$bought_nb)) < tol),
  "No hour with both excess solar and electricity bought" =
    !any(d$excess > tol & d$bought_nb > tol),
  "Excess solar = solar charged + solar lost, every hour" =
    all(abs(d$excess - (d$charge + d$spilled)) < tol),
  "Total charged = total discharged + final battery level" =
    abs(sum(d$charge) - sum(d$discharge) - d$level[nrow(d)]) < 1e-6,
  "Never more bought with the battery than without" =
    all(d$bought_b <= d$bought_nb + tol),
  "Battery never gave out more than it held" =
    all(d$discharge <= c(0, d$level[-nrow(d)]) + tol),
  "Electricity saved by the battery is no more than the excess solar" =
    sum(d$discharge) <= sum(d$excess) + tol,
  "Battery level matches the second, independent calculation" =
    max(abs(ref_level - d$level)) < 1e-6,
  "Monthly totals add up to the annual totals" =
    all(abs(colSums(monthly[, c("solar", "usage", "bought_nb", "bought_b")]) -
            c(sum(d$solar), sum(d$usage), sum(d$bought_nb), sum(d$bought_b))) < 1e-6),
  "Discounting 20 years of 1 at 6% matches the annuity formula" =
    abs(pv(rep(1, n_years), 0.06) - (1 - 1.06^(-n_years)) / 0.06) < 1e-9,
  "Price paths match a hand calculation (2023 in scenario 1, 2025 in scenario 2)" =
    abs(price_factors(1)[2] - 1.04) < tol &&
    abs(price_factors(2)[4] - 1.04 * 1.0425 * 1.045) < tol,
  "Present value at the IRR equals the battery cost (both scenarios)" =
    isTRUE(abs(pv(s1$saving, s1$irr) - battery_cost) < 1e-4) &&
    isTRUE(abs(pv(s2$saving, s2$irr) - battery_cost) < 1e-4)
)
selfcheck_tab <- data.frame(Check = names(check_list),
                            Result = ifelse(check_list, "Passed", "Failed"),
                            stringsAsFactors = FALSE)
all_pass <- all(check_list)

# Numbers used in the reasonableness paragraph
daily_use        <- sum(d$usage) / n_days
daily_solar      <- sum(d$solar) / n_days
daily_discharge  <- sum(d$discharge) / n_days
share_direct     <- sum(d$solar - d$excess) / sum(d$solar)
share_captured   <- sum(d$charge) / sum(d$excess)
share_avoided    <- extra_kwh / sum(d$bought_nb)
hours_full       <- sum(d$level >= battery_cap - 1e-9)
hours_empty      <- sum(d$level <= 1e-9)
max_solar        <- max(d$solar)

show_table(checks_tab, "Table 2: Checks on the raw data and what I found")

png("Figure_1_raw_hourly_averages.png", width = 6.3, height = 3.6, units = "in", res = 150)
plot_hourly <- function(solar_avg, usage_avg, title) {
  par(family = "serif", mar = c(4.2, 4.5, 2.5, 1))
  plot(0:23, usage_avg, type = "o", pch = 16, lwd = 2, col = "steelblue4",
       ylim = c(0, max(c(solar_avg, usage_avg)) * 1.08), xaxt = "n",
       xlab = "Hour of the day (0 = midnight to 1 am)", ylab = "Average kWh in the hour",
       main = title)
  lines(0:23, solar_avg, type = "o", pch = 16, lwd = 2, col = "darkorange")
  axis(1, at = seq(0, 23, 2))
  legend("topleft", legend = c("Electricity usage", "Solar generation"),
         col = c("steelblue4", "darkorange"), lty = 1, pch = 16, bty = "n")
}
plot_hourly(raw_solar_avg, raw_usage_avg, "Before cleaning")
invisible(dev.off())

png("Figure_2_usage_readings.png", width = 6.3, height = 3.6, units = "in", res = 150)
n_off <- sum(d$usage_raw > 40 | d$usage_raw < -15, na.rm = TRUE)
par(family = "serif", mar = c(4.2, 4.5, 2.5, 1))
plot(d$row, d$usage_raw, pch = 16, cex = 0.35, col = "grey35", ylim = c(-15, 40),
     xlab = "Hour number in the file (1 = first hour of 1 January)",
     ylab = "Usage in the hour (kWh)", main = "Hourly usage readings")
points(d$row[d$neg_usage], d$usage_raw[d$neg_usage], pch = 16, cex = 1, col = "red3")
abline(h = spike_level, lty = 2, col = "darkorange3")
legend("topright", legend = c("Negative reading", "20 kWh flag level"),
       col = c("red3", "darkorange3"), pch = c(16, NA), lty = c(NA, 2), bty = "n")
mtext(paste0(n_off, " reading(s) fall outside the range shown"), side = 3, line = 0,
      cex = 0.7, adj = 1)
invisible(dev.off())

show_table(fixed_tab, "Table 3: Rows I corrected (first 15), before and after, in kWh")

png("Figure_3_clean_hourly_averages.png", width = 6.3, height = 3.6, units = "in", res = 150)
plot_hourly(clean_solar_avg, clean_usage_avg, "After cleaning")
invisible(dev.off())

totals_tab <- data.frame(
  Item = c("Solar generation", "Electricity usage",
           "Bought from the provider, no battery",
           "Bought from the provider, with battery",
           "Extra electricity met by the battery (the difference)",
           "Excess solar with no battery",
           "Excess solar that went into the battery",
           "Excess solar lost because the battery was full"),
  kWh = nf(c(sum(d$solar), sum(d$usage), sum(d$bought_nb), sum(d$bought_b), extra_kwh,
             sum(d$excess), sum(d$charge), sum(d$spilled)), 1),
  stringsAsFactors = FALSE)
show_table(totals_tab, "Table 4: Annual totals for 2020 (kWh)")

monthly_tab <- data.frame(Month = month.abb[monthly$month],
                          Solar = nf(monthly$solar, 1),
                          Usage = nf(monthly$usage, 1),
                          `Bought, no battery` = nf(monthly$bought_nb, 1),
                          `Bought, with battery` = nf(monthly$bought_b, 1),
                          `Saved by battery` = nf(monthly$bought_nb - monthly$bought_b, 1),
                          check.names = FALSE, stringsAsFactors = FALSE)
monthly_tab[nrow(monthly_tab) + 1, ] <- c("Year", nf(sum(monthly$solar), 1),
                                          nf(sum(monthly$usage), 1),
                                          nf(sum(monthly$bought_nb), 1),
                                          nf(sum(monthly$bought_b), 1), nf(extra_kwh, 1))
show_table(monthly_tab, "Table 5: Monthly totals for 2020 (kWh)")

png("Figure_4_monthly_totals.png", width = 6.3, height = 3.6, units = "in", res = 150)
mat <- t(as.matrix(monthly[, c("solar", "usage", "bought_nb", "bought_b")]))
par(family = "serif", mar = c(4.2, 4.8, 2.5, 1))
barplot(mat, beside = TRUE, names.arg = month.abb[monthly$month],
        col = c("darkorange", "steelblue4", "grey60", "seagreen4"),
        ylim = c(0, max(mat) * 1.3), ylab = "kWh in the month",
        main = "Monthly totals, 2020",
        legend.text = c("Solar generation", "Electricity usage",
                        "Bought, no battery", "Bought, with battery"),
        args.legend = list(x = "top", bty = "n", cex = 0.75, ncol = 2))
invisible(dev.off())

png("Figure_5_battery_week.png", width = 6.3, height = 5, units = "in", res = 150)
wk <- which(d$date >= as.Date("2020-01-13") & d$date <= as.Date("2020-01-19"))
if (length(wk) > 0) {
  par(mfrow = c(2, 1), family = "serif", mar = c(3.5, 4.5, 2, 1))
  ticks <- seq(1, length(wk), by = 24)
  plot(seq_along(wk), d$usage[wk], type = "l", col = "steelblue4", xaxt = "n",
       ylim = c(0, max(d$usage[wk], d$solar[wk])), xlab = "", ylab = "kWh in the hour",
       main = "Solar generation and usage")
  lines(seq_along(wk), d$solar[wk], col = "darkorange", lwd = 2)
  axis(1, at = ticks, labels = format(d$date[wk][ticks], "%d %b"))
  legend("topleft", legend = c("Usage", "Solar"), col = c("steelblue4", "darkorange"),
         lty = 1, bty = "n", cex = 0.8)
  plot(seq_along(wk), d$level[wk], type = "l", col = "seagreen4", lwd = 2, xaxt = "n",
       ylim = c(0, battery_cap * 1.1), xlab = "", ylab = "Battery level (kWh)",
       main = "Battery charge level")
  abline(h = battery_cap, lty = 2, col = "grey40")
  axis(1, at = ticks, labels = format(d$date[wk][ticks], "%d %b"))
}
invisible(dev.off())

png("Figure_6_price_paths.png", width = 6.3, height = 3.6, units = "in", res = 150)
par(family = "serif", mar = c(4.2, 4.5, 2.5, 1))
plot(years, s2$price, type = "o", pch = 16, cex = 0.7, col = "darkorange3", lwd = 2,
     ylim = c(0, max(s2$price) * 1.05), xlab = "Year", ylab = "$ per kWh",
     main = "Electricity price")
lines(years, s1$price, type = "o", pch = 16, cex = 0.7, col = "steelblue4", lwd = 2)
legend("topleft", legend = c("Scenario 1: 4% every year",
                             "Scenario 2: 4%, rising by 0.25% each year"),
       col = c("steelblue4", "darkorange3"), lty = 1, pch = 16, bty = "n")
invisible(dev.off())

proj_tab <- data.frame(Year = as.character(years),
                       `S1 price` = nf(s1$price, 4), `S1 saving` = nf(s1$saving, 2),
                       `S1 discounted` = nf(s1$disc, 2),
                       `S2 price` = nf(s2$price, 4), `S2 saving` = nf(s2$saving, 2),
                       `S2 discounted` = nf(s2$disc, 2),
                       check.names = FALSE, stringsAsFactors = FALSE)
proj_tab[nrow(proj_tab) + 1, ] <- c("Total", "", nf(sum(s1$saving), 2), nf(sum(s1$disc), 2),
                                    "", nf(sum(s2$saving), 2), nf(sum(s2$disc), 2))
show_table(proj_tab, "Table 6: Projected savings. Price in $ per kWh, saving and discounted saving in $. S1 = scenario 1, S2 = scenario 2")

res_tab <- data.frame(
  Measure = c("Total savings over 20 years, not discounted ($)",
              "Present value of savings at 6% ($)",
              "Battery cost ($)",
              "NPV after the battery cost ($)",
              "IRR",
              "Year the discounted savings pass the cost"),
  `Scenario 1` = c(nf(sum(s1$saving), 0), nf(s1$pv, 0), nf(battery_cost, 0), nf(s1$npv, 0),
                   pct_or_na(s1$irr), payback_year(s1$disc)),
  `Scenario 2` = c(nf(sum(s2$saving), 0), nf(s2$pv, 0), nf(battery_cost, 0), nf(s2$npv, 0),
                   pct_or_na(s2$irr), payback_year(s2$disc)),
  check.names = FALSE, stringsAsFactors = FALSE)
show_table(res_tab, "Table 7: NPV and IRR of the battery for the two scenarios")

png("Figure_7_cumulative_savings.png", width = 6.3, height = 3.6, units = "in", res = 150)
par(family = "serif", mar = c(4.2, 4.5, 2.5, 1))
plot(years, cumsum(s2$disc), type = "o", pch = 16, cex = 0.7, col = "darkorange3", lwd = 2,
     ylim = c(0, max(cumsum(s2$disc), battery_cost) * 1.1), xlab = "Year",
     ylab = "Discounted savings so far ($)", main = "Running total of discounted savings")
lines(years, cumsum(s1$disc), type = "o", pch = 16, cex = 0.7, col = "steelblue4", lwd = 2)
abline(h = battery_cost, lty = 2, col = "grey30")
legend("topleft", legend = c("Scenario 1", "Scenario 2", "Battery cost"),
       col = c("steelblue4", "darkorange3", "grey30"), lty = c(1, 1, 2),
       pch = c(16, 16, NA), bty = "n")
invisible(dev.off())

show_table(selfcheck_tab, "Table 8: Self-checks built into the model")

sens_tab <- data.frame(
  Case = c("What I used", "Readings above 20 kWh replaced", "Savings received mid-year"),
  `Total usage (kWh)` = c(nf(sum(d$usage), 0), nf(sum(usage_alt), 0), nf(sum(d$usage), 0)),
  `Extra kWh from battery` = c(nf(extra_kwh, 1), nf(extra_alt, 1), nf(extra_kwh, 1)),
  `Saving in 2020 ($)` = c(nf(saving_2020, 2), nf(extra_alt * price_2022, 2), nf(saving_2020, 2)),
  `NPV S1 ($)` = c(nf(s1$npv, 0), nf(a1$npv, 0), nf(mid1, 0)),
  `NPV S2 ($)` = c(nf(s2$npv, 0), nf(a2$npv, 0), nf(mid2, 0)),
  check.names = FALSE, stringsAsFactors = FALSE)
show_table(sens_tab, "Table 9: Sensitivity of the results to two choices")

hour_tab <- data.frame(Hour = 0:23,
                       `Solar, raw` = nf(raw_solar_avg, 3), `Solar, corrected` = nf(clean_solar_avg, 3),
                       `Usage, raw` = nf(raw_usage_avg, 3), `Usage, corrected` = nf(clean_usage_avg, 3),
                       check.names = FALSE, stringsAsFactors = FALSE)
show_table(hour_tab, "Table 10: Average kWh per hour, before and after corrections")

# ------------------------------------------------------------------
# Main results
# ------------------------------------------------------------------
cat("\n===== Main results =====\n")
cat("Extra electricity met by the battery in 2020 (kWh):", nf(extra_kwh, 1), "\n")
cat("Saving in 2020 at 2022 prices:", usd(saving_2020), "\n")
cat("Scenario 1: present value", usd(s1$pv, 0), "| NPV", usd(s1$npv, 0),
    "| IRR", pct_or_na(s1$irr), "\n")
cat("Scenario 2: present value", usd(s2$pv, 0), "| NPV", usd(s2$npv, 0),
    "| IRR", pct_or_na(s2$irr), "\n")
cat("All self-checks passed:", all_pass, "\n")
