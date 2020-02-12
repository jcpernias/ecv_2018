library(tidyverse)
library(magrittr)

library(Hmisc)

weighted.median <- function(x, w, ...) {
  UseMethod("weighted.median")
}

weighted.median.default <- function(x, w, ..., na.rm = FALSE) {
  if (missing(w)) {
    return(median(x, na.rm = na.rm))
  }
  unname(wtd.quantile(x, probs = 0.5, w))
}

# Gini index
gini <- function(z, w) {
  n <- length(z)
  oidx <- order(z)
  wo <- w[oidx] / sum(w)
  zo <- z[oidx]
  x <- wo / 2 + c(0, cumsum(wo)[-n])
  zo_mean <- sum(zo * wo)
  2 * sum(wo * (zo - zo_mean) * x) / zo_mean
}

# Lorenz curve
lorenz <- function(z, w) {
  n <- length(z)
  oidx <- order(z)
  wo <- w[oidx] / sum(w)
  zo <- z[oidx]
  tibble(x = wo / 2 + c(0, cumsum(wo)[-n]),
         y = cumsum(zo * wo) / sum(zo * wo))
}


# Read original data files
orig_data_dir <- file.path('data', 'orig')


d_file_db <-
  read_csv(file.path(orig_data_dir, 'esudb18d.csv.gz'),
           col_types = 'iciciiidiii')

h_file_db <-
  read_csv(file.path(orig_data_dir, 'esudb18h.csv.gz'),
           col_types = cols(
             .default = 'i',
             HB020 = 'c',
             HY020 = 'd',
             HY022 = 'd',
             HY023 = 'd',
             HY030N = 'd',
             HY040N = 'd',
             HY050N = 'd',
             HY060N = 'd',
             HY070N = 'd',
             HY080N = 'd',
             HY081N = 'd',
             HY090N = 'd',
             HY100N = 'd',
             HY110N = 'd',
             HY120N = 'd',
             HY130N = 'd',
             HY131N = 'd',
             HY145N = 'd',
             HY170N = 'd',
             HY010 = 'd',
             HY040G = 'd',
             HY050G = 'd',
             HY060G = 'd',
             HY070G = 'd',
             HY080G = 'd',
             HY081G = 'd',
             HY090G = 'd',
             HY100G = 'd',
             HY110G = 'd',
             HY120G = 'd',
             HY130G = 'd',
             HY131G = 'd',
             HY140G = 'd',
             HS130 = 'd',
             HH060 = 'd',
             HH061 = 'd',
             HH070 = 'd',
             HX240 = 'd',
             vhRentaa = 'd',
             vhRentaAIa = 'd'
           ))

r_file_db <-
  read_csv(file.path(orig_data_dir, 'esudb18r.csv.gz'),
           col_types = cols(
             .default = 'i',
             RB020 = 'c',
             RB050 = 'd',
             RL070 = 'd'
           ))


p_file_db <-
  read_csv(file.path(orig_data_dir, 'esudb18p.csv.gz'),
           col_types = cols(
             .default = 'i',
             PB020 = 'c',
             PB040 = 'd',
             PE020 = 'c',
             PE040 = 'c',
             PL111A = 'c',
             PY010N = 'd',
             PY020N = 'd',
             PY021N = 'd',
             PY035N = 'd',
             PY050N = 'd',
             PY080N = 'd',
             PY090N = 'd',
             PY100N = 'd',
             PY110N = 'd',
             PY120N = 'd',
             PY130N = 'd',
             PY140N = 'd',
             PY010G = 'd',
             PY020G = 'd',
             PY021G = 'd',
             PY030G = 'd',
             PY035G = 'd',
             PY050G = 'd',
             PY080G = 'd',
             PY090G = 'd',
             PY100G = 'd',
             PY110G = 'd',
             PY120G = 'd',
             PY130G = 'd',
             PY140G = 'd',
             PHD02T = 'd'
           ))

# Join household databases
households <- d_file_db %>%
  left_join(h_file_db, by = c('DB030' = 'HB030'))

# Join data for adults
adults <- r_file_db %>%
  right_join(p_file_db, by = c('RB030' = 'PB030'))

# Isolate data for children
children <- r_file_db %>%
  anti_join(p_file_db, by = c('RB030' = 'PB030'))


# Build gender variables
gender_db <-
  bind_rows(adults %>%
              transmute(hh_id = as.integer(RB030 / 100),
                        woman = RB090 == 2),
            children %>%
              transmute(hh_id = as.integer(RB030 / 100),
                        woman = RB090 == 2)) %>%
  group_by(hh_id) %>%
  summarise(women = sum(woman),
            men = n() - women)

# Select income variables and demographic characteristics
hh_income_db <- households %>%
  select(hh_id = DB030,
         people = HX040,
         cunits = HX240,
         weight = DB090,
         region = DB040,
         ydisp_hh = vhRentaa,
         ydisp_ir_hh = vhRentaAIa) %>%
  mutate(ydisp_cu = ydisp_hh / cunits,
         ydisp_ir_cu = ydisp_ir_hh / cunits) %>%
  left_join(gender_db, by = 'hh_id')

# Add deciles and quintiles
decile_limits <-
  hh_income_db %$%
  wtd.quantile(ydisp_cu,
               probs = (0:10)/10,
               weights = weight * people)

hh_income_db <-
  hh_income_db %>%
  mutate(decile =
           cut(ydisp_cu, decile_limits,
               include.lowest = TRUE, labels = FALSE),
         quintile = as.integer((decile-1)/2) + 1)


# Average disposable income per consumption unit (total)
ydisp_mean <- hh_income_db %$%
  weighted.mean(ydisp_cu, weight * people)

# Average disposable income per consumption unit (women)
hh_income_db %$%
  weighted.mean(ydisp_cu, weight * women)

# Average disposable income per consumption unit (men)
hh_income_db %$%
  weighted.mean(ydisp_cu, weight * men)

# Poverty line: 60% of the median disposable income per c.u.
poverty_line <-
  hh_income_db %$%
    weighted.median(ydisp_cu, weight * people) * 0.6

# Poverty risk rate (total)
pov_rate <- hh_income_db %$%
  weighted.mean(ydisp_cu < poverty_line, weight * people)

# Poverty risk rate (women)
hh_income_db %$%
  weighted.mean(ydisp_cu < poverty_line, weight * women)

# Poverty risk rate (men)
hh_income_db %$%
  weighted.mean(ydisp_cu < poverty_line, weight * men)

# Poverty gap
hh_income_db %>%
  mutate(ratio = if_else (ydisp_cu <= poverty_line,
                          (poverty_line - ydisp_cu) / poverty_line, 0)) %>%
  summarise(gap = weighted.mean(ratio, weight * people))

hh_income_db %>%
  filter(ydisp_cu < poverty_line) %>%
  summarise(G = gini(ydisp_cu, weight * people))

# Fraction of women in the 3rd decile
hh_income_db %$%
  weighted.mean(decile == 3, weight * women)

# Fraction of men in the 3rd decile
hh_income_db %$%
  weighted.mean(decile == 3, weight * men)

# Fraction of people in the first decile across regions
hh_income_db %>%
  group_by(region) %>%
  summarise(d1 = weighted.mean(decile == 1, weight * people))

# Gini by region
hh_income_db %>%
  group_by(region) %>%
  summarise(G = gini(ydisp_cu, weight * people))

# s80/s20
y_quintiles <-
  hh_income_db %>%
  group_by(quintile) %>%
  summarise(yd_q = weighted.mean(ydisp_cu,  weight * people))


y_quintiles %$% { yd_q[5] / yd_q[1] }


# Palma ratio
y_deciles <-
  hh_income_db %>%
  group_by(decile) %>%
  summarise(yd_q = weighted.mean(ydisp_cu,  weight * people))

y_deciles %$% { sum(yd_q[10]) / sum(yd_q[1:4]) }


# Gini index
hh_income_db %$% gini(ydisp_cu, weight * people)

# Lorenz curve
hh_income_db %$% lorenz(ydisp_cu, weight * people) %$% plot(x, y, type = 'l')

