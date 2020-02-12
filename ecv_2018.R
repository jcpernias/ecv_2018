library(tidyverse)

# Read data files
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

households <- d_file_db %>%
  left_join(h_file_db, by = c('DB030' = 'HB030'))

adults <- r_file_db %>%
  right_join(p_file_db, by = c('RB030' = 'PB030'))

children <- r_file_db %>%
  anti_join(p_file_db, by = c('RB030' = 'PB030'))

library(Hmisc)

wtd.quantile(hh_data$income/hh_data$eq_scale,
             probs = (0:10)/10,
             weights = hh_data$hh_weight * hh_data$people,
             normwt = FALSE)

weighted.mean(hh_data$income/hh_data$eq_scale,
              hh_data$hh_weight * hh_data$people)

weighted.mean(hh_data$income/hh_data$people,
              hh_data$hh_weight * hh_data$people)

quantile(hh_data$income/hh_data$eq_scale, (0:10)/10)

vRentaa <- hogar_db$vhRentaa
hx240 <- hogar_db$
renta <- vRentaa / hx240

quantile(renta, (0:10)/10)

