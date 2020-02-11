library(tidyverse)

# Read data files
orig_data_dir <- file.path('data', 'orig')


hogar_base_db <- read_csv(file.path(orig_data_dir, 'esudb18d.csv.gz'))
hogar_db <- read_csv(file.path(orig_data_dir, 'esudb18h.csv.gz'))
personas_db <- read_csv(file.path(orig_data_dir, 'esudb18r.csv.gz'))
adultos_db <- read_csv(file.path(orig_data_dir, 'esudb18p.csv.gz'))


hh_base <- hogar_base_db %>%
  select(hh_id = DB030,
         region = DB040,
         hh_weight = DB090,
         urb = DB100)

hh_data <- hogar_db %>%
  select(income = vhRentaa,
         eq_scale = HX240,
         people = HX040,
         hh_id = HB030) %>%
  left_join(hh_base, by = 'hh_id')


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

