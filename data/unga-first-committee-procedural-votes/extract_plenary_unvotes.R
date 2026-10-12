# Extract UNGA plenary roll calls 2018-2019 (sessions 73-74) from the CRAN 'unvotes' package (v0.3.0),
# which redistributes Voeten, Strezhnev & Bailey "United Nations General Assembly Voting Data" (hdl:1902.1/12379).
# The package covers 1946-2019 only, and drops absences (absent = missing, i.e. the paper's M1 treatment).
.libPaths(c("~/R/library", .libPaths()))
suppressMessages({library(unvotes); library(tidyverse)})
args <- commandArgs(trailingOnly = TRUE); out <- if (length(args)) args[1] else "data/plenary_unvotes_2018_2019.csv"
map <- c("Antigua & Barbuda"="Antigua and Barbuda","Bolivia"="Bolivia (Plurinational State of)","Bosnia & Herzegovina"="Bosnia and Herzegovina",
 "Brunei"="Brunei Darussalam","Cape Verde"="Cabo Verde","Congo - Brazzaville"="Congo","Congo - Kinshasa"="Democratic Republic of the Congo",
 "Iran"="Iran (Islamic Republic of)","Laos"="Lao People’s Democratic Republic","Moldova"="Republic of Moldova","Myanmar (Burma)"="Myanmar",
 "Netherlands"="Netherlands (Kingdom of the)","North Korea"="Democratic People’s Republic of Korea","Russia"="Russian Federation",
 "São Tomé & Príncipe"="Sao Tome and Principe","South Korea"="Republic of Korea","St. Kitts & Nevis"="Saint Kitts and Nevis","St. Lucia"="Saint Lucia",
 "St. Vincent & Grenadines"="Saint Vincent and the Grenadines","Syria"="Syrian Arab Republic","Tanzania"="United Republic of Tanzania",
 "Trinidad & Tobago"="Trinidad and Tobago","Turkey"="Türkiye","United Kingdom"="United Kingdom of Great Britain and Northern Ireland",
 "United States"="United States of America","Venezuela"="Venezuela (Bolivarian Republic of)","Vietnam"="Viet Nam","Yugoslavia"="Serbia")
rc <- un_roll_calls %>% filter(format(date, "%Y") %in% c("2018","2019"))
v <- un_votes %>% semi_join(rc, by = "rcid") %>% mutate(country = recode(country, !!!map),
  vote = recode(as.character(vote), yes = "Yes", no = "No", abstain = "Abstain")) %>%
  left_join(rc %>% transmute(rcid, year = as.integer(format(date, "%Y")), session, unres), by = "rcid") %>%
  transmute(vote_id = paste0("PLEN_", rcid), year, session, unres, country, vote)
write_csv(v, out); cat("rows", nrow(v), "roll calls", n_distinct(v$vote_id), "countries", n_distinct(v$country), "\n")
