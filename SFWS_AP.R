# SFWS_Action_plan
library(tidyverse)
library(readxl)

setwd("/Users/richtyler/Documents/Repositories/SFWS")

capwords = function(s, strict = FALSE) {
  cap = function(s) paste(toupper(substring(s, 1, 1)),
                          {s = substring(s, 2); if(strict) tolower(s) else s},sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))}


AP_raw <- read_excel("./SFWS Action Plan.xlsx") %>% 
  mutate(timeframe_short = ifelse(grepl("S", timeframe, ignore.case = TRUE) == TRUE, "shortterm", NA),
         timeframe_medium = ifelse(grepl("M", timeframe, ignore.case = TRUE) == TRUE, "mediumterm", NA),
         timeframe_long = ifelse(grepl("L", timeframe, ignore.case = TRUE) == TRUE, "longterm", NA)) %>% 
  mutate(ongoing = ifelse(grepl("S", timeframe, ignore.case = TRUE) == TRUE & grepl("M", timeframe, ignore.case = TRUE) == TRUE & grepl("L", timeframe, ignore.case = TRUE) == TRUE, "ongoing", NA))  %>% 
  mutate(timeframe_js = trimws(gsub("NA", "", paste(timeframe_short, timeframe_medium, timeframe_long, ongoing, sep = " ")), which = "left")) %>% 
  mutate(level_individual = ifelse(grepl("I", level, ignore.case = TRUE) == TRUE, "individual", NA),
         level_community = ifelse(grepl("C", level, ignore.case = TRUE) == TRUE, "community", NA),
         level_place = ifelse(grepl("P", level, ignore.case = TRUE) == TRUE, "place", NA)) %>% 
  mutate(level_js = trimws(gsub("NA", "", paste(level_individual, level_community, level_place, sep = " ")), which = "left"))

write.csv(AP_raw, "./actionplan_raw.csv", row.names = FALSE, na = "")

# We have to split the code here because the next part calls AP_raw which is not created until the end of the command

AP_raw <- AP_raw %>% 
  mutate(how_many_tf = rowSums(!is.na(AP_raw[c("timeframe_short", "timeframe_medium", "timeframe_long", "ongoing")]))) %>% 
  mutate(timeframe_label = ifelse(how_many_tf == 4, "Ongoing", 
                               ifelse(how_many_tf == 2, trimws(gsub("NA and", "", paste(timeframe_short, timeframe_medium, timeframe_long, sep = " and ")), which = "left"), 
                          ifelse(how_many_tf == 1, timeframe_js, NA)))) %>% 
  mutate(timeframe_label = gsub("NA ", "", timeframe_label)) %>% 
  mutate(timeframe_label = gsub("and NA", "", timeframe_label)) %>% 
  mutate(timeframe_label = gsub("shortterm", "Short term", timeframe_label)) %>% 
  mutate(timeframe_label = gsub("mediumterm", "Medium term", timeframe_label)) %>% 
  mutate(timeframe_label = gsub("longterm", "Long term", timeframe_label)) %>% 
  mutate(how_many_levels = rowSums(!is.na(AP_raw[c("level_individual", "level_community", "level_place")]))) %>% 
  mutate(levels_label = ifelse(how_many_levels == 3, "All levels", 
                        ifelse(how_many_levels == 2, trimws(gsub("NA and", "", paste(capwords(level_individual, strict = TRUE), capwords(level_community, strict = TRUE), capwords(level_place, strict = TRUE), sep = " and ")), which = "left"), 
                        ifelse(how_many_levels == 1, capwords(level_js, strict = TRUE), NA)))) %>% 
  mutate(levels_label = gsub("NA ", "", levels_label)) %>% 
  mutate(partner_label = ifelse(grepl(",", partners, ignore.case = TRUE) == TRUE, "Multiple partners", partners)) %>% 
  mutate(partner_label = gsub("Western Sussex Hospitals NHS Foundation Trust", '<abbr title="Western Sussex Hospital NHS Foundation Trust">WSHFT</abbr>', partner_label)) %>% 
  mutate(partner_js = partners) %>% 
  mutate(partner_js = gsub("Trading Standards", "trading_standards", partner_js)) %>% 
  mutate(partner_js = gsub("Local Maternity System", "lms", partner_js)) %>% 
  mutate(partner_js = gsub("Wellbeing programme", "wellbeing_programme", partner_js, ignore.case = TRUE)) %>% 
  mutate(partner_js = gsub("Fire Service", "fire_service", partner_js)) %>% 
  mutate(partner_js = gsub("Public Health", "public_health", partner_js)) %>% 
  mutate(partner_js = gsub("Maternity at Western Hospitals", "maternity wsht", partner_js)) %>% 
  mutate(partner_js = gsub("Health4Families", "h4f", partner_js)) %>% 
  mutate(partner_js = gsub("Prisons", "prisons", partner_js)) %>% 
  mutate(partner_js = gsub("Communities", "communities", partner_js)) %>% 
  mutate(partner_js = gsub("Western Sussex Hospitals NHS Foundation Trust", "wsht", partner_js)) %>% 
  mutate(partner_js = gsub("Sussex Community NHS Foundation Trust", "scft", partner_js)) %>% 
  mutate(partner_js = gsub("Primary Care", "primary_care", partner_js)) %>% 
  mutate(partner_js = gsub("District & Boroughs", "dandb", partner_js)) %>% 
  mutate(partner_js = gsub(",", "", partner_js)) 

AP_ready <- AP_raw %>% 
  select(ap_number, ap_title, ap_text, success, progress, achieved, partners,partner_label, partner_js, hic_number, hic_label, hic_class, timeframe_js, timeframe_label, level_js, levels_label) %>% 
  mutate(div_1 = gsub("NA", "", paste('<div class = "grid-item', hic_class, timeframe_js, level_js, partner_js,'">', sep = " "))) %>% 
  mutate(div_2 = gsub("NA", "", paste0('<p class = "ap_number">',ap_number,'</p>'))) %>% 
  mutate(div_3 = gsub("NA", "", paste0('<p class = "ap_title">', ap_title,'</p>'))) %>% 
  mutate(div_4 = gsub("NA", "", paste0('<p class = "ap_text">', ap_text,'</p>'))) %>% 
  mutate(div_5 = gsub("NA", "", paste0('<p class = "success">', success,'</p>'))) %>% 
  mutate(div_6 = gsub("NA", "", paste0('<p class = "partner_label">', partner_label,'</p>'))) %>% 
  mutate(div_7 = gsub("NA", "", paste0('<p class = "hic_number">', hic_number,'</p>'))) %>% 
  mutate(div_8 = gsub("NA", "", paste0('<p class = "hic_label">', hic_label,'</p>'))) %>% 
  mutate(div_9 = gsub("NA", "", paste0('<p class = "progress">', progress,'</p>'))) %>%
  mutate(div_10 = gsub("NA", "", paste0('<p class = "achieved">', achieved,'</p>'))) %>%
  mutate(div_11 = gsub("NA", "", paste0('<p class = "timeframe_label">', timeframe_label,'</p>'))) %>% 
  mutate(div_12 = gsub("NA", "", paste0('<p class = "levels_label">', levels_label,'</p>'))) %>% 
  mutate(div_13 = paste0('<div class="tooltip_',hic_class,'">'),
         div_14 = paste0("<h2>",ap_title, "</h2>"),
         div_15 = paste0("<p>", ap_text, "</p>"),
         div_16 = paste0("<h3>What will success look like?</h3>"),
         div_17 = paste0("<p>", success, "</p>"),
         div_18 = paste0("<h3>Who are key partners for this action?</h3>"),
         div_19 = paste0("<p>",partners,"</p>"),
         div_20 = "</div>")

# We could include links or an update of progress etc.

# Note we have removed the progress and achieved divs for the time being!! (div_9 and div_10)
AP_t <- gather(as.data.frame(t(AP_ready[c("div_1","div_2","div_3","div_4","div_5","div_6","div_7","div_8","div_11","div_12","div_13","div_14","div_15","div_16","div_17","div_18","div_19","div_20","div_20")])))





write.csv(AP_t[c("value")], "./actionplan_html.csv", row.names = FALSE)


