## title: "Authorship Scorecard"
## author: "Savet Hong"
## date: "May 7, 2019"
## Purpose: Create a blank df for all TeamPSD memmbers
##
##################################################

library(tidyverse)
library(readxl)
library(stringr)

##################
## Load the Data
##################
p <- read_excel("TeamPSD_members.xlsx", sheet = "people")
topic <- read_excel("TeamPSD_members.xlsx", sheet = "categories")
manu <- read_excel("TeamPSD_members.xlsx", sheet = "manuscripts")

##################
## Setup the Data
##################
# Identify existing manuscripts and people listed
manu_clean <- manu %>%
  filter(!is.na(Manuscript)) %>%
  gather(np, participants, -Manuscript) %>%
  filter(!is.na(participants)) %>%
  mutate(fstname = participants,
         fstname = sub(" [[:alpha:]]*$", "", fstname),
         fstname = sub(" [[:alpha:]]*-[[:alpha:]]*$", "", fstname),
         lname = sub(".* (.*)$", "\\1", participants),
         fname = paste0(lname, ", ", fstname)) %>%
  select(Manuscript, fname) %>%
  arrange(Manuscript, fname) %>%
  rename(Paper = Manuscript)

prj <- unique(manu_clean$Paper)


# People on the project as potential co-authors
people <- p %>%
  mutate_if(is.character, str_trim) %>%
  mutate(fname = paste(`Last Name`, ", ", `First Name`," ", `Middle Initial`, 
                       sep = ""),
         fname = sub(" NA$", "", fname)) %>%
  arrange(fname) 

# Check People
pcheck <- full_join(people, manu_clean, by = "fname")

tm_missing <- pcheck %>%
  filter(is.na(Group)) %>%
  select(Paper, fname)

mn_missing <- pcheck %>%
  filter(is.na(Paper)) %>%
  select(Group, fname)

    ##  remove individuals from the tm_missing from the master list
    ## of potential co-authors

#Create list of df of manuscript/authors/sections
mfin <- anti_join(manu_clean, tm_missing)
sect <- unique(topic$Section)

mfin_topic <- mfin %>%
  cbind.data.frame(data.frame(matrix(vector(), nrow(mfin), length(sect),
                                     dimnames = list(c(), sect)),
                              stringsAsFactors = FALSE)) %>%
  gather(Section, value, setdiff(names(.), names(mfin))) %>%
  select(-value) %>%
  left_join(topic, by = "Section")

manu_list <- split(mfin_topic, f = mfin_topic$Paper)


#Create Genneric blank df
df <- topic %>%
  cbind.data.frame(data.frame(matrix(vector(), nrow(topic), nrow(people),
                 dimnames = list(c(), people$fname)),
                 stringsAsFactors = FALSE)) %>%
  mutate_at(4:(nrow(people)+4), as.numeric)
  
names(df) <- c(names(topic),people$fname)
##################
## Save/Export the Data
##################

# library("xlsx")
# write.xlsx(tm_mising, file = "Missing_members.xlsx", sheetName = "Missing Team Member",
#            append = FALSE)

# library(openxlsx)
# wb <- createWorkbook("wb")
# addWorksheet(wb, "Missing Members")
# addWorksheet(wb, "Missing Authors")
# writeDataTable(wb, "Missing Members", tm_missing)
# writeDataTable(wb, "Missing Authors", mn_missing)
# saveWorkbook(wb, "Missing Members.xlsx", overwrite = TRUE)


save(people, manu_list, df, file = "auth.Rdata")
