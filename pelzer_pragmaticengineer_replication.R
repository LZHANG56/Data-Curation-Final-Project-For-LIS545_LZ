# Data supplement to "Enter the Pragmatic Engineer"
# Thorben PELZER 2024

# Supplement to the journal article
# "Enter the Pragmatic Engineer" by Thorben PELZER,
# History of Education, 2024.

# 1. Locating CERD data files

# This script has been created using data from
# the Pelzer et al., Chinese Engineers Relational Database (CERD),
# version 1.7.0. The script should also work with future versions
# of the database export. The newest version of CERD can
# be found at http://doi.org/10.5281/zenodo.4075601.
# In case a newer version has been downloaded, please change
# the file names in the following lines before running the script:

CERD_persons_file <- "CERD/pelzer_cerd_170_persons.csv"
CERD_occupations_file <- "CERD/pelzer_cerd_170_occupations.csv"
CERD_employers_file <- "CERD/pelzer_cerd_170_employers.csv"
CERD_degrees_file <- "CERD/pelzer_cerd_170_degrees.csv"
CERD_colleges_file <- "CERD/pelzer_cerd_170_colleges.csv"
CERD_locations_file <- "CERD/pelzer_cerd_170_locations.csv"

# 2. Loading libraries

## Function to check which libraries need to be installed (Matthew@StackOverflow)
using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  n<-length(need)
  if(n>0){
    libsmsg<-if(n>2) paste(paste(need[1:(n-1)],collapse=", "),",",sep="") else need[1]
    print(libsmsg)
    if(n>1){
      libsmsg<-paste(libsmsg," and ", need[n],sep="")
    }
    libsmsg<-paste("The following packages could not be found: ",libsmsg,"\n\r\n\rInstall missing packages?",collapse="")
    if(winDialog(type = c("yesno"), libsmsg)=="YES"){       
      install.packages(need)
      lapply(need,require,character.only=TRUE)
    }
  }
}

## Load (or install) required libraries
using("plyr","tidyverse","stringi","scales","readr","dplyr","reshape2","DescTools")


# 3.1 Loading and wrangling CERD

## Load persons file
CERD_persons <- read_csv(CERD_persons_file)

## Create field for whole Hanyu pinyin name
CERD_persons$name_PY <- paste(CERD_persons$"Family name (Hanyu pinyin)",CERD_persons$"Given name (Hanyu pinyin)")

## Rename columns and select the relevant ones
CERD_persons <- CERD_persons %>%
  rename (ID_PERSON="Person H-ID",name_ZH="Chinese characters",birthyear="Birth date",LOC_JG="Native place H-ID",gender="Gender") %>%
  select (c("ID_PERSON","name_PY","name_ZH","gender","birthyear","LOC_JG"))

## Normalize birthdate
CERD_persons$birthyear = as.integer(str_extract(as.character(CERD_persons$birthyear), "\\d{4}"))

## Load occupations data, rename columns, select the relevant ones
CERD_occupations <- read_csv(CERD_occupations_file) %>%
  select (-c("rec_Title","Range - End date")) %>%
  rename (ID_OCC="Occupation H-ID",ID_ORG="Organisation H-ID",ID_PERSON="Parent entity (Person) H-ID",date="Range - Start date") %>%
  separate_rows("Original occupation text", sep = "\\|") %>%
  rename (position="Original occupation text")

## Load employers data, rename columns, select the relevant ones
CERD_employers <- read_csv(CERD_employers_file) %>%
  select (-c("rec_Title","Short name / acronym","Organisation type")) %>%
  rename(ORG="Full name of organisation",ID_ORG="Organisation H-ID",ID_ORG2="Mother organisation H-ID",ID_LOC="Location (places) H-ID")

## Load degrees data, rename columns, select the relevant ones
CERD_degrees <- read_csv(CERD_degrees_file) %>%
  select (-c("rec_Title","Start of studies")) %>%
  rename(ID_DEGREE="Tertiary study H-ID",ID_PERSON="Parent entity (Person) H-ID",ID_COL="Educational institution H-ID",
         date="Date awarded",field="Field(s) / major(s)",degree="Degree") %>%
  
  ## One row per major
  separate_rows("field", sep = "\\|") %>%
  
  ## Normalize names of majors
  mutate(field = recode(field, "Metallurgy 冶金" = 'Mining/Metallurgy', "Mining 採冶" = 'Mining/Metallurgy', "Mining Engineering 礦冶" =  'Mining/Metallurgy' ,
                        "Telecommunications 電信" = 'Electrical Engineering 電機', "Telecommunications 電訊" = 'Electrical Engineering 電機',
                        "Radio Engineering 無線電" = 'Electrical Engineering 電機', "Telecommunications 郵電" = 'Electrical Engineering 電機',
                        "Electrical Engineering 電工" = 'Electrical Engineering 電機', "Electrical Engineering 電機工程" = 'Electrical Engineering 電機',
                        "Metallurgical Engineering 冶金工程" = 'Mining/Metallurgy', "Mining 採礦" = 'Mining/Metallurgy', "Advanced Electricity 高等電氣" = 'Electrical Engineering 電機',
                        "Electrical Engineering 電氣工程" = 'Electrical Engineering 電機', "Electricity 電氣" = 'Electrical Engineering 電機'))

## Deselect degree type
CERD_degrees <- CERD_degrees %>% select(-c("degree"))

## Shorten name of majors
CERD_degrees$field <- ifelse(
  substr(CERD_degrees$field,1,1) %like% "[A-Za-z]",
  CERD_degrees$field %>% str_replace_all("/"," ") %>% str_replace_all("[[\\p{Han}]]", ""),
  CERD_degrees$field)

## Load colleges data, rename columns, select the relevant ones
CERD_colleges <- read_csv(CERD_colleges_file) %>%
  select (-c("rec_Title")) %>%
  rename(ID_COL="Educational institution H-ID",ID_LOC="Place H-ID",COL="Name of institution")

## Load locations data
CERD_locations <- read_csv(CERD_locations_file) %>%
  
## Create distinct fields for latitude and longitude
rename(LOC_joined="Location (mappable)")
CERD_locations$LOC_joined <- CERD_locations$LOC_joined %>% str_replace_all("POINT\\(","") %>% str_replace_all("\\)","")
CERD_locations <- CERD_locations %>%
  separate(col=LOC_joined,into=c("LONG","LAT"),sep=" ")
  CERD_locations$LAT <- CERD_locations$LAT %>% as.numeric()
  CERD_locations$LONG <- CERD_locations$LONG %>% as.numeric()

## Rename columns, select the relevant ones
CERD_locations <- CERD_locations %>% 
  select (-c("rec_Title")) %>%
  rename (ID_LOC="Place H-ID",ID_LOC2="Province (Place) H-ID",COUNTRY="Country",LOC="Primary place name",
    type="Place type",name="Primary place name")
CERD_locations$type <- CERD_locations$type %>% str_replace_all("[[\\p{Han}]]", "")

## Shorten names of locations
CERD_locations$name <-  CERD_locations$name %>% str_replace_all("\\)","") %>% str_replace_all("\\(","")
CERD_locations$name <- ifelse(
  substr(CERD_locations$name,1,1) %like% "[A-Za-z]",
  CERD_locations$name %>% str_replace_all("/"," ") %>% str_replace_all("[[\\p{Han}]]", ""),
  CERD_locations$name)

# 3.2 Get provinces for each location

## Temporary copy of locations data-frame        
CERD_locations_temp <- CERD_locations %>%
  select(-c("COUNTRY","LONG","LAT")) %>%
  rename(ID_TEMP=ID_LOC2,ID_LOC2=ID_LOC)

## Select only relevant fields
CERD_locations_temp2 <- CERD_locations %>%
  select(-c("LONG","LAT"))

## Merger of data-frame copy with original
CERD_locations_temp2 <- CERD_locations_temp2 %>%
  left_join(CERD_locations_temp,by="ID_LOC2")

## Rename column
CERD_locations_temp <- CERD_locations_temp %>% select (-c("ID_TEMP")) %>% rename(ID_TEMP=ID_LOC2)

## Merger of data-frame copy with data-frame merger                                                       
CERD_locations_temp2 <- CERD_locations_temp2 %>%
  left_join(CERD_locations_temp,by="ID_TEMP") %>%
  select(-c("ID_TEMP","ID_LOC2"))

## Take name from copy where no province name exists yet
CERD_locations_temp2 <- CERD_locations_temp2 %>%
  mutate(province=  
           ifelse(is.na(CERD_locations_temp2$name)==F,
                  CERD_locations_temp2$name,
                  ifelse(is.na(CERD_locations_temp2$name.y)==F,
                         CERD_locations_temp2$name.y,
                         CERD_locations_temp2$name.x
                  ))) %>%
  select("ID_LOC","province")

## Link generated provinces to original locations data-frame  
CERD_locations <- CERD_locations %>%
  left_join(CERD_locations_temp2,by="ID_LOC") %>%
  select (-c("ID_LOC2"))

# 3.3 Fix consistency

## Returning Kwantung Leased Territory to China
CERD_locations$COUNTRY[CERD_locations$ID_LOC==31950] <- "China"

## Make sure Taiwan is a Japanese colony
CERD_locations$COUNTRY[CERD_locations$COUNTRY=="Taiwan"] <- "Japan"

## Normalize names of provinces
CERD_locations$province <- ifelse(CERD_locations$COUNTRY=="China",CERD_locations$province %>% str_replace_all(" ","") %>%
                                    str_replace_all("ZhiliHebei","Zhili") %>% str_replace_all("FengtianLiaoning","Fengtian")  %>%
                                    str_replace_all("SonghuShanghai","Shanghai") %>% str_replace_all("CapitalAreaBeijing","Beijing"),CERD_locations$province)

## Declare Qingdao as part of Shandong
CERD_locations$province[CERD_locations$province=="Jiao'ao(Kiautschou)"] <- "Shandong"


# 4. Statistics used in the article

## Nanyang graduates working at railroad company
CERD_persons %>% full_join(CERD_degrees,by="ID_PERSON") %>%
  filter(field=="Civil Engineering " | field=="Mechanical Engineering ") %>%
  filter(date > 1911 & date < 1950) %>%
  filter(ID_COL==235) %>%
  select(ID_PERSON) %>%
  unique() %>%
  left_join(CERD_occupations,by="ID_PERSON") %>%
  drop_na() %>%
  left_join(CERD_employers,by="ID_ORG") %>%
  select(ID_PERSON,ORG,ID_ORG2) %>%
  rename(ID_ORG=ID_ORG2) %>%
  left_join(CERD_employers,by="ID_ORG") %>%
  unique() %>%
  filter(grepl("鐵路",ORG.x)==T | grepl("鐵道",ORG.x)==T | grepl("Railway",ORG.x)==T | grepl("Railroad",ORG.x)==T) %>%
  select(ID_PERSON) %>%
  unique() %>%
  count() / ## All Nanyang graduates with a known occupation
  CERD_persons %>% full_join(CERD_degrees,by="ID_PERSON") %>%
  filter(field=="Civil Engineering " | field=="Mechanical Engineering ") %>%
  filter(date > 1911 & date < 1950) %>%
  filter(ID_COL==235) %>%
  select(ID_PERSON) %>%
  unique() %>%
  left_join(CERD_occupations,by="ID_PERSON") %>%
  drop_na() %>%
  select(ID_PERSON) %>%
  unique() %>%
  count()

## Graduates from domestic colleges with a known occupation
CERD_persons %>% full_join(CERD_degrees,by="ID_PERSON") %>%
  filter(field=="Civil Engineering " | field=="Mechanical Engineering ") %>%
  filter(date > 1911 & date < 1950) %>%
  filter(ID_COL!=235) %>%
  left_join(CERD_colleges,by="ID_COL") %>%
  left_join(CERD_locations,by="ID_LOC") %>%
  filter(COUNTRY=="China") %>%
  select(ID_PERSON) %>%
  unique() %>%
  left_join(CERD_occupations,by="ID_PERSON") %>%
  drop_na() %>%
  left_join(CERD_employers,by="ID_ORG") %>%
  select(ID_PERSON,ORG,ID_ORG2) %>%
  rename(ID_ORG=ID_ORG2) %>%
  left_join(CERD_employers,by="ID_ORG") %>%
  unique() %>%
  filter(grepl("鐵路",ORG.x)==T | grepl("鐵道",ORG.x)==T | grepl("Railway",ORG.x)==T | grepl("Railroad",ORG.x)==T) %>%
  select(ID_PERSON) %>%
  unique() %>%
  count() / ## All domestic graduates working at a railroad company
  CERD_persons %>% full_join(CERD_degrees,by="ID_PERSON") %>%
  filter(field=="Civil Engineering " | field=="Mechanical Engineering ") %>%
  filter(date > 1911 & date < 1950) %>%
  filter(ID_COL!=235) %>%
  left_join(CERD_colleges,by="ID_COL") %>%
  left_join(CERD_locations,by="ID_LOC") %>%
  filter(COUNTRY=="China") %>%
  select(ID_PERSON) %>%
  unique() %>%
  left_join(CERD_occupations,by="ID_PERSON") %>%
  drop_na() %>%
  select(ID_PERSON) %>%
  unique() %>%
  count()