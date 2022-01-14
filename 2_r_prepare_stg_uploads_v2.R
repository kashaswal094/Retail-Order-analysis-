
rm(list = ls())

    library('tidyverse')
    library('reticulate')
    library('googledrive')

options(warn=-1)





raw_data_dir_ <- 'KE_QA_2021_22/Term 3/2. Report Downloads'
output_dir_ <- 'KE_QA_2021_22/Term 3/3. STG Databases/3. Output'

setwd("G:/My Drive/KE_QA_2021_22/Term 3/3. STG Databases/2. Scripts")
getwd()

sys_date <- Sys.Date()
date <- paste(substr(sys_date,1,4),"_", substr(sys_date,6,7),"_",substr(sys_date,9,10),sep="")
dow <- weekdays(sys_date)

if(dow=="Monday"){
  survey_visit <- paste(substr(sys_date-3,1,4),"_", substr(sys_date-3,6,7),"_",substr(sys_date-3,9,10),sep="")
}else{
  survey_visit <- paste(substr(sys_date-1,1,4),"_", substr(sys_date-1,6,7),"_",substr(sys_date-1,9,10),sep="")
}

files_to_be_present <- c(
  'Active Pupil Contact', 'Roster of Pupils - Active Only (Pupil Level)', 
  'Active Teachers and Academy Managers' 
)

file_active_pupils <- paste('Roster of Pupils - Active Only (Pupil Level)_',survey_visit, '.csv', sep='')
pupil_status_for_stg <- paste(survey_visit, '_0_Pupil Status By Bill cleaned', '.csv', sep='')

reported_file_path="G:\\My Drive\\KE_QA_2021_22\\Term 3\\2. Report Downloads\\"

pupil_df <- read_csv(paste(reported_file_path,file_active_pupils,sep=''), 
  skip=2, col_names=TRUE, col_types = cols()) %>%rename('CurrentStatus'='Status')

colnames(pupil_df)

# Select relevant variables and rename to match database output: 

pupil_df_renamed = pupil_df %>% 
  select("PupilID", "AcademyCode", "CurrentTotalBalance", "CurrentStatus", 
         "GradeName", 'Stream', 'FirstName', 'MiddleName', 'LastName') %>% 
  rename("AcademyName"="AcademyCode", "TotalBalance"="CurrentTotalBalance") %>% 
  filter(is.na(PupilID)==FALSE)


# Clean pupil name columns and combine into a single column:
pupil_df_renamed["PupilName"]=paste(pupil_df_renamed$FirstName,pupil_df_renamed$MiddleName,pupil_df_renamed$LastName)
pupil_df_renamed["PupilName"]=str_replace_all(pupil_df_renamed[["PupilName"]],"NA","")
pupil_df_renamed$PupilName <- str_replace_all(pupil_df_renamed$PupilName, "\\s+"," ")




# 
# for (var in c('FirstName', 'MiddleName', 'LastName')) {
#   
#   pupil_df_renamed[[var]] <- ifelse(is.na(pupil_df_renamed[[var]])==TRUE,"",pupil_df_renamed[[var]])
#   
#   pupil_df_renamed[[var]] <- str_replace_all(pupil_df_renamed[[var]],",","")
#   
#   pupil_df_renamed[[var]] <- str_replace_all(pupil_df_renamed[[var]],"\r?\n|\r","")
#   
# }
# 
# # Karishma to write docs
# pupil_df_renamed$PupilName <- str_to_title(
#   paste(pupil_df_renamed$FirstName, 
#         pupil_df_renamed$MiddleName, 
#         pupil_df_renamed$LastName, sep = " "))
# # Karishma to write docs
# pupil_df_renamed$PupilName <- str_replace_all(
#   pupil_df_renamed$PupilName, "\\s+"," ")
# 
# 

# Define the CurrentStatus Variable so the two options (- and +) match the database output:

pupil_df_defined <- pupil_df_renamed %>%mutate(CurrentStatus = ifelse(CurrentStatus=='Paid In Full',"+", 
                                ifelse(CurrentStatus=='Overdue - Grace', "+", 
                                       ifelse(CurrentStatus=='Overdue - No Service', "-", "NA")))
  ) 

# Clean up the Academy Names:
# Remove apostrophes from academies with apostrophes
pupil_df_defined$AcademyName <- gsub("'", '', pupil_df_defined$AcademyName)

# Add ECE to Primary Classes
pupil_df_defined$GradeName <- ifelse(pupil_df_defined$GradeName == "Baby Class", "ECD 1: Baby Class",
                                     ifelse(pupil_df_defined$GradeName == "Pre-primary 1","ECD 2: Pre-primary 1",
                                                        ifelse(pupil_df_defined$GradeName == "Pre-primary 2", 
                                                            "ECD 3: Pre-primary 2", pupil_df_defined$GradeName))) 
#   pupil_df_defined$GradeName)
# pupil_df_defined$GradeName <- ifelse(
#   pupil_df_defined$GradeName == "Pre-primary 1", 
#   "ECD 2: Pre-primary 1", pupil_df_defined$GradeName)
# pupil_df_defined$GradeName <- ifelse(
#   pupil_df_defined$GradeName == "Pre-primary 2", 
#   "ECD 3: Pre-primary 2", pupil_df_defined$GradeName)

# Define the classroom variable as the concatenation of GradeName and Stream:

pupil_df_defined$classroom <- paste(pupil_df_defined$GradeName, pupil_df_defined$Stream)

# Select the final variables: 

pupil_df_final <- pupil_df_defined %>% select(
  'PupilID', 'AcademyName', 'PupilName', 
  'CurrentStatus', 'TotalBalance', 
  'GradeName', 'Stream', 'classroom')%>%filter(pupil_df_defined$GradeName!="Unassigned")

# Check for duplicate Pupil IDs:

dups <- pupil_df_final[duplicated(pupil_df_final$PupilID),]

dim(dups)


# Preview the Output: 
dim(pupil_df_final); 
unique(pupil_df_final$CurrentStatus); unique(pupil_df_final$GradeName)
head(pupil_df_final)

pupil_df_final <- pupil_df_final[!duplicated(pupil_df_final$PupilID),]

# Export to CSV

download_path="G:/My Drive/KE_QA_2021_22/Term 3/3. STG Databases/3. Output/"

write.csv(
  pupil_df_final, paste(download_path,pupil_status_for_stg, sep=''), 
  row.names=FALSE, na =".")



# write.csv(
#   pupil_df_final, paste(getwd(),'/',pupil_status_for_stg, sep=''), 
#   row.names=FALSE, na =".")

# getwd();output_dir_
# 
# 
# drive_upload(
#   paste(getwd(),'/',pupil_status_for_stg, sep=''), 
#   path = paste(output_dir_,'/',pupil_status_for_stg, sep=''), 
#   type=NULL, overwrite=TRUE, verbose=FALSE)
# 
# 

message_prep_pupil_status <- paste(
  "PUPIL DATABASE: Dataset with ", nrow(pupil_df_final), 
  " pupils exported for ", date, 
  sep="")


file_active_teachers <- paste(
  'Active Teachers and Academy Managers_', survey_visit, '.csv', sep='')

academy_roster_for_stg <- paste(
  survey_visit, '_0_Academy Roster cleaned', '.csv', sep='')

# Downloading csv from drive
dat=read_csv(paste(reported_file_path,file_active_teachers,sep = ''),
  col_names=TRUE, col_types = cols())
  

# drive_download(
#   file = paste(raw_data_dir_,'/',file_active_teachers, sep=''),
#   path = paste(getwd(),'/',file_active_teachers, sep=''),
#   type=NULL, overwrite=TRUE, verbose=FALSE
# )
# 
# dat <- read_csv(
#   paste(getwd(),'/',file_active_teachers, sep=''), 
#   col_names=TRUE, col_types = cols())

needed_columns <- c("EmpID", "EmpName", "Academycode", 
                    "JobTitle","AssignedGrade","AssignedClassroom")

dat_reduced <- dat %>% 
  #select(all_of(needed_columns)) %>% 
  select(needed_columns) %>% 
  rename('AcademyName'='Academycode', 
         'GradeName'='AssignedGrade', 
         'Stream'='AssignedClassroom')

dat_reduced$classroom <- paste(
  dat_reduced$GradeName," ",dat_reduced$Stream, sep="")



# Remove Duplicates & Entries without Assignment
dat_final <- dat_reduced[!duplicated(dat_reduced$EmpID), ]
dat_final <- dat_final[!is.na(dat_final$AcademyName), ]

# Remove line breaks and carriage returns
dat_final$EmpName <- gsub("\r?\n|\r","",dat_final$EmpName)

# Add a "Teacher Missing from List" for each school
list_of_schools <- dat_final  

list_of_schools$EmpID <- NA
list_of_schools$EmpName <- NA
list_of_schools$JobTitle <- "Permanent Teacher"
list_of_schools$classroom <- NA
list_of_schools$GradeName <- NA
list_of_schools$Stream <- NA

unique_schools <- list_of_schools[!duplicated(list_of_schools),]
unique_schools  <- unique_schools %>% 
  mutate(count = seq(n()))
unique_schools$EmpID <- unique_schools$count
needed_columns <- c("EmpID" ,"EmpName", "AcademyName", 
                    "JobTitle", "GradeName", "Stream", "classroom")
unique_schools <- unique_schools[ , needed_columns]

unique_schools$EmpName <- "*Teacher Departed, Current Vacancy"


departed <- unique_schools
departed$EmpID <- departed$EmpID*10 + 1


unique_schools$EmpName <- "*Class De-streamed"
destreamed <- unique_schools
destreamed$EmpID <- destreamed$EmpID*100000 + 1


# Append all these
dat_for_upload <- rbind(dat_final, departed, destreamed)


# Add ECE to Primary Classes
dat_for_upload$GradeName <- ifelse(
  dat_for_upload$GradeName == "Baby Class", 
  "ECD 1: Baby Class", dat_for_upload$GradeName)
dat_for_upload$GradeName <- ifelse(
  dat_for_upload$GradeName == "Pre-primary 1", 
  "ECD 2: Pre-primary 1", dat_for_upload$GradeName)
dat_for_upload$GradeName <- ifelse(
  dat_for_upload$GradeName == "Pre-primary 2", 
  "ECD 3: Pre-primary 2", dat_for_upload$GradeName)

# Remove apostrophes from academies with apostrophes
dat_for_upload$AcademyName <- gsub("'", '', dat_for_upload$AcademyName)

#Remove apostrophes from names
dat_for_upload$EmpName <- gsub("'"," ",dat_for_upload$EmpName)

# Export to CSV

download_path="G:/My Drive/KE_QA_2021_22/Term 3/3. STG Databases/3. Output/"

write.csv(
  dat_for_upload, paste(download_path,academy_roster_for_stg, sep=''), 
  row.names=FALSE, na =".")

# write.csv(
#   dat_for_upload, 
#   paste(getwd(),'/',academy_roster_for_stg, sep=''),
#   row.names=FALSE, na =".")
# 
# drive_upload(
#   paste(getwd(),'/',academy_roster_for_stg, sep=''), 
#   path = paste(output_dir_,'/',academy_roster_for_stg, sep=''), 
#   type=NULL, overwrite=TRUE, verbose=FALSE)


message_prep_academy_roster <- paste(
  "ACADEMY ROSTER: Dataset with ", nrow(dat_for_upload), 
  " teachers exported for ", date, 
  sep="")

#Remove un-needed dataframes and values
rm(dat, dat_final, dat_for_upload, dat_reduced, departed, destreamed, 
   list_of_schools, unique_schools, academy_roster_for_stg, needed_columns)





























