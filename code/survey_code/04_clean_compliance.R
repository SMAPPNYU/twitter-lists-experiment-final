##############################################################################
# File-Name: 04_clean_compliance.r
# Date: 2023
# author: Tiago Ventura
# Purpose: Main script to clean the data coming from the compliance submissions survey 
# Data in: "data/pilot/compliance/compliance_raw.csv"
# Data out: "data/pilot/compliance/compliance_processed.csv" - cleaned data
#           "data/pilot/compliance/compliance_processed.rds" rds format
# status: ongoing
# Machine: MacOS High Sierra
##############################################################################

# Packages ----------------------------------------------------------------
pacman::p_load(tidyverse, scales, lubridate, here, janitor, 
               rebus, RColorBrewer, wesanderson, qualtRics, tidylog)

source(here("code", "survey_code", "utils.R"))

# open data set -----------------------------------------------------------

d_clean = read_csv(here("data", "pilot", "compliance", "compliance_raw_iid.csv"))

# Clean colnames ----------------------------------------------------------

# Remove allspaces, trim, and capital letters
d <- d_clean %>% 
  clean_names()

# check 
dim(d)
colnames(d)

# Remove test emails and incompletes------------------------------------------------------
d <- d %>%
  select(email=recipient_email, everything()) %>%
  # remove tests with treatment and control email
  filter(!str_detect(email, "treatment|control")) %>%
  filter(!is.na(email)) 
  

# check
glimpse(d)


# rename columns ----------------------------------------------------------

d <- d %>% 
  rename_all(~str_replace_all(.x, "qid8", "submission_type")) %>%
  rename_all(~str_replace_all(.x, "qid16", "image_treatment")) %>%
  rename_all(~str_replace_all(.x, "qid27", "text_image_treatment")) %>%
  rename_all(~str_replace_all(.x, "qid9", "image_control")) %>%
  rename_all(~str_replace_all(.x, "qid34", "text_image_control")) %>%
  rename_all(~str_replace_all(.x, "qid20", "link_control")) %>%
  rename_all(~str_replace_all(.x, "qid31", "text_link_control")) %>%
  rename_all(~str_replace_all(.x, "qid18", "link_treatment")) %>%
  rename_all(~str_replace_all(.x, "qid28", "text_link_treatment")) %>% glimpse()


# unify text --------------------------------------------------------
d <-  d %>% 
  # replace NA by empty string
  mutate(across(contains("text_"), ~ifelse(is.na(.x), "", .x))) %>%
  rowwise() %>%
  # paste strings by row. 
  mutate(submission_reasoning=paste0(text_image_treatment, text_image_control, text_link_control, text_link_treatment)) %>% 
  # remove original variables
  select(-contains("text_"))



# keep only id for images -------------------------------------------------
d <- d %>%
      select(!matches("image_.*_(name|type|size)")) 


# variables for analysis --------------------------------------------------

# total number of submissions
total <- d %>%
  filter(finished==TRUE) %>%
  count(email, name="compliance_all")

# merge
d <- left_join(d, total)

# total number of days submitted

one_per_day <- d %>%
  filter(finished==TRUE) %>%
  mutate(day=ymd(as_date(start_date))) %>%
  count(day, email) %>%
  ungroup() %>%
  count(email, name="compliance_per_day")

d <- left_join(d, one_per_day)

# total number of links/screenshot
temp <- d %>%
  filter(finished==TRUE) %>%
  count(email, submission_type, name="compliance_all") %>%
  filter(!is.na(submission_type)) %>%
  pivot_wider(values_from=compliance_all, 
              names_from=submission_type) %>%
  set_names(~c("email", "n_image", "n_links")) %>%
  mutate_all(~ifelse(is.na(.x), 0, .x))

d <- left_join(d, temp)


# number of participants by group
d %>%
  count(email, exp) %>%
  count(exp)

# Download images ---------------------------------------------------------
d %>%
  mutate(image_ids=ifelse(is.na(image_treatment_id), image_control_id, image_treatment_id)) %>%
  select(email, exp, image_ids, submission_type) %>%
  mutate(image_links=paste0("https://nyu.qualtrics.com/Q/File.php?F=", image_ids)) 



# select only the images
d_images <- d %>%
  # combine treatment and control
  mutate(image_ids=ifelse(is.na(image_treatment_id), image_control_id, image_treatment_id)) %>%
  # remove missing: missing comes from links and from people who did not finish the uploads
  filter(!is.na(image_ids)) %>%
  # select the most important vars
  select(email, exp, image_ids, submission_type) %>%
  # create the link to access qualtrics library
  mutate(image_links=paste0("https://nyu.qualtrics.com/Q/File.php?F=", image_ids)) %>%
  # group by email to get the number of the submission
  group_by(email) %>%
  mutate(id = row_number()) %>%
  ungroup() %>%
  # reorder
  select(id, everything()) 

#function to check image upload
is_image_upload <- function(data, var){
  data <- data %>%
    mutate(upload_ok=ifelse(nchar({{var}})>45, 1, 0))
  
  warning(paste("Number of rows is ", nrow(data), 
                "Number of Uploads is", sum(data$upload_ok)))
  
  return(data)
  
}

# check images
d_images <- d_images %>%
  is_image_upload(image_links)

# collect compliance
# get folder names
d_images <- d_images %>%
  mutate(folder_name=str_remove(str_extract(email, "(.*?)@"), "@")) %>%
  arrange(email) %>%
  mutate(to_save=paste0(folder_name, "_", id, ".png"))

# 
# # open selenium and login manually at my nyu.
# # Need that so that I can access my qualtrics account
# # my nyu login goes by hand. I can automatize that later. 
# library(RSelenium)
# library(netstat)
# rD <- rsDriver(browser = c("firefox"),
#                port = free_port(), 
#                chromever = "104.0.5112.79")
# 
# # function to create a dir and save the files
# View(compliance)
# 
# create_and_save <- function(rD, user, link, to_save){
#   
#   # create directory  
#   if(file.exists(here("data", "pilot", "images", user))){
#     print(paste("dir", user, "exists"))
#   } else dir.create(here("data", "pilot", "images", user))
#   
#   # open driver with image
#   rD$client$navigate(link)    
#   Sys.sleep(5)  
#   
#   # save a screenshot with a new name
#   rD$client$screenshot(file=here("data", "pilot", "images", user, to_save))    
#   Sys.sleep(2)  
#   
#   print(paste("Saving", user, to_save))
# }
# 
# # Running
# 
# rD$client$navigate(d_images$image_links[[400]])
# 
# # create files names
# for (i in 800:nrow(d_images)){
#   
#   create_and_save(rD, d_images$folder_name[[i]], d_images$image_links[[i]], d_images$to_save[[i]])  
#   print(i)
# }
# 
# 
# # save csv
# files <- list.files(here("data", "pilot", "images"), recursive = TRUE)
# 


# Merge back with screenshot results --------------------------------------

out <- read_csv(here("data", "pilot", "compliance", "output_screenshots.csv"))

out <- out %>%
  select(id=`Image Name`, VerifyThis, snopes, AFPFactCheck, LeadStoriesCom, APFactCheck, 
         erumors, factcheckdotorg, PolitiFact) %>%
  clean_names() %>%
  pivot_longer(cols=-id,
               names_to="fct_org", 
               values_to="n") %>%
  filter(n==1)


# merge and some manual checks
# left_join(d_images, out, by=c("to_save"="id")) %>% filter(exp=="treatment") %>%
#   filter(is.na(fct_org)) %>%
#   select(email, exp, submission_type, to_save, fct_org) %>%
#   filter(exp=="treatment") %>%
#   write_csv(here("data","pilot", "compliance", "manual_checks_compliance.csv"))

# reopen with manual classification
manual_class <- read_csv(here("data","pilot", "compliance", "manual_checks_compliance_complete.csv"))

# some screenshots get multiple authors
out <- out %>%
  group_by(id) %>%
  slice(1)

# join images with classification
d_images_n <- left_join(d_images, out, by=c("to_save"="id")) %>%
  # select a few columns
  select(email, exp, submission_type, to_save, fct_org) %>%
  # left_join with the cases who were NA from ocr, and thast we classified manually
  left_join(manual_class %>% select(everything(), fct_org_manual=fct_org)) %>% 
  # merge two measures - ocr and manual 
  mutate(fct_org=ifelse(is.na(fct_org), fct_org_manual, fct_org)) %>% 
  # remove the manual and keep only unified
  select(everything(), -fct_org_manual) %>%
  # count number of screenshots per organization
  count(email, fct_org) %>%
  # remove NAs -- people who screenshots were not from the organizations after we checkd cases by case
  filter(!is.na(fct_org)) 



# Get number of each organization from links ------------------------------
organization = c("snopes", "politifact", "verifythis", "afpfactcheck", "factcheckdotorg", "leadstoriescom","apfactcheck", "erumors")

# organization to rename
organization_names <- list("lead_stories_com"="leadstoriescom", 
                           "snopes"="snopes", 
                           "verify_this"="verifythis",
                           "afp_fact_check"=  "afpfactcheck", 
                           "factcheckdotorg"="factcheckdotorg", 
                           "politi_fact"  = "politifact", 
                           "ap_fact_check"="apfactcheck")

d_links <- d %>%
  select(email, submission_type, link_control, link_treatment) %>%
  mutate(link=ifelse(is.na(link_control), link_treatment, link_control)) %>%
  select(email, submission_type, link) %>%
  # extract the author from the domain
  mutate(author=str_to_lower(str_extract(link, "(?<=twitter\\.com\\/)(.*?)(?=\\/status)")))%>%
  # filter to where author is not na and where fct_checking organizations were the authors (both contorl and treatment)
   filter(!is.na(author),str_detect(author, paste0(organization, collapse="|"))) %>%
  # rename 
  mutate(fct_org=fct_recode(author, !!!organization_names)) %>%
  select(email, fct_org) %>%
  # sames as above 
  count(email, fct_org) %>%
  # remove NAs -- people who screenshots were not from the organizations after we checkd cases by case
  filter(!is.na(fct_org)) 


# combine data from both methods of compliance
d_compliance_images <- bind_rows(d_links, d_images_n) %>%
  # few participants submitted using both, so we need to sum their submissions
  group_by(email, fct_org) %>%
  summarise(n=sum(n)) %>%
  pivot_wider(names_from=fct_org,
              values_from=n, 
              values_fill=0) %>%
  rowwise() %>%
  mutate(correct_fct_submissions=sum(c_across(lead_stories_com:ap_fact_check))) 

# Merge with main compliance aggregated at email email ----------------------------------------------
comp <- d %>%
  select(email, exp, compliance_all, compliance_per_day, n_image, n_links) %>%
  distinct() %>%
  left_join(d_compliance_images) 

comp <- comp %>%
        mutate_at(vars(compliance_all:correct_fct_submissions), ~ifelse(is.na(.x), 0, .x)) %>%
        mutate(compliers=ifelse(correct_fct_submissions>9, 1, 0))


# Output ------------------------------------------------------------------

# saving as csv
write_csv(comp, here("data", "pilot","compliance", "compliance_clean_data.csv"))

# saving as  Rdata ~ preserves ordering in the cleaning

saveRDS(comp, file = here("data","pilot", "compliance", "compliance_clean_data.rds"))

