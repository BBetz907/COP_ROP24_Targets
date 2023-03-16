library(glamr)
library(keyring)
library(googlesheets4)
library(googledrive)
library(glitr)
ls(package:glamr)

set_email("bbetz@usaid.gov")
set_datim("BetzB")

datim_pwd()

keyring::key_list()

load_secrets()

# set_pano()
# set_s3keys()

###############
#specific Google Sheet Unique ID (found at the end of the URL)
sheet_id <- '1tGk1TR8l3WacR8qMIK0AQvFynABijAaLHeIctE1nUoM'
#read directly into R (similar to read_csv or read_xlsx)
df <- read_sheet(as_sheets_id(sheet_id), "MechID-PartnerType")
head(df)

#pull DATIM table of OU/country UIDs and sub-national unit levels
ou_table <- get_outable(datim_user(), datim_pwd())

