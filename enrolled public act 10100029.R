library(tidyverse)
library(stringr)
library(stringi)
library(pdftools)
setwd("~/GitHub/Illinois-Capital-Bill-2019")


# Read PDF ----------------------------------------------------------------
file<-list.files(pattern = "pdf$")
bill<-lapply(file, pdf_text)
bill<-paste( unlist(bill), collapse = '') #turns list into a string
funds<-c("Build Illinois Bond Fund", "Rebuild Illinois Projects Fund", "Road Fund", "Capital Development Fund",
         "Water Revolving Fund", "Anti-Pollution Bond Fund", "Anti-Pollution Fund", "Affordable Housing Trust Fund", 
         "School Infrastructure Fund", "School Construction Fund", "Transportation Bond Series A Fund",
         "Transportation Bond Series B Fund", "Transportation Bond Series D Fund", "Multi-Modal Transportation Bond Fund",
         "Coal Development Fund", "Illinois Works Fund", "Natural Areas Acquisition Fund",
         "Open Space Lands Acquisition and Development", "Park and Conservation Fund",
         "Regional Transportation Authority Capital Improvement Fund", "Downstate Mass Transportation Capital Improvement Fund",
         "Illinois National Guard Construction Fund", "State Construction Account Fund")
articles<-c("ARTICLE 2", "ARTICLE 3", "ARTICLE 4", "ARTICLE 5", "ARTICLE 6", "ARTICLE 7", "ARTICLE 8", "ARTICLE 9",
            "ARTICLE 10", "ARTICLE 11", "ARTICLE 12", "ARTICLE 13", "ARTICLE 14", "ARTICLE 15", "ARTICLE 16", "ARTICLE 17")
article_full_name<-c("2 DEPARTMENT OF COMMERCE AND ECONOMIC OPPORTUNITY", "3 DEPARTMENT OF NATURAL RESOURCES",
                     "4 DEPARTMENT OF NATURAL RESOURSES", #note the typo in the bill, lol
                     "5 DEPARTMENT OF TRANSPORTATION PERMANENT IMPROVEMENTS",
                     "6 CAPITAL DEVELOPMENT BOARD", "7 ENVIRONMENTAL PROTECTION AGENCY",
                     "8 ENVIRONMENTAL PROTECTION AGENCY", "9 DEPARTMENT OF REVENUE",
                     "10 DEPARTMENT OF MILITARY AFFAIRS", "11 DEPARTMENT OF MILITARY AFFAIRS",
                     "12 ILLINOIS STATE BOARD OF EDUCATION", "13 SECRETARY OF STATE",
                     "14 ARCHITECT OF THE CAPITOL", "15 DEPARTMENT OF COMMERCE AND ECONOMIC OPPORTUNITY",
                     "16 DEPARTMENT OF COMMERCE AND ECONOMIC OPPORTUNITY",
                     "17 DEPARTMENT OF COMMERCE AND ECONOMIC OPPORTUNITY")
index1<-paste(funds, collapse = "|") #collapses into list
index2<-paste(articles, collapse = "|")#collapses into list
index3<-paste(article_full_name,collapse = "|")#collapses into list

#Split by section and extract section numbers
hb62<-as.data.frame(bill) %>% 
  mutate(text= str_split(bill,"    Section| ARTICLE ")) %>% 
  rowwise() %>%
  unnest(text) %>% 
  rowwise %>%
  mutate(section_num = str_extract(text,"[^.]+"))

hb62$bill<-NULL


#Removing text junk
hb62$text<-str_remove_all(hb62$text, "[\r\n]") #removes row splits
hb62$section_num<-str_remove_all(hb62$section_num, "[\r\n]") #removes row splits
hb62$text<-str_remove_all(hb62$text, "Public Act 101-0029HB0062 Enrolled") #removes header
hb62$text<-str_remove_all(hb62$text,"LRB101 02974 WGH 47982 b") #removes header
hb62$section_num<-str_remove_all(hb62$section_num, "Public Act 101-0029HB0062 Enrolled") #removes header
hb62$section_num<-str_remove_all(hb62$section_num,"LRB101 02974 WGH 47982 b") #removes header
hb62$text<-str_replace_all(hb62$text, ' +', " ") #replaces two or more spaces with one space
hb62$text<- str_replace_all(hb62$text, "000 ", "000, ") #manual fix for amounts that are missing commas for extraction later
hb62$text<- str_replace_all(hb62$text, "300 ", "300, ")
hb62$text<- str_replace_all(hb62$text, "200 ", "200, ")
hb62$text<- str_replace_all(hb62$text, "500 ", "500, ")
hb62$text<- str_replace_all(hb62$text, "600 ", "600, ")
hb62$text<- str_replace_all(hb62$text, "450 ", "450, ")

#add fund names
hb62$fund<-str_extract(hb62$text,index1)
hb62$fund<-str_replace_all(hb62$fund, "Anti-Pollution Fund", "Anti-Pollution Bond Fund") #fixes typo that had wrong fund name

#Extracting dollar values and remove comma
hb62$appropriation <- str_extract_all(hb62$text, "[$][0-9,]+") #pulls out dollar amounts
hb62$appropriation <- substr(hb62$appropriation, 1, nchar(hb62$appropriation)-1) #removes extra commas and dollar signs

#add articles
hb62$section_num<- str_replace_all(hb62$section_num, "  +", " ") #move this after dept names are pulled
hb62$article<-str_extract(hb62$section_num,index3) #pulls article from text column
hb62<-separate(hb62,article, c("article","dept"), sep = " ") %>%  #pulls out article number
  fill(article) %>% 
  mutate(dept = case_when(article == 2 ~ "Department of Commerce and Economic Opportunity", #fills in dept name
                          article == 3 ~ "Department of Natural Resources",
                          article == 4 ~ "Department of Natural Resources",
                          article == 5 ~ "Department of Transportation",
                          article == 6 ~ "Capital Development Board",
                          article == 7 ~ "Environmental Protection Agency",
                          article == 8 ~ "Environmental Protection Agency",
                          article == 9 ~ "Department of Revenue",
                          article == 10 ~ "Department of Military Affairs",
                          article == 11 ~ "Department of Military Affairs",
                          article == 12 ~ "Illinois State Board of Education",
                          article == 13 ~ "Secretary of State",
                          article == 14 ~ "Architect of the Capitol",
                          article == 15 ~ "Department of Commerce and Economic Opportunity",
                          article == 16 ~ "Department of Commerce and Economic Opportunity",
                          article == 17 ~ "Department of Commerce and Economic Opportunity")) %>%
           filter(!is.na(fund), !is.na(article)) %>% #gets rid of sections without appropriations
           select(article, section_num,fund,dept,appropriation,text)

#write csv
write.csv(hb62, "trial.csv")




