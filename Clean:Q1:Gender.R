#Read in files
install.packages('qualtRics')
library(qualtRics)
data <- read_survey("geo_filtered.csv")
View(data)

#Column names
colnames(data)

#Clean Data
data_clean <- data[, -c(1:18)]
View(data_clean)
colnames(data_clean)

#Rename Columns
names(data_clean)[1] <- "Gendered Language or Metaphors:'She'"
names(data_clean)[2]<- "Provide Example 1"
names(data_clean)[3]<- "Sexual Language or Metaphors: 'Turn on'"
names(data_clean)[4]<- "Provide Example 2"
names(data_clean)[5]<- "Gendered or Sexual Language as Jargon:'knockers'"
names(data_clean)[6]<- "Provide Example 3"
names(data_clean)[7]<- "Sexist Language or Imagery: Hot Tub Example"
names(data_clean)[8]<- "Provide Example 4"
names(data_clean)[9]<- "Racially-Gendered Language: Slurs/Slang/Place Names"
names(data_clean)[10]<- "Provide Example 5"
names(data_clean)[11]<- "LGBTQ-Centered Language: Dike Joes"
names(data_clean)[12]<- "Provide Example 6"
names(data_clean)[13]<- "Other Gendered/Sexist/Sexual Language"
names(data_clean)[14]<- "Provide Example 7"
names(data_clean)[15]<- "Subdiscipline"
names(data_clean)[16]<- "If Other, Specify 1"
names(data_clean)[17]<- "Primary Scientific Tools"
names(data_clean)[18]<- "If Other, Specify 2"
names(data_clean)[19]<- "Primary Time Period"
names(data_clean)[20]<- "If Other, Specify 3"
names(data_clean)[21]<- "Career Position "
names(data_clean)[22]<- "If Other, Specify 4"
names(data_clean)[23]<- "Gender"
names(data_clean)[24]<- "If Not Listed, Specify"
names(data_clean)[25]<- "Sex"
names(data_clean)[26]<- "If Other, Specify 5"
names(data_clean)[27]<- "SexualOrientation"
names(data_clean)[28]<- "If Other, Specify 6"
names(data_clean)[29]<- "Racial Identity"
names(data_clean)[30]<- "If Other, Specify 7"
names(data_clean)[31]<- "Ethnicity"  
names(data_clean)[32]<- "If Other, Specify 8"
names(data_clean)[33]<- "Country of Origin" 
names(data_clean)[34]<- "Country of Residence"

# Separate questions from demographics
install.packages('dplyr')
library(dplyr)
install.packages('tidyverse')
library(tidyverse)
data_examples <- data_clean[, c(1,2, 3,4,5, 6, 7,8, 9,10, 11,12,13, 14)]
yes_occurrence_count<- data_examples[, c(1, 3, 5, 7,9,11,13)]
View(data_examples)  

#Change Column Names
names(yes_occurrence_count)[1] <- "Q1"
names(yes_occurrence_count)[2]<-"Q2"
names(yes_occurrence_count)[3]<-"Q3"
names(yes_occurrence_count)[4]<-"Q4"
names(yes_occurrence_count)[5]<-"Q5"
names(yes_occurrence_count)[6]<-"Q6"
names(yes_occurrence_count)[7]<-"Q7"

#Make long
install.packages("tidyr")
install.packages("dplyr")

# Load the libraries
library(tidyr)
library(dplyr)

yes_occurrence_count_long <- yes_occurrence_count %>%
  pivot_longer(cols = Q1:Q7,  # Adjust column range as needed
               names_to = "Question", 
               values_to = "Response")

# Check the types of the columns
sapply(yes_occurrence_count, class)

# Convert to numeric (if needed)
yes_occurrence_count[] <- lapply(yes_occurrence_count, as.numeric)
head(yes_occurrence_count_long)

#Count occurrences
occurrence_summary <- yes_occurrence_count_long %>%
  group_by(Question, Response) %>%
  summarise(Count = n()) %>%
  filter(Response == "Yes")  # Only keep counts for "Yes" responses (or change this to another response)

# View the results
print(occurrence_summary)
occurrence_summary_sorted <- occurrence_summary %>%
  arrange(desc(Count))

# View the sorted results
print(occurrence_summary_sorted)

#Visualize
library(ggplot2)

ggplot(occurrence_summary, aes(x = Question, y = Count, fill = Question)) +
  geom_bar(stat = "identity") +
  labs(title = "Occurrence of 'Yes' Responses by Question", 
       x = "Question", y = "Count of 'Yes' Responses") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Analysis----
#Ensure dplyr is used 
data_clean %>% dplyr::group_by(Gender) %>% dplyr::summarise(count = n())

#Gender
data_clean%>%
  group_by(Gender)%>%
  summarise(count = n())
#Subdiscipline
summary_subdiscipline<- data_clean%>%
  group_by(Subdiscipline) %>%
  summarise(count = n())
print(summary_subdiscipline, n= 209)
View(summary_subdiscipline)
#Race
summary_race<- data_clean%>%
  group_by(`Racial Identity`)%>%
  summarise(count = n())
print(summary_race)
View(summary_race)
#Country of Origin 
summary_origin<-data_clean%>%
  group_by(`Country of Origin`)%>%
  summarise(count = n())
View(summary_origin)

#Sexual Orientation
summary_orientation<-data_clean%>%
  group_by(`SexualOrientation`)%>%
  summarise(count = n())
View(summary_orientation)

#Career Position
summary_career<-data_clean%>%
  group_by(`Career Position `)%>%
  summarise(count = n())
View(summary_career)

#primary scientific tools
summary_tools<-data_clean%>%
  group_by(`Primary Scientific Tools`)%>%
  summarise(count = n())
View(summary_tools)

#Primary Time period
summary_time<-data_clean%>%
  group_by(`Primary Time Period`)%>%
  summarise(count = n())
View(summary_time)

#Ethnicity
summary_ethnicity<-data_clean%>%
  group_by(Ethnicity)%>%
  summarise(count = n())
View(summary_ethnicity)

#Country of origin
summary_origin<-data_clean%>%
  group_by(`Country of Origin`)%>%
  summarise(count = n())
View(summary_origin)
library(stringr)
summary_origin<- summary_origin %>%
  mutate(
    Country = str_trim(`Country of Origin`),  # Remove leading/trailing whitespaces
    Country = case_when(
      str_detect(`Country of Origin`, regex("^(US|USA|United States|United States of America|United states|Us|Usa|usa)$", ignore_case = TRUE)) ~ "United States",
      str_detect(`Country of Origin`, regex("^(UK|United Kingdom|England|Scotland|Uni)$", ignore_case = TRUE)) ~ "United Kingdom",
      str_detect(`Country of Origin`, regex("^(France|Francz)$", ignore_case = TRUE)) ~ "France",
      str_detect(`Country of Origin`, regex("^(Germany|germany)$", ignore_case = TRUE)) ~ "Germany",
      str_detect(`Country of Origin`, regex("^(Belgium|belgium)$", ignore_case = TRUE)) ~ "Belgium",
      str_detect(`Country of Origin`, regex("^(Russia|former USSR)$", ignore_case = TRUE)) ~ "Russia",
      str_detect(`Country of Origin`, regex("^(NA|prefer not to say|USA/Austria|USA/Germany|USA and Canada)$", ignore_case = TRUE)) ~ NA_character_,
      str_detect(`Country of Origin`, regex("^(NONE OF YOUR FUCKING BUSINESS YOU NOSEY, INTRUSIVE JERK)$", ignore_case = TRUE)) ~ NA_character_,
      TRUE ~ Country
    )
  )
library(dplyr)
country_counts <- summary_origin %>%
  group_by(Country) %>%
  summarise(Total_Count = sum(count, na.rm = TRUE)) %>%
  arrange(desc(Total_Count))

View(country_counts)
country_counts <- country_counts %>%
  filter(!row_number() %in% c(2, 33,35))

#Subdiscipline


summary_subdiscipline<- summary_subdiscipline %>%
  mutate(
    Subdiscipline = str_trim(Subdiscipline),  # Remove leading/trailing whitespaces
    Subdiscipline = case_when(
      str_detect(Subdiscipline, regex("^(Earth Surface + Lithosphere,Natural Hazards,Tectonophysics|
    Earth Surface + Lithosphere,Sedimentology,Planetary Sciences|
    Earth Surface + Lithosphere,Seismology,Tectonophysics|
    Earth Surface + Lithosphere,Tectonophysics|
    Earth Surface + Lithosphere,Tectonophysics,Education|
    Earth Surface + Lithosphere,Tectonophysics,Mineral and Rock Physics/Chemistry|
    Earth Surface + Lithosphere,Volcanology, Geochemistry, and Petrology)$", ignore_case = TRUE)) ~ "Earth Surface + Lithosphere",
      str_detect(Subdiscipline, regex("^(Earth and Planetary Surface Processes/Geomorphology|
                                      Earth and Planetary Surface Processes/Geomorphology,Education|
                                      Earth and Planetary Surface Processes/Geomorphology,Education,Other|
                                      Earth and Planetary Surface Processes/Geomorphology,Geochronology|
                                      Earth and Planetary Surface Processes/Geomorphology,Geochronology,Natural Hazards,Seismology,Tectonophysics,Earth Interior + Planetary Science,Mineral and Rock Physics/Chemistry|
                                      Earth and Planetary Surface Processes/Geomorphology,Geochronology,Sedimentology|
                                      Earth and Planetary Surface Processes/Geomorphology,Geochronology,Tectonophysics|
                                      Earth and Planetary Surface Processes/Geomorphology,Geodesy,Natural Hazards,Seismology,Volcanology, Geochemistry, and Petrology,Education|
                                      Earth and Planetary Surface Processes/Geomorphology,Natural Hazards|
                                      Earth and Planetary Surface Processes/Geomorphology,Natural Hazards,Planetary Sciences|
                                      Earth and Planetary Surface Processes/Geomorphology,Planetary Sciences|
                                      Earth and Planetary Surface Processes/Geomorphology,Sedimentology|
                                      Earth and Planetary Surface Processes/Geomorphology,Seismology,Tectonophysics,Mineral and Rock Physics/Chemistry|
                                      Earth and Planetary Surface Processes/Geomorphology,Tectonophysics)$", ignore_case = TRUE)) ~ "Earth and Planetary Surface Processes",
      str_detect(Subdiscipline, regex("^(Fluid Earth + Climate|
    Fluid Earth + Climate,Atmospheric Sciences|
    Fluid Earth + Climate,Atmospheric Sciences,Cryosphere Sciences,Global Environmental Change,Ocean Sciences,Paleoceanography and Paleoclimatology,Earth and Planetary Surface Processes/Geomorphology,Natural Hazards|
    Fluid Earth + Climate,Atmospheric Sciences,Earth and Planetary Surface Processes/Geomorphology,Natural Hazards,Education|
    Fluid Earth + Climate,Atmospheric Sciences,Global Environmental Change,Natural Hazards,Informatics|
    Fluid Earth + Climate,Atmospheric Sciences,Global Environmental Change,Ocean Sciences,Mathematical tools,Nonlinear Geophysics|
    Fluid Earth + Climate,Atmospheric Sciences,Global Environmental Change,Paleoceanography and Paleoclimatology,GeoHealth|
    Fluid Earth + Climate,Biogeosciences/Geobiology,Global Environmental Change,Paleoceanography and Paleoclimatology|
    Fluid Earth + Climate,Cryosphere Sciences|
    Fluid Earth + Climate,Cryosphere Sciences,Hydrology,Paleoceanography and Paleoclimatology,Earth Surface + Lithosphere,Earth and Planetary Surface Processes/Geomorphology,Geodesy,Natural Hazards,Study of the Earth’s Deep Interior|
    Fluid Earth + Climate,Cryosphere Sciences,Informatics|
    Fluid Earth + Climate,Cryosphere Sciences,Ocean Sciences|
    Fluid Earth + Climate,Cryosphere Sciences,Paleoceanography and Paleoclimatology,Earth Surface + Lithosphere,Earth and Planetary Surface Processes/Geomorphology|
    Fluid Earth + Climate,Cryosphere Sciences,Paleoceanography and Paleoclimatology,Earth Surface + Lithosphere,Earth and Planetary Surface Processes/Geomorphology,Geochronology|
    Fluid Earth + Climate,Cryosphere Sciences,Planetary Sciences,Nonlinear Geophysics|
    Fluid Earth + Climate,Global Environmental Change,Paleoceanography and Paleoclimatology,Earth and Planetary Surface Processes/Geomorphology|
    Fluid Earth + Climate,Hydrology|
    Fluid Earth + Climate,Hydrology,Earth Surface + Lithosphere,Earth and Planetary Surface Processes/Geomorphology,Near-Surface Geophysics,Mathematical tools|
    Fluid Earth + Climate,Ocean Sciences|
    Fluid Earth + Climate,Ocean Sciences,Education|
    Fluid Earth + Climate,Paleoceanography and Paleoclimatology|
    Fluid Earth + Climate,Paleoceanography and Paleoclimatology,Earth Surface + Lithosphere,Earth Interior + Planetary Science|
    Fluid Earth + Climate,Society + Education,Mathematical tools)$", ignore_case = TRUE)) ~ "Fluid Earth +Climate",
      str_detect(Subdiscipline, regex("^(Geochronology,Tectonophysics|
                                      Geochronology,Tectonophysics,Volcanology, Geochemistry, and Petrology,Education|
                                      Geochronology,Volcanology, Geochemistry, and Petrology)$", ignore_case = TRUE)) ~ "Geochronology",
      str_detect(Subdiscipline, regex("^(Geomagnetism, Paleomagnetism and Electromagnetism,Planetary Sciences)$", ignore_case = TRUE)) ~ "Geomagnetism",
      str_detect(Subdiscipline, regex("^(Global Environmental Change,Hydrology,Earth and Planetary Surface Processes/Geomorphology,Education|
                                      Global Environmental Change,Ocean Sciences|
                                      Global Environmental Change,Ocean Sciences,Education|
                                      Global Environmental Change,Ocean Sciences,Paleoceanography and Paleoclimatology|
                                      Global Environmental Change,Soil Science)$", ignore_case = TRUE)) ~ "Global Enviornmental Change",
      str_detect(Subdiscipline, regex("^(Hydrology|
                                      Hydrology,Earth Surface + Lithosphere|
                                      Hydrology,Earth Surface + Lithosphere,Earth and Planetary Surface Processes/Geomorphology,Geochronology|
                                      Hydrology,Earth Surface + Lithosphere,Earth and Planetary Surface Processes/Geomorphology,Geomagnetism, Paleomagnetism and Electromagnetism,Sedimentology,Tectonophysics|
                                      Hydrology,Earth and Planetary Surface Processes/Geomorphology|
                                      Hydrology,Earth and Planetary Surface Processes/Geomorphology,Education|
                                      Hydrology,Earth and Planetary Surface Processes/Geomorphology,Mineral and Rock Physics/Chemistry|
                                      Hydrology,Earth and Planetary Surface Processes/Geomorphology,Natural Hazards|
                                      Hydrology,Earth and Planetary Surface Processes/Geomorphology,Sedimentology,Planetary Sciences,Education,Science and Society|
                                      Hydrology,Ocean Sciences,Earth and Planetary Surface Processes/Geomorphology|
                                      Hydrology,Ocean Sciences,Volcanology, Geochemistry, and Petrology|
                                      Hydrology,Planetary Sciences,Science and Society|
                                      Hydrology,Soil Science)$", ignore_case = TRUE)) ~ "Hydrology",
      str_detect(Subdiscipline, regex("^(Mineral and Rock Physics/Chemistry|
                                      Mineral and Rock Physics/Chemistry,Education,Science and Society,Informatic)$", ignore_case = TRUE)) ~ "Mineral and Rock Physics/Chemistry",
      str_detect(Subdiscipline, regex("^N/A|
    Natural Hazards|
    Near-Surface Geophysics)$", ignore_case = TRUE)) ~ "Other",
      TRUE ~ Subdiscipline
    )
  )














grouped_data <- summary_orientation %>%
  mutate(Group = case_when(
    grepl("Straight", Sexual_Orientation, ignore.case = TRUE) ~ "Straight",
    grepl("Gay", Sexual_Orientation, ignore.case = TRUE) ~ "Gay",
    grepl("Lesbian", Sexual_Orientation, ignore.case = TRUE) ~ "Lesbian",
    grepl("Bisexual", Sexual_Orientation, ignore.case = TRUE) ~ "Bisexual",
    grepl("Asexual", Sexual_Orientation, ignore.case = TRUE) ~ "Asexual",
    grepl("Queer", Sexual_Orientation, ignore.case = TRUE) ~ "Queer",
    grepl("Prefer not to say", Sexual_Orientation, ignore.case = TRUE) ~ "Prefer not to say",
    is.na(Sexual_Orientation) ~ "NA",
    TRUE ~ "Other"
  ))

summary <- grouped_data %>%
  group_by(Group) %>%
  summarise(Total_Count = sum(Count, na.rm = TRUE))
print(summary)

#Vizualize
install.packages('ggplot2')
library(ggplot2)
ggplot(data_clean, aes(x = Gender))+
  geom_bar() +
  labs(title = "Gender Distribution")

#Demographics per Question
#Gender
##Gendered Language or Metaphors:'She':  Have experienced
summary_data_yes <- data_clean %>%
 filter(`Gendered Language or Metaphors:'She'`=="Yes")%>%
   group_by(Gender, `Gendered Language or Metaphors:'She'`) %>%
  summarise(count = n(), .groups = 'drop')

# To view the proportions
summary_data_yes %>%
  group_by(Gender) %>%
  mutate(proportion = count / sum(count)) %>%
  print(n = 50)
View(summary_data_yes)

#Gendered Language or Metaphors: 'She': Have not Experienced
summary_data_no<- data_clean%>%
  filter(`Gendered Language or Metaphors:'She'`=="No")%>%
  group_by(Gender, `Gendered Language or Metaphors:'She'`)%>%
  summarise(count = n(), .groups = 'drop')

summary_data_no%>%
  group_by(Gender)%>%
  mutate(propotion = count / sum(count))%>%
  print(n = 50)
View(summary_data_no)

#Question 1 and gender----
library(ggplot2)
library(tidyr)
library(dplyr)
summary_data <- data_clean %>%
  group_by(Gender, `Gendered Language or Metaphors:'She'`) %>%
  summarise(count = n(), .groups = 'drop') %>%
  complete(`Gendered Language or Metaphors:'She'`, Gender, fill = list(count = 0)) 
ggplot(summary_data, aes(x = Gender, y = count, fill = `Gendered Language or Metaphors:'She'`)) +
  geom_bar(stat = "identity") +
  labs(title = "Gender and 'Yes'/'No' Responses", 
       x = "Gender", y = "Count",
       fill = "Response") +
  theme_minimal() +
  scale_fill_manual(values = c("No" = "red", "Yes" = "green"))

#Visualization attempt 2

library(ggplot2)
install.packages("reshape2")
library(reshape2)
install.packages("plyr")
library(plyr)
library(dplyr)
library(tidyverse)
library(stringr)
all_genders = c("Genderqueer/gender non-conforming", 
                "Man or man identified", 
                "Man or man identified,Transgender", 
                "Non-binary", 
                "Prefer not to say", 
                "Woman or woman identified", 
                "Transgender,Non-binary", 
                "Transgender", 
                "Woman or woman identified,Genderqueer/gender non-conforming",
                "Woman or woman identified,Non-binary",
                "Woman or woman identified,Non-binary,Genderqueer/gender non-conforming",
                "Woman or woman identified,Transgender", 
                "A gender not listed here",
                "Non-binary,Genderqueer/gender non-conforming",
                "Transgender,Non-binary,Genderqueer/gender non-conforming")
      
summary_data$Gender = factor(summary_data$Gender, levels = all_genders)

summary_data$order = as.numeric(summary_data$Gender)/100

colnames(summary_data)[colnames(summary_data) == "Gendered Language or Metaphors:'She'"] <- "GenderLanguage"
head(summary_data)

summary_data <- summary_data %>%
  mutate(Gender = as.character(Gender),
         GenderLanguage = as.character(GenderLanguage))

# Ensure exact or partial matching using `str_detect()`
summary_data <- summary_data %>%
  mutate(
    count = case_when(
      str_detect(Gender, "Man or man identified") & GenderLanguage == "Yes" ~ 50,
      str_detect(Gender, "Man or man identified") & GenderLanguage == "No" ~ 48,
      str_detect(Gender, "Non-binary") & GenderLanguage == "Yes" ~ 4,
      str_detect(Gender, "Non-binary") & GenderLanguage == "No" ~ 1,
      str_detect(Gender, "Transgender") & GenderLanguage == "Yes" ~ 4,
      str_detect(Gender, "Transgender") & GenderLanguage == "No" ~ 1,
      str_detect(Gender, "Woman or woman identified") & GenderLanguage == "No" ~ 92,
      str_detect(Gender, "Woman or woman identified") & GenderLanguage =="Yes" ~ 113,
      str_detect(Gender, "Man or man identified, Transgender") & GenderLanguage =="No" ~ 3,
      str_detect(Gender, "Man or man identified, Transgender") & GenderLanguage =="Yes" ~ 1,
      str_detect(Gender, "Transgender,Non-binary") & GenderLanguage =="Yes" ~2,
      str_detect(Gender, "Transgender,Non-binary") & GenderLanguage =="No" ~ 1,
      
      TRUE ~ count  # Keep existing values if not updated
    )
  )
# Rename column


summary_data$GenderLanguage <- factor(summary_data$GenderLanguage, levels = c("Yes", "No"))
summary_data <- summary_data %>% filter(!is.na(GenderLanguage))
summary_data$order <- as.numeric(factor(summary_data$Gender, levels = all_genders)) / 100

library(dplyr)

# Create a manual mapping of gender identities to groups
summary_data <- summary_data %>%
  mutate(
    Gender_Group = case_when(
      Gender %in% c(
        "Non-binary",
        "Genderqueer/gender non-conforming",
        "Non-binary,Genderqueer/gender non-conforming",
        "Transgender,Non-binary",
        "Transgender,Non-binary,Genderqueer/gender non-conforming",
        "Woman or woman identified,Non-binary",
        "Woman or woman identified,Non-binary,Genderqueer/gender non-conforming",
        "Woman or woman identified,Transgender,Non-binary,Genderqueer/gender non-conforming"
      ) ~ "Fluid/Multiple Gender Identities",
      
      Gender %in% c("Man or man identified") ~ "Man",
      
      Gender %in% c("Woman or woman identified") ~ "Woman",
      
      Gender %in% c("Transgender") ~ "Transgender",
      
      Gender %in% c("Man or man identified,Transgender") ~ "Man or man identified, Transgender",
      
      Gender %in% c("Woman or woman identified,Transgender") ~ "Woman or woman identified, Transgender",
      
      
      Gender %in% c("Prefer not to say", "A gender not listed here", "Woman or woman identified,Non-binary,A gender not listed here") ~ "Other/Unspecified",
      
      TRUE ~ Gender # Keep original if not mapped
    )
  )
summary_data <- summary_data %>%
  group_by(Gender_Group) %>%
  mutate(total_responses = sum(count, na.rm = TRUE))
summary_data <- summary_data %>%
  mutate(
    normalized_count = count / total_responses  # Compute the ratio
  )
summary_data <- summary_data %>%
  filter(!is.na(normalized_count))
summary_data$order <- as.numeric(factor(summary_data$Gender_Group, levels = unique(summary_data$Gender_Group))) / 100

# Print to check manual mapping
print(unique(summary_data$Gender_Group))
summary_data <- summary_data[!is.na(summary_data$Gender_Group), ]
ggplot(data = summary_data, aes(x = factor(order), fill = GenderLanguage)) + 
  # Plot "Yes" responses
  geom_bar(data = filter(summary_data, GenderLanguage == "Yes"), aes(y = count), stat = "identity") + 
  # Plot "No" responses on the negative side
  geom_bar(data = filter(summary_data, GenderLanguage == "No"), aes(y = -count), stat = "identity") + 
  # Set x-axis labels (this can be adjusted depending on your data)
  scale_x_discrete(labels = unique(summary_data$Gender_Group)) +
  xlab("Gender") +
  coord_flip() +
  scale_y_continuous(breaks = seq(-100, 100, 20), 
                     labels = abs(seq(-100, 100, 20))) + 
  theme(text = element_text(size = 16)) + 
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Occurrence of Gendered Language or Metaphors") +
  labs(fill = "Occurrence")+
  theme(axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))
print(colnames(summary_data))


#Question 2 and Gender----

summary_data <- data_clean %>%
  group_by(Gender, `Sexual Language or Metaphors: 'Turn on'`) %>%
  summarise(count = n(), .groups = 'drop') %>%
  complete(`Sexual Language or Metaphors: 'Turn on'`, Gender, fill = list(count = 0)) 
View(summary_data)

#Visualization attempt 2

library(ggplot2)
install.packages("reshape2")
library(reshape2)
install.packages("plyr")
library(plyr)
library(dplyr)
library(tidyverse)
library(stringr)
all_genders = c("Genderqueer/gender non-conforming", 
                "Man or man identified", 
                "Man or man identified,Transgender", 
                "Non-binary", 
                "Prefer not to say", 
                "Woman or woman identified", 
                "Transgender,Non-binary", 
                "Transgender", 
                "Woman or woman identified,Genderqueer/gender non-conforming",
                "Woman or woman identified,Non-binary",
                "Woman or woman identified,Non-binary,Genderqueer/gender non-conforming",
                "Woman or woman identified,Transgender", 
                "A gender not listed here",
                "Non-binary,Genderqueer/gender non-conforming",
                "Transgender,Non-binary,Genderqueer/gender non-conforming")

summary_data$Gender = factor(summary_data$Gender, levels = all_genders)

summary_data$order = as.numeric(summary_data$Gender)/100

colnames(summary_data)[colnames(summary_data) == "Sexual Language or Metaphors: 'Turn on'"] <- "SexualLanguage"
head(summary_data)

summary_data <- summary_data %>%
  mutate(Gender = as.character(Gender),
         SexualLanguage = as.character(SexualLanguage))

# Ensure exact or partial matching using `str_detect()`
summary_data <- summary_data %>%
  mutate(
    count = case_when(
      str_detect(Gender, "Man or man identified") & GenderLanguage == "Yes" ~ 50,
      str_detect(Gender, "Man or man identified") & GenderLanguage == "No" ~ 48,
      str_detect(Gender, "Non-binary") & GenderLanguage == "Yes" ~ 4,
      str_detect(Gender, "Non-binary") & GenderLanguage == "No" ~ 1,
      str_detect(Gender, "Transgender") & GenderLanguage == "Yes" ~ 4,
      str_detect(Gender, "Transgender") & GenderLanguage == "No" ~ 1,
      str_detect(Gender, "Woman or woman identified") & GenderLanguage == "No" ~ 92,
      str_detect(Gender, "Woman or woman identified") & GenderLanguage =="Yes" ~ 113,
      str_detect(Gender, "Man or man identified, Transgender") & GenderLanguage =="No" ~ 3,
      str_detect(Gender, "Man or man identified, Transgender") & GenderLanguage =="Yes" ~ 1,
      str_detect(Gender, "Transgender,Non-binary") & GenderLanguage =="Yes" ~2,
      str_detect(Gender, "Transgender,Non-binary") & GenderLanguage =="No" ~ 1,
      
      TRUE ~ count  # Keep existing values if not updated
    )
  )


summary_data$SexualLanguage <- factor(summary_data$SexualLanguage, levels = c("Yes", "No"))
summary_data <- summary_data %>% filter(!is.na(SexualLanguage))
summary_data$order <- as.numeric(factor(summary_data$Gender, levels = all_genders)) / 100

library(dplyr)
summary_data <- summary_data[-c(37:54), ]
print(nrow(summary_data))
View(summary_data)
# Create a manual mapping of gender identities to groups
summary_data <- summary_data %>%
  mutate(
    Gender_Group = case_when(
      Gender %in% c(
        "Non-binary",
        "Genderqueer/gender non-conforming",
        "Non-binary,Genderqueer/gender non-conforming",
        "Transgender,Non-binary",
        "Transgender,Non-binary,Genderqueer/gender non-conforming",
        "Woman or woman identified,Non-binary",
        "Woman or woman identified,Non-binary,Genderqueer/gender non-conforming",
        "Woman or woman identified,Transgender,Non-binary,Genderqueer/gender non-conforming"
      ) ~ "Fluid/Multiple Gender Identities",
      
      Gender %in% c("Man or man identified") ~ "Man",
      
      Gender %in% c("Woman or woman identified") ~ "Woman",
      
      Gender %in% c("Transgender") ~ "Transgender",
      
      Gender %in% c("Man or man identified,Transgender") ~ "Man or man identified, Transgender",
      
      Gender %in% c("Woman or woman identified,Transgender") ~ "Woman or woman identified, Transgender",
      
      
      Gender %in% c("Prefer not to say", "A gender not listed here", "Woman or woman identified,Non-binary,A gender not listed here") ~ "Other/Unspecified",
      
      TRUE ~ Gender # Keep original if not mapped
    )
  )
summary_data <- summary_data %>%
  group_by(Gender_Group) %>%
  mutate(total_responses = sum(count, na.rm = TRUE))
summary_data <- summary_data %>%
  mutate(
    normalized_count = count / total_responses  # Compute the ratio
  )
summary_data <- summary_data %>%
  filter(!is.na(normalized_count))
summary_data$order <- as.numeric(factor(summary_data$Gender_Group, levels = unique(summary_data$Gender_Group))) / 100

# Print to check manual mapping
print(unique(summary_data$Gender_Group))
summary_data <- summary_data[!is.na(summary_data$Gender_Group), ]
ggplot(data = summary_data, aes(x = factor(order), fill = SexualLanguage)) + 
  # Plot "Yes" responses
  geom_bar(data = filter(summary_data, SexualLanguage == "Yes"), aes(y = count), stat = "identity") + 
  # Plot "No" responses on the negative side
  geom_bar(data = filter(summary_data, SexualLanguage == "No"), aes(y = -count), stat = "identity") + 
  # Set x-axis labels (this can be adjusted depending on your data)
  scale_x_discrete(labels = unique(summary_data$Gender_Group)) +
  xlab("Gender") +
  coord_flip() +
  scale_y_continuous(breaks = seq(-100, 120, 20), 
                     labels = abs(seq(-100, 120, 20))) + 
  theme(text = element_text(size = 16)) + 
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Occurrence of Sexual Language or Metaphors") +
  labs(fill = "Occurrence")+
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(hjust = 0.5))




#Question 3 and Gender----
#Packages
library(ggplot2)
install.packages("reshape2")
library(reshape2)
install.packages("plyr")
library(plyr)
install.packages("dplyr")
library(dplyr)
library(tidyverse)
library(stringr)
install.packages("tidyr")
library(tidyr)
summary_data <- data_clean %>%
  group_by(Gender, `Gendered or Sexual Language as Jargon:'knockers'`) %>%
  summarise(count = n(), .groups = 'drop') %>%
  complete(`Gendered or Sexual Language as Jargon:'knockers'`, Gender, fill = list(count = 0)) 
View(summary_data)

#Clean
summary_data <- summary_data[-c(37:54), ]
colnames(summary_data)[colnames(summary_data) == "Gendered or Sexual Language as Jargon:'knockers'"] <- "Jargon"
head(summary_data)


summary_data$Jargon <- factor(summary_data$Jargon, levels = c("Yes", "No"))
summary_data <- summary_data %>% filter(!is.na(Jargon))
summary_data$order <- as.numeric(factor(summary_data$Gender, levels = all_genders)) / 100

library(dplyr)
# Create a manual mapping of gender identities to groups
summary_data <- summary_data %>%
  mutate(
    Gender_Group = case_when(
      Gender %in% c(
        "Non-binary",
        "Genderqueer/gender non-conforming",
        "Non-binary,Genderqueer/gender non-conforming",
        "Transgender,Non-binary",
        "Transgender,Non-binary,Genderqueer/gender non-conforming",
        "Woman or woman identified,Non-binary",
        "Woman or woman identified,Non-binary,Genderqueer/gender non-conforming",
        "Woman or woman identified,Transgender,Non-binary,Genderqueer/gender non-conforming"
      ) ~ "Fluid/Multiple Gender Identities",
      
      Gender %in% c("Man or man identified") ~ "Man",
      
      Gender %in% c("Woman or woman identified") ~ "Woman",
      
      Gender %in% c("Transgender") ~ "Transgender",
      
      Gender %in% c("Man or man identified,Transgender") ~ "Man or man identified, Transgender",
      
      Gender %in% c("Woman or woman identified,Transgender") ~ "Woman or woman identified, Transgender",
      
      
      Gender %in% c("Prefer not to say", "A gender not listed here", "Woman or woman identified,Non-binary,A gender not listed here") ~ "Other/Unspecified",
      
      TRUE ~ Gender # Keep original if not mapped
    )
  )
summary_data <- summary_data %>%
  group_by(Gender_Group) %>%
  mutate(total_responses = sum(count, na.rm = TRUE))
summary_data <- summary_data %>%
  mutate(
    normalized_count = count / total_responses  # Compute the ratio
  )
summary_data <- summary_data %>%
  filter(!is.na(normalized_count))
summary_data$order <- as.numeric(factor(summary_data$Gender_Group, levels = unique(summary_data$Gender_Group))) / 100

# Print to check manual mapping
library(ggplot2)
print(unique(summary_data$Gender_Group))
summary_data <- summary_data[!is.na(summary_data$Gender_Group), ]
ggplot(data = summary_data, aes(x = factor(order), fill = Jargon)) + 
  # Plot "Yes" responses
  geom_bar(data = filter(summary_data, Jargon == "Yes"), aes(y = normalized_count), stat = "identity") + 
  # Plot "No" responses on the negative side
  geom_bar(data = filter(summary_data, Jargon == "No"), aes(y = -normalized_count), stat = "identity") + 
  # Set x-axis labels (this can be adjusted depending on your data)
  scale_x_discrete(labels = unique(summary_data$Gender_Group)) +
  xlab("Gender") +
  coord_flip() +
  scale_y_continuous(breaks = seq(-1, 1, 0.1), 
                     labels = abs(seq(-1, 1, 0.1))) + 
  theme(text = element_text(size = 16)) + 
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Occurrence of Gendered or Sexual Language as Jargon") +
  labs(fill = "Occurrence")+
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(hjust = 0.5))






















#Clumping Subdisciplines
# Make sure the subdiscipline column is a character vector for string manipulation
summary_subdiscipline$Subdiscipline <- as.character(summary_subdiscipline$Subdiscipline)

# Extract the first subdiscipline from the comma-separated list (if multiple subdisciplines are present)
summary_subdiscipline$BroaderSubdiscipline <- sapply(strsplit(summary_subdiscipline$Subdiscipline, ","), function(x) x[1])

# Check the first few rows to verify the extraction worked correctly
head(summary_subdiscipline)



library(dplyr)

# Summarizing the unique broader subdisciplines
unique_broad_subdisciplines <- summary_subdiscipline%>%
  distinct(BroaderSubdiscipline) %>%
  arrange(BroaderSubdiscipline)

# Displaying the unique broader subdisciplines
print(unique_broad_subdisciplines)

broad_subdiscipline_counts <- summary_subdiscipline %>%
  group_by(BroaderSubdiscipline) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Displaying the count of each broad subdiscipline
print(broad_subdiscipline_counts)
View(broad_subdiscipline_counts)





#overall data

library(dplyr)
#Q1
Q1 <- data_examples[, 1:2 ]
View(Q1)
Q1_filtered <- Q1 %>%
  filter(
    `Gendered Language or Metaphors:'She'` == "Yes")
Q1 <- Q1_filtered[!is.na(Q1_filtered[, 2]), ]
View(Q1)

#Q2
Q2 <- data_examples[, 3:4]
View(Q2)
Q2_filtered <- Q2 %>%
  filter(
    `Sexual Language or Metaphors: 'Turn on'` == "Yes")
View(Q2_filtered)
Q2 <- Q2_filtered[!is.na(Q2_filtered[, 2]), ]
View(Q2)

#Q3
Q3 <- data_examples[, 5:6]
View(Q3)
Q3_filtered <- Q3 %>%
  filter(
    `Gendered or Sexual Language as Jargon:'knockers'` == "Yes")
View(Q3_filtered)
Q3 <- Q3_filtered[!is.na(Q3_filtered[, 2]), ]
View(Q3)

#Q4
Q4 <- data_examples[, 7:8]
View(Q4)
Q4_filtered <- Q4 %>%
  filter(
    `Sexist Language or Imagery: Hot Tub Example` == "Yes")
View(Q4_filtered)
Q4 <- Q4_filtered[!is.na(Q4_filtered[, 2]), ]
View(Q4)

#Q5
Q5 <- data_examples[, 9:10]
View(Q5)
Q5_filtered <- Q5 %>%
  filter(
   `Racially-Gendered Language: Slurs/Slang/Place Names` == "Yes")
View(Q5_filtered)
Q5 <- Q5_filtered[!is.na(Q5_filtered[, 2]), ]
View(Q5)


#Q6
Q6 <- data_examples[, 11:12]
View(Q6)
Q6_filtered <- Q6 %>%
  filter(
    `LGBTQ-Centered Language: Dike Joes` == "Yes")
View(Q6_filtered)
Q6 <- Q6_filtered[!is.na(Q6_filtered[, 2]), ]
View(Q6)


#Q7
Q7 <- data_examples[, 13:14]
View(Q7)
Q7_filtered <- Q7 %>%
  filter(
    `Other Gendered/Sexist/Sexual Language` == "Yes")
View(Q7_filtered)
Q7 <- Q7_filtered[!is.na(Q7_filtered[, 2]), ]
View(Q7)

#Visualize
install.packages('ggplot2')
library(ggplot2)

#Prep Data
data_stats<- data.frame(
  Question = paste("Q", 1:7, sep = ""),
  Total_Yes = c(190, 216, 154, 51, 115, 112, 71),
  Specific_Examples = c(130,169,121, 40, 89, 82, 54),
  Total_Respondents = rep(361, 7)
)
#Proportions of specific examples
data_stats$Proportion_Yes <- data_stats$Total_Yes / data_stats$Total_Respondents
data_stats$Proportion_Specific_Examples <- data_stats$Specific_Examples / data_stats$Total_Yes

#Reshape data for plotting
data_stats_long<- data.frame(
  Question =rep(data_stats$Question, each = 2),
  Category = rep(c("Yes","Specific Examples"), times = 7),
  Count = c(data_stats$Total_Yes - data_stats$Specific_Examples, data_stats$Specific_Examples)
)

#Plot the data
library(ggplot2)
ggplot(data_stats_long, aes(x = Question, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Proportion of 'Yes' Responses and 'Specific Examples' per Question",
    x = "Question",
    y = "Count",
    fill = "Category"
  ) +
  scale_fill_manual(values = c("lightblue","darkblue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#trying something
data_stats_long <- data.frame(
  Question = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7"),
  Total_Yes = c(190, 216, 154, 51, 115, 112, 71),
  Specific_Examples = c(130, 169, 121, 40, 89, 82, 54),
  Total_Respondents = rep(361, 7)
)

# Calculate the percentage for 'Specific Examples' among those who said 'Yes'
data_stats_long$Percentage_Specific_Examples <- 
  (data_stats_long$Specific_Examples / data_stats_long$Total_Yes) * 100

# Prepare labels for text annotations
data_stats_long$Label_Yes <- paste(data_stats_long$Total_Yes, "/", data_stats_long$Total_Respondents)
data_stats_long$Label_Percentage <- paste(round(data_stats_long$Percentage_Specific_Examples, 2), "%")

# Create a stacked bar plot
ggplot(data_stats_long, aes(x = Question, y = Total_Yes, fill = "Yes")) +
  geom_bar(stat = "identity") +
  geom_bar(aes(y = Specific_Examples, fill = "Specific Examples"), stat = "identity") +
  geom_text(aes(y = Total_Yes + 10, label = Label_Yes), size = 4, vjust = 0) +  # Label outside the bar
  geom_text(aes(y = Specific_Examples / 2, label = Label_Percentage), size = 4, color = "white") +  # Label inside the bar
  labs(
    title = "Proportion of 'Yes' Responses and 'Specific Examples' per Question",
    x = "Question",
    y = "Count",
    fill = "Category"
  ) +
  scale_fill_manual(values = c("lightblue", "darkblue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




