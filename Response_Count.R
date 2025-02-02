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
names(data_clean)[27]<- "Sexual Orientation"
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

#Country of Origin 
summary_origin<-data_clean%>%
  group_by(`Country of Origin`)%>%
  summarise(count = n())
View(summary_origin)

#Sexual Orientation
summary_orientation<-data_clean%>%
  group_by(`Sexual Orientation`)%>%
  summarise(count = n())
View(summary_orientation)

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









