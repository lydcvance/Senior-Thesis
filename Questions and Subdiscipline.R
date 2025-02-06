library(dplyr)
library(tidyr)
summary_data <- data_clean %>%
  group_by(Subdiscipline, `Gendered Language or Metaphors:'She'`) %>%
  summarise(count = n(), .groups = 'drop') %>%
  complete(`Gendered Language or Metaphors:'She'`, Subdiscipline, fill = list(count = 0)) 

library(ggplot2)

#Visualization attempt 2

library(ggplot2)
install.packages("reshape2")
library(reshape2)
install.packages("plyr")
library(plyr)
library(dplyr)
library(tidyverse)
library(stringr)
all_subdiscipline = c("Cryosphere Sciences, Earth Surface and Lithosphere, Biogeosciences/Geobiology, 
                        Fluid Earth and Climate, Earth and Planetary Surface Processes/Geomorphology, 
                        Hydrology, Ocean Sciences, Paleoceanography and Paleoclimatology, Atmospheric Sciences, 
                        Volcanology, Global Environmental Change, Earth Interior and Planetary Science, Education, 
                        Geochronology, Sedimentology, Seismology, Mineral and Rock Physics/Chemistry, 
                        Near-Surface Geophysics, Space Physics and Aeronomy, Atmospheric and Space Electricity, 
                        Geomagnetism, Natural Hazards, Other, Planetary Sciences, Science and Society, Soil Science,
                        Tectonophysics")

summary_data$Subdiscipline = factor(summary_data$Subdiscipline, levels = all_subdiscipline)

summary_data$order = as.numeric(summary_data$Subdiscipline)/100

colnames(summary_data)[colnames(summary_data) == "Gendered Language or Metaphors:'She'"] <- "GenderLanguage"
head(summary_data)

summary_data <- summary_data %>%
  mutate(Subdiscipline = as.character(Subdiscipline),
         GenderLanguage = as.character(GenderLanguage))


# Rename column
summary_data$GenderLanguage <- factor(summary_data$GenderLanguage, levels = c("Yes", "No"))
summary_data <- summary_data %>% filter(!is.na(GenderLanguage))
summary_data$order <- as.numeric(factor(summary_data$Subdiscipline, levels = all_subdiscipline)) / 100

library(dplyr)

# Create a manual mapping of gender identities to groups
summary_data <- summary_data %>%
  mutate(
    Subdiscipline_Group = case_when(
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


#Sexual orientation and Q6
library(ggplot2)
library(tidyr)
library(dplyr)
summary_orientation <- data_clean %>%
  group_by(SexualOrientation, `LGBTQ-Centered Language: Dike Joes`) %>%
  summarise(count = n(), .groups = 'drop') %>%
  complete(`LGBTQ-Centered Language: Dike Joes`, SexualOrientation, fill = list(count = 0)) 
colnames(summary_orientation)[1] <- "lgbtqjokes"

#Visualization attempt
library(ggplot2)
install.packages("reshape2")
library(reshape2)
install.packages("plyr")
library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)

summary_orientation <- data.frame(
    Sexual_Orientation = c(
      "Asexual", "Asexual,Questioning or Unsure", "Bisexual", "Bisexual,Asexual",
      "Bisexual,Other", "Bisexual,Pansexual", "Bisexual,Pansexual,Queer",
       "Bisexual,Queer", "Bisexual,Queer,Asexual", "Gay", "Gay,Pansexual",
         "Gay,Queer", "Gay,Questioning or Unsure", "Lesbian", "Lesbian,Asexual",
         "Lesbian,Bisexual,Pansexual,Queer", "Lesbian,Queer", "Lesbian,Queer,Asexual",
         "Pansexual", "Pansexual,Queer", "Prefer not to say", "Queer", "Queer,Asexual",
         "Questioning or Unsure", "Straight", "Straight,Bisexual", "Straight,Queer",
         "Straight,Questioning or Unsure", NA
       ),
    Count = c(
         9, 1, 38, 1, 1, 4, 5, 4, 1, 7, 1, 1, 1, 8, 4, 1, 2, 1, 2, 6, 15, 15, 5, 1,
       204, 4, 1, 2, 16
       )
   )

summary_orientation <- summary_orientation %>%
  group_by(SexualOrientation, lgbtqjokes) %>%
  summarise(count = n(), .groups = 'drop') %>%
  complete(SexualOrientation, lgbtqjokes, fill = list(count = 0))

summary_orientation <- summary_orientation %>%
  group_by(SexualOrientation) %>%
  mutate(total_responses = sum(count),
         normalized_count = count / total_responses) %>%
  ungroup()

all_orientations <- c("Straight", "Gay", "Lesbian", "Bisexual", "Asexual", "Queer", "Prefer not to say", "Other")
summary_orientation$SexualOrientation <- factor(summary_orientation$SexualOrientation, levels = all_orientations)



grouped_data <- summary_orientation %>%
    mutate(Group = case_when(
         grepl("Straight", SexualOrientation, ignore.case = TRUE) ~ "Straight",
         grepl("Gay", SexualOrientation, ignore.case = TRUE) ~ "Gay",
         grepl("Lesbian", SexualOrientation, ignore.case = TRUE) ~ "Lesbian",
         grepl("Bisexual", SexualOrientation, ignore.case = TRUE) ~ "Bisexual",
         grepl("Asexual", SexualOrientation, ignore.case = TRUE) ~ "Asexual",
         grepl("Queer", SexualOrientation, ignore.case = TRUE) ~ "Queer",
         grepl("Prefer not to say", SexualOrientation, ignore.case = TRUE) ~ "Prefer not to say",
         is.na(SexualOrientation) ~ "NA",
         TRUE ~ "Other"
       ))
summary_orientation <- grouped_data %>%
  group_by(Group, lgbtqjokes) %>%
  summarise(count = sum(count), .groups = 'drop')
summary_orientation <- summary_orientation %>%
  group_by(Group) %>%
  mutate(total_responses = sum(count)) %>%
  ungroup()
summary_orientation <- summary_orientation %>%
  mutate(normalized_count = count / total_responses)

library(ggplot2)
ggplot(summary_orientation, aes(x = reorder(Group, -total_responses), y = ifelse(lgbtqjokes == "Yes", normalized_count, -normalized_count), fill = lgbtqjokes)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-1, 1)) +
  labs(
    x = "Sexual Orientation Group",
    y = "Proportion of Responses",
    fill = "LGBTQ-Centered Language: Dike Jokes",
    title = "Proportion of 'Yes' and 'No' Responses by Sexual Orientation Group"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5)
  )

summary <- grouped_data %>%
     group_by(Group) %>%
    summarise(Total_Count = sum(Count, na.rm = TRUE))
 total_responses <- sum(summary$Total_Count)
 summary <- summary %>%
   mutate(Proportion = Total_Count / total_responses)
 #Proportion of sexual orientation groups
 ggplot(data = summary, aes(x = reorder(Group, -Proportion), y = Proportion, fill = Group)) +
   geom_bar(stat = "identity") +
   scale_y_continuous(labels = scales::percent_format()) +
   labs(
     title = "Distribution of Sexual Orientation Groups",
     x = "Sexual Orientation Group",
     y = "Proportion of Total Responses"
   ) +
   theme_minimal() +
   theme(
     axis.text.x = element_text(angle = 45, hjust = 1),
     legend.position = "none"
   )
 
 
 
 # Filter out any rows with NA in 'GenderLanguage'
grouped_data$order <- as.numeric(grouped_data$order)
grouped_data <- grouped_data %>% filter(!is.na(Sexual_Orientation))
grouped_data$order <- as.numeric(as.character(grouped_data$order))
grouped_data$order <- as.numeric(factor(grouped_data$Sexual_Orientation, levels = all_orientations)) / 100

# Create the bar plot
library(ggplot2)
ggplot(data = grouped_data, aes(x = factor(order), fill = Sexual_Orientation)) + 
  # Plot "Yes" responses
  geom_bar(data = filter(grouped_data, Sexual_Orientation == "Yes"), aes(y = count), stat = "identity") + 
  # Plot "No" responses on the negative side
  geom_bar(data = filter(grouped_data, Sexual_Orientation == "No"), aes(y = -count), stat = "identity") + 
  # Set x-axis labels to 'Gender_Group'
  scale_x_discrete(labels = unique(grouped_data$Group)) +
  xlab("Sexual Orientation Group") +
  ylab("Count") +
  coord_flip() +
  scale_y_continuous(breaks = seq(-100, 100, 20), labels = abs(seq(-100, 100, 20))) + 
  theme(text = element_text(size = 16)) + 
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("LGBTQ-Centered Language") +
  labs(fill = "Occurrence") +
  theme(axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))
















