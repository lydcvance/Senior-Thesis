#country of origin----
library(ggplot2)
ggplot(country_counts, aes(x = reorder(Country, -Total_Count), y = Total_Count)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Country Demographics",
       x = "Country",
       y = "Total Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Subdiscipline----
ggplot(grouped_counts, aes(x = reorder(First_Subdiscipline, -total_count), y = total_count))+
      geom_bar(stat = "identity", fill = "blue")+
      labs(title = "Subdiscipline Demographics",
           x = "Subdiscipline",
           y = "Total Count") +
      theme_classic()+
      theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1),  # Adjust x-axis tick label size here
            axis.title.x = element_text(size = 12))

#Pie chart
library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Calculate percentages for pie chart
grouped_counts <- grouped_counts %>%
  mutate(percentage = total_count / sum(total_count) * 100)

# Create a color palette with 18 distinct colors
# Create a color palette with 30 distinct colors
colors <- c(brewer.pal(12, "Set3"), brewer.pal(12, "Paired"), brewer.pal(6, "Dark2"))  # 12 from Set3, 12 from Paired, 6 from Dark2
grouped_counts_filtered <- grouped_counts %>%
  filter(percentage >= 2)
# Create the pie chart
ggplot(grouped_counts_filtered, aes(x = "", y = percentage, fill = First_Subdiscipline)) + 
  geom_bar(stat = "identity", width = 1, color = "white") +  # Create the pie slices
  coord_polar(theta = "y") +  # Make it a circular pie chart
  labs(title = "Subdiscipline Demographics") +
  theme_void() +  # Remove background and axis
  theme(
    legend.title = element_blank(),  # Remove legend title
    legend.position = "right"  # Position the legend on the right
  ) +
  scale_fill_manual(values = colors) +  # Use the custom 18-color palette
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 1.5, 
           )  # Move percentages outward


#Time Period----

grouped_counts <- grouped_counts %>%
  mutate(percentage = total_count / sum(total_count) * 100)

# Create the pie chart
ggplot(grouped_counts, aes(x = "", y = percentage, fill = `Primary Time Period`)) + 
  geom_bar(stat = "identity", width = 1, color = "white") +  # Create the pie slices
  coord_polar(theta = "y") +  # Make it a circular pie chart
  labs(title = "Primary Time Period Demographics") +
  theme_void() +  # Remove background and axis
  theme(
    legend.title = element_blank(),  # Remove legend title
    legend.position = "right"  # Position the legend on the right
  ) +
  scale_fill_manual(values = colors) +  # Use the custom 18-color palette
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 1.5, 
  )

#Primary Scientific Tools----
grouped_counts <- grouped_counts %>%
  mutate(percentage = total_count / sum(total_count) * 100)
library(ggplot2)
library(RColorBrewer)

# Define the 6 colors
colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
# Create the pie chart
ggplot(grouped_counts, aes(x = "", y = percentage, fill = `Primary Scientific Tools`)) + 
  geom_bar(stat = "identity", width = 1, color = "white") +  # Create the pie slices
  coord_polar(theta = "y") +  # Make it a circular pie chart
  labs(title = "Primary Scientific Tools Demographics") +
  theme_void() +  # Remove background and axis
  theme(
    legend.title = element_blank(),  # Remove legend title
    legend.position = "right"  # Position the legend on the right
  ) +
  scale_fill_manual(values = colors) +  # Use the 6 colors defined above
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 1.5)






