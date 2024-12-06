cascade_plot <- long_data |>
  ggplot(aes(x = metric, y = value, fill = factor(category, 
                                                  levels = c("Gap", "Aware", "On ART", "Suppressed")))) +
  geom_bar(stat = "identity", position = "stack") +  
  geom_text(aes(label = ifelse(type == "weighted_N", 
                               scales::label_percent(accuracy = 1)(point), "")),
            vjust = 0.5, color = "black", size = 4, 
            position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  labs(x = "Metric", y = "Value", fill = "Category") +
  facet_wrap(~ typo, scales = "free_y") +  
  scale_fill_manual(values = c("Gap" = "gray", 
                               "Aware" = "#1b9e77", 
                               "On ART" = "#d95f02", 
                               "Suppressed" = "#7570b3")) +  
  theme(
    strip.background = element_rect(fill = "#F5F5F5", color = "black"),  
    strip.text = element_text(face = "bold", size = 14),  
    axis.title = element_blank(),  
    axis.text = element_blank(),  
    legend.position = "bottom",  
    legend.text = element_text(size = 12),
    legend.title = element_blank()
  )

cascade_plot


dist_plot <- clean_data %>%
  filter(if (county != "All") County == county else TRUE) %>%
  filter(is.finite(weight)) %>%  
  ggplot(aes(x = age, 
             fill = typology, 
             color = typology, 
             weight = weight)) + 
  stat_density(position = "identity", alpha = 0.5) +  
  labs(#title = "Age Distribution by Key Population Group", 
    x = "Age", 
    y = "Percentage",  
    fill = "Typology", 
    color = "Typology") +
  theme_minimal(base_size = 14) +  
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +  
  scale_x_continuous(expand = c(0, 0)) +  
  scale_fill_brewer(palette="Dark2") +  
  scale_color_brewer(palette = "Dark2") +  
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
    axis.title = element_text(size = 14, face = "bold"), 
    axis.text = element_text(size = 14),  
    legend.position = "top", 
    legend.text = element_text(size = 14) 
  ) +
  facet_wrap(~ typology)





library(dplyr)
library(ggplot2)
library(scales)

# Calculate medians and third quartiles for each group
quartiles_data <- clean_data %>%
  filter(is.finite(weight)) %>%
  group_by(typology) %>%
  summarize(
    median_age = median(age, weight, na.rm = TRUE),
    third_quartile_age = quantile(age, probs = 0.75, type = 7, na.rm = TRUE)
  )

# Plot with density, median, and third quartile lines
dist_plot <- clean_data %>%
  filter(if (county != "All") County == county else TRUE) %>%
  filter(is.finite(weight)) %>%  
  ggplot(aes(x = age, 
             fill = typology, 
             color = typology, 
             weight = weight)) + 
  stat_density(position = "identity", alpha = 0.5) +  
  labs(#title = "Age Distribution by Key Population Group", 
    x = "Age", 
    y = "Percentage",  
    fill = "Typology", 
    color = "Typology") +
  theme_minimal(base_size = 14) +  
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +  
  scale_x_continuous(expand = c(0, 0)) +  
  scale_fill_brewer(palette="Dark2") +  
  scale_color_brewer(palette = "Dark2") +  
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
    axis.title = element_text(size = 14, face = "bold"), 
    axis.text = element_text(size = 14),  
    legend.position = "top", 
    legend.text = element_text(size = 14) 
  ) +
  facet_wrap(~ typology) +
  
  # Add vertical lines for median and third quartile
  geom_vline(data = quartiles_data, aes(xintercept = median_age, color = typology), linetype = "dashed", size = 1) +
  geom_vline(data = quartiles_data, aes(xintercept = third_quartile_age, color = typology), linetype = "dotted", size = 1)

# Print the plot
dist_plot















