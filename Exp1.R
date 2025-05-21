# Load necessary libraries
library(lme4)
library(ggplot2)
library(dplyr)
library(readxl)
library(purrr)

# Load the data
data <- read_excel("data.xlsx")

data$Escolha <- data$Escolha + 1

# Exclude any answers that are not 1 or 2
data <- data %>% filter(Escolha %in% c(1, 2))

# Ensure 'Escolha' is treated as a factor and VLT as a categorical variable
data$Escolha <- as.factor(data$Escolha)
data$VLT <- as.factor(data$VLT)

# GLMM analysis considering all groups together with random effects for participants and items
model_all <- glmer(Escolha ~ 1 + (1 | Participante) + (1 | Frase), 
                   family = binomial(link = "logit"), 
                   data = data)
summary(model_all)

# GLMM analysis considering VLT groups separately with random effects for participants and items
model_vlt <- glmer(Escolha ~ VLT + (1 | Participante) + (1 | Frase), 
                   family = binomial(link = "logit"), 
                   data = data)
summary(model_vlt)

# Nested model comparison to test if VLT plays a significant role

nested_comparison <- anova(model_vlt, model_all, test = "Chisq")
cat("\nNested model comparison:\n")
print(nested_comparison)

# Separate analysis for each VLT group to test differences between correct and incorrect answers
results_vlt_groups <- data %>%
  group_split(VLT) %>%
  map(~ glmer(Escolha ~ 1 + (1 | Participante) + (1 | Frase), 
              family = binomial(link = "logit"), 
              data = .))

# Print summaries of models for each VLT group
vlt_levels <- unique(data$VLT)
for (i in seq_along(results_vlt_groups)) {
  cat("\nSummary for VLT group", vlt_levels[i], "\n")
  print(summary(results_vlt_groups[[i]]))
}

# Generate performance graphs
# Calculate proportions for each group
performance <- data %>%
  group_by(VLT, Escolha) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))

# Plot the performance
plot <- ggplot(performance, aes(x = VLT, y = Proportion, fill = Escolha)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Performance by VLT Group", 
       x = "VLT Group", 
       y = "Proportion", 
       fill = "Escolha") +
  theme_minimal()

# Save the plot
ggsave("performance_plot.png", plot)

# Print plot
print(plot)



# Criar o gráfico por grupo VLT
ggplot(data, aes(x = VLT, fill = factor(Escolha))) +
  geom_bar(position = "fill") +
  # Add this line to rename Tipo_De_Frase labels
  scale_fill_manual(values = c("1" = "darkgreen", "2" = "darkred"), 
                    labels = c("1" = "Correct", "2" = "Incorrect")) +
  scale_y_reverse() +
  labs(
    x = "Spea",
    y = "Proportion",
    fill = "Choice"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 20, face = "bold"),
    strip.text = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16)
  )


# Ensure the correct order: "Correto" (green) on the bottom, "Incorreto" (red) on the top

ggplot(data, aes(x = factor(Frase), fill = Escolha)) +  
  geom_bar(position = position_fill(reverse = TRUE)) +  # Reverse to ensure correct stacking +
  facet_wrap(~ VLT, labeller = labeller(VLT = c("6" = "NS", "5" = "VLT-5", "34" = "VLT-3"))) +
  scale_fill_manual(values = c("1" = "darkgreen", "2" = "darkred"), 
                    labels = c("1" = "Correct", "2" = "Incorrect")) +
  labs(
    x = "Items",
    y = "Proportion",
    fill = "Choice"
  ) +
  theme_minimal(base_size = 16) +  
  theme(
    axis.text.x = element_text(size = 12, angle = 90, hjust = 1),  
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 18, face = "bold"),
    strip.text = element_text(size = 16, face = "bold"), 
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  )


