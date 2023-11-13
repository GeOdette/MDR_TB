# Load the ggplot2 library
library(ggplot2)

# Create a data frame with the provided data
data <- data.frame(
  "Phyllogenetic_complex" = c(
    "Mycobacterium abscessus",
    "Mycobacterium asiaticum",
    "Mycobacterium gordonae",
    "Mycobacterium intracellulare",
    "Mycobacterium kansasii",
    "Mycobacterium lentiflavum",
    "Mycobacterium scrofulaceum",
    "Mycobacterium simiae",
    "Mycobacterium fortuitum",
    "Mycobacterium avium",
    "Mycobacterium szulgai" # New data point
  ),
  "No_of_species_isolated" = c(3.0, 0.5, 4.2, 35.3, 9.6, 0.6, 10.2, 4.2, 26.3, 3.0, 3.0) # New data point
)

# Create a bar plot using ggplot2
plot <- ggplot(data, aes(x = reorder(Phyllogenetic_complex, -No_of_species_isolated), y = No_of_species_isolated, fill = Phyllogenetic_complex)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the axis to have horizontal bars
  labs(x = "Phyllogenetic complex", y = "No of species isolated (%)") +
  theme_minimal()  # Use a minimal theme for the plot

# Display the plot
print(plot)
# Install and load required packages
# Install and load required packages
install.packages("knitr")
install.packages("kableExtra")
library(knitr)
library(kableExtra)

# Create a data frame with the provided information
data <- data.frame(
  "Isolate ID" = c("AB3", "AV1", "AV2", "AV4"),
  "hsp65" = c(
    "Mycobacteroides abscessus subsp. bolletii",
    "Mycobacterium avium subsp. hominissuis",
    "Mycobacterium avium subsp. paratuberculosis",
    "Mycobacterium avium subsp. hominissuis"
  ),
  "16sRNA" = c(
    "Mycobacteroides abscessus subsp. massiliense",
    "Mycobacterium avium subsp. paratuberculosis",
    "Mycobacterium avium subsp. avium",
    "Mycobacterium avium subsp. avium"
  )
)

# Generate the table with custom formatting using kableExtra
table <- kable(data, format = "markdown", col.names = c("Isolate ID", "hsp65", "16sRNA")) %>%
  kable_styling(full_width = FALSE) %>%
  row_spec(0, bold = TRUE) # Make the header row bold

# Display the table
print(table)
# Load required libraries
library(ggplot2)

# Data
species <- c("Mycobacterium intracellulare", "Mycobacterium asiaticum", "Mycobacteroides abscessus",
             "Mycobacterium avium", "Mycolicibacterium fortuitum", "Mycobacterium gordonae",
             "Mycobacterium kansasii", "Mycobacterium scrofulaceum", "Mycobacterium simiae",
             "Mycobacterium szulgai", "Mycobacterium lentiflavum")
avg_identity <- c(88.941, 81.10, 80.28, 75.3, 77.73, 71.53, 79.40, 76.03, 80.6, 74.94, 75.55)

# Create a data frame
data <- data.frame(species, avg_identity)

# Create the bar plot
ggplot(data, aes(x = species, y = avg_identity)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Species", y = "Average % Identity with h37 Reference hsp65") +
  coord_flip()

# Load required libraries
library(ggplot2)

# Data
species <- c("Mycobacterium intracellulare", "Mycobacterium asiaticum", "Mycobacteroides abscessus",
             "Mycobacterium avium", "Mycolicibacterium fortuitum", "Mycobacterium gordonae",
             "Mycobacterium kansasii", "Mycobacterium scrofulaceum", "Mycobacterium simiae",
             "Mycobacterium szulgai", "Mycobacterium lentiflavum")
avg_nucleotide_diff <- c(43.73, 78, 295, 100, 98.1875, 107, 95, 87, 77, 103, 100)

# Create a data frame
data <- data.frame(species, avg_nucleotide_diff)

# Create the grouped bar plot
ggplot(data, aes(x = species, y = avg_nucleotide_diff, fill = species)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Species", y = "Avg No. of Nucleotide Differences - K)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)

# Your data
data <- data.frame(
  Species = c("Mycolicibacterium fortuitum subsp. fortuitum",
              "Mycobacterium gordonae strain CIP104529",
              "Mycobacterium intracellulare",
              "Mycobacterium kansasii",
              "Mycobacterium scrofulaceum",
              "Mycobacterium simiae",
              "Mycobacterium szulgai",
              "Mycobacterium avium",
              "Mycobacteroides abscessus",
              "Mycobacterium lentiflavum",
              "Mycobacterium asiaticum strain ATCC"),
  hsp65_gene_mean = c(98.49, 98.25, 98.67, 98.65, 98.62, 98.21, 98.12, 98.31, 98.47, 98.75, 97.78),
  sRNA_mean = c(98.59, 98.21, 98.41, 98.24, 98.43, 98.48, 98.26, 98.09, 98.05, 98.19, 97.78)
)

ggplot(data, aes(x = reorder(Species, hsp65_gene_mean))) +
  geom_point(aes(y = hsp65_gene_mean, color = "hsp65 gene"), size = 3) +
  geom_point(aes(y = sRNA_mean, color = "16sRNA"), size = 3) +
  geom_line(aes(y = hsp65_gene_mean, group = 1, color = "hsp65 gene"), linetype = "dashed") +
  geom_line(aes(y = sRNA_mean, group = 1, color = "16sRNA"), linetype = "dashed") +
  labs(title = "Comparison of hsp65 Gene and 16sRNA Mean Percentages",
       x = "Species", y = "Percentage Mean") +
  scale_color_manual(name = "Gene",
                     values = c("hsp65 gene" = "red", "16sRNA" = "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

library(ggplot2)

# Your data
data <- data.frame(
  Species = c("Mycolicibacterium fortuitum",
              "Mycobacterium gordonae",
              "Mycobacterium intracellulare",
              "Mycobacterium kansasii",
              "Mycobacterium scrofulaceum",
              "Mycobacterium simiae",
              "Mycobacterium szulgai",
              "Mycobacterium avium",
              "Mycobacteroides abscessus",
              "Mycobacterium lentiflavum",
              "Mycobacterium asiaticum"),
  hsp65_gene_mean = c(98.49, 98.25, 98.67, 98.65, 98.62, 98.21, 98.12, 98.31, 98.47, 98.75, 97.78),
  sRNA_mean = c(98.59, 98.21, 98.41, 98.24, 98.43, 98.48, 98.26, 98.09, 98.05, 98.19, 97.78)
)

ggplot(data, aes(x = Species)) +
  geom_point(aes(y = hsp65_gene_mean, color = "hsp65 gene"), size = 3) +
  geom_point(aes(y = sRNA_mean, color = "16sRNA"), size = 3) +
  geom_line(aes(y = hsp65_gene_mean, group = 1, color = "hsp65 gene"), linetype = "dashed") +
  geom_line(aes(y = sRNA_mean, group = 1, color = "16sRNA"), linetype = "dashed") +
  labs(title = "Comparison of hsp65 Gene and 16sRNA Mean Percentages",
       x = "Species", y = "Percentage Mean") +
  scale_color_manual(name = "Gene",
                     values = c("hsp65 gene" = "red", "16sRNA" = "blue")) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

library(ggplot2)
library(stringr)

# Your data
data <- data.frame(
  Species = c("Mycolicibacterium fortuitum subsp. fortuitum",
              "Mycobacterium gordonae strain CIP104529",
              "Mycobacterium intracellulare",
              "Mycobacterium kansasii",
              "Mycobacterium scrofulaceum",
              "Mycobacterium simiae",
              "Mycobacterium szulgai",
              "Mycobacterium avium",
              "Mycobacteroides abscessus",
              "Mycobacterium lentiflavum",
              "Mycobacterium asiaticum"),
  hsp65_gene_mean = c(98.49, 98.25, 98.67, 98.65, 98.62, 98.21, 98.12, 98.31, 98.47, 98.75, 97.78),
  sRNA_mean = c(98.59, 98.21, 98.41, 98.24, 98.43, 98.48, 98.26, 98.09, 98.05, 98.19, 97.78)
)

# Modify species labels to appear in the next line
data$Species <- str_replace(data$Species, " ", "\n")

ggplot(data, aes(x = reorder(Species, hsp65_gene_mean))) +
  geom_point(aes(y = hsp65_gene_mean, color = "hsp65 gene"), size = 3) +
  geom_point(aes(y = sRNA_mean, color = "16sRNA"), size = 3) +
  geom_line(aes(y = hsp65_gene_mean, group = 1, color = "hsp65 gene"), linetype = "dashed") +
  geom_line(aes(y = sRNA_mean, group = 1, color = "16sRNA"), linetype = "dashed") +
  labs(x = "Species", y = "Percentage Mean") +
  scale_color_manual(name = "Gene",
                     values = c("hsp65 gene" = "red", "16sRNA" = "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


