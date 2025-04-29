# Basic set up stuff
install.packages(c("readxl", "survival", "survminer"))

library(readxl)
library(survival)
library(survminer)

file_path <- file.choose()
data <- read_excel(file_path)
head(data)

# Convert to NA to NA -- optional
data$`Date of Death`[data$`Date of Death` == "N/A"] <- NA

# Convert 'Date of Death' to numeric then to date (since in Excel number)
data$`Date of Death` <- as.numeric(as.character(data$`Date of Death`))
data$`Date of Death` <- as.Date(data$`Date of Death`, origin = "1899-12-30")

# Convert 'First Date of Positive Bcx' to date
data$`First Date of Positive Bcx` <- gsub(" UTC", "", data$`First Date of Positive Bcx`)
data$`First Date of Positive Bcx` <- as.Date(data$`First Date of Positive Bcx`)

# Step 3: Calculate time to event (in days), censored data from today or 360 whichever is less
current_date <- Sys.Date()  
data$time_to_event <- ifelse(
  is.na(data$`Date of Death`),
  pmin(as.numeric(difftime(current_date, data$`First Date of Positive Bcx`, units = "days")), 360),
  as.numeric(difftime(data$`Date of Death`, data$`First Date of Positive Bcx`, units = "days"))
)

# Death binary assignment
data$event <- ifelse(is.na(data$`Date of Death`), 0, 1)

# Quick check of data
table(data$event)

# Kaplan Meier survival curve by group
km_fit <- survfit(Surv(time_to_event, event) ~ Group, data = data)

# Create the plot with risk table - 365 days
plot <- ggsurvplot(
  km_fit, 
  data = data,
  conf.int = FALSE,
  risk.table = TRUE,
  risk.table.height = 0.25,
  risk.table.title = "Patients at Risk",
  pval = FALSE,
  censor = TRUE,
  censor.shape = "|",
  ggtheme = theme_minimal(),

  xlim = c(0, 365),
  break.time.by = 60,
  xlab = "Time (days)",
  ylab = "Survival Probability",
  title = "Kaplan-Meier Survival Curve",
  legend.title = "E. faecalis susceptibility",
  legend.labs = as.character(unique(data$Group)),
  surv.scale = "percent"
)

# Customizations to plot
plot$plot <- plot$plot +
  guides(color = guide_legend(override.aes = list(shape = 0, size = 0))) +
  theme(
    legend.key = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),

    # Text styling for title and labels
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),  # Modify both x and y axis text size
    axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5), # x-axis labels
    axis.text.y = element_text(size = 12, angle = 0, hjust = 0.5), # y-axis labels
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12)
  )

# Customizations to table
plot$table <- plot$table +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    
    # Remove axis titles and labels from the risk table
    axis.title.x = element_blank(),   # Remove x-axis title from risk table
    axis.title.y = element_blank(),   # Remove y-axis title from risk table
    axis.text.x = element_blank(),    # Remove x-axis labels from risk table
  )

print(plot)

