#' Author: David Fischanger
#' Date: 6-30-2024
#' Purpose: Case Study - Obtain, Scrub, Explore, Model, Interpret
#' 
# for presentation:
# agenda, objective (identify problem), insights, ppt slides



# Import Libraries 
library(dplyr)
library(readr)
library(ggplot2)
library(vtreat)
library(MLmetrics)
library(caret)


#####################################################
##############          OBTAIN          #############
#####################################################
# Obtain the data
drNotes <- read.csv('https://raw.githubusercontent.com/kwartler/teaching-datasets/main/diabetes_tables/drNotes.csv')
drugData <- read.csv('https://raw.githubusercontent.com/kwartler/teaching-datasets/main/diabetes_tables/drugData.csv')
hospitalStayData <- read.csv('https://raw.githubusercontent.com/kwartler/teaching-datasets/main/diabetes_tables/hospitalStayData.csv')
patientDemographic <- read.csv('https://raw.githubusercontent.com/kwartler/teaching-datasets/main/diabetes_tables/patientDemographics.csv')


#####################################################
#########          EXPLORE DATA         #############
#####################################################

# Explore core data
head(drNotes)            # remove
head(drugData)          # usage
head(hospitalStayData)   # usage
head(patientDemographic) # usage

#remove drNotes table as it's not used in the analytics
rm(drNotes)

# explore data quality and select the appropriate variables
summary(drugData)
apply(drugData[,2:28],2,table)   #apply drugData columnwise hence "2" in a table to analyze it
apply(hospitalStayData[,2:15],2,table)   #apply hospitalStayData columnwise hence "2" in a table to analyze it
apply(patientDemographic[,2:5],2,table)   #apply hospitalStayData columnwise hence "2" in a table to analyze it

ggplot(hospitalStayData, aes(x = number_diagnoses)) + 
#  geom_bar(fill = "steelblue", color = "black") +
  geom_density() +
  theme_minimal() + 
  labs(title = "Diabetes Patient Number of Diagnoses", x = "Number of Diagnoses", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
ggsave("diabetes_patient_number_diagnoses.jpg")

# Number outpatient histogram
ggplot(hospitalStayData, aes(x = number_outpatient)) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +  # Added 'bins' and color details for clarity
  theme_minimal() + 
  labs(title = "Diabetes Patient Number of Outpatient Visits", x = "Number of Outpatient Visits", y = "Count")
ggsave("diabetes_patient_number_outpatient.jpg")


#scrub the patientDemographic age data to remove ) replace with ]
patientDemographic$age <- gsub(")","]",patientDemographic$age)

# remove patient_weight, because it has 9.6k unknown oberservations
patientDemographic$weight <- NULL


#####################################################
############          JOIN DATA          ############
#####################################################

# Join the data
# patientDemographic>drugData>hospitalStayData
joinData <- patientDemographic %>%
  left_join(drugData, by = "id") %>%
  left_join(hospitalStayData, by = "id") # %>%

str(joinData)
summary(joinData)


#####################################################
###########          SCRUB DATA         #############
#####################################################

# SPLIT Data for Training/ Validation
set.seed(123)  # For reproducibility
idx <- sample(1:nrow(joinData), 10000*0.8)

# Splitting & Sampling into Training/Validation
training <- joinData[idx,]
validation <- joinData[-idx,]

# Exploring data & saving graphics
head(training)
summary(training)
names(training)
table(training$readmitted)

###########          BASIC GRAPHICS         #############

ggplot(training, aes(x= readmitted)) + geom_bar() + theme_minimal()
ggsave("diabetes_patient_distribution_readmitted.jpg")

ggplot(training, aes(x= gender)) + geom_bar() + theme_minimal()
ggsave("diabetes_patient_gender.jpg")

ggplot(training, aes(x= age)) + geom_bar() + theme_minimal()
ggsave("diabetes_patient_age.jpg")

ggplot(training, aes(x= race)) + geom_bar() + theme_minimal()
ggsave("diabetes_patient_race.jpg")

ggplot(training, aes(x= time_in_hospital)) + geom_bar() + theme_minimal()
ggsave("diabetes_patient_time_in_hospital.jpg")


###########          ADDITIONAL GRAPHICS         #############

# function to create a bar plot for categorical variables
create_bar_plot <- function(data, x_var, fill_var, title, x_label, y_label) {
  ggplot(data, aes_string(x = x_var, fill = fill_var)) +
    geom_bar(position = "fill", color = "black") +
    scale_fill_manual(values = c('lightcoral', 'skyblue')) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = title, x = x_label, y = y_label, fill = "Readmitted") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Readmission by AGE Group
create_bar_plot(joinData, "age", "readmitted", "Readmission Rate by AGE Group", "Age", "Proportion of Patients")
ggsave("readmission_by_age.png")


# Readmission by Medication Group
create_bar_plot(joinData, "num_medications", "readmitted", "Readmission Rate by number of medications", "Number of Medications", "Proportion of Patients")
ggsave("readmission_by_medication.png")


# Readmission by Metformin Usage
create_bar_plot(joinData, "metformin", "readmitted", "Readmission Rate by Metformin Usage", "Metformin Usage", "Proportion of Patients")
ggsave("readmission_by_metformin_usage.png")

# Readmission by Glyburide Usage
create_bar_plot(joinData, "glyburide", "readmitted", "Readmission Rate by Glyburide Usage", "Glyburide Usage", "Proportion of Patients")
ggsave("readmission_by_glyburide_usage.png")

# Readmission by Insulin Usage
create_bar_plot(joinData, "insulin", "readmitted", "Readmission Rate by Insulin Usage", "Insulin Usage", "Proportion of Patients")
ggsave("readmission_by_insulin_usage.png")

# Readmission by Discharge Disposition
create_bar_plot(joinData, "discharge_disposition_id", "readmitted", "Readmission Rate by Discharge Disposition", "Discharge Disposition", "Proportion of Patients")
ggsave("readmission_by_discharge_disposition.png")

# Readmission by Admission Source
create_bar_plot(joinData, "admission_source_id", "readmitted", "Readmission Rate by Admission Source", "Admission Source", "Proportion of Patients")
ggsave("readmission_by_admission_source.png")

# Readmission by Payer Code
create_bar_plot(joinData, "payer_code", "readmitted", "Readmission Rate by Payer Code", "Payer Code", "Proportion of Patients")
ggsave("readmission_by_payer_code.png")

# Readmission by Medical Specialty
create_bar_plot(joinData, "medical_specialty", "readmitted", "Readmission Rate by Medical Specialty", "Medical Specialty", "Proportion of Patients")
ggsave("readmission_by_medical_specialty.png")

# function to create a bar plot for numeric variables
create_binned_bar_plot <- function(data, x_var, fill_var, bins, title, x_label, y_label) {
  ggplot(data, aes_string(x = x_var, fill = fill_var)) +
    geom_histogram(bins = bins, position = "fill", color = "black") +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = c('lightcoral', 'skyblue')) +
    labs(title = title, x = x_label, y = y_label, fill = "Readmitted") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Readmission by Number of Outpatient Visits
create_binned_bar_plot(joinData, "number_outpatient", "readmitted", 30, "Readmission Rate by Number of Outpatient Visits", "Number of Outpatient Visits", "Proportion of Patients")
ggsave("readmission_by_number_outpatient.png")

# Readmission by Number of Emergency Visits
create_binned_bar_plot(joinData, "number_emergency", "readmitted", 30, "Readmission Rate by Number of Emergency Visits", "Number of Emergency Visits", "Proportion of Patients")
ggsave("readmission_by_number_emergency.png")

# Readmission by Number of Inpatient Visits
create_binned_bar_plot(joinData, "number_inpatient", "readmitted", 30, "Readmission Rate by Number of Inpatient Visits", "Number of Inpatient Visits", "Proportion of Patients")
ggsave("readmission_by_number_inpatient.png")

# Readmission by Number of Diagnoses
create_binned_bar_plot(joinData, "number_diagnoses", "readmitted", 30, "Readmission Rate by Number of Diagnoses", "Number of Diagnoses", "Proportion of Patients")
ggsave("readmission_by_number_diagnoses.png")

###############################################################################
###########       Create a treatment plan using vtreat            #############
###############################################################################

# Feature Engineering
informativeFeatures <- setdiff(names(joinData), c("id", "readmitted"))
targetVariable      <- names(joinData)[45] # = readmitted variables
successClass        <- 'TRUE'


plan <- designTreatmentsC(dframe=training, 
                          varlist=informativeFeatures, 
                          targetVariable, 
                          successClass)

treatedTraining <- prepare(plan, training)
treatedValidation <- prepare(plan, validation)


#####################################################
##########          CREATE MODEL         ############
#####################################################

# Model fitting
fit <- glm(readmitted ~ ., data = treatedTraining, family = 'binomial')
summary(fit)

# Backward Variable Selection.  
#bestFit <- step(fit, direction='backward')
#summary(bestFit)

# ---> Backward Variable Selection created an endless loop, hence we're following a manual approach:

# List of selected variables for the model from summary(fit) - removing all insignificant variables based bestFit step 1
selected_variables <- c("metformin_catB", "glyburide_catB", "insulin_catB",
                        "discharge_disposition_id_catB", "admission_source_id_catB",
                        "payer_code_catB", "medical_specialty_catB",
                        "num_lab_procedures", "num_procedures", "number_outpatient",
                        "number_emergency", "number_inpatient", "number_diagnoses",
                        "gender_lev_x_Female")

# Construct the formula for glm
formula <- as.formula(paste("readmitted ~", paste(selected_variables, collapse = " + ")))
# Model fitting with selected variables
bestFit <- glm(formula, data = treatedTraining, family = 'binomial')
# Output summary of the model
summary(bestFit)

#############
#### Re-Run optimizing the model removing num_procedures, num_lab_procedures and gender_lev_x_female
selected_variables <- c("metformin_catB", "glyburide_catB", "insulin_catB",
                        "discharge_disposition_id_catB", "admission_source_id_catB",
                        "payer_code_catB", "medical_specialty_catB", "number_outpatient",
                        "number_emergency", "number_inpatient", "number_diagnoses")


# Construct the formula for glm
formula <- as.formula(paste("readmitted ~", paste(selected_variables, collapse = " + ")))
# Model fitting with selected variables
bestFit <- glm(formula, data = treatedTraining, family = 'binomial')
# Output summary of the model
summary(bestFit)

#####################################################
###########          ASSESSMENT         #############
#####################################################

# Ensuring we're only using the selected variables in the validation set
treatedValidation_subset <- treatedValidation[, c("readmitted", selected_variables)]

# Prediction and Classification
readmittedPreds <- predict(bestFit, newdata = treatedValidation_subset, type='response')
readmittedClasses <- ifelse(readmittedPreds >= 0.5, 1, 0)

# Create custom confusion matrix
custom_cm <- table(Predicted = readmittedClasses, Actual = treatedValidation_subset$readmitted)
print("Custom Confusion Matrix:")
print(custom_cm)

# Calculate metrics manually
total <- sum(custom_cm)
accuracy <- sum(diag(custom_cm)) / total
precision <- custom_cm[2,2] / sum(custom_cm[,2])
recall <- custom_cm[2,2] / sum(custom_cm[2,])
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Accuracy:", round(accuracy, 4)))
print(paste("Precision:", round(precision, 4)))
print(paste("Recall:", round(recall, 4)))
print(paste("F1 Score:", round(f1_score, 4)))

# Use caret's confusionMatrix
library(caret)
readmittedClasses_factor <- factor(readmittedClasses, levels = c(0, 1))
validation_readmitted_factor <- factor(treatedValidation_subset$readmitted, levels = c(0, 1))

tryCatch({
  caret_cm <- confusionMatrix(readmittedClasses_factor, validation_readmitted_factor)
  print("Caret Confusion Matrix:")
  print(caret_cm)
}, error = function(e) {
  print("Caret confusionMatrix failed. Error message:")
  print(e$message)
})

# Additional analysis: Variable importance
var_importance <- abs(coef(bestFit))[-1]  # Exclude intercept
var_importance <- sort(var_importance, decreasing = TRUE)
print("Variable Importance:")
print(var_importance)

# Plot variable importance
library(ggplot2)
importance_df <- data.frame(Variable = names(var_importance), Importance = var_importance)
ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Variable Importance", x = "Variables", y = "Absolute Coefficient Value")
ggsave("variable_importance.png", width = 10, height = 8)



#############################################################################
##########          PREDICT OUTCOMES / CREATE FINAL DATAFRAME     ###########
#############################################################################

# Reattach 'id' to match high probability of readmitted with User id
treatedTraining$id <- training$id
treatedValidation$id <- validation$id


# Create a dataframe that includes 'id' and the corresponding readmission probabilities
results_df <- data.frame(id = treatedValidation$id, readmittedProb = readmittedPreds)

# Sort the results by probability in descending order
results_df <- results_df[order(-results_df$readmittedProb), ]

# Select the top 100 ids with the highest probability of readmittance
top_100_ids <- head(results_df, 100)

# Print & write the top 100 ids
print(top_100_ids)
write.csv(top_100_ids, "Top_100_High_Risk_Patients.csv", row.names = FALSE)


# end



