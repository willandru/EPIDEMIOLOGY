# Create a sample dataset
people_data <- data.frame(
  Person = c("Alice", "Bob", "Charlie", "David", "Emma"),
  Height_cm = c(160, 175, 155, 180, 162),
  Weight_kg = c(55, 70, 50, 85, 58)
)

# Display the dataset
print(people_data)

# Function to calculate BMI
calculate_bmi <- function(height, weight) {
  # BMI formula: weight (kg) / height (m)^2
  bmi <- weight / (height / 100)^2
  return(bmi)
}

# Apply the function to the dataset
people_data$BMI <- calculate_bmi(people_data$Height_cm, people_data$Weight_kg)

# Display the updated dataset
print(people_data)

