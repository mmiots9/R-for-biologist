library(tidyverse)

# Set random seed for reproducibility
set.seed(125)

# Create a custom dataset
n <- 600
sex <- factor(rep(c("Male", "Female"), times = n/2))
age <- rep(c(3, 7, 15, 30), each = n / 4)
treatment <- rep(c("Untreated", "Untreated", "T1", "T1"), times = n/4)

weight <- numeric(n)

# Create the dataset
custom_dataset <- data.frame(sex, age, treatment, weight)
rownames(custom_dataset) <- paste0("r", seq(1, n))

for (i in unique(age)) {
  indices_male <- rownames(custom_dataset %>%
    filter(age == i & sex == "Male"))
  indices_female <- rownames(custom_dataset %>%
                             filter(age == i & sex == "Female"))
  
  temp_weights_male <- round(rnorm(length(indices_male), mean = 4 + i / 2, sd = 1 ), 2)
  temp_weights_female <- round(rnorm(length(indices_female), mean = 2 + i / 2, sd = 0.7 ), 2)
    
  custom_dataset[indices_male, "weight"] <- temp_weights_male
  custom_dataset[indices_female, "weight"] <- temp_weights_female
  
}

# Add a couple of outliers
custom_dataset[15, "weight"] <- 4.2
custom_dataset[125, "weight"] <- 3.06

Task1 <- numeric(n)

for (i in seq_along(age)) {
  Task1[i] <- round(case_when(
    age[i] == 3 ~ ifelse(treatment[i] == "T1", rbinom(1, 1, prob = 0.5), rbinom(1, 1, prob = 0.3)),
    age[i] == 7 ~ ifelse(treatment[i] == "T1", rbinom(1, 1, prob = 0.55), rbinom(1, 1, prob = 0.45)),
    age[i] == 15 ~ ifelse(treatment[i] == "T1", rbinom(1, 1, prob = 0.7), rbinom(1, 1, prob = 0.58)),
    age[i] == 30 ~ ifelse(treatment[i] == "T1", rbinom(1, 1, prob = 0.9), rbinom(1, 1, prob = 0.9))), 2
  )
}

Task1 <- factor(Task1)

Task2 <- numeric(n)

for (i in seq_along(age)) {
  Task2[i] <- round(case_when(
    age[i] == 3 ~ ifelse(treatment[i] == "T1", rbinom(1, 1, prob = 0.5), rbinom(1, 1, prob = 0.6)),
    age[i] == 7 ~ ifelse(treatment[i] == "T1", rbinom(1, 1, prob = 0.55), rbinom(1, 1, prob = 0.56)),
    age[i] == 15 ~ ifelse(treatment[i] == "T1", rbinom(1, 1, prob = 0.75), rbinom(1, 1, prob = 0.53)),
    age[i] == 30 ~ ifelse(treatment[i] == "T1", rbinom(1, 1, prob = 0.88), rbinom(1, 1, prob = 0.54))), 2
  )
}

Task2 <- factor(Task2)

Task3 <- numeric(n)

for (i in seq_along(age)) {
  Task3[i] <- round(case_when(
    age[i] == 3 ~ ifelse(treatment[i] == "T1", 
                         rnorm(1, mean = 20, sd = 7) * runif(n = 1, min = 5, max = 10), 
                         rnorm(1, mean = 50, sd = 3) * runif(n = 1, min = 3, max = 5)),
    age[i] == 7 ~ ifelse(treatment[i] == "T1", 
                         rnorm(1, mean = 30, sd = 4) * runif(n = 1, min = 4, max = 8), 
                         rnorm(1, mean = 40, sd = 3) * runif(n = 1, min = 3, max = 5)),
    age[i] == 15 ~ ifelse(treatment[i] == "T1", 
                          rnorm(1, mean = 35, sd = 6.5) * runif(n = 1, min = 2.2, max = 4.8), 
                          rnorm(1, mean = 42, sd = 3.6) * runif(n = 1, min = 3.2, max = 6.8)),
    age[i] == 30 ~ ifelse(treatment[i] == "T1", 
                          rnorm(1, mean = 40, sd = 5.5) * runif(n = 1, min = 2, max = 8), 
                          rnorm(1, mean = 40, sd = 4.6) * runif(n = 1, min = 3, max = 5))), 2
  )
}

# Add a couple of outliers
Task3[150] <- 40.2
Task3[333] <- 60.43


custom_dataset <- cbind(custom_dataset, Task1, Task2, Task3)


# View the first few rows of the dataset
head(custom_dataset)

# Save
write.csv(x = custom_dataset, file = "data/Stat-test-dataset.csv", quote = F, row.names = F)