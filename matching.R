# Install and load the stringdist package
# install.packages("stringdist")
library(stringdist)

# Function to find the closest match
find_closest_match <- function(names_to_match, reference_names) {
  closest_matches <- stringdist::stringdistmatrix(names_to_match, reference_names)
  closest_indices <- apply(closest_matches, 1, which.min)
  matched_names <- reference_names[closest_indices]
  
  # Create a data frame with the original names and their matches
  result_df <- data.frame(original_names = names_to_match, matched_names)
  
  return(result_df)
}

# Example usage
# Assuming first_names and second_names are your vectors of names
first_names <- c("Dr. Naveed Akhtar Catper", "Dr. Khalil-ur-Rehman Bhatti", "Dr. Abdul Sattat Gopang", "Dr. Shakeela Shah", "Dr. Zuhra Khatoon", "Dr. Ahmed Shah")
second_names <- c("Dr. Naveed Akhtar", "Khalil ur Rehman Bhatti", "Dr. Abdul Sattat Gopang", "Dr. Shakeela Shah", "Dr. Zuhra Khatoon", "Dr. Ahmed Shah")

result <- find_closest_match(first_names, second_names)

# Print the result
print(result)

