###In this example, I've set the method parameter to "jaccard," which is suitable for approximate string matching. The max_dist parameter is set to 0.3, but you can adjust it based on your desired level of similarity. A higher value allows for more flexibility in matching.


# Install and load the stringdist package
# install.packages("stringdist")
library(stringdist)

# Function to find the approximate match
find_approximate_match <- function(names_to_match, reference_names, method = "jaccard", max_dist = 0.3) {
  approximate_matches <- stringdist::stringdistmatrix(names_to_match, reference_names, method = method)
  closest_indices <- apply(approximate_matches, 1, function(row) {
    min_dist <- min(row)
    if (min_dist <= max_dist) {
      return(which(row == min_dist)[1])
    } else {
      return(NA)
    }
  })
  matched_names <- reference_names[closest_indices]
  
  # Create a data frame with the original names and their matches
  result_df <- data.frame(original_names = names_to_match, matched_names)
  
  return(result_df)
}

# Example usage with less exact matching
# Assuming first_names and second_names are your vectors of names
first_names <- c("Dr. Naveed Akhtar Catper", "Dr. Khalil-ur-Rehman Bhatti", "Dr. Abdul Sattat Gopang", "Dr. Shakeela Shah", "Dr. Zuhra Khatoon", "Dr. Ahmed Shah")
second_names <- c("Dr. Naveed Akhtar", "Khalil ur Rehman Bhatti", "Dr. Abdul Sattat Gopang", "Dr. Shakeela Shah", "Dr. Zuhra Khatoon", "Dr. Ahmed Shah")

result <- find_approximate_match(first_names, second_names, method = "jaccard", max_dist = 0.3)

# Print the result
print(result)
