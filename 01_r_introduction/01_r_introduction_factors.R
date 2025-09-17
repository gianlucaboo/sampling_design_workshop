# -----------------------------------------------------------
# Exploring Factor Objects in R
# -----------------------------------------------------------

# 1. What are factors?
# Factors are used to handle categorical data in R.
# They store both the values (levels) and an internal integer code.

# Example data: categorical values
colors <- c("red", "blue", "red", "green", "blue", "red")

# Convert character vector to factor
color_factor <- factor(colors)

# Print the factor
print(color_factor)

# -----------------------------------------------------------
# 2. Levels
# Factors have "levels" — the distinct categories present.
levels(color_factor)

# The number of levels
nlevels(color_factor)

# -----------------------------------------------------------
# 3. Internal representation
# Factors are stored internally as integers with labels
as.integer(color_factor)

# Under the hood, "red" might be 3, "blue" = 1, "green" = 2 (depending on alphabetical order).

# -----------------------------------------------------------
# 4. Order of levels
# By default, levels are sorted alphabetically
levels(color_factor)

# You can change the order manually:
color_factor2 <- factor(colors, levels = c("red", "green", "blue"))
levels(color_factor2)

# -----------------------------------------------------------
# 5. Ordered vs Unordered factors
# Factors can also be ordinal (ordered), useful for rankings
size <- factor(c("small", "medium", "large", "medium"),
               levels = c("small", "medium", "large"),
               ordered = TRUE)

print(size)
is.ordered(size)

# Comparison works for ordered factors:
size[1] < size[2]   # TRUE (small < medium)

# -----------------------------------------------------------
# 6. Factors vs Characters
# Factors are categorical data, not strings.
class(colors)        # character
class(color_factor)  # factor

# Converting factors back to character
as.character(color_factor)

# -----------------------------------------------------------
# 7. Adding / Dropping levels
# Adding a new level that wasn’t present
levels(color_factor) <- c(levels(color_factor), "yellow")
levels(color_factor)

# Dropping unused levels
color_factor3 <- droplevels(color_factor)
levels(color_factor3)

# -----------------------------------------------------------
# 8. Summary of factors
# Summaries of factors count frequencies by level
summary(color_factor)

summary(size)   # Ordered factor summary
# -----------------------------------------------------------

# Key Takeaways:
# - Factors represent categorical data.
# - They store both values and levels (categories).
# - Useful for modeling (e.g., regression, classification).
# - Ordered factors let you represent ranked data.
# -----------------------------------------------------------