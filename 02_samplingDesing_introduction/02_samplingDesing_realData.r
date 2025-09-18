install.packages("readxl")
library(readxl)

getwd()
setwd("/Users/worldpop/Documents/GitHub/sampling_design_workshop/02_samplingDesing_introduction/data")
data <- read_xlsx("Stratified sampling.xlsx")
data$id <- c(1:nrow(data))

n <- 1000

s_ids_random <- sample(data$id, size = n, replace = FALSE)

s_random <- data |>  
  filter(id %in% s_ids_random)

mean(data$Establishments, na.rm = T)
mean(s_random$Establishments, na.rm = T)


# number of observation per strata
s_strata <- data.frame(table(data$RU))
colnames(s_strata) <- c("RU", "h")
str(s_strata)

s_strata$RU <- as.character(s_strata$RU)

s_strata$W_h <- s_strata$h/sum(s_strata$h)

s_strata$n_h <- round(n * s_strata$W_h)                      # Allocate sample size per stratum (proportional allocation)

write.csv(s_strata, "strata_weights.csv")

# draw samples from each stratum
samp_list <- lapply(c("RURAL", "URBAN"), function(h){      
  ids <- which(data$RU == h)                 # Identify units in stratum h
  pick <- sample(ids, size = s_strata[s_strata$RU==h,]$n_h,       # Randomly select n_h units without replacement
                 replace = FALSE)
  data[pick, ]                              # Return sampled data
})

samp <- do.call(rbind, samp_list)          # Combine stratum samples into one dataset

ybar_h <- tapply(samp$Establishments, samp$RU, mean)     # Compute sample mean within each stratum
s_strata_m <- sum(s_strata$W_h * ybar_h)              # Weighted average of stratum means (overall estimate)
s_random_m <- mean(s_random$Establishments)                                   
pop_m <- mean(data$Establishments, na.rm=T)                                   

s_strata_m
s_random_m
pop_m
