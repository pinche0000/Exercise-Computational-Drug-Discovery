library(ggplot2)
install.packages("tidyverse")

library(tidyverse) 

#give the fixed sesnitivity 

sensitivity <- 0.99


#given the infection prevalency:

set.seed(32L)

infection_prevalence <- seq(0.00001, 0.5, length.out = 500)
infection_prevalence


#given the different specificity:

specificity_array <- c(0.9, 0.99, 0.999, 0.9999, 0.99999)
specificity_array

#P(I|T) = (P(T|I) * P(I))/ (P(T) = >sum of probabilities) (**)

#P(I) = P(T|I = sensitivity) * P(I = Infection prevalence) + P(T|notinfected = 1- specificity) + P(not infected = 1-inefection prevalence)

#For loop that calculate the probability to be infected at different specificity levels and insert each iteration in a empty data frame (final_df)
final_df <- data.frame()

#iteration "for loop" for each specificity and calculate probability with the formula mentioned above(**)
for (i in specificity_array ){
  
  probability <- (sensitivity * infection_prevalence) / 
    ((sensitivity * infection_prevalence) + 
       ((1 - i) * (1 - infection_prevalence)))
  #Data frame containing infection prevalences probability and sopecificity
  prob_df <- data.frame(Infection_Prevalence = infection_prevalence *100,
                        Probability_To_Be_Infected = probability *100,
                        Specificity = factor(i, levels = specificity_array))
  final_df <- rbind(final_df, prob_df) #all dataframe together
}


#plotting the probability to be infected ~ Inbfection prevalence for each specificity level
ggplot(final_df, aes(x = Infection_Prevalence, y = Probability_To_Be_Infected, color = Specificity)) +
  geom_point() +
  labs(x = "Infection Prevalence (%)", y = "Probability of Getting Infected (%)",
       title = "Probability of Getting Infection ~ Infection Prevalence",
       color = "Specificity") +
  theme_classic()




p99 <- final_df|>
  filter(Specificity == 0.99)|>
  select(Probability_To_Be_Infected) |>
  pull()


p99.9 <- final_df|>
  filter(Specificity == 0.999)|>
  select(Probability_To_Be_Infected) |>
  pull()



t.test(p99, p99.9,var.equal = T)

#significant difference

p99.99 <- final_df|>
  filter(Specificity == 0.9999)|>
  select(Probability_To_Be_Infected) |>
  pull()


p99.999 <- final_df|>
  filter(Specificity == 0.99999)|>
  select(Probability_To_Be_Infected) |>
  pull()




t.test(p99, p99.99)

#significant difference 
t.test(p99, p99.999)

#significant difference 

t.test(p99.99, p99.999)

#not significant difference 

t.test(p99.9, p99.999)# significant 


