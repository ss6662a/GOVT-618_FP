# EDA for GOVT 618 Final Project 


# Load Data and libraries

library(tidyverse)
library(knitr)

# data from Penn State: https://scdb.la.psu.edu/data/2026-release-01/

SC_data <- read.csv("SCDB_2026_01_justiceCentered_Citation.csv_/SCDB_2026_01_justiceCentered_Citation.csv")



# Get a picture of the data 

dim(SC_data)
names(SC_data)

key <- c("caseId", "voteId", "term", "dateDecision", "justice", "justiceName",
         "vote", "opinion", "direction", "majority", "issueArea", "decisionDirection")

glimpse(SC_data[, key])

SC_data |>
  select(all_of(key)) |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") |>
  mutate(pct_missing = round(100 * n_missing / nrow(SC_data), 1))


n_distinct(SC_data$caseId)       # number of cases
n_distinct(SC_data$justice)      # number of justices
range(SC_data$term)              # first and last term



# clean up decision date

# convert the decision date from text to a date
SC_data <- SC_data |> mutate(dateDecision = lubridate::mdy(dateDecision))
sum(is.na(SC_data$dateDecision))      # There are no missing dates
range(SC_data$dateDecision)           # The dates range from 1946 - 2026



# cases, votes and number of justices per term of the supreme court 
per_term <- SC_data |>
  group_by(term) |>
  summarise(cases = n_distinct(caseId),
            votes = n(),
            justices = n_distinct(justice))
head(per_term)
summary(per_term$cases) # Summary of the amount of cases seen by the SC per term 


ggplot(per_term, aes(term, cases)) +
  geom_line() +
  labs(x = "Term", y = "Number of cases", title = "Cases decided per term")

# amount of votes by justice and their time in the court 
per_justice <- SC_data |>
  group_by(justiceName) |>
  summarise(first_term = min(term), last_term = max(term), votes = n()) |>
  arrange(first_term)
print(per_justice, n = 40)

# direction: 1 = conservative, 2 = liberal, NA = not classifiable or justice did not participate
SC_data |>
  count(direction) |>
  mutate(pct = round(100 * n / sum(n), 1))

# Some Votes are missing a decision, these are the explainations 
missing_reasons <- SC_data |>
  filter(is.na(direction)) |>
  mutate(reason = case_when(
    is.na(vote) ~ "Justice didn't vote",
    decisionDirection == 3 | is.na(decisionDirection) ~ "Case direction not specified",
    vote %in% c(6, 7, 8) ~ "Special vote type (dissent from cert or jurisdiction, or equally divided)",
    TRUE ~ "Other")) |>
  count(reason, name = "votes") |>
  arrange(desc(votes)) |>
  mutate(percent = round(100 * votes / sum(votes), 1))

missing_reasons <- bind_rows(
  missing_reasons,
  tibble(reason = "Total", votes = sum(missing_reasons$votes), percent = 100))

knitr::kable(missing_reasons,
             col.names = c("Reason `direction` is missing", "Votes", "Percent"))




# Keeping only votes with a direction adding liberal
SC_model <- SC_data |>
  filter(!is.na(direction)) |>
  mutate(liberal = as.integer(direction == 2))

round(100*mean(SC_model$liberal),1) # percent of overall votes that is liberal


by_justice <- SC_model |>
  group_by(justiceName) |>
  summarise(first_term = min(term), last_term = max(term),
            votes = n(), pct_liberal = round(100 * mean(liberal), 1)) |>
  arrange(desc(pct_liberal))
print(by_justice, n = 40)

ggplot(by_justice, aes(x = pct_liberal, y = reorder(justiceName, pct_liberal))) +
  geom_point() +
  geom_vline(xintercept = 100 * mean(SC_model$liberal), linetype = "dashed") +
  labs(x = "Percent of votes that are liberal", y = NULL,
       title = "Liberal-vote share by justice")

by_term <- SC_model |>
  group_by(term) |>
  summarise(votes = n(), pct_liberal = 100 * mean(liberal))

head(by_term)
summary(by_term$pct_liberal)

ggplot(by_term, aes(term, pct_liberal)) +
  geom_line() +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, span = 0.4) +
  geom_hline(yintercept = 100 * mean(SC_model$liberal), linetype = "dashed") +
  labs(x = "Term", y = "Percent of votes that are liberal",
       title = "Liberal-vote share by term")




# Notes About the Data to maybe handle

# The term date starts 19 1946 for everyone, 
# even though some justices started that year, but some started earlier
# We could manually add in when they started if we wanted, but we might not even 
# go all the way back to 1946. 


