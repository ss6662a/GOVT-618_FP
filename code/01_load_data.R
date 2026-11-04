


#### Data loading ----

jus_raw <- read_csv("data/raw/SCDB_justice.csv")

#### Data cleaning ---

jus <- subset(jus_raw, direction %in% c(1, 2)) # gets rid of missing, NEED TO DEAL WITH BETTER later
jus <- jus %>% 
  mutate(
    y = as.integer(direction == 1),
    case = as.integer(factor(caseId)),
    just = as.integer(factor(justice)),
    term = as.integer(factor(term))
  )

X <- model.matrix(~ factor(issueArea) + factor(lcDispositionDirection), data = jus)


dat <- list(
  N = nrow(jus),
  
  y = jus$y,
  case = jus$case,
  just = jus$just,
  term = jus$term,
  
  ncase = max(jus$case),
  njust = max(jus$just),
  nterm = max(jus$term),
  
  X = X,
  K = ncol(X)
)












