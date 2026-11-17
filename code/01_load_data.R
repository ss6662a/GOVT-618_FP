


#### Data loading ----

jus_raw <- read_csv("data/raw/SCDB_justice.csv")

#### Dealing with Missing values ----
# _____________DO BETTER LATER____________________
jus <- subset(jus_raw, direction %in% c(1, 2)) # gets rid of missing, NEED TO DEAL WITH BETTER later
drop_by_issue <- jus_raw %>%
  filter(decisionType %in% c(1, 2, 6, 7)) %>%
  group_by(issueArea) %>%
  summarise(n_votes = n(), pct_dropped = mean(!direction %in% c(1, 2)))
# _________________________________________


#### Lookup: appointing president's party (external to SCDB) ----
# +0.5 = Republican appointer, -0.5 = Democratic appointer; keyed on codebook justice IDs (A19)
party_lookup <- tribble(
  ~justice, ~R,
  78, -0.5,  # HLBlack (FDR)
  79, -0.5,  # SFReed
  80, -0.5,  # FFrankfurter
  81, -0.5,  # WODouglas
  82, -0.5,  # FMurphy
  84, -0.5,  # RHJackson
  85, -0.5,  # WBRutledge
  86, -0.5,  # HHBurton (Truman appointed him, though Burton was a Republican)
  87, -0.5,  # FMVinson
  88, -0.5,  # TCClark
  89, -0.5,  # SMinton
  90,  0.5,  # EWarren (Eisenhower)
  91,  0.5,  # JHarlan2
  92,  0.5,  # WJBrennan
  93,  0.5,  # CEWhittaker
  94,  0.5,  # PStewart
  95, -0.5,  # BRWhite (Kennedy)
  96, -0.5,  # AJGoldberg
  97, -0.5,  # AFortas (LBJ)
  98, -0.5,  # TMarshall
  99,  0.5,  # WEBurger (Nixon)
  100, 0.5,  # HABlackmun
  101, 0.5,  # LFPowell
  102, 0.5,  # WHRehnquist
  103, 0.5,  # JPStevens (Ford)
  104, 0.5,  # SDOConnor (Reagan)
  105, 0.5,  # AScalia
  106, 0.5,  # AMKennedy
  107, 0.5,  # DHSouter (GHW Bush)
  108, 0.5,  # CThomas
  109, -0.5, # RBGinsburg (Clinton)
  110, -0.5, # SGBreyer
  111, 0.5,  # JGRoberts (GW Bush)
  112, 0.5,  # SAAlito
  113, -0.5, # SSotomayor (Obama)
  114, -0.5, # EKagan
  115, 0.5,  # NMGorsuch (Trump)
  116, 0.5,  # BMKavanaugh
  117, 0.5,  # ACBarrett
  118, -0.5  # KBJackson (Biden)
)

#### Collapse party codes ----
# Collapse the 311 party codes (varParties) into a few groups
# NEED TO CHECK AGAINST CODEBOOK (AND probably update later)
party_grp <- function(x) {
  case_when(
    x %in% c(1, 14, 22, 23, 27) | between(x, 301, 422)       ~ "federal_gov",
    x %in% c(2:7, 15:21, 25, 26, 28)                         ~ "state_local_gov",
    x %in% c(100, 110, 126, 213, 215, 217)                   ~ "criminal",
    x %in% c(101, 103, 104, 113, 114, 119, 120, 122, 128, 133, 143, 147, 148,
             151, 158, 171, 176, 184, 185, 189, 191, 198, 205, 209, 219, 220,
             231, 238, 245)                                  ~ "business",
    TRUE                                                     ~ "other_private"
  )
}


#### Data Cleaning ----
jus <- jus_raw %>%
  arrange(voteId) %>%
  filter(decisionType %in% c(1, 2, 6, 7), # drops decrees (4), ties (5), seriatim (8)
         direction %in% c(1, 2), # codable direction; also drops non-participants
         !is.na(issueArea) # DO BETTER LATER _____________
         ) %>% 
  mutate(
    jdir = as.integer(direction == 1), # outcome: 1 = conservative vote, 2 = liberal
    cdir = as.integer(decisionDirection == 1), # NOT a covariate, case direction; for diagnostics only bc highly correlated to jdir
    unanimous = as.integer(minVotes == 0),
    term_year = term,
    term = as.integer(term - min(term) + 1L), # numbered rather than year
    case = as.integer(factor(caseId)),
    just = as.integer(factor(justice)), # justice number, unique
    era = as.character(chief),
    
    # case-level covariates
    issue_area = factor(issueArea),
    lc_dir = case_when(lcDispositionDirection == 1 ~ "con", # lower court direction
                       lcDispositionDirection == 2 ~ "lib",
                       TRUE ~ "unk"), # 3 = unspecifiable, NA = original jurisdiction
    cert = case_when(certReason == 1 ~ "no_cert", # writ of certoiori
                     certReason %in% 2:6 ~ "conflict",
                     certReason %in% 7:9 ~ "confusion",
                     certReason %in% 10:11 ~ "important_q",
                     TRUE ~ "none_other"), # 12, 13, NA
    src = case_when(between(caseSource, 21, 32)  ~ "fed_appeals",
                    caseSource %in% 300:302 ~ "state_court",
                    between(caseSource, 41, 187) ~ "fed_district",
                    TRUE ~ "other"),
    lc_disagree = replace_na(as.integer(lcDisagreement), 0L),
    pet_grp  = party_grp(petitioner),
    resp_grp = party_grp(respondent)
  ) %>%
  select(jdir, cdir, unanimous, case, just, justice, justiceName, term, term_year, era,
         issue_area, lc_dir, cert, src, lc_disagree, pet_grp, resp_grp,
         issueArea, issue, lawType, caseId, voteId, dateDecision, chief)





#### Case-level design matrix X (one row per case, ordered by case index) ----
cases <- jus %>%
  distinct(case, .keep_all = TRUE) %>%
  arrange(case) %>%
  mutate(across(c(lc_dir, cert, src, pet_grp, resp_grp), factor))

X <- model.matrix(~ issue_area + lc_dir + cert + src + pet_grp + resp_grp + lc_disagree,
                  data = cases)
stopifnot(nrow(X) == max(jus$case), !anyNA(X), qr(X)$rank == ncol(X))

#### Justice-level partisanship vector R, ordered by the just index ----
jlook <- jus %>% distinct(just, justice) %>% arrange(just) %>%
  left_join(party_lookup, by = "justice")
stopifnot(!anyNA(jlook$R))   # a failure means a justice is missing from party_lookup

#### Term table for plotting gamma later ----
term_tbl <- jus %>% group_by(term) %>%
  summarise(term_year = first(term_year), era = first(era))

#### Data list for runjags ----
dat <- list(
  N = nrow(jus), 
  y = jus$jdir,
  case = jus$case,
  just = jus$just,
  term = jus$term,
  nCase = max(jus$case),
  nJust = max(jus$just),
  nTerm = max(jus$term),
  R = jlook$R, X = X, K = ncol(X)
)

