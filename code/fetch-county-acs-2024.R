# Run manually to refresh the fixed teaching snapshot. This is not sourced by
# the lesson: rendering the site and following along require no Census key.
# Source: https://api.census.gov/data/2024/acs/acs5.html

readRenviron("~/.Renviron")
census_key <- Sys.getenv("CENSUS_API_KEY")
if (!nzchar(census_key)) stop("Configure CENSUS_API_KEY before refreshing the snapshot.")

variables <- c(
  Population = "B01003_001E", Adults25Plus = "B15003_001E",
  Bachelors = "B15003_022E", Masters = "B15003_023E",
  Professional = "B15003_024E", Doctorate = "B15003_025E",
  MedianIncome = "B19013_001E", PovertyUniverse = "B17001_001E",
  BelowPoverty = "B17001_002E", MedianAge = "B01002_001E"
)
public_query <- paste0(
  "https://api.census.gov/data/2024/acs/acs5?get=NAME,",
  paste(variables, collapse = ","), "&for=county:*"
)
# Never print the authenticated URL, including in an error message.
raw <- tryCatch(
  jsonlite::fromJSON(paste0(public_query, "&key=", census_key)),
  error = function(e) stop("Census download failed; the authenticated URL was not logged.",
                           call. = FALSE)
)
stopifnot(is.matrix(raw), nrow(raw) > 3000)
counties <- as.data.frame(raw[-1, ], stringsAsFactors = FALSE)
names(counties) <- raw[1, ]
for (name in names(variables)) {
  values <- as.numeric(counties[[variables[[name]]]])
  # Census negative sentinel values are missing estimates, not observations.
  values[values < 0] <- NA_real_
  counties[[name]] <- values
}
counties$CountyFIPS <- paste0(counties$state, counties$county)
counties$StateFIPS <- counties$state
counties$County <- sub(", [^,]+$", "", counties$NAME)
counties$State <- sub("^.*, ", "", counties$NAME)
counties$BachelorsPlus <- rowSums(counties[c("Bachelors", "Masters", "Professional", "Doctorate")])
counties$CollegePct <- 100 * counties$BachelorsPlus / counties$Adults25Plus
counties$PovertyPct <- 100 * counties$BelowPoverty / counties$PovertyUniverse
counties$IncomeK <- counties$MedianIncome / 1000
counties <- counties[counties$StateFIPS != "72", c(
  "CountyFIPS", "StateFIPS", "County", "State", "Population", "Adults25Plus",
  "Bachelors", "Masters", "Professional", "Doctorate", "BachelorsPlus",
  "CollegePct", "MedianIncome", "IncomeK", "PovertyUniverse", "BelowPoverty",
  "PovertyPct", "MedianAge"
)]
counties <- counties[order(counties$CountyFIPS), ]
stopifnot(!anyDuplicated(counties$CountyFIPS))
utils::write.csv(counties, "data/county_education_income_2024.csv", row.names = FALSE, na = "")

metadata <- list(
  source = "U.S. Census Bureau, 2020–2024 American Community Survey five-year estimates",
  source_url = "https://www.census.gov/data/developers/data-sets/acs-5year.html",
  public_query = public_query,
  downloaded = as.character(Sys.Date()),
  geography = "All counties and county equivalents in the 50 states and District of Columbia; Puerto Rico excluded",
  income_units = "Median household income in 2024 inflation-adjusted dollars",
  education_universe = "Population age 25 and older",
  poverty_universe = "Population for whom poverty status is determined",
  variables = as.list(variables),
  transformations = list(
    BachelorsPlus = "Bachelors + Masters + Professional + Doctorate",
    CollegePct = "100 * BachelorsPlus / Adults25Plus",
    IncomeK = "MedianIncome / 1000",
    PovertyPct = "100 * BelowPoverty / PovertyUniverse",
    missing = "Negative Census sentinel values replaced by missing values"
  ),
  rows = nrow(counties)
)
jsonlite::write_json(metadata, "data/county_education_income_2024_source.json",
                     auto_unbox = TRUE, pretty = TRUE)
cat("Saved", nrow(counties), "county estimates; no credentials were saved.\n")
