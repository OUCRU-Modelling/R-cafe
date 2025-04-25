renv::init()
covid_cases <- readRDS("C:/Users/Admin/OneDrive/Documents/GitHub/R-cafe/day1/data/covid_cases.rds")
str(covid_cases)
head(covid_cases)

### CHECKING IF DATE COLUMN IS IN DATE FORMAT
is_date = isinstance(covidc_case$date, (datetime, date)) #does not work -why?
is.Date <- function(covid_cases$date) {
  inherits(covid_cases$date, c("Date", "POSIXt"))
} #does not work too
help(class)
help(is.date)
??is.date
class(covid_cases$date) == "Date"
inherits(covid_cases$date, "Date")
typeof(covid_cases$date)
# NOW INSTALL LUBRIDATE
library(lubridate)
is.Date(covid_cases$date)

### MIN AND MAX DATE OF REPORT
?(min)
first_report_date <- min(covid_cases$date, na.rm=TRUE)
first_report_date
last_report_date <- max(covid_cases$date, na.rm=TRUE)
last_report_date

### SUM A ROW
rowsum(covid_cases)
