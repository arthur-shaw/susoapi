
<!-- README.md is generated from README.Rmd. Please edit that file -->

# susoapi <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->
<!-- badges: end -->

The goal of susoapi is to offer a user-friendly interface for Survey
Solutions’ RESTful and GraphQL APIs. The package provide an R wrapper
function for each Survey Solutions API endpoint, and in so doing enable
users to execute all API actions without leaving R.

## Installation

The package is not yet on CRAN, but can be installed via the following
command:

``` r
devtools::install_github("arthur-shaw/susoapi")
```

## Usage

### Scope

To execute an API action, one simply needs to find the appropriate
function for the object of interest:

-   Questionnaires
-   Assignments
-   Interviews
-   Users
-   Export

See the `Reference` for more details.

### Authentication

But before executing an action, users should provide record their
credentials for server authentication:

``` r
set_credentials(
  server = "https://example.server",
  user = "My_API_user1",
  password = "MySecretPassword2Day"
)
```

This accomplishes a few simultaneous goals:

-   Records credentials for reuse later–both within and across R
    sessions
-   Stores credentials in a safe place–outside of code

### Example commands

To get the list of questionnaires loaded on the server:

``` r
# get a dataframe of questionnaires and their attributes
all_questionnaires <- get_questionnaires()
```

To create users:

``` r
# create a supervisor
create_user(
  user_name = "TestSup000",
  user_password = "Test!Sup!000!",
  role = "Supervisor"
)

# create an interviewer in the supervisor's team
create_user(
  user_name = "TestInt007",
  user_password = "Test!Int!007!",
  role = "Interviewer",
  supervisor = "TestSup000"
)
```

To download data:

``` r
# STEP1: START AN EXPORT JOB
# specifying same same options as in user interface
# optionally specifying other options--including some not available in the UI
start_export(
  qnr_id = "72f7160c-12dc-4dd4-9df1-66af819e434d$1",
  export_type = "STATA",
  interview_status = "All",
  include_meta = TRUE
) -> started_job_id

# STEP 2: CHECK EXPORT JOB PROGESS, UNTIL COMPLETE
# specifying ID of job started in prior step
get_export_job_details(job_id = started_job_id)

# STEP 3: DOWNLOAD THE EXPORT FILE, ONCE THE JOB IS COMPLETE
# specifying:
# - job ID
# - where to download the file
get_export_file(
    job_id = started_job_id,
    path = "C:/your/file/path/"
)
```
