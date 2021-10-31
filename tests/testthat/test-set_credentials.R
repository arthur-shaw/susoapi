# store .REnviron
my_home_path <- Sys.getenv("HOME")
my_renv_path <- file.path(my_home_path, ".Renviron")

my_r_environ <- readLines(con = my_renv_path) # ingest .Renviron file

# creates REnviron file if none exists
test_that("Creates .REnviron file if none exists", {
    file.remove(my_renv_path)
    expect_true(!file.exists(my_renv_path))
    suppressMessages(
        set_credentials(
            server = "https://demo.mysurvey.solutions", 
            user = "FakeX1", 
            password = "Fake123456"
        )
    )
    expect_true(file.exists(my_renv_path))
})

# created file contains SuSo API credentials only
test_that("Created file contains API credentials", {
    created_file <- readLines(con = my_renv_path)
    expect_true(all(grepl(pattern = "^SUSO_", x = created_file)))
})

# updated REnviron contains both old entries and new SuSo API credentials
test_that("Updated REnviron contains SuSo API credentials and old entries", {
    file.remove(my_renv_path)
    fake_old_r_environ <- c(
        'ONE="TWO"',
        'THREE="FOUR"',
        'SUSO_SERVER = "https://demo.mysurvey.solutions"', 
        'SUSO_USER = "FakeX1"', 
        'SUSO_PASSWORD = "Fake123456"'
    )
    # write(x = fake_old_r_environ[1], file = my_renv_path, append = FALSE, sep = "\n")
    # write(x = fake_old_r_environ[2], file = my_renv_path, append = TRUE, sep = "\n")
    # write(x = fake_old_r_environ[1], file = my_renv_path, append = FALSE, sep = "\n")
    # purrr::walk(
    #     .x = fake_old_r_environ[2:5],
    #     .f = ~ write(x = .x, file = my_renv_path, append = TRUE, sep = "\n")
    # )
    writeLines(text = fake_old_r_environ, con = my_renv_path, sep = "\n") 
    suppressMessages(
        set_credentials(
            server = "https://example.com",
            user = "me",
            password = "you2"
        )
    )
    fake_new_r_environ <- c(
        'SUSO_SERVER = "https://example.com"',
        'SUSO_USER = "me"',
        'SUSO_PASSWORD = "you2"'        
    )
    updated_r_environ <- readLines(my_renv_path)
    # all old non-SuSo entries are in new .REnviron
    testthat::expect_true(all(fake_old_r_environ[1:2] %in% updated_r_environ))
    # all 
    all(fake_new_r_environ %in%updated_r_environ)

})

# message displayed
test_that("Message issued after function executed", {

    expect_message(
        set_credentials(
            server = "https://demo.mysurvey.solutions", 
            user = "FakeX1", 
            password = "Fake123456"
        )        
    )

})

# restore .REnvrion from before test
extract_credential <- function(r_environ, credential) {
    
    credential_line <- grep(pattern = paste0("^SUSO_", credential), x = r_environ)
    credential_entry <- r_environ[credential_line]
    credential_value <- stringr::str_extract(credential_entry, pattern = '(?<=[\"|\']).+(?=[\"|\'])')
}
suso_server <- extract_credential(r_environ = my_r_environ, credential = "SERVER")
suso_user <- extract_credential(r_environ = my_r_environ, credential = "USER")
suso_password <- extract_credential(r_environ = my_r_environ, credential = "PASSWORD")
writeLines(text = my_r_environ, con = my_renv_path)
set_credentials(
    server = suso_server,
    user = suso_user,
    password = suso_password
)
