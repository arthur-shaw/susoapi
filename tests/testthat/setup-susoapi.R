library("vcr") # *Required* as vcr is set up on loading

# locate vcr fixtures directory 
vcr_dir <- vcr::vcr_test_path("fixtures")

# if credentials are empty, replace with fake values to spoof functions for testing
if (!nzchar(Sys.getenv("SUSO_SERVER")) & !nzchar(Sys.getenv("SUSO_USER")) & !nzchar(Sys.getenv("SUSO_PASSWORD"))) {
  if (dir.exists(vcr_dir)) {
    Sys.setenv("SUSO_SERVER" = "<<fake_server>>")
    Sys.setenv("SUSO_USER" = "<<fake_user>>")
    Sys.setenv("SUSO_PASSWORD" = "<<fake_password>>")
  } else {
    stop("Cannot run tests. No fixtures folder found for `vcr`", call. = FALSE)
  }

}

# configure vcr, redacting secrets (i.e., server, user, and password)
invisible(vcr::vcr_configure(
  dir = vcr::vcr_test_path("fixtures"),
  filter_sensitive_data = list(
    "<<<fake_server>>>" = Sys.getenv('SUSO_SERVER'),
    "<<<fake_user>>>" = Sys.getenv('SUSO_USER'),
    "<<<fake_password>>>" = Sys.getenv('SUSO_PASSWORD')
  ), 
  write_disk_path = vcr_dir
))

# check that cassette names are unique
vcr::check_cassette_names()

# delete any files downloaded during testing
# TODO: figure out how to write directory so that it works on Windows
# withr::defer(
#   file.remove(paste0(vcr::vcr_test_path("fixtures"), "/document.json")), 
#   teardown_env())
# TODO: do the same for zip file downloaded by text-export-get_file.R
