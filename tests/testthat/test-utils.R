# check_internet 

    # not high priority, since trust `curl` package

# is_guid

test_that("Valid GUID returns true", {

    # without hyphens
    expect_true(is_guid("07e64554929742efad07f7fe28768b37"))

    # with hyphens
    expect_true(is_guid("07e64554-9297-42ef-ad07-f7fe28768b37"))

})

# check_guid

test_that("Returns error message if GUID invalid", {

    expect_error(check_guid(guid = "123", fail_msg = "Yo!"))

})

test_that("Returns TRUE if GUID valid", {

    expect_true(check_guid(
        guid = "07e64554-9297-42ef-ad07-f7fe28768b37", 
        fail_msg = "Yo!"
    ))

})

# is_user_name
test_that("Valid user name returns TRUE", {

    expect_true(is_user_name("HelloWorld123"))

})

test_that("Invalid user anme returns FALSE", {

    expect_false(is_user_name("A1"))

})

# logical_to_string
test_that("Returns correct string as a function of value", {

    # TRUE => "true"
    expect_identical(logical_to_string(TRUE), "true")

    # FALSE => "false"
    expect_identical(logical_to_string(FALSE), "false")

    # anything else => ""
    expect_identical(logical_to_string(NA), "")

})
