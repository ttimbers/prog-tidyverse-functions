library(digest)
library(palmerpenguins)
library(stringr)
library(testthat)

test_1 <- function(){
    fun_body <- paste(deparse(body(get_rectangle)), collapse = "")
    answer <- get_rectangle(penguins, body_mass_g > 3000, species:island)
    answer2 <- get_rectangle(penguins, body_mass_g > 3000)
    test_that("get_rectangle should return a data frame", {
        expect_true(is.data.frame(answer))
    })
    test_that('get_rectangle is not working correctly', {
        expect_equal(nrow(answer), 331)
        expect_equal(ncol(answer), 2)
        expect_equal(paste(tolower(sort(colnames(answer))), collapse = ""), 'islandspecies')
    })
    test_that('function should select everything if nothing is provided for the third argument', {
        expect_equal(nrow(answer2), 331)
        expect_equal(ncol(answer2), 8)
        expect_equal(paste(tolower(sort(colnames(answer2))), collapse = ""), 'bill_depth_mmbill_length_mmbody_mass_gflipper_length_mmislandsexspeciesyear')
    })
    test_that("`{{` was supposed to be used to embrace row_filter", {
        expect_true(str_detect(fun_body, "\\{[ ]*\\{[ ]*row_filter[ ]*\\}[ ]*\\}"))
    })
    test_that("`{{` was supposed to be used to embrace column_range", {
        expect_true(str_detect(fun_body, "\\{[ ]*\\{[ ]*column_range[ ]*\\}[ ]*\\}"))
    })
    print("success!")
}

test_2 <- function(){
    if ("a" %in% tolower(answer_2)) {
        print("Yes, R does do lazy evaluation, however this is not one of the reasons why we need to embrace (or do something equivalent) to unquoted column names.")
    } else if ("c" %in% tolower(answer_2)) {
        print("Column names can be referred to as strings in R when using the square bracket notation.")
    }
    test_that('answer is incorrect', {
        expect_equal(digest(paste(sort(tolower(answer_2)), collapse = "")), '95767987b2037a2f09c4e5c0997ec206')
    })
    print("success!")
}

test_3 <- function(){
    fun_body <- paste(deparse(body(nest_and_count)), collapse = "")
    answer <- nest_and_count(penguins, species)
    test_that("nest_and_count should return a data frame", {
        expect_true(is.data.frame(answer))
    })
    test_that('nest_and_count is not working correctly', {
        expect_equal(nrow(answer), 3)
        expect_equal(ncol(answer), 3)
        expect_equal(paste(tolower(sort(colnames(answer))), collapse = ""), 'datanspecies')
        expect_equal(sum(as.numeric(answer$n)), 344)
    })
    test_that("`:=` was supposed to be used for assignment", {
        expect_true(str_detect(fun_body, ":="))
    })
    print("success!")
}

test_4 <- function(){
    fun_body <- paste(deparse(body(get_rectangle2)), collapse = "")
    answer <- get_rectangle2(penguins, body_mass_g > 3000, species:island)
    test_that("get_rectangle2 should return a data frame", {
        expect_true(is.data.frame(answer))
    })
    test_that('get_rectangle2 is not working correctly', {
        expect_equal(nrow(answer), 331)
        expect_equal(ncol(answer), 2)
        expect_equal(paste(tolower(sort(colnames(answer))), collapse = ""), 'islandspecies')
    })
    test_that("`{{` was supposed to be used to embrace row_filter", {
        expect_true(str_detect(fun_body, "\\{[ ]*\\{[ ]*row_filter[ ]*\\}[ ]*\\}"))
    })
    test_that("`...` was supposed to be used inside select", {
        expect_true(str_detect(fun_body, "select(...)"))
    })
    print("success!")
}

test_5 <- function(){
    fun_body <- paste(deparse(body(select_and_arrange)), collapse = "")
    answer <- select_and_arrange(penguins, body_mass_g, species:island, sex)
    test_that("select_and_arrange should return a data frame", {
        expect_true(is.data.frame(answer))
    })
    test_that('select_and_arrange is not working correctly', {
        expect_equal(nrow(answer), 344)
        expect_equal(ncol(answer), 3)
        expect_equal(paste(tolower(sort(colnames(answer))), collapse = ""), 'islandsexspecies')
    })
    test_that("`{{` was supposed to be used to embrace sort_by", {
        expect_true(str_detect(fun_body, "\\{[ ]*\\{[ ]*sort_by[ ]*\\}[ ]*\\}"))
    })
    test_that("`...` was supposed to be used inside select", {
        expect_true(str_detect(fun_body, "select(...)"))
    })
    print("success!")
}

test_6 <- function(){
    fun_body <- paste(deparse(body(have_fun)), collapse = "")
    test_that("`...` was supposed to be used inside select", {
        expect_true(str_detect(fun_body, "select(...)|\\{[ ]*\\{[ ]*"))
    })
    print("success!")
}
