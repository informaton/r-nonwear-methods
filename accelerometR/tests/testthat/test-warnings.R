context("check_training_set_size function example warnings")

test_that("1 subject in dataset warning appear.", {
    expect_warning( choose_training_set_size(num_subjects = 1, training_fraction = 0.75),
    	"There is only 1 subject in your dataset, not enough to produce an estimate of test error." )
})

test_that("training_fraction specified was invalid warning appeared.", {
    expect_warning( choose_training_set_size(num_subjects = 16, training_fraction = 0),
    	"The training_fraction you specified was invalid. Using default value of training_fraction = 0.75." )
})

test_that("training_fraction you specified is smaller than recommended warning appeared.", {
    expect_warning( choose_training_set_size(num_subjects = 16, training_fraction = 1e-04),
    	"The training_fraction you specified is smaller than recommended and may result in an inflated estimate of prediction error. Typical values for training_fraction are at least 0.5." )
})

