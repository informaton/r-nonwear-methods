context("check class_tables function for equality")

test_that("class_tables function outputs for hmm actuals are equal", {
	predictions <- predict_accelerometR(acclR_data)$hmm_pred
	class_tables <- compute_class_tables(predictions,acclR_data$state, acclR_data$id)
    expect_equal( class_tables$frequency$by_actual[[1]], 785 )
    expect_equal( class_tables$frequency$by_actual[[2]], 1544 )
    expect_equal( class_tables$frequency$by_actual[[3]], 1513 )
    expect_equal( class_tables$frequency$by_actual[[4]], 1093 )
    expect_equal( class_tables$frequency$by_actual[[5]], 11633 )
    expect_equal( class_tables$frequency$by_actual[[6]], 2649 )
    expect_equal( class_tables$frequency$by_actual[[7]], 2142 )
    expect_equal( class_tables$frequency$by_actual[[8]], 603 )
    expect_equal( class_tables$frequency$by_actual[[9]], 4043 )
})


