library(testthat)

test_that("tree_by_family prints a 'phylo' object", {
  expect_s3_class(tree_by_family(c("PLAGIOCHILACEAE", "RADULACEAE", "METZGERIACEAE", "MARCHANTIACEAE", "AYTONIACEAE", "LEPICOLEACEAE", "JUNGERMANNIACEAE")), "phylo")
})

test_that("tree_by_family is precise with relationships", {
  test_tree <- tree_by_family(c("PLAGIOCHILACEAE", "RADULACEAE", "METZGERIACEAE", "MARCHANTIACEAE", "AYTONIACEAE", "LEPICOLEACEAE", "JUNGERMANNIACEAE"))
  rel <- matrix(c(8, 9, 9, 8, 10, 10, 11, 11, 12, 12, 13, 13, 9, 1, 2, 10, 3, 11, 4, 12, 5, 13, 6, 7), ncol=2)
  expect_equal(test_tree$edge, rel)
})

