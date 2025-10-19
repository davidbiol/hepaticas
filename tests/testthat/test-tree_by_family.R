library(testthat)

test_that("tree_by_family prints a 'phylo' object", {
  expect_s3_class(tree_by_family(c("Plagiochilaceae", "Radulaceae", "Metzgeriaceae", "Marchantiaceae", "Aytoniaceae", "Lepicoleaceae", "Jungermanniaceae")), "phylo")
})

test_that("tree_by_family is precise with relationships", {
  test_tree <- tree_by_family(c("Plagiochilaceae", "Radulaceae", "Metzgeriaceae", "Marchantiaceae", "Aytoniaceae", "Lepicoleaceae", "Jungermanniaceae"))
  rel <- matrix(c(8, 9, 9, 8, 10, 10, 11, 11, 12, 12, 13, 13, 9, 1, 2, 10, 3, 11, 4, 12, 5, 13, 6, 7), ncol=2)
  expect_equal(test_tree$edge, rel)
})

test_that("tree_by_family is standarizing names", {
  test_tree <- tree_by_family(c("PLAGIOCHILACEAE", "RaduLACEAE", "metzgeriaceae", "mARCHANTIACEAE", "Aytoniaceae", "Lepicoleaceae", "Jungermanniaceae"))
  rel_tip_label <- c("Marchantiaceae", "Aytoniaceae", "Metzgeriaceae", "Radulaceae", "Jungermanniaceae", "Plagiochilaceae", "Lepicoleaceae")
  expect_equal(test_tree$tip.label, rel_tip_label)
})

test_that("tree_by_family is idenifying the mismatches", {
  w <- "The following family were not found in the input tree and will be ignored: Plagiochileaceae, Radulaeae"
  expect_warning(tree_by_family(c("Plagiochileaceae", "Radulaeae", "Metzgeriaceae", "Marchantiaceae", "Aytoniaceae", "Lepicoleaceae", "Jungermanniaceae")),
                 w)
})


