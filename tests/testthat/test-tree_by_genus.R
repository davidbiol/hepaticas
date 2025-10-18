library(testthat)

test_that("tree_by_genus prints a 'phylo' object", {
  expect_s3_class(tree_by_genus(c("Bazzania", "Bazzania", "Calypogeia", "Trichocolea", "Leiomitra", "Mnioloma", "Paracromastigum", "Pseudocephalozia", "Telaranea", "Zoopsidella")), "phylo")
})

test_that("tree_by_genus is precise with relationships", {
  test_tree <- tree_by_genus(c("Bazzania", "Bazzania", "Calypogeia", "Trichocolea", "Leiomitra", "Mnioloma", "Paracromastigum", "Pseudocephalozia", "Telaranea", "Zoopsidella"))
  rel <- matrix(c(10, 11, 11, 10, 12, 13, 13, 12, 14, 14, 14, 14, 14, 11, 1, 2, 12, 13, 3, 4, 14, 5, 6, 7, 8, 9), ncol=2)
  expect_equal(test_tree$edge, rel)
})
