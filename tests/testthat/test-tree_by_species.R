library(testthat)

test_that("tree_by_species prints a 'phylo' object", {
  expect_s3_class(tree_by_species(c("Herbertus sendtneri", "Micropterygium carinatum", "Lepidozia pinnaticruris", "Bazzania pallidevirens", "Bazzania jamaicensis", "Plagiochila simplex", "Plagiochila revolvens")), "phylo")
})

test_that("tree_by_species is precise with relationships", {
  test_tree <- tree_by_species(c("Herbertus sendtneri", "Micropterygium carinatum", "Lepidozia pinnaticruris", "Bazzania pallidevirens", "Bazzania jamaicensis", "Plagiochila simplex", "Plagiochila revolvens"))
  rel <- matrix(c(8, 9, 9, 8, 10, 11, 12, 12, 11, 11, 10, 9, 1, 2, 10, 11, 12, 3, 4, 5, 6, 7), ncol=2)
  expect_equal(test_tree$edge, rel)
})

test_that("tree_by_species is standarizing names", {
  test_tree <- tree_by_species(c("Herbertus_sendtneri", "Micropterygium Carinatum", "LEPIDOZIA PINNATICRURIS", "Bazzania_PALLIDEVIRENS", "BAZZANIA jamaicensis", "Plagiochila simplex", "Plagiochila revolvens"))
  rel_tip_label <- c("Plagiochila revolvens", "Plagiochila simplex", "Bazzania jamaicensis", "Bazzania pallidevirens", "Lepidozia pinnaticruris", "Micropterygium carinatum", "Herbertus sendtneri")
  expect_equal(test_tree$tip.label, rel_tip_label)
})

test_that("tree_by_species is idenifying the mismatches", {
  w <- "The following species were not found in the input tree and will be ignored: Herbertus sendtnaeri, Micropterygium caritum"
  expect_warning(tree_by_species(c("Herbertus sendtnaeri", "Micropterygium caritum", "Lepidozia pinnaticruris", "Bazzania pallidevirens", "Bazzania jamaicensis", "Plagiochila simplex", "Plagiochila revolvens")),
                 w)
})
