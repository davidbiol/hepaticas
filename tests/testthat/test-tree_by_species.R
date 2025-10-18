library(testthat)

test_that("tree_by_species prints a 'phylo' object", {
  expect_s3_class(tree_by_species(c("Herbertus_sendtneri", "Micropterygium_carinatum", "Lepidozia_pinnaticruris", "Bazzania_pallidevirens", "Bazzania_jamaicensis", "Plagiochila_simplex", "Plagiochila_revolvens")), "phylo")
})

test_that("tree_by_species is precise with relationships", {
  test_tree <- tree_by_species(c("Herbertus_sendtneri", "Micropterygium_carinatum", "Lepidozia_pinnaticruris", "Bazzania_pallidevirens", "Bazzania_jamaicensis", "Plagiochila_simplex", "Plagiochila_revolvens"))
  rel <- matrix(c(8, 9, 9, 8, 10, 11, 12, 12, 11, 11, 10, 9, 1, 2, 10, 11, 12, 3, 4, 5, 6, 7), ncol=2)
  expect_equal(test_tree$edge, rel)
})
