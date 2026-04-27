test_that("tag_venture_description returns expected structure", {
  desc <- "AI platform for enterprise analytics"
  
  result <- tag_venture_description(desc)
  
  expect_type(result, "list")
  expect_named(result, c("tech", "product", "industry"))
})

test_that("detects AI-related synonyms", {
  desc <- "This platform uses machine learning models for inference"
  
  result <- tag_venture_description(desc)
  
  expect_true("AI" %in% result$tech)
})

test_that("detects SaaS and Platform product tags", {
  desc <- "A cloud-based software-as-a-service platform for developers"
  
  result <- tag_venture_description(desc)
  
  expect_true("SaaS" %in% result$product)
  expect_true("Platform" %in% result$product)
})

test_that("detects industry-specific synonyms", {
  desc <- "Software for electric vehicle charging networks in the energy sector"
  
  result <- tag_venture_description(desc)
  
  expect_true("Energy" %in% result$industry)
})

test_that("returns empty vectors when no matches", {
  desc <- "A completely unrelated description with no keywords"
  
  result <- tag_venture_description(desc)
  
  expect_length(result$tech, 0)
  expect_length(result$product, 0)
  expect_length(result$industry, 0)
})

test_that("handles case insensitivity", {
  desc <- "AI PLATFORM for ENTERPRISE SOFTWARE"
  
  result <- tag_venture_description(desc)
  
  expect_true("AI" %in% result$tech)
  expect_true("Platform" %in% result$product)
  expect_true("Enterprise Software" %in% result$product)
})

test_that("handles multiple matches per category", {
  desc <- "AI-powered robotics platform with IoT sensors"
  
  result <- tag_venture_description(desc)
  
  expect_true(all(c("AI", "Robotics", "IoT") %in% result$tech))
})

test_that("detects synonym-expanded AI terms", {
  desc <- "The product uses large language models and natural language processing"
  
  result <- tag_venture_description(desc)
  
  expect_true("AI" %in% result$tech)
})

test_that("detects cloud infrastructure synonyms", {
  desc <- "The company provides Kubernetes-based application hosting and distributed storage"
  
  result <- tag_venture_description(desc)
  
  expect_true("Cloud Infrastructure" %in% result$tech)
})

test_that("detects data security synonyms", {
  desc <- "The platform supports confidential computing, encryption, and audit logs"
  
  result <- tag_venture_description(desc)
  
  expect_true("Data Security" %in% result$tech)
  expect_true("Monitoring" %in% result$product)
})

test_that("detects Canadian and American spelling variants", {
  desc <- "The software optimises production planning and supports regulatory labeling"
  
  result <- tag_venture_description(desc)
  
  expect_true("Automation" %in% result$product)
  expect_true("Foodservice" %in% result$industry)
})

test_that("detects geospatial synonyms", {
  desc <- "A 3D scanning and remote sensing platform for spatial data"
  
  result <- tag_venture_description(desc)
  
  expect_true("Geospatial" %in% result$tech)
})