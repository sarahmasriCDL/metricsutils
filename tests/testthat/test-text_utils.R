test_that("tag_venture_description returns a list", {
  desc <- "We develop satellite-based earth observation tools for environmental monitoring"
  result <- tag_venture_description(desc)
  expect_type(result, "list")
})

test_that("returns empty list when no keywords match", {
  desc <- "A completely unrelated description with no matching keywords"
  result <- tag_venture_description(desc)
  expect_equal(length(result), 0)
})

test_that("stream names are valid CDL streams", {
  desc <- "We develop solar panels and battery storage for renewable energy grids"
  result <- tag_venture_description(desc)
  valid_streams <- c("Climate", "Compute", "BME", "Minerals", "Defence")
  expect_true(all(names(result) %in% valid_streams))
})

test_that("detects Climate: Energy & Power Generation", {
  desc <- "We develop solar panels and battery storage for renewable energy grids"
  result <- tag_venture_description(desc)
  expect_true("Energy & Power Generation" %in% result$Climate)
})

test_that("detects Climate: Carbon Management", {
  desc <- "Our platform helps companies decarbonize by tracking emissions and carbon capture"
  result <- tag_venture_description(desc)
  expect_true("Carbon Management" %in% result$Climate)
})

test_that("detects Compute: Robotics", {
  desc <- "We build humanoid robots for warehouse logistics"
  result <- tag_venture_description(desc)
  expect_true("Robotics" %in% result$Compute)
})

test_that("detects Compute: Edge AI Hardware & Optimization", {
  desc <- "Our FPGA-based accelerator enables on-device model inference at the edge"
  result <- tag_venture_description(desc)
  expect_true("Edge AI Hardware & Optimization" %in% result$Compute)
})

test_that("detects BME: Diagnostics & Therapeutics", {
  desc <- "We develop companion diagnostics and genomic biomarker tests for personalized medicine"
  result <- tag_venture_description(desc)
  expect_true("Diagnostics & Therapeutics" %in% result$BME)
})

test_that("detects BME: Wearable & Assistive Technologies", {
  desc <- "Our wearable biosensor enables remote monitoring of chronic conditions"
  result <- tag_venture_description(desc)
  expect_true("Wearable & Assistive Technologies" %in% result$BME)
})

test_that("detects Minerals: Ore Body Knowledge", {
  desc <- "Using hyperspectral imaging and geophysics to improve ore body characterization"
  result <- tag_venture_description(desc)
  expect_true("Ore Body Knowledge" %in% result$Minerals)
})

test_that("detects Minerals: Processing", {
  desc <- "Our hydrometallurgy process improves mineral processing efficiency through leaching"
  result <- tag_venture_description(desc)
  expect_true("Processing" %in% result$Minerals)
})

test_that("detects Defence: Space & ISR", {
  desc <- "We provide satellite communications and earth observation for ISR missions"
  result <- tag_venture_description(desc)
  expect_true("Space & ISR" %in% result$Defence)
})

test_that("detects Defence: Autonomy & Robotics", {
  desc <- "Our UAV platform supports uncrewed autonomous systems for defence applications"
  result <- tag_venture_description(desc)
  expect_true("Autonomy & Robotics" %in% result$Defence)
})

test_that("handles case insensitivity", {
  desc <- "SATELLITE-BASED EARTH OBSERVATION using EDGE AI PROCESSORS"
  result <- tag_venture_description(desc)
  expect_true("Space & ISR" %in% result$Defence)
  expect_true("Edge AI Hardware & Optimization" %in% result$Compute)
})

test_that("matches multiple streams for cross-domain ventures", {
  desc <- "We develop satellite-based earth observation tools for environmental monitoring using edge AI processors"
  result <- tag_venture_description(desc)
  expect_true("Defence" %in% names(result))
  expect_true("Compute" %in% names(result))
})

# ── new_line_to_commas ─────────────────────────────────────────────────────────

test_that("new_line_to_commas joins multiple lines with comma and space", {
  result <- new_line_to_commas("apple\nbanana\ncherry")
  expect_equal(result, "apple, banana, cherry")
})

test_that("new_line_to_commas returns input unchanged when no newlines", {
  result <- new_line_to_commas("only one line")
  expect_equal(result, "only one line")
})

test_that("new_line_to_commas returns empty string for empty input", {
  result <- new_line_to_commas("")
  expect_equal(result, "")
})

test_that("new_line_to_commas handles two lines", {
  result <- new_line_to_commas("first\nsecond")
  expect_equal(result, "first, second")
})

test_that("new_line_to_commas preserves whitespace within lines", {
  result <- new_line_to_commas("hello world\nfoo bar")
  expect_equal(result, "hello world, foo bar")
})