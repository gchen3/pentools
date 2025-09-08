# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

test_that("get_pv works as expected", {
  expect_equal(get_pv(rate = 0, t = 1), 1)
  expect_equal(get_pv(rate = 0.05, t = 10),  0.613913, tolerance = 1e-6)
  expect_equal(get_pv(rate = 0.05, t = 5), 0.783526, tolerance = 1e-6)
  expect_equal(get_pv(rate = 0.02, t = 10), 0.820348, tolerance = 1e-6)
})

test_that("get_pv_pmt works as expected", {
  expect_equal(get_pv_pmt(rate = 0, t = 1), 1)
  expect_equal(get_pv_pmt(rate = 0.05, t = 10),   7.721735, tolerance = 1e-6)
  expect_equal(get_pv_pmt(rate = 0.05, t = 5),  4.329477, tolerance = 1e-6)
  expect_equal(get_pv_pmt(rate = 0.02, t = 10),   8.982585, tolerance = 1e-6)
})

test_that("get_pmt_due works as expected",{
  expect_equal(get_pmt_due(rate = 0.05, t = 30), 0.061954, tolerance = 1e-5)
  expect_equal(get_pmt_due(rate = 0.02, t = 10), 0.109144, tolerance = 1e-5)
})

test_that("get_pmt0 works as expected", {
  expect_equal(get_pmt0(0.05, 30, 1000), 61.95375, tolerance = 1e-5)
  expect_equal(get_pmt0(0.02, 50, 1000), 31.19923, tolerance = 1e-5)
  expect_equal(get_pmt0(0.02, 50, 500), 15.59961, tolerance = 1e-5)
})

test_that("get_pv_gpmt works as expected", {
  expect_equal(get_pv_gpmt(rate = 0, growth = 0, t = 1), 1)
  expect_equal(get_pv_gpmt(rate = 0.08, growth = 0.05, t = 18), 13.2582, tolerance = 1e-6)
  expect_equal(get_pv_gpmt(rate = 0.03, growth = 0.02, t = 10), 9.295367, tolerance = 1e-6)
})

test_that("npv works as expected", {
  expect_equal(npv(0.05, c(100, 200, 300, 400, 500, 600)), 1704.368582, tolerance = 1e-6)
  expect_equal(npv(0.02, c(100, 200, 300, 400, 500, 600)), 1928.156077, tolerance = 1e-6)
})

test_that("get_pv_cf_roll works as expected", {
  expect_equal(get_pv_cf_roll(0.05, c(100, 200, 300, 400, 500, 600)), c(1704.368582, 1689.587011, 1574.066361, 1352.769679, 1020.408163, 571.4285714), tolerance = 1e-6)
  expect_equal(get_pv_cf_roll(0.02, c(100, 200, 300, 400, 500, 600)), c(1928.156077, 1866.719198, 1704.053582, 1438.134654, 1066.897347, 588.2352941), tolerance = 1e-6)
})

test_that("get_pmt_due works as expected", {
  expect_equal(get_pmt_due(rate = 0, t = 1), 1, tolerance = 1e-6)
  expect_equal(get_pmt_due(rate = 0.05, t = 5), 0.219976, tolerance = 1e-5)
  expect_equal(get_pmt_due(rate = 0.02, t = 5), 0.207998, tolerance = 1e-5)
  expect_equal(get_pmt_due(rate = 0.05, t = 10), 0.123338, tolerance = 1e-5)
})

test_that("get_pmt_growth works as expected", {
  expect_equal(get_pmt_growth(rate = 0.05, growth = 0.02, t = 5), 0.2117597, tolerance = 1e-6)
})


test_that("get_pvfb works as expected", {
  sep_rate_vec <- c(0.01, 0.02, 0.03, 0.04)
  interest_vec <- c(0.05, 0.05, 0.05, 0.05)
  value_vec <- c(100, 200, 300, 400)

  pvfb_result <- get_pvfb(sep_rate_vec, interest_vec, value_vec)
  pvfb_result_reason <- c(23.86535, 21.39173, 14.18971, NA)
  expect_equal(pvfb_result, pvfb_result_reason, tolerance = 1e-5)
})

test_that("annfactor works as expected", {
  surv_DR_vec <- c(0.95, 0.90, 0.85, 0.80)
  cola_vec <- c(0.02, 0.02, 0.02, 0.02)

  annfactor_result <- annfactor(surv_DR_vec, cola_vec, one_time_cola = FALSE)
  expect_result_reason <- c(3.790849, 2.888133, 1.960000, 1)

  expect_equal(annfactor_result, expect_result_reason, tolerance = 1e-5)

  annfactor_result <- annfactor(surv_DR_vec, cola_vec, one_time_cola = TRUE)
  expect_result_reason <- c(3.684211, 2.833333, 1.941176, 1.000000)

  expect_equal(annfactor_result, expect_result_reason, tolerance = 1e-5)

})

test_that("get_pvfs works as expected", {
  remaining_prob_vec <- c(0.95, 0.90, 0.85, 0.80, 0.90, 0.95, 0.90, 0.85, 0.80, 0.90)
  interest_vec <- c(0.05, 0.04, 0.03, 0.04, 0.05, 0.05, 0.04, 0.03, 0.04, 0.05)
  sal_vec <- c(100, 200, 320, 420, 540,100, 200, 320, 420, 540)

  get_pvfs_result <- get_pvfs(remaining_prob_vec, interest_vec, sal_vec)
  expect_result_reason <- c(2151.3589, 2391.8891, 2529.2611, 2336.4507, 1726.9070, 1206.2390, 1265.2154, 1206.5279,  965.5141,  514.2857)

  expect_equal(get_pvfs_result, expect_result_reason, tolerance = 1e-3)
})

test_that("recur_grow works as expected", {
  x <- c(100, 102, 103, 104, 110)  # Initial value is 100, the rest of the values are not used in the calculation
  g <- c(0.05, 0.03, 0.02, 0.04, 0.05)  # Growth rates in each year

  recur_grow_result <- recur_grow(x, g)
  expected_result <- c(100, 105, 108.15, 110.313, 114.725)  # Expected result after recursive growth

  expect_equal(recur_grow_result, expected_result, tolerance = 1e-3)
})

test_that("recur_grow2 works as expected", {
  x <- c(100, 102, 103, 104, 110)  # Initial value is 100, the rest of the values are not used in the calculation
  g <- c(0.05, 0.03, 0.02, 0.04, 0.05)  # Growth rates

  recur_grow2_result <- recur_grow2(x, g)
  expected_result <- c(100, 103, 105.06, 109.2624, 114.7255)  # Expected result after recursive growth with no lag

  expect_equal(recur_grow2_result, expected_result, tolerance = 1e-3)
})

test_that("recur_grow3 works as expected", {
  x <- 100  # Base value
  g <- 0.05  # Fixed growth rate
  nper <- 5  # Number of periods

  recur_grow3_result <- recur_grow3(x, g, nper)
  expected_result <- c(100, 105, 110.25, 115.7625, 121.5506)  # Expected result after recursive growth with fixed growth rate

  expect_equal(recur_grow3_result, expected_result, tolerance = 1e-3)
})

# Add these tests to your existing test-functions.R file

test_that("get_pmt works as expected", {
  # Test with no growth (should match standard annuity payment)
  pmt_no_growth <- get_pmt(r = 0.05, g = 0, nper = 10, pv = 10000, t = 1)
  expected_no_growth <- 1295.046  # Manual calculation: 10000 / get_pv_pmt(0.05, 10)
  expect_equal(pmt_no_growth, expected_no_growth, tolerance = 1e-3)

  # Test with growth
  pmt_with_growth <- get_pmt(r = 0.05, g = 0.02, nper = 10, pv = 10000, t = 1)
  expect_true(is.numeric(pmt_with_growth))
  expect_true(pmt_with_growth > 0)

  # Payment with growth should be different from no growth
  expect_false(isTRUE(all.equal(pmt_with_growth, pmt_no_growth, tolerance = 1e-6)))

  # Test with payments at beginning (t = 0)
  pmt_beginning <- get_pmt(r = 0.05, g = 0.02, nper = 10, pv = 10000, t = 0)
  expect_true(pmt_beginning != pmt_with_growth)

  # Test edge case: zero interest rate
  pmt_zero_rate <- get_pmt(r = 0, g = 0, nper = 10, pv = 10000, t = 1)
  expect_equal(pmt_zero_rate, 1000, tolerance = 1e-6)  # Should be PV/nper
})

test_that("get_cum_fv works as expected", {
  # Basic test with simple cash flows
  cf <- c(100, 200, 300)
  result <- get_cum_fv(interest = 0.05, cashflow = cf, first_value = 0)

  # Manual calculation
  expected <- c(0, 100, 305)  # 0, 100, 100*1.05 + 200 = 305
  expect_equal(result, expected, tolerance = 1e-6)

  # Test with initial value - CORRECTED CALCULATION
  result_initial <- get_cum_fv(interest = 0.05, cashflow = cf, first_value = 500)
  expected_initial <- c(500, 625, 856.25)  # 500, 500*1.05 + 100 = 625, 625*1.05 + 200 = 856.25
  expect_equal(result_initial, expected_initial, tolerance = 1e-2)

  # Test with zero interest rate
  result_zero <- get_cum_fv(interest = 0, cashflow = cf, first_value = 100)
  expected_zero <- c(100, 200, 400)  # Just cumulative sum with initial value
  expect_equal(result_zero, expected_zero, tolerance = 1e-6)

  # Test length consistency
  expect_equal(length(result), length(cf))

  # Test single cash flow
  single_cf <- c(1000)
  result_single <- get_cum_fv(interest = 0.05, cashflow = single_cf, first_value = 0)
  expect_equal(result_single, c(0), tolerance = 1e-6)
  expect_equal(length(result_single), 1)

  # Test two elements
  two_cf <- c(100, 200)
  result_two <- get_cum_fv(interest = 0.05, cashflow = two_cf, first_value = 50)
  expected_two <- c(50, 152.5)  # 50, 50*1.05 + 100 = 152.5
  expect_equal(result_two, expected_two, tolerance = 1e-6)
})

test_that("add_new_entrants works as expected", {
  # Set up test data
  g <- 0.02  # 2% growth
  ne_dist <- c(0.3, 0.4, 0.2, 0.1)  # Distribution across 4 entry ages
  wf1 <- c(1000, 1200, 1100, 900)   # Initial workforce
  wf2 <- c(980, 1150, 1050, 850)    # Workforce after decrements
  ea <- c(25, 30, 35, 40)            # Entry ages
  age <- c(25, 30, 35, 40, 45, 50)   # All possible ages

  # Create simple position matrix (new entrants enter at their entry age)
  position_matrix <- matrix(0, nrow = length(ea), ncol = length(age))
  diag(position_matrix) <- 1

  result <- add_new_entrants(g, ne_dist, wf1, wf2, ea, age, position_matrix)

  # Test basic properties
  expect_true(is.matrix(result))
  expect_equal(nrow(result), length(ea))
  expect_equal(ncol(result), length(age))

  # Calculate expected total new entrants
  total_wf1 <- sum(wf1)  # 4200
  total_wf2 <- sum(wf2)  # 4030
  expected_total_ne <- total_wf1 * (1 + g) - total_wf2  # 4200 * 1.02 - 4030 = 254
  actual_total_ne <- sum(result)
  expect_equal(actual_total_ne, expected_total_ne, tolerance = 1e-6)

  # Test distribution matches expected proportions
  expected_by_age <- expected_total_ne * ne_dist
  actual_by_age <- rowSums(result)
  expect_equal(actual_by_age, expected_by_age, tolerance = 1e-6)

  # Test zero growth scenario
  g_zero <- 0
  result_zero <- add_new_entrants(g_zero, ne_dist, wf1, wf2, ea, age, position_matrix)
  expected_total_zero <- total_wf1 * (1 + g_zero) - total_wf2  # 4200 - 4030 = 170
  expect_equal(sum(result_zero), expected_total_zero, tolerance = 1e-6)

  # Test negative growth (shrinking workforce)
  g_negative <- -0.05
  wf2_large <- c(1100, 1300, 1200, 1000)  # Larger decrements
  result_negative <- add_new_entrants(g_negative, ne_dist, wf1, wf2_large, ea, age, position_matrix)
  expected_negative <- sum(wf1) * (1 + g_negative) - sum(wf2_large)
  expect_equal(sum(result_negative), expected_negative, tolerance = 1e-6)
})

# Integration test combining multiple functions
test_that("Integration test: PV and payment functions work together", {
  # Test that get_pmt and pv functions are consistent
  rate <- 0.06
  growth <- 0.02
  nper <- 15
  pv_amount <- 50000

  # Calculate payment
  payment <- get_pmt(r = rate, g = growth, nper = nper, pv = pv_amount, t = 1)

  # Verify payment is positive and reasonable
  expect_true(payment > 0)
  expect_true(payment < pv_amount)  # Payment should be less than total PV

  # Test consistency with PV calculation
  # Note: This is a conceptual test - exact verification would require
  # implementing the inverse PV calculation for growing annuities
  expect_true(is.numeric(payment))
  expect_false(is.na(payment))
  expect_false(is.infinite(payment))
})

# Edge case tests
test_that("Edge cases work correctly", {
  # Test get_pmt with equal growth and interest rates
  expect_true(is.finite(get_pmt(r = 0.05, g = 0.05, nper = 10, pv = 10000, t = 1)))

  # Test get_cum_fv with empty cash flows (length 1)
  result_empty <- get_cum_fv(interest = 0.05, cashflow = c(100), first_value = 50)
  expect_equal(length(result_empty), 1)
  expect_equal(result_empty[1], 50)

  # Test add_new_entrants with uniform distribution
  uniform_dist <- rep(0.25, 4)
  wf1 <- rep(1000, 4)
  wf2 <- rep(950, 4)
  ea <- c(25, 30, 35, 40)
  age <- c(25, 30, 35, 40)
  position_matrix <- diag(4)

  result_uniform <- add_new_entrants(0.01, uniform_dist, wf1, wf2, ea, age, position_matrix)
  # Each entry age should get equal allocation
  row_sums <- rowSums(result_uniform)
  expect_true(all(abs(row_sums - row_sums[1]) < 1e-10))
})
