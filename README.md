# pentools

A comprehensive R package for actuarial and financial calculations, specifically designed for pension and employee benefit analysis. This package provides essential functions for present value calculations, annuity computations, cash flow analysis, and workforce planning.

## Installation

Install from GitHub using:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install pentools from GitHub
devtools::install_github("gchen3/pentools")
```

## Quick Start

```r
library(pentools)

# Calculate present value factor
get_pv(rate = 0.05, t = 3)

# Calculate NPV of cash flows
cf <- c(1000, 1200, 1500, 1800)
npv(rate = 0.06, cashflows = cf)

# Calculate payment for growing annuity
get_pmt(r = 0.05, g = 0.02, nper = 10, pv = 100000)
```

## Core Functions

### Present Value Calculations

- **`get_pv(rate, t)`** - Calculate present value factors for discounting future cash flows
- **`get_pv_pmt(rate, t)`** - Present value of standard annuity payments
- **`get_pv_gpmt(rate, growth, t)`** - Present value of growing annuity payments
- **`pv(rate, g, nper, pmt, t)`** - Comprehensive present value calculation for growing annuities
- **`npv(rate, cashflows)`** - Net present value of cash flow series
- **`get_pv_cf_roll(rate, cf)`** - Rolling present value of future cash flows

### Payment Calculations

- **`get_pmt(r, g, nper, pv, t)`** - Payment amount for growing annuities
- **`get_pmt0(r, nper, pv)`** - First payment of annuity due
- **`get_pmt_due(rate, t)`** - Payment factor for annuity due
- **`get_pmt_growth(rate, growth, t)`** - First payment for growth annuity due

### Future Value and Growth Functions

- **`get_cum_fv(interest, cashflow, first_value)`** - Cumulative future values with compound interest
- **`recur_grow(x, g)`** - Recursive growth with lag effect
- **`recur_grow2(x, g)`** - Recursive growth without lag
- **`recur_grow3(x, g, nper)`** - Recursive growth with fixed rate

### Actuarial Functions

- **`get_pvfb(sep_rate_vec, interest_vec, value_vec)`** - Present value of future benefits
- **`get_pvfs(remaining_prob_vec, interest_vec, sal_vec)`** - Present value of future salaries
- **`annfactor(surv_DR_vec, cola_vec, one_time_cola)`** - Annuity factors with survival and COLA adjustments
- **`roll_pv(rate, g, nper, pmt_vec, t)`** - Rolling present value calculations

### Workforce Planning

- **`add_new_entrants(g, ne_dist, wf1, wf2, ea, age, position_matrix)`** - Calculate and distribute new workforce entrants

## Usage Examples

### Basic Present Value Calculations

```r
# Present value factor for 5% rate over 3 years
pv_factor <- get_pv(0.05, 3)
# Result: 0.8638376

# Present value of $1 annuity for 10 years at 5%
annuity_pv <- get_pv_pmt(0.05, 10)
# Result: 7.721735

# NPV of cash flows
cash_flows <- c(100, 200, 300, 400, 500)
npv_result <- npv(0.05, cash_flows)
# Result: 1295.132
```

### Growing Annuity Calculations

```r
# Present value of growing annuity (5% rate, 2% growth, 10 periods)
pv_growing <- get_pv_gpmt(0.05, 0.02, 10)

# Calculate payment for growing annuity
payment <- get_pmt(r = 0.05, g = 0.02, nper = 15, pv = 50000, t = 1)
```

### Cash Flow Analysis

```r
# Cumulative future values
cashflows <- c(1000, 1200, 1500, 1800)
cum_fv <- get_cum_fv(interest = 0.04, cashflow = cashflows, first_value = 500)

# Rolling present value of cash flows
cf <- c(100, 200, 300, 400, 500, 600)
rolling_pv <- get_pv_cf_roll(0.05, cf)
```

### Actuarial Applications

```r
# Present value of future benefits
sep_rates <- c(0.01, 0.02, 0.03, 0.04)
interest_rates <- c(0.05, 0.05, 0.05, 0.05)
benefits <- c(100, 200, 300, 400)
pvfb <- get_pvfb(sep_rates, interest_rates, benefits)

# Annuity factors with COLA
survival_rates <- c(0.95, 0.90, 0.85, 0.80)
cola_rates <- c(0.02, 0.02, 0.02, 0.02)
ann_factors <- annfactor(survival_rates, cola_rates, one_time_cola = FALSE)
```

### Workforce Planning

```r
# Calculate new entrants needed for workforce growth
g <- 0.02  # 2% growth target
ne_dist <- c(0.3, 0.4, 0.2, 0.1)  # Entry age distribution
wf1 <- c(1000, 1200, 1100, 900)   # Initial workforce
wf2 <- c(980, 1150, 1050, 850)    # After decrements
ea <- c(25, 30, 35, 40)           # Entry ages
age <- c(25, 30, 35, 40, 45, 50)  # All ages

# Create position matrix
position_matrix <- matrix(0, nrow = length(ea), ncol = length(age))
diag(position_matrix) <- 1

new_entrants <- add_new_entrants(g, ne_dist, wf1, wf2, ea, age, position_matrix)
```

## Function Categories

| Category | Functions |
|----------|-----------|
| **Present Value** | `get_pv`, `get_pv_pmt`, `get_pv_gpmt`, `pv`, `npv`, `get_pv_cf_roll` |
| **Payments** | `get_pmt`, `get_pmt0`, `get_pmt_due`, `get_pmt_growth` |
| **Future Value** | `get_cum_fv`, `recur_grow`, `recur_grow2`, `recur_grow3` |
| **Actuarial** | `get_pvfb`, `get_pvfs`, `annfactor`, `roll_pv` |
| **Workforce** | `add_new_entrants` |

## Getting Help

Access help for any function using:

```r
# Function-specific help
?get_pv
?get_pmt
?add_new_entrants

# Package overview
help(package = "pentools")
```

## Testing

Run the test suite:

```r
devtools::test()
```
