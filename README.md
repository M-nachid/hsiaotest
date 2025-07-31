# hsiaotest

**Hsiao's Homogeneity Tests for Panel Data**

The `hsiaotest` R package provides classical and robust implementations of Hsiao's (1986) tests for panel data heterogeneity. These tests evaluate whether intercepts and/or slopes differ across cross-sectional units (e.g., countries, firms) using nested linear models and F-tests.

## 📦 Installation

You can install the development version of the package directly from GitHub:

```r
# Install devtools if you haven't
install.packages("devtools")

# Install hsiaotest from GitHub
devtools::install_github("M-nachid/hsiaotest")

# Load the package
library(hsiaotest)
