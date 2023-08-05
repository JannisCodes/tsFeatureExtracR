# tsFeatureExtracR

[![CRAN status](https://www.r-pkg.org/badges/version/tsFeatureExtracR)](https://CRAN.R-project.org/package=tsFeatureExtracR)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

`tsFeatureExtracR` is a package for R that provides a set of functions for feature extraction from time series data. Additionally, it offers tools for analyzing and dealing with missing data in extracted time series features. These functionalities help to prepare datasets for machine learning or statistical modeling and gain a better understanding of the missing data distribution. 

## Installation

You can install the development version from [GitHub](https://github.com/JannisCodes/tsFeatureExtracR) with:

```r
# install.packages("devtools")
devtools::install_github("JannisCodes/tsFeatureExtracR")
```

## Usage

```r
library(tsFeatureExtracR)
```

Then use the functions provided by the package to extract features, analyse missingness, and impute missing values.

## Dependencies

`tsFeatureExtracR` depends on R (>= 4.1.0) and uses the following packages:

- dplyr
- mice
- ggplot2
- ggthemes
- grid
- scales
- tibble
- nlme
- psych
- MASS
- mgcv

## Contributing

Contributions are welcome! For bug reports or suggestions, please open an issue.

## License

MIT © Jannis Kreienkamp

## Contact

For any other questions, feel free to reach out to Jannis Kreienkamp at jannis@data-delight.com.
