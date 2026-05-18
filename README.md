# phr — Public Health Resources

**phr** now focuses on:

- `add_*` indicator-construction functions
- core helper utilities used by those functions:
  - `utils_errors`
  - `utils_validators`
  - `utils_language`

## Installation

```r
# install.packages("remotes")
remotes::install_github("SaeedR1987/phr_indicators")
library(phr)
```

## Scope

The package includes add-functions across:

- Food security and livelihoods
- Nutrition
- Mortality
- WASH
- Household and roster standardization
- Health access

See function-level documentation in R, for example:

```r
?add_fcs
?add_muac
?add_standardized_deaths
?phr_validate_columns
```

## License

MIT — see [LICENSE.md](LICENSE.md).
