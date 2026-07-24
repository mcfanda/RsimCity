# Rsimcity

Rsimcity is an R package for designing, running, and summarising simulation
experiments. Its R6 workflow keeps fixed parameters, experimental conditions,
simulation steps, and result aggregation together in one reusable object. It
also includes tools for generating samples with specified correlations or known
population R-squared and eta-squared values.

> **Looking for function help?** Browse the complete
> [online reference](https://mcfanda.github.io/RsimCity/reference/) for every
> exported function and class.

## Install

Install the development version directly from GitHub with
[pak](https://pak.r-lib.org/):

```r
install.packages("pak")
pak::pak("mcfanda/RsimCity")
```

Alternatively, use `remotes`:

```r
install.packages("remotes")
remotes::install_github("mcfanda/RsimCity")
```

## Learn more

- [Start with the introduction](https://mcfanda.github.io/RsimCity/articles/intro.html)
- [Browse the full help reference](https://mcfanda.github.io/RsimCity/reference/)
