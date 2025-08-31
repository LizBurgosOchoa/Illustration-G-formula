# A step-by-step guide for causal mediation analysis with the G-computation (Tutorial Repository)

Cite as: <a href="https://doi.org/10.5281/zenodo.16995100"><img src="https://zenodo.org/badge/528349944.svg" alt="DOI"></a>

**Note:** This repository was created in **September 2022** and is periodically updated to ensure compatibility with the latest versions of R packages.

## What’s in this repository

This repository contains guidance for performing causal mediation analysis (CMA) with the mediational g-formula (g-computation) in R.
The workflow follows the approach described in: Burgos-Ochoa (2023) https://doi.org/10.1111/ppe.12954 

The files are:

 [CMA_gformula_tutorial.Rmd](https://github.com/LizBurgosOchoa/Illustration-G-formula/blob/main/CMA_gformula_tutorial.Rmd)
An R Markdown notebook with step-by-step guidance, explanations, and code. Use this if you want a structured tutorial that explains each step (natural course, counterfactuals, mediation scenario, TE/NDE/NIE, bootstrap).

[G-formula_BigData_Special_Issue.R](https://github.com/LizBurgosOchoa/Illustration-G-formula/blob/main/G-formula_BigData_Special_Issue.R)
A plain R script with the same code, but without explanatory text. Use this if you only want the implementation.

## Who it’s for

Researchers and practitioners with experience with data analsis in R and familiarity with regression models, who want to apply CMA using the g-computation approach to their own data.

## How to use

- Clone or download this repository.
- Open CMA_gformula_tutorial.Rmd in RStudio (or another R Markdown-capable editor).
- Run through the chunks step by step to reproduce the example and adapt it to your dataset.
- If you prefer a leaner version, use the R script G-formula_BigData_Special_Issue.R.
