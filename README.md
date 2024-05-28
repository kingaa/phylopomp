## **phylopomp**, an *R* package for POMP inference on genealogies

[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Development Release](https://img.shields.io/github/release/kingaa/phylopomp.svg)](https://github.com/kingaa/phylopomp/releases/latest)
[![](https://www.r-pkg.org/badges/version/phylopomp?color=blue)](https://cran.r-project.org/package=phylopomp)
[![R-CMD-check](https://github.com/kingaa/phylopomp/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/kingaa/phylopomp/actions/workflows/r-cmd-check.yml)
[![binary-build](https://github.com/kingaa/phylopomp/actions/workflows/binary-build.yml/badge.svg)](https://github.com/kingaa/phylopomp/actions/workflows/binary-build.yml)
[![test-coverage](https://github.com/kingaa/phylopomp/actions/workflows/test-coverage.yml/badge.svg)](https://github.com/kingaa/phylopomp/actions/workflows/test-coverage.yml)
[![codecov](https://codecov.io/gh/kingaa/phylopomp/branch/master/graph/badge.svg)](https://codecov.io/gh/kingaa/phylopomp)
[![manual](https://img.shields.io/badge/manual-HTML-brown)](https://kingaa.github.io/manuals/phylopomp/)
[![pdf-manual](https://img.shields.io/badge/manual-PDF-brown)](https://kingaa.github.io/manuals/phylopomp/pdf/)
[![doxygen](https://img.shields.io/badge/doxygen-HTML-brown)](https://kingaa.github.io/manuals/phylopomp/source/html/index.html)


### Publications

[“Exact Phylodynamic Likelihood via Structured Markov Genealogy Processes” (*arXiv:2405.17032*, 2024)](https://doi.org/10.48550/arxiv.2405.17032)  
[![](https://img.shields.io/badge/doi-10.48550/arxiv.2405.17032-yellow.svg)](https://doi.org/10.48550/arxiv.2405.17032)

[“Markov Genealogy Processes” (*Theoretical Population Biology*
**143**:77–91, 2022)](https://doi.org/10.1016/j.tpb.2021.11.003)  
[![](https://img.shields.io/badge/doi-10.1016/j.tpb.2021.11.003-yellow.svg)](https://doi.org/10.1016/j.tpb.2021.11.003)

### Related packages:

  - [**pomp**](https://github.com/kingaa/pomp/)
  - [**circumstance**](https://github.com/kingaa/circumstance/)

### Checklist for adding a new model

- [ ] create and edit `yaml/<model>.yml`
- [ ] in `yaml/`, run `R < template.R`
- [ ] `cp yaml/src/<model>.cc src`
- [ ] `cp yaml/R/<model>.R R`
- [ ] optionally edit `R/<model>.R` and `src/<model>.cc` for style and documentation
- [ ] edit `src/init.c`
- [ ] edit `R/geneal.R`, `R/simulate.R`, `R/yaml.R`
- [ ] add examples to `examples/<model.R>`
- [ ] add tests to `tests/`
