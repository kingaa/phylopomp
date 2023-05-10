## **phylopomp**, an *R* package for POMP inference on genealogies

[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Development Release](https://img.shields.io/github/release/kingaa/phylopomp.svg)](https://github.com/kingaa/phylopomp/releases/latest)
[![](https://www.r-pkg.org/badges/version/phylopomp?color=blue)](https://cran.r-project.org/package=phylopomp)
[![R-CMD-check](https://github.com/kingaa/phylopomp/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/kingaa/phylopomp/actions/workflows/r-cmd-check.yml)
[![binary-build](https://github.com/kingaa/phylopomp/actions/workflows/binary-build.yml/badge.svg)](https://github.com/kingaa/phylopomp/actions/workflows/binary-build.yml)
[![test-coverage](https://github.com/kingaa/phylopomp/actions/workflows/test-coverage.yml/badge.svg)](https://github.com/kingaa/phylopomp/actions/workflows/test-coverage.yml)
[![codecov](https://codecov.io/gh/kingaa/phylopomp/branch/master/graph/badge.svg)](https://codecov.io/gh/kingaa/phylopomp)
[![manual](https://img.shields.io/badge/manual-HTML-informational)](https://kingaa.github.io/phylopomp/manual/)
[![pdf-manual](https://img.shields.io/badge/manual-PDF-informational)](https://kingaa.github.io/phylopomp/manual/pdf/)

### Publications

[“Markov Genealogy Processes” (*Theoretical Population Biology*
**143**:77–91)](https://doi.org/10.1016/j.tpb.2021.11.003)  
[![](https://img.shields.io/badge/doi-10.1016/j.tpb.2021.11.003-yellow.svg)](https://doi.org/10.1016/j.tpb.2021.11.003)

### Related packages:

  - [**pomp**](https://kingaa.github.io/pomp/)
  - [**circumstance**](https://kingaa.github.io/circumstance/)

### Checklist for adding a new model

- [x] create and edit `src/<model>.cc`
- [x] edit `src/init.c`
- [x] edit `R/getinfo.R`
- [x] create and edit `R/<model>.R`
- [x] add examples to `examples/<model.R>`
- [x] add tests to `tests/`
