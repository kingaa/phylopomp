## **phylopomp**

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/kingaa/phylopomp/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/kingaa/phylopomp/actions/workflows/r-cmd-check.yml)
[![binary-build](https://github.com/kingaa/phylopomp/actions/workflows/binary-build.yml/badge.svg)](https://github.com/kingaa/phylopomp/actions/workflows/binary-build.yml)
[![test-coverage](https://github.com/kingaa/phylopomp/actions/workflows/test-coverage.yml/badge.svg)](https://github.com/kingaa/phylopomp/actions/workflows/test-coverage.yml)

### an *R* package for POMP inference on genealogies

["Markov Genealogy Processes" (*Theoretical Population Biology* **143**:77--91)](https://doi.org/10.1016/j.tpb.2021.11.003)

Manual: https://kingaa.github.io/manuals/phylopomp/

Related packages:

- [**pomp**](https://kingaa.github.io/pomp/)

### new model checklist

- create and edit `src/<model>.cc`
- edit `src/init.c`
- edit `R/getinfo.R`
- create and edit `R/<model>.R`
- add examples to `examples/<model.R>`
- add tests to `tests/`
