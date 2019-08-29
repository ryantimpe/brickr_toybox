LEGO Mosaics in R
================

# brickr Toy Box <img src='man/figures/logo.png' align="right" height="138" />

## Overview

[**brickr**](https://github.com/ryantimpe/brickr) is a package for
creating LEGO-esque 2D and 3D models using the R
[tidyverse](https://www.tidyverse.org/) and [Tyler
Morgan-Wall](https://twitter.com/tylermorganwall)’s
[rayshader](https://github.com/tylermorganwall/rayshader) package.

This repository contains templates and examples to help user get started
using the package\!

## brickr

The brickr package has three key uses:

  - Converting image files in to 2D and 3D LEGO mosaics
  - Building 3D LEGO models from simple data frames
  - Brick bar charts in **ggplot**

### Installation

``` r
# To install the latest version from Github:
# install.packages("devtools")
devtools::install_github("ryantimpe/brickr")

#For 3D features, rayshader is also required.
install.packages("rayshader")

#For the start kit, readxl is required.
install.packages("readxl")
```

## Starter Kit

<img src='man/figures/StarterKit.JPG' align="center" height="400" />

The Excel file “brickr\_StartKit.xlsx” provides templates, as well as a
How-To manual and examples, for laying out models in Excel and then
rendering them as 3D models in R using brickr.

  - Lay out a model and the color key in Excel using one of the template
    or examples provided in the file.
  - Import the Excel sheet into R as a data frame using [Jenny
    Bryan](https://twitter.com/JennyBryan)’s
    [readxl](https://readxl.tidyverse.org/) package.
  - Convert this data frame into a brickr object using
    `bricks_from_excel()`.

<!-- end list -->

``` r
penguin <- readxl::read_xlsx("brickr_StarterKit.xlsx", sheet = "Set_Penguin")

penguin %>% 
  bricks_from_excel() %>% 
  build_bricks(theta = 155, phi = 10)

rayshader::render_snapshot()
```

![](README_files/figure-gfm/starter_kit-1.png)<!-- -->

## Contribute

Please feel free to contribute your own examples into any of the
“Examples\_” folders by submitting a pull request\!

## Other Examples

More examples using `bricks_from_table()` and `bricks_from_coords()` can
be found at the links below.

  - [**Get
    started**](https://gist.github.com/ryantimpe/a784beaa4f798f57010369329d46ce71)
    with the framework for building a brick from scratch.
  - [**Build an
    owl**](https://gist.github.com/ryantimpe/ceab2ed6b8a4737077280fc9b0d1c886)
    with `bricks_from_table()` by manually placing each brick.
  - Generate a punny [**random forest
    model**](https://gist.github.com/ryantimpe/a7363a5e99dceabada150a43925beec7)
    using `bricks_from_coords()` and {purrr}.
