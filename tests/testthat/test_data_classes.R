# tests/testthat/test_data_classes.R
library(testthat)
source("../../R/data_classes.R")

context("Data Classes")

test_that("Sheet1Data initialization works", {
  data_row <- list(Plot.code = "Plot1", SU = 1, Sample.date = as.Date("2023-01-15"), 
                   Detector = "DetA", X = 12.34, Y = 56.78, Region = "RegionX", 
                   Elevation = 100, Aspect = 180, Slope = 30, Cop.tot = 80, 
                   Litter.cov = 10, Bare.soil.cov = 5, Tree.cov = 50, Tree.h = 15, 
                   Shrub.cov = 20, Shrub.h = 2, Herb.cov = 10, Herb.h = 0.5, 
                   Brioph.cov = 5, notes = "Valid data")
  sheet1_obj <- Sheet1Data$new(data_row)
  
  expect_equal(sheet1_obj$Plot.code, "Plot1")
  expect_equal(sheet1_obj$SU, 1)
  expect_equal(sheet1_obj$Sample.date, as.Date("2023-01-15"))
  expect_equal(sheet1_obj$Detector, "DetA")
  expect_equal(sheet1_obj$X, 12.34)
  expect_equal(sheet1_obj$Y, 56.78)
  expect_equal(sheet1_obj$Region, "RegionX")
  expect_equal(sheet1_obj$Elevation, 100)
  expect_equal(sheet1_obj$Aspect, 180)
  expect_equal(sheet1_obj$Slope, 30)
  expect_equal(sheet1_obj$Cop.tot, 80)
  expect_equal(sheet1_obj$Litter.cov, 10)
  expect_equal(sheet1_obj$Bare.soil.cov, 5)
  expect_equal(sheet1_obj$Tree.cov, 50)
  expect_equal(sheet1_obj$Tree.h, 15)
  expect_equal(sheet1_obj$Shrub.cov, 20)
  expect_equal(sheet1_obj$Shrub.h, 2)
  expect_equal(sheet1_obj$Herb.cov, 10)
  expect_equal(sheet1_obj$Herb.h, 0.5)
  expect_equal(sheet1_obj$Brioph.cov, 5)
  expect_equal(sheet1_obj$notes, "Valid data")
})

test_that("Sheet2Data initialization works", {
  data_row <- list(Plot.code = "PlotA", Subplot = 2, Species = "Species Y", 
                   species_abb = "Sp.Y", cover = 75, Layer = "Shrub", Notes = "Example notes")
  sheet2_obj <- Sheet2Data$new(data_row)
  
  expect_equal(sheet2_obj$Plot.code, "PlotA")
  expect_equal(sheet2_obj$Subplot, 2)
  expect_equal(sheet2_obj$Species, "Species Y")
  expect_equal(sheet2_obj$species_abb, "Sp.Y")
  expect_equal(sheet2_obj$cover, 75)
  expect_equal(sheet2_obj$Layer, "Shrub")
  expect_equal(sheet2_obj$Notes, "Example notes")
})
