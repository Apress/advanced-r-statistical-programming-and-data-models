## ----setup, include=TRUE, results='hide', message = FALSE, warning = FALSE, cache=FALSE----
library(checkpoint)
checkpoint("2018-09-28", R.version = "3.5.1",
  project = book_directory,
  checkpointLocation = checkpoint_directory,
  scanForPackages = FALSE,
  scan.rnw.with.knitr = TRUE, use.knitr = TRUE)

library(data.table)

options(
  width = 70,
  stringsAsFactors = FALSE,
  digits = 2)

## ------------------------------------------------------------------------

load("../ICPSR_04690/DS0001/04690-0001-Data.rda")
ls()

acl <- as.data.table(da04690.0001)
acl <- acl[, .(
  V2, V1801, V2101, V2064,
  V3007, V2623, V2636, V2640,
  V2000,
  V2200, V2201, V2202,
  V2613, V2614, V2616,
  V2618, V2681,
  V7007, V6623, V6636, V6640,
  V6201, V6202,
  V6613, V6614, V6616,
  V6618, V6681
)]

setnames(acl, names(acl), c(
  "ID", "Sex", "RaceEthnicity", "SESCategory",
  "Employment_W1", "BMI_W1", "Smoke_W1", "PhysActCat_W1",
  "AGE_W1",
  "SWL_W1", "InformalSI_W1", "FormalSI_W1",
  "SelfEsteem_W1", "Mastery_W1", "SelfEfficacy_W1",
  "CESD11_W1", "NChronic12_W1",
  "Employment_W2", "BMI_W2", "Smoke_W2", "PhysActCat_W2",
  "InformalSI_W2", "FormalSI_W2",
  "SelfEsteem_W2", "Mastery_W2", "SelfEfficacy_W2",
  "CESD11_W2", "NChronic12_W2"
           ))

acl[, ID := factor(ID)]
acl[, SESCategory := factor(SESCategory)]
acl[, SWL_W1 := SWL_W1 * -1]

saveRDS(acl, "advancedr_acl_data.RDS", compress = "xz")


