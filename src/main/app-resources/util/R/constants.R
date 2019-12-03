#!/opt/anaconda/bin/Rscript --vanilla --slave --quiet

#
# Constants to share between different R files.
#

# Constants application.xml (tag option)
cModelConfigVariant1 <- "World Wide-HYPE 1.3.6 + HydroGFD 2.0 + ECOPER"
cModelConfigVariants <- c(cModelConfigVariant1)

# Global constants not part of application.xml that
# may be used for comparsion in if-statements etc.
cHydModelVariant1 <- "Niger-HYPE"
cHydModelVariant2 <- "WestAfrica-HYPE"
cHydModelVariants <- c(cHydModelVariant1,cHydModelVariant2)

cMetHCVariant1 <- "GFD 1.3"
cMetHCVariant2 <- "HydroGFD 2.0"
cMetHCVariants <- c(cMetHCVariant1,cMetHCVariant2)

cMetFCVariant1 <- "ECOPER"
cMetFCVariants <- c(cMetFCVariant1)

cRunTypeVariant1 <- "Operational" # Rename constants, instead of 1 2 3 use some of the value for easier understanding in if-statements etc.
cRunTypeVariant2 <- "Reforecast"
cRunTypeVariant3 <- "Statefile creation" # or Cold start
cRunTypeVariants <- c(cRunTypeVariant1,cRunTypeVariant2,cRunTypeVariant3)
