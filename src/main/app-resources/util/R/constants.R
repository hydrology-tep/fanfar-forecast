#!/opt/anaconda/bin/Rscript --vanilla --slave --quiet

#
# Constants to share between different R files.
#

{
    # Constants application.xml (tag option)
    cModelConfigVariant1 <- "Niger-HYPE 1.23 + GFD 1.3 + ECOPER"
    cModelConfigVariant2 <- "WorldWide-HYPE 1.3.6 + HydroGFD 2.0 + ECOPER"
    cModelConfigVariant3 <- "WestAfrica-HYPE 0.5 + HydroGFD 2.0 + ECOPER"
    cModelConfigVariant4 <- "WestAfrica-HYPE 1.0 + HydroGFD 3.0 + ECOPER"
    cModelConfigVariant5 <- "Niger-HYPE 2.30 + HydroGFD 3.0 + ECOPER"
    cModelConfigVariant6 <- "WestAfrica-HYPE 1.2 + HydroGFD 3.0 + ECOPER"
    cModelConfigVariants <- c(cModelConfigVariant1,cModelConfigVariant2,cModelConfigVariant3,cModelConfigVariant4,cModelConfigVariant5,cModelConfigVariant6)

    # Global constants not part of application.xml that
    # may be used for comparsion in if-statements etc.
    cHydrologicalModelVariant1 <- "Niger-HYPE"
    cHydrologicalModelVariant2 <- "WorldWide-HYPE"
    cHydrologicalModelVariant3 <- "WestAfrica-HYPE"
    #Not used cHydrologicalModelVariants <- c(cHydrologicalModelVariant1,cHydrologicalModelVariant2,cHydrologicalModelVariant3)

    cMeteoHindcastVariant1 <- "GFD 1.3"
    cMeteoHindcastVariant2 <- "HydroGFD 2.0"
    cMeteoHindcastVariant3 <- "HydroGFD 3.0"
    cMeteoHindcastVariants <- c(cMeteoHindcastVariant1,cMeteoHindcastVariant2,cMeteoHindcastVariant3)

    cMeteoForecastVariant1 <- "ECOPER"
    #cMeteoForecastVariant2 <- "ODF"
    #Not used cMeteoForecastVariants <- c(cMeteoForecastVariant1,cMeteoForecastVariant2)

    cRunTypeVariantOperational <- "Operational"
    cRunTypeVariantReforecast  <- "Reforecast"
    cRunTypeVariantStatefile   <- "Statefile creation"
    #Not used cRunTypeVariants <- c(cRunTypeVariantOperational,cRunTypeVariantReforecast,cRunTypeVariantStatefile)
}
