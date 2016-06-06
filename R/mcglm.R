#' @title Australian Health Survey
#' @name ahs
#'
#' @description The Australian health survey was used by Bonat and
#'     Jorgensen (2016) as an example of multivariate count regression
#'     model. The data consists of five count response variables
#'     concerning health system access measures and nine covariates
#'     concerning social conditions in Australian for 1987-88.
#'
#' \itemize{
#'
#' \item \code{sex} - Factor with levels \code{male} and \code{female}.
#'
#' \item \code{age} - Respondent's age in years divided by 100.
#'
#' \item \code{income} - Respondent's annual income in Australian
#'     dollars divided by 1000.
#'
#' \item \code{levyplus} - Coded factor. If respondent is covered by
#'     private health insurance fund for private patients in public
#'     hospital with doctor of choice (1) or otherwise (0).
#'
#' \item \code{freepoor} - Coded factor. If respondent is covered by
#'     government because low income, recent immigrant, unemployed (1)
#'     or otherwise (0).
#'
#' \item \code{freerepa} - Coded factor. If respondent is covered free
#'     by government because of old-age or disability pension, or
#'     because invalid veteran or family of deceased veteran (1) or
#'     otherwise (0).
#'
#' \item \code{illnes} - Number of illnesses in past 2 weeks, with 5 or
#'     illnesses coded as 5.
#'
#' \item \code{actdays} - Number of days of reduced activity in the past
#'     two weeks due to illness or injury.
#'
#' \item \code{hscore} - Respondent's general health questionnaire score
#'     using Goldberg's method. High score indicates poor health.
#'
#' \item \code{chcond} - Factor with three levels. If respondent has
#'     chronic condition(s) and is limited in activity (\code{limited}),
#'     or if the respondent has chronic condition(s) but is not limited
#'     in activity (\code{nonlimited}) or otherwise (\code{otherwise},
#'     reference level).
#'
#' \item \code{Ndoc} - Number of consultations with a doctor or
#'     specialist (response variable).
#'
#' \item \code{Nndoc} - Number of consultations with health
#'     professionals (response variable).
#'
#' \item \code{Nadm} - Number of admissions to a hospital, psychiatric
#'     hospital, nursing or convalescence home in the past 12 months
#'     (response variable).
#'
#' \item \code{Nhosp} - Number of nights in a hospital during the most
#'     recent admission.
#'
#' \item \code{Nmed} - Total number of prescribed and non prescribed
#'     medications used in the past two days.
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(ahs)
#'
#' @format a \code{data.frame} with 5190 records and 15 variables.
#'
#' @source Deb, P. and Trivedi, P. K. (1997) Demand for medical care by
#'     the elderly: A finite mixture approach. Journal of Applied
#'     Econometrics 12(3):313--336.
#'
#' @source Bonat, W. H. and Jorgensen, B. (2016) Multivariate
#'     covariance generalized linear models.
#'     Journal of Royal Statistical Society - Series C X(X):XX--XX.
#'
#' @examples
#'require(mcglm)
#' data(ahs, package="mcglm")
#' form1 <- Ndoc ~ income + age
#' form2 <- Nndoc ~ income + age
#' Z0 <- mc_id(ahs)
#' fit.ahs <- mcglm(linear_pred = c(form1, form2),
#'                  matrix_pred = list(Z0, Z0), link = c("log","log"),
#'                  variance = c("poisson_tweedie","poisson_tweedie"),
#'                  data = ahs)
#' summary(fit.ahs)
NULL

#' @title Hunting in Pico Basile, Bioko Island, Equatorial Guinea.
#' @name Hunting
#'
#' @description Case study analysed in Bonat et. al. (2016) concernings
#' on data of animals hunted in the village of Basile Fang,
#' Bioko Norte Province, Bioko Island, Equatorial Guinea.
#' Monthly number of blue duikers and other small animals
#' shot or snared was collected for a random sample of 52 commercial
#' hunters from August 2010 to September 2013.
#' For each animal caught, the species, sex, method of capture and
#' altitude were documented. The data set has 1216 observations.
#'
#' \itemize{
#'
#' \item \code{ALT} - Factor five levels indicating the Altitude where
#' the animal was caught.
#'
#' \item \code{SEX} - Factor two levels \code{Female} and \code{Male}.
#'
#' \item \code{METHOD} - Factor two levels \code{Escopeta} and
#' \code{Trampa}.
#'
#' \item \code{OT} - Monthly number of other small animals hunted.
#'
#' \item \code{BD} - Monthly number of blue duikers hunted.
#'
#' \item \code{OFFSET} - Monthly number of hunter days.
#'
#' \item \code{HUNTER} - Hunter index.
#'
#' \item \code{MONTH} - Month index.
#'
#' \item \code{MONTHCALENDAR} - Month using calendar numbers
#' (1-January, ..., 12-December).
#'
#' \item \code{YEAR} - Year calendar (2010--2013).
#'
#' \item \code{HUNTER.MONTH} - Index indicating observations taken at
#' the same HUNTER and MONTH.
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(Hunting)
#'
#' @format a \code{data.frame} with 1216 records and 11 variables.
#'
#' @source Bonat, et. al. (2016). Modelling the covariance structure in
#' marginal multivariate count models: Hunting in Bioko Island.
#' Environmetrics, submitted.
#'
#' @source Bonat, W. H. (2016). Multiple Response Variables Regression
#' Models in R: The mcglm Package.
#' Journal of Statistical Software, submitted.
#'
#' @examples
#' \donttest{
#' library(mcglm)
#' library(Matrix)
#' data(Hunting, package="mcglm")
#' formu <- OT ~ METHOD*ALT + SEX + ALT*poly(MONTH, 4)
#' Z0 <- mc_id(Hunting)
#' Z1 <- mc_mixed(~0 + HUNTER.MONTH, data = Hunting)
#' fit <- mcglm(linear_pred = c(formu), matrix_pred = list(c(Z0, Z1)),
#'             link = c("log"), variance = c("poisson_tweedie"),
#'             power_fixed = c(FALSE),
#'             control_algorithm = list(max_iter = 100),
#'             offset = list(log(Hunting$OFFSET)), data = Hunting)
#' summary(fit)
#' anova(fit)
#' }
NULL

#' @title Soil Chemistry Properties Data
#' @name soil
#'
#' @description Soil chemistry properties measured on a regular grid with
#' 10 x 25 points spaced by 5 meters.
#'
#' \itemize{
#'
#' \item \code{COORD.X} - X coordinate.
#'
#' \item \code{COORD.Y} - Y coordinate.
#'
#' \item \code{SAND} - Sand portion of the sample.
#'
#' \item \code{SILT} - Silt portion of the sample.
#'
#' \item \code{CLAY} - Clay portion of the sample.
#'
#' \item \code{PHWATER} - Soil pH at water.
#'
#' \item \code{CA} - Calcium content.
#'
#' \item \code{MG} - Magnesium content.
#'
#' \item \code{K} - Potassio content.
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(soil)
#'
#' @format a \code{data.frame} with 250 records and 9 variables.
#'
#' @source Bonat, W. H. (2016). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, submitted.
#'
#' @examples
#' \donttest{
#' data(soil, package="mcglm")
#' neigh <- tri2nb(soil[,1:2])
#' Z1 <- mc_car(neigh)
#' # Linear predictor
#' form.ca <- CA ~ COORD.X*COORD.Y + SAND + SILT + CLAY + PHWATER
#' fit.ca <- mcglm(linear_pred = c(form.ca), matrix_pred = list(Z1),
#'                link = "log", variance = "tweedie", covariance = "inverse",
#'                power_fixed = FALSE, data = soil,
#'                control_algorith = list(max_iter = 500, tunning = 0.1))
#' summary(fit.ca)
#' # Conditional hypothesis test
#' mc_conditional_test(fit.ca, parameters = c("power11","tau11","tau12"),
#'                    test = 2:3, fixed = 1)
#' # Spatial autocorrelation
#' mc_compute_rho(fit.ca)
#' }
#'

NULL

#' @title Respiratory Physiotherapy on Premature Newborns.
#' @name NewBorn
#'
#' @description The NewBorn dataset consists of a prospective study
#' to assess the effect of respiratory physiotherapy on the
#' cardiopulmonary function of ventilated preterm newborn infants with
#' birth weight lower than 1500 g. The data set was collected and
#' kindly made available by the nursing team of the Waldemar Monastier
#' hospital, Campo Largo, PR, Brazil. The NewBorn dataset was analysed
#' in Bonat and Jorgensen (2016) as an example of mixed outcomes
#' regression model.
#'
#' \itemize{
#'
#' \item \code{Sex} - Factor two levels \code{Female} and \code{Male}.
#'
#' \item \code{GA} - Gestational age (weeks).
#'
#' \item \code{BW} - Birth weight (mm).
#'
#' \item \code{APGAR1M} - APGAR index in the first minute of life.
#'
#' \item \code{APGAR5M} - APGAR index in the fifth minute of life.
#'
#' \item \code{PRE} - Factor, two levels (Premature: YES; NO).
#'
#' \item \code{HD} - Factor, two levels (Hansen's disease, YES; NO).
#'
#' \item \code{SUR} - Factor, two levels (Surfactant, YES; NO).
#'
#' \item \code{JAU} - Factor, two levels (Jaundice, YES; NO).
#'
#' \item \code{PNE} - Factor, two levels (Pneumonia, YES; NO).
#'
#' \item \code{PDA} - Factor, two levels (Persistence of ductus
#' arteriosus, YES; NO).
#'
#' \item \code{PPI} - Factor, two levels (Primary pulmonary infection,
#' YES; NO).
#'
#' \item \code{OTHERS} - Factor, two levels (Other diseases, YES; NO).
#'
#' \item \code{DAYS} - Age (days).
#'
#' \item \code{AUX} - Factor, two levels (Type of respiratory auxiliary,
#' HOOD; OTHERS).
#'
#' \item \code{RR} - Respiratory rate (continuous).
#'
#' \item \code{HR} - Heart rate (continuous).
#'
#' \item \code{SPO2} - Oxygen saturation (bounded).
#'
#' \item \code{TREAT} - Factor, three levels (Respiratory physiotherapy,
#' Evaluation 1; Evaluation 2; Evaluation 3).
#'
#' \item \code{NBI} - Newborn index.
#'
#' \item \code{TIME} - Days of treatment.
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(NewBorn)
#'
#' @format a \code{data.frame} with 270 records and 21 variables.
#'
#' @source Bonat, W. H. and Jorgensen, B. (2016) Multivariate
#'     covariance generalized linear models.
#'     Journal of Royal Statistical Society - Series C X(X):XX--XX.
#'
#' @examples
#' library(mcglm)
#' library(Matrix)
#' data(NewBorn, package="mcglm")
#' formu <- SPO2 ~ Sex + APGAR1M + APGAR5M + PRE + HD + SUR
#' Z0 <- mc_id(NewBorn)
#' fit <- mcglm(linear_pred = c(formu), matrix_pred = list(Z0),
#'             link = c("logit"), variance = c("binomialP"),
#'             power_fixed = c(TRUE),
#'             data = NewBorn,
#'             control_algorithm = list(verbose = FALSE, tunning = 0.5))
#' summary(fit)
NULL

#' @title Soybeans
#' @name soya
#'
#' @description Experiment carried out in a vegetation house with
#' soybeans. The experiment has two plants by plot with three levels of
#' the factor amount of water in the soil (\code{water})
#' and five levels of potassium fertilization (\code{pot}).
#' The plots were arranged in five blocks (\code{block}).
#' Three response variables are of the interest, namely, grain yield,
#' number of seeds and number of viable peas per plant.
#' The data set has 75 observations of 7 variables.
#'
#' \itemize{
#'
#' \item \code{pot} - Factor five levels of potassium fertilization.
#'
#' \item \code{water} - Factor three levels of amount of water in the soil.
#'
#' \item \code{block} - Factor five levels.
#'
#' \item \code{grain} - Continuous - Grain yield per plant.
#'
#' \item \code{seeds} - Count - Number of seeds per plant.
#'
#' \item \code{viablepeas} - Binomial - Number of viable peas per plant.
#'
#' \item \code{totalpeas} - Binomial - Total number of peas per plant.
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(soya)
#'
#' @format a \code{data.frame} with 75 records and 7 variables.
#'
#' @source Bonat, W. H. (2016). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, submitted.
#'
#' @examples
#' library(mcglm)
#' library(Matrix)
#' data(soya, package="mcglm")
#' formu <- grain ~ block + factor(water) * factor(pot)
#' Z0 <- mc_id(soya)
#' fit <- mcglm(linear_pred = c(formu), matrix_pred = list(Z0),
#'           data = soya)
#'           anova(fit)
NULL
