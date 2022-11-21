library (dplyr)
library (readr)
library (tidyr)
library (ggplot2)
library (stringr)
library (tidytext)
library (textstem)
library (text2vec)
library (lexicon)
library (lme4)
library (caret)
library (sjstats)
library (multcomp)
library (nloptr)
library (lmerTest)
library (BBmisc)
library (quanteda)
library (lubridate)

################################################################################################################

load(file.choose())

################################################################################################################

nlopt <- function(par, fn, lower, upper, control) {
  .nloptr <<- res <- nloptr(par, fn, lb = lower, ub = upper, 
                            opts = list(algorithm = "NLOPT_LN_BOBYQA", print_level = 1,
                                        maxeval = 1000, xtol_abs = 1e-6, ftol_abs = 1e-6))
  list(par = res$solution,
       fval = res$objective,
       conv = if (res$status > 0) 0 else res$status,
       message = res$message
  )
}

################################################################################################################

NP_indi <- lmer (individualizing ~ timediff + (timediff | author), data = NP,
                 control = lmerControl (optimizer = "nloptwrap", calc.derivs = FALSE))

TD_indi <- lmer (individualizing ~ timediff + (timediff | author), data = TD,
                 control = lmerControl (optimizer = "nloptwrap", calc.derivs = FALSE))

CON_indi <- lmer (individualizing ~ timediff + (timediff | author), data = CON,
                  control = lmerControl (optimizer = "nloptwrap", calc.derivs = FALSE))

SFP_indi <- lmer (individualizing ~ timediff + (timediff | author), data = SFP,
                  control = lmerControl (optimizer = "nloptwrap", calc.derivs = FALSE))

LSC_indi <- lmer (individualizing ~ timediff + (timediff | author), data = LSC,
                  control = lmerControl (optimizer = "nloptwrap", calc.derivs = FALSE))

################################################################################################################

summary_NP_indi <- summary(NP_indi)
confint_NP_indi <- confint.merMod(NP_indi, method = "Wald")
summary_TD_indi <- summary(TD_indi)
confint_TD_indi <- confint.merMod(TD_indi, method = "Wald")
summary_CON_indi <- summary(CON_indi)
confint_CON_indi <- confint.merMod(CON_indi, method = "Wald")
summary_SFP_indi <- summary(SFP_indi)
confint_SFP_indi <- confint.merMod(SFP_indi, method = "Wald")
summary_LSC_indi <- summary(LSC_indi)
confint_LSC_indi <- confint.merMod(LSC_indi, method = "Wald")

################################################################################################################

NP_bindi <- lmer (binding ~ timediff + (timediff | author), data = NP,
                  control = lmerControl (optimizer = "nloptwrap", calc.derivs = FALSE))

TD_bindi <- lmer (binding ~ timediff + (timediff | author), data = TD,
                  control = lmerControl (optimizer = "nloptwrap", calc.derivs = FALSE))

CON_bindi <- lmer (binding ~ timediff + (timediff | author), data = CON,
                   control = lmerControl (optimizer = "nloptwrap", calc.derivs = FALSE))

SFP_bindi <- lmer (binding ~ timediff + (timediff | author), data = SFP,
                   control = lmerControl (optimizer = "nloptwrap", calc.derivs = FALSE))

LSC_bindi <- lmer (binding ~ timediff + (timediff | author), data = LSC,
                   control = lmerControl (optimizer = "nloptwrap", calc.derivs = FALSE))

################################################################################################################

summary_NP_bindi <- summary(NP_bindi)
confint_NP_bindi <- confint.merMod(NP_bindi, method = "Wald")
summary_TD_bindi <- summary(TD_bindi)
confint_TD_bindi <- confint.merMod(TD_bindi, method = "Wald")
summary_CON_bindi <- summary(CON_bindi)
confint_CON_bindi <- confint.merMod(CON_bindi, method = "Wald")
summary_SFP_bindi <- summary(SFP_bindi)
confint_SFP_bindi <- confint.merMod(SFP_bindi, method = "Wald")
summary_LSC_bindi <- summary(LSC_bindi)
confint_LSC_bindi <- confint.merMod(LSC_bindi, method = "Wald")

################################################################################################################

save(NP_indi, TD_indi, CON_indi, SFP_indi, LSC_indi, 
     NP_bindi, TD_bindi, CON_bindi, SFP_bindi, LSC_bindi,
     file = "2018_11_21_Hypothese2_ganzeModelle.RData")

save(summary_NP_indi, confint_NP_indi, summary_TD_indi, confint_TD_indi, summary_CON_indi, 
     confint_CON_indi, summary_SFP_indi, confint_SFP_indi, summary_LSC_indi, confint_LSC_indi, 
     summary_NP_bindi, confint_NP_bindi, summary_TD_bindi, confint_TD_bindi, summary_CON_bindi,  
     confint_CON_bindi, summary_SFP_bindi, confint_SFP_bindi, summary_LSC_bindi, confint_LSC_bindi,
     file = "2018_11_21_Hypothese2_Ergebnisse.RData")
