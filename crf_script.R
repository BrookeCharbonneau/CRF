library(tidyverse)
library(apaTables)
library(MBESS)
library(phia)

#load data
crf.data <- read_csv("crfData.csv")

# Using factors - CRUCIAL STEP TO GET RIGHT NUMBERS
crf.data$anxiety <- as.factor(crf.data$anxiety) 
crf.data$preparation <- as.factor(crf.data$preparation)

levels(crf.data$anxiety) <- list("Low Anxiety"=1, "High Anxiety"=2) 

levels(crf.data$preparation) <- list("Low Preparation"=1, "Medium Preparation"=2, "High Preparation"=3)


# Setting contrasts
options(contrasts = c("contr.sum", "contr.poly"))

#Run the analysis
crf.lm <- lm(mark ~ anxiety * preparation, data=crf.data)

apa.aov.table(crf.lm)
#marginal main effect of prep (b/c CI touches 0), main effect of anxiety, interaction significant

#get table of means
apa.2way.table(iv1=preparation, iv2=anxiety, dv=mark, data=crf.data, 
               show.marginal.means = TRUE, filename="Table2.doc")

#significant interation, do simple main effect
#hold anxiety constant and see the effect of preparation
testInteractions(crf.lm, fixed="anxiety", across="preparation",adjustment="none")
#simple main effect sig for low anxiety, non-sig high anxiety

#partial eta = SSeffect/SSeffect+SSerror
#low: 856.93/(856.93 + 1440) = 0.3730762
#high: 116.80/(116.80 + 1440) = 0.07502569

#CI for partial eta
#low
get.ci.partial.eta.squared(F.value=7.1411, df1=2, df2=24, conf.level = .90)
#high
get.ci.partial.eta.squared(F.value=0.9733, df1=2, df2=24, conf.level = .90)

#paired comparisons
testInteractions(crf.lm, fixed="anxiety", pairwise="preparation",adjustment="bonferroni")
#apa d table has stars for sig, but does not have bonferroni p value
#SO delete stars and use p values from bonferroni



