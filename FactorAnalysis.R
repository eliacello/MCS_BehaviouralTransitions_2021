# Loading (and install when first time) Libraries -------
install.packages("polycor")
install.packages("psych")
install.packages("psychTools")
install.packages("EFA.dimensions")

library(psych)
library(psychTools)
library(polycor)
library(EFA.dimensions)
library(reshape2)
library(ggplot2)

# Loading data ------------------------------------------
# Loading the data
setwd('/Users/Eliacello/Documents/Cam_CALM/MCS/')
df <- read.csv(paste0('./mcs3/df_MCS3.csv'), header=TRUE)
df <- read.csv(paste0('./MCS3_factorAnalysis.csv'))

# Removing other columns
id <- df[,c('MCSID')]
id <- df[,c('V1')]
df <- df[,!(names(df) %in% c('MCSID', 'CHCAGE00', 'CHCSEX00', 'X_merge'))]
df <- df[,!(names(df) %in% c('V1', 'V2', 'V3','V28'))]
colnames(df) <- seq(1, dim(df)[2])

# describe data: 
des = describe(df)
knitr::kable(des[,c("min", "max", "mean", "median", "skew", "kurtosis")], main = "Data Summary")

# Polychoric correlation ---------------------------------
poly_cor <- polychoric(df)

rho = poly_cor$rho
save(rho, file = "polychoric")
### Thresholds/Scaling results
poly_cor$tau



# Exploratory factor analysis on polychoric matrix -------
# How many factors to keep? Parallel analysis and scree plot methods
# scree plot: Sharp breaks in the plot suggest the appropriate number of components or factors to extract.
# parallel analysis: check where simulated data eigenvalues crossing with actual data eigenvalues
fa.parallel(rho, n.obs = nrow(df), fm="pa", fa="fa", main = "Scree Plot")
fa.parallel(df, fm="pa", fa="fa", main = "Scree Plot")

# Polychoric factor analysis
#  nfactor=3 to change accroding to results from scree plot and parallel analysis
poly_model = fa(rho, nfactor=5, cor="poly", fm="mle", rotate = "varimax")
poly_model = fa(df, nfactor=6, fm="mle", rotate = "varimax")
save(poly_model, file = "poly_model")
poly_model$loadings

load("poly_model")
# Cluster analysis plot
fa.diagram(poly_model)

# export factor scores (default: method="Thurstone" finds the regression based weights)
scores <- poly_model$scores 

# method="Anderson" finds weights such that the factor scores will be uncorrelated
scores_Anderson <- factor.scores(df,poly_model, method="Anderson")
Anderson_scores <- scores_Anderson$scores

# add ID to df with scores 
scores_Thurstone <- factor.scores(df,poly_model, method="Thurstone")
Thurstone_scores <- scores_Thurstone$scores
Thurstone_scores <- cbind(Thurstone_scores, 'id'=id)
write.csv(Thurstone_scores,'Thurstone_scores.csv')

# method="Anderson" finds weights such that the factor scores will be uncorrelated
scores_Anderson <- factor.scores(df,poly_model, method="Anderson")
Anderson_scores <- scores_Anderson$scores
Anderson_scores <- cbind(Anderson_scores, 'id'=id)
write.csv(Anderson_scores,'Anderson_scores.csv')


#compare the tenBerge factor score correlation to the factor correlations
cor(my.scores$scores,use="pairwise") - poly_model$Phi  #compare to the f5$Phi values


# Plot factor scores ------------------------------------------
loadings <- unclass(poly_model$loadings)
item <- c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15',
          '16','17','18','19','20','21','22','23','24') #replace with item questions
item = as.factor(item)

loadings <- cbind(loadings, 'item' = item)
colnames(loadings) <- c("inattention", "hyperactivity", 
                        "conduct", "emotion","antisocial",  #replace with your own factor labels
                        "item")

loadings_long <- melt(loadings, id="item", 
                   measure=c("inattention", "hyperactivity", 
                             "conduct", "emotion","antisocial"), 
                   variable.name="factor", value.name="loading")

colnames(loadings_long) <- c("item", "factor", "loading")

loadings_long <- loadings_long[1:120,] # only keep relevant rows (here 5 factors * 24 items ->120)

#For each test, plot the loading as length and fill color of a bar
# note that the length will be the absolute value of the loading but the 
# fill color will be the signed value, more on this below
ggplot(loadings_long, aes(item, abs(loading), fill=loading)) + 
    facet_wrap(~ factor, nrow=1) + #place the factors in separate facets
    geom_bar(stat="identity") + #make the bars
    coord_flip() + #flip the axes so the test names can be horizontal  
    #define the fill color gradient: blue=positive, red=negative
    scale_fill_gradient2(name = "Loading", 
                         high = "blue", mid = "white", low = "red", 
                         midpoint=0, guide=F) +
    ylab("Loading Strength") + #improve y-axis label
    theme_bw(base_size=10) #use a black-and0white theme with set font size


