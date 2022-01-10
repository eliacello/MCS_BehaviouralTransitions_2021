# Loading Libraries ---------------------------------
library(arrangements)
library(cluster)
library(colormap)
library(dendextend)
library(dendsort)
library(extrafont)
library(ez)
library(factoextra)
library(fmsb)
library(ggplot2)
library(ggsci)
library(hybridHclust)
library(plyr)
library(psych)
library(reshape2)
library(tidyr)
library(fpc)
library(dplyr)
library(magrittr)
library(rstatix)
library(tidyverse)
library(ggpubr)

setwd('/Users/Eliacello/Documents/Cam_CALM/MCS/')
df <- read.csv(paste0('./transitions/WholeSampleZscore_MCS5.csv'))


# Stats ---------------------------------
test <- select(df, group, emotion, conduct, hyperactivity, peer.problems, antisocial, MCSID)

test['group'] = as.factor(test$group)
test_long <- gather(test, factor, loading, -group, -MCSID)

ezANOVA(test_long, 
        dv=loading,
        between=group,
        within=factor,
        wid=MCSID)

# Defining a function to calculate Cohen's d
cohend <- function(group1, group2) {return (mean(group1) - mean(group2))/sd(c(group1, group2))}

# Contrasts
contrasts <- combinations(length(unlist(unique(test['group']))), 2)
results <- c()
for (i in seq(1, dim(contrasts)[1])){
    result <- c()
    for (factor in c('emotion', 'conduct', 'hyperactivity', 'peer.problems', 'antisocial')){
        # T-test
        factor_df <- test_long[test_long['factor'] == factor, ]
        group1 <- factor_df[factor_df['group'] == as.character(unique(test['group'])[contrasts[i, 1], ]), ]$loading
        group2 <- factor_df[factor_df['group'] == as.character(unique(test['group'])[contrasts[i, 2], ]), ]$loading
        t_tests <- t.test(group1, group2)
        d <- cohend(group1, group2) # Calculate the effect size
        p_value <- t_tests$p.value %>% p.adjust('bonferroni', 5*length(unlist(unique(test['group']))))
        result <- cbind(result, cbind(p_value, d))
        rownames(result) <- paste0(as.character(unique(test['group'])[contrasts[i, 1], ]), 
                                   ' vs ', as.character(unique(test['group'])[contrasts[i, 2], ]))
    }
    results <- rbind(results, result)
}
print(paste('Critical p-value: ', round(0.05/length(contrasts), 5)))
colnames(results) <- unlist(lapply(c('emotion', 'conduct', 'hyperactivity', 'peer.problems', 'antisocial'), function(factor){rbind(paste0(factor, '.pvalue'), paste0(factor, '.effect size'))}))
descriptive_stats1 <- rbind( results)
descriptive_stats1 <- round(data.frame(descriptive_stats1), 3)
write.csv(descriptive_stats1, paste0('./ttests_clusters_MCS7.csv'))