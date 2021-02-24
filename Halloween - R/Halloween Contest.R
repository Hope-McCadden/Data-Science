# Loading libraries

library("dplyr")
library("rcompanion")
library("car")

# --------------------------------------------------------------------------------------------------------------------------------------------

# What information can you provide about these additional genres?
# What factors impact Review Rating? 
# What about Movie Rating

# Answers:
# Horror movies with a movie rating of PG-13 had the highest reviews than any other rating. 
# PG-13 movies ratings is 25% higher than movies rated as 'R'. Possibly due to the fact that PG-13 is a family film.

# Movies that were released in November have a significant rating review than movies that were release in September or October. 
# It seem that scary movie loves really get into horror films in November than any of the 3 months listed above.

# Movies that was released with English language has a much greater count than any country listed. 
# Japanese language has a significantly higher review rating than English language. 

# According to the data, horror movies reach its peak in 2014, releasing a total of 272 international movies.
# Between 2013 to 2015, horror movies released in 2014 had a 13% increase in the review rating than in 2013 and 2015.
# After 2014 it appear that the desire to release horror movies had declined greatly, but not the love of seeing a horror movie. 
# A 197 horror movies was released in 2017, but the review rating for 2017 increased by 37% from 2014. 
# Seam like the content of the movies may have greatly improved.

# Other the horror genre, there are 15 additional genre. Taking the top 3 genres (Thriller, Sci-Fi, and Mystery) that include horror, 
# there is a slight difference (.475) in the review rating for horror movies that includes mystery genre over horror movies that has 
# Thriller genre. And a .407 difference in the review rating for horror and mystery genre over horror and Sci-Fi.


# -------------------------------------------------------------------------------------------------------------------------------------


# Data Wrangling 

# Filter the Data and Remove Missing Values
title <- na.omit(anovaMovies3a %>% filter(Movie_Rating %in% c("R", "TV-MA", "UNRATED")))
title <- na.omit(anovaMovies3a %>% filter(Movie_Rating %in% c("PG-13", "TV-14", "NOT RATED")))

# Setting Review Rating dtype to numeric
title$Review_Rating <- as.numeric(title$Review_Rating)

# ---------------------------------------------------------------------------------------------------------------------------------------------

# Test Assumptions - Normality
# The Review_Rating is well normally distributed, no need to square the variable
plotNormalHistogram(title$Review_Rating)

# ---------------------------------------------------------------------------------------------------------------------------------------------

# Movie Rating

# Testing Homogeneity of Variance - Bartlette Test
# The p-value is 0.059; the assumption of homogeneity of variance has been violated
bartlett.test(Review_Rating ~ Movie_Rating, data=title)

# the aov test is significant; the p-value is > .05
appsANOVA <- aov(title$Review_Rating ~ title$Movie_Rating)
summary(appsANOVA)

# ANOVAs unequal variance
# Deviance is significant, the p-value is 0.0002855
ANOVA <- lm(Review_Rating ~ Movie_Rating, data=title)
Anova(ANOVA, Type="II", white.adjust=TRUE)

# ---------------------------------------------------------------------------------------------------------------------------------------------

# Post Hocs - Pairwise comparisons
# There is a significant difference between movie rating 'R' and 'TV-MA'.
pairwise.t.test(title$Review_Rating, title$Movie_Rating, p.adjust="bonferroni", pool.sd = FALSE)

# Determine Means
titleMeans <- title %>% group_by(Movie_Rating) %>% summarize(Mean = mean(Review_Rating))
summary(titleMeans)



# ---------------------------------------------------------------------------------------------------------------------------------------------

# Data Wrangling 

# Filter the Data and Remove Missing Values
month_title <- na.omit(anovaMovies3a %>% filter(Month %in% c("Sep", "Oct", "Nov")))

# Test Assumptions - Normality
# The Review_Rating is well normally distributed, no need to square the variable
plotNormalHistogram(title$Review_Rating)

# -----------------------------------------------------------------------------------------------------------------

# Month

# Testing Homogeneity of Variance - Bartlette Test
# The p-value is 0.7231; the assumption of homogeneity of variance has not been violated
bartlett.test(Review_Rating ~ Month, data=title)


# the aov test is significant; the p-value is > .05
appsANOVA <- aov(title$Review_Rating ~ title$Month)
summary(appsANOVA)


# Post Hoc - not significant
pairwise.t.test(title$Review_Rating, title$Month, p.adjust="bonferroni")


# Determine Means
month_titleMeans <- title %>% group_by(Month) %>% summarize(Mean = mean(Review_Rating))
summary(titleMeans)


# ---------------------------------------------------------------------------------------------------------------------------------------------

# Language
# Data Wrangling 

# Filter the Data and Remove Missing Values
title <- na.omit(anovaMovies3a %>% filter(Language %in% c("English", "Japanese", "Spanish")))

# -----------------------------------------------------------------------------------------------------------------



# Testing Homogeneity of Variance - Bartlette Test
# The p-value is 0.679; the assumption of homogeneity of variance has not been violated
bartlett.test(Review_Rating ~ Language, data=title)


# the aov test is significant; the p-value is > .05
appsANOVA <- aov(title$Review_Rating ~ title$Language)
summary(appsANOVA)


# Post Hoc - not significant
pairwise.t.test(title$Review_Rating, title$Language, p.adjust="bonferroni")


# Determine Means
Language_titleMeans <- title %>% group_by(Language) %>% summarize(Mean = mean(Review_Rating))
summary(Language_titleMeans)


# -----------------------------------------------------------------------------------------------------------------------------------

# Year

# Data Wrangling 

# Filter the Data and Remove Missing Values
title <- na.omit(anovaMovies3a %>% filter(Year %in% c("2014", "2015", "2016", "2017")))

# -----------------------------------------------------------------------------------------------------------------



# Testing Homogeneity of Variance - Bartlette Test
# The p-value is 0.6261; the assumption of homogeneity of variance has not been violated
bartlett.test(Review_Rating ~ Year, data=title)


# the aov test is significant; the p-value is > .05
appsANOVA <- aov(title$Review_Rating ~ title$Year)
summary(appsANOVA)


# Post Hoc - not significant
pairwise.t.test(title$Review_Rating, title$Year, p.adjust="bonferroni")


# Determine Means
Year_titleMeansA <- title %>% group_by(Year) %>% summarize(Mean = mean(Review_Rating))
summary(Year_titleMeansA)

# -------------------------------------------------------------------------------------------------------------------------------

# Data Wrangling 

# Filter the Data and Remove Missing Values
title <- na.omit(anovaMovies3a %>% filter(Genre.Thriller %in% c("NO GENRE", "Thriller", " Thriller")))

# -----------------------------------------------------------------------------------------------------------------

# Genre-Thriller

# Testing Homogeneity of Variance - Bartlette Test
# The p-value is 0.4019; the assumption of homogeneity of variance has not been violated
bartlett.test(Review_Rating ~ Genre.Thriller, data=title)


# the aov test is not significant; the p-value is < .05
appsANOVA <- aov(title$Review_Rating ~ title$Genre.Thriller)
summary(appsANOVA)

# the aov test is not significant; the p-value is < .05
ANOVA <- lm(Review_Rating ~ Genre.Thriller, data=title)
Anova(ANOVA, Type="II", white.adjust=TRUE)

# Post Hoc - not significant
pairwise.t.test(title$Review_Rating, title$Genre.Thriller, p.adjust="bonferroni", pool.sd = FALSE)


# Determine Means
Thriller_titleMeansA <- title %>% group_by(Genre.Thriller) %>% summarize(Mean = mean(Review_Rating))
summary(Thriller_titleMeansA)

# --------------------------------------------------------------------------------------------------------------------------------

# Genre-Sci-Fi

# Data Wrangling 

# Filter the Data and Remove Missing Values
title <- na.omit(anovaMovies3a %>% filter(Genre.Sci.Fi %in% c("NO GENRE", " Sci-Fi")))


# Testing Homogeneity of Variance - Bartlette Test
# The p-value is 0.02173; the assumption of homogeneity of variance has been violated
bartlett.test(Review_Rating ~ Genre.Sci.Fi, data=title)


# the aov test is not significant; the p-value is > .05
ANOVA <- lm(Review_Rating ~ Genre.Sci.Fi, data=title)
Anova(ANOVA, Type="II", white.adjust=TRUE)

# Post Hoc 
pairwise.t.test(title$Review_Rating, title$Genre.Sci.Fi, p.adjust="bonferroni", pool.sd = FALSE)


# Determine Means
Sci_titleMeans <- title %>% group_by(Genre.Sci.Fi) %>% summarize(Mean = mean(Review_Rating))
summary(Sci_titleMeans)



# -----------------------------------------------------------------------------------------------------------------------------------

# Genre-Mystery

# Filter the Data and Remove Missing Values
title <- na.omit(anovaMovies3a %>% filter(Genre.Mystery %in% c("NO GENRE", " Mystery")))

# Testing Homogeneity of Variance - Bartlette Test
# The p-value is 0.1396; the assumption of homogeneity of variance has not been violated
bartlett.test(Review_Rating ~ Genre.Mystery, data=title)


# the aov test is significant; the p-value is < .05
appsANOVA <- aov(title$Review_Rating ~ title$Genre.Mystery)
summary(appsANOVA)

# the aov test is significant; the p-value is < .05
ANOVA <- lm(Review_Rating ~ Genre.Mystery, data=title)
Anova(ANOVA, Type="II", white.adjust=TRUE)

# Post Hoc -  significant
pairwise.t.test(title$Review_Rating, title$Genre.Mystery, p.adjust="bonferroni", pool.sd = FALSE)


# Determine Means
Mystery_titleMeans <- title %>% group_by(Genre.Mystery) %>% summarize(Mean = mean(Review_Rating))
summary(Mystery_titleMeans)


# ------------------------------------------------------------------------------------------------------------------------------------

