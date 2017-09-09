library("elastic")
install.packages("pastecs")
library(pastecs)
library(ggplot2)

sessionInfo()
setwd("C:/03-MyProjects")
options("scipen"=100, "digits"=4)

connect(es_base = "http://med3d.eastus.cloudapp.azure.com", es_port = 9220)
connection()

match <- '{"query": {"match" : {"bodypart03" : "psychisch krank"}}}'
ab <- Search(index="document04", type="document", body=match , asdf=TRUE, size=500)
ab.df <- data.frame(ab)

score5 <- ab$hits$hits$`_source`$score05
score4 <- ab$hits$hits$`_source`$score04
score3 <- ab$hits$hits$`_source`$score03
score2 <- ab$hits$hits$`_source`$score02
score1 <- ab$hits$hits$`_source`$score01

scored <- ab$hits$hits$`_score`

scores<-cbind(scored, score1, score2, score3, score4, score5)
stat.desc(scores)

ggplot(ab.df, aes(ab$hits$hits$`_source`$score05)) +
  geom_histogram(binwidth=1)

ggplot(ab.df, aes(ab$hits$hits$`_score`)) +
  geom_histogram(binwidth=0.1)

ggplot() + 
  geom_histogram(aes(x=score5),fill = "red", alpha = 0.2) + 
  geom_histogram(aes(x=score4), fill = "blue", alpha = 0.2) 

d = data.frame(x = c(score5, score4), 
               type=rep(c("A", "B"), c(length(score5), length(score4))))

ggplot(d) + 
  geom_density(aes(x=x, colour=type, fill=type), alpha=0.5)

boxplot(score4, score5)
plot (score4, score1)
abline(a=0, b=1)

t.test(score4,score5, mu=0, alt="two.sided", paired = T, conf.level = 0.99 )
wilcox.test(score4,score5, mu=0, alt="two.sided", paired = T, conf.level = 0.99)
z.test(score4,score5, mu=0, alt="two.sided", paired = T, conf.level = 0.99, sigma.x = NULL,
       sigma.y = NULL)


