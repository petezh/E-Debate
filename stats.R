library(ggplot2)

# read act and income data
acts19 <- as.numeric(scan("datasets/acts19.txt", what="", sep="\n"))
acts20 <- as.numeric(scan("datasets/acts20.txt", what="", sep="\n"))
income19 <- as.numeric(scan("datasets/incomes19.txt", what="", sep="\n"))
income20 <- as.numeric(scan("datasets/incomes20.txt", what="", sep="\n"))

# test for normality
ggplot(data.frame(act=acts19), aes(x=act)) + geom_histogram()
ggplot(data.frame(act=acts20), aes(x=act)) + geom_histogram()

# significance test for act
se1 = sd(acts19)/sqrt(length(acts19))
se2 = sd(acts20)/sqrt(length(acts20))
se_pooled = sqrt(se1^2 + se2^2)
1-pnorm((mean(acts20)-mean(acts19))/se_pooled)

# test for normality
ggplot(data.frame(income=income19), aes(x=income)) + geom_histogram()
ggplot(data.frame(income=income20), aes(x=income)) + geom_histogram()

# significance test for income
se1 = sd(income19)/sqrt(length(income19))
se2 = sd(income20)/sqrt(length(income20))
se_pooled = sqrt(se1^2 + se2^2)
1-pnorm((mean(income20)-mean(income19))/se_pooled)
