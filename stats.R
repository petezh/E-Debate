library(ggplot2)

# read act and income data
acts19 <- as.numeric(scan("datasets/acts19.txt", what="", sep="\n"))
acts20 <- as.numeric(scan("datasets/acts20.txt", what="", sep="\n"))
income19 <- as.numeric(scan("datasets/incomes19.txt", what="", sep="\n"))
income20 <- as.numeric(scan("datasets/incomes20.txt", what="", sep="\n"))

# test for normality
shapiro.test(acts19)
ggplot(data.frame(act=acts19), aes(x=act)) + geom_histogram()
shapiro.test(acts20)
ggplot(data.frame(act=acts20), aes(x=act)) + geom_histogram()

# significance test for act
se1 = sd(data19)/sqrt(length(data19))
se2 = sd(data20)/sqrt(length(data20))
se_pooled = sqrt(se1^2 + se2^2)
1-pnorm(0.079/se_pooled)

# test for normality
shapiro.test(income19)
ggplot(data.frame(income=income19), aes(x=income)) + geom_histogram()
shapiro.test(income20)
ggplot(data.frame(income=income20), aes(x=income)) + geom_histogram()

# significance test for income
se1 = sd(data19)/sqrt(length(data19))
se2 = sd(data20)/sqrt(length(data20))
se_pooled = sqrt(se1^2 + se2^2)
1-pnorm(811/se_pooled)
