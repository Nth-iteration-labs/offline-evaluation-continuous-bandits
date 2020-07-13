library(ggplot2)

data <- read.table("results.csv", TRUE, sep=",")
head(data)

data <- data[data$type=="setreward",]
data$store <- as.factor(data$context.StoreID)

data <- data[data$store=="15337",]

dat2 <- aggregate(data, by=list(data$context.UserID), FUN=mean)

dat2$bb <- (1-dat2$split) * dat2$revenue
c <- qplot(dat2$split, dat2$bb, xlab="Split of discount offered to customer", ylab="Profit for rebate company in euros") + stat_smooth() + geom_vline(xintercept = 0.5, linetype = "dashed", color = "red")
ggsave("company_results.eps", c, device="eps")
print(c)
