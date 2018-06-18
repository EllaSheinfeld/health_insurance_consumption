library(ggplot2)

plotDecile <- function(df, decileNum){
  
  decileDf <- df[df$decile == decileNum,]
  
  ggplot(decileDf, aes(x=exptot)) + 
    geom_histogram(binwidth = .5,color="black", fill="white") + 
    labs(title = paste("Decile",decileNum,sep = " ")) + 
    geom_vline(aes(xintercept=mean(decileDf$exptot, ne.rm=TRUE)), color="red",linetype="dashed") + 
    geom_vline(aes(xintercept=median(decileDf$exptot)), color="blue",linetype="dashed")
}

plotDecileIncom <- function(df, decileNum){
  
  decileDf <- df[df$decile == decileNum,]
  
  ggplot(decileDf, aes(x=incoment)) + 
    geom_histogram(binwidth = .5,color="black", fill="white") + 
    labs(title = paste("Decile",decileNum,sep = " ")) + 
    geom_vline(aes(xintercept=mean(decileDf$incoment, ne.rm=TRUE)), color="red",linetype="dashed") + 
    geom_vline(aes(xintercept=median(decileDf$incoment)), color="blue",linetype="dashed")
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

g <- ggplot(df) + 
  geom_histogram(aes(x=df$decile,y=..density..), bins = 30) + 
  stat_function(fun=dnorm, 
                args=list(mean=mean(df[df$class == "Lower",]$decile), sd=sd(df[df$class == "Lower",]$decile)), 
                aes(x=df$decile, colour="\rLower"), size=2) +
  stat_function(fun=dnorm, 
                args=list(mean=mean(df[df$class == "Lower Middle",]$decile), sd=sd(df[df$class == "Lower Middle",]$decile)), 
                aes(x=df$decile, colour="\rLower Middle"),  size=2) +
  stat_function(fun=dnorm, 
                args=list(mean=mean(df[df$class == "Upper Middle",]$decile), sd=sd(df[df$class == "Upper Middle",]$decile)), 
                aes(x=df$decile, colour="\rUpper Middle"), size=2) +
  stat_function(fun=dnorm, 
                args=list(mean=mean(df[df$class == "Upper",]$decile), sd=sd(df[df$class == "Upper",]$decile)), 
                aes(x=df$decile, colour="Upper"), size=2) + 
  scale_colour_manual("Classes", values =  gg_color_hue(4)) + 
  labs(x="Decile", y="Density") + 
  scale_x_continuous(breaks = c(1:10))


g2 <- ggplot(df) + geom_bar(aes(decile, ..count..,fill=Classes)) + labs(x="Decile", y="Count")

g3 <- ggplot(df,aes(df$incoment,fill=df$class))+geom_density(alpha=0.2) + xlim(0,50000) + labs(x="Income in NIS", y="Distribution") + scale_fill_manual(name="Classes",values = gg_color_hue(4))

g4 <- ggplot(df[df$Classes == "Upper Middle" | df$Classes == "Upper",]) + geom_bar(aes(ageGroups, ..count..,fill=Classes)) + labs(x="Age Groups", y="Number of Households")


# Age group graphs:

g5 <- ggplot(df) + geom_bar(aes(ageGroups, healthinsur,fill=Classes), stat = "summary", fun.y = "mean",position = "dodge") + labs(x="Age Group", y="Health Insurance Expenditure")
g6 <- ggplot(df) + geom_bar(aes(ageGroups, privateinsur,fill=Classes), stat = "summary", fun.y = "mean",position = "dodge") + labs(x="Age Group", y="Private Health Insurance Expenditure")
g7 <- ggplot(df) + geom_bar(aes(ageGroups, shs,fill=Classes), stat = "summary", fun.y = "mean",position = "dodge") + labs(x="Age Group", y="SHS Expenditure")


cols <- gg_color_hue(2)

aggprivateinsur = aggregate(df$privateinsur,by = list(decile = df$decile), FUN = mean)
aggshs = aggregate(df$shs,by = list(decile = df$decile), FUN = mean)
aggDecile <- aggprivateinsur
names(aggDecile) <- c("decile", "meanPrivateInsure")
aggDecile$meanShs <- aggshs$x

g8 <- ggplot(aggDecile) + 
  geom_line(aes(x=aggDecile$decile,y=aggDecile$meanPrivateInsure, color=cols[1]), size=2) + 
  geom_line(aes(x=aggDecile$decile,y=aggDecile$meanShs, color=cols[2]), size=2) +
  labs(x="Decile", y="Mean Expenditure") + 
  scale_colour_manual(name = "Means",values =  cols, labels = c("SHS Exp.", "Private Health Exp.")) + 
  scale_x_continuous(breaks = c(1:10))