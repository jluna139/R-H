data = CH16PR12
names(data) = c("y" , "i" , "j")
head(data)


y1 = data[data$i == 1 , 1]

y2 = data[data$i == 2 , 1]

y3 = data[data$i == 3 , 1]

y4 = data[data$i == 4 , 1]

y5 = data[data$i == 5 , 1]

# calculating the mean of each vari
y1mean = mean(y1)
y2mean = mean(y2)
y3mean = mean(y3)
y4mean = mean(y4)
y5mean = mean(y5)


level = data$i
level

fit = aov(data$y~as.factor(level), data=data )
r = 5
nT = length(data$y)
nT



mse = sum(fit$residuals^2)/(nT -r)
mse

#use the t critic variable for calculating plot
t_crit = qt(0.9, nT-r) 


s_y1 = sqrt(mse/length(y1)) 
ci_lower1 = y1mean - s_y1*t_crit 
ci_upper1 = y1mean + s_y2*t_crit 


s_y2 = sqrt(mse/length(y2)) 
ci_lower2 = y2mean - s_y2*t_crit 
ci_upper2 = y2mean + s_y2*t_crit 

s_y3 = sqrt(mse/length(y3)) 
ci_lower3 = y3mean - s_y3*t_crit 
ci_upper3 = y3mean + s_y3*t_crit 

s_y4 = sqrt(mse/length(y4)) 
ci_lower4 = y4mean - s_y4*t_crit 
ci_upper4 = y4mean + s_y4*t_crit 

s_y5 = sqrt(mse/length(y5)) 
ci_lower5 = y5mean - s_y5*t_crit 
ci_upper5 = y5mean + s_y5*t_crit 






d1_hat = y1mean-y2mean
d2_hat = y1mean -y3mean
d3_hat = y1mean - y4mean
d4_hat = y1mean - y5mean

d5_hat = y2mean - y3mean

d6_hat = y2mean - y4mean
d7_hat = y2mean - y5mean
d8_hat = y3mean - y4mean
d9_hat = y3mean - y5mean
d10_hat = y4mean - y5mean


sd_d1_hat = sqrt(mse*((1/length(y1))+(1/length(y2)))) 
sd_d2_hat = sqrt(mse*((1/length(y1))+(1/length(y3)))) 
sd_d3_hat = sqrt(mse*((1/length(y1))+(1/length(y4)))) 
sd_d4_hat = sqrt(mse*((1/length(y1))+(1/length(y5)))) 
sd_d5_hat = sqrt(mse*((1/length(y2))+(1/length(y3)))) 
sd_d6_hat = sqrt(mse*((1/length(y2))+(1/length(y4)))) 
sd_d7_hat = sqrt(mse*((1/length(y2))+(1/length(y5)))) 
sd_d8_hat = sqrt(mse*((1/length(y3))+(1/length(y4)))) 
sd_d9_hat = sqrt(mse*((1/length(y3))+(1/length(y5)))) 
sd_d10_hat = sqrt(mse*((1/length(y4))+(1/length(y5)))) 
q1 = T * sd_d1_hat
q2 = T * sd_d2_hat
q3 = T * sd_d3_hat
q4 = T * sd_d4_hat
q5 = T * sd_d5_hat
q6 = T * sd_d6_hat
q7 = T * sd_d7_hat
q8 = T * sd_d8_hat
q9 = T * sd_d9_hat
q10 = T * sd_d10_hat
ci_1 = c(d1_hat-q1, d1_hat+q1) 

ci_2 = c(d2_hat-q2, d2_hat+q2) 
ci_3 = c(d3_hat-q3, d3_hat+q3)
ci_4 = c(d4_hat-q4, d4_hat+q4)
ci_5 = c(d5_hat-q5, d5_hat+q5)
ci_6 = c(d6_hat-q6, d6_hat+q6)
ci_7 = c(d7_hat-q7, d7_hat+q7)
ci_8 = c(d8_hat-q8, d8_hat+q8)
ci_9 = c(d9_hat-q9, d9_hat+q9)
ci_10 = c(d10_hat-q10, d10_hat+q10)



ci_1
ci_2
ci_3
ci_4
ci_5
ci_6
ci_7
ci_8
ci_9
ci_10







y_total = c(y1mean, y2mean, y3mean, y4mean, y5mean)

ci_lower_total = c(ci_lower1, ci_lower2, ci_lower3, ci_lower4, ci_lower5)
ci_upper_total = c(ci_upper1, ci_upper2, ci_upper3, ci_upper4, ci_upper5)


df = data.frame(x=1:5, F=y_total, L=ci_lower_total, U=ci_upper_total) 

# import the library for use
library(ggplot2)
p = ggplot(df, aes(x=x, y=F))+ geom_point(size=5)+geom_errorbar(aes(ymax=U, ymin=L)) 
p + labs(x="factor levels", y="sample mean with CIs")




var = c(ci_lower1, ci_upper1)
var


d_hat  = y2mean - y1mean
s_d_hat = sqrt(mse*((1/length(y2))+(1/length(y1)))) 
ci_lower = d_hat - s_d_hat*t_crit 
ci_upper = d_hat + s_d_hat*t_crit 
c(ci_lower,ci_upper)


#creatign a Bonferenis plot

d1_hat = y1mean - y3mean
d2_hat = y3mean - y5mean
d3_hat = y1mean - y5mean
s_d1_hat = sqrt(mse*((1/length(y1))+(1/length(y3)))) 
s_d2_hat = sqrt(mse*((1/length(y3))+(1/length(y5)))) 

s_d3_hat = sqrt(mse*((1/length(y1))+(1/length(y5)))) 
B = qt(1-.1/(2*3), nT-r)
cil1 = c(d1_hat - B*s_d1_hat, d1_hat + B*s_d1_hat)
cil2 = c(d2_hat - B*s_d2_hat, d2_hat + B*s_d2_hat)
cil3 = c(d3_hat - B*s_d3_hat, d3_hat + B*s_d3_hat)


cil1
cil2
cil3


plot(c(d1_hat, d2_hat, d3_hat), type = "l", lwd =2, col = 'red', 
     xlab = "Factor level", ylab = 'Estimated factor level mean')
points(c(d1_hat, d2_hat , d3_hat), pch =19)







##############################################################3




h12 = y1mean - y2mean
h13 = y1mean - y3mean
h14 = y1mean - y4mean
h15 = y1mean - y5mean
h23 = y2mean - y3mean
h34 = y3mean - y4mean
h45 = y4mean - y5mean

sh12 = sqrt(mse*((1/length(y1))+(1/length(y2)))) 
sh13= sqrt(mse*((1/length(y1))+(1/length(y3)))) 
sh14 = sqrt(mse*((1/length(y1))+(1/length(y4)))) 
sh15 = sqrt(mse*((1/length(y1))+(1/length(y5)))) 
sh23 = sqrt(mse*((1/length(y2))+(1/length(y3)))) 
sh34 = sqrt(mse*((1/length(y3))+(1/length(y4)))) 
sh45 = sqrt(mse*((1/length(y4))+(1/length(y5)))) 
n=7
alpha = 0.05


















