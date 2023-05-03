install.packages("Hmisc")
library(Hmisc)

COVID19_data <- read.csv("file:///Users/ihsa332019/works/RProjects/Covid19DataAnalysis/COVID19_line_list_data.csv")
describe(COVID19_data)

# clean up the death column
COVID19_data$death_dummy <- as.integer(COVID19_data$death != 0)

# calculate the death rate
sum(COVID19_data$death_dummy) / nrow(COVID19_data)

# claim of AGE: people who die are older
dead = subset(COVID19_data, death_dummy == 1)
alive = subset(COVID19_data, death_dummy == 0)
mean(dead$age, na.rm = TRUE)   # 50% of people die with COVID-19 below the age of 68.5
mean(alive$age, na.rm = TRUE)   # 50% of people alive with COVID-19 below the age of 48.1

t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.99)
# p-value is 2.2e-16 which means it is very close to zero.
# Therefore, this test is statistically significant.

# claim of GENDER: gender has no effect on COVID-19
men = subset(COVID19_data, gender == "male")
women = subset(COVID19_data, gender == "female")
mean(men$death_dummy, na.rm = TRUE)   # 8.5%!
mean(women$death_dummy, na.rm = TRUE)   # 3.7%

t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)
# p-value is 0.002105 (< 0.05) which means it is very close to zero, too.
# Therefore, this test is also statistically significant.
