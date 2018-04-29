library(car)
library(dplyr)
library(ggplot2)
library(GGally)

data("Salaries")
# 1:
# Observations: 397 - Variables: 6
dim(Salaries)
head(Salaries)
str(Salaries)

# 2:
# Nr. of professors with more than 40 yrs of experience: 21
old = Salaries[, "yrs.service"] > 40
sum(old)

# 3:
# Nr. of professors with salary higher than 150000: 54
rich = Salaries[, "salary"] > 150000
sum(rich)

# 4:
# Mean salary of professors with more than 20 yrs of experience
# $ 122103.90
mean(Salaries[Salaries$yrs.service > 20, "salary"])

# 5:
# TODO: find out more about the dataset
summary(Salaries)

# 6:
counts = Salaries %>%
  count(sex, rank) %>%
  mutate(prop = n / sum(n))

print(counts)

# 7:
ggplot(Salaries, aes(x = yrs.service)) +
  geom_histogram(aes(y = ..density..), binwidth = 5) +
  geom_density(color="red") +
  theme_bw() +
  ggtitle("Distribution of years of service")

# 8:
ggplot(Salaries, aes(x = 1, y = salary)) +
  geom_boxplot() +
  theme_bw() +
  ggtitle("Boxplot salary")

# 9:
ggplot(Salaries, aes(x = rank, y = salary, group = rank)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw() +
  ggtitle("Boxplot salaries by rank")

# 10:
ggplot(Salaries, aes(x = yrs.since.phd, y = salary)) +
  geom_point() +
  theme_bw() +
  ggtitle("Salary vs. years since Phd")

# 11:
# Correlation between salary and years since phd: 0.41
cor(Salaries$salary, Salaries$yrs.since.phd)
cor(Salaries$salary, Salaries$yrs.service)

model = lm("salary ~ yrs.since.phd", data = Salaries)
summary(model)

# 12:
ggpairs(Salaries) +
  theme_bw()

# 13:
Salaries %>%
  group_by(rank, sex) %>%
  summarise(mean.salary = mean(salary))

# 14:
ggplot(Salaries, aes(x = sex, y = salary, group = sex)) +
  geom_boxplot() +
  theme_bw() +
  coord_flip() +
  ggtitle("Distribution of salaries conditioned on sex")

# 15:
cont = table(Salaries$sex, Salaries$discipline)
chisq.test(cont)
