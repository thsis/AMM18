import os
import pandas as pd
import numpy as np
import scipy
from matplotlib import pyplot as plt
import seaborn as sns

path_to_data = os.path.join("data", "salaries.csv")
salaries = pd.read_csv(path_to_data, sep=",", index_col=0)

# Question 1: How many variables and observations are in the dataset?
# A: 6 variables and 397 observations
print(salaries.shape)

# Question 2: How many professors have more than 40 years of service? (hint:
# you can sum() a logical vector)
# A: 21
old = sum(salaries["yrs.service"] > 40)
print("Professors with more than 40 years of service:", old)
# Question 3:  How many have salary larger than $150000?
# A: 54
rich = sum(salaries["salary"] > 150000)
print("Professors with salary higher than $150000:", rich)

# Question 4:  What is the mean salary for professors with > 20 years service?
# A: $122103.88
mean_salary_mid = salaries.loc[salaries["yrs.service"] > 20, "salary"].mean()
q4 = "Mean salary for professors with more than 20 years service:"
a4 = "{:.2f}".format(mean_salary_mid)
print(q4, a4)


# Question 5:  How do you find out more about the data set?
salaries.info()
salaries.describe()
salaries.describe(include=['O'])
for col in ["rank", "discipline", "sex"]:
    print("Counts:", col)
    print(salaries.loc[:, col].value_counts())

# Question 6:  What are the counts of men and women by rank? The proportions?
sex_counts = salaries.groupby("rank").sex.value_counts()
sex_prop = salaries.groupby("rank").sex.value_counts(normalize=True)
print("Counts of men and women by rank")
print(sex_counts)
print("Proportions of men and women by rank")
print(sex_prop)

# Question 7:  Draw a histogram for years of service. Add a density line in
# red. (Hint: plot proportions, not frequency.)
salaries.hist(column="yrs.service", density=True, bins=10)
salaries.loc[:, "yrs.service"].plot.kde(c="red")
plt.title("Distribution of years of service")
plt.xlabel("Years of service")
plt.grid()
plt.show()
plt.close()

# Question 8: Draw a box plot for salary.
salaries.boxplot("salary")
plt.title("Boxplot of salary")
plt.show()
plt.close()

# Question 9: Draw a box plot for salary by rank. Make it horizontal.
salaries.boxplot("salary", vert=False, by="rank")
plt.title("Boxplot of salary, grouped by rank")
plt.xlabel("Salary")
plt.suptitle("")
plt.show()
plt.close()

# Question 10: Plot salary vs years since PhD.
salaries.plot.scatter(x="yrs.since.phd", y="salary")
plt.xlabel("Years since PhD")
plt.ylabel("Salary")
plt.title("Salary vs. Years since PhD")
plt.show()
plt.close()


# Question 11: What is the correlation for salary vs. years since PhD and
# salary vs. years of service?
def print_salary_corr(corr, ind, cols):
    assert ind != 0
    doc = "The correlation between {} and {} is: {:.2f}".format(cols[0],
                                                                cols[ind],
                                                                corr[0, ind])
    print(doc)


cols = ["salary", "yrs.since.phd", "yrs.service"]
corr = np.corrcoef(x=salaries.loc[:, cols].values.T)
print_salary_corr(corr=corr, ind=1, cols=cols)
print_salary_corr(corr=corr, ind=2, cols=cols)


# Question 12: Are they statistically significant?
def pearson_test_decision(x, y, alpha=0.05):
    r, p = scipy.stats.pearsonr(x, y)
    print("The correlation between {} and {} is {:.2f}".format(x.name,
                                                               y.name,
                                                               r))
    print("The p value is {:.5f}".format(p))
    if p < alpha:
        print("It is significant at the {}% level".format((1-alpha)*100))
    else:
        print("It is not significant at the {}% level".format((1-alpha)*100))


pearson_test_decision(salaries["salary"], salaries["yrs.since.phd"])
pearson_test_decision(salaries["salary"], salaries["yrs.service"])

# Question 13: Draw a visualization of all bivariate relationships.
sns.pairplot(salaries)
plt.show()
plt.close()

# Question 14: What are the mean salaries, by rank and sex?
mean_salaries = salaries.groupby(["rank", "sex"]).salary.mean()
print("Mean salaries, by rank and sex:")
print(mean_salaries)

# Question 15: Plot those with a horizontal boxplot (conditioned on sex).
mean_salaries.to_frame().boxplot(by="sex", vert=False)
plt.suptitle("")
plt.xlabel("Mean salary")
plt.title("Boxplot of mean salary, grouped by sex")
plt.show()
plt.close()
# Question 16: Does the proportion of women differ by discipline?
sex_by_discipline = salaries.groupby("discipline")["sex"].value_counts()
counts = sex_by_discipline.values.reshape((2, 2))
n_discipline = counts.sum(axis=1).repeat(2).reshape(2, 2)
probs = counts / n_discipline
assert all(probs.sum(axis=1) == np.ones(2))
print("Propotion of men/women by discipline:")
print(probs)
stat, p = scipy.stats.chisquare(f_obs=probs[0, :], f_exp=probs[1, :])
print("Result of chi-Square test:")
print("value of test-statistic: {}\np-value: {}".format(stat, p))

if p < 0.05:
    print("The proportion of women does differ by discipline.")
else:
    print("The proportion of women does not differ by discipline.")
