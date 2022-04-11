# WILLIAM WIJAYA
# TP059360

library(stringr)
library(plyr)
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(ggplot2)
library(plotrix)

rm(list=ls())
setwd("C:/Users/LEGION.LAPTOP-HVMBBO3R/OneDrive - Asia Pacific University/Documents/My Documents/APU/Semester 3/Programming-for-Data-Analysis/Assignment/my/")
emp_data <- read.csv("employee_attrition.csv", header = TRUE)
emp_table <- tibble(emp_data) %>%
    mutate(
        recorddate_key = as.Date(recorddate_key, format = "%m/%d/%Y"),
        birthdate_key = as.Date(birthdate_key, format = "%m/%d/%Y"),
        orighiredate_key = as.Date(orighiredate_key, format = "%m/%d/%Y"),
        terminationdate_key = as.Date(terminationdate_key, format = "%m/%d/%Y"),
        city_name = factor(city_name),
        department_name = factor(department_name),
        job_title = factor(job_title),
        gender_short = factor(gender_short),
        gender_full = factor(gender_full),
        termreason_desc = factor(termreason_desc),
        termtype_desc = factor(termtype_desc),
        STATUS_YEAR = as.Date(as.character(STATUS_YEAR), format = "%Y"),
        STATUS = factor(STATUS),
        BUSINESS_UNIT = factor(BUSINESS_UNIT)
        )

str(emp_table)


theme_update(plot.title = element_text(hjust=0.5),
             axis.title.x = element_text(face="bold", color="black", size=10),
             axis.title.y = element_text(face="bold", color="black", size=10),
             legend.title = element_text(face="bold", size=12),
             axis.text.x = element_text(angle=90, size=8))

## Assuming the company starts naming the employee ID from 1318 and goes sequentially
empID <- emp_table$EmployeeID %>% unique
missing <- length(seq(min(empID), max(empID))) - length(empID)
unknown_period <- difftime(
  min(emp_table$recorddate_key),
  min(emp_table$orighiredate_key)
  ) %>%
  time_length("years") %>%
  floor

average_term <- missing / unknown_period

## Question 1: How is the company growth throughout the year?
### Analysis 1-1: Find the number of employees hired, terminated and total per year.
compute_hire_amt <- function(dt) {
    hire_amt <- (dt %>%
                     mutate(year = year(orighiredate_key)) %>%
                     group_by(year) %>%
                     summarise(n = n_distinct(EmployeeID))
                     )
    return(hire_amt)
}

compute_term_amt <- function(dt) {
    term_amt <- (dt %>%
                     mutate(year = year(terminationdate_key)) %>%
                     filter(year != 1900) %>%
                     group_by(year) %>%
                     summarise(n = n_distinct(EmployeeID))
                     )
    return(term_amt)
}

compute_no_employee <- function(dt) {
    hire_amt <- compute_hire_amt(dt)
    term_amt <- compute_term_amt(dt)
    merged <- merge(hire_amt, term_amt, by = "year", all = TRUE)
    colnames(merged) <- c("year", "hire", "term")
    merged$hire <- ifelse(is.na(merged$hire), 0, merged$hire)
    merged$term <- ifelse(is.na(merged$term), 0, merged$term)
    merged$final <- merged$hire - merged$term
    num_emp <- data.frame(merged$year, merged$hire, merged$term)
    total <- NULL
    for (val in merged$final) {
        if (is.null(total)) {
            total <- val
        } else {
            tmp <- total[length(total)] + val
            total <- c(total, tmp)
        }
    }
    num_emp <- cbind(num_emp, total)
    return(num_emp)
}

num_emp <- compute_no_employee(emp_table)
colnames(num_emp) <- c("year", "hired", "termed", "total")

line_plot <- num_emp %>%
    pivot_longer(
        cols = c("hired", "termed"),
        names_to = "action",
        values_to = "amount"
        ) %>%
    ggplot(aes(x=year, y=amount)) +
    geom_line(aes(color=action), size = 2) +
    scale_x_continuous(
        name = "year",
        breaks = seq(min(num_emp$year), max(num_emp$year))
    ) +
    scale_y_continuous(
        name="amount",
        breaks = seq(
            0,
            round_any(
                max(c(num_emp[,"hired"], num_emp[,"termed"])), 25, ceiling
                ),
            25
        )
    ) +
    labs(title = "\nHired vs Termed\n")

bar_plot <- ggplot(num_emp, aes(x=year, y=total)) +
    geom_bar(stat="Identity", fill="light blue") +
    geom_text(aes(label=total), size=3) +
    scale_x_continuous(
        breaks = seq(min(num_emp$year), max(num_emp$year))
    ) +
    labs(x = "\nYear\n", y = "\nAmount\n", title = "\nTotal Employees\n")



### Analysis 1-2: Find the number of recruits and terminations based on business unit per year.
term_by_var <- function(emp_table, colVec) {
    return(
        emp_table %>%
        mutate(year = year(terminationdate_key)) %>%
        # filter(year != 1900) %>%
        group_by_at(colVec)
    )
}

hire_by_var <- function(emp_table, colVec) {
    return(
        emp_table %>%
        mutate(year = year(orighiredate_key)) %>%
        group_by_at(colVec)
    )
}

# Number of employees hired yearly by BUSINESS UNIT
hire_by_year_BU <- hire_by_var(emp_table, c("year", "BUSINESS_UNIT")) %>%
    summarise(n = n_distinct(EmployeeID))

# Number of employees terminated yearly by BUSINESS UNIT
term_by_year_BU <- term_by_var(emp_table, c("year", "BUSINESS_UNIT")) %>%
    filter(year != 1900) %>%
    summarise(n = n_distinct(EmployeeID))

temp <- merge(hire_by_year_BU, term_by_year_BU,
              by = c("year", "BUSINESS_UNIT"),
              all = TRUE)
colnames(temp) <- c("year", "BUSINESS_UNIT", "hired", "termed")
temp$hired <- ifelse(is.na(temp$hired), 0, temp$hired)
temp$termed <- ifelse(is.na(temp$termed), 0, temp$termed)

long_format_temp <- temp %>%
    pivot_longer(
        cols = c("hired", "termed"),
        names_to = "action",
        values_to = "amount"
    )

total_amt_per_year <- tapply(
    long_format_temp$amount,
    long_format_temp$year,
    FUN = sum
)

# long_format_temp %>% aggregate(amount ~ year, ., sum)

analysis_1_2 <- long_format_temp %>%
    mutate(
        BU_action = factor(
            str_replace(interaction(BUSINESS_UNIT, action), "\\.", "\n")
        )
    ) %>%
    ggplot(aes(x=year, y=amount, fill=BU_action)) +
    geom_bar(stat = "Identity") +
    scale_x_continuous(
        breaks = seq(min(long_format_temp$year), max(long_format_temp$year))
    ) +
    scale_y_continuous(
        breaks = seq(0, round_any(max(total_amt_per_year), 50, ceiling), 50)
    ) +
    scale_fill_discrete(
        name = "Action on BU"
    ) +
    labs(
        x="Year",
        title = "\nHired vs Termed by Business Unit yearly\n",
    ) +
    theme(legend.text = element_text(size=8))



### Analysis 1-3: Find the number of recruits and terminations based on gender.
hire_by_year_gender <- hire_by_var(emp_table, c("year", "gender_short")) %>%
    summarise(n = n_distinct(EmployeeID))
colnames(hire_by_year_gender) <- c("year", "gender", "n")

analysis_1_3_a <- hire_by_year_gender %>%
    ggplot(aes(x=year, y=n, fill=gender)) +
    geom_bar(stat = "Identity", position = "dodge", width = 0.7) +
    scale_x_continuous(breaks = seq(min(hire_by_year_gender$year),
                                    max(hire_by_year_gender$year))) +
    scale_y_continuous(
        breaks = seq(0, round_any(max(hire_by_year_gender$n), 25, ceiling), 25)
    ) +
    scale_fill_discrete(name = "Gender") +
    labs(x="Year", title = "\nNumbers of hired employees by gender yearly")



term_by_year_gender <- term_by_var(emp_table, c("year", "gender_short")) %>%
    filter(year != 1900) %>%
    summarise(n = n_distinct(EmployeeID))
colnames(term_by_year_gender) <- c("year", "gender", "n")

analysis_1_3_b <- term_by_year_gender %>%
    ggplot(aes(x=year, y=n, fill=gender)) +
    geom_bar(stat = "Identity", position = "dodge", width = 0.7) +
    scale_x_continuous(breaks = seq(min(term_by_year_gender$year),
                                    max(term_by_year_gender$year))) +
    scale_y_continuous(
        breaks = seq(0, round_any(max(term_by_year_gender$n), 25, ceiling), 25)
    ) +
    scale_fill_discrete(name = "Gender") +
    labs(x="Year", title = "\nNumbers of termed employees by gender yearly")

by_year_gender <- merge(hire_by_year_gender, term_by_year_gender,
                        by = c("year", "gender"), all = TRUE)
colnames(by_year_gender) <- c("year", "gender", "hired", "termed")
by_year_gender$hired <- ifelse(is.na(by_year_gender$hired), 0,
                               by_year_gender$hired)
by_year_gender$termed <- ifelse(is.na(by_year_gender$termed), 0,
                               by_year_gender$termed)

by_year_gender_long <- by_year_gender %>%
    pivot_longer(cols = c("hired", "termed"),
                 names_to = "action", values_to = "amount")

overall_gender <- by_year_gender_long %>%
    group_by(gender, action) %>%
    summarise(total = sum(amount)) %>%
    ungroup() %>%
    mutate(gender_action = factor(paste(gender, action))) %>%
    select(gender_action, total) %>%
    ggplot(aes(x="", y=total, fill=gender_action)) +
    geom_bar(stat = "Identity", color="white", width=0.1) +
    geom_text(aes(label=total), position = position_stack(vjust=0.5)) +
    coord_polar("y") +
    labs(title = "Gender hired and termed overall",
         x = "", y = "") +
    scale_fill_discrete(name = "Gender and action") +
    theme_void()



### Analysis 1-4: Find the frequency of term reason each year and the overall.
reason_type_variation <- factor(paste(emp_table$termreason_desc, emp_table$termtype_desc)) %>% levels()

term_var <- emp_table %>%
    filter(termreason_desc != "Not Applicable") %>%
    mutate(year = year(STATUS_YEAR)) %>%
    group_by(termreason_desc, year) %>%
    summarise(n = n()) %>%
    arrange(year)

term_var %>%
    ggplot(aes(x=year, y=n, fill=termreason_desc)) +
    geom_bar(stat="Identity", position = "dodge", width=0.4) +
    scale_x_continuous(breaks = seq(min(term_var$year), max(term_var$year))) +
    scale_y_continuous(breaks = seq(0,
                                    round_any(max(term_var$n), 10, ceiling),
                                    10)) +
    labs(title = "Termination variation per year", y = "amount")

### Analysis 1-5: Find the relation against department
hire_by_dept <- hire_by_var(emp_table, c("year", "department_name")) %>%
  summarise(n = n_distinct(EmployeeID))

term_by_dept <- term_by_var(emp_table, c("year", "department_name")) %>%
  filter(year != 1900) %>%
  summarise(n = n_distinct(EmployeeID))

dept_merged <- merge(hire_by_dept, term_by_dept,
      by = c("year", "department_name"),
      all = TRUE)

colnames(dept_merged) <- c("year", "dept", "hired", "termed")
dept_merged$hired <- ifelse(is.na(dept_merged$hired), 0, dept_merged$hired)
dept_merged$termed <- ifelse(is.na(dept_merged$termed), 0, dept_merged$termed)

dept_merged %>%
  pivot_longer(cols = c("hired", "termed"),
               names_to = "action",
               values_to = "number") %>%
  group_by(year, action) %>%
  ggplot(aes(x=year, y=number)) +
  geom_point(aes(colour=action, shape=action, size=number)) +
  scale_size(range = c(1, 3)) +
  labs(title = "Number of hired and termed based on department") +
  facet_wrap(~dept)


## Question 2: How is the age trend for hired and terminated throughout the years and the overall statistics?
### Analysis 2-1: Find the age hired throughout the years.
year_diff <- function(date1, date2) {
    return(difftime(date1, date2) %>%
               time_length("years") %>%
               abs %>%
               floor)
}

hired_by_age <- emp_table %>%
  mutate(year = year(orighiredate_key),
         hired_age = year_diff(orighiredate_key, birthdate_key)) %>%
  group_by(year, BUSINESS_UNIT, EmployeeID) %>%
  summarise(age = hired_age) %>%
  unique

hba <- cbind(hired_by_age, action=rep("hired", nrow(hired_by_age)))
    
### Analysis 2-2: Find the age terminated throughout the years.
termed_by_age <- emp_table %>%
  mutate(year = year(terminationdate_key),
         termed_age = year_diff(terminationdate_key, birthdate_key)) %>%
  filter(year != 1900) %>%
  group_by(year, BUSINESS_UNIT, EmployeeID) %>%
  summarise(age = termed_age) %>%
  unique

tba <- cbind(termed_by_age, action=rep("termed", nrow(termed_by_age)))

htba <- rbind(hba, tba)

### Analysis 2-3: Find age trend based on business unit.
age_trend <- htba %>%
    group_by(action, BUSINESS_UNIT, year) %>%
    summarise(mean_age = mean(age)) %>%
    mutate(BU_action = factor(paste(action, BUSINESS_UNIT))) %>%
    ggplot(aes(x=year, y=mean_age)) +
    geom_line(aes(color=BU_action), size=1.2) +
    scale_x_continuous(breaks = seq(min(htba$year), max(htba$year))) +
    scale_color_discrete(name = "Business Unit\non action") +
    labs(title = "Age trend")


age_dist <- htba %>%
  group_by(action, BUSINESS_UNIT, year) %>%
  mutate(BU_action = factor(paste(action, BUSINESS_UNIT))) %>%
  ggplot(aes(x=year, y=age)) +
  geom_point(aes(color=age), size=2.5) +
  scale_color_gradient("age", low = "blue", high = "red") +
  stat_smooth(method = lm, fullrange = TRUE, aes(fill=BU_action)) +
  scale_x_continuous(breaks = seq(min(htba$year), max(htba$year), 3)) +
  scale_fill_discrete(name = "Business Unit\non action") +
  labs(title = "Age Distribution") +
  facet_wrap(~BU_action)

term_test <- term_by_var(
  emp_table,
  c("year", "termreason_desc", "BUSINESS_UNIT", "gender_short")) %>%
  filter(termreason_desc != "Not Applicable") %>%
  ungroup %>%
  mutate(BU_gender = factor(paste(gender_short, BUSINESS_UNIT))) %>%
  group_by(year, termreason_desc, BU_gender) %>%
  summarise(mean_age = mean(age)) %>%
  ggplot(aes(x=termreason_desc, y=mean_age, fill=BU_gender)) +
  geom_bar(stat="Identity", position = "dodge", color="black") +
  scale_fill_discrete(name = "Business Unit\nby gender") +
  labs(title = "Terminated employees yearly age average\nbased on Termination Reason by Business Unit and Gender",
       x = "Termination Reason", y = "Mean Age") +
  theme(axis.text.x = element_text(angle=45, vjust=0.5)) +
  facet_wrap(~year)

hire_test <- hire_by_var(
    emp_table,
    c("year", "BUSINESS_UNIT", "gender_full")
  ) %>% 
  mutate(hired_age = year_diff(orighiredate_key, birthdate_key)) %>%
  group_by(year, BUSINESS_UNIT, gender_full, EmployeeID) %>%
  summarise(hired_age = min(hired_age)) %>%
  ungroup %>%
  group_by(year, BUSINESS_UNIT, gender_full) %>%
  summarise(mean_hired = mean(hired_age)) %>%
  ggplot(aes(x=BUSINESS_UNIT, y=mean_hired, fill=gender_full)) +
  geom_bar(stat="Identity", position = "dodge", color="black") +
  scale_fill_discrete(name = "Gender") +
  labs(title = "Hired employees yearly age average\nbased on Business Unit and Gender",
       x = "Business Unit", y = "Mean Age") +
  theme(axis.text.x = element_text(angle=45, vjust=0.5)) +
  facet_wrap(~year)
  
### Analysis 2-4: Find the overall age statistics
overall_age_box <- htba %>%
    mutate(BU_action = factor(paste(BUSINESS_UNIT, action))) %>%
    ggplot(aes(x=BU_action, y=age, fill=BU_action)) +
    geom_boxplot() +
    stat_summary(fun=mean, geom="point", aes(shape="mean"),
                 size=3, show.legend=TRUE) +
    stat_summary(fun=mean, geom="text",
                 hjust = -0.5, aes(label=round(..y.., digits=1)),
                 show.legend = FALSE) +
    scale_shape_manual("Statistics", values=c("mean"="x")) +
    scale_fill_discrete(name = "Business Unit\non\naction") +
    labs(title = "Age statistics by Business Unit",
         x = "Business Unit on Action") +
    theme(axis.text.x = element_text(angle=0))

hired_agegroup <- hired_by_age %>%
    mutate(agegroup = case_when(
        age < 20 ~ '<20',
        age < 30 ~ '20-29',
        age < 40 ~ '30-39',
        age < 50 ~ '40-49',
        age < 60 ~ '50-59',
        age < 70 ~ '60-69',
        age < 80 ~ '70-79',
        age < 90 ~ '80-89',
    )) %>%
    group_by(agegroup) %>%
    summarise(n = n())

overall_age_hired_pie <- pie3D(hired_agegroup[["n"]],
      labels = factor(hired_agegroup$agegroup),
      main = "Overall hired agegroup")


termed_agegroup <- termed_by_age %>%
    mutate(agegroup = case_when(
        age < 20 ~ '<20',
        age < 30 ~ '20-29',
        age < 40 ~ '30-39',
        age < 50 ~ '40-49',
        age < 60 ~ '50-59',
        age < 70 ~ '60-69',
        age < 80 ~ '70-79',
        age < 90 ~ '80-89',
    )) %>%
    group_by(agegroup) %>%
    summarise(n = n())

overall_age_termed_pie <- pie3D(termed_agegroup[["n"]],
      labels = factor(termed_agegroup$agegroup),
      main = "Overall termed agegroup")


## Question 3: What are the characteristics of employees based on termination reason?

### Analysis 3-1: Find number of termination reasons based on gender.
term_gender_count <- term_by_var(emp_table,
                              c("termreason_desc", "gender_short")) %>%
  subset(termreason_desc != "Not Applicable") %>%
  summarise(n = n())

pie_gender_term <- pie(term_gender_count$n,
    labels = paste0(term_gender_count$termreason_desc,
                    " (", term_gender_count$gender_short, "): ",
                    term_gender_count$n),
    main = "Gender Chart on Termination Reason")

### Analysis 3-2: Find age statistics for each termination reason
term_age <- term_by_var(emp_table, c("termreason_desc")) %>%
  filter(termreason_desc != "Not Applicable") %>%
  summarise(age)

means <- term_age %>% summarise(age = mean(age))

term_age %>%
  ggplot(aes(x = termreason_desc, y = age, color=termreason_desc)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", aes(shape="mean"),
               size=3, show.legend=TRUE) +
  stat_summary(fun=min, geom="point", aes(shape="min"),
               size=3, show.legend=TRUE) +
  stat_summary(fun=max, geom="point", aes(shape="max"),
               size=3, show.legend=TRUE) +
  stat_summary(fun=mean, geom="text",
               hjust = -0.5, aes(label=round(..y.., digits=1)),
               show.legend = FALSE) +
  stat_summary(fun=min, geom="text", 
               vjust = 1.4, aes(label=round(..y.., digits=1)),
               show.legend = FALSE) +
  stat_summary(fun=max, geom="text", 
               vjust = -0.7, aes(label=round(..y.., digits=1)),
               show.legend = FALSE) +
  scale_shape_manual("Statistics", values=c("mean"="x", "min"="m", "max"="M")) +
  scale_color_discrete(name = "Term Reason") +
  labs(title = "Age overall statistics on termination reason",
       x = "Termination Reason") +
  theme(axis.text.x = element_text(angle=0))


### Analysis 3-3: Find the number of termination reasons based on business unit.
term_unit <- term_by_var(emp_table, c("termreason_desc", "BUSINESS_UNIT")) %>%
  subset(termreason_desc != "Not Applicable") %>%
  summarise(n = n())

### Analysis 3-4: Find the top and the least terminated department based on termination reason.
term_dept_n <- term_by_var(emp_table,
                           c("termreason_desc", "department_name")) %>%
  subset(termreason_desc != "Not Applicable") %>%
  summarise(n = n()) %>%
  ungroup %>%
  group_by(termreason_desc)

term_dept_n %>% slice(which.max(n))
term_dept_n %>% slice(which.min(n))

### Analysis 3-5: Find the top and the least terminated city based on termination reason.
term_city_n <- term_by_var(emp_table, c("termreason_desc", "city_name")) %>%
  subset(termreason_desc != "Not Applicable") %>%
  summarise(n = n()) %>%
  ungroup %>%
  group_by(termreason_desc)

term_city_n %>% slice(which.max(n))
term_city_n %>% slice(which.min(n))

### Analysis 3-6: Find the top and the least terminated job title based on termination reason.
term_job_n <- term_by_var(emp_table, c("termreason_desc", "job_title")) %>%
  subset(termreason_desc != "Not Applicable") %>%
  summarise(n = n()) %>%
  ungroup %>%
  group_by(termreason_desc)

term_job_n %>% slice(which.max(n))
term_job_n %>% slice(which.min(n))



## Question 4: What are the characteristics of hired employees?

### Analysis 4-1: Find the top 20% of hired employees based on job title.

hire_n_job_title <- hire_by_var(emp_table, c("EmployeeID", "job_title")) %>%
  slice(which.min(length_of_service)) %>%
  group_by(job_title) %>%
  summarise(n = n())

hire_n_job_title %>% filter(n >= quantile(n, 0.8)) %>% arrange(desc(n))

### Analysis 4-2: Find the top 20% of hired employees based on department.
hire_n_department_name <- hire_by_var(emp_table, c("EmployeeID", "department_name")) %>%
  slice(which.min(length_of_service)) %>%
  group_by(department_name) %>%
  summarise(n = n())

hire_n_department_name %>% filter(n >= quantile(n, 0.8)) %>% arrange(desc(n))

### Analysis 4-3: Find the top 20% of hired employees based on city.
hire_n_city_name <- hire_by_var(emp_table, c("EmployeeID", "city_name")) %>%
  slice(which.min(length_of_service)) %>%
  group_by(city_name) %>%
  summarise(n = n())

hire_n_city_name %>% filter(n >= quantile(n, 0.8)) %>% arrange(desc(n))


### Analysis 4-4: Find the top 20% of hired job title based on gender.
hire_n_gender_job <- hire_by_var(emp_table, c("EmployeeID")) %>%
  slice(which.min(length_of_service)) %>%
  group_by(gender_short, job_title) %>%
  summarise(n = n())

spread_format <- hire_n_gender_job %>%
  spread(gender_short, n, fill=0)

colnames(spread_format) <- c("job", "Female", "Male")
spread_format <- cbind(spread_format,
                       total=spread_format$Female + spread_format$Male)

spread_format %>%
  filter(total >= quantile(total, 0.8)) %>%
  arrange(desc(total))

## Question 5: What are the characteristics which affect the length of service for terminated employees?

# Variation in length of service before termination

service_df <- emp_table %>%
  group_by(EmployeeID) %>%
  slice(which.max(length_of_service))

term_service <- service_df %>%
  filter(STATUS == "TERMINATED") %>%
  rename(service = length_of_service)

top_n_los <- function(df, x, byCol) {
  df %>%
    ungroup %>%
    filter(service >= quantile(service, x)) %>%
    group_by_at(byCol) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
}

last_n_los <- function(df, x, byCol) {
  df %>%
    ungroup %>%
    filter(service <= quantile(service, x)) %>%
    group_by_at(byCol) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
}

### Analysis 5-1: Find the average age for the top 10% longest and shortest service.
term_service %>%
  ungroup %>%
  filter(service >= quantile(service, 0.9)) %>%
  pull(age) %>%
  summary

term_service %>%
  ungroup %>%
  filter(service <= quantile(service, 0.1)) %>%
  pull(age) %>%
  summary

### Analysis 5-2: Find the gender count for the top 10% longest and shortest service.
top_n_los(term_service, 0.9, "gender_short")

last_n_los(term_service, 0.1, "gender_short")

### Analysis 5-3: Find the average length of service based on the gender.
term_service %>%
  ungroup %>%
  aggregate(service ~ gender_short, ., mean) %>%
  arrange(desc(service))

### Analysis 5-4: Find the business unit count for the top 10% longest and shortest service.
top_n_los(term_service, 0.9, "BUSINESS_UNIT")

last_n_los(term_service, 0.1, "BUSINESS_UNIT")

### Analysis 5-5: Find the average length of service based on the gender.
term_service %>%
  ungroup %>%
  aggregate(service ~ BUSINESS_UNIT, ., mean) %>%
  arrange(desc(service))

### Analysis 5-6: Find the city count for the top 1% longest and shortest service.
top_n_los(term_service, 0.99, "city_name")

last_n_los(term_service, 0.01, "city_name")

### Analysis 5-7: Find the best and worst average length of service based on the city.
term_service %>%
  ungroup %>%
  aggregate(service ~ city_name, ., mean) %>%
  filter(service >= quantile(service, 0.9)) %>%
  arrange(desc(service))

term_service %>%
  ungroup %>%
  aggregate(service ~ city_name, ., mean) %>%
  filter(service <= quantile(service, 0.1)) %>%
  arrange(desc(service))

### Analysis 5-8: Find the job title count for the top 1% longest and shortest service.
top_n_los(term_service, 0.99, "job_title")

last_n_los(term_service, 0.01, "job_title")

### Analysis 5-9: Find the best and worst average length of service based on the job title.
term_service %>%
  ungroup %>%
  aggregate(service ~ job_title, ., mean) %>%
  filter(service >= quantile(service, 0.9)) %>%
  arrange(desc(service))

term_service %>%
  ungroup %>%
  aggregate(service ~ job_title, ., mean) %>%
  filter(service <= quantile(service, 0.1)) %>%
  arrange(desc(service))

### Analysis 5-10: Find the department count for the top 1% longest and shortest service.
top_n_los(term_service, 0.99, "department_name")

last_n_los(term_service, 0.01, "department_name")

### Analysis 5-11: Find the best and worst average length of service based on the department.
term_service %>%
  ungroup %>%
  aggregate(service ~ department_name, ., mean) %>%
  filter(service >= quantile(service, 0.9)) %>%
  arrange(desc(service))

term_service %>%
  ungroup %>%
  aggregate(service ~ department_name, ., mean) %>%
  filter(service <= quantile(service, 0.1)) %>%
  arrange(desc(service))

### Analysis 5-12: Find the store name count for the top 1% longest and shortest service.
top_n_los(term_service, 0.99, "store_name")

last_n_los(term_service, 0.01, "store_name")

### Analysis 5-13: Find the best and worst average length of service based on the store name.
term_service %>%
  ungroup %>%
  aggregate(service ~ store_name, ., mean) %>%
  filter(service >= quantile(service, 0.9)) %>%
  arrange(desc(service))

term_service %>%
  ungroup %>%
  aggregate(service ~ store_name, ., mean) %>%
  filter(service <= quantile(service, 0.1)) %>%
  arrange(desc(service))

### Analysis 5-14: Find the term reason count for the top 10% longest and shortest service.
top_n_los(term_service, 0.9, "termreason_desc")

last_n_los(term_service, 0.1, "termreason_desc")

### Analysis 5-15: Find the average length of service based on the term reason.
term_service %>%
  ungroup %>%
  aggregate(service ~ termreason_desc, ., mean) %>%
  arrange(desc(service))

### Analysis 5-16: Find the age group count for the top 10% longest and shortest service.
term_ageG_service <-  term_service %>%
  mutate(agegroup = case_when(
    age < 20 ~ '<20',
    age < 30 ~ '20-29',
    age < 40 ~ '30-39',
    age < 50 ~ '40-49',
    age < 60 ~ '50-59',
    age < 70 ~ '60-69',
    age < 80 ~ '70-79',
    age < 90 ~ '80-89',
))

term_ageG_service %>% top_n_los(0.9, "agegroup")
term_ageG_service %>% last_n_los(0.1, "agegroup")

### Analysis 5-17: Find the average length of service based on the age group.
term_ageG_service %>%
  aggregate(service ~ agegroup, ., mean) %>%
  arrange(desc(service))

### Analysis 5-18: Find the reason of termination for the 4 less than 20 years old employees with less than one year experience.
term_ageG_service %>%
  filter(agegroup == "<20") %>%
  select(termreason_desc, city_name, job_title, terminationdate_key)



## Question 6: What are the characteristics which affect the length of service based on active employees?
hire_service <- service_df %>%
  filter(STATUS == "ACTIVE") %>%
  rename(service = length_of_service)

### Analysis 6-1: Find the average age for the top 10% longest and shortest service.
hire_service %>%
  ungroup %>%
  filter(service >= quantile(service, 0.9)) %>%
  pull(age) %>%
  summary

hire_service %>%
  ungroup %>%
  filter(service <= quantile(service, 0.1)) %>%
  pull(age) %>%
  summary

### Analysis 6-2: Find the gender count for the top 10% longest and shortest service.
top_n_los(hire_service, 0.9, "gender_short")

last_n_los(hire_service, 0.1, "gender_short")

### Analysis 6-3: Find the average length of service based on the gender.
hire_service %>%
  ungroup %>%
  aggregate(service ~ gender_short, ., mean) %>%
  arrange(desc(service))

### Analysis 6-4: Find the business unit count for the top 10% longest and shortest service.
top_n_los(hire_service, 0.9, "BUSINESS_UNIT")

last_n_los(hire_service, 0.1, "BUSINESS_UNIT")

### Analysis 6-5: Find the average length of service based on the business unit.
hire_service %>%
  ungroup %>%
  aggregate(service ~ BUSINESS_UNIT, ., mean) %>%
  arrange(desc(service))

### Analysis 6-6: Find the city count for the top 1% longest and shortest service.
top_n_los(hire_service, 0.99, "city_name")

last_n_los(hire_service, 0.01, "city_name")

### Analysis 6-7: Find the best and worst average length of service based on the city.
hire_service %>%
  ungroup %>%
  aggregate(service ~ city_name, ., mean) %>%
  filter(service >= quantile(service, 0.9)) %>%
  arrange(desc(service))

hire_service %>%
  ungroup %>%
  aggregate(service ~ city_name, ., mean) %>%
  filter(service <= quantile(service, 0.1)) %>%
  arrange(desc(service))

### Analysis 6-8: Find the job title count for the top 1% longest and shortest service.
top_n_los(hire_service, 0.99, "job_title")

last_n_los(hire_service, 0.01, "job_title")

### Analysis 6-9: Find the best and worst average length of service based on the job title.
hire_service %>%
  ungroup %>%
  aggregate(service ~ job_title, ., mean) %>%
  filter(service >= quantile(service, 0.9)) %>%
  arrange(desc(service))

hire_service %>%
  ungroup %>%
  aggregate(service ~ job_title, ., mean) %>%
  filter(service <= quantile(service, 0.1)) %>%
  arrange(desc(service))

### Analysis 6-10: Find the department count for the top 1% longest and shortest service.
top_n_los(hire_service, 0.99, "department_name")

last_n_los(hire_service, 0.01, "department_name")

### Analysis 6-11: Find the best and worst average length of service based on the department.
hire_service %>%
  ungroup %>%
  aggregate(service ~ department_name, ., mean) %>%
  filter(service >= quantile(service, 0.9)) %>%
  arrange(desc(service))


hire_service %>%
  ungroup %>%
  aggregate(service ~ department_name, ., mean) %>%
  filter(service <= quantile(service, 0.1)) %>%
  arrange(desc(service))

### Analysis 6-12: Find the store name count for the top 10% longest and shortest service.
top_n_los(hire_service, 0.99, "store_name")

last_n_los(hire_service, 0.01, "store_name")

### Analysis 6-13: Find the best and worst average length of service based on the store name.
hire_service %>%
  ungroup %>%
  aggregate(service ~ store_name, ., mean) %>%
  filter(service >= quantile(service, 0.9)) %>%
  arrange(desc(service))

hire_service %>%
  ungroup %>%
  aggregate(service ~ store_name, ., mean) %>%
  filter(service <= quantile(service, 0.1)) %>%
  arrange(desc(service))

### Analysis 6-14: Find the average length of service based on active employee.
hire_service %>%
  ungroup %>%
  aggregate(service ~ termreason_desc, ., mean) %>%
  arrange(desc(service))

### Analysis 6-15: Find the age group count for the top 10% longest and shortest service.
hire_ageG_service <-  hire_service %>%
  mutate(agegroup = case_when(
    age < 20 ~ '<20',
    age < 30 ~ '20-29',
    age < 40 ~ '30-39',
    age < 50 ~ '40-49',
    age < 60 ~ '50-59',
    age < 70 ~ '60-69',
    age < 80 ~ '70-79',
    age < 90 ~ '80-89',
))

hire_ageG_service %>% top_n_los(0.9, "agegroup")
hire_ageG_service %>% last_n_los(0.1, "agegroup")

### Analysis 6-16: Find the average length of service based on the age group.
hire_ageG_service %>%
  aggregate(service ~ agegroup, ., mean) %>%
  arrange(desc(service))

###
# emp_table %>% filter(BUSINESS_UNIT == "HEADOFFICE") %>% select(job_title) %>% unique
# emp_table %>% filter(BUSINESS_UNIT == "STORES") %>% select(job_title) %>% unique
# 
# emp_table %>% filter(BUSINESS_UNIT == "HEADOFFICE") %>% select(department_name) %>% unique
# emp_table %>% filter(BUSINESS_UNIT == "STORES") %>% select(department_name) %>% unique
