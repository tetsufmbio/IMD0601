mydf <- read.csv("2014-07-08.csv", stringsAsFactors = FALSE)
class(mydf)
dim(mydf)
head(mydf)
summary(mydf)
str(mydf)

# carregar biblioteca dplyr
library(dplyr)
cran <- tbl_df(mydf)
rm("mydf")
cran
str(cran)

select(cran, ip_id, package, country)
select(cran, r_arch:country)
select(cran, country:r_arch)
select(cran, -time)
select(cran, -(5:20))
select(cran, -(X:size))


filter(cran, package == "swirl")
filter(cran, r_version == "3.1.1", country == "US")
filter(cran, r_version <= "3.0.2", country == "IN")
filter(cran, country == "US" | country == "IN")
filter(cran, size > 100500, r_os == "linux-gnu")
filter(cran, !is.na(r_version))

cran2 <- select(cran, size:ip_id)
arrange(cran2, ip_id)
arrange(cran2, desc(ip_id))
arrange(cran2, package, ip_id)
arrange(cran2, country, desc(r_version), ip_id)

cran3 <- select(cran, ip_id, package, size)
cran3
mutate(cran3, teste = "teste")
mutate(cran3, size_mb = size / 2^20)
mutate(cran3, size_mb = size / 2^20, size_gb = size_mb / 2^10)
mutate(cran3, correct_size = size + 1000)

summarize(cran3, mean(size))
cran
by_package <- group_by(cran, package)
by_package
summarize(by_package, mean(size))

pack_sum <- summarize(by_package,
                      count = n(),
                      unique = n_distinct(ip_id),
                      countries = n_distinct(country),
                      avg_bytes = mean(size))
pack_sum
View(pack_sum)
print(pack_sum)

quantile(pack_sum$count, probs = 0.99)
top_counts <- filter(pack_sum, count > 679)
top_counts
View(top_counts)
top_counts_sorted <- arrange(top_counts, desc(count))
View(top_counts_sorted)

# exerc?cio
quantile(pack_sum$unique, probs = 0.99)
top_unique <- filter(pack_sum, unique > 465)
View(top_unique)
top_unique_sorted <- arrange(top_unique, desc(unique))
View(top_unique_sorted)

# tidyr
library(tidyr)

students
gather(students, sex, count, -grade)

students2
res <- gather(students2, sex_class, count, -grade)
separate(res, sex_class, c("sex","class"))
separate(data = res, col = sex_class, into = c("sex", "class"))

students3
res <- gather(students3, class, grade, class1:class5, na.rm=TRUE)
res
res <- spread(res, test, grade)
res
library(readr)
parse_number("class5")
mutate(res, class = parse_number(class))

students4
student_info <- select(students4, id, name, sex)
unique(student_info)
gradebook <- select(students4, id, class, midterm, final) 
gradebook

passed
failed
passed <- mutate(passed, status = "passed")
passed
failed <- mutate(failed, status = "failed")
failed
bind_rows(passed, failed)

# exerc?cio
sat2 <- sat %>%
  select(-contains("total")) %>%
  gather(part_sex, count, -score_range) %>%
  separate(part_sex, c("part", "sex")) %>%
  ### <Your call to group_by()> 
  group_by(part, sex) %>%
  mutate(total = sum(count),
         prop = count / total
  ) 

library(tidyr)
library(dplyr)


sat2 <- select(sat, -contains("total"))
sat2
sat3 <- gather(sat2, part_sex, count, -score_range)
sat3
sat4 <- separate(sat3, part_sex, c("part", "sex"))
sat4
sat5 <- group_by(sat4, part, sex)
sat5
sat6 <- mutate(sat5, total = sum(count), prop = count / total)
sat6
