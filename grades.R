grades.wide <- data.table::fread("grades-resaved.csv")
grades.wide[["Quiz Aug 20 [Total Pts: 50 Score] |3137913"]]
grades.tall <- nc::capture_melt_single(
  grades.wide,
  category="[^ ]+",
  name=".*?",
  " \\[Total Pts: ",
  total="[0-9]+", as.integer,
  na.rm=FALSE)
names(grades.tall)
table(grades.tall$category)
table(grades.tall[["Child Course ID"]])
grades.tall[, is.grad := grepl("CS-599", `Child Course ID`)]
table(grades.tall$is.grad)
grades.tall[, weight := data.table::fcase(
  category=="final", 0.1,
  category=="Mid-term", 0.1,
  category=="Quiz", 0.1,
  category=="Homework" & is.grad, 0.6,
  category=="Homework" & !is.grad, 0.7,
  category=="R", 0.1)]
grades.tall[!is.finite(weight)]
grades.tall[, value.int := ifelse(
  is.na(value) | value=="In Progress", 0L, as.integer(value))]
grades.tall[!is.finite(value.int)]
category.stats <- grades.tall[!(category=="R" & !is.grad), .(
  value=sum(value.int),
  total=sum(total),
  N=.N
), by=.(`Last Name`, `First Name`, `Student ID`, is.grad, category, weight)]
student.stats <- category.stats[, .(
  score=sum(weight*value/total),
  weight=sum(weight),
  N=.N
), by=.(`Last Name`, `First Name`, `Student ID`, is.grad)]
stopifnot(student.stats$weight==1)
student.stats[order(score)]
student.stats[is.grad==FALSE][order(`Last Name`)]
student.stats[is.grad==TRUE][order(`Last Name`)]
