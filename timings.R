scripts <- sprintf("day%d.R",1:7)
for (s in scripts) source(s)

# individuals
(res <- bench::mark(
  day1(),
  day2(),
  day3(),
  day4(),
  day5(),
  day5dt(),
  day6(),
  day6ql(),
  day7(),
  check=FALSE
))
plot(res)


# all base solutions
system.time({
  day1();
  day2();
  day3();
  day4();
  day5();
  day6();
  day7()
})

# fastest solutions (includes tweaks to mpjdem and quentin leclerc solutions)
system.time({
  day1();
  day2();
  day3();
  day4();
  day5dt();
  day6ql();
  day7()
})
