

library(readxl)
library(tidyverse)


# Cohort 2003 ####


## Exam Files ====

c03_base <- read_excel("Raw-Data/2003/ZBYZ2003_Demographics&Grades.XLS")

c03_base2 <- read_excel("Raw-Data/2003/ZBYZ2003_Demographics&Grades_2.xlsx")

c03_zkg1g2 <- read_excel("Raw-Data/2003/2003级中考高一高二成绩.XLS")

c03_gk <- read_excel("Raw-Data/2003/2003级高考成绩.XLS")

c03_zk <- read_excel("Raw-Data/2003/2003级高三成绩/g2003级入学成绩.xlsx")

c03_jc1 <- read_excel("Raw-Data/2003/2003级高三成绩/高三第1次测试.xlsx")

c03_jc2 <- read_excel("Raw-Data/2003/2003级高三成绩/高三第2次测试.xlsx")

c03_jc3 <- read_excel("Raw-Data/2003/2003级高三成绩/高三第3次测试.xlsx")

c03_jc4 <- read_excel("Raw-Data/2003/2003级高三成绩/高三第4次成绩.xlsx")

c03_jc5 <- read_excel("Raw-Data/2003/2003级高三成绩/高三第5次成绩.xlsx")

c03_mn1 <- read_excel("Raw-Data/2003/2003级高三成绩/高三一模成绩.xlsx")

c03_mn2 <- read_excel("Raw-Data/2003/2003级高三成绩/高三二模成绩.xlsx")


# Cohort 2004 ####


## Exam Files ====

c04_base <- read_excel("Raw-Data/2004/ZBYZ2004_Demographics&Grades.XLS")

c04_base2 <- read_excel("Raw-Data/2004/ZBYZ2004_Demographics&Grades_2.xlsx")

c04_zkg1g2 <- read_excel("Raw-Data/2004/2004级中考高一高二成绩.XLS")

c04_20050128qm_a <- read_excel("Raw-Data/2004/G2004/2005年1月28期末A卷.xlsx")

c04_20050128qm_b <- read_excel("Raw-Data/2004/G2004/2005年1月28期末B卷.xlsx")

c04_20050324yk <- read_excel("Raw-Data/2004/G2004/2005年3月24日月考.xlsx")

c04_20050630qm <- read_excel("Raw-Data/2004/G2004/2005年6月期末.xlsx")

c04_20050924yk <- read_excel("Raw-Data/2004/G2004/2005年9月24日月考.xlsx")

c04_20051104qz <- read_excel("Raw-Data/2004/G2004/2005年11月4日期中.xlsx")

c04_20051216yk <- read_excel("Raw-Data/2004/G2004/2005年12月16日测试.xlsx")

c04_20060115qm <- read_excel("Raw-Data/2004/G2004/2006年1月15日期末.xlsx") 

c04_20060212yk <- read_excel("Raw-Data/2004/G2004/2006年2月12日竞赛.xlsx")

c04_20060324yk <- read_excel("Raw-Data/2004/G2004/2006年3月24日月考.xlsx")

c04_20060426qz <- read_excel("Raw-Data/2004/G2004/2006年4月26日期中_新.xlsx")

c04_20060612yk <- read_excel("Raw-Data/2004/G2004/2006年6月12日月考.xlsx")

c04_20060712qm <- read_excel("Raw-Data/2004/G2004/2006年7月12日期末.xlsx")

c04_20060902yk <- read_excel("Raw-Data/2004/G2004/2006年9月2日月考.xlsx")

c04_20061012yk <- read_excel("Raw-Data/2004/G2004/2006年10月12日摸底考.xlsx")

c04_jc1 <- read_excel("Raw-Data/2004/G2004/高三一次检测.xlsx")

c04_jc2 <- read_excel("Raw-Data/2004/G2004/高三二次检测.xlsx")

c04_jc3 <- read_excel("Raw-Data/2004/G2004/高三三次检测.xlsx")

c04_jc4 <- read_excel("Raw-Data/2004/G2004/高三四次检测.xlsx")

c04_mn1 <- read_excel("Raw-Data/2004/G2004/高三一模.xlsx")

c04_mn2 <- read_excel("Raw-Data/2004/G2004/高三二模成绩.xlsx")

c04_gk <- read_excel("Raw-Data/2004/G2004/高考成绩.xlsx")

## 6d Files ====

c04_20050924yk_6d <- read_excel("Raw-Data/2004/G2004/2005年9月24日月考6d.xlsx")

c04_20051104qz_6d <- read_excel("Raw-Data/2004/G2004/2005年11月4日期中6d.xlsx")

c04_20051216yk_6d <- read_excel("Raw-Data/2004/G2004/2005年12月16日测试6d.xlsx")

c04_20060115qm_6d <- read_excel("Raw-Data/2004/G2004/2006年1月15日期末6d.xlsx") 

c04_20060212yk_6d <- read_excel("Raw-Data/2004/G2004/2006年2月12日竞赛6d.xlsx")

c04_20060324yk_6d <- read_excel("Raw-Data/2004/G2004/2006年3月24日月考6d.xlsx")

c04_20060426qz_6d <- read_excel("Raw-Data/2004/G2004/2006年4月26日期中_新6d.xlsx")

c04_20060612yk_6d <- read_excel("Raw-Data/2004/G2004/2006年6月12日月考6d.xlsx")

c04_20060712qm_6d <- read_excel("Raw-Data/2004/G2004/2006年7月12日期末6d.xlsx")

## Misc ====

c04_md <- read_excel("Raw-Data/2004/G2004/md.xlsx")

c04_md1 <- read_excel("Raw-Data/2004/G2004/md1.xlsx")

c04_testname <- read_excel("Raw-Data/2004/G2004/testname.xlsx")


# Cohort 2005 ####


## Exam Files ====

c05_base <- read_excel("Raw-Data/2005/ZBYZ2005_Demographics&Grades.XLS")

c05_base2 <- read_excel("Raw-Data/2005/ZBYZ2005_Demographics&Grades_2.xlsx")

c05_zkg1g2g3 <- read_excel("Raw-Data/2005/2005级中考高一高二高三成绩.XLS")

c05_gk <- read_excel("Raw-Data/2005/2005级高考成绩.XLS")

c05_zk <- read_excel("Raw-Data/2005/G2005/2005年入学成绩.xlsx")

c05_20051106qz <- read_excel("Raw-Data/2005/G2005/2005年11月6日期中.xlsx")

c05_20051106qz_2 <- read_excel("Raw-Data/2005/G2005/2005年11月6日期中之二.xlsx")

c05_20051222yk <- read_excel("Raw-Data/2005/G2005/2005年12月22日月考（六段分科）.xlsx")

c05_20060115qm <- read_excel("Raw-Data/2005/G2005/2006年1月15日期末.xlsx")

c05_20060212yk <- read_excel("Raw-Data/2005/G2005/2006年2月12日竞赛.xlsx")

c05_20060324yk <- read_excel("Raw-Data/2005/G2005/2006年3月24日月考.xlsx")

c05_20060818yk <- read_excel("Raw-Data/2005/G2005/2006年8月18日自主学习测试.xlsx")

c05_20061005yk <- read_excel("Raw-Data/2005/G2005/2006年10月5日测试.xlsx")

c05_20061106qz <- read_excel("Raw-Data/2005/G2005/2006年11月6日期中.xlsx")

c05_20061129yk <- read_excel("Raw-Data/2005/G2005/2006年11月29日月考.xlsx")

c05_20070206qm <- read_excel("Raw-Data/2005/G2005/2007年2月6日期末.xlsx")

c05_20070329yk <- read_excel("Raw-Data/2005/G2005/2007年3月29日月考.xlsx")

c05_20070429qz <- read_excel("Raw-Data/2005/G2005/2007年4月29日期中.xlsx")

c05_20070701qm_bhnl <- read_excel("Raw-Data/2005/G2005/2007年7月1日期末_不含能力.xlsx")

c05_20070701qm_hnl <- read_excel("Raw-Data/2005/G2005/2007年7月1日期末_含能力.xlsx")

c05_20070901yk_wj <- read_excel("Raw-Data/2005/G2005/2007年9月1日月考(含往届).xlsx")

c05_20070901yk_yj <- read_excel("Raw-Data/2005/G2005/2007年9月1日月考(应届).xlsx")

c05_20070928jc_wj <- read_excel("Raw-Data/2005/G2005/2007年9月28日一次摸底(含往届).xlsx")

c05_20070928jc_yj <- read_excel("Raw-Data/2005/G2005/2007年9月28日一次摸底(应届).xlsx")

c05_20071110qz_wj <- read_excel("Raw-Data/2005/G2005/2007年11月10日期中(全部).xlsx")

c05_20071110qz_yj <- read_excel("Raw-Data/2005/G2005/2007年11月10日期中(应届).xlsx")

c05_20071217jc_wj <- read_excel("Raw-Data/2005/G2005/2007年12月17日二摸底(含往届排名).xlsx")

c05_20071217jc_yj <- read_excel("Raw-Data/2005/G2005/2007年12月17日二摸底(应届生排名).xlsx")

c05_20080201qm_wj <- read_excel("Raw-Data/2005/G2005/2008年2月1日期末(含往届).xlsx")

c05_20080201qm_yj <- read_excel("Raw-Data/2005/G2005/2008年2月1日期末应届.xlsx")

c05_20080229mn1_wj_dk <- read_excel("Raw-Data/2005/G2005/2008年2月29日一模(含往届三段单科).xlsx")

c05_20080229mn1_wj_zh <- read_excel("Raw-Data/2005/G2005/2008年2月29日一模(含往届三段综合).xlsx")

c05_20080430mn2_yj <- read_excel("Raw-Data/2005/G2005/2008年4月30日二模应届排名.xlsx")

c05_20080430mn2_wj <- read_excel("Raw-Data/2005/G2005/2008年4月30日二模应往届排名.xlsx")

## 6d Files ====

c05_20051106qz_6d <- read_excel("Raw-Data/2005/G2005/2005年11月6日期中6d.xlsx")

c05_20051106qz_2_6d <- read_excel("Raw-Data/2005/G2005/2005年11月6日期中之二6d.xlsx")

c05_20051222yk_6d <- read_excel("Raw-Data/2005/G2005/2005年12月22日月考（六段分科）6d.xlsx")

c05_20060115qm_6d <- read_excel("Raw-Data/2005/G2005/2006年1月15日期末6d.xlsx")

c05_20060212yk_6d <- read_excel("Raw-Data/2005/G2005/2006年2月12日竞赛6d.xlsx")

c05_20060324yk_6d <- read_excel("Raw-Data/2005/G2005/2006年3月24日月考6d.xlsx")

c05_20060712qm_6d <- read_excel("Raw-Data/2005/G2005/2006年7月12日期末6d.xlsx")

c05_20060818yk_6d <- read_excel("Raw-Data/2005/G2005/2006年8月18日自主学习测试6d.xlsx")

c05_20061005yk_6d <- read_excel("Raw-Data/2005/G2005/2006年10月5日测试6d.xlsx")

c05_20061005yk_6d1<- read_excel("Raw-Data/2005/G2005/2006年10月5日测试6d1.xlsx")

c05_20061106qz_6d <- read_excel("Raw-Data/2005/G2005/2006年11月6日期中6d.xlsx")

c05_20061129yk_6d <- read_excel("Raw-Data/2005/G2005/2006年11月29日月考6d.xlsx")

c05_20070206qm_6d <- read_excel("Raw-Data/2005/G2005/2007年2月6日期末6d.xlsx")

c05_20070329yk_6d <- read_excel("Raw-Data/2005/G2005/2007年3月29日月考6d.xlsx")

c05_20070429qz_6d <- read_excel("Raw-Data/2005/G2005/2007年4月29日期中6d.xlsx")

c05_20070701qm_bhnl_6d <- read_excel("Raw-Data/2005/G2005/2007年7月1日期末_不含能力6d.xlsx")

c05_20070701qm_hnl_6d <- read_excel("Raw-Data/2005/G2005/2007年7月1日期末_含能力6d.xlsx")

c05_20070901yk_wj_6d <- read_excel("Raw-Data/2005/G2005/2007年9月1日月考(含往届)6d.xlsx")

c05_20070901yk_yj_6d <- read_excel("Raw-Data/2005/G2005/2007年9月1日月考(应届)6d.xlsx")

c05_20070928jc_wj_6d <- read_excel("Raw-Data/2005/G2005/2007年9月28日一次摸底(含往届)6d.xlsx")

c05_20070928jc_yj_6d <- read_excel("Raw-Data/2005/G2005/2007年9月28日一次摸底(应届)6d.xlsx")

c05_20071110qz_wj_6d <- read_excel("Raw-Data/2005/G2005/2007年11月10日期中(全部)6d.xlsx")

c05_20071110qz_yj_6d <- read_excel("Raw-Data/2005/G2005/2007年11月10日期中(应届)6d.xlsx")

c05_20071217jc_wj_6d <- read_excel("Raw-Data/2005/G2005/2007年12月17日二摸底(含往届排名)6d.xlsx")

c05_20071217jc_yj_6d <- read_excel("Raw-Data/2005/G2005/2007年12月17日二摸底(应届生排名)6d.xlsx")

c05_20080201qm_wj_6d <- read_excel("Raw-Data/2005/G2005/2008年2月1日期末(含往届)6D.xlsx")

c05_20080201qm_yj_6d <- read_excel("Raw-Data/2005/G2005/2008年2月1日期末应届6D.xlsx")

c05_20080229mn1_wj_dk_6d <- read_excel("Raw-Data/2005/G2005/2008年2月29日一模(含往届三段单科)6d.xlsx")

c05_20080229mn1_wj_zh_6d <- read_excel("Raw-Data/2005/G2005/2008年2月29日一模(含往届三段综合)6d.xlsx")

c05_20080430mn2_wj_6d <- read_excel("Raw-Data/2005/G2005/2008年4月30日二模应往届排名6d.xlsx")

## Misc ====

c05_20060426qz <- read_excel("Raw-Data/2005/G2005/2006年4月26日期中.xlsx")

c05_20060712qm <- read_excel("Raw-Data/2005/G2005/2006年7月12日期末.xlsx")

c05_md <- read_excel("Raw-Data/2005/G2005/md.xlsx")

c05_md1 <- read_excel("Raw-Data/2005/G2005/md1.xlsx")

c05_testname <- read_excel("Raw-Data/2005/G2005/testname.xlsx")


# Cohort 2006 ####


## Exam Files ====

c06_base <- read_excel("Raw-Data/2006/ZBYZ2006_Demographics&Grades.XLS")

c06_base2 <- read_excel("Raw-Data/2006/ZBYZ2006_Demographics&Grades_2.xlsx")

c06_base3 <- read_excel("Raw-Data/2006/ZBYZ2006_Demographics&Grades_3.xlsx")

c06_gk <- read_excel("Raw-Data/2006/2006级高考成绩.XLS")

c06_zk <- read_excel("Raw-Data/2006/G2006/入学成绩.xlsx")

c06_20061005yk <- read_excel("Raw-Data/2006/G2006/2006年10月5日测试.xlsx")

c06_20061106qz <- read_excel("Raw-Data/2006/G2006/2006年11月6日期中.xlsx")

c06_20061106qz_qb <- read_excel("Raw-Data/2006/G2006/2006年11月6日期中实验普通排名.xlsx")

c06_20061106qz_xb <- read_excel("Raw-Data/2006/G2006/2006年11月6日期中新班.xlsx")

c06_20061226yk_pt <- read_excel("Raw-Data/2006/G2006/2006年12月26日月考普通班排名.xlsx")

c06_20061226yk_qb <- read_excel("Raw-Data/2006/G2006/2006年12月26日月考全部班排名.xlsx")

c06_20070206qm_pt <- read_excel("Raw-Data/2006/G2006/2007年2月6日期末普通班排名.xlsx")

c06_20070206qm_qb <- read_excel("Raw-Data/2006/G2006/2007年2月6日期末全部班排名.xlsx")

c06_20070310yk_pt <- read_excel("Raw-Data/2006/G2006/2007年3月10日自学竞赛普通班排名.xlsx")

c06_20070310yk_qb <- read_excel("Raw-Data/2006/G2006/2007年3月10日自学竞赛全部班排名.xlsx")

c06_20070405yk_pt <- read_excel("Raw-Data/2006/G2006/2007年4月5日月考普通班.xlsx")

c06_20070405yk_qb <- read_excel("Raw-Data/2006/G2006/2007年4月5日月考全部班排名.xlsx")

c06_20070429qz_pt <- read_excel("Raw-Data/2006/G2006/2007年4月29日期中普通班.xlsx")

c06_20070429qz_qb <- read_excel("Raw-Data/2006/G2006/2007年4月29日期中全部班.xlsx")

c06_20070701qm_pt <- read_excel("Raw-Data/2006/G2006/2007年7月1日期末普通班排名.xlsx")

c06_20070701qm_qb <- read_excel("Raw-Data/2006/G2006/2007年7月1日期末全部班排名.xlsx")

c06_20070917yk_pt <- read_excel("Raw-Data/2006/G2006/2007年9月17日月考普通班排名.xlsx")

c06_20070917yk_qb <- read_excel("Raw-Data/2006/G2006/2007年9月17日月考全部班排名.xlsx")

c06_20071115qz_pt <- read_excel("Raw-Data/2006/G2006/2007年11月15日期中普通班排名.xlsx")

c06_20071115qz_qb <- read_excel("Raw-Data/2006/G2006/2007年11月15日期中全部班排名.xlsx")

c06_20071227yk_pt <- read_excel("Raw-Data/2006/G2006/2007年12月27日月考平行班排名.xlsx")

c06_20071227yk_qb <- read_excel("Raw-Data/2006/G2006/2007年12月27日月考全部班排名.xlsx")

c06_20080126qm_pt <- read_excel("Raw-Data/2006/G2006/2008年1月26日期末普通班排名.xlsx")

c06_20080126qm_qb <- read_excel("Raw-Data/2006/G2006/2008年1月26日期末全部班排名.xlsx")

c06_20080505qz_gkkm <- read_excel("Raw-Data/2006/G2006/2008年5月5日期中高考科目.xlsx")

c06_20080505qz_qbkm_pt <- read_excel("Raw-Data/2006/G2006/2008年5月5日期中普通班所有科目.xlsx")

c06_20080505qz_qbkm <- read_excel("Raw-Data/2006/G2006/2008年5月5日期中所有科目.xlsx")

c06_20080707qm_pt <- read_excel("Raw-Data/2006/G2006/2008年7月7日期末普通班.xlsx")

c06_20080707qm_qb <- read_excel("Raw-Data/2006/G2006/2008年7月7日期末全部班.xlsx")

c06_20081007jc_qb <- read_excel("Raw-Data/2006/G2006/2008年10月7日测试全部班.xlsx")

c06_20081106jc_before <- read_excel("Raw-Data/2006/G2006/2008年11月6日测试综合未折成绩.xlsx")

c06_20081106jc_after <- read_excel("Raw-Data/2006/G2006/2008年11月6日测试综合折后成绩.xlsx")

c06_20090115qm <- read_excel("Raw-Data/2006/G2006/2009年1月15日期末.xlsx")

c06_20090215jc <- read_excel("Raw-Data/2006/G2006/2009年2月15日测试.xlsx")

c06_20090315mn1 <- read_excel("Raw-Data/2006/G2006/2009年3月15日一模.xlsx")

c06_20090315mn1_after <- read_excel("Raw-Data/2006/G2006/2009年3月15日一模折后.xlsx")

c06_20090429mn2 <- read_excel("Raw-Data/2006/G2006/2009年4月29日二模.xlsx")

## 6d Files ====

c06_20061005yk_6d <- read_excel("Raw-Data/2006/G2006/2006年10月5日测试6d.xlsx")

c06_20061106qz_6d <- read_excel("Raw-Data/2006/G2006/2006年11月6日期中6d.xlsx")

c06_20061106qz_qb_6d <- read_excel("Raw-Data/2006/G2006/2006年11月6日期中实验普通排名6d.xlsx")

c06_20061226yk_pt_6d <- read_excel("Raw-Data/2006/G2006/2006年12月26日月考普通班排名6d.xlsx")

c06_20061226yk_qb_6d <- read_excel("Raw-Data/2006/G2006/2006年12月26日月考全部班排名6d.xlsx")

c06_20070206qm_pt_6d <- read_excel("Raw-Data/2006/G2006/2007年2月6日期末普通班排名6d.xlsx")

c06_20070206qm_qb_6d <- read_excel("Raw-Data/2006/G2006/2007年2月6日期末全部班排名6d.xlsx")

c06_20070310yk_pt_6d <- read_excel("Raw-Data/2006/G2006/2007年3月10日自学竞赛普通班排名6d.xlsx")

c06_20070310yk_qb_6d <- read_excel("Raw-Data/2006/G2006/2007年3月10日自学竞赛全部班排名6d.xlsx")

c06_20070405yk_pt_6d <- read_excel("Raw-Data/2006/G2006/2007年4月5日月考普通班6d.xlsx")

c06_20070405yk_qb_6d <- read_excel("Raw-Data/2006/G2006/2007年4月5日月考全部班排名6d.xlsx")

c06_20070429qz_pt_6d <- read_excel("Raw-Data/2006/G2006/2007年4月29日期中普通班6d.xlsx")

c06_20070429qz_qb_6d<- read_excel("Raw-Data/2006/G2006/2007年4月29日期中全部班6d.xlsx")

c06_20070701qm_qb_6d <- read_excel("Raw-Data/2006/G2006/2007年7月1日期末全部班排名6d.xlsx")

c06_20070917yk_pt_6d <- read_excel("Raw-Data/2006/G2006/2007年9月17日月考普通班排名6d.xlsx")

c06_20071115qz_pt_6d <- read_excel("Raw-Data/2006/G2006/2007年11月15日期中普通班排名6d.xlsx")

c06_20071115qz_qb_6d <- read_excel("Raw-Data/2006/G2006/2007年11月15日期中全部班排名6d.xlsx")

c06_20071227yk_pt_6d <- read_excel("Raw-Data/2006/G2006/2007年12月27日月考平行班排名6d.xlsx")

c06_20071227yk_qb_6d <- read_excel("Raw-Data/2006/G2006/2007年12月27日月考全部班排名6d.xlsx")

c06_20080126qm_pt_6d <- read_excel("Raw-Data/2006/G2006/2008年1月26日期末普通班排名6d.xlsx")

c06_20080126qm_qb_6d <- read_excel("Raw-Data/2006/G2006/2008年1月26日期末全部班排名6d.xlsx")

c06_20080505qz_gkkm_6d <- read_excel("Raw-Data/2006/G2006/2008年5月5日期中高考科目6d.xlsx")

c06_20080505qz_gkkm_pt_6d <- read_excel("Raw-Data/2006/G2006/2008年5月5日期中普通班高考科目6d.xlsx")

c06_20080505qz_qbkm_pt_6d <- read_excel("Raw-Data/2006/G2006/2008年5月5日期中普通班所有科目6d.xlsx")

c06_20080505qz_qbkm_6d <- read_excel("Raw-Data/2006/G2006/2008年5月5日期中所有科目6d.xlsx")

c06_20080707qm_pt_6d <- read_excel("Raw-Data/2006/G2006/2008年7月7日期末普通班6d.xlsx")

c06_20080707qm_qb_6d <- read_excel("Raw-Data/2006/G2006/2008年7月7日期末全部班6d.xlsx")

c06_20090215jc_6d <- read_excel("Raw-Data/2006/G2006/2009年2月15日测试6d.xlsx")
                      
c06_20090315mn1_6d <- read_excel("Raw-Data/2006/G2006/2009年3月15日一模6d.xlsx")

c06_20090429mn2_6d <- read_excel("Raw-Data/2006/G2006/2009年4月29日二模6d.xlsx")

## Misc ====

c06_md <- read_excel("Raw-Data/2006/G2006/md.xlsx")

c06_testname <- read_excel("Raw-Data/2006/G2006/testname.xlsx")


# Cohort 2007 ####


## Exam Files ====

c07_base <- read_excel("Raw-Data/2007/ZBYZ2007_Demographics&Grades.XLS")

c07_base2 <- read_excel("Raw-Data/2007/ZBYZ2007_Demographics&Grades_2.xlsx")

c07_zkg1gk <- read_excel("Raw-Data/2007/2007级中考高一高考成绩.XLS") 

c07_gk <- read_excel("Raw-Data/2007/2007级高考成绩.XLS")

c07_20071013yk_gkkm <- read_excel("Raw-Data/2007/G2007/2007年10月13日月考高考科目.xlsx")

c07_20071013yk_qbkm <- read_excel("Raw-Data/2007/G2007/2007年10月13日月考全部科目.xlsx")

c07_20071118qz <- read_excel("Raw-Data/2007/G2007/2007年11月18日期中.xlsx")

c07_20071118qz_gkkm <- read_excel("Raw-Data/2007/G2007/2007年11月18日期中高考科目.xlsx")

c07_20071220yk <- read_excel("Raw-Data/2007/G2007/2007年12月20日月考.xlsx")

c07_20080125qm_gkkm <- read_excel("Raw-Data/2007/G2007/2008年1月25日期末高考科目.xlsx")

c07_20080125qm_qbkm <- read_excel("Raw-Data/2007/G2007/2008年1月25日期末全部科目.xlsx")

c07_20080506qz <- read_excel("Raw-Data/2007/G2007/2008年5月6日期中.xlsx")

c07_20080706qm <- read_excel("Raw-Data/2007/G2007/2008年7月6日期末.xlsx")

c07_20081103qz <- read_excel("Raw-Data/2007/G2007/2008年11月3日期中.xlsx")

c07_20090118qm <- read_excel("Raw-Data/2007/G2007/2009年1月18日期末.xlsx")

c07_20090415qz <- read_excel("Raw-Data/2007/G2007/2009年4月15日期中.xlsx")

c07_20090708qm <- read_excel("Raw-Data/2007/G2007/2009年7月8日期末.xlsx")

c07_20091009jc <- read_excel("Raw-Data/2007/G2007/2009年10月9日测试.xlsx")

c07_20091111qz <- read_excel("Raw-Data/2007/G2007/2009年11月11日期中.xlsx")

c07_20091111qz_bxb <- read_excel("Raw-Data/2007/G2007/2009年11月11日期中补习班.xlsx")

c07_20100203qm_wj <- read_excel("Raw-Data/2007/G2007/2010年2月3日期末含往届.xlsx")

c07_20100203qm_yj <- read_excel("Raw-Data/2007/G2007/2010年2月3日期末应届.xlsx")

c07_20100430mn2_wj <- read_excel("Raw-Data/2007/G2007/2010年4月30日二模含往届排名.xlsx")

c07_20100430mn2_yj <- read_excel("Raw-Data/2007/G2007/2010年4月30日二模应届.xlsx")

## 6d Files ====

c07_20071013yk_gkkm_6d <- read_excel("Raw-Data/2007/G2007/2007年10月13日月考高考科目6d.xlsx")

c07_20071013yk_qbkm_6d <- read_excel("Raw-Data/2007/G2007/2007年10月13日月考全部科目6d.xlsx")

c07_20071118qz_6d <- read_excel("Raw-Data/2007/G2007/2007年11月18日期中6d.xlsx")

c07_20071118qz_gkkm_6d <- read_excel("Raw-Data/2007/G2007/2007年11月18日期中高考科目6d.xlsx")

c07_20071220yk_6d <- read_excel("Raw-Data/2007/G2007/2007年12月20日月考6d.xlsx")

c07_20080125qm_gkkm_6d <- read_excel("Raw-Data/2007/G2007/2008年1月25日期末高考科目6d.xlsx")

c07_20080125qm_qbkm_6d <- read_excel("Raw-Data/2007/G2007/2008年1月25日期末全部科目6d.xlsx")

c07_20080506qz_6d <- read_excel("Raw-Data/2007/G2007/2008年5月6日期中6d.xlsx")

c07_20080706qm_6d <- read_excel("Raw-Data/2007/G2007/2008年7月6日期末6D.xlsx")

c07_20081103qz_6d <- read_excel("Raw-Data/2007/G2007/2008年11月3日期中6d.xlsx")

c07_20090118qm_6d <- read_excel("Raw-Data/2007/G2007/2009年1月18日期末6d.xlsx")

c07_20090415qz_6d <- read_excel("Raw-Data/2007/G2007/2009年4月15日期中6d.xlsx")

c07_20090708qm_6d <- read_excel("Raw-Data/2007/G2007/2009年7月8日期末6d.xlsx")

c07_20091009jc_6d <- read_excel("Raw-Data/2007/G2007/2009年10月9日测试6d.xlsx")

c07_20091111qz_6d <- read_excel("Raw-Data/2007/G2007/2009年11月11日期中6d.xlsx")

c07_20100203qm_yj_6d <- read_excel("Raw-Data/2007/G2007/2010年2月3日期末应届6d.xlsx")

c07_20100430mn2_yj_6d <- read_excel("Raw-Data/2007/G2007/2010年4月30日二模应届6d.xlsx")

## Misc ====

c07_md <- read_excel("Raw-Data/2007/G2007/md.xlsx")

c07_md1 <- read_excel("Raw-Data/2007/G2007/md1.xlsx")

c07_md2 <- read_excel("Raw-Data/2007/G2007/md2.xlsx")

c07_testname <- read_excel("Raw-Data/2007/G2007/testname.xlsx")


# Cohort 2008 ####


## Exam Files ====

c08_base <- read_excel("Raw-Data/2008/ZBYZ2008_Demographics&Grades.xlsx")

c08_20081106qz <- read_excel("Raw-Data/2008/G2008/2008年11月6日期中.xlsx")

c08_20090116qm <- read_excel("Raw-Data/2008/G2008/2009年1月16日期末.xlsx")

c08_20090416qz <- read_excel("Raw-Data/2008/G2008/2009年4月16日期中.xlsx")

c08_20090709qm <- read_excel("Raw-Data/2008/G2008/2009年7月9日期末.xlsx")

c08_20091111qz_xzb_dy <- read_excel("Raw-Data/2008/G2008/2009年11月11日期中按行政班分单元排名但语数外年级排名.xlsx")

c08_20091111qz_zb_dy <- read_excel("Raw-Data/2008/G2008/2009年11月11日期中按走班分单元排名但语数外年级排名.xlsx")

c08_20091111qz_xzb <- read_excel("Raw-Data/2008/G2008/2009年11月11日期中行政班.xlsx")

c08_20091111qz_zb_bfwl <- read_excel("Raw-Data/2008/G2008/2009年11月11日期中走班不分文理排名.xlsx")

c08_20091111qz_zb_fwl <- read_excel("Raw-Data/2008/G2008/2009年11月11日期中走班分文理排名.xlsx")

c08_20100201qm_dy <- read_excel("Raw-Data/2008/G2008/2010年2月1日期末成绩按单元排名.xlsx")

c08_20100201qm_dy_2 <- read_excel("Raw-Data/2008/G2008/2010年2月1日期末语英年级排名数文理排名其他单元排名.xlsx")

c08_20100429qz <- read_excel("Raw-Data/2008/G2008/2010年4月29日期中.xlsx")

c08_20100715qm <- read_excel("Raw-Data/2008/G2008/2010年7月15日期末.xlsx")

c08_20101008jc <- read_excel("Raw-Data/2008/G2008/2010年10月8日测试.xlsx")

c08_20101008jc_bxb <- read_excel("Raw-Data/2008/G2008/2010年10月8日测试补习班.xlsx")

c08_20101112qz_bxb <- read_excel("Raw-Data/2008/G2008/2010年11月12日期中含补习班.xlsx")

c08_20101112qz_yj <- read_excel("Raw-Data/2008/G2008/2010年11月12日期中应届.xlsx")

c08_20101222jc_bxb <- read_excel("Raw-Data/2008/G2008/2010年12月22日测试含补习班.xlsx")

c08_20101222jc_yj <- read_excel("Raw-Data/2008/G2008/2010年12月22日测试应届.xlsx")

c08_20110122jc_wj <- read_excel("Raw-Data/2008/G2008/2011年1月22日测试含往届.xlsx")

c08_20110122jc_yj <- read_excel("Raw-Data/2008/G2008/2011年1月22日测试应届.xlsx")

c08_20110318mn1_wj <- read_excel("Raw-Data/2008/G2008/2011年3月18日一模含往届.xlsx")

c08_20110318mn1_yj <- read_excel("Raw-Data/2008/G2008/2011年3月18日一模应届.xlsx")

c08_20110427mn2_wj <- read_excel("Raw-Data/2008/G2008/2011年4月27日二模(含往届).xlsx")

c08_20110427mn2_yj <- read_excel("Raw-Data/2008/G2008/2011年4月27日二模应届.xlsx")

c08_20110528mn3_wj <- read_excel("Raw-Data/2008/G2008/2011年5月28日三模(含往届).xlsx")

c08_20110528mn3_yj <- read_excel("Raw-Data/2008/G2008/2011年5月28日三模应届.xlsx")

## 6d Files ====

c08_20081106qz_6d <- read_excel("Raw-Data/2008/G2008/2008年11月6日期中6d.xlsx")

c08_20090116qm_6d <- read_excel("Raw-Data/2008/G2008/2009年1月16日期末6d.xlsx")

c08_20090416qz_6d <- read_excel("Raw-Data/2008/G2008/2009年4月16日期中6d.xlsx")

c08_20090709qm_6d <- read_excel("Raw-Data/2008/G2008/2009年7月9日期末6d.xlsx")

c08_20100201qm_dy_6d <- read_excel("Raw-Data/2008/G2008/2010年2月1日期末成绩按单元排名6d.xlsx")

c08_20100429qz_6d <- read_excel("Raw-Data/2008/G2008/2010年4月29日期中6d.xlsx")

c08_20100715qm_6d <- read_excel("Raw-Data/2008/G2008/2010年7月15日期末6d.xlsx")

c08_20101008jc_6d <- read_excel("Raw-Data/2008/G2008/2010年10月8日测试6d.xlsx")

c08_20101112qz_yj_6d <- read_excel("Raw-Data/2008/G2008/2010年11月12日期中应届6d.xlsx")

c08_20101222jc_yj_6d <- read_excel("Raw-Data/2008/G2008/2010年12月22日测试应届6d.xlsx")

c08_20110122jc_yj_6d <- read_excel("Raw-Data/2008/G2008/2011年1月22日测试应届6d.xlsx")

c08_20110318mn1_yj_6d <- read_excel("Raw-Data/2008/G2008/2011年3月18日一模应届6d.xlsx")

c08_20110427mn2_yj_6d <- read_excel("Raw-Data/2008/G2008/2011年4月27日二模应届6D.xlsx")

c08_20110528mn3_wj_6d <- read_excel("Raw-Data/2008/G2008/2011年5月28日三模(含往届)6d.xlsx")

c08_20110528mn3_yj_6d <- read_excel("Raw-Data/2008/G2008/2011年5月28日三模应届6d.xlsx")

## Misc ====

c08_md <- read_excel("Raw-Data/2008/G2008/md.xlsx")

c08_testname <- read_excel("Raw-Data/2008/G2008/testname.xlsx")


# Cohort 2009 ####


## Exam Files ====

c09_base <- read_excel("Raw-Data/2009/ZBYZ2009_Demographics&Grades.xlsx")

c09_20091111qz_2p <- read_excel("Raw-Data/2009/G2009/2009年11月11日期中分两部分排名.xlsx")

c09_20091111qz_qnj <- read_excel("Raw-Data/2009/G2009/2009年11月11日期中语数外全年级排名.xlsx")

c09_20100201qm_2p <- read_excel("Raw-Data/2009/G2009/2010年02月1日期末分两部分排名.xlsx")

c09_20100201qm_qnj <- read_excel("Raw-Data/2009/G2009/2010年02月1日期末语数外年级排名.xlsx")

c09_20100304yk <- read_excel("Raw-Data/2009/G2009/2010年03月4日测试.xlsx")

c09_20100430qz <- read_excel("Raw-Data/2009/G2009/2010年04月30日期中.xlsx")

c09_20100715qm <- read_excel("Raw-Data/2009/G2009/2010年07月15日期末.xlsx")

c09_20101015yk <- read_excel("Raw-Data/2009/G2009/2010年10月15日测试.xlsx")

c09_20101112qz_lk <- read_excel("Raw-Data/2009/G2009/2010年11月12日期中理科生.xlsx")

c09_20101112qz_wk <- read_excel("Raw-Data/2009/G2009/2010年11月12日期中文科生.xlsx")

c09_20110122qm_xzb <- read_excel("Raw-Data/2009/G2009/2011年1月22日期末班号为行政班班名次全为行政班.xlsx")

c09_20110122qm_xzb_fwl <- read_excel("Raw-Data/2009/G2009/2011年1月22日期末全部按行政班分文理排名班名次(班号行政班).xlsx")

c09_20110122qm_xzb_dy <- read_excel("Raw-Data/2009/G2009/2011年1月22日期末全部按行政班语英不分科其他分科排名(班号行政班号).xlsx")

c09_20110122qm_zb <- read_excel("Raw-Data/2009/G2009/2011年1月22日期末数物化政历地走班排名其它行政班排名(班号走班号).xlsx")

c09_20110428qz <- read_excel("Raw-Data/2009/G2009/2011年4月28日期中.xlsx")

c09_20110708qm <- read_excel("Raw-Data/2009/G2009/2011年7月8日期末.xlsx")

c09_20111007jc_wj <- read_excel("Raw-Data/2009/G2009/2011年10月7日测试含往届排名.xlsx")

c09_20111007jc_yj <- read_excel("Raw-Data/2009/G2009/2011年10月7日测试应届.xlsx")

c09_20111106qz_wj <- read_excel("Raw-Data/2009/G2009/2011年11月6日期中含往届.xlsx")

c09_20111106qz_yj <- read_excel("Raw-Data/2009/G2009/2011年11月6日期中应届.xlsx")

c09_20111212jc_wj <- read_excel("Raw-Data/2009/G2009/2011年12月12日测试含往届.xlsx")

c09_20111212jc_yj <- read_excel("Raw-Data/2009/G2009/2011年12月12日测试应届.xlsx")

c09_20120113qm_wj_bzh <- read_excel("Raw-Data/2009/G2009/2012年1月13日期末含往届不折合.xlsx")

c09_20120113qm_wj_zh <- read_excel("Raw-Data/2009/G2009/2012年1月13日期末含往届理综文综折合.xlsx")

c09_20120113qm_yj_bzh <- read_excel("Raw-Data/2009/G2009/2012年1月13日期末应届不折合.xlsx")

c09_20120113qm_yj_zh <- read_excel("Raw-Data/2009/G2009/2012年1月13日期末应届理综文综折合.xlsx")

c09_20120209jc <- read_excel("Raw-Data/2009/G2009/2012年2月9日测试.xlsx")

c09_20120209jc_wj <- read_excel("Raw-Data/2009/G2009/2012年2月9日测试含往届.xlsx")

c09_20120303mn1_wj <- read_excel("Raw-Data/2009/G2009/2012年3月3日一模往届.xlsx")

c09_20120303mn1_yj <- read_excel("Raw-Data/2009/G2009/2012年3月3日一模应届.xlsx")

c09_20120303mn1_yj_x <- read_excel("Raw-Data/2009/G2009/2012年3月3日一模应届x.xlsx")

c09_20120303mn1_yj_xx <- read_excel("Raw-Data/2009/G2009/2012年3月3日一模应届xx.xlsx")

c09_20120326jc_yj <- read_excel("Raw-Data/2009/G2009/2012年3月26测试应届.xlsx")

c09_20120326jc_wj <- read_excel("Raw-Data/2009/G2009/2012年3月26日测试往届.xlsx")

c09_20120427mn2_wj <- read_excel("Raw-Data/2009/G2009/2012年4月27日二模含往届.xlsx")

c09_20120427mn2_yj <- read_excel("Raw-Data/2009/G2009/2012年4月27日二模应届.xlsx")

c09_20120527mn3_wj <- read_excel("Raw-Data/2009/G2009/2012年5月27日三模含往届.xlsx")

c09_20120527mn3_yj <- read_excel("Raw-Data/2009/G2009/2012年5月27日三模应届.xlsx")

## 6d Files ====

c09_20091111qz_2p_6d <- read_excel("Raw-Data/2009/G2009/2009年11月11日期中分两部分排名6d.xlsx")

c09_20100201qm_2p_6d <- read_excel("Raw-Data/2009/G2009/2010年02月1日期末分两部分排名6d.xlsx")

c09_20100304yk_6d <- read_excel("Raw-Data/2009/G2009/2010年03月4日测试6d.xlsx")

c09_20100430qz_6d <- read_excel("Raw-Data/2009/G2009/2010年04月30日期中6d.xlsx")

c09_20100715qm_6d <- read_excel("Raw-Data/2009/G2009/2010年07月15日期末6d.xlsx")

c09_20101015yk_6d <- read_excel("Raw-Data/2009/G2009/2010年10月15日测试6d.xlsx")

c09_20101112qz_lk_6d <- read_excel("Raw-Data/2009/G2009/2010年11月12日期中理科生6d.xlsx")

c09_20101112qz_wk_6d <- read_excel("Raw-Data/2009/G2009/2010年11月12日期中文科生6d.xlsx")

c09_20110428qz_6d <- read_excel("Raw-Data/2009/G2009/2011年4月28日期中6d.xlsx")

c09_20110708qm_6d <- read_excel("Raw-Data/2009/G2009/2011年7月8日期末6d.xlsx")

c09_20111007jc_yj_6d <- read_excel("Raw-Data/2009/G2009/2011年10月7日测试应届6d.xlsx")

c09_20111106qz_yj_6d <- read_excel("Raw-Data/2009/G2009/2011年11月6日期中应届6d.xlsx")

c09_20111212jc_yj_6d <- read_excel("Raw-Data/2009/G2009/2011年12月12日测试应届6d.xlsx")

c09_20120113qm_yj_bzh_6d <- read_excel("Raw-Data/2009/G2009/2012年1月13日期末应届不折合6d.xlsx")

c09_20120113qm_yj_zh_6d <- read_excel("Raw-Data/2009/G2009/2012年1月13日期末应届理综文综折合6d.xlsx")

c09_20120209jc_6d <- read_excel("Raw-Data/2009/G2009/2012年2月9日测试6d.xlsx")

c09_20120303mn1_yj_6d <- read_excel("Raw-Data/2009/G2009/2012年3月3日一模应届6d.xlsx")

c09_20120303mn1_yj_x_6d <- read_excel("Raw-Data/2009/G2009/2012年3月3日一模应届6dx.xlsx")

c09_20120303mn1_yj_xx_6d <- read_excel("Raw-Data/2009/G2009/2012年3月3日一模应届6dxx.xlsx")

c09_20120326jc_yj_6d <- read_excel("Raw-Data/2009/G2009/2012年3月26测试应届6d.xlsx")

c09_20120427mn2_yj_6d <- read_excel("Raw-Data/2009/G2009/2012年4月27日二模应届6d.xlsx")

c09_20120527mn3_yj_6d <- read_excel("Raw-Data/2009/G2009/2012年5月27日三模应届6d.xlsx")

## Misc ====

c09_md <- read_excel("Raw-Data/2009/G2009/md.xlsx")

c09_testname <- read_excel("Raw-Data/2009/G2009/testname.xlsx")


# Cohort 2010 ####


## Exam Files ====

c10_base <- read_excel("Raw-Data/2010/ZBYZ2010_Demographics&Grades.xlsx")

c10_gk <- read_excel("Raw-Data/2010/2010级高考成绩.xls", sheet = "Sheet5")

c10_gk_info <- read_excel("Raw-Data/2010/2010级高考报名信息.xls") 

c10_20101110yk <- read_excel("Raw-Data/2010/G2010/2010年11月10日测试.xlsx")

c10_20110122qm <- read_excel("Raw-Data/2010/G2010/2011年1月22日期末.xlsx")

c10_20110428qz <- read_excel("Raw-Data/2010/G2010/2011年4月28日期中.xlsx")

c10_20110708qm <- read_excel("Raw-Data/2010/G2010/2011年7月8日期末.xlsx")

c10_20111104qz <- read_excel("Raw-Data/2010/G2010/2011年11月4日期中.xlsx")

c10_20120113qm <- read_excel("Raw-Data/2010/G2010/2012年1月13日期末.xlsx")

c10_20120419qz <- read_excel("Raw-Data/2010/G2010/2012年4月19日期中.xlsx")

c10_20120706qm <- read_excel("Raw-Data/2010/G2010/2012年7月6日期末.xlsx")

c10_20121009jc <- read_excel("Raw-Data/2010/G2010/2012年10月9日测试.xlsx")

c10_20121009jc_wj <- read_excel("Raw-Data/2010/G2010/2012年10月9日测试含往届排名.xlsx")

c10_20121108jc_wj <- read_excel("Raw-Data/2010/G2010/2012年11月8日测试含往届.xlsx")

c10_20121108jc_yj <- read_excel("Raw-Data/2010/G2010/2012年11月8日测试应届.xlsx")

c10_20121214jc_wj <- read_excel("Raw-Data/2010/G2010/2012年12月14日测试含往届.xlsx")

c10_20121214jc_yj <- read_excel("Raw-Data/2010/G2010/2012年12月14日测试应届.xlsx")

c10_20130125qm_wj <- read_excel("Raw-Data/2010/G2010/2013年1月25日期末含往届.xlsx")

c10_20130125qm_yj <- read_excel("Raw-Data/2010/G2010/2013年1月25日期末应届.xlsx")

c10_20130307mn1 <- read_excel("Raw-Data/2010/G2010/2013年3月7日一模.xlsx")

c10_20130307mn1_wj <- read_excel("Raw-Data/2010/G2010/2013年3月7日一模含往届.xlsx")

c10_20130401yk <- read_excel("Raw-Data/2010/G2010/2013年4月1日月考.xlsx")

c10_20130401yk_wj <- read_excel("Raw-Data/2010/G2010/2013年4月1日月考含往届.xlsx")

c10_20130426mn2_wj <- read_excel("Raw-Data/2010/G2010/2013年4月26日二模（含往届）.xlsx")

c10_20130426mn2_yj <- read_excel("Raw-Data/2010/G2010/2013年4月26日二模应届.xlsx")

c10_20130527mn3_wj <- read_excel("Raw-Data/2010/G2010/2013年5月27日三模（含往届）.xlsx")

c10_20130527mn3 <- read_excel("Raw-Data/2010/G2010/2013年5月27日三模.xlsx")

## 6d Files ====

c10_20101110yk_6d <- read_excel("Raw-Data/2010/G2010/2010年11月10日测试6d.xlsx")

c10_20110122qm_6d <- read_excel("Raw-Data/2010/G2010/2011年1月22日期末6d.xlsx")

c10_20110428qz_6d <- read_excel("Raw-Data/2010/G2010/2011年4月28日期中6d.xlsx")

c10_20110708qm_6d <- read_excel("Raw-Data/2010/G2010/2011年7月8日期末6d.xlsx")

c10_20111104qz_6d <- read_excel("Raw-Data/2010/G2010/2011年11月4日期中6d.xlsx")

c10_20120113qm_6d <- read_excel("Raw-Data/2010/G2010/2012年1月13日期末6d.xlsx")

c10_20120419qz_6d <- read_excel("Raw-Data/2010/G2010/2012年4月19日期中6d.xlsx")

c10_20120706qm_6d <- read_excel("Raw-Data/2010/G2010/2012年7月6日期末6d.xlsx")

c10_20121009jc_6d <- read_excel("Raw-Data/2010/G2010/2012年10月9日测试6d.xlsx")

c10_20121108jc_yj_6d <- read_excel("Raw-Data/2010/G2010/2012年11月8日测试应届6d.xlsx")

c10_20121214jc_yj_6d <- read_excel("Raw-Data/2010/G2010/2012年12月14日测试应届6d.xlsx")

c10_20130125qm_yj_6d <- read_excel("Raw-Data/2010/G2010/2013年1月25日期末应届6d.xlsx")

c10_20130307mn1_6d <- read_excel("Raw-Data/2010/G2010/2013年3月7日一模6d.xlsx")

c10_20130401yk_6d <- read_excel("Raw-Data/2010/G2010/2013年4月1日月考6d.xlsx")

c10_20130426mn2_yj_6d <- read_excel("Raw-Data/2010/G2010/2013年4月26日二模应届6d.xlsx")

c10_20130527mn3_6d <- read_excel("Raw-Data/2010/G2010/2013年5月27日三模6d.xlsx")

## Misc ====

c10_md <- read_excel("Raw-Data/2010/G2010/md.xlsx")

c10_testname <- read_excel("Raw-Data/2010/G2010/testname.xlsx")


# Cohort 2011 ####


## Exam Files ====

c11_base <- read_excel("Raw-Data/2011/ZBYZ2011_Demographics&Grades.xlsx")

c11_gk <- read_excel("Raw-Data/2011/2011级高考成绩.xls")

c11_gk_info <- read_excel("Raw-Data/2011/2011级高考报名.xls")

c11_zk <- read_excel("Raw-Data/2011/G2011/2011年10月入学.xlsx")

c11_20111110qz <- read_excel("Raw-Data/2011/G2011/2011年11月10日期中.xlsx")

c11_20120113qm <- read_excel("Raw-Data/2011/G2011/2012年1月13日期末.xlsx")

c11_20120420qz <- read_excel("Raw-Data/2011/G2011/2012年4月20日期中.xlsx")

c11_20120701qm <- read_excel("Raw-Data/2011/G2011/2012年7月期末.xlsx")

c11_20121015yk_jxb <- read_excel("Raw-Data/2011/G2011/2012年10月15日测试教学班.xlsx")

c11_20121015yk_xzb <- read_excel("Raw-Data/2011/G2011/2012年10月15日测试语英行政班.xlsx")

c11_20121114qz <- read_excel("Raw-Data/2011/G2011/2012年11月14日期中.xlsx")

c11_20121114qz_xzb <- read_excel("Raw-Data/2011/G2011/2012年11月14日期中行政班语英.xlsx")

c11_20130130qm <- read_excel("Raw-Data/2011/G2011/2013年1月30日期末.xlsx")

c11_20130130qm_1 <- read_excel("Raw-Data/2011/G2011/2013年1月30日期末1.xlsx")

c11_20130325yk <- read_excel("Raw-Data/2011/G2011/2013年3月25日测试.xlsx")

c11_20130507qz <- read_excel("Raw-Data/2011/G2011/2013年5月7日期中.xlsx")

c11_20130707qm <- read_excel("Raw-Data/2011/G2011/2013年7月7日期末.xlsx")

c11_20131012jc <- read_excel("Raw-Data/2011/G2011/2013年10月12日测试.xlsx")

c11_20131107qz <- read_excel("Raw-Data/2011/G2011/2013年11月7日期中.xlsx")

c11_20131222jc_zh <- read_excel("Raw-Data/2011/G2011/2013年12月22日测试（综合）.xlsx")

c11_20131222jc <- read_excel("Raw-Data/2011/G2011/2013年12月22日测试.xlsx")

c11_20140117jc <- read_excel("Raw-Data/2011/G2011/2014年1月17日市统考政历地物化生分文理.xlsx")

c11_20140120qm <- read_excel("Raw-Data/2011/G2011/2014年1月20期末.xlsx")

c11_20140306mn1 <- read_excel("Raw-Data/2011/G2011/2014年3月6日一模.xlsx")

c11_20140422mn2 <- read_excel("Raw-Data/2011/G2011/2014年4月44日二模.xlsx")

## 6d Files ====

c11_20111110qz_6d <- read_excel("Raw-Data/2011/G2011/2011年11月10日期中6d.xlsx")

c11_20120113qm_6d <- read_excel("Raw-Data/2011/G2011/2012年1月13日期末6d.xlsx")

c11_20120420qz_6d <- read_excel("Raw-Data/2011/G2011/2012年4月20日期中6d.xlsx")

c11_20120701qm_6d <- read_excel("Raw-Data/2011/G2011/2012年7月期末6d.xlsx")

c11_20121015yk_jxb_6d <- read_excel("Raw-Data/2011/G2011/2012年10月15日测试教学班6d.xlsx")

c11_20121015yk_xzb_6d <- read_excel("Raw-Data/2011/G2011/2012年10月15日测试语英行政班6d.xlsx")

c11_20121114qz_6d <- read_excel("Raw-Data/2011/G2011/2012年11月14日期中6d.xlsx")

c11_20121114qz_xzb_6d <- read_excel("Raw-Data/2011/G2011/2012年11月14日期中行政班语英6d.xlsx")

c11_20130130qm_6d <- read_excel("Raw-Data/2011/G2011/2013年1月30日期末6d.xlsx")

c11_20130130qm_1_6d <- read_excel("Raw-Data/2011/G2011/2013年1月30日期末6d1.xlsx")

c11_20130325yk_6d <- read_excel("Raw-Data/2011/G2011/2013年3月25日测试6d.xlsx")

c11_20130507qz_6d <- read_excel("Raw-Data/2011/G2011/2013年5月7日期中6d.xlsx")

c11_20130707qm_6d <- read_excel("Raw-Data/2011/G2011/2013年7月7日期末6d.xlsx")

c11_20131012jc_6d <- read_excel("Raw-Data/2011/G2011/2013年10月12日测试6d.xlsx")

c11_20131107qz_6d <- read_excel("Raw-Data/2011/G2011/2013年11月7日期中6d.xlsx")

c11_20131222jc_6d <- read_excel("Raw-Data/2011/G2011/2013年12月22日测试6d.xlsx")

c11_20140120qm_6d <- read_excel("Raw-Data/2011/G2011/2014年1月20期末6d.xlsx")

## Misc ====

c11_md <- read_excel("Raw-Data/2011/G2011/md.xlsx")

c11_testname <- read_excel("Raw-Data/2011/G2011/testname.xlsx")

c11_testname_a <- read_excel("Raw-Data/2011/G2011/testname 的副本.xlsx")


# Cohort 2012 ####


## Exam Files ====

c12_base <- read_excel("Raw-Data/2012/ZBYZ2012_Demographics&Grades.xlsx")

c12_gk <- read_excel("Raw-Data/2012/2012级高考成绩.XLS")

c12_gk_info <- read_excel("Raw-Data/2012/2012带高考考号.xlsx")

c12_20121011yk <- read_excel("Raw-Data/2012/G2012/2012年10月11日入学测验.xlsx")

c12_20121115qz <- read_excel("Raw-Data/2012/G2012/2012年11月15日期中.xlsx")

c12_20130105yk <- read_excel("Raw-Data/2012/G2012/2013年1月5日测试.xlsx")

c12_20130130qm <- read_excel("Raw-Data/2012/G2012/2013年1月30日期末.xlsx")

c12_20130407yk <- read_excel("Raw-Data/2012/G2012/2013年4月7日测试.xlsx")

c12_20130504qz <- read_excel("Raw-Data/2012/G2012/2013年5月4日期中.xlsx")

c12_20130620yk <- read_excel("Raw-Data/2012/G2012/2013年6月20日测试.xlsx")

c12_20130707qm_3k <- read_excel("Raw-Data/2012/G2012/2013年7月7日期末语数外三科.xlsx")

c12_20130708qm <- read_excel("Raw-Data/2012/G2012/2013年7月8日期末.xlsx")

c12_20131008yk <- read_excel("Raw-Data/2012/G2012/2013年10月8日测试.xlsx")

c12_20131112qz <- read_excel("Raw-Data/2012/G2012/2013年11月12日期中.xlsx")

c12_20140115qm <- read_excel("Raw-Data/2012/G2012/2014年1月15日期末.xlsx")

c12_20140218yk <- read_excel("Raw-Data/2012/G2012/2014年2月18日开学测试.xlsx")

c12_20140320yk <- read_excel("Raw-Data/2012/G2012/2014年3月20日测试.xlsx")

c12_20140320yk_5d <- read_excel("Raw-Data/2012/G2012/2014年3月20日测试五段.xlsx")

c12_20140422qz <- read_excel("Raw-Data/2012/G2012/2014年4月22日期中.xlsx")
                             
c12_20140528yk <- read_excel("Raw-Data/2012/G2012/2014年5月28日测试.xlsx")

c12_20140708qm <- read_excel("Raw-Data/2012/G2012/2014年7月8日期末.xlsx")

c12_20140901jc <- read_excel("Raw-Data/2012/G2012/2014年9月1日测试.xlsx")

c12_20141006jc <- read_excel("Raw-Data/2012/G2012/2014年10月6日测试.xlsx")

c12_20141006jc_x <- read_excel("Raw-Data/2012/G2012/2014年10月6日测试x.xlsx")

c12_20141112qz <- read_excel("Raw-Data/2012/G2012/2014年11月12日期中.xlsx")

c12_20141211jc <- read_excel("Raw-Data/2012/G2012/2014年12月11日测试.xlsx")

c12_20150108jc <- read_excel("Raw-Data/2012/G2012/2015年1月8日摸底考试.xlsx")

c12_20150202qm_a <- read_excel("Raw-Data/2012/G2012/2015年2月2日期末潍坊题.xlsx")

c12_20150202qm_b <- read_excel("Raw-Data/2012/G2012/2015年2月2日期末有综合.xlsx")

c12_20150315mn1 <- read_excel("Raw-Data/2012/G2012/2015年3月15日一模.xlsx")

c12_20150413jc <- read_excel("Raw-Data/2012/G2012/2015年4月13日测试.xlsx")

c12_20150506mn2 <- read_excel("Raw-Data/2012/G2012/2015年5月6日二模.xlsx")

c12_20150526mn3 <- read_excel("Raw-Data/2012/G2012/2015年5月26日三模.xlsx")

## 6d Files ====

c12_20121011yk_6d <- read_excel("Raw-Data/2012/G2012/2012年10月11日入学测验6d.xlsx")

c12_20121115qz_6d <- read_excel("Raw-Data/2012/G2012/2012年11月15日期中6d.xlsx")

c12_20130105yk_6d <- read_excel("Raw-Data/2012/G2012/2013年1月5日测试6d.xlsx")

c12_20130130qm_6d <- read_excel("Raw-Data/2012/G2012/2013年1月30日期末6d.xlsx")

c12_20130407yk_6d <- read_excel("Raw-Data/2012/G2012/2013年4月7日测试6d.xlsx")

c12_20130504qz_6d <- read_excel("Raw-Data/2012/G2012/2013年5月4日期中6d.xlsx")

c12_20130707qm_3k_6d <- read_excel("Raw-Data/2012/G2012/2013年7月7日期末语数外三科6d.xlsx")

c12_20131008yk_6d <- read_excel("Raw-Data/2012/G2012/2013年10月8日测试6d.xlsx")

c12_20131112qz_6d <- read_excel("Raw-Data/2012/G2012/2013年11月12日期中6d.xlsx")

c12_20140115qm_6d <- read_excel("Raw-Data/2012/G2012/2014年1月15日期末6d.xlsx")

c12_20140218yk_6d <- read_excel("Raw-Data/2012/G2012/2014年2月18日开学测试6d.xlsx")

c12_20140320yk_6d <- read_excel("Raw-Data/2012/G2012/2014年3月20日测试6d.xlsx")

c12_20140320yk_5d_6d <- read_excel("Raw-Data/2012/G2012/2014年3月20日测试五段6d.xlsx")

c12_20140422qz_6d <- read_excel("Raw-Data/2012/G2012/2014年4月22日期中6d.xlsx")

c12_20140528yk_6d <- read_excel("Raw-Data/2012/G2012/2014年5月28日测试6d.xlsx")

c12_20140708qm_6d <- read_excel("Raw-Data/2012/G2012/2014年7月8日期末6d.xlsx")

c12_20140901jc_6d <- read_excel("Raw-Data/2012/G2012/2014年9月1日测试6d.xlsx")

c12_20141006jc_6d <- read_excel("Raw-Data/2012/G2012/2014年10月6日测试6d.xlsx")

c12_20141112qz_6d <- read_excel("Raw-Data/2012/G2012/2014年11月12日期中6d.xlsx")

c12_20141211jc_6d <- read_excel("Raw-Data/2012/G2012/2014年12月11日测试6d.xlsx")

c12_20150108jc_6d <- read_excel("Raw-Data/2012/G2012/2015年1月8日摸底考试6d.xlsx")

c12_20150315mn1_6d <- read_excel("Raw-Data/2012/G2012/2015年3月15日一模6d.xlsx")

c12_20150413jc_6d <- read_excel("Raw-Data/2012/G2012/2015年4月13日测试6d.xlsx")

c12_20150506mn2_6d <- read_excel("Raw-Data/2012/G2012/2015年5月6日二模6d.xlsx")

## Misc ====

c12_md <- read_excel("Raw-Data/2012/G2012/md.xlsx")

c12_testname <- read_excel("Raw-Data/2012/G2012/testname.xlsx")

c12_testname_a <- read_excel("Raw-Data/2012/G2012/testname 的副本.xlsx")

c12_testname_2 <- read_excel("Raw-Data/2012/G2012/testname2.xlsx")

c12_testname_2_a <- read_excel("Raw-Data/2012/G2012/testname又副本.xlsx")


# Cohort 2013 ####


## Exam Files ====

c13_base <- read_excel("Raw-Data/2013/ZBYZ2013_Demographics&Grades.xlsx")

c13_gk <- read_excel("Raw-Data/2013/2013级高考成绩.xlsx")

c13_20131012yk <- read_excel("Raw-Data/2013/G2013/2013年10月12日测试.xlsx")

c13_20131105qz <- read_excel("Raw-Data/2013/G2013/2013年11月05日期中.xlsx")

c13_20140120qm <- read_excel("Raw-Data/2013/G2013/2014年1月20日期末.xlsx")

c13_20140220yk <- read_excel("Raw-Data/2013/G2013/2014年2月20日开学测试.xlsx")

c13_20140420qz <- read_excel("Raw-Data/2013/G2013/2014年4月20日期中.xlsx")

c13_20140526yk <- read_excel("Raw-Data/2013/G2013/2014年5月26日六科测试.xlsx")

c13_20140703qm <- read_excel("Raw-Data/2013/G2013/2014年7月3日期末.xlsx")

c13_20140912yk <- read_excel("Raw-Data/2013/G2013/2014年9月12日开学测试.xlsx")

c13_20141008yk <- read_excel("Raw-Data/2013/G2013/2014年10月8日测试.xlsx")

c13_20141117qz <- read_excel("Raw-Data/2013/G2013/2014年11月17日期中.xlsx")

c13_20150104yk <- read_excel("Raw-Data/2013/G2013/2015年1月4日测试.xlsx")

c13_20150201qm <- read_excel("Raw-Data/2013/G2013/2015年2月期末.xlsx")

c13_20150307yk <- read_excel("Raw-Data/2013/G2013/2015年3月7日开学测试.xlsx")

c13_20150401yk <- read_excel("Raw-Data/2013/G2013/2015年4月测试.xlsx")

c13_20150508qz <- read_excel("Raw-Data/2013/G2013/2015年5月8日期中.xlsx")

c13_20150707qm <- read_excel("Raw-Data/2013/G2013/2015年7月7日期末.xlsx")

c13_20150928jc <- read_excel("Raw-Data/2013/G2013/2015年9月28日测试.xlsx")

c13_20151102qz <- read_excel("Raw-Data/2013/G2013/2015年11月2日期中.xlsx")

c13_20151217jc <- read_excel("Raw-Data/2013/G2013/2015年12月17日摸底考试.xlsx")

c13_20160128qm <- read_excel("Raw-Data/2013/G2013/2016年1月28日期末.xlsx")

c13_20160223jc <- read_excel("Raw-Data/2013/G2013/2016年2月23日开学联考.xlsx")

c13_20160306mn1 <- read_excel("Raw-Data/2013/G2013/2016年3月6日一模.xlsx")

c13_20160403jc <- read_excel("Raw-Data/2013/G2013/2016年4月3日测试.xlsx")

c13_20160504mn2 <- read_excel("Raw-Data/2013/G2013/2016年5月4日二模.xlsx")

## 6d Files ====

c13_20131012yk_6d <- read_excel("Raw-Data/2013/G2013/2013年10月12日测试6d.xlsx")

c13_20131105qz_6d <- read_excel("Raw-Data/2013/G2013/2013年11月05日期中6d.xlsx")

c13_20140120qm_6d <- read_excel("Raw-Data/2013/G2013/2014年1月20日期末6d.xlsx")

c13_20140220yk_6d <- read_excel("Raw-Data/2013/G2013/2014年2月20日开学测试6d.xlsx")

c13_20140420qz_6d <- read_excel("Raw-Data/2013/G2013/2014年4月20日期中6d.xlsx")

c13_20140526yk_6d <- read_excel("Raw-Data/2013/G2013/2014年5月26日六科测试6D.xlsx")

c13_20140703qm_6d <- read_excel("Raw-Data/2013/G2013/2014年7月3日期末6d.xlsx")

c13_20140912yk_6d <- read_excel("Raw-Data/2013/G2013/2014年9月12日开学测试6d.xlsx")

c13_20141008yk_6d <- read_excel("Raw-Data/2013/G2013/2014年10月8日测试6d.xlsx")

c13_20141117qz_6d <- read_excel("Raw-Data/2013/G2013/2014年11月17日期中6d.xlsx")

c13_20150104yk_6d <- read_excel("Raw-Data/2013/G2013/2015年1月4日测试6d.xlsx")

c13_20150201qm_6d <- read_excel("Raw-Data/2013/G2013/2015年2月期末6d.xlsx")

c13_20150307yk_6d <- read_excel("Raw-Data/2013/G2013/2015年3月7日开学测试6d.xlsx")

c13_20150401yk_6d <- read_excel("Raw-Data/2013/G2013/2015年4月测试6d.xlsx")

c13_20150508qz_6d <- read_excel("Raw-Data/2013/G2013/2015年5月8日期中6d.xlsx")

c13_20150707qm_6d <- read_excel("Raw-Data/2013/G2013/2015年7月7日期末6d.xlsx")

c13_20150928jc_6d <- read_excel("Raw-Data/2013/G2013/2015年9月28日测试6d.xlsx")

c13_20151102qz_6d <- read_excel("Raw-Data/2013/G2013/2015年11月2日期中6d.xlsx")

c13_20151217jc_6d <- read_excel("Raw-Data/2013/G2013/2015年12月17日摸底考试6d.xlsx")

c13_20160128qm_6d <- read_excel("Raw-Data/2013/G2013/2016年1月28日期末6d.xlsx")

c13_20160223jc_6d <- read_excel("Raw-Data/2013/G2013/2016年2月23日开学联考6d.xlsx")

c13_20160403jc_6d <- read_excel("Raw-Data/2013/G2013/2016年4月3日测试6d.xlsx")

## Misc ====

c13_md <- read_excel("Raw-Data/2013/G2013/md.xlsx")

c13_testname <- read_excel("Raw-Data/2013/G2013/testname.xlsx")


# Cohort 2014 ####


## Exam Files ====

c14_base <- read_excel("Raw-Data/2014/ZBYZ2014_Demographics&Grades.xlsx")

c14_gk <- read_excel("Raw-Data/2014/2014级高考成绩.xls") 

c14_zk <- read_excel("Raw-Data/2014/G2014/2014年9月入学成绩.xlsx")

c14_20141012yk <- read_excel("Raw-Data/2014/G2014/2014年10月12日测试.xlsx")

c14_20141121qz <- read_excel("Raw-Data/2014/G2014/2014年11月21日期中.xlsx")

c14_20150105yk <- read_excel("Raw-Data/2014/G2014/2015年1月5日测试.xlsx")

c14_20150205qm <- read_excel("Raw-Data/2014/G2014/2015年2月5日期末.xlsx")

c14_20150305yk <- read_excel("Raw-Data/2014/G2014/2015年3月5日开学测试.xlsx")

c14_20150427qz <- read_excel("Raw-Data/2014/G2014/2015年4月27日期中.xlsx")

c14_20150612yk <- read_excel("Raw-Data/2014/G2014/2015年6月12日测试.xlsx")

c14_20150705qm <- read_excel("Raw-Data/2014/G2014/2015年7月5日期末.xlsx")

c14_20150901yk <- read_excel("Raw-Data/2014/G2014/2015年9月1日开学测试.xlsx")

c14_20151009yk <- read_excel("Raw-Data/2014/G2014/2015年10月9日测试.xlsx")

c14_20151112qz <- read_excel("Raw-Data/2014/G2014/2015年11月12日期中.xlsx")

c14_20160127qm <- read_excel("Raw-Data/2014/G2014/2016年1月27日期末.xlsx")

c14_20160403yk <- read_excel("Raw-Data/2014/G2014/2016年4月3日测试.xlsx")

c14_20160510qz <- read_excel("Raw-Data/2014/G2014/2016年5月10日期中.xlsx")

c14_20160607qm <- read_excel("Raw-Data/2014/G2014/2016年6月7日期末.xlsx")

c14_20160830jc <- read_excel("Raw-Data/2014/G2014/2016年8月30日开学测试.xlsx")

c14_20161008jc <- read_excel("Raw-Data/2014/G2014/2016年10月8日测试.xlsx")

c14_20161116qz <- read_excel("Raw-Data/2014/G2014/2016年11月16日期中.xlsx")

c14_20161215jc <- read_excel("Raw-Data/2014/G2014/2016年12月15日摸底.xlsx")

c14_20170114qm <- read_excel("Raw-Data/2014/G2014/2017年1月14日期末.xlsx")

c14_20170211jc <- read_excel("Raw-Data/2014/G2014/2017年2月11日开学测试.xlsx")

c14_20170305mn1 <- read_excel("Raw-Data/2014/G2014/2017年3月5日一模.xlsx")

c14_20170401jc <- read_excel("Raw-Data/2014/G2014/2017年4月1日测试.xlsx")

c14_20170505mn2 <- read_excel("Raw-Data/2014/G2014/2017年5月5日二模.xlsx")

c14_20170525mn3 <- read_excel("Raw-Data/2014/G2014/2017年5月25日三模.xlsx")

## 6d Files ====

c14_20141012yk_6d <- read_excel("Raw-Data/2014/G2014/2014年10月12日测试6d.xlsx")

c14_20141121qz_6d <- read_excel("Raw-Data/2014/G2014/2014年11月21日期中6D.xlsx")

c14_20150105yk_6d <- read_excel("Raw-Data/2014/G2014/2015年1月5日测试6d.xlsx")

c14_20150205qm_6d <- read_excel("Raw-Data/2014/G2014/2015年2月5日期末6d.xlsx")

c14_20150305yk_6d <- read_excel("Raw-Data/2014/G2014/2015年3月5日开学测试6d.xlsx")

c14_20150427qz_6d <- read_excel("Raw-Data/2014/G2014/2015年4月27日期中6d.xlsx")

c14_20150612yk_6d <- read_excel("Raw-Data/2014/G2014/2015年6月12日测试6d.xlsx")

c14_20150705qm_6d <- read_excel("Raw-Data/2014/G2014/2015年7月5日期末6d.xlsx")

c14_20150901yk_6d <- read_excel("Raw-Data/2014/G2014/2015年9月1日开学测试6d.xlsx")

c14_20151009yk_6d <- read_excel("Raw-Data/2014/G2014/2015年10月9日测试6d.xlsx")

c14_20151112qz_6d <- read_excel("Raw-Data/2014/G2014/2015年11月12日期中6d.xlsx")

c14_20160127qm_6d <- read_excel("Raw-Data/2014/G2014/2016年1月27日期末6d.xlsx")

c14_20160403yk_6d <- read_excel("Raw-Data/2014/G2014/2016年4月3日测试6d.xlsx")

c14_20160510qz_6d <- read_excel("Raw-Data/2014/G2014/2016年5月10日期中6d.xlsx")

c14_20160607qm_6d <- read_excel("Raw-Data/2014/G2014/2016年6月7日期末6d.xlsx")

c14_20160830jc_6d <- read_excel("Raw-Data/2014/G2014/2016年8月30日开学测试6d.xlsx")

c14_20161008jc_6d <- read_excel("Raw-Data/2014/G2014/2016年10月8日测试6d.xlsx")

c14_20161116qz_6d <- read_excel("Raw-Data/2014/G2014/2016年11月16日期中6d.xlsx")

c14_20161215jc_6d <- read_excel("Raw-Data/2014/G2014/2016年12月15日摸底6D.xlsx")

c14_20170114qm_6d <- read_excel("Raw-Data/2014/G2014/2017年1月14日期末6d.xlsx")

c14_20170211jc_6d <- read_excel("Raw-Data/2014/G2014/2017年2月11日开学测试6D.xlsx")

c14_20170305mn1_6d <- read_excel("Raw-Data/2014/G2014/2017年3月5日一模6d.xlsx")

c14_20170401jc_6d <- read_excel("Raw-Data/2014/G2014/2017年4月1日测试6d.xlsx")

c14_20170505mn2_6d <- read_excel("Raw-Data/2014/G2014/2017年5月5日二模6d.xlsx")

c14_20170525mn3_6d <- read_excel("Raw-Data/2014/G2014/2017年5月25日三模6d.xlsx")

## Misc ====

c14_md <- read_excel("Raw-Data/2014/G2014/md.xlsx")

c14_testname <- read_excel("Raw-Data/2014/G2014/testname.xlsx")


# Processing ####


# Combine c04_20050128qm_a and c04_20050125qm_b (Footnote 1)
c04_20050128qm <- bind_rows(c04_20050128qm_a, c04_20050128qm_b)

# Add Ability score to c05_20070701qm_hnl (Footnote 7)
c05_20070701qm_hnl$能力 <- c05_20070701qm_hnl$总成绩 - c05_20070701qm_bhnl$总成绩

# Combine c09_20101112_qz_lk and c09_20101112_qz_wk (Footnote 15)
c09_20101112qz <- bind_rows(c09_20101112qz_lk, c09_20101112qz_wk)


# Three pairs of students share identical `zcxjh` in c10_gk
c10_gk %>%
  filter(
    duplicated(zcxjh) | duplicated(zcxjh, fromLast = TRUE), !is.na(zcxjh) 
  ) %>% 
  # Extract XJH, name, and father's name
  select(zcxjh, 姓名, jtcy1_xm)

# zcxjh               姓名   jtcy1_xm
# <chr>               <chr>  <chr>   
# 1 2010370301000130966 孙康   孙凤林  
# 2 2010370301000130966 常嘉琪 常建交  
# 3 2010370301000130112 王晞   王忠    
# 4 2010370301000130112 苏天宇 苏同伟  
# 5 2010370301000130853 王文烨 王维刚  
# 6 2010370301000130853 刘阳   刘绪枝

# Extract these students' XJH from c10_base
c10_base %>% 
  filter(
    str_detect(xm, "孙康|常嘉琪|王晞|苏天宇|王文烨|刘阳"), 
    str_detect(父姓名mzk, "孙凤林|常建交|王忠|苏同伟|王维刚|刘绪枝")
  ) %>% 
  select(zcxh, xm, 父姓名mzk)

# zcxh                xm        父姓名mzk
# <chr>               <chr>     <chr>    
# 1 2010370301001030112 苏天宇027 苏同伟200
# 2 2010370301000130112 王晞65    王忠52   
# 3 2010370301000130583 刘阳56    刘绪枝200
# 4 2010370301000130853 王文烨527 王维刚200
# 5 2010370301000130966 孙康38    孙凤林200
# 6 2010370301000130971 常嘉琪 07 常建交200

# Replace correct XJH value in c10_gk, notice that the `zcxjh` column is `character`
c10_gk$zcxjh[c10_gk$姓名 == "苏天宇" & c10_gk$jtcy1_xm == "苏同伟"] <- "2010370301001030112"
c10_gk$zcxjh[c10_gk$姓名 == "刘阳" & c10_gk$jtcy1_xm == "刘绪枝"] <- "2010370301000130583"
c10_gk$zcxjh[c10_gk$姓名 == "常嘉琪" & c10_gk$jtcy1_xm == "常建交"] <- "2010370301000130971" 


# Save to .RData ####


save.image("Raw-Data.RData")

