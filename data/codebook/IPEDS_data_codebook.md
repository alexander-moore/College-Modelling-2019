IPEDS\_data\_codebook
================
Alexander Moore
February 15, 2019

Variable Codebook for IPEDS Dataset
===================================

EJ Arce February 14, 2019

-   unitid (character ) -&gt; school\_id
    -   unique, six-digit ID number for each institution in the survey.
-   instnm (character) -&gt; school\_name
    -   The name of each institution and, if applicable, the location of the campus.
-   stabbr (character) -&gt; state
    -   State abbreviation for the institution's location
-   hloffer (factor) -&gt; highest\_degree
    -   Highest level of degree offered.
        -   NA = Not Available
        -   1 = Postsecondary award, certificate or diploma of less than one academic year
        -   2 = Postsecondary award, certificate or diploma of at least one but less than two academic years
        -   3 = Associate's degree
        -   4 = Postsecondary award, certificate or diploma of at least two but less than four academic years
        -   5 = Bachelor's degree
        -   6 = Postbaccalaureate certificate
        -   7 = Master's degree
        -   8 = Post-master's certificate
        -   9 = Doctor's degree

1.  locale (factor) -&gt; urban
    -   Degree of urbanization ranging from “large city” to “rural.” From IPEDS: "They are based on a school’s physical address. The urban-centric locale codes introduced in this file are assigned through a methodology developed by the U.S. Census Bureau’s Population Division in 2005. The urban-centric locale codes apply current geographic concepts to the original NCES locale codes used on IPEDS files through 2004. American Samoa, the Commonwealth of the Northern Mariana Islands, Guam, and the Virgin Islands, were not assigned a locale code because the geographic and governmental structures of these entities do not fit the definitional scheme used to derive the code."
        -   11 = City: Large: Territory inside an urbanized area and inside a principal city with population of 250,000 or more.
        -   12 = City: Midsize: Territory inside an urbanized area and inside a principal city with population less than 250,000 and greater than or equal to 100,000.
        -   13 = City: Small: Territory inside an urbanized area and inside a principal city with population less than 100,000.
        -   21 = Suburb: Large: Territory outside a principal city and inside an urbanized area with population of 250,000 or more.
        -   22 = Suburb: Midsize: Territory outside a principal city and inside an urbanized area with population less than 250,000 and greater than or equal to 100,000.
        -   23 = Suburb: Small: Territory outside a principal city and inside an urbanized area with population less than 100,000.
        -   31 = Town: Fringe: Territory inside an urban cluster that is less than or equal to 10 miles from an urbanized area.
        -   32 = Town: Distant: Territory inside an urban cluster that is more than 10 miles and less than or equal to 35 miles from an urbanized area.
        -   33 = Town: Remote: Territory inside an urban cluster that is more than 35 miles of an urbanized area.
        -   41 - Rural: Fringe: Census-defined rural territory that is less than or equal to 5 miles from an urbanized area, as well as rural territory that is less than or equal to 2.5 miles from an urban cluster.
        -   42 = Rural: Distant: Census-defined rural territory that is more than 5 miles but less than or equal to 25 miles from an urbanized area, as well as rural territory that is more than 2.5 miles but less than or equal to 10 miles from an urban cluster.
        -   43 = Rural: Remote: Census-defined rural territory that is more than 25 miles from an urbanized area and is also more than 10 miles from an urban cluster.
2.  instsize (factor) -&gt; school\_size
    -   A categorical variable that is ordered:
        -   1 = Under 1,000
        -   2 = 1,000 - 4,999
        -   3 = 5,000 - 9,999
        -   4 = 10,000 - 19,999
        -   5 = 20,000 and above
3.  longitud (numeric) -&gt; longitude
    -   Longitude number for institution's location.
4.  latitude (numeric)
    -   Lattitude number for institution's location.
5.  relaffil (factor) -&gt; rel\_affil
    -   Institution's religious affilation. Data was originally categorical with many different religions, but has been recoded as Yes for schools that have a religious affiliation and No for schools that don't, with one NA for a school that did not report status.
6.  ft\_ug (logical) -&gt; full\_time\_undergrad
    -   Indicator variable for if institution enrolls full-time undergraduate students.
        -   1 = Yes
        -   0 = No
7.  ftgdnidp (logical) -&gt; part\_time\_grad
    -   Indicator variable for if institution enrolls part-time graduate students.
        -   1 = Yes
        -   0 = No
8.  pt\_ug (logical) -&gt; part\_time\_undergrad
    -   Indicator variable for if institution enrolls part-time undergraduate students.
        -   1 = Yes
        -   0 = No
9.  ptgdnidp (logical) -&gt; part\_time\_grad\_prof
    -   Indicator variable for if institution enrolls part-time graduate (not including doctor's professional practice) students.
        -   1 = Yes
        -   0 = No
10. roomcap (numeric) -&gt; dorm\_capacity
    -   Total dorm capacity. A lot of missingness.
11. rmbrdamt (numeric) -&gt; room\_board
    -   Combined charge for room and board. Also a lot of missingness.
12. athassoc (logical) -&gt; NAA\_member
    -   Categorical variable for National Athletic Association membership status.
        -   1 = Yes
        -   0 = No
13. assoc1 (logical) -&gt; NCAA\_member
    -   Categorical variable for National Collegiate Athletic Association membership status.
        -   1 = Yes
        -   0 = No
14. tuition2 (numeric) -&gt; in\_state\_tuition
    -   Average in-state tuition for full-time undergraduate students. About 10% missing.
15. tuition3 (numeric) -&gt; out\_state\_tuition
    -   Average out-of-state tuition for full-time undergraduate students. Same cases missing as tuition2.
16. prgmofr (numeric) -&gt; num\_programs
    -   Number of programs offered. Ranges from 1-103 and has NA values for over half of their entries.
17. ciptuit1 (character) -&gt; tuit\_fees
    -   Tuitions and fees for institutions with no full-time, first-time, undergraduate students. Same missingness as prgmofr.
18. eftotlt (numeric) -&gt; total\_enrolled
    -   Total students enrolled.
19. eftotlm (numeric) -&gt; male\_enrolled
    -   Total male students enrolled.
20. eftotlw (numeric) -&gt; female\_enrolled
    -   Total female students enrolled.
21. efaiant (numeric) -&gt; native\_enrolled
    -   Total American Indian or Alaska Native students enrolled.
22. efasiat (numeric) -&gt; asian\_enrolled
    -   Total Asian students enrolled.
23. efbkaat (numeric) -&gt; black\_enrolled
    -   Total Black or African American students enrolled.
24. efhispt (numeric) -&gt; hispanic\_enrolled
    -   Total Hispanic students enrolled.
25. efnhpit (numeric) -&gt; pacific\_enrolled
    -   Total Native Hawaiian or Other Pacific Islander students enrolled.
26. efwhitt (numeric) -&gt; white\_enrolled
    -   Total White students enrolled.
27. ef2mort (numeric) -&gt; mixed\_enrolled
    -   Total mixed race (2 or more) students enrolled.
28. efnralt (numeric) -&gt; alien\_enrolled
    -   Total nonresident alien students enrolled.
29. ret\_pcf (numeric) -&gt; retention\_rate
    -   Full time retention rate. Ranges from 0-100.
30. stufacr (numeric) -&gt; student\_faculty\_ratio
    -   Student:faculty ratio. Ranges from 1 to 142
31. sistotl (numeric) -&gt; total\_faculty
    -   Total number of faculty.
32. sisprof (numeric)
    -   Total number of professors.
33. sisascp (numeric)
    -   Total number of associate professors.
34. sisastp (numeric)
    -   Total number of assistant professors.
35. sisinst (numeric)
    -   Total number of instructors.
36. sislect (numeric)
    -   Total number of lecturers.
37. scfa13p (numeric) -&gt; percent\_out\_state
    -   Percentage of students who are paying out-of-state tuition. Ranges from 0-80.
38. scfy2 (numeric) -&gt; total\_undergrad
    -   Total number of undergraduate students at the institution. **I propose this be our size indicator**
39. scfy1n (numeric)
    -   Total number of full time, undergraduate, degree-seeking students at the institution.
40. scfy1p (numeric) -&gt; pct\_ft\_undergrad
    -   Percentage of total students who are full time, undergraduate, degree-seeking students.
41. uagrntp (numeric) -&gt; pct\_fin\_aid
    -   Percentage of undergraduate students awarded federal, state, local, institutional or other sources of grant aid. Ranges from 0-100.
42. uagrnta (numeric) -&gt; avg\_fin\_aid
    -   Average amount of federal, state, local, institutional or other sources of grant aid granded to undergraduate students. Ranges from 250-53290.
43. control (factor)
    -   A public/private non-profit/private for-profit classifier for institutions
        -   1 Public
        -   2 Private not-for-profit
        -   3 Private for-profit
        -   -3 {Not available}
44. carnegie (factor)
    -   A variable classifying colleges as:
        -   15 Doctoral/Research Universities--Extensive
        -   16 Doctoral/Research Universities--Intensive
        -   21 Masters Colleges and Universities I
        -   22 Masters Colleges and Universities II
        -   31 Baccalaureate Colleges--Liberal Arts
        -   32 Baccalaureate Colleges--General
        -   33 Baccalaureate/Associates Colleges
        -   40 Associates Colleges
        -   51 Theological seminaries and other specialized faith-related institutions
        -   52 Medical schools and medical centers
        -   53 Other separate health profession schools
        -   54 Schools of engineering and technology
        -   55 Schools of business and management
        -   56 Schools of art, music, and design
        -   57 Schools of law
        -   58 Teachers colleges
        -   59 Other specialized institutions
        -   60 Tribal colleges
        -   -3 {Item not available}
45. grtotlt (numeric)
    -   Grand total of students who graduated in previous spring semester.
46. grtotlm (numeric)
    -   Grand total of male students who graduated in previous spring semester.
47. grtotlw (numeric)
    -   Grand total of female students who graduated in previous spring semester.
48. applcn (numeric)
    -   Total number of applicants to the institution.
49. admssn (numeric)
    -   Total number of students admitted to the institution.
50. enrlt (numeric) -&gt; first\_year\_enrolled
    -   Total number of enrolled first-year students.
51. actcm25 (numeric) -&gt; act\_25
    -   ACT composite 25th perentile score. Schools that submitted SAT scores had those entries converted to the ACT equivalent.
52. actcm75 (numeric) -&gt; act\_75
    -   ACT composite 25th perentile score. Schools that submitted SAT scores had those entries converted to the ACT equivalent.
53. ptotal (numeric) -&gt; total\_programs
    -   Total number of programs offered by the institution
54. passoc (numeric) -&gt; total\_assoc
    -   Total numer of Associate's degree programs offered
55. pbachl (numeric) -&gt; total\_bachelor
    -   Total numer of Bachelor's degree programs offered
56. pmastr (numeric) -&gt; total\_masters
    -   Total numer of Master's degree programs offered
57. pdocrs (numeric) -&gt; total\_doc\_research
    -   Total numer of Doctor's degree-research/scholarship programs offered
58. pdocpp (numeric) -&gt; total\_doc\_prof
    -   Total numer of Doctor's degree-professional practice programs offered

------------------------------------------------------------------------

1.  gr\_prop\_male (numeric)
    -   Proportion of male student graduates last spring semester. (grtotlm / grtotltl)
2.  gr\_prop\_fem (numeric)
    -   Proportion of female student graduates last spring semester. (grtotlw / grtotltl)
3.  prop\_admit (numeric)
    -   Proportion of admitted students from applied students. (admssn / applcn)
4.  prof\_total (numeric)
    -   sisprof + sisascp + sisastp **Temporary info to be dropped. used in proportions**
5.  instr\_total (numeric)
    -   sisinst + sislect **Temporary info to be dropped. used in proportions**
6.  prof\_prop (numeric)
    -   proportion of professors to undergraduate students. (prof\_total / scfy2)
7.  prop\_instr (numeric)
    -   proportion of instructors to undergraduate students. (inst\_total / scfy2)
8.  prop\_efnralt (numeric)
    -   proportion of nonresident alien students enrolled to undergrads. (efnralt / scfy2)

------------------------------------------------------------------------

Variable Codebook for Mobility Report Card Dataset via [MRC](https://opportunityinsights.org/wp-content/uploads/2018/03/Codebook-MRC-Table-1.pdf)

1.  par\_median
    -   Median parent household income
2.  k\_median
    -   Median child individual earnings in 2014 (rounded to the nearest $100)
3.  par\_q1
    -   Fraction of parents in the Bottom 20% of the income distribution
4.  par\_top1pc
    -   Fraction of parents in the Top 1% of the income distribution
5.  kq5\_cond\_parq1
    -   Percent of children who reach the Top 20% of the income distribution among children with parents in the Bottom 20% of the income distribution
6.  ktop1pc\_cond\_parq1
    -   Percent of children who reach the Top 1% of the income distribution among children with parents in the Bottom 20% of the income distribution
7.  mr\_kq5\_pq1
    -   Mobility Rate: Percent of students who have parents in the Bottom 20% of the income distribution and reach the Top 20% of the income distribution
8.  mr\_ktop1\_pq1
    -   Upper-Tail Mobility Rate: Percent of students who have parents in the Bottom 20% of the income distribution and reach the Top 1% of the income distribution
9.  trend\_parq1
    -   Change in % of Parents from the Bottom 20% of the income distribution between the 1980 and 1991 cohorts
10. trend\_bottom40
    -   Change in % of Parents from the Bottom 40% of the income distribution between the 1980 and 1991 cohorts
11. count
    -   Average number of children per cohort
