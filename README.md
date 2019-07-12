# An Alternative to Rank-Based College Modelling
Authored by [EJ Arce](https://github.com/ejarce), [Simon Couch](https://github.com/simonpcouch), and [Alexander Moore](https://github.com/moorea1)

A practicum project advised by [Dr. Kelly McConville](https://github.com/mcconvil)

**Abstract**: Rank-based college exploration, such as that popularized by U.S. News College Rankings, plays a pivotal role in how prospective college students discover and compare potential schools. However, despite its popularity, embedding hierarchy into college exploration reaffirms entrenched status obsession more than it educates and informs. We propose and implement an alternative method for prospective students and institutional researchers to evaluate colleges focusing on what makes a college similar, not better, than another college. Data from IPEDS and the Mobility Report Card from the Equality of Opportunity Project are joined and provided to users in an R Shiny application that allows users to compare United States educational institutions through visualization, comparison of raw data, and model-based assessment of school similarity.

FIX THIS PLOT:
![](docs/index/basic_plotting.png)

----------------------------------
## Repo Organization
**/data**: All the data used in the creation of the paper and application. This includes publically-available IPEDS surveys and Equality of Opportunity's Mobility Report Card data.

**/doc**: Documentation relating to the project. This includes the bookdown document which knits the paper, literature cited, work logs, and practicum class activities.

**/shiny**: The deliverable shiny application. This includes all backend code necessary to utilize the source code for the shiny application to explore and make changes as necessary. The app is also available hosted.

----------------------------------
## Document Sections

**Chapter 1**: Introduction to the problem of college ranking and an exploration of alternatives.

**Chapter 2**: Data acquisition, analyses, and methods.

**Chapter 3**: The Shiny Application deliverable and use guide.

**Conclusion**: A discussion on the merits of rank-based and similarity-based college research. Inference on the challenges of data and dimension arising in this domain, and conclusions on improvements.
