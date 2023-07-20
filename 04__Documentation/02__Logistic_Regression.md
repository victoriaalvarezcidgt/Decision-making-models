<h1 align = "center"> Logistic Regression </h1>
<ul>
  <li>
    Logistic Regression is  one of the machine learning algorithms that is used typically for supervised learning that involves binary categorical outcome; that is, the categories of the label or response variable are usually two such as yes/no, true/false, \textbf{good/bad}, and benign/malignant, etc.
  </li>
</ul>
<title>Markdown for Write-Up</title>
Toheeb Jimoh
2023-07-20
R Markdown
This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see http://rmarkdown.rstudio.com.

When you click the Knit button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

Why would we think this should work? Recall that,

𝔼̂[𝑌∣𝑋=𝑥]=𝑋𝛽̂ .

Since 𝑌
 is limited to values of 0
 and 1
, we have

𝔼[𝑌∣𝑋=𝑥]=𝑃(𝑌=1∣𝑋=𝑥).

It would then seem reasonable that 𝐗𝛽̂ 
 is a reasonable estimate of 𝑃(𝑌=1∣𝑋=𝑥)
. We test this on the Default data.
