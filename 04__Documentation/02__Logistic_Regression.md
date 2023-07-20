<h1 align = "center"> Logistic Regression </h1>
<ul>
  <li>
    Logistic Regression is  one of the machine learning algorithms that is used typically for supervised learning that involves binary categorical outcome; that is, the categories of the label or response variable are usually two such as yes/no, true/false, \textbf{good/bad}, and benign/malignant, etc.
  </li>
</ul>
<h2>R Markdown</h2>
<p>This is an R Markdown document. Markdown is a simple formatting
syntax for authoring HTML, PDF, and MS Word documents. For more details
on using R Markdown see <a href="http://rmarkdown.rstudio.com" class="uri">http://rmarkdown.rstudio.com</a>.</p>
<p>When you click the <strong>Knit</strong> button a document will be
generated that includes both content as well as the output of any
embedded R code chunks within the document. You can embed an R code
chunk like this:</p>
<p>Why would we think this should work? Recall that,</p>
<p><span class="math display">\[
\hat{\mathbb{E}}[Y \mid X = x] = X\hat{\beta}.
\]</span></p>
<p>Since <span class="math inline">\(Y\)</span> is limited to values of
<span class="math inline">\(0\)</span> and <span class="math inline">\(1\)</span>, we have</p>
<p><span class="math display">\[
\mathbb{E}[Y \mid X = x] = P(Y = 1 \mid X = x).
\]</span></p>
<p>It would then seem reasonable that <span class="math inline">\(\mathbf{X}\hat{\beta}\)</span> is a reasonable
estimate of <span class="math inline">\(P(Y = 1 \mid X = x)\)</span>. We
test this on the <code>Default</code> data.</p>
