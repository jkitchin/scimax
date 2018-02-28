
# Table of Contents

1.  [Equations](#org13b154e)
2.  [Code](#org787e021)
3.  [Tables](#org16d6f6d)
4.  [Citations  label:sec-citations](#org45bc413)
5.  [Radio targets](#org89c6898)
6.  [Cross-references](#org3a316c0)
7.  [needed](#orgfe13ef9)
8.  [Exporting](#org0d860f6)

Why? Don't we already have org-mode? Yes, but some places like Markdown, and it is no fun to write when you have really technical documents.


<a id="org13b154e"></a>

# Equations

Suppose you have this equation to solve:

<a name="eq-sle">eq-sle</a>

\begin{equation}
8 = x - 4
\end{equation}

    print(f'x = {8 + 4}')

    x = 12

The results above show the answer to <eq-sle>.


<a id="org787e021"></a>

# Code

    %matplotlib inline
    import matplotlib.pyplot as plt

    plt.plot([1, 2, 4, 8])

    [<matplotlib.lines.Line2D at 0x10c6d34e0>]

![img](obipy-resources/0a58dae9b8af7857c4824224987cae2f-18961DFU.png)

You might like a caption.


<figure>
  <img src="obipy-resources/0a58dae9b8af7857c4824224987cae2f-18961DFU.png">
  <figcaption>A figure with a  caption. <a name="fig-data">fig-data</a></figcaption>
</figure>]]


<a id="org16d6f6d"></a>

# Tables

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">
<caption class="t-above"><span class="table-number">Table 1:</span> A data table. <a name="tab-data">tab-data</a></caption>

<colgroup>
<col  class="org-right" />

<col  class="org-right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-right">x</th>
<th scope="col" class="org-right">y</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-right">1</td>
<td class="org-right">1</td>
</tr>


<tr>
<td class="org-right">2</td>
<td class="org-right">4</td>
</tr>


<tr>
<td class="org-right">3</td>
<td class="org-right">9</td>
</tr>


<tr>
<td class="org-right">4</td>
<td class="org-right">16</td>
</tr>
</tbody>
</table>


<a id="org45bc413"></a>

# Citations  <a name="sec-citations">sec-citations</a>

You can have proper scientific citations like this [^kitchin-2015-examp], including multiple references [^kitchin-2015-data-surfac-scien]<sup>,</sup>[^kitchin-2015-examp]<sup>,</sup>[^kitchin-2016-autom-data].


<a id="org89c6898"></a>

# Radio targets

In org-mode you can define a <div name="target">target</div> that you can make a link to later.


<a id="org3a316c0"></a>

# Cross-references

Remember in Table [tab-data](#tab-data)?  How about <eq-sle>? Or that figure we put a caption on (Fig.  [fig-data](#fig-data)).

How about section [sec-citations](#sec-citations) on citations?

Remember the [target](#target) we referred to earlier?


<a id="orgfe13ef9"></a>

# TODO needed

-   [ ] references to tables
-   [ ] fix eqref in md export in org-ref
-   [ ] targets need a better export


<a id="org0d860f6"></a>

# Exporting

To a buffer:

    (pop-to-buffer (org-export-to-buffer 'scimax-md "*scimax-md-export*"))

    #<buffer *scimax-md-export*>

    (org-export-to-file 'scimax-md "scimax-md.md")

    scimax-md.md

# Bibliography
[^kitchin-2015-examp]: Kitchin, Examples of Effective Data Sharing in Scientific Publishing, <i>{ACS Catalysis}</i>, <b>5(6)</b>, 3894-3899 (2015). <a href=" http://dx.doi.org/10.1021/acscatal.5b00538 ">link</a>. <a href="http://dx.doi.org/10.1021/acscatal.5b00538">doi</a>.

[^kitchin-2015-data-surfac-scien]: "John Kitchin", Data Sharing in Surface Science, <i>"Surface Science "</i>, <b>647()</b>, 103-107 (2016). <a href="http://www.sciencedirect.com/science/article/pii/S0039602815001326">link</a>. <a href="http://dx.doi.org/10.1016/j.susc.2015.05.007">doi</a>.

[^kitchin-2016-autom-data]: "Kitchin, Van Gulick \& Zilinski, Automating Data Sharing Through Authoring Tools, <i>"International Journal on Digital Libraries"</i>, <b>18(2)</b>, 93--98 (2016). <a href="http://dx.doi.org/10.1007/s00799-016-0173-7">link</a>. <a href="http://dx.doi.org/10.1007/s00799-016-0173-7">doi</a>.
