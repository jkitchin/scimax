
# Table of Contents

1.  [Headings](#org707bdf5)
    1.  [subheading](#org1ae46c1)
        1.  [subsubheading](#orga706fc1)
2.  [Markups](#orgad5e545)
3.  [Lists](#org30e0fec)
    1.  [Numbered lists](#orgd66bb84)
    2.  [plain lists](#orgb2fe309)
    3.  [checklists](#org5dd51f9)
    4.  [definition lists](#org1bd68d0)
4.  [Equations](#org87512d8)
5.  [Code blocks](#org412ef97)
6.  [Tables](#orgf1eb3e5)
7.  [Citations  label:sec-citations](#org0207a5e)
8.  [Radio targets](#orgda9299d)
9.  [Cross-references](#orgcfe2230)
10. [needed](#org1cf27f2)
11. [Exporting](#orgfe4e8cf)

Why? Don't we already have org-mode? Yes, but some places like Markdown, it is no fun to write when you have really technical documents, and it would be harder to get markdown-mode to be as good as org-mode than to do this.


<a id="org707bdf5"></a>

# Headings

It goes without saying I hope.


<a id="org1ae46c1"></a>

## subheading


<a id="orga706fc1"></a>

### subsubheading

Anything deeper than this gets turned into paragraphs by default.


<a id="orgad5e545"></a>

# Markups

**bold** *italics* <span class="underline">underline</span> <del>strike</del> `verbatim` `code`

subscripts: H<sub>2</sub>O

superscripts: H<sup>+</sup>


<a id="org30e0fec"></a>

# Lists


<a id="orgd66bb84"></a>

## Numbered lists

1.  one
2.  two
3.  three

Note these letters will render as numbers.

1.  apple
2.  bear
3.  cat


<a id="orgb2fe309"></a>

## plain lists

-   one
-   two
-   three
    -   with nesting
        -   deeper
    -   back in
-   all the way


<a id="org5dd51f9"></a>

## checklists

-   [ ] one
-   [ ] two
-   [ ] three


<a id="org1bd68d0"></a>

## definition lists

-   **org-mode:** what makes this possible
-   **emacs:** the other thing you need


<a id="org87512d8"></a>

# Equations

Suppose you have this equation to solve:

<a name="eq-sle">eq-sle</a>

\begin{equation}
8 = x - 4
\end{equation}

    print(f'x = {8 + 4}')

    x = 12

The results above show the answer to <eq-sle>.


<a id="org412ef97"></a>

# Code blocks

    %matplotlib inline
    import matplotlib.pyplot as plt

    plt.plot([1, 2, 4, 8])

    [<matplotlib.lines.Line2D at 0x10c6d34e0>]

![img](obipy-resources/0a58dae9b8af7857c4824224987cae2f-18961DFU.png)

You might like a caption.

./obipy-resources/0a58dae9b8af7857c4824224987cae2f-18961DFU.png


<a id="orgf1eb3e5"></a>

# Tables

You can have tables, with captions and labels.

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


<a id="org0207a5e"></a>

# Citations  <a name="sec-citations">sec-citations</a>

You can have proper scientific citations like this <sup>[kitchin-2015-examp]</sup>, including multiple references <sup>[kitchin-2015-data-surfac-scien]</sup><sup>,</sup><sup>[kitchin-2015-examp]</sup><sup>,</sup><sup>[kitchin-2016-autom-data]</sup>.


<a id="orgda9299d"></a>

# Radio targets

In org-mode you can define a <a name="target"></a>target that you can make a link to later.


<a id="orgcfe2230"></a>

# Cross-references

Remember in Table [tab-data](#tab-data)?  How about <eq-sle>? Or that figure we put a caption on (Fig.  [fig-data](#fig-data)).

How about section [sec-citations](#sec-citations) on citations?

Remember the [target](#target) we referred to earlier?


<a id="org1cf27f2"></a>

# TODO needed

-   [ ] fix eqref in md export in org-ref
-   [ ] redo how labels are done. Should they be visible?


<a id="orgfe4e8cf"></a>

# Exporting

    (require 'scimax-md)

    scimax-md

To a buffer:

    (pop-to-buffer (org-export-to-buffer 'scimax-md "*scimax-md-export*"))

    #<buffer *scimax-md-export*>

    (org-export-to-file 'scimax-md "scimax-md.md")

    scimax-md.md

# Bibliography
<a id="kitchin-2015-examp>kitchin-2015-examp</a>: Kitchin, Examples of Effective Data Sharing in Scientific Publishing, <i>{ACS Catalysis}</i>, <b>5(6)</b>, 3894-3899 (2015). <a href=" http://dx.doi.org/10.1021/acscatal.5b00538 ">link</a>. <a href="http://dx.doi.org/10.1021/acscatal.5b00538">doi</a>.

<a id="kitchin-2015-data-surfac-scien>kitchin-2015-data-surfac-scien</a>: "John Kitchin", Data Sharing in Surface Science, <i>"Surface Science "</i>, <b>647()</b>, 103-107 (2016). <a href="http://www.sciencedirect.com/science/article/pii/S0039602815001326">link</a>. <a href="http://dx.doi.org/10.1016/j.susc.2015.05.007">doi</a>.

<a id="kitchin-2016-autom-data>kitchin-2016-autom-data</a>: "Kitchin, Van Gulick \& Zilinski, Automating Data Sharing Through Authoring Tools, <i>"International Journal on Digital Libraries"</i>, <b>18(2)</b>, 93--98 (2016). <a href="http://dx.doi.org/10.1007/s00799-016-0173-7">link</a>. <a href="http://dx.doi.org/10.1007/s00799-016-0173-7">doi</a>.
