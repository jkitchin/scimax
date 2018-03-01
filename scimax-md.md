
# Table of Contents

1.  [Headings](#orgab7d121)
    1.  [subheading](#org8074f2d)
        1.  [subsubheading](#org37db8e4)
2.  [Markups](#org28c5644)
3.  [Lists](#org0106ee1)
    1.  [Numbered lists](#orgf6894b9)
    2.  [plain lists](#orga82f2eb)
    3.  [checklists](#orgf33273d)
    4.  [definition lists](#orga85c1e8)
4.  [Equations](#org4518a9f)
5.  [Code blocks](#orged492fd)
6.  [Tables](#org3276d0a)
7.  [Citations  label:sec-citations](#org1e4cdca)
8.  [Radio targets](#org08b30b8)
9.  [Cross-references](#orgfe87e9d)
10. [Exporting a single file](#org458aaf5)
11. [Handling projects](#org7c84e29)
12. [Downsides to this approach](#org50bdd10)

Why? Don't we already have org-mode? Yes, but some places like Markdown, it is no fun to write when you have really technical documents, and it would be harder to get markdown-mode to be as good as org-mode than to do this.

Github's rendering of org-mode is only ok. This might be a nicer way to get better Github pages.


<a id="orgab7d121"></a>

# Headings

It goes without saying I hope, that we use headings to organize things.


<a id="org8074f2d"></a>

## subheading


<a id="org37db8e4"></a>

### subsubheading

Anything deeper than this gets turned into paragraphs by default.


<a id="org28c5644"></a>

# Markups

**bold** *italics* <span class="underline">underline</span> <del>strike</del> `verbatim` `code`

subscripts: H<sub>2</sub>O

superscripts: H<sup>+</sup>

Regular urls are fine: <http://google.com>.


<a id="org0106ee1"></a>

# Lists


<a id="orgf6894b9"></a>

## Numbered lists

1.  one
2.  two
3.  three

Note these letters will render as numbers.

1.  apple
2.  bear
3.  cat


<a id="orga82f2eb"></a>

## plain lists

-   one
-   two
-   three
    -   with nesting
        -   deeper
    -   back in
-   all the way


<a id="orgf33273d"></a>

## checklists

-   [ ] one
-   [ ] two
-   [ ] three


<a id="orga85c1e8"></a>

## definition lists

-   **org-mode:** what makes this possible
-   **emacs:** the other thing you need


<a id="org4518a9f"></a>

# Equations

Suppose you have this equation to solve:

\[8 = x - 4\]  <a name="eq-sle"></a>

You can put a label near this and refer to it later.


<a id="orged492fd"></a>

# Code blocks

    %matplotlib inline
    import matplotlib.pyplot as plt

    plt.plot([1, 2, 4, 8])

    [<matplotlib.lines.Line2D at 0x10c6d34e0>]

![img](obipy-resources/0a58dae9b8af7857c4824224987cae2f-18961DFU.png)

You might like a caption with a label you can refer to later.


<figure>
  <img src="./obipy-resources/0a58dae9b8af7857c4824224987cae2f-18961DFU.png">
  <figcaption>Figure nil: A figure with a  caption. <a name="fig-data"></a></figcaption>
</figure>


<a id="org3276d0a"></a>

# Tables

You can have tables, with captions and labels.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">
<caption class="t-above"><span class="table-number">Table 1:</span> A data table. <a name="tab-data"></a></caption>

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

Here is another table:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">
<caption class="t-above"><span class="table-number">Table 2:</span> A count of categories. <a name="tab-cat"></a></caption>

<colgroup>
<col  class="org-left" />

<col  class="org-right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">category</th>
<th scope="col" class="org-right">count</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">apples</td>
<td class="org-right">2</td>
</tr>


<tr>
<td class="org-left">oranges</td>
<td class="org-right">4</td>
</tr>
</tbody>
</table>


<a id="org1e4cdca"></a>

# Citations  <a name="sec-citations"></a>

You can have proper scientific citations like this <sup id="9e3ad98c9008c49c9d14834ca3913eb6"><a href="#kitchin-2015-examp" title="Kitchin, Examples of Effective Data Sharing in Scientific Publishing, {ACS Catalysis}, v(6), 3894-3899 (2015).">kitchin-2015-examp</a></sup>, including multiple references <sup id="66b54b1976758a93506a846c2666419b"><a href="#kitchin-2015-data-surfac-scien" title="John Kitchin, Data Sharing in Surface Science, Surface Science , v(), 103-107 (2016).">kitchin-2015-data-surfac-scien</a></sup><sup>,</sup><sup id="9e3ad98c9008c49c9d14834ca3913eb6"><a href="#kitchin-2015-examp" title="Kitchin, Examples of Effective Data Sharing in Scientific Publishing, {ACS Catalysis}, v(6), 3894-3899 (2015).">kitchin-2015-examp</a></sup><sup>,</sup><sup id="fe4ece7c7b3687ca21f32c0ee4e0a542"><a href="#kitchin-2016-autom-data" title="Kitchin, Van Gulick \&amp; Zilinski, Automating Data Sharing Through Authoring Tools, International Journal on Digital Libraries, v(2), 93--98 (2016).">kitchin-2016-autom-data</a></sup>. Check out the tooltips on them in the html that Github renders.

org-ref helps you insert citations from a bibtex database.

It is conceivable to have numbered citations, and fancier formatting, but I have no plans to implement that.


<a id="org08b30b8"></a>

# Radio targets

In org-mode you can define a <a name="target"></a>target that you can make a link to later.


<a id="orgfe87e9d"></a>

# Cross-references

Remember Table [tab-data](#tab-data) or the category Table ([tab-cat](#tab-cat))?   Or that figure we put a caption on (Fig.  [fig-data](#fig-data)).

How about section [sec-citations](#sec-citations) on citations?

Remember the [target](#target) we referred to earlier?

What matters the most in cross-references is that org-ref helps you complete them.

    print(f'x = {8 + 4}')

    x = 12

The results above show the answer to Eq. [eq-sle](#eq-sle).


<a id="org458aaf5"></a>

# Exporting a single file

    (require 'scimax-md)

    scimax-md

To a buffer:

    (pop-to-buffer (org-export-to-buffer 'scimax-md "*scimax-md-export*"))

    #<buffer *scimax-md-export*>

    (require 'scimax-md)
    (org-export-to-file 'scimax-md "scimax-md.md")

    scimax-md.md

# Bibliography
<a id="kitchin-2015-examp">[kitchin-2015-examp]</a> Kitchin, Examples of Effective Data Sharing in Scientific Publishing, <i>{ACS Catalysis}</i>, <b>5(6)</b>, 3894-3899 (2015). <a href=" http://dx.doi.org/10.1021/acscatal.5b00538 ">link</a>. <a href="http://dx.doi.org/10.1021/acscatal.5b00538">doi</a>. [↩](#9e3ad98c9008c49c9d14834ca3913eb6)

<a id="kitchin-2015-data-surfac-scien">[kitchin-2015-data-surfac-scien]</a> "John Kitchin", Data Sharing in Surface Science, <i>"Surface Science "</i>, <b>647()</b>, 103-107 (2016). <a href="http://www.sciencedirect.com/science/article/pii/S0039602815001326">link</a>. <a href="http://dx.doi.org/10.1016/j.susc.2015.05.007">doi</a>. [↩](#66b54b1976758a93506a846c2666419b)

<a id="kitchin-2016-autom-data">[kitchin-2016-autom-data]</a> "Kitchin, Van Gulick \& Zilinski, Automating Data Sharing Through Authoring Tools, <i>"International Journal on Digital Libraries"</i>, <b>18(2)</b>, 93--98 (2016). <a href="http://dx.doi.org/10.1007/s00799-016-0173-7">link</a>. <a href="http://dx.doi.org/10.1007/s00799-016-0173-7">doi</a>. [↩](#fe4ece7c7b3687ca21f32c0ee4e0a542)


<a id="org7c84e29"></a>

# Handling projects


<a id="org50bdd10"></a>

# Downsides to this approach

I never read or edit the markdown that is produced. There is probably a lot of stuff in it you would never write yourself. If that is a problem, there is a lot to do to get rid of it. Especially the way I use html to get features might not be considered very standard.

This is a one way conversion. If someone edits the markdown, and you re-export, you will clobber their changes.

Not every corner of org-mode has been tested yet.
