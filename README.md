This is a plugin for [Stog](http://zoggy.github.com/stog/).

It allows to use the following elements in pages and posts:
- footnotes
- bibliography

## Usage

To install:

    git clone git@github.com:zoggy/stog-writing.git
    cd stog-writing
    make all install

(you ust have [Menhir](http://cristal.inria.fr/~fpottier/menhir/) installed)
This will install the stog-writing package with ocamlfind.

To use:

    stog --package stog-writing ...

## Footnotes

Footnotes are indicated with the following syntax:

    <note>...</note>

As usual, you can put any xml in the <note> node, it will be
rewritten using the current environment.

For this to work, all the XML tree containing notes must be
included in a

    <prepare-notes>
    ...
    </prepare-notes>

node.

At last, all footnotes are inserted when a <notes/> node is encountered.
Numbering is automatic.

Example:

    <prepare-notes>
      ...
      bla bla bla<note>Hey, this is a footnote</note>.
      ...
      pim pam poum<note>This is a second note.</note>.
      ...
      <notes/>
    </prepare-notes>

## Bibliography

To define the bibliograpy, you must add one or more elements with a "bib-files" field.

    bib-files="file1.bib,file2.bib,..."

`bib-files` indicates the files to read entries from, in bibtex format.

In your pages and posts, you can use this syntax to cite an entry:

    <cite href="entryid"/>

The format of the reference can be set by various means, in order
of priority:

- in the children of the <cite> node, for example:

        <cite href="..."><bib-entry-author/>, <i><bib-entry-title/></i></cite>

- using the `format` attribute, for example:

        <cite href="entryid" format="$(author): $(title)"/>

- setting `cite-format` in the environment, for example in the header
  of the page/article:

        cite-format=<bib-entry-author/>, <i><bib-entry-title/></i>

These examples will display the contents of the fields `author` and `title`
of the entry as text for the reference link.

The default format is

    [<bib-entry-id/>]

To include the complete list of entries, use the following syntax
in the element having the "bib-files" field:

    <bibliography/>

The `sort` attribute can be used to indicate which fields must be used
to sort the entries. Default is to sort on the `id` field. The `reverse`
field can be set to `true` to reverse the order of the sort. Example:

    <bibliography reverse="true" sort="year"/>

The bibliography command will output each bibliography entry
using the `bib_entry.tmpl` template. You can use the `bib_entry.tmpl`
file included as an example. You will have to place it in your stog
template directory.
