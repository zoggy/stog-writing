This is a plugin for [Stog](http://zoggy.github.com/stog/).

It allows to use the following elements in pages and posts:
- footnotes
- bibliography

## Usage

To install:

    git clone git@github.com:zoggy/stog-writing.git
    cd stog-writing
    make all install

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

To define the bibliograpy, you must add to the root `index.html` two
fields:

    bib-files=file1.bib,file2.bib,...
    bib-page=yourpage

`bib-files` indicates the files to read entries from, in bibtex format.
`bib-page` indicates in which page the bibliography will be. This page
must exist.

In your pages and posts, you can use this syntax to cite an entry:

    <cite href="entryid"/>

The `format` attribute can be used to specify the format of the text of the link,
for example:

    <cite href="entryid" format="$(author): $(title)"/>

will display the contents of the fields `author` and `title` of the entry.
The default format is `$(id)`.

To include the complete list of entries, use the following syntax:

    <bibliography/>

The `sort` attribute can be used to indicate which fields must be used
to sort the entries. Default is to sort on the `id` field. The `reverse`
field can be set to `true` to reverse the order of the sort. Example:

    <bibliography reverse="true" sort="year"/>

The bibliography command will output each bibliography entry
using the `bib_entry.tmpl` template. You can use the `bib_entry.tmpl`
file included as an example. You will have to place it in your stog
template directory.
