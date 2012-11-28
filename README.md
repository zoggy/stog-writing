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

### Defining bibliographies

Bibliographies can be defined in any element header, with the following syntax:

    <bibliographies ...>
      <bibliography .../>
      <bibliography .../>
      ...
    </bibliographies>

The `<bibliographies>` node can have the following attributes:

- `sort="..."` : the items in each bibliography will be sorted by default according,
              to the list of fields,
- `reverse="true"` : the items in each bibliography will be sorted in reverse order,
- `prefix="..."` : a default string to use as prefix for ids of entries read
                from bibtex files,

Each `<bibliography>` node can have the following attributes:

- `name="..."` : the name of the bibliography, for further reference; default name is "default";
              each bibliography must have a site-wide unique name,
- `files="..."` : a comma-separated list of filenames, in bibtex format;
               filenames are relative to the element source file,
- `sort`, `reverse` and `prefix` : can be used to override the same attributes of the
              `<bibliographies>` node.

Using attribute `bib-files="file1.bib,file2.bib,..."` in an element header is a shortcut for

    <bibliographies>
      <bibliography files="file1.bib,file2.bib,..."/>
    </bibliographies>

### Using bibliographies

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

    [<bib-entry-rank/>]

The rank is the position of the item in its bibliography.

To include the complete list of entries of a bibliography, use the following node:

    <bibliography/>

The attribute `name="..."` can be used to specify the name of the bibliography to
insert. Default name is "default".

The `<bibliography>` node will rewritten into the list of its entries,
each entry being inserted using the `bib_entry.tmpl` template. You can use
the `bib_entry.tmpl` file included as an example. You will have to place it in your stog
template directory.
