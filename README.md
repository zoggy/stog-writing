This is a plugin for [Stog](http://zoggy.github.com/stog/).

Ths plugin allows to use the following elements in pages and posts:
- footnotes
- bibliography

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