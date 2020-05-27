# sql-table-editor-dialog
Generates an editor dialog based solely on the name of an SQL table and the row ID.

<b>How to use?</b><br>
<pre>
(create-table-editor appname "My dialog message" my-db "my-table" "my-primary-id" "my-primary-value")
</pre>
This command creates and displays a dialog that lets one edit the specified row (by specifying the primary column and its ID).
