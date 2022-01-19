Generating MetaCyc SmartTables
================

## Online SmartTables

### Create Table 1: Compounds and IDs

1.  Go to www.metacyc.org and log in.
2.  Navigate to \[SmartTables\] -\> \[Special SmartTables\].
3.  Choose \[All compounds of MetaCyc\].
4.  If you see a warning message, make sure you create a writeable copy
    of the SmartTable.
5.  Delete all columns except for the \[Compound\] column by clicking on
    the column header (in navy blue), and then in the control panel on
    the right hand side, clicking \[Column \>\] -\> \[\> Delete Column\]
6.  Then click the \[Compound\] column header, and you should then see
    three dropdown menus above the table: \[Add Transform Column\],
    \[Add Property Column\], and \[Enrichments\].
7.  \[Add Property Column\] -\> \[Database Links\]. Choose:
      - \[CAS\]
      - \[HMDB\]
      - \[KEGG LIGAND\]
      - \[PubChem-compound\]
8.  On the right-hand pane, under \[Operations\], choose \[Export \>\]
    -\> \[\> to Spreadsheet File\], choose \[frame IDs\] as the “format
    type for values in spreadsheet”, and export the SmartTable.

<!-- end list -->

    /database_updates/1-compounds-ids.tsv

### Create Table 4: Genes and IDs

1.  Go to www.metacyc.org and log in.
2.  Navigate to \[SmartTables\] -\> \[Special SmartTables\].
3.  Choose \[All genes of MetaCyc\].
4.  If you see a warning message, make sure you create a writeable copy
    of the SmartTable.
5.  Delete all columns except for the \[Gene Name\] column by clicking
    on the column header (in navy blue), and then in the control panel
    on the right hand side, clicking \[Column \>\] -\> \[\> Delete
    Column\]
6.  Then click the \[Gene Name\] column header, and you should then see
    three dropdown menus above the table: \[Add Transform Column\],
    \[Add Property Column\], and \[Enrichments\].
7.  \[Add Property Column\] -\> \[Database Links\]. Choose:
      - \[Ensembl Human\]
      - \[GeneCards\] - Official Gene Symbol
8.  On the right-hand pane, under \[Operations\], choose \[Export \>\]
    -\> \[\> to Spreadsheet File\], choose \[frame IDs\] as the “format
    type for values in spreadsheet”, and export the SmartTable.

<!-- end list -->

    /database_updates/4-genes-ids.tsv

## SmartTables in pathway-tools

pathway-tools offers more consistent results, better user interface, and
is generally less buggy than the online SmartTables. However, they
should theoretically provide the same results, and you can test this. To
download and install pathway-tools, you will need to contact MetaCyc
support for an educational license. They will verify your request and
then email you a link with a username and password to log in and
download the software.

1.  Download and install pathway-tools (currently version 21.0)
2.  Run pathway-tools, and in the visual interface click ‘MetaCyc’

### Create Table 2: Reactions of Compounds

1.  From the \[SmartTables\] menu dropdown choose \[Create New
    SmartTable\] -\> \[Containing All\] -\> \[Compounds\]
2.  Once list of all compounds appears (currently 15950 Total), choose
    \[SmartTables\] -\> \[Transform SmartTable\] -\> \[Reactions of
    Compounds\]
3.  Save the SmartTable: \[SmartTables\] -\> \[Export SmartTable\] -\>
    \[Tab-delimited table\], and select save directory & filename.
4.  When prompted, specify \[Identifiers\] as the export choice.

<!-- end list -->

    /database_updates/2-compounds-reactions.tsv

### Create Table 3: Genes of Reactions

1.  From the \[SmartTables\] menu dropdown choose \[Create New
    SmartTable\] -\> \[Containing All\] -\> \[Reactions\]
2.  Once the list fo all reactions appears (currently 15311 Reactions),
    choose \[SmartTables\] -\> \[Transform SmartTable\] -\> \[Genes of a
    Reaction\]
3.  Save the SmartTable: \[SmartTables\] -\> \[Export SmartTable\] -\>
    \[Tab-delimited table\], and select save directory & filename.
4.  When prompted, specify \[Identifiers\] as the export choice.

<!-- end list -->

    /database_updates/3-reactions-genes.tsv

### Create Table 5: Pathways and Reactions

1.  From the \[SmartTables\] menu dropdown choose \[Create New
    SmartTable\] -\> \[Containing All\] -\> \[Pathways\]
2.  Once list of all compounds appears (currently 2903 Total), choose
    \[SmartTables\] -\> \[Transform SmartTable\] -\> \[Reactions of
    pathway\]
3.  Save the SmartTable: \[SmartTables\] -\> \[Export SmartTable\] -\>
    \[Tab-delimited table\], and select save directory & filename.
4.  When prompted, specify \[Identifiers\] as the export choice.

<!-- end list -->

    /database_updates/5-pathways-reactions.tsv

## Next steps
See the script `database_updates/munge_metacyc_data.R` to clean the newly
created files so they're usable by MetaBridge.
