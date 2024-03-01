# Generating MetaCyc SmartTables


## Online SmartTables
We'll start by downloading data from [MetaCyc](metacyc.org); you'll need to 
create an account, using an institutional email.


### Create Table 1: Compounds and IDs

1. Navigate to [SmartTables], [Special SmartTables].
2. Choose [All compounds of MetaCyc].
3. Delete all columns except for the [Compound] column by clicking on the column
header (in navy blue), and then in the control panel on the right hand side,
select [Column] -> [Delete Column]
4. Click the [Compound] column header, and you should then see three dropdown
menus above the table.
5. Select [Add Property Column] -> [Database Links]. Choose "HMDB" and "KEGG
LIGAND"
6. On the right-hand pane under [Operations], choose [Export] -> [to Spreadsheet
File], then [frame IDs] as the "format type for values in spreadsheet"
7. Save the table in the desired directory as **"1-compounds-ids.tsv"**


### Create Table 4: Genes and IDs

1. Navigate to [SmartTables], [Special SmartTables].
2. Choose [All genes of MetaCyc].
3. Delete all columns except for the [Gene Name] using the same steps as above.
4. Click the [Gene Name] column header, and three
dropdown menus should appear above the table.
5. Select [Add Property Column] -> [Database Links], then choose [Ensembl Human]
and [GeneCards].
6. On the right-hand pane under [Operations], choose [Export] -> [to Spreadsheet
File], then [frame IDs] as the "format type for values in spreadsheet".
7. Save the table in the desired directory as **"4-genes-ids.tsv"**


## SmartTables in PathwayTools

**PathwayTools** offers more consistent results, better user interface, and
is generally less buggy than the online SmartTables. However, they should
theoretically provide the same results, and you can test this. To download and
install PathwayTools, you will need to contact MetaCyc support for an
educational license. They will verify your request and then email you a link
with a username and password to log in and download the software.


### Create Table 2: Reactions of Compounds

1. Run pathway-tools, and on the first page click "MetaCyc".
2. From the [SmartTables] menu dropdown choose [Create New SmartTable] ->
[Containing All] -> [Compounds].
3. Once the table of all compounds appears, select [SmartTables] -> [Transform 
SmartTable] -> [Reactions of Compounds].
4. To save the SmartTable select [SmartTables] -> [Export SmartTable] ->
[Tab-delimited table], and select the save directory. Save the table as
**"2-compounds-reactions.tsv"**.
5. When prompted, specify [Identifiers] as the export choice.


### Create Table 3: Genes of Reactions

1. From the [SmartTables] menu dropdown choose [Create New SmartTable] ->
[Containing All] -> [Reactions].
2. Once the list fo all reactions appears, choose [SmartTables] -> [Transform 
SmartTable] -> [Genes of a Reaction].
3. To save the SmartTable select [SmartTables] -> [Export SmartTable] ->
[Tab-delimited table], and select the save directory. Save the table as
**"3-reactions-genes.tsv"**.
4. When prompted, specify [Identifiers] as the export choice.


### Create Table 5: Pathways and Reactions

1. From the [SmartTables] menu dropdown choose [Create New SmartTable] -> 
[Containing All] -> [Pathways].
2. Once list of all compounds appears, choose [SmartTables] -> [Transform 
SmartTable] -> [Reactions of pathway].
3. To save the SmartTable select [SmartTables] -> [Export SmartTable] ->
[Tab-delimited table], and select the save directory. Save the table as
**"5-pathways-reactions.tsv"**.
4. When prompted, specify [Identifiers] as the export choice.


## Next steps
Place all five of the TSV files into the "database_updates" directory, and see
the script `database_updates/munge_metacyc_data.R` to clean the newly
created files so they're usable by MetaBridge.
