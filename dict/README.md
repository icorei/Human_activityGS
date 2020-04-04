# Instructions for *dict* (dictionaries) folder

This folder includes files that are used in the text-analysis to either exclude words and phrases or to combine related terms into broader concepts.

## File formats

Files *common_wordlist*, *common_not_in_dictionary_wordlist* and *wordlist* are in csv (comma-separated-values) format.

Files *regions_terms* and *species_terms* are simple text files with one entry per row.

File *camera_trap.liwc* is in LIWC format.

## How to edit the files

### csv Files

They are exported from R, based on the current literature search, do not edit.

### txt Files

Copy species and region terms from *common_not_in_dictionary_wordlist*. These will be arranged later to group similar species / regions.

### LIWC Files

Copy as many word combinations as possible from *common_not_in_dictionary_wordlist* and organize them according to broader standardized terms. Try to re-use defined terms if possible.

The lines with single '%' signs delimit a header for term definition: each line has a three digit term code followed by the  term name. Example:

```
%
010 Behaviour

100 Habitat
110 Forest
%
```

The lines in the header are organized according to broad/flexible categories. They can be rearranged and new categories can be added using any unused code. Example:

```
%
010 Behaviour
030 Population
031 Density_Estimation

100 Habitat
110 Forest
130 Palm_plantation
%
```

After the header each line represents a word combination from *wordlist* followed by the three digit term code that corresponds to the chosen main term. For example the word combination `forest_type` belong to term `Forest` with code 110, etc:

```
forest_type       110
tropic_forest     110

oil_palm          130
palm_plantat      130
```
