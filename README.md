libclustER
==========

A LIBrary of graph CLUSTering algorithms for the purpose of Entity Resolution (a.k.a. record linkage or duplicate detection).

Usage and Setup:
-Pull version from repo
-open up R shell
-Change working directory to "src" directory
-runthe following in R: source("libcluster.R");
-Go nuts with exploring

Suggestion:
Familiarize yourself with igraph library before using libclustER.

Note:
All clustering functions are meant to take in an igraph object as input.
All clustering functions return clusterings as a list of numeric vectors.
