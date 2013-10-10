libclustER
==========

A LIBrary of graph CLUSTering algorithms for the purpose of Entity Resolution (a.k.a. record linkage or duplicate 
detection).

Usage and Setup:
----------------
(1) Pull version from repo.

(2) Open up R shell.

(3) Change working directory to "trunk/src" directory.

(4) Run the following in R: 

source("libcluster.R");

(5) Explore or see the wiki (https://github.com/hussaibi/libclustER/wiki) 
for more specific information about the library

Suggestion:
-----------
Familiarize yourself with igraph library before using libclustER.

Note:
-----
All clustering functions are meant to take in an igraph object as input. They are suffixed with `.cluster` or `.clustering`
All clustering functions return clusterings as a list of vectors.
