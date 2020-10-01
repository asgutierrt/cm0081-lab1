# cm0081-lab1
Name: Ana Sofia Gutierrez

Module containing the function StringsOfDFA

Takes a DFA and an integer number k and returns how many words of size k belong to the language of the given DFA

stringsOfDFA :: DFA s c -> Int -> Int

The Dfa is defined by the module DFA given in the assignment

General description

The program executes recursively the number S of ways to reach each accepting state in the DFA in k-steps by adding the number of words of k-1 length that can reach each state times the number of words that can go from that state to an accepting state in 1 step.

OS Microsoft Windows 10 Home Version 10.0.18363

GHC, version 8.10.1

HLint v3.1.6, (C) Neil Mitchell 2006-2020
