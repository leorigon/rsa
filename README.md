# RSA

This repository contains an old homework from the algorithm design and analysis discipline of the Master's Degree in Applied Computing course of UDESC. Is a simple prototype of RSA algorithm using Miller-Rabin test and the Pollard Rho heuristic to try to break the key. 

## requirements:

- Stack;
- Some text editor.

## to run the RSA encryption:

Fill the `entry` file with some text and type ```stack build && stack exec rsa```
on terminal. After this, the algorithm will give you the pair of keys and the encoded text (`encoded-entry`). 

## to run the Pollard Rho

After create a pair of keys with the RSA algorithm, run ```stack exec pollardRho```.  

## TODO:
- Create a set of tests using some haskell tests library;]
- Create option to set the size of the keys to break;
- Fix some perfomance issues and improve the code.