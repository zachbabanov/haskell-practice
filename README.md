# haskell-practice
## Dependencies  
  ### Gaussian elimination
  This project has no dependencies, so for build you will need a basic Haskell platform install:
  1. For Ubuntu and Ubuntu-like Unix systems
  You probably need to install ghc compiler and cabal package manager
  ```
  sudo apt-get install haskell-platform cabal
  ```
  Also, you may need to install libraries for correct gcc and ghc work
  ```
  sudo apt-get install g++ gcc libc6-dev libffi-dev libgmp-dev
  ```
  For any library you can't install use 'libname-dev' instead
 2. For Windows
 It's recommended to use GHCup utility to install and manage packages and utilities on Windows machines
 ```
 Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }
 ```
 If it is not possible on your exact device try to install it manually from haskell.org and cabal from chocolatey
 
 - https://www.haskell.org/ghc/download.html  
 - https://community.chocolatey.org/packages/cabal  
 
 ### Patience sort  
 - Data.List
 1. For Ubuntu and Ubuntu-like Unix systems
 ```
 sudo cabal install base --force-reinstalls
 ```
 2. For Windows
 ```
 cabal install base
 ```
 ### Minesweeper
 - Data.Map 
 - Data.Set
 - Graphics.Gloss
 - System.Random
 - System.Random.Shuffle  

As for myself, I builded this exact program on Ubuntu 20.02 via glosslib. System.Random, System.Random.Shuffle and Graphics.Gloss may be temporary or permanently broken on windows devices, so try to resolve this problem by yourself. If it's correctly building and packages are found by cabal you are all done to start.  
1. For Ubuntu and Ubuntu-like Unix systems
```
sudo cabal install containers random random-shuffle gloss --force-reinstalls
```
And you also need to install gloss depend libs
```
sudo apt-get install libghc-gloss-dev
```
2. For Windows
```
cabal install containers random random-shuffle gloss --force-reinstalls
```
## Installation and build
  After all package dependencies are installed, you need to start a program either from ghc, ghci or build it into executable file. Note that you need to be in the same directory with file you starting/building  
  For example for patiencesort.hs:  
  ```
  ghc patiencesort.hs
  ```
  or
  ```
  ghci patiencesort.hs
  ```
  or
  ```
  ghc patiencesort.hs -0 patiencesort
  ```
  For the last option, after you successfully build an executable, you can start program from file:
  ```
  ./patiencesort
  ```  
  It is highly recommended to use last option for GUI programs, such as Minesweeper game in this repository and start it only from already builded file
