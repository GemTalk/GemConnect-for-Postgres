# GemConnect for Postgres (GCfP)

Postgres access via GemStone FFI

## Prerequisites
Installation instructions assume that you have registered SSH Keys with your GitHub account. See [Connecting to GitHub with SSH](https://help.github.com/articles/connecting-to-github-with-ssh/) for more information.

You must have git installed: [git setup](https://help.github.com/articles/set-up-git/)

You must have Postgres installed. GCfP was built and tested on Linux using Postgres 11. Other versions of Postgres will probalby work but might not.

You must have GemStone/64 version 3.6.2 or later installed and running.

## Installation

1. Find the Postgres client shared library on your system. The file name is libpq.so and it should be in /usr/lib or /usr/local/lib.

2. Set the enviroment variable POSTGRES_LIB to reference the ***full path*** to the Postgres shared library.
```
export POSTGRES_LIB=/usr/lib/libpq.so
```

3. cd to the src directory
```
cd src
```
4. Login to topaz GemStone as SystemUser
```
________________________________________________________________________________
|              GemStone/S64 Object-Oriented Data Management System               |
|                    Copyright (C) GemTalk Systems 1986-2021                     |
|                              All rights reserved.                              |
|                            Covered by U.S Patents:                             |
|        6,256,637 Transactional virtual machine architecture (1998-2018)        |
|          6,360,219 Object queues with concurrent updating (1998-2018)          |
|              6,567,905 Generational Garbage Collector (2001-2021)              |
|     6,681,226 Selective Pessimistic Locking for a Concurrently Updateable      |
|                              Database (2001-2021)                              |
+--------------------------------------------------------------------------------+
|    PROGRAM: topaz, Linear GemStone Interface (Linked Session)                  |
|    VERSION: 3.6.1.1, Mon Jun 28 17:55:54 2021 (branch 3.6.1.1)                 |
|     COMMIT: 2021-06-28T17:05:42-07:00 c614a8ebfa62650b399493b66c879b1ad2cc4bed |
|  BUILT FOR: x86-64 (Linux)                                                     |
| RUNNING ON: 8-CPU moop x86_64 (Linux 4.15.0-147-generic #151-Ubuntu SMP Fri Jun|
| 18 19:21:19 UTC 2021)                                                          |
|  PROCESSOR: 4-core Intel(R) Core(TM) i7-7700K CPU @ 4.20GHz (Kaby Lake-DT)     |
|     MEMORY: 64287 MB                                                           |
| PROCESS ID: 14343     DATE: 09/16/21 14:36:18 PDT  (UTC -7:00)                 |
|   USER IDS: REAL=normg (300) EFFECTIVE=normg (300) LOGIN=normg (300)           |
|________________________________________________________________________________|
Warning, executable configuration file not found
    /moop4/users/normg/fileout/git/gem.conf

Reading initialization file /moop4/users/normg/gs3.6.1.1.tmp/topaz.ini
topaz> login
[Info]: LNK client/gem GCI levels = 36000/36000
--- 09/16/21 14:36:20.272 PDT Login
[Info]: User ID: SystemUser
[Info]: Repository: normtmp_3.6.1.1
[Info]: Session ID: 5 login at 09/16/21 14:36:20.274 PDT
[Info]: GCI Client Host: 
[Info]: Page server PID: -1
[Info]: using libicu version 58.2
[Info]: Gave this process preference for OOM killer: wrote to /proc/14343/oom_score_adj value 250
[09/16/21 14:36:20.277 PDT]
  gci login: currSession 1  linked session 
successful login
topaz 1> 
```
5. Input the install script install.topaz. A log file named GemConnectForPostres.install.log will be created.
```
topaz>input install.topaz
```

