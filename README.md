# GemConnect for Postgres (GCfP)

Postgres access via GemStone FFI

## Prerequisites

### User

Download this project as a zip file and unzip it.

### Developer

If you wish to contribute to this project you should have git installed ([git setup](https://help.github.com/articles/set-up-git/)) and then fork the project to your GitHub account. You may then clone the project using HTTPS or SSH keys. See [Connecting to GitHub with SSH](https://help.github.com/articles/connecting-to-github-with-ssh/) for more information on SSH.

### Postgres

You must have Postgres installed. GCfP was built and tested on Linux using Postgres 11. Other versions of Postgres will probably work but might not.

### GemStone/S 64 Bit

You must have GemStone/64 version 3.6.2 or later installed and running.

## Installation

1. Find the Postgres client shared library on your system. The file name is libpq.so and it should be in /usr/lib or /usr/local/lib.

2. Set the envirnoment variable POSTGRES_LIB to reference the ***full path*** to the Postgres shared library (this is only needed during installation. Once initialized, GCfP remembers where this library is located).
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
|    VERSION: 3.6.2, Mon Jul 19 07:50:57 2021 normg private build (branch 3.6.2) |
|     COMMIT: 2021-07-12T08:23:33-07:00 5204eb442b124442f8edb39de4d52c77d9f25fb0 |
|  BUILT FOR: x86-64 (Linux)                                                     |
| RUNNING ON: 8-CPU moop x86_64 (Linux 4.15.0-147-generic #151-Ubuntu SMP Fri Jun|
| 18 19:21:19 UTC 2021)                                                          |
|  PROCESSOR: 4-core Intel(R) Core(TM) i7-7700K CPU @ 4.20GHz (Kaby Lake-DT)     |
|     MEMORY: 64287 MB                                                           |
| PROCESS ID: 18819     DATE: 09/16/21 14:54:09 PDT  (UTC -7:00)                 |
|   USER IDS: REAL=normg (300) EFFECTIVE=normg (300) LOGIN=normg (300)           |
| BUILD TYPE: SLOW                                                               |
+--------------------------------------------------------------------------------+
|   GEMSTONE_NRS_ALL = #netldi:ldinormg#dir:/export/moop3/users/normg/gs362_2/slow50/gs/product/data
|________________________________________________________________________________|
Warning, executable configuration file not found
    /export/moop3/users/normg/gs362_2/slow50/gs/product/data/gem.conf

Reading initialization file /home/normg/.topazini
topaz> login
[Info]: LNK client/gem GCI levels = 36200/36200
ShrPcClientConnect got newClientId 9 newSlot 10
--- 09/16/21 14:54:10.152 PDT Login
[Info]: User ID: SystemUser
[Info]: Repository: norm
[Info]: Session ID: 5 login at 09/16/21 14:54:10.155 PDT
[Info]: GCI Client Host:
[Info]: Page server PID: -1
[Info]: using libicu version 58.2
[Info]: Loaded /export/moop3/users/normg/gs362_2/slow50/gs/product/lib/libicu.58.2.so
[Info]: Gave this process preference for OOM killer: wrote to /proc/18819/oom_score_adj value 250
[09/16/21 14:54:10.157 PDT]
  gci login: currSession 1  linked session
successful login
topaz 1>
```
5. Complete **either** step A **or** B, not both. Step C is optional.

- A) Installing code into a new SymbolListDictionary:
To create and install in a new Symbol Dictionary named GemConnectForPostgres, input the install script install.topaz.
A log file named GemConnectForPostres.install.log will be created.
```
  topaz>input install.topaz
```

- B) To install code into Globals:
  Input the install script install.into.globals.topaz. A log file named GemConnectForPostres.install.log will be created.
```
  topaz>input install.into.globals.topaz
```

- C) (Optional) To install the unit tests:
  Input the install script install.tests.topaz. A log file named GemConnectForPostres.install.tests.log will be created.
  Unit test classes will be installed in the same SymbolListDictionary as GemConnectForPostgres.
```
  topaz>input install.tests.topaz
```

6. If all goes well you should see a zero errorcount:
```
true
topaz 1 +> errorcount
0
topaz 1 +> output pop
topaz 1>
```
7. Quit out of topaz. You are finished the installation!

## Quick Getting Started

### Getting a connection to Posgres
1. First get an instance of GsPostgresConnectionParameters and intialize it to connect to your Postgres database. For example:
```
| params |
params := (GsPostgresConnectionParameters new)
		host: 'localhost';
		port: 5432;
		dbname: 'MyPostgresDb';
		connect_timeout: 10;
		yourself .
```

2. Now get an instance of GsPostgresConnection using the parameters object from step 1:
```
| connection |
connection := GsPostgresConnection
				newWithParameters: params.
```

3. Now you have an active connection to Postgres. You can use it to execute queries on Postgres which return a GsPostgresReadStream
object, or execute insert, update, or delete operations on rows in Postgres.

## Running the Unit Tests

To run the unit tests, you first need to install the unit tests (see step 5-C above) and create a writable Postgres database. Use the createdb Postgres command to create database:
```
/usr/local/pgsql/bin/createdb test
```

Next you need to edit the class method named #defaultParameters in class PostgresTestCase to reference the data you just created:
```
category: 'Parameters'
classmethod: PostgresTestCase
defaultParameters
	^(GsPostgresConnectionParameters new)
		host: 'localhost';
		port: 5432;
		dbname: 'test';
		connect_timeout: 10;
		yourself
%
```

Finally you are ready to run the tests:
```
PostgresTestCase run
```
or
```
PostgresTestCase debugEx
```
to debug tests.
