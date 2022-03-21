fileformat utf8
set compile_env: 0
! ------------------- Class definition for WidgetWithStrings
expectvalue /Class
doit
Object subclass: 'WidgetWithStrings'
  instVarNames: #( id lastUpdate lastUpdateNoTz
                    isActive activeDate activeTime nameSb
                    nameDb nameQb balance encryptedData
                    rdbPostLoadCalled)
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: (UserGlobals at: #GcfpInstallDict)
  options: #()

%
set compile_env: 0
! ------------------- Class definition for WidgetWithUnicode
expectvalue /Class
doit
WidgetWithStrings subclass: 'WidgetWithUnicode'
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: (UserGlobals at: #GcfpInstallDict)
  options: #()

%
set compile_env: 0
! ------------------- Class definition for PostgresTestCase
expectvalue /Class
doit
GsTestCase subclass: 'PostgresTestCase'
  instVarNames: #( connection errorNum)
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: (UserGlobals at: #GcfpInstallDict)
  options: #()

%
set compile_env: 0
! ------------------- Class definition for WidgetWithStringsOldColumnMap
expectvalue /Class
doit
WidgetWithStrings subclass: 'WidgetWithStringsOldColumnMap'
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: (UserGlobals at: #GcfpInstallDict)
  options: #()

%
set compile_env: 0
! ------------------- Class definition for WidgetWithUnicodeOldColumnMap
expectvalue /Class
doit
WidgetWithUnicode subclass: 'WidgetWithUnicodeOldColumnMap'
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: (UserGlobals at: #GcfpInstallDict)
  options: #()

%
set compile_env: 0
! ------------------- Class definition for WidgetWithUnicodeOldColumnMap
expectvalue /Class
doit
Object subclass: 'TestNullTupleObject'
  instVarNames: #('exitCode')
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: (UserGlobals at: #GcfpInstallDict)
  options: #()

%
