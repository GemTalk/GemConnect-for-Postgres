fileformat utf8
! ------------------- Remove existing behavior from WidgetWithStrings
removeAllMethods WidgetWithStrings
removeAllClassMethods WidgetWithStrings
! ------------------- Class methods for WidgetWithStrings
category: 'Class Mapping Support'
classmethod: WidgetWithStrings
classFor: aSymbol otherwise: anotherSymbol

^Globals at: aSymbol otherwise: (Globals at: anotherSymbol)
%
category: 'Class Mapping Support'
classmethod: WidgetWithStrings
classForDate

^ self classFor: #SmallDate otherwise: #Date
%
category: 'Class Mapping Support'
classmethod: WidgetWithStrings
classForDateAndTime

^ self classFor: #SmallDateAndTime otherwise: #DateAndTime
%
category: 'Class Mapping Support'
classmethod: WidgetWithStrings
classForFloat

^ self classFor: #SmallDouble otherwise: #Float
%
category: 'Class Mapping Support'
classmethod: WidgetWithStrings
classForScaledDecimal

^ self classFor: #SmallScaledDecimal otherwise: #ScaledDecimal
%
category: 'Class Mapping Support'
classmethod: WidgetWithStrings
classForTime

^ self classFor: #SmallTime otherwise: #Time
%
category: 'Class Mapping Support'
classmethod: WidgetWithStrings
convertToDbStrings: anArray

^anArray
%
category: 'Class Mapping Support'
classmethod: WidgetWithStrings
convertToQbStrings: anArray

^anArray
%
category: 'Class Mapping Support'
classmethod: WidgetWithStrings
convertToSbStrings: anArray

^anArray
%
category: 'Class Mapping Support'
classmethod: WidgetWithStrings
idFromInt: anInt

^anInt
%
category: 'Class Mapping Support'
classmethod: WidgetWithStrings
lastUpdateClass
	^ self classForDateAndTime
%
category: 'Class Mapping Support'
classmethod: WidgetWithStrings
lastUpdateToLastUpdateNoTz: aTimeObj

^aTimeObj offset: Duration zero
%
category: 'Instance Creation'
classmethod: WidgetWithStrings
new

^super new initialize
%
category: 'Instance Creation'
classmethod: WidgetWithStrings
newInstances: count

| result allSb allDb allQb pgstc rows |
pgstc := (System myUserProfile resolveSymbol: #PostgresTestCase) value .
rows := pgstc widgetTableNumRows .
allSb := self convertToSbStrings: pgstc widgetTableAllNames_sbs .
allDb := self convertToDbStrings: pgstc widgetTableAllNames_dbs .
allQb := self convertToQbStrings: pgstc widgetTableAllNames_qbs .

result := Array new: count .
1 to: count do:[:n| |inst idx|
	idx := 1 + (n \\ rows).
	inst := self new.
	inst id: (self idFromInt: n) ;
		lastUpdate: self lastUpdateClass now ;
		lastUpdateNoTz: (self lastUpdateToLastUpdateNoTz:  inst lastUpdate) ;
		isActive: true ;
		activeDate: Date today ;
		activeTime: Time now ;
		nameSb: (allSb at: idx) ;
		nameDb: (allDb at: idx) ;
		nameQb: (allQb at: idx) ;
		balance: self randomBalance ;
		encryptedData: (ByteArray withRandomBytes: 32) .
		result at: n put: inst.
].
^ result
%
category: 'Class Mapping Support'
classmethod: WidgetWithStrings
randomBalance

"WidgetWithStrings randomBalance"
| int |
int := Random new integerBetween: 1 and: 100000.
^ (ScaledDecimal for: int scale: 2) / 100
%
category: 'Postgres Support'
classmethod: WidgetWithStrings
rdbColumnMapping

^ Array new
	add: (GsPostgresColumnMapEntry newForColumn: 'id' instVar: 'id' instVarClass: SmallInteger) ;
	add: (GsPostgresColumnMapEntry newForColumn: 'last_update' instVar: 'lastUpdate' instVarClass: self classForDateAndTime ) ;
	add: (GsPostgresColumnMapEntry newForColumn: 'last_update_notz' instVar: 'lastUpdateNoTz' instVarClass: self classForDateAndTime ) ;
	add: (GsPostgresColumnMapEntry newForColumn: 'is_active' instVar: 'isActive' instVarClass: Boolean) ;
	add: (GsPostgresColumnMapEntry newForColumn: 'date' instVar: 'activeDate' instVarClass: self classForDate ) ;
	add: (GsPostgresColumnMapEntry newForColumn: 'time' instVar: 'activeTime' instVarClass: self classForTime ) ;
	add: (GsPostgresColumnMapEntry newForColumn: 'name_sb' instVar: 'nameSb' instVarClass: String ) ;
	add: (GsPostgresColumnMapEntry newForColumn: 'name_db' instVar: 'nameDb' instVarClass: DoubleByteString ) ;
	add: (GsPostgresColumnMapEntry newForColumn: 'name_qb' instVar: 'nameQb' instVarClass: QuadByteString );
	add: (GsPostgresColumnMapEntry newForColumn: 'balance' instVar: 'balance' instVarClass: self classForScaledDecimal ) ;
	add: (GsPostgresColumnMapEntry newForColumn: 'encrypted_data' instVar: 'encryptedData' instVarClass: ByteArray );
	yourself
%
category: 'Postgres Support'
classmethod: WidgetWithStrings
rdbPrimaryKeyMaps


^ Array new
	add: (GsPostgresColumnMapEntry newForColumn: 'id' instVar: 'id' ) ;
	yourself
%
category: 'Postgres Support'
classmethod: WidgetWithStrings
rdbTableName


^ (System myUserProfile resolveSymbol: #PostgresTestCase) value widgetTableName
%
! ------------------- Instance methods for WidgetWithStrings
category: 'Comparing'
method: WidgetWithStrings
= another

id = another id ifFalse:[ ^ self unequal ].
lastUpdate = another lastUpdate ifFalse:[ ^ self unequal ].
lastUpdateNoTz = another lastUpdateNoTz ifFalse:[ ^ self unequal ].
isActive = another isActive ifFalse:[ ^ self unequal ].
activeDate = another activeDate ifFalse:[ ^ self unequal ].
activeTime = another activeTime ifFalse:[ ^ self unequal ].
nameSb = another nameSb ifFalse:[ ^ self unequal ].
nameDb = another nameDb ifFalse:[ ^ self unequal ].
nameQb = another nameQb ifFalse:[ ^ self unequal ].
balance = another balance ifFalse:[ ^ self unequal ].
encryptedData = another encryptedData ifFalse:[ ^ self unequal ].
^true
%
category: 'Accessing'
method: WidgetWithStrings
activeDate
	^activeDate
%
category: 'Updating'
method: WidgetWithStrings
activeDate: newValue
	activeDate := newValue
%
category: 'Accessing'
method: WidgetWithStrings
activeTime
	^activeTime
%
category: 'Updating'
method: WidgetWithStrings
activeTime: newValue
	activeTime := newValue
%
category: 'Accessing'
method: WidgetWithStrings
balance
	^balance
%
category: 'Updating'
method: WidgetWithStrings
balance: newValue
	balance := newValue
%
category: 'Accessing'
method: WidgetWithStrings
encryptedData
	^encryptedData
%
category: 'Updating'
method: WidgetWithStrings
encryptedData: newValue
	encryptedData := newValue
%
category: 'Accessing'
method: WidgetWithStrings
id
	^id
%
category: 'Updating'
method: WidgetWithStrings
id: newValue
	id := newValue
%
category: 'Initialize'
method: WidgetWithStrings
initialize

rdbPostLoadCalled := false.
^ self
%
category: 'Accessing'
method: WidgetWithStrings
isActive
	^isActive
%
category: 'Updating'
method: WidgetWithStrings
isActive: newValue
	isActive := newValue
%
category: 'Accessing'
method: WidgetWithStrings
lastUpdate
	^lastUpdate
%
category: 'Updating'
method: WidgetWithStrings
lastUpdate: newValue
	lastUpdate := newValue
%
category: 'Accessing'
method: WidgetWithStrings
lastUpdateNoTz
	^lastUpdateNoTz
%
category: 'Updating'
method: WidgetWithStrings
lastUpdateNoTz: newValue
	lastUpdateNoTz := newValue
%
category: 'Accessing'
method: WidgetWithStrings
nameDb
	^nameDb
%
category: 'Updating'
method: WidgetWithStrings
nameDb: newValue
	nameDb := newValue
%
category: 'Accessing'
method: WidgetWithStrings
nameQb
	^nameQb
%
category: 'Updating'
method: WidgetWithStrings
nameQb: newValue
	nameQb := newValue
%
category: 'Accessing'
method: WidgetWithStrings
nameSb
	^nameSb
%
category: 'Updating'
method: WidgetWithStrings
nameSb: newValue
	nameSb := newValue
%
category: 'RDB Support'
method: WidgetWithStrings
rdbPostLoad

rdbPostLoadCalled := true.
^ self
%
category: 'Accessing'
method: WidgetWithStrings
rdbPostLoadCalled
	^rdbPostLoadCalled
%
category: 'Updating'
method: WidgetWithStrings
rdbPostLoadCalled: newValue
	rdbPostLoadCalled := newValue
%
category: 'Comparing'
method: WidgetWithStrings
unequal

"A place to set breakpoints"
^ false
%
! ------------------- Remove existing behavior from WidgetWithUnicode
removeAllMethods WidgetWithUnicode
removeAllClassMethods WidgetWithUnicode
! ------------------- Class methods for WidgetWithUnicode
category: 'Class Mapping Support'
classmethod: WidgetWithUnicode
convertToDbStrings: anArray

^anArray collect:[:e| Unicode16 withAll: e ]
%
category: 'Class Mapping Support'
classmethod: WidgetWithUnicode
convertToQbStrings: anArray

^anArray collect:[:e| Unicode32 withAll: e ]
%
category: 'Class Mapping Support'
classmethod: WidgetWithUnicode
convertToSbStrings: anArray

^anArray collect:[:e| Unicode7 withAll: e ]
%
category: 'Class Mapping Support'
classmethod: WidgetWithUnicode
idFromInt: anInt

^anInt asString
%
category: 'Class Mapping Support'
classmethod: WidgetWithUnicode
lastUpdateClass
	^ DateTime
%
category: 'Class Mapping Support'
classmethod: WidgetWithUnicode
lastUpdateToLastUpdateNoTz: aTimeObj

^aTimeObj
%
category: 'Class Mapping Support'
classmethod: WidgetWithUnicode
randomBalance

"WidgetWithUnicode randomBalance"
^ Float fromString: super randomBalance asString
%
category: 'Postgres Support'
classmethod: WidgetWithUnicode
rdbColumnMapping

^ Array new
	add: (GsPostgresColumnMapEntry newForColumn: 'id' instVar: 'id' instVarClass: String) ;
	add: (GsPostgresColumnMapEntry newForColumn: 'last_update' instVar: 'lastUpdate' instVarClass: DateTime ) ;
	add: (GsPostgresColumnMapEntry newForColumn: 'last_update_notz' instVar: 'lastUpdateNoTz' instVarClass: DateTime ) ;
	add: (GsPostgresColumnMapEntry newForColumn: 'is_active' instVar: 'isActive' instVarClass: Boolean) ;
	add: (GsPostgresColumnMapEntry newForColumn: 'date' instVar: 'activeDate' instVarClass: self classForDate ) ;
	add: (GsPostgresColumnMapEntry newForColumn: 'time' instVar: 'activeTime' instVarClass: self classForTime) ;
	add: (GsPostgresColumnMapEntry newForColumn: 'name_sb' instVar: 'nameSb' instVarClass: Unicode7 ) ;
	add: (GsPostgresColumnMapEntry newForColumn: 'name_db' instVar: 'nameDb' instVarClass: Unicode16 ) ;
	add: (GsPostgresColumnMapEntry newForColumn: 'name_qb' instVar: 'nameQb' instVarClass: Unicode32 );
	add: (GsPostgresColumnMapEntry newForColumn: 'balance' instVar: 'balance' instVarClass: self classForFloat ) ;
	add: (GsPostgresColumnMapEntry newForColumn: 'encrypted_data' instVar: 'encryptedData' instVarClass: ByteArray );
	yourself
%
! ------------------- Instance methods for WidgetWithUnicode
! ------------------- Remove existing behavior from PostgresTestCase
removeAllMethods PostgresTestCase
removeAllClassMethods PostgresTestCase
! ------------------- Class methods for PostgresTestCase
category: 'Parameters'
classmethod: PostgresTestCase
defaultParameters

	^(GsPostgresConnectionParameters new)
		host: 'localhost';
		port: 5432;
		dbname: 'demo';
		connect_timeout: 10;
		yourself
%
category: 'Utilities'
classmethod: PostgresTestCase
doubleByteStringFromCodePointArray: anArray

| sz result|
sz := anArray size.
result := DoubleByteString new: sz .
1 to: sz do:[:n| result codePointAt: n put: (anArray at: n) ].
^ result
%
category: 'Utilities'
classmethod: PostgresTestCase
quadByteStringFromCodePointArray: anArray

| sz result|
sz := anArray size.
result := QuadByteString new: sz .
1 to: sz do:[:n| result codePointAt: n put: (anArray at: n) ].
^ result
%
category: 'SQL Generation'
classmethod: PostgresTestCase
sqlForCreateTableNamed: aName columnNames: colArray columnTypes: typeArray

"
PostgresTestCase sqlForCreateTableNamed: 'table1' columnTypes: { 'character(3)' . 'timestamp with time zone' }
"

	| ws |
	ws := AppendStream on: String new.
	ws
		nextPutAll: 'CREATE TABLE ';
		nextPutAll: aName;
		nextPutAll: ' ('.
	1 to: colArray size
		do:
			[:n |
			n > 1
				ifTrue:
					[ws
						nextPut: $,;
						space].
			ws
				nextPutAll: (colArray at: n)  ;
				space;
				nextPutAll: (typeArray at: n)  ].
	^ws
		nextPutAll: ');';
		contents
%
category: 'SQL Generation'
classmethod: PostgresTestCase
sqlForCreateTableNamed: aName columnTypes: typesArray

	| colNames sz |
	sz := typesArray size.
	colNames := Array new: sz.
	1 to: sz do: [:n | colNames at: n put: 'column' , n asString].
	^self
		sqlForCreateTableNamed: aName
		columnNames: colNames
		columnTypes: typesArray
%
category: 'SQL Generation'
classmethod: PostgresTestCase
sqlForDeleteFromTable: aName column: colName value: value
"
PostgresTestCase sqlForDeleteFromTable: 'table1' column: 'column1' value: 'abc'
"

|ws|
ws := AppendStream on: String new.
^ ws nextPutAll: 'DELETE FROM ' ;
	nextPutAll: aName ;
	nextPutAll: ' WHERE ';
	nextPutAll: colName ;
	nextPutAll: ' = ' ;
	nextPutAll: value ;
	nextPut: $; ;
	contents
%
category: 'SQL Generation'
classmethod: PostgresTestCase
sqlForDeleteFromTable: aName value: value

^ self sqlForDeleteFromTable: aName column: 'column1' value: value
%
category: 'SQL Generation'
classmethod: PostgresTestCase
sqlForDropTableNamed: aName
	"
PostgresTestCase sqlForDropTableNamed: 'table1'
"

	^'DROP TABLE ' , aName , ';'
%
category: 'SQL Generation'
classmethod: PostgresTestCase
sqlForInsertValue: aValue intoTableNamed: aName

"
PostgresTestCase sqlForInsertValue: 'abc' intoTableNamed: 'table1'
"

	| ws |
	ws := AppendStream on: String new.
	^ ws
		nextPutAll: 'INSERT INTO ';
		nextPutAll: aName;
		nextPutAll: ' VALUES (';
		nextPutAll: aValue ;
		nextPutAll: ');' ;
		contents
%
category: 'SQL Generation'
classmethod: PostgresTestCase
sqlForInsertValues: valueArray intoTableNamed: aName
	"
PostgresTestCase sqlForInsertValue: 'abc' intoTableNamed: 'table1'
"

	| ws |
	ws := AppendStream on: String new.
	ws
		nextPutAll: 'INSERT INTO ';
		nextPutAll: aName;
		nextPutAll: ' VALUES ('.
	1 to: valueArray size
		do:
			[:n |
			n == 1 ifFalse: [ws nextPutAll: ', '].
			ws nextPutAll: (GsPostgresWriteStream postgresStringForObject: (valueArray at: n) escaped: true )].
	^ws
		nextPutAll: ');';
		contents
%
category: 'SQL Generation'
classmethod: PostgresTestCase
sqlForSelectAllFromTable: aName

"
PostgresTestCase sqlForSelectAllFromTable: 'table1'
"

^ 'SELECT * FROM ', aName, ';'

%
category: 'SQL Generation'
classmethod: PostgresTestCase
sqlForSelectAllFromTable: aName orderBy: colName

"
PostgresTestCase sqlForSelectAllFromTable: 'table1'
"

^ 'SELECT * FROM ', aName, ' order by ', colName, ' asc ;'

%
category: 'SQL Generation'
classmethod: PostgresTestCase
sqlForUpdateTable: aName column: colName oldValue: oldVal newValue: newVal
"
PostgresTestCase sqlForUpdateTable: 'table1' column: 'column1' oldValue: 'abc' newValue: 'def'
"

|ws|
ws := AppendStream on: String new.
^ ws nextPutAll: 'UPDATE ' ;
	nextPutAll: aName ;
	nextPutAll: ' SET ';
	nextPutAll: colName ;
	nextPutAll: ' = ' ;
	nextPutAll: newVal ;
	nextPutAll: ' WHERE ';
	nextPutAll: colName ;
	nextPutAll: ' = ' ;
	nextPutAll: oldVal ;
	nextPut: $; ;
	contents
%
category: 'SQL Generation'
classmethod: PostgresTestCase
sqlForUpdateTable: aName oldValue: oldVal newValue: newVal

^ self sqlForUpdateTable: aName column: 'column1' oldValue: oldVal newValue: newVal
%
category: 'Widget Table'
classmethod: PostgresTestCase
widgetTableAllBalances

^ { 55.67 . 456.89 . 2342.98 . 2.3 . 345.58 }
%
category: 'Widget Table'
classmethod: PostgresTestCase
widgetTableAllColumns

	| result |
	result := Array new.
	^result
		add: self widgetTableAllIds;
		add: self widgetTableAllLastUpdates;
		add: self widgetTableAllLastUpdates;
		add: self widgetTableAllIsActive;
		add: self widgetTableAllDates;
		add: self widgetTableAllTimes;
		add: self widgetTableAllNames_sbs;
		add: self widgetTableAllNames_dbs;
		add: self widgetTableAllNames_qbs;
		add: self widgetTableAllBalances;
		add: self widgetTableAllEncryptedData;
		yourself
%
category: 'Widget Table'
classmethod: PostgresTestCase
widgetTableAllDates

| date result |
date := Date newDay: 266 year: 2021.
result := Array new: self widgetTableNumRows.
result at: 1 put: date.
2 to: self widgetTableNumRows do:[:n| result at: n put: (date addDays: n) ] .
^ result
%
category: 'Widget Table'
classmethod: PostgresTestCase
widgetTableAllEncryptedData

| result |
result := Array new.
result add: (ByteArray fromHexString: 'F909A4CC2961A98EAB76D673D1DD2D14D261AE1101F8D1C3D13FBA458A43477C') ;
 add: (ByteArray fromHexString: '618B443D7CAF048E88694C76B26B3EF4A490C85398AF8CACA1FBE792450DC6D7') ;
 add: (ByteArray fromHexString:  '2C346B8506F2D235960E54ABDA950DAEB677C51C46A7756DFCB5AEAE50E430E1');
 add: (ByteArray fromHexString:  '771BC69D0CD628396A787905A36FC70866879E90464C23958AB20706CD6FE302' );
 add: (ByteArray fromHexString:  '4EB92CEF07E59B54D79F663605AC19BABE6E04FF7838736513D34FE0B1823A5B' ).
^ result
%
category: 'Widget Table'
classmethod: PostgresTestCase
widgetTableAllIds

^ {  1001 . 1002 . 1003 . 1004 . 1005 }
%
category: 'Widget Table'
classmethod: PostgresTestCase
widgetTableAllIsActive

^ { true . true . true . false . true }
%
category: 'Widget Table'
classmethod: PostgresTestCase
widgetTableAllLastUpdates

"PostgresTestCase widgetTableAllLastUpdates"
| now result |
now := DateAndTime year: 2021 month: 3 day: 31 hour: 14 minute: 25 second: 43 .
result := Array new: self widgetTableNumRows.
result at: 1 put: now.
2 to: self widgetTableNumRows do:[:n| result at: n put: (now + (Duration seconds: (n * 600))) ].
^ result
%
category: 'Widget Table'
classmethod: PostgresTestCase
widgetTableAllNames_dbs

| codePointArray sz result |
codePointArray := self widgetTableDoubleByteCodePoints .
sz := codePointArray size .
result := Array new: sz.
1 to: sz do:[:n| result at: n put: (self doubleByteStringFromCodePointArray: (codePointArray at: n))].
^ result
%
category: 'Widget Table'
classmethod: PostgresTestCase
widgetTableAllNames_qbs

| codePointArray sz result |
codePointArray := self widgetTableQuadByteCodePoints .
sz := codePointArray size .
result := Array new: sz.
1 to: sz do:[:n| result at: n put: (self quadByteStringFromCodePointArray: (codePointArray at: n))].
^ result
%
category: 'Widget Table'
classmethod: PostgresTestCase
widgetTableAllNames_sbs

^ { 'Foster' . 'Ware' . 'Green' . 'Otis' . 'Singh' }
%
category: 'Widget Table'
classmethod: PostgresTestCase
widgetTableAllTimes

| time result |
time := Time fromSeconds: 3722 .
result := Array new: self widgetTableNumRows.
result at: 1 put: time.
2 to: self widgetTableNumRows do:[:n| result at: n put: (time addSeconds: n * 3600) ] .
^ result
%
category: 'Widget Table'
classmethod: PostgresTestCase
widgetTableColumnNames

	^
	{'id'.
	'last_update'.
	'last_update_notz' .
	'is_active'.
	'date'.
	'time'.
	'name_sb'.
	'name_db'.
	'name_qb'.
	'balance'.
	'encrypted_data'}
%
category: 'Widget Table'
classmethod: PostgresTestCase
widgetTableColumnTypes

	^
	{'integer primary key'.
	'timestamp with time zone'.
	'timestamp without time zone'.
	'boolean'.
	'date'.
	'time'.
	'varchar(30)'.
	'varchar(30)'.
	'varchar(30)'.
	'money'.
	'bytea'}
%
category: 'Widget Table'
classmethod: PostgresTestCase
widgetTableDoubleByteCodePoints

^ {

{ 16r92eb . 16r72aa . 16r5f86 . 16rb36c . 16r2746 . 16rff46 . 16ra932 . 16rf4d3 .
16r7016 . 16re3bf . 16reebc . 16r1112 . 16rbed0 . 16r0548 . 16r4a07 . 16r3590 .
16raab4 . 16re728 . 16ra59a . 16rc47e } .

{ 16raaee . 16r137c . 16rcf70 . 16r6220 . 16raf1b . 16r4127 . 16r9bfc . 16ra138 .
  16r7d74 . 16ra842 . 16r9f67 . 16r5faa . 16r2d62 . 16r11f9 . 16rb1db . 16r530b .
  16r1b45 . 16r4590 . 16r9707 . 16rc72b } .

{ 16r5090 . 16r8f9c . 16r9cc2 . 16rfee1 . 16re0b3 . 16r7c66 . 16rea35 . 16r1ab4 .
  16r6711 . 16rb8fb . 16r9a29 . 16r1236 . 16r8da7 . 16ra5a1 . 16r137b . 16r2c90 .
  16rc26a . 16rf01b . 16ra11d . 16rf955 } .

{ 16rfd19 . 16r132b . 16r7ace . 16rea7e . 16r888a . 16r30ee . 16rb28d . 16r90a3 .
  16rb371 . 16rb00c . 16r4d54 . 16r544d . 16r5eb0 . 16rb6fc . 16r134f . 16r146a .
  16r2c89 . 16r2b83 . 16re900 . 16rf10f } .

{ 16r9403 . 16r146f . 16r3a73 . 16r6640 . 16raf7b . 16r08bb . 16r8507 . 16rc538 .
  16rabb5 . 16r0ecf . 16r9a22 . 16rfce6 . 16raf90 . 16r955d . 16r7f94 . 16r51d5 .
  16r70c3 . 16r248f . 16rf08f . 16r62f6 }
}
%
category: 'Widget Table'
classmethod: PostgresTestCase
widgetTableInsertStatements

"PostgresTestCase widgetTableInsertStatements"

| allCols numRows result tableName |
allCols := self widgetTableAllColumns .
tableName := self widgetTableName .
numRows := allCols first size .
result := Array new.
1 to: numRows do:[:n| |values|
	values :=allCols collect:[:e| e at: n ].
	result add: (self sqlForInsertValues: values intoTableNamed: tableName).
].
^ result

%
category: 'Widget Table'
classmethod: PostgresTestCase
widgetTableName
	^'widget_table'
%
category: 'Widget Table'
classmethod: PostgresTestCase
widgetTableNumColumns

"Number of columns  in widget table for this test"
	^ 11
%
category: 'Widget Table'
classmethod: PostgresTestCase
widgetTableNumRows

"Number of rows created in widget table for this test"
	^ 5
%
category: 'Widget Table'
classmethod: PostgresTestCase
widgetTableQuadByteCodePoints

^ {
{ 16r20779 . 16r20C53 . 16r20C78 . 16r20C96 . 16r20CCF .
  16r20CD5 . 16r20D15 . 16r20D7C . 16r20D7F . 16r20E0E .
  16r20E0F . 16r20E77 . 16r20E9D . 16r20EA2 . 16r20ED7 .
  16r20EF9 . 16r20EFA . 16r20F2D . 16r20F2E . 16r20F4C } .

{ 16r20FB4 . 16r20FBC . 16r20FEA . 16r2105C . 16r2106F .
  16r21075 . 16r21076 . 16r2107B . 16r210C1 . 16r210C9 .
  16r211D9 . 16r220C7 . 16r227B5 . 16r22AD5 . 16r22B43 .
  16r22BCA . 16r22C51 . 16r22C55 . 16r22CC2 . 16r22D08 } .

{ 16r22D4C . 16r22D67 . 16r22EB3 . 16r23CB7 . 16r244D3 .
  16r24DB8 . 16r24DEA . 16r2512B . 16r26258 . 16r267CC .
  16r269F2 . 16r269FA . 16r27A3E . 16r2815D . 16r28207 .
  16r282E2 . 16r28CCA . 16r28CCD . 16r28CD2 . 16r29D98 } .

{ 16r20779 . 16r20C53 . 16r20C78 . 16r20C96 . 16r20CCF .
  16r20FB4 . 16r20FBC . 16r20FEA . 16r2105C . 16r2106F .
  16r22D4C . 16r22D67 . 16r22EB3 . 16r23CB7 . 16r244D3 .
  16r20CD5 . 16r20D15 . 16r20D7C . 16r20D7F . 16r20E0E } .

{ 16r20E0F . 16r20E77 . 16r20E9D . 16r20EA2 . 16r20ED7 .
  16r20EF9 . 16r20EFA . 16r20F2D . 16r20F2E . 16r20F4C .
  16r211D9 . 16r220C7 . 16r227B5 . 16r22AD5 . 16r22B43 .
  16r22BCA . 16r22C51 . 16r22C55 . 16r22CC2 . 16r22D08 }
}
%
! ------------------- Instance methods for PostgresTestCase
category: 'Transactions'
method: PostgresTestCase
begin

self deny: self inTransaction.
self connection beginTransaction.
self assert: self inTransaction.
^self
%
category: 'Transactions'
method: PostgresTestCase
commit

self assert: self inTransaction.
self connection commitTransaction.
self deny: self inTransaction.
^self
%
category: 'Accessing'
method: PostgresTestCase
connection
	^connection
%
category: 'Updating'
method: PostgresTestCase
connection: newValue
	connection := newValue
%
category: 'Tables'
method: PostgresTestCase
createTableNamed: aName columnNames: colArray columnTypes: typeArray

	^self
		assert: (self connection
					executeNoResults: (self class sqlForCreateTableNamed: aName
					columnNames: colArray
							columnTypes: typeArray))
			identical: self connection;
		yourself
%
category: 'Tables'
method: PostgresTestCase
createTableNamed: aName columnTypes: anArray

	^self
		assert: (self connection
					executeNoResults: (self class sqlForCreateTableNamed: aName
							columnTypes: anArray))
			identical: self connection;
		yourself
%
category: 'Tables'
method: PostgresTestCase
createTableNamed: aName withColumnType: aString

	^self
		assert: (self connection
					executeNoResults: (self class sqlForCreateTableNamed: aName
							columnTypes: { aString } ))
			identical: self connection;
		yourself
%
category: 'Setup'
method: PostgresTestCase
createWidgetTable
	"Create an empty table in Postgres for testing tuple classes"

	| tableName |
	tableName := self class widgetTableName.
	^self
		dropTableNamed: tableName;
		createTableNamed: tableName
			columnNames: self class widgetTableColumnNames
			columnTypes: self class widgetTableColumnTypes;
		yourself
%
category: 'Tables'
method: PostgresTestCase
deleteFromTable: table value: value

	| str result |
	str := self class sqlForDeleteFromTable: table  value: value .
	^self
		assert: str class identical: String;
		assert: (result := self connection executeNoResults: str)
			identical: self connection;
		yourself
%
category: 'Tables'
method: PostgresTestCase
dropTableNamed: aName

	^(self tableNameExists: aName)
		ifTrue:
			[self
				assert: (self connection
							executeNoResults: (self class sqlForDropTableNamed: aName))
					identical: self connection;
				yourself]
		ifFalse: [self]
%
category: 'Setup'
method: PostgresTestCase
dropWidgetTable
	"Create an empty table in Postgres for testing tuple classes"

	| tableName |
	tableName := self class widgetTableName.
	^self
		dropTableNamed: tableName ;
		yourself
%
category: 'Accessing'
method: PostgresTestCase
errorNum
	^errorNum
%
category: 'Updating'
method: PostgresTestCase
errorNum: newValue
	errorNum := newValue
%
category: 'Testing'
method: PostgresTestCase
hasReadStream: rs

^ GsPostgresConnection hasReadStream: rs forConnection: self connection
%
category: 'Testing'
method: PostgresTestCase
hasWriteStream: ws

^ GsPostgresConnection hasWriteStream: ws forConnection: self connection
%
category: 'Tables'
method: PostgresTestCase
insertValue: value intoTable: table

| str |
str := self class sqlForInsertValue: value  intoTableNamed: table.
^ self assert: str class identical: String ;
	assert: ( self connection executeNoResults: str) identical: self connection ;
	yourself

%
category: 'Transactions'
method: PostgresTestCase
inTransaction

^ self connection inTransaction.
%
category: 'Setup'
method: PostgresTestCase
populateWidgetTable

"Populate a table in Postgres for testing tuple classes"

	| tableName inserts |
	tableName := self class widgetTableName.
	inserts := self class widgetTableInsertStatements .
	inserts do:[:e| self connection executeNoResults: e ].
	^ self
%
category: 'Transactions'
method: PostgresTestCase
rollback

self assert: self inTransaction.
self connection rollbackTransaction.
self deny: self inTransaction.
^self
%
category: 'Tables'
method: PostgresTestCase
selectAllFromTable: table

| str result |
str := self class sqlForSelectAllFromTable: table.

 self assert: str class identical: String ;
	assert: (result := self connection execute: str) class identical: GsPostgresReadStream .
^ result

%
category: 'Tests (private)'
method: PostgresTestCase
setUnicodeStrings: boolean while: aBlock

	| save |
	save := self connection unicodeStrings.
	^ [self connection unicodeStrings: boolean.
	aBlock value]
			ensure: [self connection unicodeStrings: save]
%
category: 'Setup'
method: PostgresTestCase
setUp

	super setUp.
	connection := GsPostgresConnection
				newWithParameters: self class defaultParameters.
	^self
		assert: connection connect;
		assert: connection connected;
		errorNum: GsPostgresConnection errorNumber;
		yourself
%
category: 'Tables'
method: PostgresTestCase
tableNameExists: aTableName

| ws |
ws := AppendStream on: String new.
ws nextPutAll: 'SELECT EXISTS (SELECT 1 FROM information_schema.tables WHERE  table_name = ';
	nextPut: $' ;
	nextPutAll: aTableName ;
	nextPut: $' ;
	nextPutAll: ');'.

^ self connection executeAndReturnOneRow: ws contents
%
category: 'Tables'
method: PostgresTestCase
tableNameForClass: aClass

^ ('test_', aClass printString) asLowercase
%
category: 'Setup'
method: PostgresTestCase
tearDown


	super tearDown.
	self connection ifNotNil: [self connection disconnect].
	^self
%
category: 'Tests'
method: PostgresTestCase
test_Boolean

	| pgType cls createBlock updateBlock |
	cls := Boolean .
	pgType := 'boolean'.
	createBlock :=  [:cls | true ].
	updateBlock := [:obj | obj not ].
	self
		_test_pgType: pgType
		withGsClass: cls
		gsCreateBlock: createBlock
		gsUpdateBlock: updateBlock.
	^self
%
category: 'Tests'
method: PostgresTestCase
test_ByteArray

	| pgType cls createBlock updateBlock |
	cls := ByteArray .
	pgType := 'bytea'.
	createBlock :=  [:cls | cls fromHexString: 'ff00aa' ].
	updateBlock := [:obj | obj class  fromHexString: 'ee0099' ].
	self
		_test_pgType: pgType
		withGsClass: cls
		gsCreateBlock: createBlock
		gsUpdateBlock: updateBlock.
	^self
%
category: 'Tests'
method: PostgresTestCase
test_characterEncoding

self assert: (self connection characterEncoding == #UTF8) .
^ self
%
category: 'Tests'
method: PostgresTestCase
test_Date

	| pgType cls createBlock updateBlock |
	cls := Globals at: #SmallDate ifAbsent: [Date].
	pgType := 'date'.
	createBlock := [:cls | cls today].
	updateBlock := [:obj | obj addDays: 7].
	self
		_test_pgType: pgType
		withGsClass: cls
		gsCreateBlock: createBlock
		gsUpdateBlock: updateBlock.
	^self
%
category: 'Tests'
method: PostgresTestCase
test_DateAndTime

	| pgTypes d cls createBlocks updateBlock |
	cls := Globals at: #SmallDateAndTime ifAbsent: [DateAndTime].
	pgTypes :=
			{'timestamp with time zone'.
			'timestamp without time zone'}.
	createBlocks :=
			{[:cls | cls now].
			[:cls | cls now offset: Duration zero]}.
	updateBlock := [:obj | obj + (Duration seconds: 3600 * 24)].
	100 timesRepeat:
			["Run 100 times to ensure milli/micro seconds are correct"
			1 to: pgTypes size
				do:
					[:n |
					| pgType blk |
					pgType := pgTypes at: n.
					blk := createBlocks at: n.
					self
						_test_pgType: pgType
						withGsClass: cls
						gsCreateBlock: blk
						gsUpdateBlock: updateBlock]].
	^self
%
category: 'Tests'
method: PostgresTestCase
test_DateTime

	| pgTypes d cls createBlocks updateBlock |
	cls := DateTime.
	pgTypes :=
			{'timestamp with time zone'.
			'timestamp without time zone'}.
	createBlocks :=
			{[:cls | cls now].
			[:cls | cls now]}.
	updateBlock := [:obj | obj addSeconds: 3600].
	100 timesRepeat:
			["Run 100 times to ensure milli/micro seconds are correct"
			1 to: pgTypes size
				do:
					[:n |
					| pgType blk |
					pgType := pgTypes at: n.
					blk := createBlocks at: n.
					self
						_test_pgType: pgType
						withGsClass: cls
						gsCreateBlock: blk
						gsUpdateBlock: updateBlock
						overrideClass: true	"Override, otherwise we map timestamps to DateAndTime, not DateTime."]].
	^self
%
category: 'Tests'
method: PostgresTestCase
test_DoubleByteString

	^self setUnicodeStrings: false
		while:
			[self
				_test_TwoByteString: DoubleByteString;
				_test_TwoByteString2: DoubleByteString]
%
category: 'Tests'
method: PostgresTestCase
test_emptyString

	| pgType tableName str rs oc obj |
	tableName := 'empty_string_table'.
	pgType := 'varchar(20)'.
	
	[self
		dropTableNamed: tableName;
		createTableNamed: tableName withColumnType: pgType;
		assert: (str := GsPostgresWriteStream postgresStringForObject: String new
							escaped: true) equals: '' quoted ;
		insertValue: str intoTable: tableName;
		assert: (rs := self selectAllFromTable: tableName) class
			identical: GsPostgresReadStream;
		assert: (oc := rs next) class identical: OrderedCollection;
		assert: oc size identical: 1;
		assert: (obj := oc first) equals: String new ]
			ensure: 
				[rs ifNotNil: [rs free].
				self dropTableNamed: tableName].
	^self
%
category: 'Tests'
method: PostgresTestCase
test_execute

	|  rs obj |

	^[self
		createWidgetTable ;
		populateWidgetTable ;
		should:[self connection execute: 'garbage' ] raise: self errorNum ;
		assert: ((rs := self connection execute: ('select * from ', self class widgetTableName )) class) equals: GsPostgresReadStream ;
		assert: (self hasReadStream: rs) ;
		assert: (obj := rs next) class equals: OrderedCollection ;
		assert: obj size equals: self class widgetTableNumColumns;
		assert: rs numColumns equals: self class widgetTableNumColumns  ;
		assert: rs readLimit equals: self class widgetTableNumRows  ;
		assert: rs position equals: 1;
		deny: rs atBeginning ;
		deny: rs atEnd;
		assert: rs beforeEnd ;
		assert: rs isExternal ;
		assert: rs free equals: rs ;
		deny: (self hasReadStream: rs) ;
		yourself ] ensure:[ self dropWidgetTable ]
%
category: 'Tests'
method: PostgresTestCase
test_executeReturnRowsAffected

	| cmd |
	cmd := String
				withAll: 'SELECT pg_catalog.set_config(''search_path'','''',false)'.
	^self
		assert: (self connection executeReturnRowsAffected: cmd) equals: 1;
		should: [self connection executeReturnRowsAffected: 'garbage'] raise: self errorNum ;
		yourself
%
category: 'Tests'
method: PostgresTestCase
test_fetchTupleFromPostgres

	^
	[self
		createWidgetTable;
		populateWidgetTable;
		_test_fetchTuplesFromPostgresForTupleClassName: #WidgetWithStrings;
		_test_fetchTuplesFromPostgresForTupleClassName: #WidgetWithUnicode ;
		_test_fetchTuplesFromPostgresForTupleClassName: #WidgetWithStringsOldColumnMap;
		_test_fetchTuplesFromPostgresForTupleClassName: #WidgetWithUnicodeOldColumnMap ]
			ensure: [self dropWidgetTable]
%
category: 'Tests'
method: PostgresTestCase
test_Float

	| pgType cls createBlock updateBlock |
	cls := Globals at: #SmallDouble ifAbsent: [Float].
	pgType := 'double precision'.
	createBlock :=  [:cls | cls fromString: '3.1415927' ].
	updateBlock := [:obj | obj negated ].
	self
		_test_pgType: pgType
		withGsClass: cls
		gsCreateBlock: createBlock
		gsUpdateBlock: updateBlock.
	^self
%
category: 'Tests'
method: PostgresTestCase
test_insertUpdateDeleteTuples

	^self
		_test_insertUpdateDeleteTuplesFromPostgresForTupleClassName: #WidgetWithStrings ;
		_test_insertUpdateDeleteTuplesFromPostgresForTupleClassName: #WidgetWithUnicode ;
		yourself
%
category: 'Tests'
method: PostgresTestCase
test_LargeInteger

	| pgType cls createBlock updateBlock |
	cls := LargeInteger .
	pgType := 'bigint'.
	createBlock :=  [:cls | LargeInteger fromString: '9223372036854775807' ].
	updateBlock := [:obj | obj negated - 1 ].
	self
		_test_pgType: pgType
		withGsClass: cls
		gsCreateBlock: createBlock
		gsUpdateBlock: updateBlock.
	^self
%
category: 'Tests'
method: PostgresTestCase
test_null

	| pgType tableName str rs oc obj |
	tableName := 'null_table'.
	pgType := 'varchar(20)'.
	
	[self
		dropTableNamed: tableName;
		createTableNamed: tableName withColumnType: pgType;
		assert: (str := GsPostgresWriteStream postgresStringForObject: nil
							escaped: true) equals: 'NULL' ;
		insertValue: str intoTable: tableName;
		assert: (rs := self selectAllFromTable: tableName) class
			identical: GsPostgresReadStream;
		assert: (oc := rs next) class identical: OrderedCollection;
		assert: oc size identical: 1;
		assert: (obj := oc first) identical: nil ]
			ensure: 
				[rs ifNotNil: [rs free].
				self dropTableNamed: tableName].
	^self
%
category: 'Tests'
method: PostgresTestCase
test_QuadByteString

	^self setUnicodeStrings: false
		while: [self _test_FourByteString: QuadByteString]
%
category: 'Tests'
method: PostgresTestCase
test_SmallInteger

	| pgType cls createBlock updateBlock |
	cls := SmallInteger .
	pgType := 'bigint'.
	createBlock :=  [:cls | cls maximumValue ].
	updateBlock := [:obj | obj class maximumValue negated - 1 ].
	self
		_test_pgType: pgType
		withGsClass: cls
		gsCreateBlock: createBlock
		gsUpdateBlock: updateBlock.
	^self
%
category: 'Tests'
method: PostgresTestCase
test_Time

	| pgTypes d cls createBlocks updateBlock |
	cls := Globals at: #SmallTime ifAbsent: [Time].
	pgTypes :=
			{'time with time zone'.
			'time without time zone' }.
	createBlocks :=
			{[:cls | cls now] .
			[:cls | cls now ]
}.
	updateBlock := [:obj | obj addSeconds: 3600 * 24 ].
	1 to: pgTypes size
		do:
			[:n |
			| pgType blk |
			pgType := pgTypes at: n.
			blk := createBlocks at: n.
			self
				_test_pgType: pgType
				withGsClass: cls
				gsCreateBlock: blk
				gsUpdateBlock: updateBlock].
	^self
%
category: 'Tests'
method: PostgresTestCase
test_Unicode16

	^self setUnicodeStrings: true
		while:
			[self
				_test_TwoByteString: Unicode16;
				_test_TwoByteString2: Unicode16]
%
category: 'Tests'
method: PostgresTestCase
test_Unicode32

	^self setUnicodeStrings: true
		while: [self _test_FourByteString: Unicode32]
%
category: 'Tests'
method: PostgresTestCase
test_version

self assert: (self connection libraryVersion class == SmallInteger).
^ self
%
category: 'Tables'
method: PostgresTestCase
updateTable: table oldValue: oldVal newValue: newVal

	| str result |
	str := self class sqlForUpdateTable: table oldValue: oldVal newValue: newVal.
	^self
		assert: str class identical: String;
		assert: (result := self connection executeNoResults: str)
			identical: self connection;
		yourself
%
category: 'Tests'
method: PostgresTestCase
validateTupleObject: obj
	"Scan the column map and ensure each inst var is a member of the right class"

	| collMap |
	collMap := obj class rdbColumnMapping.
	collMap
		ifNotNil:
			[self assert: obj rdbPostLoadCalled.
			(collMap first isMemberOf: Array)
				ifTrue: [self validateTupleObject: obj withOldCollMap: collMap]
				ifFalse: [self validateTupleObject: obj withNewCollMap: collMap]]
%
category: 'Tests'
method: PostgresTestCase
validateTupleObject: obj withNewCollMap: newMap
	"Scan the column map and ensure each inst var is a member of the right class"

	self assert: obj rdbPostLoadCalled.
	newMap do:
			[:entry |
			| iv |
			iv := obj perform: (GsPostgresColumnMapEntry getterSelectorForEntry: entry).
			self assert: iv class identical: entry instVarClass]
%
category: 'Tests'
method: PostgresTestCase
validateTupleObject: obj withOldCollMap: oldMap
	"Scan the column map and ensure each inst var is a member of the right class"

	self assert: obj rdbPostLoadCalled.
	oldMap do:
			[:entry |
			| iv |
			self assert: (entry isMemberOf: Array).
			iv := obj perform: (GsPostgresColumnMapEntry getterSelectorForEntry: entry) "getMethodSelector" ]
%
category: 'Tests (private)'
method: PostgresTestCase
_testWriteStreamRollbackForStream: ws withCollection: coll

self
	assert: (self hasWriteStream: ws);
	deny: ws hasUnflushedData ;
	assert: ws numTuplesFlushed identical: 0 ;
	deny: self inTransaction ;
	begin ;
	assert: self inTransaction .
1 to: coll size do:[:n|
	self 	assert: self inTransaction ;
		assert: ws position identical: (n - 1);
		assert: (ws nextPut: (coll at: n)) identical: ws;
		assert: ws position identical: n;
		assert: ws hasUnflushedData .
		(n \\ 10) == 0 ifTrue:[ 	self assert: ws flush identical: ws ].
].

self assert: self inTransaction ;
	rollback ;
	deny: self inTransaction.

ws numTuplesFlushed: 0 .

self
	begin ;
	assert: (self hasWriteStream: ws);
	deny: ws hasUnflushedData ;
	assert: ws numTuplesFlushed identical: 0 ;
	assert: (ws nextPutAll: coll) identical: ws;
	assert: ws numTuplesUnflushed identical: coll size ;
	assert: ws numTuplesFlushed identical: 0 ;
	assert: ws hasUnflushedData ;
	rollback ;
	deny: ws hasUnflushedData ;
	assert: ws numTuplesFlushed identical: 0 ;
	begin ;
	assert: (ws nextPutAll: coll) identical: ws;
	assert: ws numTuplesUnflushed identical: coll size ;
	assert: ws flush identical: ws ;
	deny: ws hasUnflushedData ;
	assert: ws numTuplesFlushed identical: coll size;
	rollback .
ws 	numTuplesFlushed: 0 .

^ self
%
category: 'Tests (private)'
method: PostgresTestCase
_test_fetchTuplesFromPostgresForTupleClass: aTupleClass readStream: rs

	self
		assert: rs class equals: GsPostgresReadStream;
		assert: (self hasReadStream: rs);
		deny: rs atEnd;
		assert: rs atBeginning;
		assert: rs beforeEnd;
		assert: rs isExternal;
		assert: rs position identical: 0;
		assert: rs readLimit identical: self class widgetTableNumRows.
	[rs atEnd] whileFalse:
			[| obj |
			obj := rs next.
			self
				deny: obj identical: nil;
				assert: obj class identical: aTupleClass;
				validateTupleObject: obj].
	^self
		assert: rs atEnd;
		deny: rs atBeginning;
		deny: rs beforeEnd;
		assert: rs isExternal;
		deny: rs position identical: 0;
		assert: rs position identical: rs readLimit;
		yourself
%
category: 'Tests (private)'
method: PostgresTestCase
_test_fetchTuplesFromPostgresForTupleClassName: aSymbol

	| rs assoc aTupleClass cmdStr |
	self
		deny: (assoc := System myUserProfile resolveSymbol: aSymbol) identical: nil;
		assert: (aTupleClass := assoc value) isBehavior.
	cmdStr := 'select * from ' , self class widgetTableName.

	[self
		assert: (rs := self connection execute: cmdStr tupleClass: aTupleClass) class equals: GsPostgresReadStream ;
		_test_fetchTuplesFromPostgresForTupleClass: aTupleClass readStream: rs .
		rs free .
		self deny: (self hasReadStream: rs).
		rs := nil.
	] ensure: [rs ifNotNil: [rs free]].

	[self
		assert: (rs := self connection openCursorOn: cmdStr) class equals: GsPostgresReadStream ;
		_test_fetchTuplesFromPostgresForTupleClass: OrderedCollection readStream: rs .
		rs free .
		self deny: (self hasReadStream: rs).
		rs := nil.
	] ensure: [rs ifNotNil: [rs free]].
	[self
		assert: (rs := self connection openCursorOn: cmdStr tupleClass: aTupleClass) class equals: GsPostgresReadStream ;
		_test_fetchTuplesFromPostgresForTupleClass: aTupleClass readStream: rs .
		rs free .
		self deny: (self hasReadStream: rs).
		rs := nil.
	] ensure: [rs ifNotNil: [rs free]].
	[self
		assert: (rs := self connection openCursorOn: cmdStr tupleClass: aTupleClass columnMapping: aTupleClass rdbColumnMapping) class equals: GsPostgresReadStream ;
		_test_fetchTuplesFromPostgresForTupleClass: aTupleClass readStream: rs .
		rs free .
		self deny: (self hasReadStream: rs).
		rs := nil.
	] ensure: [rs ifNotNil: [rs free]].
	^self
%
category: 'Tests (private)'
method: PostgresTestCase
_test_FourByteString: cls

	| pgType createBlock updateBlock |
	pgType := 'varchar(20)'.
	createBlock :=
			[:cls |
			(cls new: 6)
				codePointAt: 1 put: 16r2070e;
				codePointAt: 2 put: 16r20731;
				codePointAt: 3 put: 16r20779;
				codePointAt: 4 put: 16r20c53;
				codePointAt: 5 put: 16r20c78;
				codePointAt: 6 put: 16r20c96;
				yourself].
	updateBlock := [:obj | obj reverse].
	self
		_test_pgType: pgType
		withGsClass: cls
		gsCreateBlock: createBlock
		gsUpdateBlock: updateBlock.
	^self
%
category: 'Tests (private)'
method: PostgresTestCase
_test_insertUpdateDeleteTuplesFromPostgresForTupleClassName: aSymbol

	| rs ws assoc aTupleClass objs objsFromPg |
	self
		deny: (assoc := System myUserProfile resolveSymbol: aSymbol) identical: nil;
		assert: (aTupleClass := assoc value) isBehavior;
		createWidgetTable.
	objs := aTupleClass newInstances: 100.
	self
		assert: objs size identical: 100;
		assert: objs first class == aTupleClass.

"Run the rollback tests"
	self
		assert: (ws := self connection openInsertCursorOn: aTupleClass) class
			equals: GsPostgresWriteStream ;
		_testWriteStreamRollbackForStream: ws withCollection: objs ;
		assert: ws free identical: ws ;
		deny: (self hasWriteStream: ws) .

	self
		assert: (ws := self connection openInsertCursorOn: aTupleClass) class
			equals: GsPostgresWriteStream;
		assert: ws isExternal;
		assert: (self hasWriteStream: ws);
		deny: ws hasUnflushedData ;
		deny: self inTransaction ;
		begin ;
		assert: self inTransaction ;
		assert: (ws nextPutAll: objs) identical: ws;
		assert: ws hasUnflushedData ;
		assert: self inTransaction ;
		commit ;
		deny: self inTransaction ;
		deny: ws hasUnflushedData ;
		assert: ws free identical: ws ;
		deny: (self hasWriteStream: ws) .


	[| a b |
	self
		assert: (rs := self connection
							execute: (self class sqlForSelectAllFromTable: self class widgetTableName
									orderBy: 'id')
							tupleClass: aTupleClass) class
			identical: GsPostgresReadStream;
		assert: rs size identical: objs size;
		assert: (self hasReadStream: rs) .
	1 to: objsFromPg size
		do: [:n | self assert: (a := ws next) equals: (b := objs at: n)]]
			ensure: [rs ifNotNil: [rs free]].
	self deny: (self hasReadStream: rs) .
	objs do: [:e | e isActive: false].
	self
		assert: (ws := self connection openUpdateCursorOn: aTupleClass) class
			equals: GsPostgresWriteStream;
		deny: self inTransaction ;
		assert: (self hasWriteStream: ws);
		deny: ws hasUnflushedData ;
		assert: (ws nextPutAll: objs) identical: ws;
		assert: ws hasUnflushedData ;
		deny: self inTransaction ;
		assert: ws flush identical: ws ;
		deny: ws hasUnflushedData ;
		deny: self inTransaction ;
		assert: ws free identical: ws ;
		deny: (self hasWriteStream: ws) .

	[self
		assert: (rs := self connection
							execute: (self class sqlForSelectAllFromTable: self class widgetTableName
									orderBy: 'id')
							tupleClass: aTupleClass) class
			identical: GsPostgresReadStream;
		assert: (objsFromPg := rs contents) class identical: OrderedCollection;
		assert: objsFromPg size identical: objs size;
		assert: (self hasReadStream: rs) .
	1 to: objsFromPg size
		do: [:n | |a b |
			self assert: (a:= objsFromPg at: n) equals: (b :=objs at: n)]]
			ensure: [rs ifNotNil: [rs free]].
	self deny: (self hasReadStream: rs) .
	self
		assert: (ws := self connection openDeleteCursorOn: aTupleClass) class
			equals: GsPostgresWriteStream;
		assert: (self hasWriteStream: ws);
		deny: ws hasUnflushedData ;
		deny: self inTransaction ;
		begin ;
		assert: self inTransaction ;
		assert: (ws nextPutAll: objs) identical: ws;
		assert: ws hasUnflushedData ;
		assert: self inTransaction ;
		commit ;
		deny: self inTransaction ;
		deny: ws hasUnflushedData ;
		assert: ws free identical: ws ;
		deny: (self hasWriteStream: ws) .

	[self
		assert: (rs := self connection
							execute: (self class sqlForSelectAllFromTable: self class widgetTableName
									orderBy: 'id')
							tupleClass: aTupleClass) class
			identical: GsPostgresReadStream;
		assert: (self hasReadStream: rs) ;
		assert: rs size identical: 0]
			ensure: [rs ifNotNil: [rs free]].
	self deny: (self hasReadStream: rs) .
	^self
%
category: 'Tests (private)'
method: PostgresTestCase
_test_pgType: pgType withGsClass: gsClass gsCreateBlock: createBlock gsUpdateBlock: updateBlock

^ self _test_pgType: pgType withGsClass: gsClass gsCreateBlock: createBlock gsUpdateBlock: updateBlock overrideClass: false
%
category: 'Tests (private)'
method: PostgresTestCase
_test_pgType: pgType withGsClass: gsClass gsCreateBlock: createBlock gsUpdateBlock: updateBlock overrideClass: override

	| tableName obj str rs oc newObj newStr |
	tableName := self tableNameForClass: gsClass.
	self
		dropTableNamed: tableName;
		createTableNamed: tableName withColumnType: pgType.
	obj := createBlock value: gsClass.
	self
		assert: (str := GsPostgresWriteStream postgresStringForObject: obj escaped: true) class
		identical: String.
	self insertValue: str intoTable: tableName.
	rs := self selectAllFromTable: tableName.
	self
		assert: rs class identical: GsPostgresReadStream ;
		assert: (self hasReadStream: rs).
	override ifTrue:[ rs overrideType: gsClass forColumnNumber: 1 ] . "Some types like DateTime need this"
	self	assert: (oc := rs next) class identical: OrderedCollection;
		assert: oc size identical: 1;
		assert: (newObj := oc first) class identical: gsClass;
		assert: obj equals: newObj.
	rs free.
	self deny: (self hasReadStream: rs).
	obj := updateBlock value: obj.
	self
		assert: (newStr := GsPostgresWriteStream postgresStringForObject: obj escaped: true)
				class
		identical: String.
	self updateTable: tableName oldValue: str newValue: newStr.
	rs := self selectAllFromTable: tableName.
	self
		assert: rs class identical: GsPostgresReadStream ;
		assert: (self hasReadStream: rs).
	override ifTrue:[ rs overrideType: gsClass forColumnNumber: 1 ] . "Some types like DateTime need this"
	self	assert: (oc := rs next) class identical: OrderedCollection;
		assert: oc size identical: 1;
		assert: (newObj := oc first) class identical: gsClass;
		assert: newObj equals: obj.
	rs free.
	self deny: (self hasReadStream: rs).
	self deleteFromTable: tableName value: newStr.
	rs := self selectAllFromTable: tableName.
	self
		assert: (self hasReadStream: rs) ;
		assert: rs next identical: nil.
	rs free.
	^ self deny: (self hasReadStream: rs) ;
		dropTableNamed: tableName;
		yourself
%
category: 'Tests (private)'
method: PostgresTestCase
_test_TwoByteString2: cls

	| pgType createBlock updateBlock |

	pgType := 'varchar(20)'.
	createBlock :=
			[:cls |
			(cls new: 5)
				codePointAt: 1 put: 16r4f60;
				codePointAt: 2 put: 16r597d;
				codePointAt: 3 put: 16rff0c;
				codePointAt: 4 put: 16r4e16;
				codePointAt: 5 put: 16r754c;
				yourself].
	updateBlock := [:obj | obj reverse].
	self
		_test_pgType: pgType
		withGsClass: cls
		gsCreateBlock: createBlock
		gsUpdateBlock: updateBlock.
	^self
%
category: 'Tests (private)'
method: PostgresTestCase
_test_TwoByteString: cls

"cls should be DoubleByteString or Unicode16"

	| pgType createBlock updateBlock |
	pgType := 'varchar(20)'.
	createBlock :=  [:cls | (cls new: 4) codePointAt: 1 put: 16r441 ;  codePointAt: 2 put: 16r43b ; codePointAt: 3 put: 16r43e ; codePointAt: 4 put: 16r43d ; yourself ].
	updateBlock := [:obj | obj reverse ].
	self
		_test_pgType: pgType
		withGsClass: cls
		gsCreateBlock: createBlock
		gsUpdateBlock: updateBlock.
	^self
%
! ------------------- Remove existing behavior from WidgetWithStringsOldColumnMap
removeAllMethods WidgetWithStringsOldColumnMap
removeAllClassMethods WidgetWithStringsOldColumnMap
! ------------------- Class methods for WidgetWithStringsOldColumnMap
category: 'Posgres Support'
classmethod: WidgetWithStringsOldColumnMap
rdbColumnMapping

^ Array new
	add: (Array with: 'id' with: 'id') ; "2 element case"
	add: (Array with: 'last_update' with: 'lastUpdate') ; "2 element case"
	add: (Array with: 'last_update_notz' with: 'lastUpdateNoTz' with: 'lastUpdateNoTz' with: 'lastUpdateNoTz:') ;
	add: (Array with: 'is_active' with: 'isActive' with: 'isActive' with: 'isActive:') ;
	add: (Array with: 'date' with: 'activeDate' with: 'activeDate' with: 'activeDate:'  ) ;
	add: (Array with: 'time' with: 'activeTime' with: 'activeTime' with: 'activeTime:') ;
	add: (Array with: 'name_sb' with: 'nameSb' with: 'nameSb' ) ; "3 element case"
	add: (Array with: 'name_db' with: 'nameDb' with: 'nameDb' with: 'nameDb:') ;
	add: (Array with: 'name_qb' with: 'nameQb' with: 'nameQb' with: 'nameQb:');
	add: (Array with: 'balance' with: 'balance' with: 'balance' with: 'balance:' ) ;
	add: (Array with: 'encrypted_data' with: 'encryptedData' with: 'encryptedData' with: 'encryptedData:');
	yourself
%
category: 'Posgres Support'
classmethod: WidgetWithStringsOldColumnMap
rdbPrimaryKeyMaps


^ Array new
	add: (Array with: 'id' with: 'id') ; "2 element case"
	yourself
%
! ------------------- Instance methods for WidgetWithStringsOldColumnMap
! ------------------- Remove existing behavior from WidgetWithUnicodeOldColumnMap
removeAllMethods WidgetWithUnicodeOldColumnMap
removeAllClassMethods WidgetWithUnicodeOldColumnMap
! ------------------- Class methods for WidgetWithUnicodeOldColumnMap
category: 'Postgres Support'
classmethod: WidgetWithUnicodeOldColumnMap
rdbColumnMapping

^ Array new
	add: (Array with: 'id' with: 'id') ; "2 element case"
	add: (Array with: 'last_update' with: 'lastUpdate') ; "2 element case"
	add: (Array with: 'last_update_notz' with: 'lastUpdateNoTz' with: 'lastUpdateNoTz' with: 'lastUpdateNoTz:') ;
	add: (Array with: 'is_active' with: 'isActive' with: 'isActive' with: 'isActive:') ;
	add: (Array with: 'date' with: 'activeDate' with: 'activeDate' with: 'activeDate:'  ) ;
	add: (Array with: 'time' with: 'activeTime' with: 'activeTime' with: 'activeTime:' ) ;
	add: (Array with: 'name_sb' with: 'nameSb' with: 'nameSb' ) ; "3 element case"
	add: (Array with: 'name_db' with: 'nameDb' with: 'nameDb' with: 'nameDb:' ) ;
	add: (Array with: 'name_qb' with: 'nameQb' with: 'nameQb' with: 'nameQb:' );
	add: (Array with: 'balance' with: 'balance' with: 'balance' with: 'balance:' ) ;
	add: (Array with: 'encrypted_data' with: 'encryptedData' with: 'encryptedData' with: 'encryptedData:');
	yourself
%
category: 'Postgres Support'
classmethod: WidgetWithUnicodeOldColumnMap
rdbPrimaryKeyMaps


^ Array new
	add: (Array with: 'id' with: 'id' ) ;
	yourself
%
! ------------------- Instance methods for WidgetWithUnicodeOldColumnMap
