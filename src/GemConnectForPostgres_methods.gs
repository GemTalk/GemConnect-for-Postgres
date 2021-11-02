fileformat utf8
! ------------------- Remove existing behavior from GsPostgresWriteStream
removeAllMethods GsPostgresWriteStream
removeAllClassMethods GsPostgresWriteStream
! ------------------- Class methods for GsPostgresWriteStream
category: 'Converting'
classmethod: GsPostgresWriteStream
booleanToPostgresString: bool escaped: isEscaped

^ bool asString
%
category: 'Converting'
classmethod: GsPostgresWriteStream
byteArrayToPostgresString: aByteArray escaped: isEscaped
	"Convert a multi-byte string to code point literals understood by Postgres."

	| ws |
	ws := AppendStream on: String new.
	ws
		nextPutAll: '\x';
		nextPutAll: aByteArray asHexString .
	^ isEscaped ifTrue:[ ws contents quoted ] ifFalse:[ ws contents ]
%
category: 'Converting'
classmethod: GsPostgresWriteStream
dateAndTimeToPostgresString: aDateAndTime escaped: isEscaped

^ aDateAndTime printString quoted
%
category: 'Converting'
classmethod: GsPostgresWriteStream
dateTimeToPostgresString: aDateTime escaped: isEscaped
	"GsPostgresWriteStream dateTimeToPostgresString: DateTime now  escaped: false"
	"^ (aDateTime asStringUsingFormat: #(3 2 1 $- 1 1 $: true true false true true)) quoted"

	| localDt aString aTimeZone |
	aTimeZone := aDateTime timeZone.
	localDt := aDateTime addSeconds: (aDateTime _localOffset: aTimeZone).
	aString := localDt
				asStringStdUsingFormat: #(3 2 1 $- 1 1 $: true true false true true).
	aString
		add: $.;
		addAll: (self zeroPadLeft: (aDateTime millisecondsGmt \\ 1000) asString toDigits: 3)   ;
		add: Character space.
	(aDateTime isDstIn: aTimeZone)
		ifTrue: [aString addAll: aTimeZone dstPrintString]
		ifFalse: [aString addAll: aTimeZone standardPrintString].
	^aString quoted
%
category: 'Converting'
classmethod: GsPostgresWriteStream
dateToPostgresString: aDate  escaped: isEscaped

^ (aDate asStringUsingFormat: #( 3 2 1 $- 1 1)) quoted
%
category: 'Converting'
classmethod: GsPostgresWriteStream
doubleByteStringToPostgresString: aMbString  escaped: isEscaped
	"Convert a multi-byte string to code point literals understood by Postgres."

"Postgres code point format for 6 hex digits is:
U&'\+xxxxxx'
"
^ isEscaped ifTrue:[
	| ws |
	ws := AppendStream on: String new.
	ws nextPutAll: 'CONCAT('.
	1 to: aMbString size
		do:
			[:n |
			n > 1 ifTrue: [ws nextPutAll: ', '].
			ws
				nextPutAll: 'E' ;
				nextPut: $';
				nextPutAll: '\u';
				nextPutAll: ((aMbString codePointAt: n) asHexStringWithLength: 4);
				nextPut: $'].
	ws
		nextPut: $);
		contents
] ifFalse:[ aMbString ]
%
category: 'Converting'
classmethod: GsPostgresWriteStream
floatToPostgresString: anObj escaped: isEscaped

^ isEscaped ifTrue:[ anObj asString quoted ] ifFalse:[ anObj asString ]
%
category: 'Class Initialization'
classmethod: GsPostgresWriteStream
initialize


self initializeClassToPostgresStringTable
%
category: 'Class Initialization'
classmethod: GsPostgresWriteStream
initializeClassToPostgresStringTable
	"GsPostgresWriteStream initializeClassToPostgresStringTable"

	| qbMethSel blk dbMethSel |
	qbMethSel := Array with: self with: #quadByteStringToPostgresString:escaped: .
	dbMethSel := Array with: self with: #doubleByteStringToPostgresString:escaped: .
	ClassToPostgresStringTable := IdentityKeyValueDictionary new.
	ClassToPostgresStringTable
		at: DateAndTime put: (Array with: self with: #dateAndTimeToPostgresString:escaped: );
		at: DateTime put: (Array with: self with: #dateTimeToPostgresString:escaped:);
		at: DoubleByteString put: dbMethSel;
		at: QuadByteString put: qbMethSel;
		at: Unicode16 put: dbMethSel;
		at: Unicode32 put: qbMethSel;
		at: Date put: (Array with: self with: #dateToPostgresString:escaped: );
		at: Time put: (Array with: self with: #timeToPostgresString:escaped: );
		at: ByteArray put: (Array with: self with: #byteArrayToPostgresString:escaped: ) ;
		at: SmallInteger put: (Array with: self with: #smallIntegerToPostgresString:escaped: ) ;
		at: Boolean put: (Array with: self with: #booleanToPostgresString:escaped: ) ;
		at: ScaledDecimal put: (Array with: self with: #scaledDecimalToPostgresString:escaped: ) ;
		at: Float put: (Array with: self with: #floatToPostgresString:escaped: ) .
	"Add new special classes if this GS version has them"
	blk :=
			[:sym :existingCls |
			| cls |
			(cls := Globals at: sym otherwise: nil)
				ifNotNil:
					[ClassToPostgresStringTable at: cls
						put: (ClassToPostgresStringTable at: existingCls)]].
	blk
		value: #SmallDateAndTime value: DateAndTime;
		value: #SmallDate value: Date;
		value: #SmallTime value: Time ;
		value: #SmallScaledDecimal value: ScaledDecimal ;
		value: #SmallDouble value: Float .

	^self
%
category: 'Converting'
classmethod: GsPostgresWriteStream
multiByteStringToPostgresString: aMbString
	"Convert a multi-byte string to code point literals understood by Postgres."

"Postgres code point format for 6 hex digits is:
U&'\+xxxxxx'
"

	| ws |
	ws := AppendStream on: String new.
	ws nextPutAll: 'CONCAT('.
	1 to: aMbString size
		do:
			[:n |
			n > 1 ifTrue: [ws nextPutAll: ', '].
			ws
				nextPutAll: 'E' ;
				nextPut: $';
				nextPutAll: '\U';
				nextPutAll: ((aMbString codePointAt: n) asHexStringWithLength: 6);
				nextPut: $'].
	^ws
		nextPut: $);
		contents
%
category: 'Instance Creation'
classmethod: GsPostgresWriteStream
new

	^ super new initialize
%
category: 'Instance Creation'
classmethod: GsPostgresWriteStream
newForCommand: sql connection: conn tupleClass: aClass columnMapping: colMap keyMap: keyMap bindInfo: aBindInfo

^ self new
	initializeWithConnection: conn ;
	sql: sql ;
	tupleClass: aClass ;
	columnMap: colMap ;
	keyMap: keyMap ;
	yourself
%
category: 'Converting'
classmethod: GsPostgresWriteStream
postgresStringForObject: anObject escaped: isEscaped

| arrayOrNil x |
^ (arrayOrNil := ClassToPostgresStringTable at: anObject class otherwise: nil)
	ifNil:[ isEscaped ifTrue:[ anObject asString quoted ] ifFalse:[ anObject asString ]]
	ifNotNil:[ x := arrayOrNil first perform: arrayOrNil last with: anObject with: isEscaped ] "First element is receiver, second element is selector, arg is anObject"
%
category: 'Converting'
classmethod: GsPostgresWriteStream
quadByteStringToPostgresString: aMbString escaped: isEscaped
	"Convert a multi-byte string to code point literals understood by Postgres."
	"Postgres code point format for 6 hex digits is:
U&'\+xxxxxx'
"

	^isEscaped
		ifTrue:
			[| ws |
			ws := AppendStream on: String new.
			ws nextPutAll: 'CONCAT('.
			1 to: aMbString size
				do:
					[:n |
					n > 1 ifTrue: [ws nextPutAll: ', '].
					ws
						nextPutAll: 'E';
						nextPut: $';
						nextPutAll: '\U';
						nextPutAll: ((aMbString codePointAt: n) asHexStringWithLength: 8);
						nextPut: $'].
			ws
				nextPut: $);
				contents]
		ifFalse: [aMbString]
%
category: 'Converting'
classmethod: GsPostgresWriteStream
scaledDecimalToPostgresString: anObj  escaped: isEscaped

^   isEscaped ifTrue:[ anObj asString quoted ] ifFalse:[ anObj asString ]
%
category: 'Converting'
classmethod: GsPostgresWriteStream
smallIntegerToPostgresString: anInt  escaped: isEscaped

^ isEscaped ifTrue:[ anInt asString quoted ] ifFalse:[ anInt asString ]
%
category: 'Converting'
classmethod: GsPostgresWriteStream
timeToPostgresString: aTime escaped: isEscaped
	"GsPostgresWriteStream timeToPostgresString: Time now  escaped: true"

	| result micros |
	micros := self zeroPadLeft: (aTime asMicroseconds \\ 1000000) asString toDigits: 6 .
	result := aTime asString , $. , micros .
	^isEscaped ifTrue: [result quoted] ifFalse: [result]
%
category: 'Converting'
classmethod: GsPostgresWriteStream
zeroPadLeft: aString toDigits: anInt

	| result sz |
	(sz := aString size) >= anInt ifTrue: [^aString]. "Handle easy case first - no padding needed"
	result := String new.
	anInt - sz timesRepeat: [result add: $0].
	result addAll: aString.
	^result
%
! ------------------- Instance methods for GsPostgresWriteStream
category: 'Stream Operations'
method: GsPostgresWriteStream
clear

"Clears the contents of the receiver and makes it empty."

	collection size: 0 .
	position := 0.
%
category: 'Accessing'
method: GsPostgresWriteStream
collection
	^collection
%
category: 'Updating'
method: GsPostgresWriteStream
collection: newValue
	collection := newValue
%
category: 'Accessing'
method: GsPostgresWriteStream
columnMap
	^columnMap
%
category: 'Updating'
method: GsPostgresWriteStream
columnMap: newValue
	columnMap := newValue
%
category: 'Accessing'
method: GsPostgresWriteStream
conn
	^conn
%
category: 'Updating'
method: GsPostgresWriteStream
conn: newValue
	conn := newValue
%
category: 'Flushing'
method: GsPostgresWriteStream
executePreparedWith: aTupleObject

	| params cMap cls |
	cls := self class.
	self validateTupleObject: aTupleObject.
	params := Array new.
	cMap := self columnMap.
	1 to: cMap size
		do:
			[:n |
			| getter obj |
			getter := (cMap at: n) getMethodSelector.	"getter selector, a Symbol"
			obj := aTupleObject perform: getter.	"Fetch the object from the tuple"
			params add: (cls postgresStringForObject: obj escaped: false )	"Convert to postgres string"].
	keyMap notNil
		ifTrue:
			[1 to: keyMap size
				do:
					[:n |
					| getter obj |
					getter := (keyMap at: n) getMethodSelector.	"getter selector, a Symbol"
					obj := aTupleObject perform: getter.	"Fetch the object from the tuple"
					params add: (cls postgresStringForObject: obj escaped: false)	"Convert to postgres string"]].
	^self conn executePreparedStatementWithName: self preparedStatementName
		parameters: params
%
category: 'Flushing'
method: GsPostgresWriteStream
flush

"Each execute runs in its own transaction"
position + 1 to: collection size do:[:n|
	self executePreparedWith: (collection at: n).
].
position := collection size.
^ self
%
category: 'Flushing'
method: GsPostgresWriteStream
flushInOneTransaction

"Flushes all objects in a single transaction"

self conn begin.
self flush .
self conn commit.
^ self
%
category: 'Freeing'
method: GsPostgresWriteStream
free

GsPostgresConnection removeWriteStream: self.
^ self
%
category: 'Testing'
method: GsPostgresWriteStream
hasUnflushedData

^ position < collection size
%
category: 'Initialize'
method: GsPostgresWriteStream
initialize
	collection := Array new.
	position := 0.
	preparedStatementName := GsUuidV4 new asString .
%
category: 'Initialize'
method: GsPostgresWriteStream
initializeWithConnection: aConn
	"initialize method must already be called before we get here!"

	GsPostgresConnection addWriteStream: self.
	self
		conn: aConn;
		libpq: aConn libpq
%
category: 'Testing'
method: GsPostgresWriteStream
isExternal
	^ true
%
category: 'Accessing'
method: GsPostgresWriteStream
keyMap
	^keyMap
%
category: 'Updating'
method: GsPostgresWriteStream
keyMap: newValue
	keyMap := newValue
%
category: 'Accessing'
method: GsPostgresWriteStream
libpq
	^libpq
%
category: 'Updating'
method: GsPostgresWriteStream
libpq: newValue
	libpq := newValue
%
category: 'Stream Operations'
method: GsPostgresWriteStream
nextPut: tupleObj

collection add: tupleObj .
^ self
%
category: 'Stream Operations'
method: GsPostgresWriteStream
nextPutAll: aCollection

collection addAll: aCollection .
^ self
%
category: 'Accessing'
method: GsPostgresWriteStream
position
	^position
%
category: 'Stream Operations'
method: GsPostgresWriteStream
position: newValue
	position := newValue
%
category: 'Accessing'
method: GsPostgresWriteStream
preparedStatementName
	^preparedStatementName
%
category: 'Updating'
method: GsPostgresWriteStream
preparedStatementName: newValue
	preparedStatementName := newValue
%
category: 'Command Execution'
method: GsPostgresWriteStream
prepareStatement

^self conn prepareStatement: self sql withName: self preparedStatementName numberParameters: self columnMap size
%
category: 'Accessing'
method: GsPostgresWriteStream
sql
	^sql
%
category: 'Updating'
method: GsPostgresWriteStream
sql: newValue
	sql := newValue
%
category: 'Accessing'
method: GsPostgresWriteStream
tupleClass
	^tupleClass
%
category: 'Updating'
method: GsPostgresWriteStream
tupleClass: newValue
	tupleClass := newValue
%
category: 'Flushing'
method: GsPostgresWriteStream
validateTupleObject: anObj

(anObj isKindOf: self tupleClass)
	ifFalse:[ ArgumentError new initialize ; object: anObj ; messageText: ('Expected a kind of ', self tupleClass asString) ; signal ].
%
! ------------------- Remove existing behavior from GsLibpq
removeAllMethods GsLibpq
removeAllClassMethods GsLibpq
! ------------------- Class methods for GsLibpq
category: 'Functions'
classmethod: GsLibpq
functionList

"Canonical list of functions names in libpq.so that we can call.
Each name must have an initializer instance method named #iniatialize_FN where FN is the symbol in the array.
PLEASE keep in alphabetical order!"

^ {
	#'PQclear' .
	#'PQcmdStatus' .
	#'PQcmdTuples' .
	#'PQconnectdb' .
	#'PQdescribePrepared' .
	#'PQerrorMessage' .
	#'PQexec' .
	#'PQexecParams' .
	#'PQexecPrepared' .
	#'PQfformat' .
	#'PQfinish' .
	#'PQfmod' .
	#'PQfname' .
	#'PQfnumber' .
	#'PQfsize' .
	#'PQftablecol' .
	#'PQftype' .
	#'PQgetlength' .
	#'PQgetvalue' .
	#'PQlibVersion' .
	#'PQnfields' .
	#'PQntuples' .
	#'PQprepare' .
	#'PQresStatus' .
	#'PQresultErrorField' .
	#'PQresultErrorMessage' .
	#'PQresultStatus' .
	#'PQresultVerboseErrorMessage' .
	#'PQstatus'  .
	#'PQclientEncoding' .
	#'pg_encoding_to_char' .
	#'PQsetClientEncoding' .
	#'PQtrace' .
	#'PQuntrace'
}
%
category: 'Accessing'
classmethod: GsLibpq
libraryPath

"GsLibpq libraryPath"
^ libraryPath
%
category: 'Class Initialization'
classmethod: GsLibpq
libraryPath: aString

libraryPath := aString
%
category: 'Instance Creation'
classmethod: GsLibpq
new

^ super new initialize
%
category: 'Instance Creation'
classmethod: GsLibpq
newWithLibraryPath: aLibraryPath

self libraryPath: aLibraryPath .
^ self new
%
category: 'Accessing'
classmethod: GsLibpq
postgresBinDirectory

"Derive the bin directory from the library path"
"GsLibpq postgresBinDirectory"

^((ReadStreamPortable on: self libraryPath) upToAll: '/lib/') addAll: '/bin/' ; yourself
%
category: 'Read Me'
classmethod: GsLibpq
readme


^'
Steps for adding a new C callout to GsLibpq:

1) Add the function name symbol to the #functionList class method. Keep names in alphabetical order!

2) Create an function initializer instance method which initializes the callout and add it to category "Function Initializers".
You must know the return type and arguments types of the Postgres libpq C function.
See methods in instance category "Function Initializers" for examples.

3) Create an instance method that will call the libpq C function. The method name should match the C function name
if possible. This method will invoke the C callout and pass the argument objects to C. Put the method in instance
category "Postgres Callouts". See methods in category "Postgres Callouts" for examples.

'
%
! ------------------- Instance methods for GsLibpq
category: 'Private'
method: GsLibpq
basicLoginWithString: aConnectionString

"Makes the basic login call without checking the status. Caller must call #PQstatus: to confirm login succeeded."

^ self PQconnectDb: aConnectionString
%
category: 'Accessing'
method: GsLibpq
calloutTable
	^calloutTable
%
category: 'Updating'
method: GsLibpq
calloutTable: newValue
	calloutTable := newValue
%
category: 'Accessing'
method: GsLibpq
cLibrary
	^cLibrary
%
category: 'Updating'
method: GsLibpq
cLibrary: newValue
	cLibrary := newValue
%
category: 'Testing'
method: GsLibpq
connectionOk: aConn

^ 0 == (self statusForConnection: aConn)
%
category: 'Error Handling'
method: GsLibpq
errorMessageForConnection: conn

"Caller must not free the resultring string"

^ (self calloutTable at: #PQerrorMessage) callWith: { conn }
%
category: 'Initialize'
method: GsLibpq
initialize

calloutTable := SymbolKeyValueDictionary new: self class functionList size .
cLibrary := CLibrary named: self class libraryPath .
self initializeCallouts .
%
category: 'Initialize'
method: GsLibpq
initializeCallouts

self class functionList do:[:sym| self perform: (self initializeMethodNameForFunction: sym) ].

"Alternate call to PQgetvalue which returns a ptr instead of char *"
self calloutTable at: #PQgetvalue_binary put: (CCallout library: self cLibrary name: #PQgetvalue asString result: #'ptr' args: #(#'ptr' #'int32' #'int32') varArgsAfter: -1)

%
category: 'Private'
method: GsLibpq
initializeForFunctionNamed: aSymbol result: resultSym args: argArray

calloutTable at: aSymbol put: (CCallout library: self cLibrary name: aSymbol asString result: resultSym args: argArray varArgsAfter: -1)
%
category: 'Initialize'
method: GsLibpq
initializeMethodNameForFunction: sym

"Function initializer method is initialize_FN"

^ ('initialize_', sym asString) asSymbol
%
category: 'Function Initializers'
method: GsLibpq
initialize_pg_encoding_to_char
	self initializeForFunctionNamed: #pg_encoding_to_char result: #'char*' args: #(#'int32')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQclear
	self initializeForFunctionNamed: #PQclear result: #'void' args: #(#'ptr')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQclientEncoding
	self initializeForFunctionNamed: #PQclientEncoding result: #'int32' args: #(#'ptr')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQcmdStatus
	self initializeForFunctionNamed: #PQcmdStatus result: #'char*' args: #(#'ptr')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQcmdTuples
	self initializeForFunctionNamed: #PQcmdTuples result: #'char*' args: #(#'ptr')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQconnectdb

self initializeForFunctionNamed: #PQconnectdb result: #'ptr' args: #(#'const char*')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQdescribePrepared
	self initializeForFunctionNamed: #PQdescribePrepared result: #'ptr' args: #(#'ptr' #'const char*')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQerrorMessage

self initializeForFunctionNamed: #PQerrorMessage result: #'char*' args: #(#'ptr')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQexec
	self initializeForFunctionNamed: #PQexec result: #'ptr' args: #(#'ptr'  #'const char*')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQexecParams
	self initializeForFunctionNamed: #PQexecParams result: #'ptr' args: #(#'ptr' #'const char*' #'int32' #'ptr' #'ptr' #'ptr' #'ptr' #'int32' )
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQexecPrepared
	self initializeForFunctionNamed: #PQexecPrepared result: #'ptr' args: #(#'ptr' #'const char*'  #'int32' #'ptr' #'ptr' #'ptr' #'int32')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQfformat
	self initializeForFunctionNamed: #PQfformat result: #'int32' args: #(#'ptr' #'int32')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQfinish

self initializeForFunctionNamed: #PQfinish result: #'void' args: #(#'ptr')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQfmod
	self initializeForFunctionNamed: #PQfmod result: #'int32' args: #(#'ptr' #'int32')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQfname
	self initializeForFunctionNamed: #PQfname result: #'char*' args: #(#'ptr' #'int32')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQfnumber
	self initializeForFunctionNamed: #PQfnumber result: #'int32' args: #(#'ptr' #'const char*')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQfsize
	self initializeForFunctionNamed: #PQfsize result: #'int32' args: #(#'ptr' #'int32')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQftablecol
	self initializeForFunctionNamed: #PQftablecol result: #'int32' args: #(#'ptr' #'int32')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQftype
	self initializeForFunctionNamed: #PQftype result: #'uint32' args: #(#'ptr'  #'int32')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQgetlength
	self initializeForFunctionNamed: #PQgetlength result: #'int32' args: #(#'ptr' #'int32' #'int32')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQgetvalue
	self initializeForFunctionNamed: #PQgetvalue result: #'char*' args: #(#'ptr' #'int32' #'int32')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQlibVersion
	self initializeForFunctionNamed: #PQlibVersion result: #'int32' args: #()
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQnfields
	self initializeForFunctionNamed: #PQnfields result: #'int32' args: #(#'ptr')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQntuples
	self initializeForFunctionNamed: #PQntuples result: #'int32' args: #(#'ptr')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQprepare
	self initializeForFunctionNamed: #PQprepare result: #'ptr' args: #(#'ptr' #'const char*' #'const char*' #'int32' #'ptr' )
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQresStatus
	self initializeForFunctionNamed: #PQresStatus result: #'char*' args: #(#'int32')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQresultErrorField
	self initializeForFunctionNamed: #PQresultErrorField result: #'char*' args: #(#'ptr' #'int32')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQresultErrorMessage
	self initializeForFunctionNamed: #PQresultErrorMessage result: #'char*' args: #(#'ptr')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQresultStatus
	self initializeForFunctionNamed: #PQresultStatus result: #'int32' args: #(#'ptr')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQresultVerboseErrorMessage
	self initializeForFunctionNamed: #PQresultVerboseErrorMessage result: #'char*' args: #(#'ptr' #'int32' #'int32')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQsetClientEncoding
	self initializeForFunctionNamed: #PQsetClientEncoding result: #'int32' args: #(#'ptr' #'char*')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQstatus

self initializeForFunctionNamed: #PQstatus result: #'int32' args: #(#'ptr')
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQtrace

	self
		initializeForFunctionNamed: #PQtrace
		result: #void
		args: #(#ptr #ptr)
%
category: 'Function Initializers'
method: GsLibpq
initialize_PQuntrace

	self
		initializeForFunctionNamed: #PQuntrace
		result: #void
		args: #(#ptr)
%
category: 'Private'
method: GsLibpq
logoutConnection: conn

^ (self calloutTable at: #PQfinish) callWith: { conn }
%
category: 'Postgres Callouts'
method: GsLibpq
pg_encoding_to_char: anInt

^ (self calloutTable at: #pg_encoding_to_char ) callWith: { anInt }
%
category: 'Postgres Callouts'
method: GsLibpq
PQclear: aPqResultCptr

^ (self calloutTable at: #PQclear) callWith: { aPqResultCptr }
%
category: 'Postgres Callouts'
method: GsLibpq
PQclientEncoding: conn

^ (self calloutTable at: #PQclientEncoding) callWith: { conn }
%
category: 'Postgres Callouts'
method: GsLibpq
PQcmdStatus: aPqResultCptr

^ (self calloutTable at: #PQcmdStatus) callWith: { aPqResultCptr }
%
category: 'Postgres Callouts'
method: GsLibpq
PQcmdTuples: aPqResultCptr

^ (self calloutTable at: #PQcmdTuples) callWith: { aPqResultCptr }
%
category: 'Postgres Callouts'
method: GsLibpq
PQconnectDb: aConnectionString

^ (self calloutTable at: #PQconnectdb) callWith: { aConnectionString }
%
category: 'Postgres Callouts'
method: GsLibpq
PQdescribePrepared: startementName on: pgConn



^ (self calloutTable at: #PQdescribePrepared ) callWith: { pgConn . startementName }
%
category: 'Postgres Callouts'
method: GsLibpq
PQerrorMessage: conn

"Caller must not free the resulting string"

^ (self calloutTable at: #PQerrorMessage) callWith: { conn }
%
category: 'Postgres Callouts'
method: GsLibpq
PQexec: query onConnection: conn

^ (self calloutTable at: #PQexec) callWith: { conn . query }
%
category: 'Postgres Callouts'
method: GsLibpq
PQexecParams:  cmdString numParams: numParms paramTypes: arrayOfInts1 paramValues: arrayOfStrings paramLengths: arrayOfInts2 paramFormats: arrayOfInts3 resultFormat: formatInt on: pgConn

| paramTypes paramValues paramLengths paramFormats |
arrayOfInts1 ifNotNil:[ paramTypes := CByteArray fromArrayOfInt32: arrayOfInts1].
arrayOfStrings ifNotNil:[ paramValues := CByteArray fromArrayEncodeUtf8: arrayOfStrings extraNullPointer: false ].
arrayOfInts2 ifNotNil:[ paramLengths := CByteArray fromArrayOfInt32: arrayOfInts2].
arrayOfInts3 ifNotNil:[ paramFormats := CByteArray fromArrayOfInt32: arrayOfInts3].

^ (self calloutTable at: #PQexecParams ) callWith: { pgConn . cmdString . numParms . paramTypes . paramValues . paramLengths . paramFormats . formatInt }
%
category: 'Postgres Callouts'
method: GsLibpq
PQexecParams:  cmdString  paramTypes: arrayOfInts1 paramValues: arrayOfParams paramLengths: arrayOfInts2 paramFormats: arrayOfInts3 resultFormat: formatInt on: pgConn

| paramTypes paramValues paramLengths paramFormats |
arrayOfInts1 ifNotNil:[ paramTypes := CByteArray fromArrayOfInt32: arrayOfInts1].
arrayOfParams ifNotNil:[ paramValues := CByteArray fromArrayEncodeUtf8: arrayOfParams extraNullPointer: false  ].
arrayOfInts2 ifNotNil:[ paramLengths := CByteArray fromArrayOfInt32: arrayOfInts2].
arrayOfInts3 ifNotNil:[ paramFormats := CByteArray fromArrayOfInt32: arrayOfInts3].

^ (self calloutTable at: #PQexecParams ) callWith: { pgConn . cmdString . arrayOfParams size . paramTypes . paramValues . paramLengths . paramFormats . formatInt }
%
category: 'Postgres Callouts'
method: GsLibpq
PQexecPrepared:  startementName paramValues: arrayOfParams paramLengths: arrayOfInts2 paramFormats: arrayOfInts3 resultFormat: formatInt on: pgConn

|  paramValues paramLengths paramFormats |
arrayOfParams ifNotNil:[ paramValues := CByteArray fromArrayEncodeUtf8: arrayOfParams extraNullPointer: false ].
arrayOfInts2 ifNotNil:[ paramLengths := CByteArray fromArrayOfInt32: arrayOfInts2].
arrayOfInts3 ifNotNil:[ paramFormats := CByteArray fromArrayOfInt32: arrayOfInts3].

^ (self calloutTable at: #PQexecPrepared ) callWith: { pgConn . startementName .  arrayOfParams size .  paramValues . paramLengths . paramFormats . formatInt }
%
category: 'Postgres Callouts'
method: GsLibpq
PQfformat: aPqResultCptr fieldNum: anInt

^ (self calloutTable at: #PQfformat) callWith: { aPqResultCptr . anInt }
%
category: 'Postgres Callouts'
method: GsLibpq
PQfmod: aPqResultCptr fieldNum: anInt

^ (self calloutTable at: #PQfmod) callWith: { aPqResultCptr . anInt }
%
category: 'Postgres Callouts'
method: GsLibpq
PQfname: aPqResultCptr fieldNum: anInt

^ (self calloutTable at: #PQfname) callWith: { aPqResultCptr . anInt }
%
category: 'Postgres Callouts'
method: GsLibpq
PQfnumber: aPqResultCptr columnName: cName

^ (self calloutTable at: #PQfnumber) callWith: { aPqResultCptr . cName }
%
category: 'Postgres Callouts'
method: GsLibpq
PQfsize: aPqResultCptr fieldNum: anInt

^ (self calloutTable at: #PQfsize) callWith: { aPqResultCptr . anInt }
%
category: 'Postgres Callouts'
method: GsLibpq
PQftablecol: aPqResultCptr fieldNum: anInt

^ (self calloutTable at: #PQftablecol) callWith: { aPqResultCptr . anInt }
%
category: 'Postgres Callouts'
method: GsLibpq
PQftype: aPqResultCptr fieldNum: anInt

^ (self calloutTable at: #PQftype) callWith: { aPqResultCptr . anInt }
%
category: 'Postgres Callouts'
method: GsLibpq
PQgetlength: aPqResultCptr row: rowInt column: columnInt

^ (self calloutTable at: #PQgetlength) callWith: { aPqResultCptr . rowInt . columnInt }
%
category: 'Postgres Callouts'
method: GsLibpq
PQgetvalue: aPqResultCptr row: rowInt column: columnInt

^ (self calloutTable at: #PQgetvalue) callWith: { aPqResultCptr . rowInt . columnInt }
%
category: 'Postgres Callouts'
method: GsLibpq
PQgetvalue_binary: aPqResultCptr row: rowInt column: columnInt

"Like #PQgetvalue:row:column: , except the result is a CByteArray instread of a String"
^ (self calloutTable at: #PQgetvalue_binary) callWith: { aPqResultCptr . rowInt . columnInt }
%
category: 'Postgres Callouts'
method: GsLibpq
PQlibVersion

^ (self calloutTable at: #PQlibVersion) callWith: nil
%
category: 'Postgres Callouts'
method: GsLibpq
PQnfields: aPqResultCptr

^ (self calloutTable at: #PQnfields) callWith: { aPqResultCptr }
%
category: 'Postgres Callouts'
method: GsLibpq
PQntuples: aPqResultCptr

^ (self calloutTable at: #PQntuples) callWith: { aPqResultCptr }
%
category: 'Postgres Callouts'
method: GsLibpq
PQprepare:  cmdString statementName: aString numParams: numParms paramTypes: arrayOfInts1 on: pgConn

| paramTypes  |
arrayOfInts1 ifNotNil:[ paramTypes := CByteArray fromArrayOfInt32: arrayOfInts1].

^ (self calloutTable at: #PQprepare ) callWith: { pgConn . aString . cmdString . numParms . paramTypes }
%
category: 'Postgres Callouts'
method: GsLibpq
PQresStatus: anInt

^ (self calloutTable at: #PQresStatus) callWith: { anInt }
%
category: 'Postgres Callouts'
method: GsLibpq
PQresultErrorField: aPqResultCptr fieldCode: anInt

^ (self calloutTable at: #PQresultErrorField:) callWith: { aPqResultCptr . anInt }
%
category: 'Postgres Callouts'
method: GsLibpq
PQresultErrorMessage: aPqResultCptr

^ (self calloutTable at: #PQresultErrorMessage) callWith: { aPqResultCptr }
%
category: 'Postgres Callouts'
method: GsLibpq
PQresultStatus: aPqResultCptr

^ (self calloutTable at: #PQresultStatus) callWith: { aPqResultCptr }
%
category: 'Postgres Callouts'
method: GsLibpq
PQresultVerboseErrorMessage: aPqResultCptr verbosity: anInt contextVisibility: contextVis

^ (self calloutTable at: #PQresultErrorMessage) callWith: { aPqResultCptr . anInt . contextVis }
%
category: 'Postgres Callouts'
method: GsLibpq
PQsetClientEncoding: aString on: conn

^ (self calloutTable at: #PQsetClientEncoding) callWith: { conn . aString }
%
category: 'Error Handling'
method: GsLibpq
raiseErrorFor: conn

"Raise an exception for conn, which must be a GsPostgresConnection."

^GsPostgresConnection errorClass signal: (self statusForConnection: conn)
%
category: 'Error Handling'
method: GsLibpq
statusForConnection: aConn

"Answer a SmallInteger indicating the status of the current connection.
For blocking calls, should return CONNECTION_OK (0) or CONNECTION_BAD(1)"

^ (self calloutTable at: #PQstatus) callWith: { aConn }
%
! ------------------- Remove existing behavior from GsPostgresConnection
removeAllMethods GsPostgresConnection
removeAllClassMethods GsPostgresConnection
! ------------------- Class methods for GsPostgresConnection
category: 'Private'
classmethod: GsPostgresConnection
addConnection: aGsPostgresConnection

"Private. Do not call directly unless you know what you're doing."

self _allPostgresConnections add: aGsPostgresConnection
%
category: 'Private'
classmethod: GsPostgresConnection
addWriteStream: aGsPostgresWriteStream

"Private. Do not call directly unless you know what you're doing."

self _allPostgresWriteStreams add: aGsPostgresWriteStream
%
category: 'Connection Management'
classmethod: GsPostgresConnection
allPostgresConnections

"Answer an Array of *connected* GsPostgresConnection objects."

^ self _allPostgresConnections asArray
%
category: 'Private'
classmethod: GsPostgresConnection
clearAllStreams

"Private. Do not call directly unless you know what you're doing."

self _allPostgresWriteStreams do:[:e| e clear ]
%
category: 'Connection Management'
classmethod: GsPostgresConnection
connectionWithName: aName

"Answer aGsPostgresConnection cached under the name aName, or nil if not found."

^ self _namedConnectionDictionary at: aName otherwise:  nil
%
category: 'Error Handling'
classmethod: GsPostgresConnection
errorClass

"Answer the class used to raise Postgres errors. Use PostgresError if available, otherwise use ExternalError."

^Globals at: #PostgresError otherwise: (Globals at: #ExternalError)
%
category: 'Error Handling'
classmethod: GsPostgresConnection
errorNumber

"Answer the GemStone error number used to raise Postgres errors."

	^self hasPostgresError ifTrue: [2761 "PostgresError" ] ifFalse: [2713 "ExternalError" ]
%
category: 'Private'
classmethod: GsPostgresConnection
flushAllStreams

"Private. Do not call directly unless you know what you're doing."

self _allPostgresWriteStreams do:[:e| e flush ]
%
category: 'SQL Generation'
classmethod: GsPostgresConnection
generateBindSQLDeleteForTable: tableName keys: keyNames

"Returns a string representing a properly formatted SQL delete statement
 with bind variables.  The given table name string and key name array are
 used to generate this delete statement."

   | sqlSt whereCl |

   sqlSt := String new.
   whereCl := self generateBindSQLWhereClause: keyNames.

   (whereCl isEmpty) ifFalse: [
      sqlSt add: 'DELETE FROM '; add: tableName.
      sqlSt add: ' '; add: whereCl.
      ].

   ^sqlSt
%
category: 'SQL Generation'
classmethod: GsPostgresConnection
generateBindSQLInsertForTable: tableName columns: colNames

"Returns a string representing a properly formatted SQL insert statement
 with bind variables.  The given table name string and column array are
 used to generate this insert statement."

   | sqlSt valuesCl |

   sqlSt := String new.
   valuesCl := self generateBindSQLValuesClause: colNames.

   (valuesCl isEmpty) ifFalse: [
      sqlSt add: 'INSERT INTO '; add: tableName.
      sqlSt add: ' '; add: valuesCl.
      ].

   ^sqlSt
%
category: 'SQL Generation'
classmethod: GsPostgresConnection
generateBindSQLSetClause: colNames

"Returns a string representing a properly formatted SQL set clause with
 bind variables.  The given  column name array is used to generate this
 set clause."

   | setCl |

   colNames _validateClass: Array.
   setCl := String new.
   1 to: (colNames size) do: [:i |
      (setCl isEmpty) ifTrue: [
         setCl := String withAll: 'SET '.
         ]
      ifFalse: [
         setCl add: ', '.
         ].

      setCl add: (colNames at: i); add: ' = $'.
      setCl add: i asString .
      ].

   ^setCl
%
category: 'SQL Generation'
classmethod: GsPostgresConnection
generateBindSQLUpdateForTable: tableName columns: colNames keys: keyNames

"Returns a string representing a properly formatted SQL update statement
 with bind variables.  The given table name string, column name array,
 and key name array are used to generate this update statement."

   | sqlSt setCl whereCl |

   sqlSt := String new.
   setCl := self generateBindSQLSetClause: colNames.
   whereCl := self generateBindSQLWhereClause: keyNames bindVarOffset: colNames size.

   (setCl isEmpty or: [whereCl isEmpty]) ifFalse: [
      sqlSt add: 'UPDATE '; add: tableName.
      sqlSt add: ' '; add: setCl; add: ' '; add: whereCl.
      ].

   ^sqlSt
%
category: 'SQL Generation'
classmethod: GsPostgresConnection
generateBindSQLValuesClause: colNames

"Returns a string representing a properly formatted SQL values clause
 with bind variables. The given column name array is used to generate
 this values clause."

   | valuesCl vals bindName |

   colNames _validateClass: Array.
   valuesCl := String new.
   1 to: (colNames size) do: [:i |
      (valuesCl isEmpty) ifTrue: [
         valuesCl add: '('.
         vals := String withAll: ') VALUES ('.
         ]
      ifFalse: [
         valuesCl add: ', '.
         vals add: ', '.
         ].

      valuesCl add: (colNames at: i).
	vals add: $$ .
      vals add: i asString.
      ].

   (vals notNil) ifTrue: [
      valuesCl add: vals; add: ')'.
      ].
   ^valuesCl
%
category: 'SQL Generation'
classmethod: GsPostgresConnection
generateBindSQLWhereClause: keyNames

"Returns a string representing a properly formatted SQL where clause
 with bind variables.  The given  key name array is used to generate
 this where clause."

^ self generateBindSQLWhereClause: keyNames bindVarOffset: 0
%
category: 'SQL Generation'
classmethod: GsPostgresConnection
generateBindSQLWhereClause: keyNames bindVarOffset: offset

"Returns a string representing a properly formatted SQL where clause
 with bind variables.  The given  key name array is used to generate
 this where clause."

   | whereCl |

   keyNames _validateClass: Array.
   whereCl := String new.
   1 to: (keyNames size) do: [:i |
      (whereCl isEmpty) ifTrue: [
         whereCl add: 'WHERE '.
         ]
      ifFalse: [
         whereCl add: ' AND '.
         ].
      whereCl add: (keyNames at: i).
	whereCl add: ' = $' .
	whereCl add: (i + offset) asString .
      ].

   ^whereCl
%
category: 'SQL Generation'
classmethod: GsPostgresConnection
generateSQLSetClause: colsNvals

"Returns a string representing a properly formatted SQL set clause.  The given
 column/value array is used to generate this set clause."

   | setCl |

   colsNvals _validateClass: Array.
   setCl := String new.
   1 to: (colsNvals size) by: 2 do: [:i |
      (setCl isEmpty) ifTrue: [
         setCl := String withAll: 'SET '.
         ]
      ifFalse: [
         setCl add: ', '.
         ].

      setCl add: (colsNvals at: i); add: ' = '.
      setCl add: (self getRdbValueFor: (colsNvals at: (i + 1))).
      ].

   ^setCl
%
category: 'Private'
classmethod: GsPostgresConnection
getPostgresSessionState

"
Private. Do not call directly unless you know what you're doing.
Get the session state array used by Postgres and do lazy initialize if needed.
See comments in #newSessionStateArray method for the array contents."

| idx result sys |
sys := System .
idx := self sessionStateIndex . " 25 "
(result := sys __sessionStateAt: idx)
	ifNil:[  sys __sessionStateAt: idx put: (result := self newSessionStateArray) ].
^ result
%
category: 'Error Handling'
classmethod: GsPostgresConnection
hasPostgresError

"Answer true if the class PostgresError exists in the repository, false otherwise."

^Globals includesKey: #PostgresError
%
category: 'Private'
classmethod: GsPostgresConnection
hasWriteStream: aGsPostgresWriteStream

"Private. Do not call directly unless you know what you're doing."

^ self _allPostgresWriteStreams includesIdentical: aGsPostgresWriteStream
%
category: 'Instance Creation'
classmethod: GsPostgresConnection
new

^ self shouldNotImplement: #new
%
category: 'Instance Creation'
classmethod: GsPostgresConnection
new: size

^ self shouldNotImplement: #new:
%
category: 'Private'
classmethod: GsPostgresConnection
newSessionStateArray

"Private. Do not call directly unless you know what you're doing.
Format of Array stored at session state index 25:
	1 - AllPostgresConnections (connected connections only) (IdentitySet)
	2 - NamedConnectionsDictionary (KeyValueDictionary, name -> connection)
	3 - NamedConnectionsReverseDictionary (IdentityKeyValueDictionary, connection -> Array of names)
	4 - AllWriteStreams (IdentitySet)
"

	^Array
		with: IdentitySet new
		with: KeyValueDictionary new
		with: IdentityKeyValueDictionary new
		with: IdentitySet new
%
category: 'Instance Creation'
classmethod: GsPostgresConnection
newWithParameters: aGsPostgresConnectionParameters

"Create a new Postgres connection using given parameters. The connection is created in a disconnected state. Unicode string classes are not used by default"

^ self newWithParameters: aGsPostgresConnectionParameters unicodeStrings: false
%
category: 'Instance Creation'
classmethod: GsPostgresConnection
newWithParameters: aGsPostgresConnectionParameters unicodeStrings: aBoolean

"Create a new Postgres connection using given parameters.
If aBoolean is true, multibyte strings from Postgres are translated to Unicode16 and Unicode32 objects.
If aBoolean is false, multibyte strings from Postgres are translated to DoubleByteString and QuadByteString objects.
The connection is created in a disconnected state."

^ self basicNew libpq: GsLibpq new ; pgParameters: aGsPostgresConnectionParameters ; unicodeStrings: aBoolean ; yourself
%
category: 'Error Handling'
classmethod: GsPostgresConnection
raiseErrorWithMessage: msg

"Raises an error with message msg"

	^self errorClass signal: msg
%
category: 'Private'
classmethod: GsPostgresConnection
removeConnection: aGsPostgresConnection

"Private. Do not call directly unless you know what you're doing."
self _allPostgresConnections removeIfPresent: aGsPostgresConnection
%
category: 'Private'
classmethod: GsPostgresConnection
removeFromCacheKey: aName

"Private. Do not call directly unless you know what you're doing."
| inst |
inst := self _namedConnectionDictionary removeKey: aName otherwise: nil.
inst ifNotNil:[ (self _namedConnectionReverseDictionary at: inst)  removeIfPresent: aName ].
%
category: 'Private'
classmethod: GsPostgresConnection
removeWriteStream: aGsPostgresWriteStream

"Private. Do not call directly unless you know what you're doing."

self _allPostgresWriteStreams removeIfPresent: aGsPostgresWriteStream
%
category: 'Constants'
classmethod: GsPostgresConnection
sessionStateIndex

"Index used to fetch/store an Array in session state.
See System __sessionStateAt:
"

	^ 25
%
category: 'Private'
classmethod: GsPostgresConnection
_allPostgresConnections

"Private. Do not call directly unless you know what you're doing."

^ self getPostgresSessionState at: 1
%
category: 'Private'
classmethod: GsPostgresConnection
_allPostgresWriteStreams

"Private. Do not call directly unless you know what you're doing."

^ self getPostgresSessionState at: 4
%
category: 'Private'
classmethod: GsPostgresConnection
_cacheConnection: aGsPostgresConnection withName: aName

"Private. Do not call directly unless you know what you're doing."

"Disallows duplicate keys"
(self connectionWithName: aName) notNil
	ifTrue:[ ^ self raiseErrorWithMessage: ('Illegal attempt to reuse cached session name ', aName) ].

self _namedConnectionDictionary at: aName put: aGsPostgresConnection.
(self _namedConnectionReverseDictionary at: aGsPostgresConnection ifAbsentPut:[ Array new]) add: aName.
%
category: 'Private'
classmethod: GsPostgresConnection
_namedConnectionDictionary

"Private. Do not call directly unless you know what you're doing."

^ self getPostgresSessionState at: 2
%
category: 'Private'
classmethod: GsPostgresConnection
_namedConnectionReverseDictionary

"Private. Do not call directly unless you know what you're doing.
Key: aGsPostgresConnection
Value: Array of keys in in namedConnectionDictionary for aGsPostgresConnection"

^ self getPostgresSessionState at: 3
%
! ------------------- Instance methods for GsPostgresConnection
category: 'Transaction Control'
method: GsPostgresConnection
abort

"Executes ROLLBACK on Postgres"

^ self rollback
%
category: 'Private'
method: GsPostgresConnection
basicExecute: aString

"Private. Do not call directly unless you know what you're doing.
Executes aString and returns a GsPostresResult object.
Caller must send #free to the object to free C memory when no longer needed."

^ self basicExecute: aString tupleClass: nil
%
category: 'Private'
method: GsPostgresConnection
basicExecute: aString tupleClass: aClass
"Private. Do not call directly unless you know what you're doing.
Executes aString and returns a GsPostresResult object.
Caller must send #free to the object to free C memory when no longer needed."

	^GsPostgresResult
		newWithLibrary: self libpq
		result: (self primExecute: aString)
		tupleClass: aClass
		sql: aString
		unicodeStrings: self unicodeStrings
%
category: 'Private'
method: GsPostgresConnection
basicExecuteWithParameters: aString numParameters: count paramTypes: typeArray paramValues: values paramLengths: lengths paramFormats: formats resultFormat: formatInt

"Private. Do not call directly unless you know what you're doing."

	| pgResult |
	pgResult := self
				primExecuteWithParameters: aString
				numParameters: count
				paramTypes: typeArray
				paramValues: values
				paramLengths: lengths
				paramFormats: formats
				resultFormat: formatInt.
	^GsPostgresResult
		newWithLibrary: self libpq
		result: pgResult
		sql: aString
		unicodeStrings: self unicodeStrings
%
category: 'Transaction Control'
method: GsPostgresConnection
begin

"Executes BEGIN on Postgres"

self executeNoResults: 'BEGIN' .
^ self
%
category: 'Transaction Control'
method: GsPostgresConnection
beginTransaction

"Executes BEGIN on Postgres"

^ self begin
%
category: 'Connection Management'
method: GsPostgresConnection
cacheWithName: aName

"Add the receiver to the connection cache using key aName. It is an error if aName is already a key in the cache.
GsPostgresConnections may be cached multiple times under unique names."

self class _cacheConnection: self withName: aName
%
category: 'Character Encoding'
method: GsPostgresConnection
characterEncoding

"Answers a Symbol describing the current client character encoding"

^ (self libpq pg_encoding_to_char: (self libpq PQclientEncoding: self pqConnCptr)) asSymbol
%
category: 'Transaction Control'
method: GsPostgresConnection
commit

"Executes COMMIT on Postgres"

self class flushAllStreams .
self executeNoResults: 'COMMIT' .
^ self
%
category: 'Transaction Control'
method: GsPostgresConnection
commitTransaction

"Executes COMMIT on Postgres"

^ self commit
%
category: 'Connection Management'
method: GsPostgresConnection
connect

"Establishes a connection with Postgres. Returns true on success or raises an exception on error."

self connected ifTrue:[ ^ true ] .
pqConnCptr := libpq basicLoginWithString:  self pgParameters asLoginString .
^ (self connected)
	ifTrue:[ self class addConnection: self. true ]
	ifFalse:[ | msg |
			msg := libpq errorMessageForConnection: self pqConnCptr .
			libpq logoutConnection: self pqConnCptr.
			pqConnCptr := nil.
			self raiseErrorWithMessage: msg
	]
%
category: 'Connection Management'
method: GsPostgresConnection
connected

"Answer true if the receiver has a connection to Postgres, false otherwise."

^ libpq connectionOk: pqConnCptr
%
category: 'Connection Management'
method: GsPostgresConnection
disconnect

"Terminates the receiver's connection with Postgres if it has one."

self pqConnCptr notNil ifTrue:[
	self libpq logoutConnection: pqConnCptr.
	self class removeConnection: self.
	pqConnCptr := nil
]
%
category: 'Transaction Control'
method: GsPostgresConnection
end

"Executes COMMIT on Postgres"

^ self commit
%
category: 'Command Execution'
method: GsPostgresConnection
execute: aString

"Executes aString and returns a GsPostgresReadStream object.
Caller must send #free to the result object to free C memory when no longer needed."

	^self execute: aString tupleClass: nil
%
category: 'Command Execution'
method: GsPostgresConnection
execute: aString tupleClass: aClass

"Executes aString and returns a GsPostgresReadStream object.
Caller must send #free to the object to free C memory when no longer needed."

	| result |
	^(result := self basicExecute: aString tupleClass: aClass) statusIsOk
		ifTrue: [GsPostgresReadStream on: result]
		ifFalse:
			[result free.
			self raiseErrorWithMessage: self lastErrorMessage]
%
category: 'Command Execution'
method: GsPostgresConnection
executeAndReturnOneRow: aString
"
Executes the string and returns the first row.
If the first row contains only 1 element, return that element.
If the first row contains more than 1 element, return an OrderedCollection containing the first row.
Returns nil if the execution produced no results.
"

	| pgResult result |
	pgResult := self basicExecute: aString.
	^
	[pgResult statusIsOk
		ifTrue:
			[result := pgResult tupleAtRow: 1.
			result size == 0
				ifTrue: [nil]
				ifFalse: [result size == 1 ifTrue: [result first] ifFalse: [result]]]
		ifFalse: [result := self raiseErrorWithMessage: self lastErrorMessage]]
			ensure: [pgResult free]
%
category: 'Command Execution'
method: GsPostgresConnection
executeNoResults: aString

"Executes aString and returns the receiver on success or raises an exception on error"

| result |
result := self basicExecute: aString .
^ [ result statusIsOk
	ifTrue:[ self ]
	ifFalse:[ self raiseErrorWithMessage: self lastErrorMessage]
] ensure:[ result free ]
%
category: 'Command Execution'
method: GsPostgresConnection
executePreparedStatementWithName: aName parameters: anArray

"Executes a previously prepared statement named aName. Returns true on success or raises an exception on error."

|  result|

result := GsPostgresResult newWithLibrary: self libpq result: (self primExecutePrepared: aName parameters: anArray) unicodeStrings: self unicodeStrings.
^ [ result statusIsOk
	ifTrue:[true ]
	ifFalse:[ self raiseErrorWithMessage: self lastErrorMessage]
] ensure:[ result free ]
%
category: 'Command Execution'
method: GsPostgresConnection
executeReturnRowsAffected: aString

"Returns an integer indicating the number of rows affected by executing aString.
Raises an exception on error."

| result |
result := self basicExecute: aString .
^ [ result statusIsOk
	ifTrue:[ result primRowsAffected ]
	ifFalse:[ self raiseErrorWithMessage: self lastErrorMessage]
] ensure:[ result free ]
%
category: 'Command Execution'
method: GsPostgresConnection
executeWithParameters: aString numParameters: count paramTypes: typeArray paramValues: values paramLengths: lengths paramFormats: formats resultFormat: formatInt

"Executes aString in Postgres where the parameters are contained in values, which must be an Array.
Sinced the elements in values are not part of the executable SQL statement, they must not be escaped.
Returns a GsPostgresReadStream on success, which must be freed with the #free message when no longer needed.
Raises an exception on error."

	| result |
	result := self
				basicExecuteWithParameters: aString
				numParameters: count
				paramTypes: typeArray
				paramValues: values
				paramLengths: lengths
				paramFormats: formats
				resultFormat: formatInt.
	^result isStatusOk
		ifTrue: [GsPostgresReadStream on: result]
		ifFalse:
			[result free.
			self raiseErrorWithMessage: self lastErrorMessage]
%
category: 'Connection Management'
method: GsPostgresConnection
isConnected

"Answer true if the receiver has a connection to Postgres, false otherwise."
^ self connected
%
category: 'Error Handling'
method: GsPostgresConnection
lastErrorMessage

"Answer a String representing the last error messag from Postgres."

	^ self libpq errorMessageForConnection: self pqConnCptr
%
category: 'Accessing'
method: GsPostgresConnection
libpq
	^libpq
%
category: 'Updating'
method: GsPostgresConnection
libpq: newValue
	libpq := newValue
%
category: 'Querying'
method: GsPostgresConnection
libraryVersion

"Answer a String representing the version of Postgres library.
Calls the Postgres function PQlibVersion. See https://www.postgresql.org/docs/11/libpq-misc.html for more information on this function."

^ self libpq PQlibVersion
%
category: 'Command Execution'
method: GsPostgresConnection
openDeleteCursorOn: tupleClass

"Returns an instance of GsPostgresWriteStream which may be used to delete rows the table referenced by tupleClass.
The tuple class must specify the key mapping (method #rdbPrimaryKeyMaps) and table name (method #rdbTableName)."

   ^self openDeleteCursorOn: tupleClass
		keyMapping: (tupleClass rdbPrimaryKeyMaps)
%
category: 'Command Execution'
method: GsPostgresConnection
openDeleteCursorOn: tupleClass keyMapping: keyMap
	"Returns an instance of GsPostgresWriteStream which may be used to delete rows the table referenced by tupleClass using key map keyMap.
keyMap must be an array of GsPostgresColumnMapEntry objects.
The tuple class must specify the name (method #rdbTableName)."

	^self
		openDeleteCursorOn: tupleClass
		keyMapping: keyMap
		tableName: tupleClass rdbTableName
%
category: 'Command Execution'
method: GsPostgresConnection
openDeleteCursorOn: tupleClass keyMapping: keyMap tableName: tableName
	"Returns an instance of GsPostgresWriteStream which may be used to delete rows from table tableName using key map keyMap."

	| keyNames sql bindInfo result |
	keyNames := keyMap collect: [:x | x columnName].
	bindInfo := Array with: tableName.
	bindInfo addAll: keyNames.
	sql := self class generateBindSQLDeleteForTable: tableName keys: keyNames.
	result := GsPostgresWriteStream
				newForCommand: sql
				connection: self
				tupleClass: tupleClass
				columnMapping: nil
				keyMap: keyMap
				bindInfo: bindInfo.
	result prepareStatement.
	^result
%
category: 'Command Execution'
method: GsPostgresConnection
openInsertCursorOn: tupleClass

"Returns an instance of GsPostgresWriteStream which may be used to insert rows into the table referenced by tupleClass.
The tuple class must specify the column mapping (method #rdbColumnMapping) and table name (method #rdbTableName)."

	^self openInsertCursorOn: tupleClass
		columnMapping: tupleClass rdbColumnMapping
%
category: 'Command Execution'
method: GsPostgresConnection
openInsertCursorOn: tupleClass columnMapping: columnMap

"Returns an instance of GsPostgresWriteStream which may be used to insert rows into the table referenced by tupleClass.
The tuple class must specify the table name (method #rdbTableName)."

	^self
		openInsertCursorOn: tupleClass
		columnMapping: columnMap
		tableName: tupleClass rdbTableName
%
category: 'Command Execution'
method: GsPostgresConnection
openInsertCursorOn: tupleClass columnMapping: columnMap tableName: tableName

"Returns an instance of GsPostgresWriteStream which may be used to insert rows into table tableName."

	| colNames sql bindInfo result |
	colNames := columnMap collect: [:x | x columnName ].
	bindInfo := (Array with: tableName) addAll: colNames.
	sql := self class generateBindSQLInsertForTable: tableName
				columns: colNames.
	result := GsPostgresWriteStream
				newForCommand: sql
				connection: self
				tupleClass: tupleClass
				columnMapping: columnMap
				keyMap:  nil
				bindInfo: bindInfo.
	result prepareStatement.
	^ result
%
category: 'Command Execution'
method: GsPostgresConnection
openUpdateCursorOn: tupleClass

"Returns an instance of GsPostgresWriteStream which may be used to update rows in the table referenced by tupleClass.
The tuple class must specify the key mapping (method #rdbPrimaryKeyMaps), column mapping (method #rdbColumnMapping) and table name (method #rdbTableName)."

   ^self openUpdateCursorOn: tupleClass
         columnMapping: (tupleClass rdbColumnMapping)
%
category: 'Command Execution'
method: GsPostgresConnection
openUpdateCursorOn: tupleClass columnMapping: columnMap

"Returns an instance of GsPostgresWriteStream which may be used to update rows in the table referenced by tupleClass.
The tuple class must specify the key mapping (method #rdbPrimaryKeyMaps) and table name (method #rdbTableName)."

   | tableName |
   tableName := tupleClass rdbTableName.
   ^ self openUpdateCursorOn: tupleClass
	columnMapping: columnMap
	keyMapping: (tupleClass rdbPrimaryKeyMaps)
%
category: 'Command Execution'
method: GsPostgresConnection
openUpdateCursorOn: tupleClass columnMapping: columnMap keyMapping: keyMap

"Returns an instance of GsPostgresWriteStream which may be used to update rows in the table referenced by tupleClass.
The tuple class must specify the table name (method #rdbTableName)."

   ^ self openUpdateCursorOn: tupleClass
	columnMapping: columnMap
	keyMapping: keyMap
	tableName: (tupleClass rdbTableName)
%
category: 'Command Execution'
method: GsPostgresConnection
openUpdateCursorOn: tupleClass columnMapping: columnMap keyMapping: keyMap tableName: tableName
	"Returns an instance of GsPostgresWriteStream which may be used to update rows in table tableName."

	| colNames sql bindInfo result keyNames |
	colNames := columnMap collect: [:x | x columnName].
	keyNames := keyMap collect: [:x | x columnName].
	bindInfo := (Array with: tableName) addAll: colNames.
	sql := self class
				generateBindSQLUpdateForTable: tableName
				columns: colNames
				keys: keyNames.
	result := GsPostgresWriteStream
				newForCommand: sql
				connection: self
				tupleClass: tupleClass
				columnMapping: columnMap
				keyMap: keyMap
				bindInfo: bindInfo.
	result prepareStatement.
	^result
%
category: 'Accessing'
method: GsPostgresConnection
pgParameters
	^pgParameters
%
category: 'Updating'
method: GsPostgresConnection
pgParameters: newValue
	pgParameters := newValue
%
category: 'Accessing'
method: GsPostgresConnection
pqConnCptr
	^pqConnCptr
%
category: 'Updating'
method: GsPostgresConnection
pqConnCptr: newValue
	pqConnCptr := newValue
%
category: 'Command Execution'
method: GsPostgresConnection
prepareStatement: sql withName: sName numberParameters: anInt
	"Prepares a statement specified by sql with anInt parmaters and with name sName for subsequent execution in Postgres.
Returns true on success or raises an exception on error."

	| paramTypes result |
	anInt > 0
		ifTrue:
			[paramTypes := Array new.
			anInt timesRepeat: [paramTypes add: 0]	"0 means let the backend decide on the type"].
	result := GsPostgresResult
				newWithLibrary: self libpq
				result: (self primPrepare: sql witName: sName paramenters: paramTypes)
				sql: sql
				unicodeStrings: self unicodeStrings.
	^
	[result statusIsOk
		ifTrue: [true]
		ifFalse: [self raiseErrorWithMessage: self lastErrorMessage]]
			ensure: [result free]
%
category: 'Private'
method: GsPostgresConnection
primExecute: aString

"Private. Do not call directly unless you know what you're doing.
Executes aString in Postgres and returns an instance of CPointer which references a PGresult * object in C which must be freed when no longer required.
Raises an exception on error."

| pqResultCptr |
^ (pqResultCptr := self libpq PQexec: aString onConnection: self pqConnCptr) isNull
	ifTrue:[  self raiseErrorWithMessage: ('NULL returned by #PQexec ' , aString )]
	ifFalse:[ pqResultCptr ]
%
category: 'Private'
method: GsPostgresConnection
primExecutePrepared: cmdName parameters: arrayOfParams

"Private. Do not call directly unless you know what you're doing.
Executes a previously prepared command with name cmdName in Postgres with parameters arrayOfParams.
Parameters must  ot be esacped.
Returns an instance of CPointer which references a PGresult * object in C which must be freed when no longer required.
Raises an exception on error."

| pgResult |
^ (pgResult := self libpq PQexecPrepared:  cmdName paramValues: arrayOfParams paramLengths: nil paramFormats: nil resultFormat: 1 on: self pqConnCptr) isNull
	ifTrue:[  self raiseErrorWithMessage: ('NULL returned by #PQexecPrepared: ' , cmdName )]
	ifFalse:[ pgResult ]
%
category: 'Private'
method: GsPostgresConnection
primExecuteWithParameters: aString numParameters: count paramTypes: typeArray paramValues: values paramLengths: lengths paramFormats: formats resultFormat: formatInt

"Private. Do not call directly unless you know what you're doing."

| pgResult |
pgResult := self libpq PQexecParams:  aString numParams: count paramTypes: typeArray paramValues: values paramLengths: lengths paramFormats: formats resultFormat: formatInt on: self conn.
^ pgResult isNull
	ifTrue:[  self raiseErrorWithMessage: ('NULL returned by #executeWithParameters: ' , aString )]
	ifFalse:[pgResult]
%
category: 'Private'
method: GsPostgresConnection
primPrepare: sql witName: sName paramenters: anArray

"Private. Do not call directly unless you know what you're doing.
Prepares string sql with name sName and parameters anArray for subsequenty execution."

| pgResult |
^ (pgResult := self libpq PQprepare: sql  statementName: sName numParams: anArray size paramTypes: anArray on: self pqConnCptr) isNull
	ifTrue:[  self raiseErrorWithMessage: ('NULL returned by #PQprepare ' , sql )]
	ifFalse:[ pgResult ]
%
category: 'Error Handling'
method: GsPostgresConnection
raiseErrorWithMessage: msg

"Raises an exception with message msg"

^ self class raiseErrorWithMessage: msg
%
category: 'Transaction Control'
method: GsPostgresConnection
rollback

"Executes ROLLBACK on Postgres"

self class clearAllStreams .
self executeNoResults: 'ROLLBACK' .
^ self
%
category: 'Transaction Control'
method: GsPostgresConnection
rollbackTransaction

"Executes ROLLBACK on Postgres"

^ self rollback
%
category: 'Character Encoding'
method: GsPostgresConnection
setCharacterEncoding: aSymbol

"Sets the client character encoding to be a Symbol. Returns true on success or raises an exception on error.
The Postgres documentation lists the valid character encodings here: https://www.postgresql.org/docs/current/multibyte.html.
Encodings other than #Utf8 are unlikely to function correctly."

^  (self libpq PQsetClientEncoding: aSymbol asString on: pqConnCptr) == 0
	ifTrue:[ true ]
	ifFalse:[ self raiseErrorWithMessage: ('Invalid character encoding: ', aSymbol asString) ]
%
category: 'Accessing'
method: GsPostgresConnection
unicodeStrings
	^unicodeStrings
%
category: 'Updating'
method: GsPostgresConnection
unicodeStrings: newValue
	unicodeStrings := newValue
%
! ------------------- Remove existing behavior from GsPostgresConnectionParameters
removeAllMethods GsPostgresConnectionParameters
removeAllClassMethods GsPostgresConnectionParameters
! ------------------- Class methods for GsPostgresConnectionParameters
category: 'Class Initialization'
classmethod: GsPostgresConnectionParameters
initialize

"Initialize the receiver"

AllLegalParameters := IdentitySet withAll: self legalParameters
%
category: 'Class Initialization'
classmethod: GsPostgresConnectionParameters
legalParameters

"Answer an Array of Symbols representing all legal Postgres login parameters.

See https://www.postgresql.org/docs/11/libpq-connect.html#LIBPQ-PARAMKEYWORDS for meanings."

^ {
	#host .
	#hostaddr .
	#port .
	#dbname .
	#user .
	#password .
	#passfile .
	#connect_timeout .
	#client_encoding .
	#options .
	#application_name .
	#fallback_application_name .
	#keepalives .
	#keepalives_idle .
	#keepalives_interval .
	#keepalives_count .
	#replication .
	#sslmode .
	#sslcompression .
	#sslcert .
	#sslkey .
	#sslrootcert .
	#sslcrl .
	#requirepeer .
	#krbsrvname .
	#service .
	#target_session_attrs
}



%
category: 'Instance Creation'
classmethod: GsPostgresConnectionParameters
new

"Answer a new instance of the receiver"

^super new initialize
%
category: 'Testing'
classmethod: GsPostgresConnectionParameters
parameterIsCollection: aParameter

"Answer true if aParameter is a Postgres login parameter that accepts a collection argument, false otherwise."

^self parametersThatAreCollections includesIdentical: aParameter asSymbol
%
category: 'Testing'
classmethod: GsPostgresConnectionParameters
parameterIsLegal: aParameter

"Answer true if aParameter is a legal Postgres login parameter, false otherwise."

^AllLegalParameters includesIdentical: aParameter asSymbol
%
category: 'Private'
classmethod: GsPostgresConnectionParameters
parametersThatAreCollections

"Private. Do not call directly unless you know what you're doing.
Array of Symbols which are login parameters that accept multiple values.

See https://www.postgresql.org/docs/11/libpq-connect.html#LIBPQ-PARAMKEYWORDS for meanings."

^ {
	#host .
	#hostaddr .
	#port
}



%
! ------------------- Instance methods for GsPostgresConnectionParameters
category: 'Private'
method: GsPostgresConnectionParameters
addValue: val toCollectionForParameter: param

"Private. Do not call directly unless you know what you're doing."

(self collectionForParameter: param) add: val
%
category: 'Accessing'
method: GsPostgresConnectionParameters
application_name
	^ parameterDictionary at: #application_name otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
application_name: newValue
	parameterDictionary at: #application_name put: newValue
%
category: 'Formatting'
method: GsPostgresConnectionParameters
asLoginString

"Answer the receiver represented as a Postgres login string."

| result |
result := String new.
self writeLoginStringOn: (AppendStream on: result) .
^ result
%
category: 'Accessing'
method: GsPostgresConnectionParameters
client_encoding
	^ parameterDictionary at: #client_encoding otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
client_encoding: newValue
	parameterDictionary at: #client_encoding put: newValue
%
category: 'Private'
method: GsPostgresConnectionParameters
collectionForParameter: param

"Private. Do not call directly unless you know what you're doing."

^ parameterDictionary at: param ifAbsentPut:[ OrderedCollection new]
%
category: 'Accessing'
method: GsPostgresConnectionParameters
connect_timeout
	^ parameterDictionary at: #connect_timeout otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
connect_timeout: newValue
	parameterDictionary at: #connect_timeout put: newValue
%
category: 'Accessing'
method: GsPostgresConnectionParameters
dbname
	^ parameterDictionary at: #dbname otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
dbname: newValue
	parameterDictionary at: #dbname put: newValue
%
category: 'Accessing'
method: GsPostgresConnectionParameters
fallback_application_name
	^ parameterDictionary at: #fallback_application_name otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
fallback_application_name: newValue
	parameterDictionary at: #fallback_application_name put: newValue
%
category: 'Accessing'
method: GsPostgresConnectionParameters
host
	^ parameterDictionary at: #host otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
host: newValue
	self addValue: newValue toCollectionForParameter: #host
%
category: 'Accessing'
method: GsPostgresConnectionParameters
hostaddr
	^ parameterDictionary at: #hostaddr otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
hostaddr: newValue
	self addValue: newValue toCollectionForParameter: #hostaddr
%
category: 'Initialize'
method: GsPostgresConnectionParameters
initialize
	parameterDictionary := SymbolKeyValueDictionary new.
%
category: 'Accessing'
method: GsPostgresConnectionParameters
keepalives
	^ parameterDictionary at: #keepalives otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
keepalives: newValue
	parameterDictionary at: #keepalives put: newValue
%
category: 'Accessing'
method: GsPostgresConnectionParameters
keepalives_count
	^ parameterDictionary at: #keepalives_count otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
keepalives_count: newValue
	parameterDictionary at: #keepalives_count put: newValue
%
category: 'Accessing'
method: GsPostgresConnectionParameters
keepalives_idle
	^ parameterDictionary at: #keepalives_idle otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
keepalives_idle: newValue
	parameterDictionary at: #keepalives_idle put: newValue
%
category: 'Accessing'
method: GsPostgresConnectionParameters
keepalives_interval
	^ parameterDictionary at: #keepalives_interval otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
keepalives_interval: newValue
	parameterDictionary at: #keepalives_interval put: newValue
%
category: 'Accessing'
method: GsPostgresConnectionParameters
krbsrvname
	^ parameterDictionary at: #krbsrvname otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
krbsrvname: newValue
	parameterDictionary at: #krbsrvname put: newValue
%
category: 'Accessing'
method: GsPostgresConnectionParameters
options
	^ parameterDictionary at: #options otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
options: newValue
	parameterDictionary at: #options put: newValue
%
category: 'Accessing'
method: GsPostgresConnectionParameters
passfile
	^ parameterDictionary at: #passfile otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
passfile: newValue
	parameterDictionary at: #passfile put: newValue
%
category: 'Accessing'
method: GsPostgresConnectionParameters
password
	^ parameterDictionary at: #password otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
password: newValue
	parameterDictionary at: #password put: newValue
%
category: 'Accessing'
method: GsPostgresConnectionParameters
port
	^ parameterDictionary at: #port otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
port: newValue
	self addValue: newValue toCollectionForParameter: #port
%
category: 'Accessing'
method: GsPostgresConnectionParameters
replication
	^ parameterDictionary at: #replication otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
replication: newValue
	parameterDictionary at: #replication put: newValue
%
category: 'Accessing'
method: GsPostgresConnectionParameters
requirepeer
	^ parameterDictionary at: #requirepeer otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
requirepeer: newValue
	parameterDictionary at: #requirepeer put: newValue
%
category: 'Accessing'
method: GsPostgresConnectionParameters
service
	^ parameterDictionary at: #service otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
service: newValue
	parameterDictionary at: #service put: newValue
%
category: 'Accessing'
method: GsPostgresConnectionParameters
sslcert
	^ parameterDictionary at: #sslcert otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
sslcert: newValue
	parameterDictionary at: #sslcert put: newValue
%
category: 'Accessing'
method: GsPostgresConnectionParameters
sslcompression
	^ parameterDictionary at: #sslcompression otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
sslcompression: newValue
	parameterDictionary at: #sslcompression put: newValue
%
category: 'Accessing'
method: GsPostgresConnectionParameters
sslcrl
	^ parameterDictionary at: #sslcrl otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
sslcrl: newValue
	parameterDictionary at: #sslcrl put: newValue
%
category: 'Accessing'
method: GsPostgresConnectionParameters
sslkey
	^ parameterDictionary at: #sslkey otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
sslkey: newValue
	parameterDictionary at: #sslkey put: newValue
%
category: 'Accessing'
method: GsPostgresConnectionParameters
sslmode
	^ parameterDictionary at: #sslmode otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
sslmode: newValue
	parameterDictionary at: #sslmode put: newValue
%
category: 'Accessing'
method: GsPostgresConnectionParameters
sslrootcert
	^ parameterDictionary at: #sslrootcert otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
sslrootcert: newValue
	parameterDictionary at: #sslrootcert put: newValue
%
category: 'Accessing'
method: GsPostgresConnectionParameters
target_session_attrs
	^ parameterDictionary at: #target_session_attrs otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
target_session_attrs: newValue
	parameterDictionary at: #target_session_attrs put: newValue
%
category: 'Accessing'
method: GsPostgresConnectionParameters
user
	^ parameterDictionary at: #user otherwise: nil
%
category: 'Updating'
method: GsPostgresConnectionParameters
user: newValue
	parameterDictionary at: #user put: newValue
%
category: 'Formatting'
method: GsPostgresConnectionParameters
writeLoginStringOn: ws

"Write the receiver as a Postgres login string to ws which is expected to be a writable stream."

parameterDictionary keysAndValuesDo:[:k :v|
	(self class parameterIsCollection: k)
		ifTrue:[ self writeParameter: k collection: v toStream: ws ]
		ifFalse:[ ws isEmpty ifFalse:[ws space ].
			       ws nextPutAll: k asString ;
					nextPut: $= ;
					nextPutAll: v asString.
		]
].
^ self
%
category: 'Formatting'
method: GsPostgresConnectionParameters
writeParameter: param collection: array toStream: ws

"Write the given parameter param to the stream ws."

ws isEmpty
	ifFalse:[ws space ].
ws nextPutAll: param asString ;
	nextPut: $= .
1 to: array size do:[:n| n == 1 ifFalse:[ ws nextPut: $, ]. ws nextPutAll: (array at: n) asString].
^ self


%
! ------------------- Remove existing behavior from GsPostgresReadStream
removeAllMethods GsPostgresReadStream
removeAllClassMethods GsPostgresReadStream
! ------------------- Class methods for GsPostgresReadStream
category: 'Species'
classmethod: GsPostgresReadStream
collectionSpecies

	^ OrderedCollection
%
category: 'Private'
classmethod: GsPostgresReadStream
on: aGsPostgresResult

"Private. Do not call directly unless you know what you're doing."

	^(self new initialize)
		queryResult: aGsPostgresResult;
		readLimit: aGsPostgresResult numRows ;
		yourself
%
! ------------------- Instance methods for GsPostgresReadStream
category: 'Testing'
method: GsPostgresReadStream
atBeginning
"Answer true if the stream is positioned at the beginning"

^position == 0
%
category: 'Testing'
method: GsPostgresReadStream
atEnd

"Answer true if there are no more elements in the stream."

^ self position >= self readLimit
%
category: 'Testing'
method: GsPostgresReadStream
beforeEnd
"Returns true if the receiver can access more objects, false if not .
 GemStone extension. "

^position < self readLimit
%
category: 'Freeing'
method: GsPostgresReadStream
close


^ self free
%
category: 'Accessing'
method: GsPostgresReadStream
collectionSpecies

^ self class collectionSpecies
%
category: 'Accessing'
method: GsPostgresReadStream
columnNameAt: index

"Answer the column name with the given index. The first column has an index of 1."

^ self queryResult columnNameAt: index
%
category: 'Stream Operations'
method: GsPostgresReadStream
contents

"Return the entire contents of the receiver in an OrderedCollection.
Warning: may cause out of memory errors for querys with large result sets!"

| result |
result := self collectionSpecies new.
1 to: self readLimit do:[:n| result add: (self queryResult tupleAtRow: n) ] .
^ result
%
category: 'Freeing'
method: GsPostgresReadStream
free

"Free the C memory used by the receiver. The receiver may not be read after this message has been sent."

	self queryResult
		ifNotNil:
			[self queryResult free.
			self
				queryResult: nil;
				position: 0].
	^self
%
category: 'Initialize'
method: GsPostgresReadStream
initialize

position := 0.
^ self
%
category: 'Testing'
method: GsPostgresReadStream
isExternal

"Returns true if the source of the receiver's information
 is external to the image, and false otherwise."

^true
%
category: 'Stream Operations'
method: GsPostgresReadStream
next

"Answer the next element in the stream or nil if there are no more elements."

^ self atEnd
	ifTrue:[ nil ]
	ifFalse:[ position := position + 1.
			self queryResult tupleAtRow: position ]
%
category: 'Stream Operations'
method: GsPostgresReadStream
next: amount

"Answer an OrderedCollection containing the next amount elements in the stream or as many elements that have not yet been read, whichever is less.
Answers nil if there are no more elements."

|  result |
result := self collectionSpecies new.
amount timesRepeat:[ | obj |
	(obj := self next) ifNil:[ ^ result ]
					ifNotNil:[ result add: obj]
].

^ result
%
category: 'Stream Operations'
method: GsPostgresReadStream
next: n into: aCollection startingAt: startIndex

"Read n objects into the given collection.
Return aCollection or a partial copy if less than
n elements have been read."

| max pos coll |
coll := self next: n.
aCollection
	replaceFrom: startIndex
	to: startIndex+coll size -1
	with: coll
	startingAt: 1.
^ aCollection
%
category: 'Stream Operations'
method: GsPostgresReadStream
nextMatchFor: anObject

	"The first object is removed from the receiver's future sequence value and appended to the end of
	the receiver's past sequence values. The value that would result from sending #= to the object with
	anObject as the argument is returned.
	The results are undefined if there are no future sequence values in the receiver.

	ANSI 5.9.2.6"

	^self next = anObject.
%
category: 'Stream Operations'
method: GsPostgresReadStream
nextPut: anObject

"Disallowed.  You cannot write to a ReadStream."

self shouldNotImplement: #nextPut:
%
category: 'Stream Operations'
method: GsPostgresReadStream
nextPutAll: anObject

"Disallowed.  You cannot write to a ReadStream."

self shouldNotImplement: #nextPutAll:
%
category: 'Accessing'
method: GsPostgresReadStream
numColumns

"Answer the number of columns."

^ self queryResult numColumns
%
category: 'Overriding'
method: GsPostgresReadStream
overrideType: aClass forColumnNumber: anInt

"Forces objects created from column number anInt to be instances of aClass,
overriding the default type mapping.
aClass must be a kind of Class.
anInt must be a valid column number for the receiver.
Returns the reciever on success."

self queryResult overrideType: aClass forColumnNumber: anInt .
^ self
%
category: 'Stream Operations'
method: GsPostgresReadStream
peek

"Answer the next element in the stream without advancing the position, or nil if there are no more elements."

^ self atEnd
	ifTrue:[ nil ]
	ifFalse:[ self queryResult tupleAtRow: (position + 1 )]
%
category: 'Stream Operations'
method: GsPostgresReadStream
peekN: amount

"Answer an OrderedCollection containing the next amount elements in  in the stream without advancing the position, or nil if there are no more elements."

|  result savePosition |
savePosition := self position.
result := self next: amount.
self position: savePosition.
^ result
%
category: 'Accessing'
method: GsPostgresReadStream
position

"Answer the position. The first position is 0."

	^position
%
category: 'Updating'
method: GsPostgresReadStream
position: newValue
	position := newValue
%
category: 'Accessing'
method: GsPostgresReadStream
queryResult
	^queryResult
%
category: 'Updating'
method: GsPostgresReadStream
queryResult: newValue
	queryResult := newValue
%
category: 'Accessing'
method: GsPostgresReadStream
readLimit

"Answer the readLimit, which is the number of elements in the stream."

	^readLimit
%
category: 'Updating'
method: GsPostgresReadStream
readLimit: newValue
	readLimit := newValue
%
category: 'Positioning'
method: GsPostgresReadStream
reset
"Set the receiver's position to the beginning of the sequence of objects."

position := 0
%
category: 'Updating'
method: GsPostgresReadStream
setToEnd
"Set the position of the receiver to the end of the sequence of objects."

position := readLimit
%
category: 'Accessing'
method: GsPostgresReadStream
size
"Compatibility with other streams"
^readLimit
%
category: 'Updating'
method: GsPostgresReadStream
skip: anInteger
"Set the receiver's position to be the current position+anInteger. Do not
 throw error if skipAmount would exceed collection bounds - ANSI compliance. "

self position: ((position + anInteger max: 0) min: readLimit)
%
category: 'Stream Operations'
method: GsPostgresReadStream
upToEnd

"Return all elements in the receiver from the current position up to the end of the stream.
Warning: may cause out of memory errors for querys with large result sets!"

^ self next: (self readLimit - self position)
%
! ------------------- Remove existing behavior from GsPostgresColumnMapEntry
removeAllMethods GsPostgresColumnMapEntry
removeAllClassMethods GsPostgresColumnMapEntry
! ------------------- Class methods for GsPostgresColumnMapEntry
category: 'Defaults'
classmethod: GsPostgresColumnMapEntry
defaultGetSelectorForInstVar: anIv

"Answer a symbol which is the defaul accessor selector for instance variable anIv"

^ anIv asSymbol
%
category: 'Defaults'
classmethod: GsPostgresColumnMapEntry
defaultSetSelectorForInstVar: anIv

"Answer a symbol which is the defaul setter selector for instance variable anIv (anIv:) "

^ ((String withAll: anIv asString) add: $: ; yourself ) asSymbol
%
category: 'Instance Creation'
classmethod: GsPostgresColumnMapEntry
newForColumn: colName instVar: ivName

"Creates a new instance with default getter and setter method names and no inst var object kind"

	^self
		newForColumn: colName
		instVar: ivName
		getSelector: (self defaultGetSelectorForInstVar: ivName)
		setSelector: (self defaultSetSelectorForInstVar: ivName)
		instVarClass: nil
%
category: 'Instance Creation'
classmethod: GsPostgresColumnMapEntry
newForColumn: colName instVar: ivName getSelector: getter setSelector: setter instVarClass: aClass

	^(self new)
		columnName: colName asSymbol;
		instVarName: ivName asSymbol;
		getMethodSelector: getter asSymbol ;
		setMethodSelector: setter asSymbol ;
		instVarClass: aClass ;
		yourself
%
category: 'Instance Creation'
classmethod: GsPostgresColumnMapEntry
newForColumn: colName instVar: ivName instVarClass: aClass

	^self
		newForColumn: colName
		instVar: ivName
		getSelector: (self defaultGetSelectorForInstVar: ivName)
		setSelector: (self defaultSetSelectorForInstVar: ivName)
		instVarClass: aClass
%
! ------------------- Instance methods for GsPostgresColumnMapEntry
category: 'Accessing'
method: GsPostgresColumnMapEntry
columnName
	^columnName
%
category: 'Updating'
method: GsPostgresColumnMapEntry
columnName: newValue
	columnName := newValue
%
category: 'Accessing'
method: GsPostgresColumnMapEntry
getMethodSelector
	^getMethodSelector
%
category: 'Updating'
method: GsPostgresColumnMapEntry
getMethodSelector: newValue
	getMethodSelector := newValue
%
category: 'Accessing'
method: GsPostgresColumnMapEntry
instVarClass
	^instVarClass
%
category: 'Updating'
method: GsPostgresColumnMapEntry
instVarClass: newValue
	instVarClass := newValue
%
category: 'Accessing'
method: GsPostgresColumnMapEntry
instVarName
	^instVarName
%
category: 'Updating'
method: GsPostgresColumnMapEntry
instVarName: newValue
	instVarName := newValue
%
category: 'Accessing'
method: GsPostgresColumnMapEntry
setMethodSelector
	^setMethodSelector
%
category: 'Updating'
method: GsPostgresColumnMapEntry
setMethodSelector: newValue
	setMethodSelector := newValue
%
! ------------------- Remove existing behavior from GsPostgresResult
removeAllMethods GsPostgresResult
removeAllClassMethods GsPostgresResult
! ------------------- Class methods for GsPostgresResult
category: 'Converting'
classmethod: GsPostgresResult
booleanFromPostgresString: pgString

pgString = 't' ifTrue:[ ^ true ].
pgString = 'f' ifTrue:[ ^ false ].
^ self raiseErrorWithMessage: ('invalid postgres string for boolean: ', pgString)
%
category: 'Converting'
classmethod: GsPostgresResult
createByteArrayFromString: pgString

"String format should be \x followed by hex digits."

	((pgString at: 1) ~~ $\ or: [(pgString at: 2) ~~ $x])
		ifTrue:
			[^self
				raiseErrorWithMessage: 'invalid postgres string for ByteArray: ' , pgString].
	^ByteArray fromHexString: (pgString copyFrom: 3 to: pgString size)
%
category: 'Converting'
classmethod: GsPostgresResult
createFloatFromString: aString

^ self createInstanceOf: Float fromFloatString: aString
%
category: 'Converting'
classmethod: GsPostgresResult
createInstanceOf: aClass fromFloatString: aString
	"GsPostgresResult createInstanceOf: Float fromFloatString: '  $3,331.82'"
	"GsPostgresResult createInstanceOf: ScaledDecimal fromFloatString: '  $3,331.82'"
	"GsPostgresResult createInstanceOf: SmallDouble fromFloatString: '  $3,331.82'"
	"GsPostgresResult createInstanceOf: SmallDouble fromFloatString: '  3.1415927'"

	"This code is needed to make Postgres 'money' type map correctly to ScaledDecimal. The $$ and $, characters need to be removed"

	| rs str |
	rs := ReadStreamPortable on: aString.
	rs skipSeparators.
	rs peek == $$
		ifTrue:  "We have a currency "
			[str := String new.
			[rs atEnd] whileFalse:
					[| char |
					char := rs next.
					char isDigit
						ifTrue: [str add: char]
						ifFalse: [rs position == (rs size - 2) ifTrue: [str add: char]]]]
		ifFalse: [str := rs upToEnd]. "Not a currency, just create the class from what we have"
	^aClass fromString: str
%
category: 'Converting'
classmethod: GsPostgresResult
createInstanceOf: aClass fromPostgresString: aString

"Create a new instance of aClass from Postgres string aString. Raise an error if the conversion could not be performed."

	| array |
	array := ClassAndSelctorTable at: aClass otherwise: nil.
	^array
		ifNil: [aClass fromString: aString]
		ifNotNil: [array first perform: (array at: 2) with: aString]
%
category: 'Converting'
classmethod: GsPostgresResult
createNonUnicodeFromCByteArray: cb

"Answers an instance of String, DoubleByteString or QuadByterString"

^ cb decodeUTF8from: 0 to: (cb size - 1) unicode: false
%
category: 'Converting'
classmethod: GsPostgresResult
createScaledDecimalFromString: aString

	"GsPostgresResult createScaledDecimalFromString: '  $3,331.82'"


^ self createInstanceOf: ScaledDecimal fromFloatString: aString
%
category: 'Converting'
classmethod: GsPostgresResult
createStringFromString: aString

^ aString decodeFromUTF8ToString
%
category: 'Converting'
classmethod: GsPostgresResult
createSymbolFromString: aString

^ aString decodeFromUTF8ToString asSymbol
%
category: 'Converting'
classmethod: GsPostgresResult
createUnicodeFromCByteArray: cb

"Answer an instance of Unicode7, Unicode16 or Unicode32"

^ cb decodeUTF8from: 0 to: (cb size - 1) unicode: true
%
category: 'Converting'
classmethod: GsPostgresResult
createUnicodeFromString: aString

^ aString decodeFromUTF8ToUnicode
%
category: 'Converting'
classmethod: GsPostgresResult
dateAndTimeFromTimestampTz: aString

"Create a DateAndTime (or SmallDateAndTime if  supported) object from a Postgres timestamp string.
The Postgres timestamp string may or may not include the timezone.

GsPostgresResult dateAndTimeFromTimestampTz: '2017-08-25 04:50:00-07'
"

	| rs year month day hour minute second tzoffset sign negate haveMicro micro |
	negate := false.
	tzoffset := 0.
	rs := ReadStreamPortable on: aString.
	year := (rs upTo: $-) asInteger.
	month := (rs upTo: $-) asInteger.
	day := (rs upTo: Character space) asInteger.
	hour := (rs upTo: $:) asInteger.
	minute := (rs upTo: $:) asInteger.
	second := (rs upToAnyOf:
					{$+.
					$-}
				do: [:char | negate := char == $-]) asFloat.
	rs atEnd
		ifFalse:
			["We have a time zone offset, either + or -"
			tzoffset := rs upToEnd asInteger * 3600.
			negate ifTrue: [tzoffset := tzoffset negated]].
	^DateAndTime
		year: year
		month: month
		day: day
		hour: hour
		minute: minute
		second: second
		offset: (Duration seconds: tzoffset)
%
category: 'Converting'
classmethod: GsPostgresResult
dateFromPostgresString: aString

"Create a Date (or SmallDate if supported) form a Postgres Date string."

^Date fromString: aString usingFormat: #(3 2 1 $- 1 1)
%
category: 'Converting'
classmethod: GsPostgresResult
dateTimeFromTimestampDiscardTz: aString

"Convert a Postgres timezone string to a DateTime object.

*** WARNING ***
There is no good way to convert a Postgres timezone offset (-07) to a GS timezone object.
For this reason, the Postgres timezone offset is DISGARDED!! Use DateAndTime instead of DateTime
to avoid this problem.
*** WARNING ***

GsPostgresResult dateTimeFromTimestampDiscardTz: '2017-08-25 04:50:12.222-07'
"

	| rs year month day hour minute second ms tmp haveMs |
	haveMs := false.
	rs := ReadStreamPortable on: aString.
	year := (rs upTo: $-) asInteger.
	month := (rs upTo: $-) asInteger.
	day := (rs upTo: Character space) asInteger.
	hour := (rs upTo: $:) asInteger.
	minute := (rs upTo: $:) asInteger.
	second := (rs upToAnyOf:
					{$+.
					$-.
					$.}
				do: [:char | haveMs := char == $.]) asInteger.
	haveMs
		ifTrue:
			[ms := rs upToAny:
							{$+.
							$-}.
			[ms size < 3] whileTrue: [ms add: $0].
			ms := ms asInteger]
		ifFalse: [ms := 0].
	^(DateTime
		newWithYear: year
		month: month
		day: day
		hours: hour
		minutes: minute
		seconds: second) addMilliseconds: ms
%
category: 'Converting'
classmethod: GsPostgresResult
defaultClassForOid: anInt

"Answer the default GemStone class for Postgres type with OID anInt.
Answer nil if no default maping is defined."

^ FieldTypeTable at: anInt otherwise: nil
%
category: 'Class Initialization'
classmethod: GsPostgresResult
initialize

"GsPostgresResult initialize"
^ self initializeExecStatusTable ; initializeFieldTypeTable ; initializeClassAndSelctorTable
%
category: 'Class Initialization'
classmethod: GsPostgresResult
initializeClassAndSelctorTable
	"GsPostgresResult initializeClassAndSelctorTable"

	| blk |
	ClassAndSelctorTable := IdentityKeyValueDictionary new.
	ClassAndSelctorTable
		at: Boolean put: (Array with: self with: #booleanFromPostgresString:);
		at: DateAndTime put: (Array with: self with: #dateAndTimeFromTimestampTz:);
		at: DateTime put: (Array with: self with: #dateTimeFromTimestampDiscardTz:);
		at: Date put: (Array with: self with: #dateFromPostgresString:);
		at: Time put: (Array with: self with: #timeFromPostgresTimeString:);
		at: Unicode7 put: (Array with: self with: #createUnicodeFromString:);
		at: Unicode16 put: (Array with: self with: #createUnicodeFromString:);
		at: Unicode32 put: (Array with: self with: #createUnicodeFromString:);
		at: String put: (Array with: self with: #createStringFromString:);
		at: DoubleByteString put: (Array with: self with: #createStringFromString:);
		at: QuadByteString put: (Array with: self with: #createStringFromString:);
		at: ByteArray put: (Array with: self with: #createByteArrayFromString:);
		at: Symbol put: (Array with: self with: #createSymbolFromString:);
		at: ScaledDecimal
			put: (Array with: self with: #createScaledDecimalFromString:) ;
		at: Float
			put: (Array with: self with: #createFloatFromString:).

	"Add new special classes if this GS version has them"
	blk :=
			[:sym :existingCls |
			| cls |
			(cls := Globals at: sym otherwise: nil)
				ifNotNil:
					[ClassAndSelctorTable at: cls put: (ClassAndSelctorTable at: existingCls)]].
	blk
		value: #SmallDateAndTime value: DateAndTime;
		value: #SmallDate value: Date;
		value: #SmallTime value: Time;
		value: #SmallScaledDecimal value: ScaledDecimal ;
		value: #SmallDouble value: Float.
	^self
%
category: 'Class Initialization'
classmethod: GsPostgresResult
initializeExecStatusTable

ExecStatusTable := IntegerKeyValueDictionary new.
ExecStatusTable
	at: 0 put: #PGRES_EMPTY_QUERY ; "empty query string was executed"
	at: 1 put: #PGRES_COMMAND_OK ; "a query command that doesn't return aything was executed properly by the backend"
	at: 2 put: #PGRES_TUPLES_OK ; "a query command that returns tuples was executed properly by the backend, PGresult contains the result tuples"
	at: 3 put: #PGRES_COPY_OUT ; "Copy Out data transfer in progress"
	at: 4 put: #PGRES_COPY_IN ; "Copy In data transfer in progress"
	at: 5 put: #PGRES_BAD_RESPONSE ; "an unexpected response was recv'd from the backend"
	at: 6 put: #PGRES_NONFATAL_ERROR ; "notice or warning message"
	at: 7 put: #PGRES_FATAL_ERROR ; "query failed"
	at: 8 put: #PGRES_COPY_BOTH ; "Copy In/Out data transfer in progress"
	at: 9 put: #PGRES_SINGLE_TUPL . "single tuple from larger result set"
^self
%
category: 'Class Initialization'
classmethod: GsPostgresResult
initializeFieldTypeTable

"Set the table that maps Postgres OID to GemStone.
This table specifies the default translation only and may be overridden.

GsPostgresResult initializeFieldTypeTable
"

FieldTypeTable := IntegerKeyValueDictionary new.
FieldTypeTable
	at: 16 put: Boolean ;"BOOLOID"
	at: 17 put: ByteArray ; "BYTEAOID"
	at: 18 put: Character ; "CHAROID"
	at: 20 put: LargeInteger ; "INT8OID"
	at: 21 put: SmallInteger ; "INT2OID"
	at: 23 put: SmallInteger ; "INT4OID"
	at: 700 put: Float ; "FLOAT4OID"
	at: 701 put: Float ; "FLOAT8OID"
	at: 1082 put: Date ; "DATEOID"
	at: 1083 put: Time ; "TIMEOID"
	at: 1266 put: Time ; "TIMETZOID"
	at: 1114 put: DateAndTime ; "TIMESTAMPOID"
	at: 1184 put: DateAndTime ; "TIMESTAMPTZOID"
	at: 1700 put: ScaledDecimal . "NUMERICOID"

^self
%
category: 'Instance Creation'
classmethod: GsPostgresResult
newWithLibrary: aGsLibpq result: aPqResultCptr sql: aString unicodeStrings: aBoolean

"Create a new instance. aGsLibpq is the instance of GsLibpq creating the new instance and
aPqResultCptr is C pointer to a PGresult. The result does not use a tuple class."

	^self
		newWithLibrary: aGsLibpq
		result: aPqResultCptr
		tupleClass: nil
		sql: aString
		unicodeStrings: aBoolean
%
category: 'Instance Creation'
classmethod: GsPostgresResult
newWithLibrary: aGsLibpq result: aPqResultCptr tupleClass: aClass sql: aString unicodeStrings: aBoolean

"Create a new instance. aGsLibpq is the instance of GsLibpq creating the new instance and
aPqResultCptr is C pointer to a PGresult. The result uses aClass for its tuple class."

	^(self new)
		libpq: aGsLibpq;
		pqResultCptr: aPqResultCptr;
		tupleClass: aClass ;
		sql: aString ;
		unicodeStrings: aBoolean ;
		initialize
%
category: 'Instance Creation'
classmethod: GsPostgresResult
newWithLibrary: aGsLibpq result: aPqResultCptr unicodeStrings: aBoolean

"Create a new instance. aGsLibpq is the instance of GsLibpq creating the new instance and
aPqResultCptr is C pointer to a PGresult. The result has no tuple class and no SQL
statement. Used to create results when executing prepared SQL statements."

	^self newWithLibrary: aGsLibpq result: aPqResultCptr sql: nil unicodeStrings: aBoolean
%
category: 'Converting'
classmethod: GsPostgresResult
postgresOids

^ self

"The following is a list of all valid Postgres OID types taken from the  source code:


#define BOOLOID 16
#define BYTEAOID 17
#define CHAROID 18
#define NAMEOID 19
#define INT8OID 20
#define INT2OID 21
#define INT2VECTOROID 22
#define INT4OID 23
#define REGPROCOID 24
#define TEXTOID 25
#define OIDOID 26
#define TIDOID 27
#define XIDOID 28
#define CIDOID 29
#define OIDVECTOROID 30
#define JSONOID 114
#define XMLOID 142
#define XMLARRAYOID 143
#define JSONARRAYOID 199
#define PGNODETREEOID 194
#define PGNDISTINCTOID 3361
#define PGDEPENDENCIESOID 3402
#define PGDDLCOMMANDOID 32
#define SMGROID 210
#define POINTOID 600
#define LSEGOID 601
#define PATHOID 602
#define BOXOID 603
#define POLYGONOID 604
#define LINEOID 628
#define LINEARRAYOID 629
#define FLOAT4OID 700
#define FLOAT8OID 701
#define ABSTIMEOID 702
#define RELTIMEOID 703
#define TINTERVALOID 704
#define UNKNOWNOID 705
#define CIRCLEOID 718
#define CIRCLEARRAYOID 719
#define CASHOID 790
#define MONEYARRAYOID 791
#define MACADDROID 829
#define INETOID 869
#define CIDROID 650
#define MACADDR8OID 774
#define BOOLARRAYOID 1000
#define BYTEAARRAYOID 1001
#define CHARARRAYOID 1002
#define NAMEARRAYOID 1003
#define INT2ARRAYOID 1005
#define INT2VECTORARRAYOID 1006
#define INT4ARRAYOID 1007
#define REGPROCARRAYOID 1008
#define TEXTARRAYOID 1009
#define OIDARRAYOID 1028
#define TIDARRAYOID 1010
#define XIDARRAYOID 1011
#define CIDARRAYOID 1012
#define OIDVECTORARRAYOID 1013
#define BPCHARARRAYOID 1014
#define VARCHARARRAYOID 1015
#define INT8ARRAYOID 1016
#define POINTARRAYOID 1017
#define LSEGARRAYOID 1018
#define PATHARRAYOID 1019
#define BOXARRAYOID 1020
#define FLOAT4ARRAYOID 1021
#define FLOAT8ARRAYOID 1022
#define ABSTIMEARRAYOID 1023
#define RELTIMEARRAYOID 1024
#define TINTERVALARRAYOID 1025
#define POLYGONARRAYOID 1027
#define ACLITEMOID 1033
#define ACLITEMARRAYOID 1034
#define MACADDRARRAYOID 1040
#define MACADDR8ARRAYOID 775
#define INETARRAYOID 1041
#define CIDRARRAYOID 651
#define CSTRINGARRAYOID 1263
#define BPCHAROID 1042
#define VARCHAROID 1043
#define DATEOID 1082
#define TIMEOID 1083
#define TIMESTAMPOID 1114
#define TIMESTAMPARRAYOID 1115
#define DATEARRAYOID 1182
#define TIMEARRAYOID 1183
#define TIMESTAMPTZOID 1184
#define TIMESTAMPTZARRAYOID 1185
#define INTERVALOID 1186
#define INTERVALARRAYOID 1187
#define NUMERICARRAYOID 1231
#define TIMETZOID 1266
#define TIMETZARRAYOID 1270
#define BITOID 1560
#define BITARRAYOID 1561
#define VARBITOID 1562
#define VARBITARRAYOID 1563
#define NUMERICOID 1700
#define REFCURSOROID 1790
#define REFCURSORARRAYOID 2201
#define REGPROCEDUREOID 2202
#define REGOPEROID 2203
#define REGOPERATOROID 2204
#define REGCLASSOID 2205
#define REGTYPEOID 2206
#define REGROLEOID 4096
#define REGNAMESPACEOID 4089
#define REGPROCEDUREARRAYOID 2207
#define REGOPERARRAYOID 2208
#define REGOPERATORARRAYOID 2209
#define REGCLASSARRAYOID 2210
#define REGTYPEARRAYOID 2211
#define REGROLEARRAYOID 4097
#define REGNAMESPACEARRAYOID 4090
#define UUIDOID 2950
#define UUIDARRAYOID 2951
#define LSNOID 3220
#define PG_LSNARRAYOID 3221
#define TSVECTOROID 3614
#define GTSVECTOROID 3642
#define TSQUERYOID 3615
#define REGCONFIGOID 3734
#define REGDICTIONARYOID 3769
#define TSVECTORARRAYOID 3643
#define GTSVECTORARRAYOID 3644
#define TSQUERYARRAYOID 3645
#define REGCONFIGARRAYOID 3735
#define REGDICTIONARYARRAYOID 3770
#define JSONBOID 3802
#define JSONBARRAYOID 3807
#define TXID_SNAPSHOTOID 2970
#define TXID_SNAPSHOTARRAYOID 2949
#define INT4RANGEOID 3904
#define INT4RANGEARRAYOID 3905
#define NUMRANGEOID 3906
#define NUMRANGEARRAYOID 3907
#define TSRANGEOID 3908
#define TSRANGEARRAYOID 3909
#define TSTZRANGEOID 3910
#define TSTZRANGEARRAYOID 3911
#define DATERANGEOID 3912
#define DATERANGEARRAYOID 3913
#define INT8RANGEOID 3926
#define INT8RANGEARRAYOID 3927
#define RECORDOID 2249
#define RECORDARRAYOID 2287
#define CSTRINGOID 2275
#define ANYOID 2276
#define ANYARRAYOID 2277
#define VOIDOID 2278
#define TRIGGEROID 2279
#define EVTTRIGGEROID 3838
#define LANGUAGE_HANDLEROID 2280
#define INTERNALOID 2281
#define OPAQUEOID 2282
#define ANYELEMENTOID 2283
#define ANYNONARRAYOID 2776
#define ANYENUMOID 3500
#define FDW_HANDLEROID 3115
#define INDEX_AM_HANDLEROID 325
#define TSM_HANDLEROID 3310
#define ANYRANGEOID 3831

"
%
category: 'Error Handling'
classmethod: GsPostgresResult
raiseErrorWithMessage: msg

"Raises an exception with message msg"

	^GsPostgresConnection errorClass signal: msg
%
category: 'Converting'
classmethod: GsPostgresResult
symbolForResultStatus: aStatus

"Convert a Postgres result status to a Symbol describing said status.
Raises an error if aStatus is not a valid Postgres status."

^ ExecStatusTable at: aStatus ifAbsent:[ GsPostgresConnection errorClass signal: ('Unknown result status ' , aStatus asString) ]
%
category: 'Converting'
classmethod: GsPostgresResult
timeFromPostgresTimeString: aString

"Convert a Postgres Time string into an instance of Time (or SmallTime if supported)

Note: Postgres times with timezones are not supported and the timezone is discarded."

| rs  secs micro |
rs := ReadStreamPortable on: aString.
secs :=(rs upTo: $: ) asInteger * 3600. "hours"
secs := secs + ((rs upTo: $: ) asInteger * 60). "minutes"
"Stop at $+ or $- which is the start of the time zone offset, if any. Otherwise read to the end to get the second"
micro := (((rs upToAny: #( $+ $-))  asFloat) * 1000000) asInteger . "seconds with micro"
^Time fromMicroseconds: ((secs * 1000000) + micro)
%
! ------------------- Instance methods for GsPostgresResult
category: 'Acessing Tuples'
method: GsPostgresResult
allTuples

"Return an OrderedCollection of all rows.
Warning: this method can cause out of memory errors if the receiver has many rows."

| result rows |
rows := self numRows .
result := OrderedCollection new: rows.
1 to: rows do:[:n| result at: n put: (self tupleAtRow: n) ].
^ result
%
category: 'Accessing'
method: GsPostgresResult
columnMapEntries
	^columnMapEntries
%
category: 'Updating'
method: GsPostgresResult
columnMapEntries: newValue
	columnMapEntries := newValue
%
category: 'Accessing Fields'
method: GsPostgresResult
columnNameAt: anInt

"Answer a String which is the name of the column at index anInt. The first column has an index of 1."
	^ self libpq PQfname: self pqResultCptr fieldNum: (anInt - 1)
%
category: 'Accessing'
method: GsPostgresResult
columnNameMap
	^columnNameMap
%
category: 'Updating'
method: GsPostgresResult
columnNameMap: newValue
	columnNameMap := newValue
%
category: 'Accessing Fields'
method: GsPostgresResult
columnNumberForName: cName

"Answer a SmallInteger which is the index of the column with name cName The first column has an index of 1."
^ self columnNameMap at: cName asSymbol otherwise: -1

%
category: 'Accessing'
method: GsPostgresResult
columnTypes
	^columnTypes
%
category: 'Updating'
method: GsPostgresResult
columnTypes: newValue
	columnTypes := newValue
%
category: 'Queries'
method: GsPostgresResult
commandStatus

"Returns a String which is the command status tag from the SQL command that generated the PGresult.
Commonly this is just the name of the command, but it might include additional data such as the number of rows processed."

^ self libpq PQcmdStatus: self pqResultCptr .
%
category: 'Accessing'
method: GsPostgresResult
defaultStringClass

	^ self unicodeStrings ifTrue:[ Unicode7 ] ifFalse:[ String ]
%
category: 'Type Mapping'
method: GsPostgresResult
defaultTypeForColumn: anInt

"Answer a Class which represents the default class used to represent objects created from column number anInt. The first column number is 1."

^self columnTypes at: anInt
%
category: 'Private'
method: GsPostgresResult
fetchDataInto: tupleInst fromRow: rowInt usingColumnMap: colMapEntry

"Private. Do not call directly unless you know what you're doing."

	tupleInst perform: colMapEntry setMethodSelector
		with: (self objectAtRow: rowInt columnName: colMapEntry columnName preferredClass: colMapEntry instVarClass)
%
category: 'Freeing'
method: GsPostgresResult
free

"Calls #PGclear on the pqResultCptr pointer. This releases the memory used by pqResultCptr and renders it invalid."

self libpq PQclear: self pqResultCptr .
self libpq: nil ; pqResultCptr: nil ; numRows: 0
%
category: 'Initialize'
method: GsPostgresResult
initialize

"Initializes and returns the receiver"

	self unicodeStrings ifNil:[ self unicodeStrings: false ].
	^self
		setColumnMapEntries ;
		setNumberOfRows;
		setNumberOfColumns;
		setPgColumnTypes;
		setColumnTypes;
		setColumnNameMap
%
category: 'Accessing'
method: GsPostgresResult
libpq
	^libpq
%
category: 'Updating'
method: GsPostgresResult
libpq: newValue
	libpq := newValue
%
category: 'Accessing Fields'
method: GsPostgresResult
numberOfFields

"Returns the number of fields (columns) in the receiver"

	^ self libpq PQnfields: self pqResultCptr
%
category: 'Accessing'
method: GsPostgresResult
numColumns
	^numColumns
%
category: 'Updating'
method: GsPostgresResult
numColumns: newValue
	numColumns := newValue
%
category: 'Accessing'
method: GsPostgresResult
numRows
	^numRows
%
category: 'Updating'
method: GsPostgresResult
numRows: newValue
	numRows := newValue
%
category: 'Acessing Tuples'
method: GsPostgresResult
objectAtRow: rowInt column: columnInt

"Answer the object at row rowInt and column columnInt. The first row and column both have an index of 1.
The default mapping of Postgres type (OID) to GemStone class is used."

^ self objectAtRow: rowInt column: columnInt preferredClass: nil
%
category: 'Acessing Tuples'
method: GsPostgresResult
objectAtRow: rowInt column: columnInt preferredClass: aClassOrNil

"Answer the object at row rowInt and column columnInt. The first row and column both have an index of 1.
If aClassOrNil is not nil, answer an instance of aClassOrNil, which is expected to be a Class. Otherwise
the default class mapping of Postgres type (OID) to GemStone class is used to create the result."


	| obj cls |
	(obj := self primTupleAtRow: rowInt column: columnInt) size == 0
		ifTrue: [^''].	"Early exit for empty string case"
	cls := aClassOrNil ifNil: [self defaultTypeForColumn: columnInt].

	"If there's a class to map to, create a new object of that class. Otherwise return the one we have."
	^ self class createInstanceOf: cls fromPostgresString: obj

%
category: 'Acessing Tuples'
method: GsPostgresResult
objectAtRow: rowInt columnName: colName preferredClass: aClassOrNil

"Answer the object at row rowInt and column named colName. The first row has an index of 1.
If aClassOrNil is not nil, answer an instance of aClassOrNil, which is expected to be a Class. Otherwise
the default class mapping of Postgres type (OID) to GemStone class is used to create the result."

| colNum |
colNum := self columnNumberForName: colName .
^ colNum == -1
	ifTrue:[  self raiseErrorWithMessage: ('Invalid column name ', colName asString) ]
	ifFalse:[ self objectAtRow: rowInt column: colNum preferredClass: aClassOrNil ]
%
category: 'Overriding'
method: GsPostgresResult
overrideType: aClass forColumnNumber: anInt

"Forces objects created from column number anInt to be instances of aClass,
overriding the default type mapping.
aClass must be a kind of Class.
anInt must be a valid column number for the receiver.
Returns the reciever on success."

aClass _validateIsClass .
anInt _validateClass: SmallInteger .
(anInt < 1 or:[ anInt > self numColumns])
	ifTrue:[  ^ self _errorIndexOutOfRange: anInt ].

self columnTypes at: anInt put: aClass.
^ self
%
category: 'Accessing'
method: GsPostgresResult
pgColumnTypes
	^pgColumnTypes
%
category: 'Updating'
method: GsPostgresResult
pgColumnTypes: newValue
	pgColumnTypes := newValue
%
category: 'Private'
method: GsPostgresResult
postgresTypeForColumn: anInt

"Private. Do not call directly unless you know what you're doing.
Use cached Postgres column types. Must only be called after initialization."

^self pgColumnTypes at: anInt
%
category: 'Accessing'
method: GsPostgresResult
pqResultCptr
	^pqResultCptr
%
category: 'Updating'
method: GsPostgresResult
pqResultCptr: newValue
	pqResultCptr := newValue
%
category: 'Private'
method: GsPostgresResult
primBinaryTupleAtRow: rowInt column: columnInt

"Private. Do not call directly unless you know what you're doing.
Like #primTupleAtRow:column: except a CByteArray is retuned instead of a String"

	| cPointer length |
	cPointer := self libpq
				PQgetvalue_binary: pqResultCptr
				row: rowInt - 1
				column: columnInt - 1.
	length := cPointer isNull
				ifTrue: [0]
				ifFalse: [self primGetLengthForTupleAtRow: rowInt column: columnInt].
^CByteArray fromCPointer: cPointer numBytes: length
%
category: 'Private'
method: GsPostgresResult
primColumnNumberForName: cName

"Private. Do not call directly unless you know what you're doing.
Returns C-based (zero-based) column number for the given name by calling C"

^ self libpq PQfnumber: self pqResultCptr columnName: cName
%
category: 'Private'
method: GsPostgresResult
primGetLengthForTupleAtRow: rowInt column: columnInt

"Private. Do not call directly unless you know what you're doing.
Like #primTupleAtRow:column: except a CByteArray is retuned instead of a String"

^ self libpq PQgetlength: pqResultCptr row: (rowInt - 1) column: (columnInt - 1)
%
category: 'Private'
method: GsPostgresResult
primNumberOfColumns

"Private. Do not call directly unless you know what you're doing.
Call C to get the number of columns in the receiver"

	^ self libpq PQnfields: self pqResultCptr
%
category: 'Private'
method: GsPostgresResult
primNumberOfRows

"Private. Do not call directly unless you know what you're doing.
Returns the number of tuples (rows) in the receiver."

	^self libpq PQntuples: pqResultCptr
%
category: 'Private'
method: GsPostgresResult
primPostgresTypeForColumn: anInt

"Private. Do not call directly unless you know what you're doing.
Call PQftype to get the Postgres type for the column.
Convert column id from Smalltalk to C"

^self libpq PQftype: self pqResultCptr fieldNum: (anInt - 1)
%
category: 'Private'
method: GsPostgresResult
primRowsAffected

"Private. Do not call directly unless you know what you're doing."

| resultStr |
resultStr := self libpq PQcmdTuples: self pqResultCptr.
^ ( resultStr size == 0 )
	ifTrue:[ 0 ]
	ifFalse:[ resultStr asInteger]
%
category: 'Private'
method: GsPostgresResult
primTupleAtRow: rowInt column: columnInt

"Private. Do not call directly unless you know what you're doing.
Convert from 1-based Smalltalk offset to 0-based C offset."

^ self libpq PQgetvalue: pqResultCptr row: (rowInt - 1) column: (columnInt - 1)
%
category: 'Private'
method: GsPostgresResult
primTypeForColumn: anInt

"Private. Do not call directly unless you know what you're doing."

^(self class defaultClassForOid: (self postgresTypeForColumn: anInt)) ifNil:[ self defaultStringClass ]
%
category: 'Error Handling'
method: GsPostgresResult
raiseErrorWithMessage: msg

"Raises an exception with message msg"

^ self class raiseErrorWithMessage: msg
%
category: 'Queries'
method: GsPostgresResult
resultStatus

"Returns a SmallInteger representing the Postgres status of the execution used to generate the receiver."

^ self libpq PQresultStatus: pqResultCptr
%
category: 'Queries'
method: GsPostgresResult
resultStatusSymbol

"Returns a Symbol representing the Postgres status of the execution used to generate the receiver."

^ self class symbolForResultStatus: self resultStatus
%
category: 'Initialize (Private)'
method: GsPostgresResult
setColumnMapEntries

"Private. Do not call directly unless you know what you're doing."

	self tupleClass
		ifNotNil: [self columnMapEntries: self tupleClass rdbColumnMapping].
	^self
%
category: 'Initialize (Private)'
method: GsPostgresResult
setColumnNameMap

"Private. Do not call directly unless you know what you're doing."

columnNameMap := SymbolKeyValueDictionary new.
1 to: self numColumns do:[:n| columnNameMap at: (self columnNameAt: n) asSymbol put: n ].
^ self
%
category: 'Initialize (Private)'
method: GsPostgresResult
setColumnTypes

"Private. Do not call directly unless you know what you're doing.
Set columnTypes to be an OrderedCollection of classes representing the Class for each column.
Invoke setPgColumnTypes before this method."

 | sz |
sz := self pgColumnTypes size.
columnTypes := OrderedCollection new: sz .
1 to:  sz do:[:n|  |pgType|
	pgType := pgColumnTypes at: n.
	columnTypes at: n put: (self primTypeForColumn: n).
].

^ self
%
category: 'Initialize (Private)'
method: GsPostgresResult
setNumberOfColumns

"Private. Do not call directly unless you know what you're doing."

self numColumns: self primNumberOfColumns.
^ self
%
category: 'Initialize (Private)'
method: GsPostgresResult
setNumberOfRows

"Private. Do not call directly unless you know what you're doing."

self numRows: self primNumberOfRows .
^ self
%
category: 'Initialize (Private)'
method: GsPostgresResult
setPgColumnTypes


"Private. Do not call directly unless you know what you're doing.
Set pgColumnTypes to be an OrderedCollection of OIDs (integers) representing the Postgres OID for each column"

pgColumnTypes := OrderedCollection new: self numColumns.
1 to: self numColumns do:[:n| pgColumnTypes at: n put: (self primPostgresTypeForColumn: n) ].
^ self
%
category: 'Accessing'
method: GsPostgresResult
sql
	^sql
%
category: 'Updating'
method: GsPostgresResult
sql: newValue
	sql := newValue
%
category: 'Testing'
method: GsPostgresResult
statusIsCommandOk

"Answer true if the Postgres command status is PGRES_COMMAND_OK, false otherwise."

^self resultStatusSymbol == #PGRES_COMMAND_OK
%
category: 'Testing'
method: GsPostgresResult
statusIsOk

"Answer true if the either Postgres result status or command status indicate success, false otherwise."

^ self statusIsCommandOk or:[ self statusIsTuplesOk]
%
category: 'Testing'
method: GsPostgresResult
statusIsTuplesOk

"Answer true if the Postgres result status is PGRES_TUPLES_OK, false otherwise."

^self resultStatusSymbol == #PGRES_TUPLES_OK
%
category: 'Acessing Tuples'
method: GsPostgresResult
tupleAtRow: rowInt

"Answer the tuple for the given row number. The first row has an index of 1.
Result will be an instance of tupleClass, or an instance of OrderedCollection is no tupleClass is specified."

	| result |
	result := tupleClass isNil
				ifTrue: [OrderedCollection new: self numColumns]
				ifFalse: [tupleClass new].
	^self tupleAtRow: rowInt using: result
%
category: 'Acessing Tuples'
method: GsPostgresResult
tupleAtRow: rowInt using: tupleInst

"Store that date from row rowInt into the objet tupleInst. The first row has an index of 1.
If the receiver's tupleClass is nil, tupleInst is assumed to be an OrderedCollection.
If the receiver's tupleClass is not nil, tupleInst is assumed to be an instance of tupleClass. In this case
the selectors in the receiver's columnMapEntries objects are used to store the results into tupleInst.
Returns tupleInst"

	| result |
	self tupleClass
		ifNil:
			[1 to: self numColumns
				do: [:n | tupleInst at: n put: (self objectAtRow: rowInt column: n)]]
		ifNotNil:
			[self columnMapEntries do:
					[:colMapEntry |
					self fetchDataInto: tupleInst fromRow: rowInt usingColumnMap: colMapEntry].
			tupleInst rdbPostLoad].
	^tupleInst
%
category: 'Accessing'
method: GsPostgresResult
tupleClass
	^tupleClass
%
category: 'Updating'
method: GsPostgresResult
tupleClass: newValue
	tupleClass := newValue
%
category: 'Accessing'
method: GsPostgresResult
unicodeStrings
	^unicodeStrings
%
category: 'Updating'
method: GsPostgresResult
unicodeStrings: newValue
	unicodeStrings := newValue
%
