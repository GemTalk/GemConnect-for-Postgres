fileformat utf8
set compile_env: 0
! ------------------- Class definition for GsPostgresWriteStream
expectvalue /Class
doit
Object subclass: 'GsPostgresWriteStream'
  instVarNames: #( conn libpq tupleClass
                    sql preparedStatementName numTuplesFlushed collection
                    columnMap keyMap position)
  classVars: #( ClassToPostgresStringTable)
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: (UserGlobals at: #GcfpInstallDict)
  options: #()

%
expectvalue /Class
doit
GsPostgresWriteStream comment: 
'GsPostgresWriteStream is a stream classed used to send updates to Postgres.
Do not create GsPostgresWriteStream directly. Rather use one of the methods in
GsPostgresConnection such as #openDeleteCursorOn:,  #openInsertCursorOn: or #openUpdateCursorOn:
which return a GsPostgresWriteStream.

GsPostgresWriteStream object are write-only. Reading from them is not allowed.

Writes to the stream are buffered and Postgres is not updated until the stream is flushed.
Use the #flush or #flushInOneTransaction method to flush the writes to Postgres.

Instance Variables:
	conn (GsPostgresConnection) - reference to the connection for this stream.
	libpq (Libqp) - reference to the Postgres library object for this stream.
	tupleClass (Class) - objects added to the stream are expected to be members of this class.
	sql (String) - The SQL statement used to perform the write operation.
	preparedStatementName (String) - name of the prepared statement in Postgres. Randomly generated as a UUID.
	numTuplesFlushed (SmallInteger) - number of tuples flushed to Postgres.
	collection (Array) - The collection of objects in the stream.
	columnMap (Array) - An array of GsPostgresColumnMapEntry objects for the tuple class.
	keyMap (Array) - An array of GsPostgresColumnMapEntry objects for the tuple class specifying the keys in the target table.
	position (SmallInteger) - total elements added to the stream. The first position is 0.
Class Variables
	ClassToPostgresStringTable (IdentityKeyValueDictionary) - table used to define methods for converting GemStone objects to
		their Postgres string representations.
		keys: Class
		values: anArray
			anArray at: 1 - (Class) receiver of the #perform selector in element 2. Usually GsPostgresWriteStream.
			anArray at: 2 - (Symbol) 2 argument selector used to convert the GemStone class to a Postgres string.

In some cases Postgres requires strings to be escaped and in other cases requires string not be escaped.
Literals imbedded in SQL command must generally be escape while arguments to execute a prepared statement
must not be escaped.


'
%
set compile_env: 0
! ------------------- Class definition for GsLibpq
expectvalue /Class
doit
Object subclass: 'GsLibpq'
  instVarNames: #( calloutTable cLibrary)
  classVars: #( libraryPath)
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: (UserGlobals at: #GcfpInstallDict)
  options: #()

%
expectvalue /Class
doit
GsLibpq comment: 
'GsLibpq is an object used to interface with the Postgres client shared library, libpq, vai the GemStone FFI libray interface.
Most methods in this class are private and should not be called directly. Use GsPostgresConnection to make FFI calls to libpq.

Instance Variables:
	calloutTable (SymbolKeyValueDictionary) - the table which holds Postgres callouts.
		keys: (Symbol) Postgres Function name
		values: (CCallout) - a CCallout for the function.
	cLibrary (CLibrary) - an instance of CLibrary for the Postgres client library.

Class Variables
	libraryPath (String) - Full path and file name of the libpq Postgres client library on this host.

'
%
set compile_env: 0
! ------------------- Class definition for GsPostgresConnection
expectvalue /Class
doit
(UserGlobals at: #GcfpPgConnSuperClass) subclass: 'GsPostgresConnection'
  instVarNames: #( libpq pqConnCptr pgParameters
                    unicodeStrings inTransaction)
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: (UserGlobals at: #GcfpInstallDict)
  options: #()

%
expectvalue /Class
doit
GsPostgresConnection comment: 
'GsPostgresConnection represents a connection to Postgres.
If GsRdbConnection is present, GsPostgresConnection will be a subclass of it.
If GsRdbConnection is not present, GsPostgresConnection will be a subclass Object.
GsRdbConnection is a class installed by GemConnect for Oracle.
Instance Variables:
	libpq (GsLibpq) - an instance of GsLibpq. Contains information about the Postgres shared library connect.
	pqConnCptr (CPointer) - Pointer to the C state of the Postgres connection (PGconn *)
	pgParameters (GsPostgresConnectionParameters) - parameters object for this connection
	unicodeStrings - (Boolean) specifies the default classes used to translate multibyte strings from Postgres into objects.
		true means return Unicode classses (Unicode16, Unicode32).
		false means return DoubleByteString and QuadByteString objects.
		Setting can be overridden by setting inst var class in GsPostgresColumnMapEntry to be the desired class.
	inTransaction (Boolean) - true means the session is in a transaction, false means it is not.
'
%
set compile_env: 0
! ------------------- Class definition for GsPostgresConnectionParameters
expectvalue /Class
doit
Object subclass: 'GsPostgresConnectionParameters'
  instVarNames: #( parameterDictionary)
  classVars: #( AllLegalParameters)
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: (UserGlobals at: #GcfpInstallDict)
  options: #()

%
expectvalue /Class
doit
GsPostgresConnectionParameters comment: 
'GsPostgresConnectionParameters holds a list of login parameters for a GsPostgresConnection.

Instance Varaibles:
	parameterDictionary (SymbolKeyValueDictioary) - a dictionary of connection parameter keys and values.
		The key is the parameter name and the value is the value of that parameter, usually a String but sometimes an Array.
		Certain  parameters such as #host consist of a collection of values.
		See the Postgres documentation for a list of legal connection parameters.

Class Variables:
	AllLegalParameters (IdentitySet) - Collection of symbols which are legal Postgres login parameters.
'
%
set compile_env: 0
! ------------------- Class definition for GsPostgresReadStream
expectvalue /Class
doit
Object subclass: 'GsPostgresReadStream'
  instVarNames: #( conn position queryResult
                    readLimit)
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: (UserGlobals at: #GcfpInstallDict)
  options: #()

%
expectvalue /Class
doit
GsPostgresReadStream comment: 
'GsPostgresReadStream is a stream class used to present the results of Postgres SQL queries.
GsPostgresReadStream objects are created by the GsPostgresConnection>>execute: method and
are not intended to be created directly by the application.
GsPostgresReadStreams are read only objects and writing to them is not allowed.
References to memory in C is maintained by these objects.
To release this memory, send the #free message when the object is no longer needed.

Instance Variables:
	position (SmallInteger) - position in the stream. The first position is 0 per ANSI standards.
	queryResult (GsPostgresResult) - object that contains C state and information about the query results.
	readLimit (SmallInteger) - number of elements in the stream.

'
%
set compile_env: 0
! ------------------- Class definition for GsPostgresColumnMapEntry
expectvalue /Class
doit
Array subclass: 'GsPostgresColumnMapEntry'
  instVarNames: #( instVarClass)
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: (UserGlobals at: #GcfpInstallDict)
  options: #()

%
expectvalue /Class
doit
GsPostgresColumnMapEntry comment: 
'GsPostgresColumnMapEntry is a classed used specify mappings between Postgres columns and instance variables of tuple objects.
To be compatible with GemConnect for Oracle, it is a subclass of array so its instance variables can be a accessed either by
getter/setter methods or by index using at: and at:put:, which is what is expected by older versions of GemConnect.

Instance Variables:
	columnName (String) - name of the Postgres column to map to.
	instVarName (String) - name of the instance variable in the tuple class
	getMethodSelector (Symbol) - zero argument method selector used to return instVarName in the tuple object.
	setMethodSelector (Symbol) - one argument method selector used to update instVarName in the tuple object.
	instVarClass (Class) - preferred GemStone class of the inst var. nil means used the system default mapping based on the Postgres column type.


	'
%
set compile_env: 0
! ------------------- Class definition for GsPostgresResult
expectvalue /Class
doit
Object subclass: 'GsPostgresResult'
  instVarNames: #( libpq pqResultCptr numRows
                    numColumns pgColumnTypes columnTypes columnNameMap
                    tupleClass sql columnMapEntries unicodeStrings)
  classVars: #( ClassAndSelctorTable ExecStatusTable FieldTypeTable)
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: (UserGlobals at: #GcfpInstallDict)
  options: #( dbTransient)

%
expectvalue /Class
doit
GsPostgresResult comment: 
'GsPostgresResult contains information about the result of a statement executed in Postgres such as a query.
These objects must be freed with the #free message in order to release memory in C.
Normally a GsPostgresResult object is referenced by a GsPostgresReadStream object, and freeing the read stream
automatically frees the result object.

Instance Variables:
	libpq (GsLibpq) - an instance of GsLibpq. Contains information about the Postgres shared library.
	pqResultCPointer (CPointer) - a reference to a PGresult * object in C.
	numRows (SmallInteger) - number of rows the query produced.
	numColumns (SmallInteger) - number of columns in the query result.
	pgColumnTypes (OrderedCollection)  - a list of SmallIntegers which represent the OIDs of the Postgres types for the column in the result.
	columnTypes (OrderedCollection)  - a list of classes which represent the GemStone type of each column in the result.
	columnNameMap (SymbolKeyValueDictionary) - keys: columnName (Symbol), values: columnId (SmallInteger)
	tupleClass (Class) - class mapped to each tuple (row) of the query result. nil means each row will be returned as an OrderedCollection.
	sql (String) - the SQL statement executed in Postgres which generated the receiver.
	columnMapEntries (Array) - if tupleClass is not nil, a list of GsPostgresColumnMapEntry objects. Otherwise nil.
	unicodeStrings (Boolean) - true means multibyte strings are returned as Unicode16 and Unicode32 objects by default.
		false means multibyte strings are returned as DoubleByteString and QuadByteString objects by default.

Class Variables:
	ClassAndSelctorTable (IdentityKeyValueDictionary) - keys: type (Class). values: (Array) arguments used to #perform: the conversion from a
		Postgres string to the type as follows:
			array at: 1 (Class) - receiver of the #perform:with: message, normally GsPostgresResult (class)
			array at: 2 (Symbol) - one argument method selector used to perform the conversion.
	ExecStatusTable (IntegerKeyValueDictionary) - keys: Postgres status code (SmallInteger), values: Postgres status (Symbol). This table is used
		to translate Postgres status codes to a Symbol.
	FieldTypeTable (IntegerKeyValueDictionary) - keys: Postgres OID (SmallInterger), values: GemStone class (Class). This table defines the default
		mapping of Postgres types to GemStone classes.


'
%
