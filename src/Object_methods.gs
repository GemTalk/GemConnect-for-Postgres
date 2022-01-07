! ========================================================================
! Copyright (C) GemTalk Systems. 1986-2020.  All Rights Reserved
!
! Name - Object.gs
!
! ========================================================================

!
! Instance Category 'Rdb Methods'
!
category: 'Rdb Methods'
method: Object
rdbPostLoad

"This message should be re-implemented by developers to provide custom behavior
 for their application classes.  The default behavior is to simply return self.

 This message is sent by GemStone to each tuple object after it is loaded with
 information from the relational database. It is not intended to be called by
 methods implemented by application developers.

 Typical re-implementations will include actions such as determining whether the
 object should be tracked for changes, time stamping the object for the purpose
 of tracking coherency between data read into GemStone and the data in the
 relational database, or storing a copy of the object for use in generating
 updates.

 Care should be taken to have re-implementations return self.

ARGUMENTS:

   none

RETURN VALUE:

   unspecified

ERRORS:

   none
"
%
!
! Class Category 'Rdb Methods'
!
category: 'Rdb Methods'
classmethod: Object
rdbColumnMapping

"This message should be re-implemented by developers to provide custom behavior
 for their application classes.  The default behavior is to return nil.

 This message returns the default column mapping for all tuple objects.  This
 column mapping message is sent by GemStone when a GsRdbReadStream is created
 and when generating SQL commands. If this message has not been re-implemented
 to provide a column mapping for the tuple class, no column mapping will be used
 by the read stream when generating tuples from the relational data.

 Re-implementations should take care to return a properly formatted column
 mapping.

ARGUMENTS:

   none

RETURN VALUE:

   <UndefinedObject>

ERRORS:

   none
"

   ^nil
%
classmethod: Object
rdbPrimaryKeyMaps

"This message should be re-implemented by developers to provide custom behavior
 for their application classes.  The default behavior is to return nil.

 This message returns the default primary key maps for all tuple objects.  This
 primary key maps message is sent by GemStone when generating SQL commands.

 Re-implementations should take care to return a properly formatted column
 mapping.

ARGUMENTS:

   none

RETURN VALUE:

   <UndefinedObject>

ERRORS:

   none
"

   ^nil
%
classmethod: Object
rdbTableName

"This message should be re-implemented by developers to provide custom behavior
 for their application classes.  The default behavior is to return nil.

 This message returns the default table name for all tuple objects.  This table
 name message is sent by GemStone when generating SQL commands.

 Re-implementations should take care to return a string corresponding to a real
 relational database table.

ARGUMENTS:

   none

RETURN VALUE:

   <UndefinedObject>

ERRORS:

   none
"

   ^nil
%
