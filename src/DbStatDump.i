/*------------------------------------------------------------------------
    File        : DbStatDump.i
    Purpose     :

    Syntax      :

    Description : Define the shared temp-tables to store the most recent stats

    Author(s)   : George Potemkin
    Created     : Jun 05, 2010
    Modified    : Jul 01, 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&GLOBAL-DEFINE Sep "~t"

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) GT 10.2
OR (DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) EQ 10.2
    AND SUBSTRING(PROVERSION, INDEX(PROVERSION,".") + 2, 1) GE "B":U)
&THEN
&GLOBAL-DEFINE Version_GE_102B TRUE
&ENDIF
&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) GT 10.1
OR (DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) EQ 10.1
    AND SUBSTRING(PROVERSION, INDEX(PROVERSION,".") + 2, 1) GE "B":U)
&THEN
&GLOBAL-DEFINE Version_GE_101B TRUE
&ENDIF

DEFINE {1} SHARED TEMP-TABLE ttDbStat NO-UNDO
  FIELD DbHost      AS CHARACTER
  FIELD DbPath      LIKE _FileList._FileList-Name
  FIELD DbStartTime LIKE _MstrBlk._MstrBlk-oprdate
  FIELD DbTimeStamp LIKE _MstrBlk._MstrBlk-TimeStamp INITIAL ?

  FIELD SnapshotTimeStamp AS CHARACTER

  FIELD DbAccesses  LIKE _ActSummary._Summary-DbAccesses

  FIELD RecRead     LIKE _ActRecord._Record-RecRead
  FIELD RecUpdate   LIKE _ActRecord._Record-RecUpd
  FIELD RecCreate   LIKE _ActRecord._Record-RecCreat
  FIELD RecDelete   LIKE _ActRecord._Record-RecDel

  FIELD BytesRead   LIKE _ActRecord._Record-BytesRead
  FIELD BytesUpdate LIKE _ActRecord._Record-BytesUpd
  FIELD BytesCreate LIKE _ActRecord._Record-BytesCreat
  FIELD BytesDelete LIKE _ActRecord._Record-BytesDel

  FIELD IdxRead     LIKE _ActIndex._Index-Find
  FIELD IdxCreate   LIKE _ActIndex._Index-Create
  FIELD IdxDelete   LIKE _ActIndex._Index-Delete

  FIELD DbReads     LIKE _ActSummary._Summary-DbReads
  FIELD DbWrites    LIKE _ActSummary._Summary-DbWrites
  FIELD DatDbReads  LIKE _ActIOType._IOType-DataReads
  FIELD DatDbWrites LIKE _ActIOType._IOType-DataWrts
  FIELD IdxDbReads  LIKE _ActIOType._IOType-IdxRds
  FIELD IdxDbWrites LIKE _ActIOType._IOType-IdxWrts

  FIELD BiWrites    LIKE _ActBILog._BiLog-TotalWrts
  FIELD BiBytesWrtn LIKE _ActBILog._BiLog-BytesWrtn
  FIELD BiNotesWrtn LIKE _ActBILog._BiLog-RecWriten
  FIELD TransComm   LIKE _ActSummary._Summary-TransComm

/*  _Latch._Latch-lock & _Latch._Latch-wait are 32-bit integer 
but _Resrc._Resrc-lock & _Resrc._Resrc-wait are 64-bit integer.
Sum of _Latch-lock can easily overflow 2 billion limit.
I don't want to declare these fields as int64 
because the program will not with the earlier Progress versions. */
  FIELD LatchLock   LIKE _Resrc._Resrc-lock /* _Latch._Latch-lock */
  FIELD LatchWait   LIKE _Resrc._Resrc-wait /* _Latch._Latch-wait */
  FIELD ResrcLock   LIKE _Resrc._Resrc-lock
  FIELD ResrcWait   LIKE _Resrc._Resrc-wait
  FIELD SemWaits    LIKE _ActOther._Other-Wait
  FIELD TxeLock     LIKE _TxeLock._Txe-Locks EXTENT 0
  FIELD TxeWait     LIKE _TxeLock._Txe-Waits EXTENT 0
  FIELD Checkpoints LIKE _ActSummary._Summary-Chkpts
  FIELD Flushed     LIKE _ActSummary._Summary-Flushed
  FIELD MostLocks   LIKE _DbStatus._DbStatus-MostLocks
  FIELD B1LogicRds  LIKE _ActBuffer._Buffer-LogicRds
  FIELD B1LogicWrts LIKE _ActBuffer._Buffer-LogicWrts
  FIELD B1OSRds     LIKE _ActBuffer._Buffer-OSRds
  FIELD B1OSWrts    LIKE _ActBuffer._Buffer-OSWrts
  FIELD B2LogicRds  LIKE _ActBuffer._Buffer-LogicRds
  FIELD B2LogicWrts LIKE _ActBuffer._Buffer-LogicWrts
  FIELD B2OSRds     LIKE _ActBuffer._Buffer-OSRds
  FIELD B2OSWrts    LIKE _ActBuffer._Buffer-OSWrts
  FIELD B2LRUEnbld  LIKE _ActBuffer._Buffer-LRUEnabled
  FIELD AIWWrites   LIKE _ActAILog._AILog-AIWWrites
  FIELD AIBusyBufW  LIKE _ActAILog._AILog-BBuffWaits
  FIELD AINoBufAvail LIKE _ActAILog._AILog-NoBufAvail
  FIELD AIPrtlWrts  LIKE _ActAILog._AILog-PartialWrt
  FIELD AITotWrts   LIKE _ActAILog._AILog-TotWrites
  FIELD BIWWrites   LIKE _ActBILog._BILog-BIWWrites
  FIELD TblIdxSkip  AS INT64
  FIELD AIBytesWrt  LIKE _ActAILog._AILog-BytesWritn
  INDEX DbStatIdx IS UNIQUE
        DbHost
        DbPath
. /* TEMP-TABLE ttDbStat */


DEFINE {1} SHARED TEMP-TABLE ttAreaStat NO-UNDO
  FIELD DbHost      AS CHARACTER
  FIELD DbPath      LIKE _FileList._FileList-Name
  FIELD AreaNumber  LIKE _Area._Area-number
  FIELD AreaName    LIKE _Area._Area-Name
  FIELD AreaHWM     LIKE _AreaStatus._AreaStatus-Hiwater
  FIELD AreaReads   LIKE _ActIOFile._IOFile-Reads
  FIELD AreaWrites  LIKE _ActIOFile._IOFile-Writes
  FIELD TableRead   LIKE _TableStat._TableStat-Read
  FIELD TableUpdate LIKE _TableStat._TableStat-Update
  FIELD TableCreate LIKE _TableStat._TableStat-Create
  FIELD TableDelete LIKE _TableStat._TableStat-Delete
  FIELD IndexRead   LIKE _IndexStat._IndexStat-Read
  FIELD IndexCreate LIKE _IndexStat._IndexStat-Create
  FIELD IndexDelete LIKE _IndexStat._IndexStat-Delete

  INDEX AreaStatIdx IS UNIQUE
        DbHost
        DbPath
        AreaNumber
. /* TEMP-TABLE ttAreaStat */


DEFINE {1} SHARED TEMP-TABLE ttTableStat NO-UNDO
  FIELD DbHost      AS CHARACTER
  FIELD DbPath      LIKE _FileList._FileList-Name
  FIELD TableNumber LIKE _File._File-Number
  FIELD TableName   LIKE _File._File-Name /* The field is not used */
  FIELD TableRead   LIKE _TableStat._TableStat-Read
  FIELD TableUpdate LIKE _TableStat._TableStat-Update
  FIELD TableCreate LIKE _TableStat._TableStat-Create
  FIELD TableDelete LIKE _TableStat._TableStat-Delete
&IF DEFINED(Version_GE_102B)
&THEN
  FIELD TableOsRead LIKE _TableStat._TableStat-OsRead
&ENDIF

  INDEX TableStatIdx IS UNIQUE
        DbHost
        DbPath
        TableNumber
. /* TEMP-TABLE ttTableStat */

/*
ttTableStat.TableName, ttIndexStat.TableName and ttIndexStat.IndexName
These fields are not used to create the reports.
But data in these fields can be usefull if someone is going to convert data
from DbStatDump.LastSnapshot.d to the stats report since db startup.
*/

DEFINE {1} SHARED TEMP-TABLE ttIndexStat NO-UNDO
  FIELD DbHost      AS CHARACTER
  FIELD DbPath      LIKE _FileList._FileList-Name
  FIELD IndexNumber LIKE _Index._Idx-Num
  FIELD TableName   LIKE _File._File-Name   /* The field is not used */
  FIELD IndexName   LIKE _Index._Index-Name /* The field is not used */
  FIELD IndexRead   LIKE _IndexStat._IndexStat-Read
  FIELD IndexCreate LIKE _IndexStat._IndexStat-Create
  FIELD IndexDelete LIKE _IndexStat._IndexStat-Delete
&IF DEFINED(Version_GE_102B)
&THEN
  FIELD IndexOsRead LIKE _IndexStat._IndexStat-OsRead
&ENDIF

  INDEX IndexStatIdx IS UNIQUE
        DbHost
        DbPath
        IndexNumber
. /* TEMP-TABLE ttIndexStat */


DEFINE {1} SHARED TEMP-TABLE ttResrcStat NO-UNDO
  FIELD DbHost    AS CHARACTER
  FIELD DbPath    LIKE _FileList._FileList-Name
  FIELD ResrcType AS CHARACTER
  FIELD ResrcName LIKE _Resrc._Resrc-Name
  FIELD ResrcLock LIKE _Resrc._Resrc-lock
  FIELD ResrcWait LIKE _Resrc._Resrc-wait

  INDEX ResrcStatIdx IS UNIQUE
      DbHost
      DbPath
      ResrcType
      ResrcName
. /* TEMP-TABLE ttResrcStat */

DEFINE {1} SHARED TEMP-TABLE ttAreaStatDump NO-UNDO SERIALIZE-NAME "ttAreaStat"
  FIELD DbHost      AS CHARACTER
  FIELD DbNam       AS CHARACTER
  FIELD AreaName    AS CHARACTER
  FIELD AreaNumber  AS INTEGER
  FIELD AreaRPB     AS INTEGER
  FIELD AreaClusterSize AS INTEGER
  FIELD AreaExtents AS INTEGER
  FIELD TableCount  AS INTEGER
  FIELD IndexCount  AS INTEGER
  FIELD AreaHWM     AS INT64
  FIELD AreaReads   AS INT64
  FIELD AreaWrites  AS INT64
  FIELD TableRead   AS INT64
  FIELD TableUpdate AS INT64
  FIELD TableCreate AS INT64
  FIELD TableDelete AS INT64
  FIELD IndexRead   AS INT64
  FIELD IndexCreate AS INT64
  FIELD IndexDelete AS INT64.

DEFINE {1} SHARED TEMP-TABLE ttTableStatDump NO-UNDO SERIALIZE-NAME "ttTableStat"
  FIELD DbHost      AS CHARACTER
  FIELD DbNam       AS CHARACTER
  FIELD areaName    AS CHARACTER
  FIELD tableName   AS CHARACTER
  FIELD indexCount  AS INTEGER
  FIELD TableRead   AS INT64
  FIELD TableUpdate AS INT64
  FIELD TableCreate AS INT64
  FIELD TableDelete AS INT64
  FIELD TableOsRead AS INT64
  /*
  FIELD TableReadSec   AS DECIMAL
  FIELD TableUpdateSec AS DECIMAL
  FIELD TableCreateSec AS DECIMAL
  FIELD TableDeleteSec AS DECIMAL
  FIELD TableOsReadSec AS DECIMAL
  */
  .

DEFINE {1} SHARED TEMP-TABLE ttIndexStatDump NO-UNDO SERIALIZE-NAME "ttIndexStat"
  FIELD DbHost      AS CHARACTER
  FIELD DbNam       AS CHARACTER
  FIELD AreaName    AS CHARACTER
  FIELD TableName   AS CHARACTER
  FIELD IndexName   AS CHARACTER
  FIELD IndexAttr   AS CHARACTER
  FIELD RootBlock   AS INT64
  FIELD IndexRead   AS INT64
  FIELD IndexCreate AS INT64
  FIELD IndexDelete AS INT64
  FIELD IndexOsRead AS INT64
  /*
  FIELD IndexReadSec AS DECIMAL
  FIELD IndexCreateSec AS DECIMAL
  FIELD IndexDeleteSec AS DECIMAL
  FIELD IndexOsReadSec AS DECIMAL
  */
  .

DEFINE {1} SHARED TEMP-TABLE ttDbStatDump NO-UNDO SERIALIZE-NAME "ttDbStat"
  FIELD DbHost      AS CHARACTER
  FIELD DbNam       AS CHARACTER
  FIELD DbPath      AS CHARACTER
  FIELD DbUptime AS INTEGER
  FIELD DbBlockSize AS INTEGER
  FIELD DbVersionn  AS INTEGER
  FIELD ConnectNotes AS CHARACTER
  FIELD LastTransId AS INT64
  FIELD DbSize      AS INT64
  FIELD BiSize      AS INT64
  FIELD Spin        AS INTEGER
  FIELD TableStatBase AS INTEGER
  FIELD TableRangeSize AS INTEGER
  FIELD HighestTableId AS INTEGER
  FIELD TableCount     AS INTEGER
  FIELD IndexStatBase  AS INTEGER
  FIELD IndexRangeSize AS INTEGER
  FIELD HighestIndexId AS INTEGER
  FIELD IndexCount     AS INTEGER
  FIELD DbStartTime    AS CHARACTER
  FIELD DbTimeStamp    AS CHARACTER
  FIELD PreviousSnapshot AS CHARACTER
  FIELD CurrSnaphost     AS CHARACTER
  FIELD SnapshotInterval AS INTEGER
  FIELD DbAccesses  AS INT64
  FIELD RecRead     AS INT64
  FIELD RecUpdate   AS INT64
  FIELD RecCreate   AS INT64
  FIELD RecDelete   AS INT64
  FIELD BytesRead   AS INT64
  FIELD BytesUpdate AS INT64
  FIELD BytesCreate AS INT64
  FIELD BytesDelete AS INT64
  FIELD IdxRead     AS INT64
  FIELD IdxCreate   AS INT64
  FIELD IdxDelete   AS INT64
  FIELD DbReads     AS INT64
  FIELD DbWrites    AS INT64
  FIELD DatDbReads  AS INT64
  FIELD DatDbWrites AS INT64
  FIELD IdxDbReads  AS INT64
  FIELD IdxDbWrites AS INT64
  FIELD BiWrites    AS INT64
  FIELD BiBytesWrtn AS INT64
  FIELD BiNotesWrtn AS INT64
  FIELD TransComm   AS INT64
  FIELD LatchLock   AS INT64
  FIELD LatchWait   AS INT64
  FIELD ResrcLock   AS INT64
  FIELD ResrcWait   AS INT64
  FIELD SemWaits    AS INT64
  FIELD TxeLock     AS INT64
  FIELD TxeWait     AS INT64
  FIELD SnapshotEtime AS INTEGER
  FIELD Checkpoints AS INT64
  FIELD BufFlushed  AS INT64
  FIELD MostLocks   AS INT64
  FIELD NumLocks    AS INT64
  FIELD B1LogicRds  AS INT64
  FIELD B1LogicWrts AS INT64
  FIELD B1OSRds     AS INT64
  FIELD B1OSWrts    AS INT64
  FIELD B2LogicRds  AS INT64
  FIELD B2LogicWrts AS INT64
  FIELD B2OSRds     AS INT64
  FIELD B2OSWrts    AS INT64
  FIELD B2LRUEnbld  AS LOGICAL
  FIELD AIWWrites   AS INT64
  FIELD AIBusyBufW  AS INT64
  FIELD AINoBufAvail AS INT64
  FIELD AIPrtlWrts  AS INT64
  FIELD AITotWrts   AS INT64
  FIELD BIWWrites   AS INT64
  FIELD AIBytesWrt  AS INT64
  FIELD SelfServCnt AS INT
  FIELD RemCltCnt   AS INT
  FIELD BrkrCnt     AS INT
  FIELD SrvrCnt     AS INT
  FIELD BtchCnt     AS INT
  FIELD B2Size      AS INT64
  .

DEFINE {1} SHARED TEMP-TABLE ttResrcStatDump NO-UNDO SERIALIZE-NAME "ttResrcStat"
  FIELD DbHost AS CHARACTER
  FIELD DbNam  AS CHARACTER
  FIELD ResrcType AS CHARACTER
  FIELD ResrcName AS CHARACTER
  FIELD ResrcLock AS INT64
  FIELD ResrcWait AS INT64
  /*FIELD ResrcLockSec AS DECIMAL
  FIELD ResrcWaitSec AS DECIMAL*/.
