/*------------------------------------------------------------------------
    File        : DbStatDump2.p
    Purpose     : Dump _TableStat/_IndexStat for DICTDB db.

    Syntax      :

    Description :

    Author(s)   : George Potemkin
    Created     : May 23, 2010
    Modified    : Jul 01, 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER ipLocalHost AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipTimeStamp AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipBaseDir   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipSkipRate  AS INTEGER   NO-UNDO.

/* Define the shared temp-tables to store the most recent stats: */
{DbStatDump.i}

/* Caching Db Schema: */

DEFINE TEMP-TABLE tt_Area NO-UNDO
  FIELD uArea-Number      LIKE _Area._Area-Number
  FIELD uArea-Name        LIKE _Area._Area-Name
  FIELD uArea-blocksize   LIKE _Area._Area-blocksize
  FIELD uArea-recbits     LIKE _Area._Area-recbits
  FIELD uArea-clustersize LIKE _Area._Area-clustersize
  FIELD uArea-extents     LIKE _Area._Area-extents
  FIELD AreaHWM           LIKE _AreaStatus._AreaStatus-Hiwater
  FIELD AreaReads         LIKE _ActIOFile._IOFile-Reads
  FIELD AreaWrites        LIKE _ActIOFile._IOFile-Writes

  FIELD TableCount        AS INTEGER
  FIELD IndexCount        AS INTEGER

  FIELD TableRead         LIKE _TableStat._TableStat-Read
  FIELD TableUpdate       LIKE _TableStat._TableStat-Update
  FIELD TableCreate       LIKE _TableStat._TableStat-Create
  FIELD TableDelete       LIKE _TableStat._TableStat-Delete

  FIELD IndexRead         LIKE _IndexStat._IndexStat-Read
  FIELD IndexCreate       LIKE _IndexStat._IndexStat-Create
  FIELD IndexDelete       LIKE _IndexStat._IndexStat-Delete

  INDEX uArea-Number
        uArea-Number
. /* DEFINE TEMP-TABLE tt_Area */

DEFINE TEMP-TABLE tt_File NO-UNDO
  FIELD uFile-Number LIKE _File._File-Number
  FIELD uFile-Name   LIKE _File._File-Name
  FIELD uPrime-Index LIKE _File._Prime-Index
  FIELD uFileRecid   AS RECID
  FIELD IndexCount   AS INTEGER
  INDEX uFile-Number
        uFile-Number
  INDEX uFileRecid
        uFileRecid
. /* DEFINE TEMP-TABLE tt_File */

DEFINE TEMP-TABLE tt_Index NO-UNDO
  FIELD uIdx-Num    LIKE _Index._Idx-Num
  FIELD uIndex-Name LIKE _Index._Index-Name
  FIELD uFile-recid LIKE _Index._File-recid
  FIELD IndexAttr   AS CHARACTER
  INDEX uIdx-Num
        uIdx-Num
. /* DEFINE TEMP-TABLE tt_Index */

DEFINE TEMP-TABLE tt_StorageObject NO-UNDO
  FIELD uObject-Type   LIKE _StorageObject._Object-Type
  FIELD uObject-Number LIKE _StorageObject._Object-Number
  FIELD uArea-Number   LIKE _StorageObject._Area-Number
  FIELD uObject-root   LIKE _StorageObject._Object-root
  INDEX uObject-Number
        uObject-Type
        uObject-Number
. /* DEFINE TEMP-TABLE tt_StorageObject */

DEFINE VARIABLE vDbHost  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vDbPath  LIKE _FileList._FileList-Name NO-UNDO.
DEFINE VARIABLE vConnect AS CHARACTER NO-UNDO.
DEFINE VARIABLE vParam   AS CHARACTER NO-UNDO.
DEFINE VARIABLE vDbSize  AS DECIMAL   NO-UNDO.

DEFINE VARIABLE vTableRangeSize AS INTEGER NO-UNDO.
DEFINE VARIABLE vMinTableStatId LIKE _TableStat._TableStat-Id NO-UNDO.
DEFINE VARIABLE vMaxTableStatId LIKE _TableStat._TableStat-Id NO-UNDO.
DEFINE VARIABLE vHighestTableId LIKE _File._File-Number NO-UNDO.
DEFINE VARIABLE vDbTableCount   AS INTEGER NO-UNDO.

DEFINE VARIABLE vIndexRangeSize AS INTEGER NO-UNDO.
DEFINE VARIABLE vMinIndexStatId LIKE _IndexStat._IndexStat-Id NO-UNDO.
DEFINE VARIABLE vMaxIndexStatId LIKE _IndexStat._IndexStat-Id NO-UNDO.
DEFINE VARIABLE vHighestIndexId LIKE _Index._Idx-Num NO-UNDO.
DEFINE VARIABLE vDbIndexCount   AS INTEGER NO-UNDO.

DEFINE VARIABLE vPrevStatDate   AS DATE    NO-UNDO.
DEFINE VARIABLE vPrevStatTime   AS INTEGER NO-UNDO.
DEFINE VARIABLE vCurrInterval   AS INTEGER NO-UNDO.
DEFINE VARIABLE vIgnorePrevStat AS LOGICAL NO-UNDO.
DEFINE VARIABLE vIgnoreTblIdx   AS LOGICAL NO-UNDO.

DEFINE STREAM DumpFile.
DEFINE VARIABLE vDumpPrefix AS CHARACTER NO-UNDO.

DEFINE VARIABLE i AS INTEGER NO-UNDO.

/* ******************************  Functions  ****************************** */

/* ------------------------------------------------------------------------- */
FUNCTION Stamp2Date RETURNS DATE (INPUT ipString AS CHARACTER):
/* Input string must have a format like: 20100607_145148 */
  DEFINE VARIABLE vDate AS DATE NO-UNDO INITIAL ?.
  ASSIGN vDate = DATE(INTEGER(SUBSTRING(ipString, 5,2)), /*Month*/
                      INTEGER(SUBSTRING(ipString, 7,2)), /*Day  */
                      INTEGER(SUBSTRING(ipString, 1,4))) /*Year */
  NO-ERROR.
  RETURN vDate.
END FUNCTION. /* String2Date */

/* ------------------------------------------------------------------------- */
FUNCTION Stamp2Time RETURNS INTEGER (INPUT ipString AS CHARACTER):
/* Input string must have a format like: 20100607_145148 */
  DEFINE VARIABLE vTime AS INTEGER NO-UNDO INITIAL ?.
  ASSIGN vTime = INTEGER(SUBSTRING(ipString,10,2)) /*HH*/ * 3600
               + INTEGER(SUBSTRING(ipString,12,2)) /*MM*/ * 60
               + INTEGER(SUBSTRING(ipString,14,2)) /*SS*/
  NO-ERROR.
  RETURN vTime.
END FUNCTION. /* String2Time */

/* ------------------------------------------------------------------------- */
FUNCTION String2Date RETURNS DATE (INPUT ipString AS CHARACTER).
/* Input string must have a format like: Tue Jul 27 12:11:45 2004 */
  DEFINE VARIABLE vDate AS DATE NO-UNDO INITIAL ?.
  DEFINE VARIABLE vMonthList AS CHARACTER NO-UNDO
    INITIAL "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec".

  ASSIGN ipString = TRIM(REPLACE(ipString, "  ":U, " ":U))
         vDate = DATE( /*Month:*/ LOOKUP (ENTRY(2,ipString," ":U),vMonthList),
                       /*Day:  */ INTEGER(ENTRY(3,ipString," ":U)),
                       /*Year: */ INTEGER(ENTRY(5,ipString," ":U)))
  NO-ERROR.
  RETURN vDate.
END FUNCTION. /* String2Date */

/* ------------------------------------------------------------------------- */
FUNCTION String2Time RETURNS INTEGER (INPUT ipString AS CHARACTER).
/* Input string must have a format like: Tue Jul 27 12:11:45 2004 */
  DEFINE VARIABLE vTime AS INTEGER NO-UNDO INITIAL ?.
  ASSIGN ipString = TRIM(REPLACE(ipString, "  ":U, " ":U))
         ipString = ENTRY(4,ipString," ":U)
         vTime    = INTEGER(ENTRY(1, ipString, ":":U)) * 3600 /* HH */
                  + INTEGER(ENTRY(2, ipString, ":":U)) * 60   /* MM */
                  + INTEGER(ENTRY(3, ipString, ":":U))        /* SS */
  NO-ERROR.
  RETURN vTime.
END FUNCTION. /* String2Time */


/* TODO En 10.1A (et jusque quelle version ?) il faut un paramètre INTEGER) */
/* ------------------------------------------------------------------------- */
&IF DEFINED(Version_GE_101B) &THEN
FUNCTION PerSec RETURNS DECIMAL (INPUT ipValue AS DECIMAL).
&ELSE
FUNCTION PerSec RETURNS DECIMAL (INPUT ipValue AS INTEGER).
&ENDIF
  RETURN TRUNCATE(ipValue / vCurrInterval, 2).
END FUNCTION. /* String2Time */

/* ******************************  Procedures  ***************************** */

/* ------------------------------------------------------------------------- */
PROCEDURE ReadSchema.

  DEFINE OUTPUT PARAMETER opHighestTableId LIKE _File._File-Number NO-UNDO.
  DEFINE OUTPUT PARAMETER opHighestIndexId LIKE _Index._Idx-Num NO-UNDO.
  DEFINE OUTPUT PARAMETER opDbTableCount   AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER opDbIndexCount   AS INTEGER NO-UNDO.

  FOR FIRST  DICTDB._Db NO-LOCK
      WHERE  DICTDB._Db._Db-local:

    FOR EACH DICTDB._File OF DICTDB._Db NO-LOCK
       WHERE DICTDB._File._File-Number GT 0
         AND DICTDB._File._File-Number LT 32768
    TRANSACTION:

      CREATE tt_File.
      ASSIGN tt_File.uFile-Number = DICTDB._File._File-Number
             tt_File.uFile-Name   = DICTDB._File._File-Name
             tt_File.uFileRecid   = RECID(DICTDB._File)
             tt_File.uPrime-Index = DICTDB._File._Prime-Index
             opDbTableCount       = opDbTableCount + 1
      . /* ASSIGN */
    END. /* FOR EACH _File */

    FOR EACH DICTDB._StorageObject OF DICTDB._Db NO-LOCK
       WHERE DICTDB._StorageObject._Object-type   GE 1
         AND DICTDB._StorageObject._Object-type   LE 2
    TRANSACTION:

      CREATE tt_StorageObject.
      ASSIGN
        tt_StorageObject.uObject-Type   = DICTDB._StorageObject._Object-Type
        tt_StorageObject.uObject-Number = DICTDB._StorageObject._Object-Number
        tt_StorageObject.uArea-Number   = DICTDB._StorageObject._Area-Number
        tt_StorageObject.uObject-root   = DICTDB._StorageObject._Object-root
      . /* ASSIGN */
    END. /* FOR EACH _StorageObject */
  END. /* FOR FIRST _Db */

  FOR EACH DICTDB._Area NO-LOCK
     WHERE DICTDB._Area._Area-Type GE 3
       AND DICTDB._Area._Area-Type LE 6
  TRANSACTION:

    CREATE tt_Area.
    ASSIGN tt_Area.uArea-Number      = DICTDB._Area._Area-Number
           tt_Area.uArea-Name        = DICTDB._Area._Area-Name
           tt_Area.uArea-blocksize   = DICTDB._Area._Area-blocksize
           tt_Area.uArea-recbits     = DICTDB._Area._Area-recbits
           tt_Area.uArea-clustersize = DICTDB._Area._Area-clustersize
           tt_Area.uArea-extents     = DICTDB._Area._Area-extents

    . /* ASSIGN */
  END. /* EACH _Area */

  FOR EACH DICTDB._Index NO-LOCK
     WHERE DICTDB._Index._Idx-Num GT 0
       AND DICTDB._Index._Idx-Num LT 32768,

    FIRST tt_File NO-LOCK
    WHERE tt_File.uFileRecid EQ DICTDB._Index._File-Recid
  TRANSACTION:

    CREATE tt_Index.
    ASSIGN tt_Index.uIdx-Num    = DICTDB._Index._Idx-Num
           tt_Index.uIndex-Name = DICTDB._Index._Index-Name
           tt_Index.uFile-Recid = DICTDB._Index._File-Recid
           tt_Index.IndexAttr   =
             (IF DICTDB._Index._Active THEN "":U ELSE "i":U) +
             (IF tt_File.uPrime-Index EQ RECID(DICTDB._Index)
              THEN "p":U ELSE "":U) +
             (IF DICTDB._Index._Unique THEN "u":U ELSE "":U) +
             (IF DICTDB._Index._Wordidx GT 0 THEN "w":U ELSE "":U) +
             "c" + STRING(DICTDB._Index._num-comp)
           opDbIndexCount     = opDbIndexCount     + 1
           tt_File.IndexCount = tt_File.IndexCount + 1
    . /* ASSIGN */
  END. /* FOR EACH _Index */

/* HighestTableId: */
  FOR EACH tt_File NO-LOCK
        BY tt_File.uFile-Number DESCENDING:
    ASSIGN opHighestTableId = tt_File.uFile-Number.
    LEAVE.
  END.

/* HighestIndexId: */
  FOR EACH tt_Index NO-LOCK
        BY tt_Index.uIdx-Num DESCENDING:
    ASSIGN opHighestIndexId = tt_Index.uIdx-Num.
    LEAVE.
  END.

END PROCEDURE. /* ReadSchema */

/* -------------------------------------------------------------------- */
PROCEDURE DbStat.
  DEFINE BUFFER bufActivity2 FOR DICTDB._ActBuffer.

  DEFINE VARIABLE vBiSize AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vLatchLock LIKE _Resrc._Resrc-lock /*_Latch._Latch-lock*/ NO-UNDO.
  DEFINE VARIABLE vLatchWait LIKE _Resrc._Resrc-wait /*_Latch._Latch-wait*/ NO-UNDO.
  DEFINE VARIABLE vResrcLock LIKE _Resrc._Resrc-lock           NO-UNDO.
  DEFINE VARIABLE vResrcWait LIKE _Resrc._Resrc-wait           NO-UNDO.
  DEFINE VARIABLE vTxeLock   LIKE _TxeLock._Txe-Locks EXTENT 0 NO-UNDO.
  DEFINE VARIABLE vTxeWait   LIKE _TxeLock._Txe-Waits EXTENT 0 NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.


/* Primary Recovery Area: */
  FOR FIRST DICTDB._AreaStatus NO-LOCK
      WHERE DICTDB._AreaStatus._AreaStatus-Areanum EQ 3,
      FIRST tt_Area NO-LOCK
      WHERE tt_Area.uArea-number EQ DICTDB._AreaStatus._AreaStatus-Areanum:

    ASSIGN vBiSize = DICTDB._AreaStatus._AreaStatus-Hiwater
                   * tt_Area.uArea-blocksize.
  END.

/* Resources: */
  ASSIGN vResrcLock = 0  /* Number of times the resource was locked */
         vResrcWait = 0  /* Number of times a process waited on resource */
  . /* ASSIGN */
  FOR EACH DICTDB._Resrc NO-LOCK:
    ASSIGN vResrcLock = vResrcLock + DICTDB._Resrc._Resrc-lock
           vResrcWait = vResrcWait + DICTDB._Resrc._Resrc-wait.
  END.

/* Latches: */
  ASSIGN vLatchLock = 0  /* Number of times the latch was locked */
         vLatchWait = 0  /* Number of times a process had to nap */
  . /* ASSIGN */
  FOR EACH DICTDB._Latch NO-LOCK
     WHERE DICTDB._Latch._Latch-id GT 1:
    ASSIGN vLatchLock = vLatchLock + DICTDB._Latch._Latch-lock
           vLatchWait = vLatchWait + DICTDB._Latch._Latch-wait.
  END.

/* TXE Locks: */
  FOR FIRST DICTDB._TxeLock NO-LOCK:
    DO i = 1 TO 9:
      ASSIGN vTxeLock = vTxeLock + DICTDB._TxeLock._Txe-Locks[i]
             vTxeWait = vTxeWait + DICTDB._TxeLock._Txe-Waits[i].
    END.
  END.

  FOR FIRST ttDbStat EXCLUSIVE
      WHERE ttDbStat.DbHost EQ vDbHost
        AND ttDbStat.DbPath EQ vDbPath,

      FIRST tt_Area NO-LOCK WHERE tt_Area.uArea-number EQ 6, /* Schema Area */

      FIRST DICTDB._ActSummary NO-LOCK,
      FIRST DICTDB._ActIOType  NO-LOCK,
      FIRST DICTDB._ActRecord  NO-LOCK,
      FIRST DICTDB._ActIndex   NO-LOCK,
      FIRST DICTDB._ActBILog   NO-LOCK,
      FIRST DICTDB._ActOther   NO-LOCK,
      FIRST DICTDB._MstrBlk    NO-LOCK,
      FIRST DICTDB._Startup    NO-LOCK,
      FIRST DICTDB._DbStatus   NO-LOCK,
      FIRST DICTDB._ActBuffer  WHERE _ActBuffer._Buffer-Id = 2 NO-LOCK,
      FIRST bufActivity2 WHERE bufActivity2._Buffer-Id = 3 NO-LOCK,
      FIRST DICTDB._ActAILog   NO-LOCK
  TRANSACTION:

    IF ttDbStat.DbTimeStamp NE ? THEN
    IF ttDbStat.DbTimeStamp NE DICTDB._MstrBlk._MstrBlk-TimeStamp THEN
    ASSIGN vConnect = "SchemaChanged ":U + vConnect.

    CREATE ttDbStatDump.
    ASSIGN ttDbStatDump.DbHost = vDbHost
           ttDbStatDump.DbNam = LDBNAME("DICTDB":U)
           ttDbStatDump.DbPath = vDbPath
           ttDbStatDump.DbUptime = DICTDB._ActSummary._Summary-UpTime
           ttDbStatDump.DbBlockSize = tt_Area.uArea-blocksize
           ttDbStatDump.DbBlockSize = DICTDB._MstrBlk._MstrBlk-DbVers
           ttDbStatDump.ConnectNotes =  vConnect + " ":U + DBRESTRICTION("DICTDB":U)
           ttDbStatDump.LastTransId = DICTDB._MstrBlk._MstrBlk-LastTask
           ttDbStatDump.DbSize = vDbSize * tt_Area.uArea-blocksize
           ttDbStatDump.BiSize = vBiSize
           ttDbStatDump.Spin = DICTDB._Startup._Startup-Spin
           ttDbStatDump.B2Size = DICTDB._Startup._Startup-Alternate_Buffs
           ttDbStatDump.TableStatBase = vMinTableStatId
           ttDbStatDump.TableRangeSize = vTableRangeSize
           ttDbStatDump.HighestTableId = vHighestTableId
           ttDbStatDump.TableCount = vDbTableCount
           ttDbStatDump.IndexStatBase = vMinIndexStatId
           ttDbStatDump.IndexRangeSize = vIndexRangeSize
           ttDbStatDump.HighestIndexId = vHighestIndexId
           ttDbStatDump.IndexCount = vDbIndexCount
           ttDbStatDump.DbStartTime = DICTDB._MstrBlk._MstrBlk-oprdate
           ttDbStatDump.DbTimeStamp = DICTDB._MstrBlk._MstrBlk-TimeStamp

           ttDbStatDump.PreviousSnapshot = ttDbStat.SnapshotTimeStamp
           ttDbStatDump.CurrSnaphost = ipTimeStamp
           ttDbStatDump.SnapshotInterval = vCurrInterval

           ttDbStatDump.DbAccesses = DICTDB._ActSummary._Summary-DbAccesses - ttDbStat.DbAccesses

           ttDbStatDump.RecRead = DICTDB._ActRecord._Record-RecRead  - ttDbStat.RecRead
           ttDbStatDump.RecUpdate = DICTDB._ActRecord._Record-RecUpd   - ttDbStat.RecUpdate
           ttDbStatDump.RecCreate = DICTDB._ActRecord._Record-RecCreat - ttDbStat.RecCreate
           ttDbStatDump.RecDelete = DICTDB._ActRecord._Record-RecDel   - ttDbStat.RecDelete

           ttDbStatDump.BytesRead = DICTDB._ActRecord._Record-BytesRead  - ttDbStat.BytesRead
           ttDbStatDump.BytesUpdate = DICTDB._ActRecord._Record-BytesUpd   - ttDbStat.BytesUpdate
           ttDbStatDump.BytesCreate = DICTDB._ActRecord._Record-BytesCreat - ttDbStat.BytesCreate
           ttDbStatDump.BytesDelete = DICTDB._ActRecord._Record-BytesDel   - ttDbStat.BytesDelete

           ttDbStatDump.IdxRead = DICTDB._ActIndex._Index-Find   - ttDbStat.IdxRead
           ttDbStatDump.IdxCreate = DICTDB._ActIndex._Index-Create - ttDbStat.IdxCreate
           ttDbStatDump.IdxDelete = DICTDB._ActIndex._Index-Delete - ttDbStat.IdxDelete

           ttDbStatDump.DbReads = DICTDB._ActSummary._Summary-DbReads  - ttDbStat.DbReads
           ttDbStatDump.DatDbReads = DICTDB._ActIOType._IOType-DataReads  - ttDbStat.DatDbReads
           ttDbStatDump.IdxDbReads = DICTDB._ActIOType._IOType-IdxRds     - ttDbStat.IdxDbReads
           ttDbStatDump.DbWrites = DICTDB._ActSummary._Summary-DbWrites - ttDbStat.DbWrites
           ttDbStatDump.DatDbWrites = DICTDB._ActIOType._IOType-DataWrts   - ttDbStat.DatDbWrites
           ttDbStatDump.IdxDbWrites = DICTDB._ActIOType._IOType-IdxWrts    - ttDbStat.IdxDbWrites

           ttDbStatDump.BiWrites = DICTDB._ActBILog._BiLog-TotalWrts - ttDbStat.BiWrites
           ttDbStatDump.BiBytesWrtn = DICTDB._ActBILog._BiLog-BytesWrtn - ttDbStat.BiBytesWrtn
           ttDbStatDump.BiNotesWrtn = DICTDB._ActBILog._BiLog-RecWriten - ttDbStat.BiNotesWrtn
           ttDbStatDump.TransComm = DICTDB._ActSummary._Summary-TransComm  - ttDbStat.TransComm

           ttDbStatDump.LatchLock = vLatchLock - ttDbStat.LatchLock
           ttDbStatDump.LatchWait = vLatchWait - ttDbStat.LatchWait
           ttDbStatDump.ResrcLock = vResrcLock - ttDbStat.ResrcLock
           ttDbStatDump.ResrcWait = vResrcWait - ttDbStat.ResrcWait
           ttDbStatDump.SemWaits = DICTDB._ActOther._Other-Wait - ttDbStat.SemWaits
           ttDbStatDump.TxeLock = vTxeLock   - ttDbStat.TxeLock
           ttDbStatDump.TxeWait = vTxeWait   - ttDbStat.TxeWait

           ttDbStatDump.SnapshotETime = ETIME
           ttDbStatDump.Checkpoints = DICTDB._ActSummary._Summary-Chkpts - ttDbStat.Checkpoints
           ttDbStatDump.BufFlushed = DICTDB._ActSummary._Summary-Flushed - ttDbStat.Flushed
           ttDbStatDump.MostLocks = DICTDB._DbStatus._DbStatus-MostLocks
           ttDbStatDump.NumLocks = DICTDB._DbStatus._DbStatus-NumLocks
           ttDbStatDump.B1LogicRds = DICTDB._ActBuffer._Buffer-LogicRds - ttDbStat.B1LogicRds
           ttDbStatDump.B1LogicWrts = DICTDB._ActBuffer._Buffer-LogicWrts - ttDbStat.B1LogicWrts
           ttDbStatDump.B1OSRds = DICTDB._ActBuffer._Buffer-OSRds - ttDbStat.B1OSRds
           ttDbStatDump.B1OSWrts = DICTDB._ActBuffer._Buffer-OSWrts - ttDbStat.B1OSWrts
           ttDbStatDump.B2LogicRds = bufActivity2._Buffer-LogicRds - ttDbStat.B2LogicRds
           ttDbStatDump.B2LogicWrts = bufActivity2._Buffer-LogicWrts - ttDbStat.B2LogicWrts
           ttDbStatDump.B2OSRds = bufActivity2._Buffer-OSRds - ttDbStat.B2OSRds
           ttDbStatDump.B2OSWrts = bufActivity2._Buffer-OSWrts - ttDbStat.B2OSWrts
           ttDbStatDump.AIWWrites = DICTDB._ActAILog._AiLog-AIWWrites - ttDbStat.AIWWrites
           ttDbStatDump.AIBusyBufW = DICTDB._ActAILog._AiLog-BBuffWaits - ttDbStat.AIBusyBufW
           ttDbStatDump.AINoBufAvail = DICTDB._ActAILog._AiLog-NoBufAvail - ttDbStat.AINoBufAvail
           ttDbStatDump.AIPrtlWrts = DICTDB._ActAILog._AiLog-PartialWrt - ttDbStat.AIPrtlWrts
           ttDbStatDump.AITotWrts = DICTDB._ActAILog._AiLog-TotWrites - ttDbStat.AITotWrts
           ttDbStatDump.BIWWrites = DICTDB._ActBILog._BiLog-BIWWrites - ttDbStat.BIWWrites
           ttDbStatDump.AIBytesWrt = DICTDB._ActAILog._AiLog-BytesWritn - ttDbStat.AIBytesWrt
           .
   
     FOR EACH DICTDB._Connect WHERE DICTDB._Connect._Connect-Name GT "" NO-LOCK:
         IF DICTDB._Connect._Connect-Type EQ 'BATCH' THEN ttDbStatDump.BtchCnt = ttDbStatDump.BtchCnt + 1.
         ELSE IF DICTDB._Connect._Connect-Type EQ 'REMC' THEN ttDbStatDump.RemCltCnt = ttDbStatDump.RemCltCnt + 1.
         ELSE IF DICTDB._Connect._Connect-Type EQ 'BROK' THEN ttDbStatDump.BrkrCnt = ttDbStatDump.BrkrCnt + 1.
         ELSE IF DICTDB._Connect._Connect-Type EQ 'SERV' OR DICTDB._Connect._Connect-Type EQ 'SQSV' THEN ttDbStatDump.SrvrCnt = ttDbStatDump.SrvrCnt + 1.
         ELSE IF DICTDB._Connect._Connect-Type EQ 'SELF' THEN ttDbStatDump.SelfServCnt = ttDbStatDump.SelfServCnt + 1.
     END.

/* Save the current snapshot: */
    ASSIGN
      ttDbStat.DbStartTime = DICTDB._MstrBlk._MstrBlk-oprdate
      ttDbStat.DbTimeStamp = DICTDB._MstrBlk._MstrBlk-TimeStamp

      ttDbStat.SnapshotTimeStamp = ipTimeStamp

      ttDbStat.DbAccesses  = DICTDB._ActSummary._Summary-DbAccesses
      ttDbStat.RecRead     = DICTDB._ActRecord._Record-RecRead
      ttDbStat.RecUpdate   = DICTDB._ActRecord._Record-RecUpd
      ttDbStat.RecCreate   = DICTDB._ActRecord._Record-RecCreat
      ttDbStat.RecDelete   = DICTDB._ActRecord._Record-RecDel

      ttDbStat.BytesRead   = DICTDB._ActRecord._Record-BytesRead
      ttDbStat.BytesUpdate = DICTDB._ActRecord._Record-BytesUpd
      ttDbStat.BytesCreate = DICTDB._ActRecord._Record-BytesCreat
      ttDbStat.BytesDelete = DICTDB._ActRecord._Record-BytesDel

      ttDbStat.IdxRead     = DICTDB._ActIndex._Index-Find
      ttDbStat.IdxCreate   = DICTDB._ActIndex._Index-Create
      ttDbStat.IdxDelete   = DICTDB._ActIndex._Index-Delete

      ttDbStat.DbReads     = DICTDB._ActSummary._Summary-DbReads
      ttDbStat.DbWrites    = DICTDB._ActSummary._Summary-DbWrites
      ttDbStat.DatDbReads  = DICTDB._ActIOType._IOType-DataReads
      ttDbStat.DatDbWrites = DICTDB._ActIOType._IOType-DataWrts
      ttDbStat.IdxDbReads  = DICTDB._ActIOType._IOType-IdxRds
      ttDbStat.IdxDbWrites = DICTDB._ActIOType._IOType-IdxWrts

      ttDbStat.BiWrites    = DICTDB._ActBILog._BiLog-TotalWrts
      ttDbStat.BiBytesWrtn = DICTDB._ActBILog._BiLog-BytesWrtn
      ttDbStat.BiNotesWrtn = DICTDB._ActBILog._BiLog-RecWriten
      ttDbStat.TransComm   = DICTDB._ActSummary._Summary-TransComm

      ttDbStat.LatchLock   = vLatchLock
      ttDbStat.LatchWait   = vLatchWait
      ttDbStat.ResrcLock   = vResrcLock
      ttDbStat.ResrcWait   = vResrcWait
      ttDbStat.SemWaits    = DICTDB._ActOther._Other-Wait
      ttDbStat.TxeLock     = vTxeLock
      ttDbStat.TxeWait     = vTxeWait
      ttDbStat.Checkpoints = DICTDB._ActSummary._Summary-Chkpts
      ttDbStat.Flushed     = DICTDB._ActSummary._Summary-Flushed
      ttDbStat.MostLocks   = DICTDB._DbStatus._DbStatus-MostLocks
      ttDbStat.B1LogicRds = DICTDB._ActBuffer._Buffer-LogicRds
      ttDbStat.B1LogicWrts = DICTDB._ActBuffer._Buffer-LogicWrts
      ttDbStat.B1OSRds = DICTDB._ActBuffer._Buffer-OSRds
      ttDbStat.B1OSWrts = DICTDB._ActBuffer._Buffer-OSWrts
      ttDbStat.B2LogicRds = bufActivity2._Buffer-LogicRds
      ttDbStat.B2LogicWrts = bufActivity2._Buffer-LogicWrts
      ttDbStat.B2OSRds = bufActivity2._Buffer-OSRds
      ttDbStat.B2OSWrts = bufActivity2._Buffer-OSWrts
      ttDbStat.B2LRUEnbld = bufActivity2._Buffer-LRUEnabled
      ttDbStat.AIWWrites = DICTDB._ActAILog._AiLog-AIWWrites
      ttDbStat.AIBusyBufW = DICTDB._ActAILog._AiLog-BBuffWaits
      ttDbStat.AINoBufAvail = DICTDB._ActAILog._AiLog-NoBufAvail
      ttDbStat.AIPrtlWrts = DICTDB._ActAILog._AiLog-PartialWrt
      ttDbStat.AITotWrts = DICTDB._ActAILog._AiLog-TotWrites
      ttDbStat.AIBytesWrt = DICTDB._ActAILog._AiLog-BytesWritn
      ttDbStat.BIWWrites = DICTDB._ActBILog._BiLog-BIWWrites
      ttDbStat.TblIdxSkip = ttDbStat.TblIdxSkip + 1
    . /* ASSIGN */
  END. /* FOR FIRST ttDbStat */

END PROCEDURE. /* DbStat */

/* -------------------------------------------------------------------- */
PROCEDURE AreaStat.

  DEFINE VARIABLE vExtentId AS INTEGER NO-UNDO.

/* Performance rules for remote connections:
   1. Use FOR EACH for db tables when possible.
   2. Don't read db tables inside transaction block.
*/
/* Initiate ttAreaStat records and copy _AreaStatus to tt_Area: */
  FOR EACH DICTDB._AreaStatus NO-LOCK
     WHERE DICTDB._AreaStatus._AreaStatus-Areanum GE 6,

     FIRST tt_Area NO-LOCK
     WHERE tt_Area.uArea-number EQ DICTDB._AreaStatus._AreaStatus-Areanum
  TRANSACTION:

    FIND FIRST ttAreaStat NO-LOCK
         WHERE ttAreaStat.DbHost     EQ vDbHost
           AND ttAreaStat.DbPath     EQ vDbPath
           AND ttAreaStat.AreaNumber EQ tt_Area.uArea-number
    NO-ERROR.

    IF NOT AVAILABLE ttAreaStat THEN
    CREATE ttAreaStat.

    IF vIgnorePrevStat THEN
    ASSIGN
      ttAreaStat.AreaHWM     = 0
      ttAreaStat.AreaReads   = 0
      ttAreaStat.AreaWrites  = 0
      ttAreaStat.TableRead   = 0
      ttAreaStat.TableUpdate = 0
      ttAreaStat.TableCreate = 0
      ttAreaStat.TableDelete = 0
      ttAreaStat.IndexRead   = 0
      ttAreaStat.IndexCreate = 0
      ttAreaStat.IndexDelete = 0
    . /* ASSIGN */
    
    ASSIGN
      ttAreaStat.DbHost      = vDbHost
      ttAreaStat.DbPath      = vDbPath
      ttAreaStat.AreaNumber  = tt_Area.uArea-number
      ttAreaStat.AreaName    = tt_Area.uArea-Name

      tt_Area.AreaHWM    = DICTDB._AreaStatus._AreaStatus-Hiwater
      tt_Area.AreaReads  = 0
      tt_Area.AreaWrites = 0
    . /* ASSIGN */
  END.

  ASSIGN vExtentId = 0.
  FOR EACH DICTDB._AreaExtent NO-LOCK:

    ASSIGN vExtentId = vExtentId + 1.
/* The records in _ActIOFile and _AreaExtent used to be in the same order: */
    FIND FIRST DICTDB._ActIOFile NO-LOCK
         WHERE DICTDB._ActIOFile._IOFile-Id EQ vExtentId
    NO-ERROR.

/* ...otherwise do a full scan for the matches: */
    IF NOT AVAILABLE DICTDB._ActIOFile
    OR NOT DICTDB._ActIOFile._IOFile-FileName MATCHES
    ("*" + DICTDB._AreaExtent._Extent-path)
    THEN
    FIND FIRST DICTDB._ActIOFile NO-LOCK
         WHERE DICTDB._ActIOFile._IOFile-FileName MATCHES
        ("*" + DICTDB._AreaExtent._Extent-path)
    NO-ERROR.

    IF AVAILABLE DICTDB._ActIOFile THEN
    FOR FIRST tt_Area
        WHERE tt_Area.uArea-number EQ DICTDB._AreaExtent._Area-number:
      ASSIGN
        vExtentId          = DICTDB._ActIOFile._IOFile-Id
        tt_Area.AreaReads  = tt_Area.AreaReads
                           + DICTDB._ActIOFile._IOFile-Reads
        tt_Area.AreaWrites = tt_Area.AreaWrites
                           + DICTDB._ActIOFile._IOFile-Writes
      . /* ASSIGN */
    END.
  END. /* FOR EACH DICTDB._AreaExtent OF DICTDB._Area */

  FOR EACH tt_Area NO-LOCK
     WHERE tt_Area.uArea-number GE 6,
    
     FIRST ttAreaStat EXCLUSIVE
     WHERE ttAreaStat.AreaNumber EQ tt_Area.uArea-number:

    CREATE ttAreaStatDump.
    ASSIGN ttAreaStatDump.DbHost = vDbHost
           ttAreaStatDump.DbNam = LDBNAME("DICTDB":U)
           ttAreaStatDump.AreaName = tt_Area.uArea-name
           ttAreaStatDump.AreaNumber = tt_Area.uArea-number
           ttAreaStatDump.AreaRPB = INTEGER(EXP(2, tt_Area.uArea-recbits))
           ttAreaStatDump.AreaClusterSize = tt_Area.uArea-clustersize
           ttAreaStatDump.AreaExtents = tt_Area.uArea-extents
           ttAreaStatDump.TableCount = tt_Area.TableCount
           ttAreaStatDump.IndexCount = tt_Area.IndexCount
           ttAreaStatDump.AreaHWM = tt_Area.AreaHWM
           ttAreaStatDump.AreaReads = tt_Area.AreaReads   - ttAreaStat.AreaReads
           ttAreaStatDump.AreaWrites = tt_Area.AreaWrites  - ttAreaStat.AreaWrites
           ttAreaStatDump.TableRead = tt_Area.TableRead   - ttAreaStat.TableRead
           ttAreaStatDump.TableUpdate = tt_Area.TableUpdate - ttAreaStat.TableUpdate
           ttAreaStatDump.TableCreate = tt_Area.TableCreate - ttAreaStat.TableCreate
           ttAreaStatDump.TableDelete = tt_Area.TableDelete - ttAreaStat.TableDelete
           ttAreaStatDump.IndexRead = tt_Area.IndexRead   - ttAreaStat.IndexRead
           ttAreaStatDump.IndexCreate = tt_Area.IndexCreate - ttAreaStat.IndexCreate
           ttAreaStatDump.IndexDelete = tt_Area.IndexDelete - ttAreaStat.IndexDelete
           .

    ASSIGN
      vDbSize = vDbSize + tt_Area.AreaHWM
/* Save the current snapshot: */
      ttAreaStat.AreaReads   = tt_Area.AreaReads
      ttAreaStat.AreaWrites  = tt_Area.AreaWrites
      ttAreaStat.TableRead   = tt_Area.TableRead
      ttAreaStat.TableUpdate = tt_Area.TableUpdate
      ttAreaStat.TableCreate = tt_Area.TableCreate
      ttAreaStat.TableDelete = tt_Area.TableDelete
      ttAreaStat.IndexRead   = tt_Area.IndexRead
      ttAreaStat.IndexCreate = tt_Area.IndexCreate
      ttAreaStat.IndexDelete = tt_Area.IndexDelete
    . /* ASSIGN */

    ACCUMULATE tt_Area.uArea-extents (TOTAL).
    ACCUMULATE tt_Area.TableCount    (TOTAL).
    ACCUMULATE tt_Area.IndexCount    (TOTAL).
/* DbStat reports vDbTableCount and vDbIndexCount.
   The accumulated values reported by AreaStat must match the values in DbStat.
   It's just a kind of sanity check.
*/
  END. /* FOR EACH _Area */

END PROCEDURE. /* AreaStat */

/* -------------------------------------------------------------------- */
PROCEDURE TableStat.

  FOR FIRST ttDbStat NO-LOCK
      WHERE ttDbStat.DbHost EQ vDbHost
        AND ttDbStat.DbPath EQ vDbPath,

      FIRST DICTDB._ActRecord NO-LOCK,
      FIRST DICTDB._ActIOType NO-LOCK:

  END. /* FOR FIRST ttDbStat, FIRST _ActRecord */

  FOR EACH DICTDB._TableStat NO-LOCK,

     FIRST tt_File NO-LOCK
     WHERE tt_File.uFile-Number EQ DICTDB._TableStat._TableStat-id,

     FIRST tt_StorageObject NO-LOCK
     WHERE tt_StorageObject.uObject-Type   EQ 1
       AND tt_StorageObject.uObject-Number EQ tt_File.uFile-Number,

     FIRST tt_Area
     WHERE tt_Area.uArea-Number EQ tt_StorageObject.uArea-Number

     WHILE DICTDB._TableStat._TableStat-id LE vHighestTableId
  TRANSACTION:

    FIND FIRST ttTableStat EXCLUSIVE
         WHERE ttTableStat.DbHost      EQ vDbHost
           AND ttTableStat.DbPath      EQ vDbPath
           AND ttTableStat.TableNumber EQ tt_File.uFile-Number
    NO-ERROR.

    IF NOT AVAILABLE ttTableStat THEN
    CREATE ttTableStat.

    IF vIgnorePrevStat THEN
    ASSIGN
      ttTableStat.TableRead   = 0
      ttTableStat.TableUpdate = 0
      ttTableStat.TableCreate = 0
      ttTableStat.TableDelete = 0
      &IF DEFINED(Version_GE_102B)
      &THEN
      ttTableStat.TableOsRead = 0
      &ENDIF
    . /* ASSIGN */

    CREATE ttTableStatDump.
    ASSIGN ttTableStatDump.DbHost = vDbHost
           ttTableStatDump.DbNam = LDBNAME("DICTDB":U)
           ttTableStatDump.AreaName = tt_Area.uArea-Name
           ttTableStatDump.TableNam = tt_File.uFile-Name
           ttTableStatDump.IndexCount = tt_File.IndexCount
           ttTableStatDump.TableRead = DICTDB._TableStat._TableStat-Read   - ttTableStat.TableRead
           ttTableStatDump.TableUpdate = DICTDB._TableStat._TableStat-Update - ttTableStat.TableUpdate
           ttTableStatDump.TableCreate = DICTDB._TableStat._TableStat-Create - ttTableStat.TableCreate
           ttTableStatDump.TableDelete = DICTDB._TableStat._TableStat-Delete - ttTableStat.TableDelete
&IF DEFINED(Version_GE_102B)
&THEN
           ttTableStatDump.TableOsRead = DICTDB._TableStat._TableStat-OsRead - ttTableStat.TableOsRead
&ENDIF
/*           ttTableStatDump.TableReadSec = PerSec(DICTDB._TableStat._TableStat-Read   - ttTableStat.TableRead)
           
           ttTableStatDump.TableUpdateSec = PerSec(DICTDB._TableStat._TableStat-Update - ttTableStat.TableUpdate)
           ttTableStatDump.TableCreateSec = PerSec(DICTDB._TableStat._TableStat-Create - ttTableStat.TableCreate)
           ttTableStatDump.TableDeleteSec = PerSec(DICTDB._TableStat._TableStat-Delete - ttTableStat.TableDelete)
&IF DEFINED(Version_GE_102B)
&THEN
           ttTableStatDump.TableOsReadSec = PerSec(DICTDB._TableStat._TableStat-OsRead - ttTableStat.TableOsRead)
&ENDIF
*/
           .

    ASSIGN
/* Accumulate table stats per area: */
      tt_Area.TableCount  = tt_Area.TableCount  + 1
      tt_Area.TableRead   = tt_Area.TableRead   + DICTDB._TableStat._TableStat-Read
      tt_Area.TableUpdate = tt_Area.TableUpdate + DICTDB._TableStat._TableStat-Update
      tt_Area.TableCreate = tt_Area.TableCreate + DICTDB._TableStat._TableStat-Create
      tt_Area.TableDelete = tt_Area.TableDelete + DICTDB._TableStat._TableStat-Delete
/* Save the current snapshot: */
      ttTableStat.DbHost      = vDbHost
      ttTableStat.DbPath      = vDbPath
      ttTableStat.TableNumber = tt_File.uFile-Number
      ttTableStat.TableName   = tt_File.uFile-Name
      ttTableStat.TableRead   = DICTDB._TableStat._TableStat-Read
      ttTableStat.TableUpdate = DICTDB._TableStat._TableStat-Update
      ttTableStat.TableCreate = DICTDB._TableStat._TableStat-Create
      ttTableStat.TableDelete = DICTDB._TableStat._TableStat-Delete
      &IF DEFINED(Version_GE_102B)
      &THEN
      ttTableStat.TableOsRead = DICTDB._TableStat._TableStat-OsRead
      &ENDIF
    . /* ASSIGN  */
  END. /* FOR EACH _TableStat etc */

/* The list of tables outside TableRangeSize: */
  FOR EACH tt_File NO-LOCK
     WHERE tt_File.uFile-Number LT vMinTableStatId
        OR tt_File.uFile-Number GT vMaxTableStatId,

     FIRST tt_StorageObject NO-LOCK
     WHERE tt_StorageObject.uObject-Type   EQ 1
       AND tt_StorageObject.uObject-Number EQ tt_File.uFile-Number,

     FIRST tt_Area
     WHERE tt_Area.uArea-Number EQ tt_StorageObject.uArea-Number:


    ASSIGN tt_Area.TableCount = tt_Area.TableCount + 1.
  END. /* EACH DICTDB._TableStat */

END PROCEDURE. /* TableStat */

/* -------------------------------------------------------------------- */
PROCEDURE IndexStat.

  FOR FIRST ttDbStat NO-LOCK
      WHERE ttDbStat.DbHost EQ vDbHost
        AND ttDbStat.DbPath EQ vDbPath,

      FIRST DICTDB._ActIndex  NO-LOCK,
      FIRST DICTDB._ActIOType NO-LOCK:

  END. /* FOR FIRST ttDbStat, FIRST _ActIndex */

  FOR EACH DICTDB._IndexStat NO-LOCK,

     FIRST tt_Index NO-LOCK
     WHERE tt_Index.uIdx-Num EQ DICTDB._IndexStat._IndexStat-id,

     FIRST tt_StorageObject NO-LOCK
     WHERE tt_StorageObject.uObject-Type   EQ 2
       AND tt_StorageObject.uObject-Number EQ tt_Index.uIdx-Num,

     FIRST tt_File NO-LOCK
     WHERE tt_File.uFileRecid EQ tt_Index.uFile-Recid,

     FIRST tt_Area NO-LOCK
     WHERE tt_Area.uArea-Number EQ tt_StorageObject.uArea-Number

     WHILE DICTDB._IndexStat._IndexStat-id LE vHighestIndexId
  TRANSACTION:

    FIND FIRST ttIndexStat
         WHERE ttIndexStat.DbHost      EQ vDbHost
           AND ttIndexStat.DbPath      EQ vDbPath
           AND ttIndexStat.IndexNumber EQ tt_Index.uIdx-Num
    NO-ERROR.

    IF NOT AVAILABLE ttIndexStat THEN
    CREATE ttIndexStat.

    IF vIgnorePrevStat THEN
    ASSIGN
      ttIndexStat.IndexRead   = 0
      ttIndexStat.IndexCreate = 0
      ttIndexStat.IndexDelete = 0
      &IF DEFINED(Version_GE_102B)
      &THEN
      ttIndexStat.IndexOsRead = 0
      &ENDIF

    . /* ASSIGN */


    CREATE ttIndexStatDump.
    ASSIGN ttIndexStatDump.DbHost = vDbHost
           ttIndexStatDump.DbNam = LDBNAME("DICTDB":U)
           ttIndexStatDump.AreaName = tt_Area.uArea-Name
           ttIndexStatDump.TableName = tt_File.uFile-Name
           ttIndexStatDump.IndexName = tt_Index.uIndex-Name
           ttIndexStatDump.IndexAttr = tt_Index.IndexAttr
           ttIndexStatDump.RootBlock = tt_StorageObject.uObject-root
           ttIndexStatDump.IndexRead = DICTDB._IndexStat._IndexStat-Read   - ttIndexStat.IndexRead
           ttIndexStatDump.IndexCreate = DICTDB._IndexStat._IndexStat-Create - ttIndexStat.IndexCreate
           ttIndexStatDump.IndexDelete = DICTDB._IndexStat._IndexStat-Delete - ttIndexStat.IndexDelete
&IF DEFINED(Version_GE_102B)
&THEN
           ttIndexStatDump.IndexOsRead = DICTDB._IndexStat._IndexStat-OsRead - ttIndexStat.IndexOsRead
&ENDIF
/*
           ttIndexStatDump.IndexReadSec = PerSec(DICTDB._IndexStat._IndexStat-Read   - ttIndexStat.IndexRead  )
           ttIndexStatDump.IndexCreateSec = PerSec(DICTDB._IndexStat._IndexStat-Create - ttIndexStat.IndexCreate)
           ttIndexStatDump.IndexDeleteSec = PerSec(DICTDB._IndexStat._IndexStat-Delete - ttIndexStat.IndexDelete)
&IF DEFINED(Version_GE_102B)
&THEN
           ttIndexStatDump.IndexOsReadSec = PerSec(DICTDB._IndexStat._IndexStat-OsRead - ttIndexStat.IndexOsRead)
&ENDIF
*/
    .

    ASSIGN
/* Accumulate index stats per area: */
      tt_Area.IndexCount  = tt_Area.IndexCount + 1
      tt_Area.IndexRead   = tt_Area.IndexRead   + DICTDB._IndexStat._IndexStat-Read
      tt_Area.IndexCreate = tt_Area.IndexCreate + DICTDB._IndexStat._IndexStat-Create
      tt_Area.IndexDelete = tt_Area.IndexDelete + DICTDB._IndexStat._IndexStat-Delete
/* Save the current snapshot: */
      ttIndexStat.DbHost      = vDbHost
      ttIndexStat.DbPath      = vDbPath
      ttIndexStat.IndexNumber = tt_Index.uIdx-Num
      ttIndexStat.TableName   = tt_File.uFile-Name
      ttIndexStat.IndexName   = tt_Index.uIndex-Name
      ttIndexStat.IndexRead   = DICTDB._IndexStat._IndexStat-Read
      ttIndexStat.IndexCreate = DICTDB._IndexStat._IndexStat-Create
      ttIndexStat.IndexDelete = DICTDB._IndexStat._IndexStat-Delete
      &IF DEFINED(Version_GE_102B)
      &THEN
      ttIndexStat.IndexOsRead = DICTDB._IndexStat._IndexStat-OsRead
      &ENDIF
    . /* ASSIGN */

  END. /* EACH _IndexStat etc */

/* The list of indexes outside IndexRangeSize: */
  FOR EACH tt_Index NO-LOCK
     WHERE tt_Index.uIdx-Num LT vMinIndexStatId
        OR tt_Index.uIdx-Num GT vMaxIndexStatId,

     FIRST tt_StorageObject NO-LOCK
     WHERE tt_StorageObject.uObject-Type   EQ 2
       AND tt_StorageObject.uObject-Number EQ tt_Index.uIdx-Num,

     FIRST tt_File NO-LOCK
     WHERE tt_File.uFileRecid EQ tt_Index.uFile-Recid,

     FIRST tt_Area NO-LOCK
     WHERE tt_Area.uArea-Number EQ tt_StorageObject.uArea-Number:


    ASSIGN tt_Area.IndexCount = tt_Area.IndexCount + 1.

  END. /* EACH DICTDB._IndexStat */


END PROCEDURE. /* IndexStat */

/* -------------------------------------------------------------------- */
PROCEDURE ResrcStat.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

/* Resources: */
  FOR EACH DICTDB._Resrc NO-LOCK
  TRANSACTION:

    FIND FIRST ttResrcStat EXCLUSIVE
         WHERE ttResrcStat.DbHost    EQ vDbHost
           AND ttResrcStat.DbPath    EQ vDbPath
           AND ttResrcStat.ResrcType EQ "Resource":U
           AND ttResrcStat.ResrcName EQ DICTDB._Resrc._Resrc-Name
    NO-ERROR.

    IF NOT AVAILABLE ttResrcStat THEN
    CREATE ttResrcStat.

    IF vIgnorePrevStat THEN
    ASSIGN
      ttResrcStat.ResrcLock = 0
      ttResrcStat.ResrcWait = 0
    . /* ASSIGN */

    CREATE ttResrcStatDump.
    ASSIGN ttResrcStatDump.DbHost = vDbHost
           ttResrcStatDump.DbNam = LDBNAME("DICTDB":U)
           ttResrcStatDump.ResrcType = "Resource":U
           ttResrcStatDump.ResrcName = DICTDB._Resrc._Resrc-Name
           ttResrcStatDump.ResrcLock = DICTDB._Resrc._Resrc-lock - ttResrcStat.ResrcLock
           ttResrcStatDump.ResrcWait = DICTDB._Resrc._Resrc-wait - ttResrcStat.ResrcWait
           /*ttResrcStatDump.ResrcLockSec = PerSec(DICTDB._Resrc._Resrc-lock - ttResrcStat.ResrcLock)
           ttResrcStatDump.ResrcWaitSec= PerSec(DICTDB._Resrc._Resrc-wait - ttResrcStat.ResrcWait)*/
    .

/* Save the current snapshot: */
    ASSIGN
      ttResrcStat.DbHost    = vDbHost
      ttResrcStat.DbPath    = vDbPath
      ttResrcStat.ResrcType = "Resource":U
      ttResrcStat.ResrcName = DICTDB._Resrc._Resrc-Name
      ttResrcStat.ResrcLock = DICTDB._Resrc._Resrc-lock
      ttResrcStat.ResrcWait = DICTDB._Resrc._Resrc-wait
    . /* ASSIGN */

  END. /* FOR EACH _Resrc */

/* Latches: */
  FOR EACH DICTDB._Latch NO-LOCK
     WHERE DICTDB._Latch._Latch-id NE 1
  TRANSACTION:

    FIND FIRST ttResrcStat EXCLUSIVE
         WHERE ttResrcStat.DbHost    EQ vDbHost
           AND ttResrcStat.DbPath    EQ vDbPath
           AND ttResrcStat.ResrcType EQ "Latch":U
           AND ttResrcStat.ResrcName EQ DICTDB._Latch._Latch-Name
     NO-ERROR.

    IF NOT AVAILABLE ttResrcStat THEN
    CREATE ttResrcStat.

    IF vIgnorePrevStat THEN
    ASSIGN
      ttResrcStat.ResrcLock = 0
      ttResrcStat.ResrcWait = 0
    . /* ASSIGN */

    CREATE ttResrcStatDump.
    ASSIGN ttResrcStatDump.DbHost = vDbHost
           ttResrcStatDump.DbNam = LDBNAME("DICTDB":U)
           ttResrcStatDump.ResrcType = "Latch":U
           ttResrcStatDump.ResrcName = DICTDB._Latch._Latch-Name
           ttResrcStatDump.ResrcLock = DICTDB._Latch._Latch-lock - ttResrcStat.ResrcLock
           ttResrcStatDump.ResrcWait = DICTDB._Latch._Latch-wait - ttResrcStat.ResrcWait
           /*ttResrcStatDump.ResrcLockSec = PerSec(DICTDB._Latch._Latch-lock - ttResrcStat.ResrcLock)
           ttResrcStatDump.ResrcWaitSec= PerSec(DICTDB._Latch._Latch-wait - ttResrcStat.ResrcWait)*/
    .


/* Save the current snapshot: */
    ASSIGN
      ttResrcStat.DbHost    = vDbHost
      ttResrcStat.DbPath    = vDbPath
      ttResrcStat.ResrcType = "Latch":U
      ttResrcStat.ResrcName = DICTDB._Latch._Latch-Name
      ttResrcStat.ResrcLock = DICTDB._Latch._Latch-lock
      ttResrcStat.ResrcWait = DICTDB._Latch._Latch-wait
    . /* ASSIGN */

  END. /* FOR EACH _Latch */

/* TXE Locks: */
  FOR FIRST DICTDB._TxeLock NO-LOCK:
    DO i = 1 TO 9
    TRANSACTION:

      FIND FIRST ttResrcStat EXCLUSIVE
           WHERE ttResrcStat.DbHost    EQ vDbHost
             AND ttResrcStat.DbPath    EQ vDbPath
             AND ttResrcStat.ResrcType EQ "TxeLock":U
             AND ttResrcStat.ResrcName EQ DICTDB._TxeLock._Txe-Type[i]
       NO-ERROR.

      IF NOT AVAILABLE ttResrcStat THEN
      CREATE ttResrcStat.

      IF vIgnorePrevStat THEN
      ASSIGN
        ttResrcStat.ResrcLock = 0
        ttResrcStat.ResrcWait = 0
      . /* ASSIGN */

    CREATE ttResrcStatDump.
    ASSIGN ttResrcStatDump.DbHost = vDbHost
           ttResrcStatDump.DbNam = LDBNAME("DICTDB":U)
           ttResrcStatDump.ResrcType = "TxeLock":U
           ttResrcStatDump.ResrcName = DICTDB._TxeLock._Txe-Type[i]
           ttResrcStatDump.ResrcLock = DICTDB._TxeLock._Txe-Locks[i] - ttResrcStat.ResrcLock
           ttResrcStatDump.ResrcWait = DICTDB._TxeLock._Txe-Waits[i] - ttResrcStat.ResrcWait
           /*ttResrcStatDump.ResrcLockSec = PerSec(DICTDB._TxeLock._Txe-Locks[i] - ttResrcStat.ResrcLock)
           ttResrcStatDump.ResrcWaitSec= PerSec(DICTDB._TxeLock._Txe-Waits[i] - ttResrcStat.ResrcWait)*/
    .

/* Save the current snapshot: */
      ASSIGN
        ttResrcStat.DbHost    = vDbHost
        ttResrcStat.DbPath    = vDbPath
        ttResrcStat.ResrcType = "TxeLock":U
        ttResrcStat.ResrcName = DICTDB._TxeLock._Txe-Type[i]
        ttResrcStat.ResrcLock = DICTDB._TxeLock._Txe-Locks[i]
        ttResrcStat.ResrcWait = DICTDB._TxeLock._Txe-Waits[i]
      . /* ASSIGN */

    END. /* DO i = 1 TO 9 */
  END. /* FOR FIRST _TxeLock */

END PROCEDURE. /* ResrcStat */

/* -------------------------------------------------------------------- */
PROCEDURE CheckPrevStat.
  DEFINE INPUT  PARAMETER ipSkipRate       AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER opIgnorePrevStat AS LOGICAL NO-UNDO.
  DEFINE OUTPUT PARAMETER opIgnoreTblIdx   AS LOGICAL NO-UNDO.
  
  ASSIGN opIgnorePrevStat = TRUE.

  FIND FIRST ttDbStat
       WHERE ttDbStat.DbHost EQ vDbHost
         AND ttDbStat.DbPath EQ vDbPath
  NO-ERROR.

  FOR FIRST DICTDB._ActSummary NO-LOCK:

    IF AVAILABLE ttDbStat THEN
    ASSIGN
      vCurrInterval = (TODAY - Stamp2Date(ttDbStat.SnapshotTimeStamp)) * 86400
                    + (TIME  - Stamp2Time(ttDbStat.SnapshotTimeStamp))
      opIgnorePrevStat = vCurrInterval GT DICTDB._ActSummary._Summary-UpTime
    . /* ASSIGN */
    ELSE
    CREATE ttDbStat.

    IF opIgnorePrevStat THEN
    ASSIGN
      vCurrInterval   = DICTDB._ActSummary._Summary-UpTime

      ttDbStat.DbHost = vDbHost
      ttDbStat.DbPath = vDbPath
      ttDbStat.SnapshotTimeStamp = ""

      ttDbStat.DbAccesses  = 0
      ttDbStat.RecRead     = 0
      ttDbStat.RecUpdate   = 0
      ttDbStat.RecCreate   = 0
      ttDbStat.RecDelete   = 0

      ttDbStat.BytesRead   = 0
      ttDbStat.BytesUpdate = 0
      ttDbStat.BytesCreate = 0
      ttDbStat.BytesDelete = 0

      ttDbStat.IdxRead     = 0
      ttDbStat.IdxCreate   = 0
      ttDbStat.IdxDelete   = 0

      ttDbStat.DbReads     = 0
      ttDbStat.DbWrites    = 0
      ttDbStat.DatDbReads  = 0
      ttDbStat.DatDbWrites = 0
      ttDbStat.IdxDbReads  = 0
      ttDbStat.IdxDbWrites = 0

      ttDbStat.BiWrites    = 0
      ttDbStat.BiBytesWrtn = 0
      ttDbStat.BiNotesWrtn = 0
      ttDbStat.TransComm   = 0

      ttDbStat.LatchLock   = 0
      ttDbStat.LatchWait   = 0
      ttDbStat.ResrcLock   = 0
      ttDbStat.ResrcWait   = 0
      ttDbStat.SemWaits    = 0
      ttDbStat.TxeLock     = 0
      ttDbStat.TxeWait     = 0
      ttDbStat.Checkpoints = 0
      ttDbStat.Flushed     = 0.
    . /* ASSIGN */
    ASSIGN opIgnoreTblIdx = (ttDbStat.TblIdxSkip MODULO ipSkipRate) NE 0.
  END. /* FOR FIRST _ActSummary */
END PROCEDURE. /* CheckPrevStat */

/* ***************************  Main Block  *************************** */

ASSIGN
  vDumpPrefix = ipBaseDir + "/DbStatDump." + ipTimeStamp + ".":U
  vDbHost = ipLocalHost
  vConnect = "Local"
. /* ASSIGN */

DO i = 1 TO NUM-ENTRIES( DBPARAM("DICTDB":U) ):
  ASSIGN vParam = ENTRY(i, DBPARAM("DICTDB":U) ).

  IF NOT vParam BEGINS "-H ":U THEN
  NEXT.

  ASSIGN vDbHost = SUBSTRING(vParam, 4)
         vConnect = "Remote".
  LEAVE.
END.

FOR FIRST DICTDB._FileList NO-LOCK
    WHERE DICTDB._FileList._FileList-Name MATCHES "*.db":U:

  ASSIGN vDbPath = SUBSTRING(DICTDB._FileList._FileList-Name, 1,
                      LENGTH(DICTDB._FileList._FileList-Name) - 3) /*Cut .db*/
  . /* ASSIGN */
END. /* FOR FIRST _FileList */

FOR LAST DICTDB._TableStat NO-LOCK:
  ASSIGN
    vTableRangeSize = RECID(DICTDB._TableStat)
    vMaxTableStatId = DICTDB._TableStat._TableStat-Id
    vMinTableStatId = vMaxTableStatId - vTableRangeSize + 1
  . /* ASSIGN */
END.

FOR LAST DICTDB._IndexStat NO-LOCK:
  ASSIGN
    vIndexRangeSize = RECID(DICTDB._IndexStat)
    vMaxIndexStatId = DICTDB._IndexStat._IndexStat-Id
    vMinIndexStatId = vMaxIndexStatId - vIndexRangeSize + 1
  . /* ASSIGN */
END.

DEFINE VARIABLE vETime AS INTEGER NO-UNDO.
ETIME(TRUE).

DEFINE VARIABLE vErrorsLogFile AS CHARACTER NO-UNDO.
ASSIGN vErrorsLogFile = ipBaseDir + "/DbStatDump.":U + ipTimeStamp + ".Errors.log":U.

OUTPUT TO VALUE(vErrorsLogFile) APPEND KEEP-MESSAGES.

RUN ReadSchema(OUTPUT vHighestTableId,
               OUTPUT vHighestIndexId,
               OUTPUT vDbTableCount,
               OUTPUT vDbIndexCount).

IF (ipSkipRate EQ 0) THEN ipSkipRate = 1.
RUN CheckPrevStat(INPUT ipSkipRate, OUTPUT vIgnorePrevStat, OUTPUT vIgnoreTblIdx).

IF (NOT vIgnoreTblIdx) THEN DO:
RUN IndexStat.  /* set tt_Area.IndexCount
                       tt_Area.IndexRead
                       tt_Area.IndexCreate
                       tt_Area.IndexDelete */

RUN TableStat.  /* set tt_Area.TableCount
                       tt_Area.TableRead
                       tt_Area.TableUpdate
                       tt_Area.TableCreate
                       tt_Area.TableDelete */
END.
RUN AreaStat.   /* set vDbSize */
RUN ResrcStat.
RUN DbStat.

OUTPUT CLOSE.

FILE-INFO:FILE-NAME = vErrorsLogFile.
IF  FILE-INFO:FULL-PATHNAME NE ?
AND FILE-INFO:FILE-SIZE     EQ 0 THEN
OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

