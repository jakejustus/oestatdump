/*------------------------------------------------------------------------
    File        : DbStatDump.p
    Purpose     : Dump _TableStat/_IndexStat for all connected dbs.

    Syntax      :

    Description :

    Author(s)   : George Potemkin
    Created     : May 23, 2010
    Modified    : Jul 01, 2010
    Notes       : Use DbStatDump2.p (should be in PROPATH).
  ----------------------------------------------------------------------*/
/*PROPATH = PROPATH + ",D:\Proapps\WRK\DbStatDump".*/

/* Full path to the file that stores the last snapshot: */
DEFINE VARIABLE vLastSnapshot AS CHARACTER   NO-UNDO
  INITIAL "./DbStatDump.LastSnapshot.d":U.

DEFINE VARIABLE vErrorsLogFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE vLastSnapshotDir AS CHARACTER NO-UNDO.

DEFINE STREAM DumpFile.
DEFINE STREAM JsonFile.
DEFINE STREAM Influx.

/* Define the shared temp-tables to store the most recent stats: */
{DbStatDump.i NEW}

/* *****************************  Functions  ************************** */

FUNCTION GetLocalHostName RETURNS CHARACTER:
  DEFINE VARIABLE vHostName AS CHARACTER NO-UNDO INITIAL 'unknown'.
  DEFINE VARIABLE vTcpName  AS CHARACTER NO-UNDO INITIAL ''.
  DEFINE VARIABLE vLength   AS INTEGER   NO-UNDO INITIAL 100.
  DEFINE VARIABLE vReturn   AS INTEGER   NO-UNDO INITIAL 0.

  IF OPSYS EQ "WIN32" THEN
  DO: /* Call Win32 routine to get host name */
    RUN gethostname(OUTPUT vTcpName,
                    INPUT  vLength,
                    OUTPUT vReturn).
    IF vReturn EQ 0 THEN
    ASSIGN vHostName = ENTRY(1, vTcpName, CHR(0)).
  END.
  ELSE DO:
    /* get UNIX host name */
    INPUT THROUGH uname -n.
    IMPORT vHostName.
    INPUT CLOSE.
  END.

  RETURN vHostName.
END FUNCTION. /* GetLocalHostName */

/* ***************************  Procedures  *************************** */

PROCEDURE gethostname EXTERNAL "wsock32.dll" :
   DEFINE OUTPUT PARAMETER p-Hostname AS CHARACTER.
   DEFINE INPUT  PARAMETER p-Length   AS LONG.
   DEFINE RETURN PARAMETER p-Return   AS LONG.
END PROCEDURE.

/* -------------------------------------------------------------------- */

PROCEDURE LoadDbStat.

  DEFINE VARIABLE vCurrSeek AS INTEGER NO-UNDO.
  DEFINE VARIABLE vPrevSeek AS INTEGER NO-UNDO.

  DEFINE VARIABLE vSlash AS CHARACTER NO-UNDO.
  ASSIGN vSlash = IF OPSYS EQ "Unix":U THEN "/":U ELSE "~\":U.

  IF SESSION:PARAMETER NE "":U THEN
  ASSIGN vLastSnapshot = SESSION:PARAMETER.

  ASSIGN
    vLastSnapshotDir = SUBSTRING(vLastSnapshot, 1,
                     MAX(R-INDEX(vLastSnapshot, "/":U), 
                         R-INDEX(vLastSnapshot, "~\":U)) - 1)
    vLastSnapshot = SUBSTRING(vLastSnapshot, 
                       LENGTH(vLastSnapshotDir) + 1)
  . /* ASSIGN */

  IF vLastSnapshotDir EQ "":U THEN
  ASSIGN vLastSnapshotDir = ".":U.

  IF vLastSnapshot BEGINS "/":U
  OR vLastSnapshot BEGINS "~\":U THEN
  ASSIGN vLastSnapshot = SUBSTRING(vLastSnapshot, 2).

  IF vLastSnapshot EQ "":U THEN
  ASSIGN vLastSnapshot = "DbStatDump.LastSnapshot.d":U.

  ASSIGN FILE-INFO:FILE-NAME = vLastSnapshotDir.
  IF  FILE-INFO:FULL-PATHNAME NE ?
  AND FILE-INFO:FILE-TYPE MATCHES "*D*":U
  THEN ASSIGN vLastSnapshot = FILE-INFO:FULL-PATHNAME + vSlash + vLastSnapshot.
  ELSE 
  DO:
    ASSIGN vLastSnapshot = ".":U + vSlash + vLastSnapshot.
    OUTPUT TO VALUE(vErrorsLogFile) APPEND.
    PUT UNFORMATTED
      "Dir " vLastSnapshotDir " does not exist. "
      "Last snapshot will be stored in " vLastSnapshot SKIP.
    OUTPUT CLOSE.
  END.
  
  ASSIGN vCurrSeek = ?
         vPrevSeek = ?.
  FILE-INFO:FILE-NAME = vLastSnapshot.
  IF  FILE-INFO:FULL-PATHNAME NE ?
  AND FILE-INFO:FILE-TYPE MATCHES "*F*":U
  AND FILE-INFO:FILE-TYPE MATCHES "*R*":U
  THEN DO:
    INPUT STREAM DumpFile FROM VALUE(FILE-INFO:FULL-PATHNAME).

/* Import previous DbStat: */
    REPEAT:
      CREATE ttDbStat.
      IMPORT STREAM DumpFile DELIMITER {&Sep}
             ttDbStat.
    END.
    DELETE   ttDbStat.

/* Import previous AreaStat: */
    IF SEEK(DumpFile) NE ? THEN
    REPEAT:
      CREATE ttAreaStat.
      IMPORT STREAM DumpFile DELIMITER {&Sep}
             ttAreaStat.
    END.
    DELETE   ttAreaStat.

/* Import previous TableStat: */
    IF SEEK(DumpFile) NE ? THEN
    REPEAT:
      CREATE ttTableStat.
      IMPORT STREAM DumpFile DELIMITER {&Sep}
             ttTableStat.
    END.
    DELETE   ttTableStat.

/* Import previous IndexStat: */
    IF SEEK(DumpFile) NE ? THEN
    REPEAT:
      CREATE ttIndexStat.
      IMPORT STREAM DumpFile DELIMITER {&Sep}
             ttIndexStat.
    END.
    DELETE   ttIndexStat.

/* Import previous ResrcStat: */
    IF SEEK(DumpFile) NE ? THEN
    REPEAT:
      CREATE ttResrcStat.
      IMPORT STREAM DumpFile DELIMITER {&Sep}
             ttResrcStat.
    END.
    DELETE   ttResrcStat.

    ASSIGN vCurrSeek = SEEK(DumpFile).
    IF vCurrSeek NE ? THEN
    DO ON ENDKEY UNDO, LEAVE:
      IMPORT STREAM DumpFile vPrevSeek.
    END.
    INPUT STREAM DumpFile CLOSE.

    IF vCurrSeek NE vPrevSeek THEN
    DO:
      OUTPUT TO VALUE(vErrorsLogFile) APPEND.
      PUT UNFORMATTED
        FILE-INFO:FULL-PATHNAME
        " has wrong size: now " vCurrSeek ", was " vPrevSeek SKIP.
      OUTPUT CLOSE.
    END.
  END. /* If LastSnapshot exists */
END PROCEDURE. /* LoadDbStat */

/* -------------------------------------------------------------------- */

PROCEDURE DumpDbStat.

  DEFINE VARIABLE vCurrSeek AS INTEGER NO-UNDO.

/* Export DbStat: */
  OUTPUT STREAM DumpFile TO VALUE (vLastSnapshot).
  FOR EACH ttDbStat NO-LOCK:
    EXPORT STREAM DumpFile DELIMITER {&Sep}
           ttDbStat.
  END.
  PUT STREAM DumpFile UNFORMATTED ".":U SKIP.

/* Export AreaStat: */
  FOR EACH ttAreaStat NO-LOCK:
    EXPORT STREAM DumpFile DELIMITER {&Sep}
           ttAreaStat.
  END.
  PUT STREAM DumpFile UNFORMATTED ".":U SKIP.

/* Export TableStat: */
  FOR EACH ttTableStat NO-LOCK
     WHERE ttTableStat.TableRead   NE 0
        OR ttTableStat.TableUpdate NE 0
        OR ttTableStat.TableCreate NE 0
        OR ttTableStat.TableDelete NE 0:
    EXPORT STREAM DumpFile DELIMITER {&Sep}
           ttTableStat.
  END.
  PUT STREAM DumpFile UNFORMATTED ".":U SKIP.

/* Export IndexStat: */
  FOR EACH ttIndexStat NO-LOCK
     WHERE ttIndexStat.IndexRead   NE 0
        OR ttIndexStat.IndexCreate NE 0
        OR ttIndexStat.IndexDelete NE 0:
    EXPORT STREAM DumpFile DELIMITER {&Sep}
           ttIndexStat.
  END.
  PUT STREAM DumpFile UNFORMATTED ".":U SKIP.

/* Export ResrcStat: */
  FOR EACH ttResrcStat NO-LOCK
     WHERE ttResrcStat.ResrcLock NE 0
        OR ttResrcStat.ResrcWait NE 0:
    EXPORT STREAM DumpFile DELIMITER {&Sep}
           ttResrcStat.
  END.
  PUT STREAM DumpFile UNFORMATTED ".":U          SKIP.
  ASSIGN vCurrSeek = SEEK(DumpFile).
  PUT STREAM DumpFile UNFORMATTED vCurrSeek SKIP.
  OUTPUT STREAM DumpFile CLOSE.

END PROCEDURE. /* DumpDbStat */


/* ***************************  Main Block  *************************** */

DEFINE VARIABLE vTimeStamp     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vTimeStamp2    AS DATETIME-TZ NO-UNDO.
DEFINE VARIABLE vDbID AS INTEGER NO-UNDO.
DEFINE VARIABLE skipRate AS INTEGER NO-UNDO.

ASSIGN
  vTimeStamp = STRING(YEAR(TODAY))
             + STRING(MONTH(TODAY),"99":U)
             + STRING(DAY(TODAY),"99":U) + "_":U
             + REPLACE(STRING(TIME, "HH:MM:SS":U), ":":U, "":U)
  vTimeStamp2 = NOW.
  vErrorsLogFile = vLastSnapshotDir + "/DbStatDump.":U + vTimeStamp + ".Errors.log":U
. /* ASSIGN */

RUN LoadDbStat.
ASSIGN skipRate = INTEGER(OS-GETENV("SKIPRATE")).
IF (skipRate EQ ?) OR (skipRate EQ 0) THEN
  ASSIGN skipRate = 1.
/* LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("Skip rate &1", skipRate)). */

REPEAT vDbID = 1 TO NUM-DBS:
  CREATE ALIAS DICTDB FOR DATABASE VALUE(LDBNAME(vDbID)).
  RUN DbStatDump2.p( GetLocalHostName(), vTimeStamp, vLastSnapshotDir, skipRate).
END.

RUN DumpDbStat.
RUN DumpDbStatInflux.

PROCEDURE DumpDbStatJson:

  OUTPUT STREAM JsonFile TO VALUE(vLastSnapshotDir + '/DatabaseStats.' + vTimeStamp + '.json').
  PUT STREAM JsonFile UNFORMATTED '~{~n "request": ~{~n  "timestamp": "' + ISO-DATE(vTimeStamp2) + '",~n  "data": ~{~n   "ttDbStat":' SKIP.
  BUFFER ttDbStatDump:WRITE-JSON('STREAM', 'JsonFile', TRUE, 'utf-8', TRUE, TRUE).
  PUT STREAM JsonFile UNFORMATTED '~n,   "ttTableStat":' SKIP.
  BUFFER ttTableStatDump:WRITE-JSON('STREAM', 'JsonFile', TRUE, 'utf-8', TRUE, TRUE).
  PUT STREAM JsonFile UNFORMATTED '~n,   "ttIndexStat":' SKIP.
  BUFFER ttIndexStatDump:WRITE-JSON('STREAM', 'JsonFile', TRUE, 'utf-8', TRUE, TRUE).
  PUT STREAM JsonFile UNFORMATTED '~n,   "ttAreaStat":' SKIP.
  BUFFER ttAreaStatDump:WRITE-JSON('STREAM', 'JsonFile', TRUE, 'utf-8', TRUE, TRUE).
  PUT STREAM JsonFile UNFORMATTED '~n,   "ttResrcStat":' SKIP.
  BUFFER ttResrcStatDump:WRITE-JSON('STREAM', 'JsonFile', TRUE, 'utf-8', TRUE, TRUE).
  
  PUT STREAM JsonFile UNFORMATTED '~n  ~}~n ~}~n~}' SKIP.
  OUTPUT STREAM JsonFile CLOSE.

END PROCEDURE.

PROCEDURE DumpDbStatInflux:
  DEFINE VARIABLE secTS AS INTEGER NO-UNDO.

  ASSIGN secTS = INTERVAL(vTimeStamp2, DATETIME(1, 1, 1970, 0, 0), 'seconds') - (TIMEZONE(vtimestamp2) * 60) .
  
  OUTPUT STREAM Influx TO VALUE(vLastSnapshotDir + '/TableStats.influx.txt') BINARY.
  FOR EACH ttTableStatDump NO-LOCK:
      PUT STREAM Influx UNFORMATTED SUBSTITUTE("tablestat,hostname=&2,dbname=&3,table=~"&4~" reads=&5i,deletes=&6i,updates=&7i,osreads=&8i &9000000000", "", ttTableStatDump.DbHost, ttTableStatDump.DbNam, ttTableStatDump.tableName, ttTableStatDump.TableRead, ttTableStatDump.TableDelete, ttTableStatDump.TableUpdate, ttTableStatDump.TableOsRead, secTS).
      PUT STREAM Influx UNFORMATTED CHR(10).
  END.
  OUTPUT STREAM Influx CLOSE.
  
  OUTPUT STREAM Influx TO VALUE(vLastSnapshotDir + '/IndexStats.influx.txt') BINARY.
  FOR EACH ttIndexStatDump NO-LOCK:
      PUT STREAM Influx UNFORMATTED SUBSTITUTE("indexstat,hostname=&2,dbname=&3,index=~"&4~" reads=&5i,deletes=&6i,osreads=&8i &9000000000", "", ttIndexStatDump.DbHost, ttIndexStatDump.DbNam, ttIndexStatDump.tableName + '.' + ttIndexStatDump.IndexName, ttIndexStatDump.IndexRead, ttIndexStatDump.IndexDelete, "", ttIndexStatDump.IndexOsRead, secTS).
      PUT STREAM Influx UNFORMATTED CHR(10).
  END.
  OUTPUT STREAM Influx CLOSE.

  OUTPUT STREAM Influx TO VALUE(vLastSnapshotDir + '/ResrcStats.influx.txt') BINARY.
  FOR EACH ttResrcStatDump WHERE ttResrcStatDump.ResrcLock GT 0 OR ttResrcStatDump.ResrcWait GT 0 NO-LOCK:
      PUT STREAM Influx UNFORMATTED SUBSTITUTE("resrcstat,hostname=&2,dbname=&3,type=~"&4~",resource=~"&5~" locks=&6i,waits=&7i &9000000000", "", ttResrcStatDump.DbHost, ttResrcStatDump.DbNam, trim(ttResrcStatDump.resrcType), replace(trim(ttResrcStatDump.resrcName), ' ', '_'), ttResrcStatDump.ResrcLock, ttResrcStatDump.ResrcWait, "", secTS).
      PUT STREAM Influx UNFORMATTED CHR(10).
  END.
  OUTPUT STREAM Influx CLOSE.

  OUTPUT STREAM Influx TO VALUE(vLastSnapshotDir + '/AreaStats.influx.txt') BINARY.
  FOR EACH ttAreaStatDump NO-LOCK:
      PUT STREAM Influx UNFORMATTED SUBSTITUTE("areastat,hostname=&2,dbname=&3,area=~"&4~" reads=&5i,writes=&6i &9000000000", "", ttAreaStatDump.DbHost, ttAreaStatDump.DbNam, replace(ttAreaStatDump.areaName, ' ', '_'), ttAreaStatDump.AreaReads, ttAreaStatDump.AreaWrites, "", "", secTS).
      PUT STREAM Influx UNFORMATTED CHR(10).
  END.
  OUTPUT STREAM Influx CLOSE.

  OUTPUT STREAM Influx TO VALUE(vLastSnapshotDir + '/DbStats.influx.txt') BINARY.
  FOR EACH ttDbStatDump NO-LOCK:
      PUT STREAM Influx UNFORMATTED SUBSTITUTE("dbstat,hostname=&2,dbname=&3 ", "", ttDbStatDump.DbHost, ttDbStatDump.DbNam).
      PUT STREAM Influx UNFORMATTED "b2lru=" + (if ttDbStatDump.B2LRUEnbld then '1' else '0').
      PUT STREAM Influx UNFORMATTED "i,tblrngovfw=" + (if ttDbStatDump.HighestTableId > ttDbStatDump.TableStatBase + ttDbStatDump.TableRangeSize then '1' else '0').
      PUT STREAM Influx UNFORMATTED "i,idxrngovfw=" + (if ttDbStatDump.HighestIndexId > ttDbStatDump.IndexStatBase + ttDbStatDump.IndexRangeSize then '1' else '0').
      PUT STREAM Influx UNFORMATTED "i,recreads=" + STRING(ttDbStatDump.RecRead).
      PUT STREAM Influx UNFORMATTED "i,recupdates=" + STRING(ttDbStatDump.RecUpdate).
      PUT STREAM Influx UNFORMATTED "i,recdeletes=" + STRING(ttDbStatDump.RecDelete).
      PUT STREAM Influx UNFORMATTED "i,b2size=" + STRING(ttDbStatDump.B2Size).
      PUT STREAM Influx UNFORMATTED "i,brokcnt=" + STRING(ttDbStatDump.BrkrCnt).
      PUT STREAM Influx UNFORMATTED "i,srvrcnt=" + STRING(ttDbStatDump.SrvrCnt).
      PUT STREAM Influx UNFORMATTED "i,remcltcnt=" + STRING(ttDbStatDump.RemCltCnt).
      PUT STREAM Influx UNFORMATTED "i,batchcltcnt=" + STRING(ttDbStatDump.BtchCnt).
      PUT STREAM Influx UNFORMATTED "i,selfcltcnt=" + STRING(ttDbStatDump.SelfServCnt).
      PUT STREAM Influx UNFORMATTED "i,lockhwm=" + STRING(ttDbStatDump.MostLocks).
      PUT STREAM Influx UNFORMATTED "i,checkpoints=" + STRING(ttDbStatDump.Checkpoints).
      PUT STREAM Influx UNFORMATTED "i,buf_flushed=" + STRING(ttDbStatDump.BufFlushed).
      PUT STREAM Influx UNFORMATTED "i,trx=" + STRING(ttDbStatDump.TransComm).
      PUT STREAM Influx UNFORMATTED "i,locks=" + STRING(ttDbStatDump.NumLocks).
      PUT STREAM Influx UNFORMATTED "i,b1logreads=" + STRING(ttDbStatDump.B1LogicRds).
      PUT STREAM Influx UNFORMATTED "i,b1logwrites=" + STRING(ttDbStatDump.B1LogicWrts).
      PUT STREAM Influx UNFORMATTED "i,b1osreads=" + STRING(ttDbStatDump.B1OSRds).
      PUT STREAM Influx UNFORMATTED "i,b1oswrites=" + STRING(ttDbStatDump.B1OSWrts).
      PUT STREAM Influx UNFORMATTED "i,b2logreads=" + STRING(ttDbStatDump.B2LogicRds).
      PUT STREAM Influx UNFORMATTED "i,b2logwrites=" + STRING(ttDbStatDump.B2LogicWrts).
      PUT STREAM Influx UNFORMATTED "i,b2osreads=" + STRING(ttDbStatDump.B2OSRds).
      PUT STREAM Influx UNFORMATTED "i,b2oswrites=" + STRING(ttDbStatDump.B2OSWrts).
      PUT STREAM Influx UNFORMATTED "i,aiwwrites=" + STRING(ttDbStatDump.AIWWrites).
      PUT STREAM Influx UNFORMATTED "i,aibusybufw=" + STRING(ttDbStatDump.AIBusyBufW).
      PUT STREAM Influx UNFORMATTED "i,ainobufavail=" + STRING(ttDbStatDump.AINoBufAvail).
      PUT STREAM Influx UNFORMATTED "i,aipartialwrites=" + STRING(ttDbStatDump.AIPrtlWrts).
      PUT STREAM Influx UNFORMATTED "i,aitotwrites=" + STRING(ttDbStatDump.AITotWrts).
      PUT STREAM Influx UNFORMATTED "i,aibytesw=" + STRING(ttDbStatDump.AIBytesWrt).
      PUT STREAM Influx UNFORMATTED "i,biwwrites=" + STRING(ttDbStatDump.BIWWrites).
      PUT STREAM Influx UNFORMATTED "i,bibytesw=" + STRING(ttDbStatDump.BIBytesWrtn).
      PUT STREAM Influx UNFORMATTED "i,bitotwrites=" + STRING(ttDbStatDump.BIWrites).

      PUT STREAM Influx UNFORMATTED "i " + STRING(secTS) + "000000000" + CHR(10).
  END.  
  OUTPUT STREAM Influx CLOSE.
  
END PROCEDURE.
