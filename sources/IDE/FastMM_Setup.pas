unit FastMM_Setup;

{$IFOPT D+}
{$DEFINE FASTMM_DEBUG}
{$ELSE}
{.$DEFINE FASTMM_DEBUG} // <-- uncomment for release-mode tests
{$ENDIF}

interface

procedure SetupFastMM;

implementation

uses
  FastMM5;

procedure SetupFastMM;
begin
  {$IFDEF FASTMM_DEBUG}
  var
  All_FastMM_Events :TFastMM_MemoryManagerEventTypeSet :=[
    {Another third party memory manager has already been installed.}
    mmetAnotherThirdPartyMemoryManagerAlreadyInstalled,
    {FastMM cannot be installed, because memory has already been allocated through the default memory manager.}
    mmetCannotInstallAfterDefaultMemoryManagerHasBeenUsed,
    {When an attempt is made to install or use a shared memory manager, but the memory manager has already been used to
    allocate memory.}
    mmetCannotSwitchToSharedMemoryManagerWithLivePointers,
    {Details about an individual memory leak.}
    mmetUnexpectedMemoryLeakDetail,
    {Summary of memory leaks}
    mmetUnexpectedMemoryLeakSummary,
    {When an attempt to free or reallocate a debug block that has already been freed is detected.}
    mmetDebugBlockDoubleFree,
    mmetDebugBlockReallocOfFreedBlock,
    {When a corruption of the memory pool is detected.}
    mmetDebugBlockHeaderCorruption,
    mmetDebugBlockFooterCorruption,
    mmetDebugBlockModifiedAfterFree,
    {When a virtual method is called on a freed object.}
    mmetVirtualMethodCallOnFreedObject];

  FastMM_SetDebugModeStackTraceEntryCount(16);

  {
  FastMM_LogToFileEvents:=FastMM_LogToFileEvents+
               [
                 mmetUnexpectedMemoryLeakSummary,
                 mmetUnexpectedMemoryLeakDetail
               ];
  }

  FastMM_LogToFileEvents:=All_FastMM_Events;
//  FastMM_OutputDebugStringEvents:=All_FastMM_Events;
//  FastMM_MessageBoxEvents:=All_FastMM_Events;

  // Extremely slow:
  //FastMM_DebugMode_ScanForCorruptionBeforeEveryOperation:=True;


  FastMM_EnterDebugMode;

  {$ELSE}

  FastMM_LogToFileEvents:=[];
  FastMM_OutputDebugStringEvents:=[];
  FastMM_MessageBoxEvents:=[];

  FastMM_SetOptimizationStrategy(mmosOptimizeForSpeed);

// ??? AV if we dont do this:
  FastMM_EnterDebugMode;  // <<--- TODO: REMOVE THIS LINE HERE !!!

  {$ENDIF}
end;

end.
