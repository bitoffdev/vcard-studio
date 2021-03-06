program vCardStudio;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, clocale,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UFormMain, UCore, Diff, Common, SysUtils, UFormCompare;

{$R *.res}

{$if declared(UseHeapTrace)}
const
  HeapTraceLog = 'heaptrclog.trc';
{$ENDIF}

begin
  Application.Scaled:=True;
  Application.Title:='vCard Studio';
  {$if declared(UseHeapTrace)}
  DeleteFile(ExtractFilePath(ParamStr(0)) + HeapTraceLog);
  SetHeapTraceOutput(ExtractFilePath(ParamStr(0)) + HeapTraceLog);
  {$ENDIF}
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TCore, Core);
  Application.Run;
end.

