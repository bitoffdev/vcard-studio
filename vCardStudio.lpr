program vCardStudio;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UFormMain, UCore, Common, CoolTranslator, UDataFile,
  SysUtils, UFormContacts, UFormContact, UFormFindDuplicity, UFormGenerate
  { you can add units after this };

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

