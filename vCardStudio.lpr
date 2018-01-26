program vCardStudio;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UFormMain, UCore, Common, CoolTranslator, UDataFile,
  TemplateGenerics, SysUtils, UFormContacts, UFormContact
  { you can add units after this };

{$R *.res}

{$IFDEF DEBUG}
const
  HeapTraceLog = 'heaptrclog.trc';
{$ENDIF}

begin
  Application.Title := 'vCard Studio';
  {$IFDEF DEBUG}
  // Heap trace
  DeleteFile(ExtractFilePath(ParamStr(0)) + HeapTraceLog);
  SetHeapTraceOutput(ExtractFilePath(ParamStr(0)) + HeapTraceLog);
  {$ENDIF}
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TCore, Core);
  Application.CreateForm(TFormContacts, FormContacts);
  Application.Run;
end.

