unit UDataFile;

{$mode delphi}

interface

uses
  Classes, SysUtils, fgl;

type
  { TDataFile }

  TDataFile = class
  private
    FFileName: string;
    FModified: Boolean;
    FOnModify: TNotifyEvent;
    procedure SetFileName(AValue: string);
    procedure SetModified(AValue: Boolean);
    procedure DoOnModify;
  public
    function GetFileExt: string; virtual;
    function GetFileName: string; virtual;
    function GetFileFilter: string; virtual;
    procedure Assign(Source: TDataFile);
    procedure LoadFromFile(FileName: string); virtual;
    procedure SaveToFile(FileName: string); virtual;
    constructor Create; virtual;
    property FileName: string read FFileName write SetFileName;
    property Modified: Boolean read FModified write SetModified;
    property OnModify: TNotifyEvent read FOnModify write FOnModify;
  end;

  TDataFileClass = class of TDataFile;

  TDataFiles = class(TFPGObjectList<TDataFile>)
  end;


implementation

resourcestring
  SDataFileName = 'File';
  SAllFiles = 'All files';

{ TDataFile }

procedure TDataFile.SetModified(AValue: Boolean);
begin
  if FModified = AValue then Exit;
  FModified := AValue;
  DoOnModify;
end;

procedure TDataFile.DoOnModify;
begin
  if Assigned(FOnModify) then FOnModify(Self);
end;

function TDataFile.GetFileExt: string;
begin
  Result := '.dat';
end;

function TDataFile.GetFileName: string;
begin
  Result := SDataFileName;
end;

function TDataFile.GetFileFilter: string;
begin
  Result := SAllFiles + '|*.*';
end;

procedure TDataFile.Assign(Source: TDataFile);
begin
  FFileName := Source.FFileName;
  FModified := Source.FModified;
end;

procedure TDataFile.LoadFromFile(FileName: string);
begin
  FModified := False;
  Self.FileName := FileName;
end;

procedure TDataFile.SaveToFile(FileName: string);
begin
  Self.FileName := FileName;
  FModified := False;
end;

constructor TDataFile.Create;
begin
  FileName := GetFileName + GetFileExt;
end;

procedure TDataFile.SetFileName(AValue: string);
begin
  if FFileName = AValue then Exit;
  FFileName := AValue;
end;


end.

