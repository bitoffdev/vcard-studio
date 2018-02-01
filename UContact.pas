unit UContact;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, Dialogs, UDataFile;

type
  TContactsFile = class;

  TStringEvent = procedure (Text: string) of object;

  TDataType = (dtString, dtInteger, dtDate, dtDateTime, dtImage);

  TContactFieldIndex = (cfFirstName, cfMiddleName, cfLastName, cfTelPrefCell,
    cfTelCell, cfTelHome, cfTelHome2, cfTelWork, cfTitle, cfOrganization,
    cfAddress, cfNote);

  TContactField = class
    Name: string;
    Index: TContactFieldIndex;
    DataType: TDataType;
  end;

  { TContactFields }

  TContactFields = class(TObjectList)
    function AddNew(Name: string; Index: TContactFieldIndex; DataType:
      TDataType): TContactField;
  end;

  { TContact }

  TContact = class
  private
    function GetField(Index: TContactFieldIndex): string;
    procedure SetField(Index: TContactFieldIndex; AValue: string);
  public
    Parent: TContactsFile;
    Version: string;
    FirstName: string;
    MiddleName: string;
    LastName: string;
    TitleBefore: string;
    TitleAfter: string;
    FullName: string;
    TelPrefCell: string;
    TelCell: string;
    TelHome: string;
    TelHome2: string;
    TelWork: string;
    TelVoip: string;
    TelPrefWorkVoice: string;
    TelPrefHomeVoice: string;
    TelHomeVoice: string;
    TelWorkVoice: string;
    EmailHome: string;
    EmailInternet: string;
    NickName: string;
    Note: string;
    Role: string;
    Title: string;
    Categories: string;
    Organization: string;
    AdrHome: string;
    HomeAddressStreet: string;
    HomeAddressCity: string;
    HomeAddressCountry: string;
    XTimesContacted: string;
    XLastTimeContacted: string;
    Photo: string;
    XJabber: string;
    property Fields[Index: TContactFieldIndex]: string read GetField write SetField;
  end;

  TContacts = class(TObjectList)
    ContactsFile: TContactsFile;
  end;

  { TContactsFile }

  TContactsFile = class(TDataFile)
  private
    FOnError: TStringEvent;
    function GetNext(var Text: string; Separator: string): string;
    procedure InitFields;
  public
    Fields: TContactFields;
    Contacts: TContacts;
    function GetFileName: string; override;
    function GetFileExt: string; override;
    function GetFileFilter: string; override;
    procedure SaveToFile(FileName: string); override;
    procedure LoadFromFile(FileName: string); override;
    constructor Create; override;
    destructor Destroy; override;
    property OnError: TStringEvent read FOnError write FOnError;
  end;


implementation

resourcestring
  SVCardFile = 'vCard file';
  SUnknownCommand = 'Unknown command: %s';

{ TContactFields }

function TContactFields.AddNew(Name: string; Index: TContactFieldIndex;
  DataType: TDataType): TContactField;
begin
  Result := TContactField.Create;
  Result.Name := Name;
  Result.Index := Index;
  Result.DataType := DataType;
  Add(Result);
end;

{ TContact }

function TContact.GetField(Index: TContactFieldIndex): string;
begin
  case Index of
    cfFirstName: Result := FirstName;
    cfMiddleName: Result := MiddleName;
    cfLastName: Result := LastName;
    cfTelPrefCell: Result := TelPrefCell;
    cfTelCell: Result := TelCell;
    cfTelHome: Result := TelHome;
    cfTelHome2: Result := TelHome2;
    cfTelWork: Result := TelWork;
    cfTitle: Result := Title;
    cfOrganization: Result := Organization;
    cfAddress: Result := AdrHome;
    cfNote: Result := Note;
  end;
end;

procedure TContact.SetField(Index: TContactFieldIndex; AValue: string);
begin
  case Index of
    cfFirstName: FirstName := AValue;
    cfMiddleName: MiddleName := AValue;
    cfLastName: LastName := AValue;
    cfTelPrefCell: TelPrefCell := AValue;
    cfTelCell: TelCell := AValue;
    cfTelHome: TelHome := AValue;
    cfTelHome2: TelHome2 := AValue;
    cfTelWork: TelWork := AValue;
    cfTitle: Title := AValue;
    cfOrganization: Organization := AValue;
    cfAddress: AdrHome := AValue;
    cfNote: Note := AValue;
  end;
end;

{ TContactsFile }

function TContactsFile.GetNext(var Text: string; Separator: string): string;
begin
  if Pos(Separator, Text) > 0 then begin
    Result := Copy(Text, 1, Pos(Separator, Text) - 1);
    Delete(Text, 1, Length(Result) + Length(Separator));
  end else begin
    Result := Text;
    Text := '';
  end;
end;

procedure TContactsFile.InitFields;
begin
  with Fields do begin
    AddNew('First Name', cfFirstName, dtString);
    AddNew('Middle Name', cfMiddleName, dtString);
    AddNew('Last Name', cfLastName, dtString);
    AddNew('Preferred cell phone', cfTelPrefCell, dtString);
    AddNew('Cell phone', cfTelCell, dtString);
    AddNew('Home phone', cfTelHome, dtString);
    AddNew('Home phone 2', cfTelHome2, dtString);
    AddNew('Home work', cfTelWork, dtString);
    AddNew('Title', cfTitle, dtString);
    AddNew('Organization', cfOrganization, dtString);
    AddNew('Address', cfAddress, dtString);
    AddNew('Note', cfNote, dtString);
  end;
end;

function TContactsFile.GetFileName: string;
begin
  Result := SVCardFile;
end;

function TContactsFile.GetFileExt: string;
begin
  Result := '.vcf';
end;

function TContactsFile.GetFileFilter: string;
begin
  Result := GetFileName + ' (' + GetFileExt + ')|*' + GetFileExt + '|' + inherited;
end;

procedure TContactsFile.SaveToFile(FileName: string);
var
  Output: TStringList;
  I: Integer;
begin
  inherited;
  try
    Output := TStringList.Create;
    for I := 0 to Contacts.Count - 1 do
    with TContact(Contacts[I]), Output do begin
      Add('BEGIN:VCARD');
      if Version <> '' then Add('VERSION:' + Version);
      if (LastName <> '') or (FirstName <> '') or (MiddleName <> '') or (TitleBefore <> '') or (TitleAfter <> '') then
        Add('N:' + LastName + ';' + FirstName + ';' + MiddleName + ';' + TitleBefore + ';' + TitleAfter);
      if FullName <> '' then Add('FN:' + FullName);
      if TelPrefCell <> '' then Add('TEL;PREF;CELL:' + TelPrefCell);
      if TelCell <> '' then Add('TEL;CELL:' + TelCell);
      if TelHome <> '' then Add('TEL;HOME:' + TelHome);
      if TelHome2 <> '' then Add('TEL;HOME2:' + TelHome2);
      if TelWork <> '' then Add('TEL;WORK:' + TelWork);
      if TelVoip <> '' then Add('TEL;VOIP:' + TelVoip);
      if TelPrefWorkVoice <> '' then Add('TEL;PREF;WORK;VOICE:' + TelPrefWorkVoice);
      if TelPrefHomeVoice <> '' then Add('TEL;PREF;HOME;VOICE:' + TelPrefHomeVoice);
      if TelHomeVoice <> '' then Add('TEL;HOME;VOICE:' + TelHomeVoice);
      if TelWorkVoice <> '' then Add('TEL;WORK;VOICE:' + TelWorkVoice);
      if NickName <> '' then Add('X-NICKNAME:' + NickName);
      if XJabber <> '' then Add('X-JABBER:' + XJabber);
      if Note <> '' then Add('NOTE:' + Note);
      if AdrHome <> '' then Add('ADR;HOME:' + AdrHome);
      if EmailHome <> '' then Add('EMAIL;HOME:' + EmailHome);
      if EmailInternet <> '' then Add('EMAIL;INTERNET:' + EmailInternet);
      if Role <> '' then Add('TITLE:' + Role);
      if Categories <> '' then Add('CATEGORIES:' + Categories);
      if Organization <> '' then Add('ORG:' + Organization);
      if XTimesContacted <> '' then Add('X-TIMES_CONTACTED:' + XTimesContacted);
      if XLastTimeContacted <> '' then Add('X-LAST_TIME_CONTACTED:' + XLastTimeContacted);
      if (HomeAddressCity <> '') or (HomeAddressStreet <> '') or
        (HomeAddressCountry <> '') then Add('ADR;HOME:;;' + HomeAddressStreet + ';' + HomeAddressCity + ';;;' + HomeAddressCountry);
      if Photo <> '' then Add('PHOTO;ENCODING=BASE64;JPEG:' + Photo);
      Add('END:VCARD');
    end;
    Output.SaveToFile(FileName);
  finally
    Output.Free;
  end
end;

procedure TContactsFile.LoadFromFile(FileName: string);
var
  Lines: TStringList;
  Line: string;
  I: Integer;
  NewRecord: TContact;
  Command: string;
  CommandPart: string;
  Charset: string;
  Encoding: string;
  CommandItems: TStringList;
begin
  inherited;
  Contacts.Clear;
  Lines := TStringList.Create;
  Lines.LoadFromFile(FileName);
  try
    CommandItems := TStringList.Create;
    CommandItems.Delimiter := ';';
    I := 0;
    while I < Lines.Count do begin
      Line := Lines[I];
      if Line = 'BEGIN:VCARD' then begin
        NewRecord := TContact.Create;
        NewRecord.Parent := Self;
      end else
      if Line = 'END:VCARD' then begin
        Contacts.Add(NewRecord);
        NewRecord := nil;
      end else
      if Pos(':', Line) > 0 then begin
        CommandPart := GetNext(Line, ':');
        CommandItems.DelimitedText := CommandPart;
        if CommandItems.IndexOfName('CHARSET') >= 0 then begin
          Charset := CommandItems.Values['CHARSET'];
          CommandItems.Delete(CommandItems.IndexOfName('CHARSET'));
        end
        else if CommandItems.IndexOfName('ENCODING') >= 0 then begin
          Encoding := CommandItems.Values['ENCODING'];
          CommandItems.Delete(CommandItems.IndexOfName('ENCODING'));
        end;
        Command := CommandItems.DelimitedText;

        if Command = 'FN' then NewRecord.FullName := Line
        else if Command = 'N' then begin
          NewRecord.LastName := GetNext(Line, ';');
          NewRecord.FirstName := GetNext(Line, ';');
          NewRecord.MiddleName := GetNext(Line, ';');
          NewRecord.TitleBefore := GetNext(Line, ';');
          NewRecord.TitleAfter := GetNext(Line, ';');
        end
        else if Command = 'VERSION' then NewRecord.Version := Line
        else if Command = 'TEL;PREF;CELL' then NewRecord.TelPrefCell := Line
        else if Command = 'TEL;CELL' then NewRecord.TelCell := Line
        else if Command = 'TEL;HOME' then NewRecord.TelHome := Line
        else if Command = 'TEL;HOME2' then NewRecord.TelHome2 := Line
        else if Command = 'TEL;WORK' then NewRecord.TelWork := Line
        else if Command = 'TEL;VOIP' then NewRecord.TelVoip := Line
        else if Command = 'TEL;PREF;WORK;VOICE' then NewRecord.TelPrefWorkVoice := Line
        else if Command = 'TEL;PREF;HOME;VOICE' then NewRecord.TelPrefHOMEVoice := Line
        else if Command = 'TEL;HOME;VOICE' then NewRecord.TelHomeVoice := Line
        else if Command = 'TEL;WORK;VOICE' then NewRecord.TelWorkVoice := Line
        else if Command = 'ADR;HOME' then NewRecord.AdrHome := Line
        else if Command = 'X-NICKNAME' then NewRecord.NickName := Line
        else if Command = 'EMAIL;HOME' then NewRecord.EmailHome := Line
        else if Command = 'EMAIL:INTERNET' then NewRecord.EmailInternet := Line
        else if Command = 'NOTE' then NewRecord.Note := Line
        else if Command = 'ORG' then NewRecord.Organization := Line
        else if Command = 'X-JABBER' then NewRecord.XJabber := Line
        else if Command = 'TITLE' then NewRecord.Role := Line
        else if Command = 'X-TIMES_CONTACTED' then NewRecord.XTimesContacted := Line
        else if Command = 'X-LAST_TIME_CONTACTED' then NewRecord.XLastTimeContacted := Line
        else if Command = 'PHOTO' then begin
          NewRecord.Photo := Line;
          repeat
            Inc(I);
            Line := Lines[I];
            if Copy(Line, 1, 1) = ' ' then NewRecord.Photo := NewRecord.Photo + Line;
          until Copy(Line, 1, 1) = '';
        end
        else if Assigned(FOnError) then FOnError('Unknown command: ' + Command);
      end;
      Inc(I);
    end;
    CommandItems.Free;
  finally
    Lines.Free;
  end;
end;

constructor TContactsFile.Create;
begin
  inherited;
  Contacts := TContacts.Create;
  Contacts.ContactsFile := Self;
  Fields := TContactFields.Create;
  InitFields;
end;

destructor TContactsFile.Destroy;
begin
  FreeAndNil(Fields);
  FreeAndNil(Contacts);
  inherited;
end;

end.

