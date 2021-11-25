unit UContact;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, fgl, Dialogs, UDataFile, LazUTF8, Base64;

type
  TContactsFile = class;

  TErrorEvent = procedure (Text: string; Line: Integer) of object;

  TDataType = (dtString, dtInteger, dtDate, dtDateTime, dtImage);

  TContactFieldIndex = (cfFirstName, cfMiddleName, cfLastName, cfTitleBefore,
    cfTitleAfter, cfFullName, cfTelCell, cfTelHome, cfTelHome2, cfTelWork, cfTelVoip,
    cfTelMain, cfEmail,
    cfEmailHome, cfEmailInternet, cfNickName, cfNote, cfRole, cfTitle,
    cfCategories, cfOrganization, cfAdrHome, cfHomeAddressStreet,
    cfHomeAddressCity, cfHomeAddressCountry, cfXTimesContacted,
    cfXLastTimeContacted, cfPhoto, cfXJabber, cfDayOfBirth, cfRevision,
    cfVersion);

  TContactField = class
    SysName: string;
    Groups: TStringArray;
    Title: string;
    Index: TContactFieldIndex;
    ValueIndex: Integer;
    DataType: TDataType;
  end;

  { TContactFields }

  TContactFields = class(TFPGObjectList<TContactField>)
    function AddNew(Name: string; Groups: TStringArray; Title: string; Index: TContactFieldIndex; DataType:
      TDataType; ValueIndex: Integer = -1): TContactField;
    function GetByIndex(Index: TContactFieldIndex): TContactField;
    procedure LoadToStrings(AItems: TStrings);
  end;

  { TContactProperty }

  TContactProperty = class
    Name: string;
    Attributes: TStringList;
    Values: TStringList;
    Encoding: string;
    Charset: string;
    procedure EvaluateAttributes;
    function GetDecodedValue: string;
    function MatchNameGroups(AName: string; Groups: TStringArray): Boolean;
    procedure Assign(Source: TContactProperty);
    constructor Create;
    destructor Destroy; override;
  end;

  { TContactProperties }

  TContactProperties = class(TFPGObjectList<TContactProperty>)
    function GetByName(Name: string): TContactProperty;
    function GetByNameGroups(Name: string; Groups: TStringArray): TContactProperty;
    function GetByNameGroupsMultiple(Name: string; Groups: TStringArray): TContactProperties;
  end;

  { TContact }

  TContact = class
  private
    function GetField(Index: TContactFieldIndex): string;
    procedure SetField(Index: TContactFieldIndex; AValue: string);
  public
    Properties: TContactProperties;
    Parent: TContactsFile;
    function GetProperty(Index: TContactFieldIndex): TContactProperty;
    procedure Assign(Source: TContact);
    function UpdateFrom(Source: TContact): Boolean;
    constructor Create;
    destructor Destroy; override;
    property Fields[Index: TContactFieldIndex]: string read GetField write SetField;
  end;

  { TContacts }

  TContacts = class(TFPGObjectList<TContact>)
    ContactsFile: TContactsFile;
    procedure AssignToList(List: TFPGObjectList<TObject>);
    function AddNew: TContact;
    function Search(FullName: string): TContact;
    function ToString: ansistring; override;
  end;

  { TContactsFile }

  TContactsFile = class(TDataFile)
  private
    FOnError: TErrorEvent;
    procedure InitFields;
    procedure Error(Text: string; Line: Integer);
    function NewItem(Key, Value: string): string;
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
    property OnError: TErrorEvent read FOnError write FOnError;
  end;


implementation

resourcestring
  SVCardFile = 'vCard file';
  SUnsupportedContactFieldsIndex = 'Unsupported contact field index';
  SUnknownCommand = 'Unknown command: %s';
  SFoundPropertiesBeforeBlockStart = 'Found properties before the start of block';
  SFoundBlockEndWithoutBlockStart = 'Found block end without block start';
  SFieldIndexNotDefined = 'Field index not defined';

function GetNext(var Text: string; Separator: string): string;
begin
  if Pos(Separator, Text) > 0 then begin
    Result := Copy(Text, 1, Pos(Separator, Text) - 1);
    Delete(Text, 1, Length(Result) + Length(Separator));
  end else begin
    Result := Text;
    Text := '';
  end;
end;

function IsAsciiString(Text: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to Length(Text) do
    if Ord(Text[I]) > 128 then begin
      Result := False;
      Break;
    end;
end;

{ TContactProperties }

function TContactProperties.GetByName(Name: string): TContactProperty;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and (Items[I].Name <> Name) do Inc(I);
  if I < Count then Result := Items[I]
    else Result := nil;
end;

function TContactProperties.GetByNameGroups(Name: string; Groups: TStringArray
  ): TContactProperty;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and not Items[I].MatchNameGroups(Name, Groups) do Inc(I);
  if I < Count then Result := Items[I]
    else Result := nil;
end;

function TContactProperties.GetByNameGroupsMultiple(Name: string;
  Groups: TStringArray): TContactProperties;
var
  I: Integer;
begin
  Result := TContactProperties.Create(False);
  for I := 0 to Count - 1 do
  if Items[I].MatchNameGroups(Name, Groups) then
    Result.Add(Items[I]);
end;

{ TContactProperty }

procedure TContactProperty.EvaluateAttributes;
begin
  if Attributes.IndexOf('BASE64') <> -1 then
    Encoding := 'BASE64'
  else
  if Attributes.IndexOfName('ENCODING') <> -1 then
    Encoding := Attributes.Values['ENCODING']
    else Encoding := '';

  if Attributes.IndexOfName('CHARSET') <> -1 then
    Charset := Attributes.Values['CHARSET']
    else Charset := '';
end;

function TContactProperty.GetDecodedValue: string;
begin
  if Encoding = 'BASE64' then
    Result := DecodeStringBase64(Values.DelimitedText)
  else
  if Encoding = 'QUOTED-PRINTABLE' then
    Result := Values.DelimitedText
  else Result := '';
end;

function TContactProperty.MatchNameGroups(AName: string; Groups: TStringArray
  ): Boolean;
var
  I: Integer;
begin
  Result := Name = AName;
  if Result then begin
    for I := 0 to Length(Groups) - 1 do
      if Attributes.IndexOf(Groups[I]) = -1 then begin
        Result := False;
        Break;
      end;
  end;
end;

procedure TContactProperty.Assign(Source: TContactProperty);
begin
  Name := Source.Name;
  Attributes.Assign(Source.Attributes);
  Values.Assign(Source.Values);
end;

constructor TContactProperty.Create;
begin
  Attributes := TStringList.Create;
  Attributes.Delimiter := ';';
  Attributes.NameValueSeparator := '=';
  Attributes.StrictDelimiter := True;
  Values := TStringList.Create;
  Values.Delimiter := ';';
  Values.NameValueSeparator := '=';
  Values.StrictDelimiter := True;
end;

destructor TContactProperty.Destroy;
begin
  FreeAndNil(Values);
  FreeAndNil(Attributes);
  inherited;
end;

{ TContacts }

procedure TContacts.AssignToList(List: TFPGObjectList<TObject>);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(Items[I]);
end;

function TContacts.AddNew: TContact;
begin
  Result := TContact.Create;
  Result.Parent := ContactsFile;
  Add(Result);
end;

function TContacts.Search(FullName: string): TContact;
var
  Contact: TContact;
begin
  Result := nil;
  for Contact in Self do
    if Contact.Fields[cfFullName] = FullName then begin
      Result := Contact;
      Break;
    end;
end;

function TContacts.ToString: ansistring;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do begin
    if I > 0 then Result := Result + ', ';
    Result := Result + Items[I].Fields[cfFullName];
  end;
end;

{ TContactFields }

function TContactFields.AddNew(Name: string; Groups: TStringArray; Title: string; Index: TContactFieldIndex;
  DataType: TDataType; ValueIndex: Integer = -1): TContactField;
begin
  Result := TContactField.Create;
  Result.SysName := Name;
  Result.Groups := Groups;
  Result.Title := Title;
  Result.Index := Index;
  Result.ValueIndex := ValueIndex;
  Result.DataType := DataType;
  Add(Result);
end;

function TContactFields.GetByIndex(Index: TContactFieldIndex): TContactField;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and (Items[I].Index <> Index) do Inc(I);
  if I < Count then Result := Items[I]
    else Result := nil;
end;

procedure TContactFields.LoadToStrings(AItems: TStrings);
var
  I: Integer;
begin
  while AItems.Count < Count do AItems.Add('');
  while AItems.Count > Count do AItems.Delete(AItems.Count - 1);
  for I := 0 to Count - 1 do
    AItems[I] := Items[I].Title;
end;

{ TContact }

function TContact.GetField(Index: TContactFieldIndex): string;
var
  Prop: TContactProperty;
  Field: TContactField;
begin
  Prop := GetProperty(Index);
  if Assigned(Prop) then begin
    Field := Parent.Fields.GetByIndex(Index);
    if Field.ValueIndex <> -1 then begin
      if Field.ValueIndex < Prop.Values.Count then
        Result := Prop.Values.Strings[Field.ValueIndex]
        else Result := '';
    end else Result := Prop.Values.DelimitedText;
  end else Result := '';
end;

procedure TContact.SetField(Index: TContactFieldIndex; AValue: string);
var
  Prop: TContactProperty;
  Field: TContactField;
  I: Integer;
begin
  Field := Parent.Fields.GetByIndex(Index);
  if Assigned(Field) then begin
    Prop := Properties.GetByNameGroups(Field.SysName, Field.Groups);
    if not Assigned(Prop) then begin
      Prop := TContactProperty.Create;
      Prop.Name := Field.SysName;
      for I := 0 to Length(Field.Groups) - 1 do
        Prop.Attributes.Add(Field.Groups[I]);
      Properties.Add(Prop);
    end;
    if Field.ValueIndex <> -1 then begin
      while Prop.Values.Count <= Field.ValueIndex do Prop.Values.Add('');
      Prop.Values.Strings[Field.ValueIndex] := AValue
    end else Prop.Values.DelimitedText := AValue;
  end else raise Exception.Create(SFieldIndexNotDefined);
end;

function TContact.GetProperty(Index: TContactFieldIndex): TContactProperty;
var
  Prop: TContactProperty;
  Field: TContactField;
begin
  Field := Parent.Fields.GetByIndex(Index);
  if Assigned(Field) then begin
    Result := Properties.GetByNameGroups(Field.SysName, Field.Groups);
  end else raise Exception.Create(SFieldIndexNotDefined);
end;

procedure TContact.Assign(Source: TContact);
begin
  Properties.Assign(Source.Properties);
end;

function TContact.UpdateFrom(Source: TContact): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Parent.Fields.Count - 1 do begin
    if (Source.Fields[Parent.Fields[I].Index] <> '') and
      (Source.Fields[Parent.Fields[I].Index] <>
      Fields[Parent.Fields[I].Index]) then begin
        Result := True;
        Fields[Parent.Fields[I].Index] := Source.Fields[Parent.Fields[I].Index];
      end;
  end;
end;

constructor TContact.Create;
begin
  Properties := TContactProperties.Create;
end;

destructor TContact.Destroy;
begin
  FreeAndNil(Properties);
  inherited;
end;

{ TContactsFile }

procedure TContactsFile.InitFields;
begin
  with Fields do begin
    AddNew('N', [], 'Last Name', cfLastName, dtString, 0);
    AddNew('N', [], 'First Name', cfFirstName, dtString, 1);
    AddNew('N', [], 'Middle Name', cfMiddleName, dtString, 2);
    AddNew('N', [], 'Title Before', cfTitleBefore, dtString, 3);
    AddNew('N', [], 'Title After', cfTitleAfter, dtString, 4);
    AddNew('FN', [], 'Full Name', cfFullName, dtString);
    AddNew('TEL', ['CELL'], 'Cell phone', cfTelCell, dtString);
    AddNew('TEL', ['HOME'], 'Home phone', cfTelHome, dtString);
    AddNew('TEL', ['HOME2'], 'Home phone 2', cfTelHome2, dtString);
    AddNew('TEL', ['WORK'], 'Home work', cfTelWork, dtString);
    AddNew('TEL', ['VOIP'], 'Tel VoIP', cfTelVoip, dtString);
    AddNew('TEL', ['MAIN'], 'Tel Main', cfTelMain, dtString);
    AddNew('EMAIL', [], 'Email', cfEmail, dtString);
    AddNew('EMAIL', ['HOME'], 'Email Home', cfEmailHome, dtString);
    AddNew('EMAIL', ['INTERNET'], 'Email Internet', cfEmailInternet, dtString);
    AddNew('X-NICKNAME', [], 'Nick Name', cfNickName, dtString);
    AddNew('NOTE', [], 'Note', cfNote, dtString);
    AddNew('ROLE', [], 'Role', cfRole, dtString);
    AddNew('TITLE', [], 'Title', cfTitle, dtString);
    AddNew('CATEGORIES', [], 'Categories', cfCategories, dtString);
    AddNew('ORG', [], 'Organization', cfOrganization, dtString, 0);
    AddNew('ORG', [], 'Division', cfOrganization, dtString, 1);
    AddNew('ADR', ['HOME'], 'Home Address', cfAdrHome, dtString);
    AddNew('ADR', ['HOME'], 'Home Address Street', cfHomeAddressStreet, dtString, 1);
    AddNew('ADR', ['HOME'], 'Home Address City', cfHomeAddressCity, dtString, 2);
    AddNew('ADR', ['HOME'], 'Home Address Country', cfHomeAddressCountry, dtString, 3);
    AddNew('X-TIMES_CONTACTED', [], 'Times Contacted', cfXTimesContacted, dtString);
    AddNew('X-LAST_TIME_CONTACTED', [], 'Last Time Contacted', cfXLastTimeContacted, dtString);
    AddNew('PHOTO', [], 'Photo', cfPhoto, dtString);
    AddNew('X-JABBER', [], 'Jabber', cfXJabber, dtString);
    AddNew('BDAY', [], 'Day of birth', cfDayOfBirth, dtString);
    AddNew('REV', [], 'Revision', cfRevision, dtString);
  end;
end;

procedure TContactsFile.Error(Text: string; Line: Integer);
begin
  if Assigned(FOnError) then FOnError(Text, Line);
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

function TContactsFile.NewItem(Key, Value: string): string;
var
  Charset: string;
begin
  if not IsAsciiString(Value) then Charset := ';CHARSET=UTF-8'
    else Charset := '';
  Result := Key + Charset + ':' + Value;
end;

procedure TContactsFile.SaveToFile(FileName: string);
var
  Output: TStringList;
  I: Integer;
  J: Integer;
  Value: string;
  NameText: string;
begin
  inherited;
  try
    Output := TStringList.Create;
    for I := 0 to Contacts.Count - 1 do
    with Contacts[I], Output do begin
      Add('BEGIN:VCARD');
      for J := 0 to Properties.Count - 1 do
      with Properties[J] do begin
        Value := Values.DelimitedText;
        if Pos(LineEnding, Value) > 0 then begin
          NameText := Name;
          if Attributes.Count > 0 then
            NameText := NameText + ';' + Attributes.DelimitedText;
          Add(NameText + ':' + GetNext(Value, LineEnding));
          while Pos(LineEnding, Value) > 0 do begin
            Add(' ' + GetNext(Value, LineEnding));
          end;
          Add(' ' + GetNext(Value, LineEnding));
          Add('');
        end else begin
          NameText := Name;
          if Attributes.Count > 0 then
            NameText := NameText + ';' + Attributes.DelimitedText;
          Add(NameText + ':' + Value);
        end;
      end;
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
  Value: string;
  I: Integer;
  NewRecord: TContact;
  NewProperty: TContactProperty;
  CommandPart: string;
  Names: string;
begin
  inherited;
  NewRecord := nil;
  Contacts.Clear;
  Lines := TStringList.Create;
  Lines.LoadFromFile(FileName);
  try
    I := 0;
    while I < Lines.Count do begin
      Line := Lines[I];
      if Line = '' then
      else
      if Line = 'BEGIN:VCARD' then begin
        NewRecord := TContact.Create;
        NewRecord.Parent := Self;
      end else
      if Line = 'END:VCARD' then begin
        if Assigned(NewRecord) then begin
          Contacts.Add(NewRecord);
          NewRecord := nil;
        end else Error(SFoundBlockEndWithoutBlockStart, I + 1);
      end else
      if Pos(':', Line) > 0 then begin
        CommandPart := GetNext(Line, ':');
        if Assigned(NewRecord) then begin
          Names := CommandPart;
          Value := Line;
          while True do begin
            Inc(I);
            if (Length(Lines[I]) > 0) and (Lines[I][1] = ' ') then begin
              Value := Value + Trim(Lines[I]);
            end else begin
              Dec(I);
              Break;
            end;
          end;
          NewProperty := NewRecord.Properties.GetByName(Names);
          if not Assigned(NewProperty) then begin
            NewProperty := TContactProperty.Create;
            NewRecord.Properties.Add(NewProperty);
          end;
          NewProperty.Attributes.DelimitedText := Names;
          if NewProperty.Attributes.Count > 0 then begin
            NewProperty.Name := NewProperty.Attributes[0];
            NewProperty.Attributes.Delete(0);
          end;
          NewProperty.Values.DelimitedText := Value;
          NewProperty.EvaluateAttributes;
        end else Error(SFoundPropertiesBeforeBlockStart, I + 1);
      end;
      Inc(I);
    end;
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

