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
    cfTelMain, cfEmail, cfTel, cfUid, cfUrl,
    cfEmailHome, cfEmailInternet, cfNickName, cfNote, cfRole, cfTitle,
    cfCategories, cfOrganization, cfDepartment,
    cfHomeAddress, cfHomeAddressStreet, cfHomeAddressCity, cfHomeAddressCountry,
    cfWorkAddress, cfWorkAddressStreet, cfWorkAddressCity, cfWorkAddressCountry,
    cfXTimesContacted, cfXLastTimeContacted, cfPhoto, cfXJabber, cfDayOfBirth, cfRevision,
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

uses
  UQuotedPrintable;

resourcestring
  SVCardFile = 'vCard file';
  SUnsupportedContactFieldsIndex = 'Unsupported contact field index';
  SUnknownCommand = 'Unknown command: %s';
  SFoundPropertiesBeforeBlockStart = 'Found properties before the start of block';
  SFoundBlockEndWithoutBlockStart = 'Found block end without block start';
  SFieldIndexNotDefined = 'Field index not defined';
  SLastName = 'Last Name';
  SFirstName = 'First Name';
  SMiddleName = 'Middle Name';
  STitleBefore = 'Title Before';
  STitleAfter = 'Title After';
  SFullName = 'Full Name';
  STelephone = 'Telephone';
  SCellPhone = 'Cell phone';
  SHomePhone = 'Home phone';
  SHomePhone2 = 'Home phone 2';
  SWorkPhone = 'Work phone';
  SVoipPhone = 'VoIP phone';
  SMainPhone = 'Main phone';
  SEmail = 'E-mail';
  SHomeEmail = 'Home Email';
  SInternetEmail = 'Internet Email';
  SNickName = 'Nick Name';
  SNote = 'Note';
  SRole = 'Role';
  STitle = 'Title';
  SCategories = 'Categories';
  SOrganization = 'Organization';
  SDepartement = 'Departement';
  SHomeAddress = 'Home Address';
  SHomeAddressStreet = 'Home Address Street';
  SHomeAddressCity = 'Home Address City';
  SHomeAddressCountry = 'Home Address Country';
  SWorkAddress = 'Home Address';
  SWorkAddressStreet = 'Work Address Street';
  SWorkAddressCity = 'Work Address City';
  SWorkAddressCountry = 'Work Address Country';
  STimesContacted = 'Times Contacted';
  SLastTimeContacted = 'Last Time Contacted';
  SPhoto = 'Photo';
  SJabber = 'Jabber';
  SDayOfBirth = 'Day of birth';
  SRevision = 'Revision';
  SUniqueIdentifier = 'Unique identifier';
  SWebAddress = 'Web address';

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
var
  I: Integer;
begin
  if Attributes.IndexOf('BASE64') <> -1 then
    Encoding := 'BASE64'
  else
  if Attributes.IndexOfName('ENCODING') <> -1 then begin
    Encoding := Attributes.Values['ENCODING'];
    if (Encoding = 'QUOTED-PRINTABLE') or (Encoding = 'BASE64') then begin
      Values.DelimitedText := GetDecodedValue;
      Attributes.Delete(Attributes.IndexOfName('ENCODING'));
    end;
  end else Encoding := '';

  if Attributes.IndexOfName('CHARSET') <> -1 then
    Charset := Attributes.Values['CHARSET']
    else Charset := '';

  // Simplify TYPE attribute from TYPE=VALUE into VALUE
  for I := 0 to Attributes.Count - 1 do begin
    if Attributes.Names[I] = 'TYPE' then
      Attributes.Strings[I] := Attributes.Values['TYPE'];
  end;
end;

function TContactProperty.GetDecodedValue: string;
begin
  if Encoding = 'BASE64' then
    Result := DecodeStringBase64(Values.DelimitedText)
  else
  if Encoding = 'QUOTED-PRINTABLE' then
    Result := DecodeQuotedPrintable(Values.DelimitedText)
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
  while List.Count > Count do List.Delete(List.Count - 1);
  while List.Count < Count do List.Add(nil);
  for I := 0 to Count - 1 do
    List[I] := Items[I];
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
var
  I: Integer;
begin
  while Properties.Count < Source.Properties.Count do
    Properties.Add(TContactProperty.Create);
  while Properties.Count > Source.Properties.Count do
    Properties.Delete(Properties.Count - 1);
  for I := 0 to Properties.Count - 1 do
    Properties[I].Assign(Source.Properties[I]);
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
    AddNew('N', [], SLastName, cfLastName, dtString, 0);
    AddNew('N', [], SFirstName, cfFirstName, dtString, 1);
    AddNew('N', [], SMiddleName, cfMiddleName, dtString, 2);
    AddNew('N', [], STitleBefore, cfTitleBefore, dtString, 3);
    AddNew('N', [], STitleAfter, cfTitleAfter, dtString, 4);
    AddNew('FN', [], SFullName, cfFullName, dtString);
    AddNew('TEL', [], STelephone, cfTel, dtString);
    AddNew('TEL', ['CELL'], SCellPhone, cfTelCell, dtString);
    AddNew('TEL', ['HOME'], SHomePhone, cfTelHome, dtString);
    AddNew('TEL', ['HOME2'], SHomePhone2, cfTelHome2, dtString);
    AddNew('TEL', ['WORK'], SWorkPhone, cfTelWork, dtString);
    AddNew('TEL', ['VOIP'], SVoipPhone, cfTelVoip, dtString);
    AddNew('TEL', ['MAIN'], SMainPhone, cfTelMain, dtString);
    AddNew('EMAIL', [], SEmail, cfEmail, dtString);
    AddNew('EMAIL', ['HOME'], SHomeEmail, cfEmailHome, dtString);
    AddNew('EMAIL', ['INTERNET'], SInternetEmail, cfEmailInternet, dtString);
    AddNew('NICKNAME', [], SNickName, cfNickName, dtString);
    AddNew('NOTE', [], SNote, cfNote, dtString);
    AddNew('ROLE', [], SRole, cfRole, dtString);
    AddNew('TITLE', [], STitle, cfTitle, dtString);
    AddNew('CATEGORIES', [], SCategories, cfCategories, dtString);
    AddNew('ORG', [], SOrganization, cfOrganization, dtString, 0);
    AddNew('ORG', [], SDepartement, cfDepartment, dtString, 1);
    AddNew('ADR', ['HOME'], SHomeAddress, cfHomeAddress, dtString);
    AddNew('ADR', ['HOME'], SHomeAddressStreet, cfHomeAddressStreet, dtString, 1);
    AddNew('ADR', ['HOME'], SHomeAddressCity, cfHomeAddressCity, dtString, 2);
    AddNew('ADR', ['HOME'], SHomeAddressCountry, cfHomeAddressCountry, dtString, 3);
    AddNew('ADR', ['WORK'], SWorkAddress, cfWorkAddress, dtString);
    AddNew('ADR', ['WORK'], SWorkAddressStreet, cfWorkAddressStreet, dtString, 1);
    AddNew('ADR', ['WORK'], SWorkAddressCity, cfWorkAddressCity, dtString, 2);
    AddNew('ADR', ['WORK'], SWorkAddressCountry, cfWorkAddressCountry, dtString, 3);
    AddNew('X-TIMES_CONTACTED', [], STimesContacted, cfXTimesContacted, dtString);
    AddNew('X-LAST_TIME_CONTACTED', [], SLastTimeContacted, cfXLastTimeContacted, dtString);
    AddNew('PHOTO', [], SPhoto, cfPhoto, dtString);
    AddNew('X-JABBER', [], SJabber, cfXJabber, dtString);
    AddNew('BDAY', [], SDayOfBirth, cfDayOfBirth, dtString);
    AddNew('REV', [], SRevision, cfRevision, dtString);
    AddNew('UID', [], SUniqueIdentifier, cfUid, dtString);
    AddNew('URL', [], SWebAddress, cfUrl, dtString);
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
          NewProperty.Attributes.DelimitedText := UpperCase(Names);
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

