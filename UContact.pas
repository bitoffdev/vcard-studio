unit UContact;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, fgl, Dialogs, UDataFile, LazUTF8, Base64;

type
  TContactsFile = class;

  TErrorEvent = procedure (Text: string; Line: Integer) of object;

  TDataType = (dtString, dtInteger, dtDate, dtDateTime, dtImage, dtStringList);

  TContactFieldIndex = (cfFirstName, cfMiddleName, cfLastName, cfTitleBefore,
    cfTitleAfter, cfFullName,
    cfTel, cfTelCell, cfTelFax, cfTelPager, cfTelHome2, cfTelVoip, cfTelMain,
    cfTelHome, cfTelCellHome, cfTelFaxHome, cfTelPagerHome,
    cfTelWork, cfTelCellWork, cfTelFaxWork, cfTelPagerWork,
    cfEmail, cfUid, cfUrl, cfUrlHome, cfUrlWork,
    cfEmailHome, cfEmailWork, cfEmailInternet, cfNickName, cfNote, cfRole, cfTitle,
    cfCategories, cfOrganization, cfDepartment,
    cfHomeAddressStreet, cfHomeAddressStreetExtended, cfHomeAddressCity, cfHomeAddressCountry,
    cfHomeAddressPostalCode, cfHomeAddressRegion, cfHomeAddressPostOfficeBox,
    cfWorkAddressStreet, cfWorkAddressStreetExtended, cfWorkAddressCity, cfWorkAddressCountry,
    cfWorkAddressPostalCode, cfWorkAddressRegion, cfWorkAddressPostOfficeBox,
    cfXTimesContacted, cfXLastTimeContacted, cfPhoto, cfXJabber, cfDayOfBirth, cfRevision,
    cfVersion, cfAnniversary);

  TContactField = class
    SysName: string;
    Groups: TStringArray;
    NoGroups: TStringArray;
    Title: string;
    Index: TContactFieldIndex;
    ValueIndex: Integer;
    DataType: TDataType;
  end;

  { TContactFields }

  TContactFields = class(TFPGObjectList<TContactField>)
    function AddNew(Name: string; Groups: array of string; NoGroups: array of string;
      Title: string; Index: TContactFieldIndex; DataType:
      TDataType; ValueIndex: Integer = -1): TContactField;
    function GetByIndex(Index: TContactFieldIndex): TContactField;
    procedure LoadToStrings(AItems: TStrings);
  end;

  { TContactProperty }

  TContactProperty = class
  private
    function GetValueItem(Index: Integer): string;
    procedure SetValueItem(Index: Integer; AValue: string);
  public
    Name: string;
    Attributes: TStringList;
    Value: string;
    Encoding: string;
    Charset: string;
    procedure EvaluateAttributes;
    function GetDecodedValue: string;
    function GetEncodedValue: string;
    function MatchNameGroups(AName: string; Groups: TStringArray;
      NoGroups: TStringArray): Boolean;
    procedure Assign(Source: TContactProperty);
    constructor Create;
    destructor Destroy; override;
    property ValueItem[Index: Integer]: string read GetValueItem write SetValueItem;
  end;

  { TContactProperties }

  TContactProperties = class(TFPGObjectList<TContactProperty>)
    procedure AssignToList(List: TFPGObjectList<TObject>);
    function GetByName(Name: string): TContactProperty;
    function GetByNameGroups(Name: string; Groups: TStringArray;
      NoGroups: TStringArray): TContactProperty;
    function GetByNameGroupsMultiple(Name: string; Groups: TStringArray;
      NoGroups: TStringArray): TContactProperties;
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
  SMobilePhone = 'Mobile phone';
  SPager = 'Pager';
  SFax = 'Fax';
  SHomePhone = 'Home phone';
  SHomeMobile = 'Home mobile';
  SHomeFax = 'Home fax';
  SHomePager = 'Home pager';
  SWorkPhone = 'Work phone';
  SWorkFax = 'Work fax';
  SWorkPager = 'Work pager';
  SWorkMobile = 'Work mobile';
  SHomePhone2 = 'Home phone 2';
  SVoipPhone = 'VoIP phone';
  SMainPhone = 'Main phone';
  SEmail = 'E-mail';
  SHomeEmail = 'Home E-mail';
  SWorkEmail = 'Work E-mail';
  SInternetEmail = 'Internet E-mail';
  SNickName = 'Nick name';
  SNote = 'Note';
  SRole = 'Role';
  STitle = 'Title';
  SCategories = 'Categories';
  SOrganization = 'Organization';
  SDepartement = 'Departement';
  SHomeAddressPostOfficeBox = 'Home address post office box';
  SHomeAddressStreetExtended = 'Home address extended street';
  SHomeAddressStreet = 'Home address street';
  SHomeAddressCity = 'Home address city';
  SHomeAddressRegion = 'Home address region';
  SHomeAddressPostalCode = 'Home address postal code';
  SHomeAddressCountry = 'Home address country';
  SWorkAddressPostOfficeBox = 'Work address post office box';
  SWorkAddressStreetExtended = 'Work address extended street';
  SWorkAddressStreet = 'Work address street';
  SWorkAddressCity = 'Work address city';
  SWorkAddressRegion = 'Work address region';
  SWorkAddressPostalCode = 'Work address postal code';
  SWorkAddressCountry = 'Work address country';
  STimesContacted = 'Times Contacted';
  SLastTimeContacted = 'Last Time Contacted';
  SPhoto = 'Photo';
  SJabber = 'Jabber';
  SDayOfBirth = 'Day of birth';
  SAnniversary = 'Anniversary';
  SRevision = 'Revision';
  SUniqueIdentifier = 'Unique identifier';
  SWebAddress = 'Web address';
  SWebAddressHome = 'Web address home';
  SWebAddressWork = 'Web address work';

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

procedure TContactProperties.AssignToList(List: TFPGObjectList<TObject>);
var
  I: Integer;
begin
  while List.Count > Count do List.Delete(List.Count - 1);
  while List.Count < Count do List.Add(nil);
  for I := 0 to Count - 1 do
    List[I] := Items[I];
end;

function TContactProperties.GetByName(Name: string): TContactProperty;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and (Items[I].Name <> Name) do Inc(I);
  if I < Count then Result := Items[I]
    else Result := nil;
end;

function TContactProperties.GetByNameGroups(Name: string; Groups: TStringArray;
  NoGroups: TStringArray): TContactProperty;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and not Items[I].MatchNameGroups(Name, Groups, NoGroups) do Inc(I);
  if I < Count then Result := Items[I]
    else Result := nil;
end;

function TContactProperties.GetByNameGroupsMultiple(Name: string;
  Groups: TStringArray; NoGroups: TStringArray): TContactProperties;
var
  I: Integer;
begin
  Result := TContactProperties.Create(False);
  for I := 0 to Count - 1 do
  if Items[I].MatchNameGroups(Name, Groups, NoGroups) then
    Result.Add(Items[I]);
end;

{ TContactProperty }

function TContactProperty.GetValueItem(Index: Integer): string;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.Delimiter := ';';
    List.NameValueSeparator := '=';
    List.StrictDelimiter := True;
    List.DelimitedText := Value;
    if Index < List.Count then
      Result := List.Strings[Index]
      else Result := '';
  finally
    List.Free;
  end;
end;

procedure TContactProperty.SetValueItem(Index: Integer; AValue: string);
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.Delimiter := ';';
    List.NameValueSeparator := '=';
    List.StrictDelimiter := True;
    List.DelimitedText := Value;

    // Extend subitems count
    while List.Count <= Index do
      List.Add('');

    List.Strings[Index] := AValue;

    // Remove empty items
    while (List.Count > 0) and (List.Strings[List.Count - 1] = '') do
      List.Delete(List.Count - 1);

    Value := List.DelimitedText;
  finally
    List.Free;
  end;
end;

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
      Value := GetDecodedValue;
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
  if Encoding = 'BASE64' then begin
    Result := DecodeStringBase64(Value);
  end else
  if Encoding = 'QUOTED-PRINTABLE' then begin
    Result := DecodeQuotedPrintable(Value);
  end
  else Result := '';
end;

function TContactProperty.GetEncodedValue: string;
begin
  if Encoding = 'BASE64' then begin
    Result := EncodeStringBase64(Value);
  end else
  if Encoding = 'QUOTED-PRINTABLE' then begin
    Result := EncodeQuotedPrintable(Value);
  end
  else Result := '';
end;

function TContactProperty.MatchNameGroups(AName: string; Groups: TStringArray;
  NoGroups: TStringArray): Boolean;
var
  I: Integer;
begin
  Result := Name = AName;
  if Result and (Length(Groups) > 0) then begin
    for I := 0 to Length(Groups) - 1 do
      if (Attributes.IndexOf(Groups[I]) = -1) and
      (Attributes.IndexOf('TYPE=' + Groups[I]) = -1) then begin
        Result := False;
        Break;
      end;
  end;
  if Result and (Length(NoGroups) > 0) then begin
    for I := 0 to Length(NoGroups) - 1 do
      if (Attributes.IndexOf(NoGroups[I]) <> -1) or
      (Attributes.IndexOf('TYPE=' + NoGroups[I]) <> -1) then begin
        Result := False;
        Break;
      end;
  end;
end;

procedure TContactProperty.Assign(Source: TContactProperty);
begin
  Name := Source.Name;
  Attributes.Assign(Source.Attributes);
  Value := Source.Value;
  Encoding := Source.Encoding;
  Charset := Source.Charset;
end;

constructor TContactProperty.Create;
begin
  Attributes := TStringList.Create;
  Attributes.Delimiter := ';';
  Attributes.NameValueSeparator := '=';
  Attributes.StrictDelimiter := True;
end;

destructor TContactProperty.Destroy;
begin
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

function TContactFields.AddNew(Name: string; Groups: array of string;
  NoGroups: array of string; Title: string; Index: TContactFieldIndex;
  DataType: TDataType; ValueIndex: Integer = -1): TContactField;
var
  I: Integer;
begin
  Result := TContactField.Create;
  Result.SysName := Name;
  SetLength(Result.Groups, Length(Groups));
  for I := 0 to Length(Groups) - 1 do
    Result.Groups[I] := Groups[I];
  SetLength(Result.NoGroups, Length(NoGroups));
  for I := 0 to Length(NoGroups) - 1 do
    Result.NoGroups[I] := NoGroups[I];
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
      Result := Prop.ValueItem[Field.ValueIndex]
    end else Result := Prop.Value;
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
    Prop := Properties.GetByNameGroups(Field.SysName, Field.Groups, Field.NoGroups);
    if (not Assigned(Prop)) and (AValue <> '') then begin
      Prop := TContactProperty.Create;
      Prop.Name := Field.SysName;
      for I := 0 to Length(Field.Groups) - 1 do
        Prop.Attributes.Add(Field.Groups[I]);
      Properties.Add(Prop);
    end;
    if Assigned(Prop) then begin
      if Field.ValueIndex <> -1 then begin
        Prop.ValueItem[Field.ValueIndex] := AValue;
      end else Prop.Value := AValue;

      // Remove if empty
      if Prop.Value = '' then begin
        Properties.Remove(Prop);
      end;
    end;
  end else raise Exception.Create(SFieldIndexNotDefined);
end;

function TContact.GetProperty(Index: TContactFieldIndex): TContactProperty;
var
  Prop: TContactProperty;
  Field: TContactField;
begin
  Field := Parent.Fields.GetByIndex(Index);
  if Assigned(Field) then begin
    Result := Properties.GetByNameGroups(Field.SysName, Field.Groups, Field.NoGroups);
  end else raise Exception.Create(SFieldIndexNotDefined);
end;

procedure TContact.Assign(Source: TContact);
var
  I: Integer;
begin
  Parent := Source.Parent;
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
    AddNew('N', [], [], SLastName, cfLastName, dtString, 0);
    AddNew('N', [], [], SFirstName, cfFirstName, dtString, 1);
    AddNew('N', [], [], SMiddleName, cfMiddleName, dtString, 2);
    AddNew('N', [], [], STitleBefore, cfTitleBefore, dtString, 3);
    AddNew('N', [], [], STitleAfter, cfTitleAfter, dtString, 4);
    AddNew('FN', [], [], SFullName, cfFullName, dtString);
    AddNew('TEL', [], ['CELL', 'FAX', 'PAGER', 'WORK', 'HOME'], STelephone, cfTel, dtString);
    AddNew('TEL', ['CELL'], ['WORK', 'HOME'], SMobilePhone, cfTelCell, dtString);
    AddNew('TEL', ['FAX'], ['WORK', 'HOME'], SFax, cfTelFax, dtString);
    AddNew('TEL', ['PAGER'], ['WORK', 'HOME'], SPager, cfTelPager, dtString);
    AddNew('TEL', ['HOME'], ['CELL', 'FAX', 'PAGER'], SHomePhone, cfTelHome, dtString);
    AddNew('TEL', ['HOME', 'CELL'], [], SHomeMobile, cfTelCellHome, dtString);
    AddNew('TEL', ['HOME', 'FAX'], [], SHomeFax, cfTelFaxHome, dtString);
    AddNew('TEL', ['HOME', 'PAGER'], [], SHomePager, cfTelPagerHome, dtString);
    AddNew('TEL', ['WORK'], ['CELL', 'FAX', 'PAGER'], SWorkPhone, cfTelWork, dtString);
    AddNew('TEL', ['WORK', 'CELL'], [], SWorkMobile, cfTelCellWork, dtString);
    AddNew('TEL', ['WORK', 'FAX'], [], SWorkFax, cfTelFaxWork, dtString);
    AddNew('TEL', ['WORK', 'PAGER'], [], SWorkPager, cfTelPagerWork, dtString);
    AddNew('TEL', ['HOME2'], [], SHomePhone2, cfTelHome2, dtString);
    AddNew('TEL', ['VOIP'], [], SVoipPhone, cfTelVoip, dtString);
    AddNew('TEL', ['MAIN'], [], SMainPhone, cfTelMain, dtString);
    AddNew('EMAIL', [], ['HOME', 'WORK', 'INTERNET'], SEmail, cfEmail, dtString);
    AddNew('EMAIL', ['HOME'], [], SHomeEmail, cfEmailHome, dtString);
    AddNew('EMAIL', ['WORK'], [], SWorkEmail, cfEmailWork, dtString);
    AddNew('EMAIL', ['INTERNET'], [], SInternetEmail, cfEmailInternet, dtString);
    AddNew('NICKNAME', [], [], SNickName, cfNickName, dtString);
    AddNew('NOTE', [], [], SNote, cfNote, dtString);
    AddNew('ROLE', [], [], SRole, cfRole, dtString);
    AddNew('TITLE', [], [], STitle, cfTitle, dtString);
    AddNew('CATEGORIES', [], [], SCategories, cfCategories, dtString);
    AddNew('ORG', [], [], SOrganization, cfOrganization, dtString, 0);
    AddNew('ORG', [], [], SDepartement, cfDepartment, dtString, 1);
    AddNew('ADR', ['HOME'], [], SHomeAddressPostOfficeBox, cfHomeAddressPostOfficeBox, dtString, 0);
    AddNew('ADR', ['HOME'], [], SHomeAddressStreetExtended, cfHomeAddressStreetExtended, dtString, 1);
    AddNew('ADR', ['HOME'], [], SHomeAddressStreet, cfHomeAddressStreet, dtString, 2);
    AddNew('ADR', ['HOME'], [], SHomeAddressCity, cfHomeAddressCity, dtString, 3);
    AddNew('ADR', ['HOME'], [], SHomeAddressRegion, cfHomeAddressRegion, dtString, 4);
    AddNew('ADR', ['HOME'], [], SHomeAddressPostalCode, cfHomeAddressPostalCode, dtString, 5);
    AddNew('ADR', ['HOME'], [], SHomeAddressCountry, cfHomeAddressCountry, dtString, 6);
    AddNew('ADR', ['WORK'], [], SWorkAddressPostOfficeBox, cfWorkAddressPostOfficeBox, dtString, 0);
    AddNew('ADR', ['WORK'], [], SWorkAddressStreetExtended, cfWorkAddressStreetExtended, dtString, 1);
    AddNew('ADR', ['WORK'], [], SWorkAddressStreet, cfWorkAddressStreet, dtString, 2);
    AddNew('ADR', ['WORK'], [], SWorkAddressCity, cfWorkAddressCity, dtString, 3);
    AddNew('ADR', ['WORK'], [], SWorkAddressRegion, cfWorkAddressRegion, dtString, 4);
    AddNew('ADR', ['WORK'], [], SWorkAddressPostalCode, cfWorkAddressPostalCode, dtString, 5);
    AddNew('ADR', ['WORK'], [], SWorkAddressCountry, cfWorkAddressCountry, dtString, 6);
    AddNew('X-TIMES_CONTACTED', [], [], STimesContacted, cfXTimesContacted, dtString);
    AddNew('X-LAST_TIME_CONTACTED', [], [], SLastTimeContacted, cfXLastTimeContacted, dtString);
    AddNew('PHOTO', [], [], SPhoto, cfPhoto, dtImage);
    AddNew('X-JABBER', [], [], SJabber, cfXJabber, dtString);
    AddNew('BDAY', [], [], SDayOfBirth, cfDayOfBirth, dtDate);
    AddNew('ANNIVERSARY', [], [], SAnniversary, cfAnniversary, dtDate);
    AddNew('REV', [], [], SRevision, cfRevision, dtString);
    AddNew('UID', [], [], SUniqueIdentifier, cfUid, dtString);
    AddNew('URL', [], ['HOME', 'WORK'], SWebAddress, cfUrl, dtString);
    AddNew('URL', ['HOME'], [], SWebAddressHome, cfUrlHome, dtString);
    AddNew('URL', ['WORK'], [], SWebAddressWork, cfUrlWork, dtString);
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
  NameText: string;
  Value2: string;
  Text: string;
  LineIndex: Integer;
  OutText: string;
  LinePrefix: string;
const
  MaxLineLength = 73;
begin
  inherited;
  try
    Output := TStringList.Create;
    for I := 0 to Contacts.Count - 1 do
    with Contacts[I], Output do begin
      Add('BEGIN:VCARD');
      for J := 0 to Properties.Count - 1 do
      with Properties[J] do begin
        NameText := Name;
        if Attributes.Count > 0 then
          NameText := NameText + ';' + Attributes.DelimitedText;
        if Encoding <> '' then begin
          Value2 := GetEncodedValue;
          NameText := NameText + ';ENCODING=' + Encoding;
        end else Value2 := Value;
        if Pos(LineEnding, Value2) > 0 then begin
          Add(NameText + ':' + GetNext(Value2, LineEnding));
          while Pos(LineEnding, Value2) > 0 do begin
            Add(' ' + GetNext(Value2, LineEnding));
          end;
          Add(' ' + GetNext(Value2, LineEnding));
          Add('');
        end else begin
          OutText := NameText + ':' + Value2;
          LineIndex := 0;
          LinePrefix := '';
          while True do begin
            if Length(OutText) > MaxLineLength then begin
              if (LineIndex > 0) and (LinePrefix = '') then LinePrefix := ' ';
              Add(LinePrefix + Copy(OutText, 1, MaxLineLength));
              System.Delete(OutText, 1, MaxLineLength);
              Inc(LineIndex);
              Continue;
            end else begin
              Add(LinePrefix + OutText);
              Break;
            end;
          end;
          if LinePrefix <> '' then Add('');
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
            end else
            if (Length(Lines[I]) > 0) and (Length(Value) > 0) and (Value[Length(Value)] = '=') and
              (Lines[I][1] = '=') then begin
              Value := Value + Copy(Trim(Lines[I]), 2, MaxInt);
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
          NewProperty.Value := Value;
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

