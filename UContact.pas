unit UContact;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, fgl, Dialogs, UDataFile, LazUTF8, Base64;

type
  TContactsFile = class;

  TErrorEvent = procedure (Text: string; Line: Integer) of object;

  TDataType = (dtNone, dtString, dtInteger, dtDate, dtDateTime, dtImage, dtStringList);

  TContactFieldIndex = (cfNone, cfFirstName, cfMiddleName, cfLastName, cfTitleBefore,
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
    cfXTimesContacted, cfXLastTimeContacted, cfPhoto, cfDayOfBirth, cfRevision,
    cfVersion, cfAnniversary, cfGender, cfLogo,
    cfJabber, cfIcq, cfWindowsLive, cfGoogleTalk, cfAim, cfQq, cfYahoo, cfIrc,
    cfSkype, cfMsn, cfGroupWise, cfGaduGadu,
    cfTwitter, cfFacebook, cfInstagram, cfSnapchat, cfMatrix, cfYoutube,
    cfPeerTube, cfLinkedIn, cfMastodon, cfMySpace, cfReddit);

  TContactFieldIndexes = TFPGList<TContactFieldIndex>;

  TContactFilterItem = class
    FieldIndex: TContactFieldIndex;
    Value: string;
  end;

  { TContactFilterItems }

  TContactFilterItems = class(TFPGObjectList<TContactFilterItem>)
    function AddNew(FieldIndex: TContactFieldIndex; Value: string): TContactFilterItem;
  end;

  TContactFields = class;

  { TContactField }

  TContactField = class
    SysName: string;
    Groups: TStringArray;
    NoGroups: TStringArray;
    Title: string;
    Index: TContactFieldIndex;
    ValueIndex: Integer;
    DataType: TDataType;
    Alternatives: TContactFields;
    function AddAlternative(Name: string; Groups: array of string; NoGroups:
      array of string): TContactField;
    function GroupsContain(Name: string): Boolean;
    function Match(ASysName: string; AGroups: TStringArray): Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  { TContactFields }

  TContactFields = class(TFPGObjectList<TContactField>)
    function AddNew(Name: string; Groups: array of string; NoGroups: array of string;
      Title: string; Index: TContactFieldIndex; DataType:
      TDataType = dtNone; ValueIndex: Integer = -1): TContactField;
    function GetBySysName(SysName: string): TContactField;
    function GetBySysNameGroups(SysName: string; Groups: TStringArray): TContactField;
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
    function AddNew(Name, Value: string): TContactProperty;
    procedure Assign(Source: TContactProperties);
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
    FModified: Boolean;
    FOnModify: TNotifyEvent;
    class var FFields: TContactFields;
    function GetField(Index: TContactFieldIndex): string;
    procedure SetField(Index: TContactFieldIndex; AValue: string);
    procedure SetModified(AValue: Boolean);
    procedure DoOnModify;
  public
    Properties: TContactProperties;
    ContactsFile: TContactsFile;
    class function GetFields: TContactFields; static;
    function HasField(FieldIndex: TContactFieldIndex): Boolean;
    function FullNameToFileName: string;
    function GetProperty(Field: TContactField): TContactProperty; overload;
    function GetProperty(FieldIndex: TContactFieldIndex): TContactProperty; overload;
    procedure Assign(Source: TContact);
    function UpdateFrom(Source: TContact): Boolean;
    constructor Create;
    destructor Destroy; override;
    class destructor Destroy2;
    procedure SaveToStrings(Output: TStrings);
    function LoadFromStrings(Lines: TStrings; StartLine: Integer = 0): Integer;
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    property Fields[Index: TContactFieldIndex]: string read GetField write SetField;
    property Modified: Boolean read FModified write SetModified;
    property OnModify: TNotifyEvent read FOnModify write FOnModify;
  end;

  TGetContactEvent = function (Contact: TContact): TContact of object;

  { TContacts }

  TContacts = class(TFPGObjectList<TContact>)
    ContactsFile: TContactsFile;
    procedure Assign(Source: TContacts);
    procedure AddContacts(Contacts: TContacts);
    procedure InsertContacts(Index: Integer; Contacts: TContacts);
    procedure AssignToList(List: TFPGObjectList<TObject>);
    function AddNew: TContact;
    function Search(Text: string; FieldIndex: TContactFieldIndex): TContact;
    function CountByField(FieldIndex: TContactFieldIndex): Integer;
    procedure Merge(Contact: TContact; FieldIndex: TContactFieldIndex);
    function ToString: ansistring; override;
  end;

  { TContactsFile }

  TContactsFile = class(TDataFile)
  private
    FOnError: TErrorEvent;
    procedure Error(Text: string; Line: Integer);
    function NewItem(Key, Value: string): string;
  public
    Contacts: TContacts;
    function GetFileName: string; override;
    function GetFileExt: string; override;
    function GetFileFilter: string; override;
    procedure SaveToStrings(Output: TStrings);
    procedure LoadFromStrings(Lines: TStrings);
    procedure SaveToFile(FileName: string); override;
    procedure LoadFromFile(FileName: string); override;
    constructor Create; override;
    destructor Destroy; override;
    property OnError: TErrorEvent read FOnError write FOnError;
  end;

const
  VCardFileExt = '.vcf';


implementation

uses
  UQuotedPrintable, UCommon;

const
  VCardBegin = 'BEGIN:VCARD';
  VCardEnd = 'END:VCARD';

resourcestring
  SVCardFile = 'vCard file';
  SExpectedVCardBegin = 'Expected vCard begin';
  SFieldIndexNotDefined = 'Field index not defined';
  SContactHasNoParent = 'Contact has no parent';
  SExpectedProperty = 'Expected contact property';
  SVersion = 'Version';
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
  SLogo = 'Logo';
  SJabber = 'Jabber';
  SDayOfBirth = 'Day of birth';
  SAnniversary = 'Anniversary';
  SRevision = 'Revision';
  SUniqueIdentifier = 'Unique identifier';
  SWebAddress = 'Web address';
  SWebAddressHome = 'Web address home';
  SWebAddressWork = 'Web address work';
  SGender = 'Gender';
  // Chat
  SMsn = 'MSN';
  SGoogleTalk = 'Google Talk';
  SWindowsLive = 'Windows Live';
  SAim = 'AIM';
  SQq = 'QQ';
  SIrc = 'IRC';
  SIcq = 'ICQ';
  SYahoo = 'Yahoo!';
  SSkype = 'Skype';
  SMatrix = 'Matrix';
  SGroupWise = 'GroupWise';
  SGaduGadu = 'GaduGadu';
  // Social
  STwitter = 'Twitter';
  SFacebook = 'Facebook';
  SInstagram = 'Instagram';
  SMastodon = 'Mastodon';
  SSnapchat = 'Snapchat';
  SLinkedIn = 'LinkedIn';
  SYouTube = 'YouTube';
  SPeerTube = 'PeerTube';
  SReddit = 'Reddit';
  SMySpace = 'MySpace';

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

function EndsWith(Text, What: string): Boolean;
begin
  Result := Copy(Text, Length(Text) - Length(What) + 1, MaxInt) = What;
end;

function EncodeEscaped(Text: string): string;
var
  I: Integer;
  O: Integer;
begin
  Result := '';
  I := 1;
  O := 1;
  SetLength(Result, Length(Text)); // Preallocate string
  while I <= Length(Text) do begin
    if Text[I] in [',', '\', ';'] then begin
        Result[O] := '\';
        Inc(O);
        Result[O] := Text[I];
        SetLength(Result, Length(Result) + 1);
        Inc(O);
      end else begin
        Result[O] := Text[I];
        Inc(O);
      end;
    Inc(I);
  end;
  SetLength(Result, O - 1);
end;

function DecodeEscaped(Text: string): string;
var
  I: Integer;
  O: Integer;
  Escaped: Boolean;
begin
  Result := '';
  I := 1;
  O := 1;
  Escaped := False;
  SetLength(Result, Length(Text)); // Preallocate string
  while I <= Length(Text) do begin
    if Escaped then begin
      Result[O] := Text[I];
      Inc(O);
      Escaped := False;
    end else begin
      if Text[I] = '\' then begin
        Escaped := True;
      end else begin
        Result[O] := Text[I];
        Inc(O);
      end;
    end;
    Inc(I);
  end;
  SetLength(Result, O - 1);
end;

{ TContactFilterItems }

function TContactFilterItems.AddNew(FieldIndex: TContactFieldIndex;
  Value: string): TContactFilterItem;
begin
  Result := TContactFilterItem.Create;
  Result.FieldIndex := FieldIndex;
  Result.Value := Value;
  Add(Result);
end;

{ TContactField }

function TContactField.AddAlternative(Name: string; Groups: array of string;
  NoGroups: array of string): TContactField;
begin
  Result := Alternatives.AddNew(Name, Groups, NoGroups, Title, Index, DataType, ValueIndex);
end;

function TContactField.GroupsContain(Name: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(Groups) - 1 do
    if Groups[I] = Name then begin
      Result := True;
      Break;
    end;
end;

function TContactField.Match(ASysName: string; AGroups: TStringArray): Boolean;
var
  I: Integer;
begin
  Result := ASysName = SysName;
  if Result then begin
    for I := 0 to Length(AGroups) - 1 do begin
      if not GroupsContain(AGroups[I]) then begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

constructor TContactField.Create;
begin
  Alternatives := TContactFields.Create;
end;

destructor TContactField.Destroy;
begin
  FreeAndNil(Alternatives);
  inherited;
end;

{ TContactProperties }

function TContactProperties.AddNew(Name, Value: string): TContactProperty;
begin
  Result := TContactProperty.Create;
  Result.Name := Name;
  Result.Value := Value;
  Add(Result);
end;

procedure TContactProperties.Assign(Source: TContactProperties);
var
  I: Integer;
begin
  while Count < Source.Count do
    Add(TContactProperty.Create);
  while Count > Source.Count do
    Delete(Count - 1);
  for I := 0 to Count - 1 do
    Items[I].Assign(Source.Items[I]);
end;

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
  while (I < Count) and (Items[I].Name <> Name) and (not EndsWith(Items[I].Name, '.' + Name)) do Inc(I);
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
  if Attributes.IndexOf('BASE64') <> -1 then begin
    Encoding := 'BASE64';
    Value := GetDecodedValue;
  end else
  if Attributes.IndexOfName('ENCODING') <> -1 then begin
    Encoding := Attributes.Values['ENCODING'];
    if (Encoding = 'B') or (Encoding = 'b') then Encoding := 'BASE64';
    if (Encoding = 'Q') or (Encoding = 'q') then Encoding := 'QUOTED-PRINTABLE';
    if (Encoding = 'QUOTED-PRINTABLE') or (Encoding = 'BASE64') then begin
      Value := GetDecodedValue;
      Attributes.Delete(Attributes.IndexOfName('ENCODING'));
    end else
  end else Encoding := '';

  if Attributes.IndexOfName('CHARSET') <> -1 then
    Charset := Attributes.Values['CHARSET']
    else Charset := '';

  // Simplify TYPE attribute from TYPE=VALUE into VALUE
  for I := 0 to Attributes.Count - 1 do begin
    if Attributes.Names[I] = 'TYPE' then
      Attributes.Strings[I] := Attributes.Values['TYPE'];
    if Attributes.Names[I] = 'type' then
      Attributes.Strings[I] := Attributes.Values['type'];
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
  Result := (Name = AName) or EndsWith(Name, '.' + AName);
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

procedure TContacts.Assign(Source: TContacts);
var
  I: Integer;
begin
  while Count < Source.Count do
    Add(TContact.Create);
  while Count > Source.Count do
    Delete(Count - 1);
  for I := 0 to Count - 1 do begin
    Items[I].Assign(Source.Items[I]);
    Items[I].ContactsFile := ContactsFile;
  end;
end;

procedure TContacts.AddContacts(Contacts: TContacts);
var
  I: Integer;
  NewContact: TContact;
begin
  for I := 0 to Contacts.Count - 1 do begin
    NewContact := TContact.Create;
    NewContact.Assign(Contacts[I]);
    NewContact.ContactsFile := ContactsFile;
    Add(NewContact);
  end;
end;

procedure TContacts.InsertContacts(Index: Integer; Contacts: TContacts);
var
  I: Integer;
  NewContact: TContact;
begin
  for I := 0 to Contacts.Count - 1 do begin
    NewContact := TContact.Create;
    NewContact.Assign(Contacts[I]);
    NewContact.ContactsFile := ContactsFile;
    Insert(Index, NewContact);
    Inc(Index);
  end;
end;

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
  Result.ContactsFile := ContactsFile;
  Add(Result);
end;

function TContacts.Search(Text: string; FieldIndex: TContactFieldIndex): TContact;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Fields[FieldIndex] = Text then begin
      Result := Items[I];
      Break;
    end;
end;

function TContacts.CountByField(FieldIndex: TContactFieldIndex): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].HasField(FieldIndex) then
      Inc(Result);
end;

procedure TContacts.Merge(Contact: TContact; FieldIndex: TContactFieldIndex);
var
  NewContact: TContact;
begin
  NewContact := Search(Contact.Fields[FieldIndex], FieldIndex);
  if Assigned(NewContact) then begin
    NewContact.UpdateFrom(Contact);
  end else begin
    NewContact := TContact.Create;
    NewContact.Assign(Contact);
    NewContact.ContactsFile := ContactsFile;
    Add(NewContact);
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
  DataType: TDataType = dtNone; ValueIndex: Integer = -1): TContactField;
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

function TContactFields.GetBySysName(SysName: string): TContactField;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and (Items[I].SysName <> SysName) do Inc(I);
  if I < Count then Result := Items[I]
    else Result := nil;
end;

function TContactFields.GetBySysNameGroups(SysName: string; Groups: TStringArray
  ): TContactField;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and not Items[I].Match(SysName, Groups) do Inc(I);
  if I < Count then Result := Items[I]
    else Result := nil;
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
  AItems.BeginUpdate;
  try
    while AItems.Count < Count do AItems.Add('');
    while AItems.Count > Count do AItems.Delete(AItems.Count - 1);
    for I := 0 to Count - 1 do begin
      AItems.Objects[I] := Items[I];
      AItems[I] := Items[I].Title;
    end;
    SortStrings(AItems);
  finally
    AItems.EndUpdate;
  end;
end;

{ TContact }

class function TContact.GetFields: TContactFields;
begin
  if not Assigned(FFields) then begin
    FFields := TContactFields.Create;
    with FFields do begin
    AddNew('VERSION', [], [], SVersion, cfVersion, dtString);
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
    with AddNew('NICKNAME', [], [], SNickName, cfNickName, dtString) do
      AddAlternative('X-NICKNAME', [], []);
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
    AddNew('LOGO', [], [], SLogo, cfLogo, dtImage);
    AddNew('BDAY', [], [], SDayOfBirth, cfDayOfBirth, dtDate);
    with AddNew('ANNIVERSARY', [], [], SAnniversary, cfAnniversary, dtDate) do
      AddAlternative('X-EVOLUTION-ANNIVERSARY', [], []);
    AddNew('REV', [], [], SRevision, cfRevision, dtString);
    AddNew('UID', [], [], SUniqueIdentifier, cfUid, dtString);
    AddNew('URL', [], ['HOME', 'WORK'], SWebAddress, cfUrl, dtString);
    AddNew('URL', ['HOME'], [], SWebAddressHome, cfUrlHome, dtString);
    AddNew('URL', ['WORK'], [], SWebAddressWork, cfUrlWork, dtString);
    with AddNew('GENDER', [], [], SGender, cfGender, dtString) do
      AddAlternative('X-CENTRUM-CZ-SEX', [], []);
    // Chat
    AddNew('X-MATRIX', [], [], SMatrix, cfMatrix, dtString);
    AddNew('X-JABBER', [], [], SJabber, cfJabber, dtString);
    AddNew('X-AIM', [], [], SAim, cfAim, dtString);
    AddNew('X-Windows Live', [], [], SWindowsLive, cfWindowsLive, dtString);
    AddNew('X-YAHOO', [], [], SYahoo, cfYahoo, dtString);
    with AddNew('X-SKYPE-USERNAME', [], [], SSkype, cfSkype, dtString) do begin
      AddAlternative('X-SKYPE', [], []);
      AddAlternative('X-CENTRUM-CZ-SKYPE', [], []);
    end;
    AddNew('X-QQ', [], [], SQq, cfQq, dtString);
    AddNew('X-GOOGLE-TALK', [], [], SGoogleTalk, cfGoogleTalk, dtString);
    with AddNew('X-ICQ', [], [], SIcq, cfIcq, dtString) do
      AddAlternative('X-CENTRUM-CZ-ICQ', [], []);
    AddNew('X-IRC', [], [], SIrc, cfIrc, dtString);
    with AddNew('X-MSN', [], [], SMsn, cfMsn, dtString) do
      AddAlternative('X-CENTRUM-CZ-MSN', [], []);
    AddNew('X-GROUPWISE', [], [], SGroupWise, cfGroupWise, dtString);
    AddNew('X-GADUGADU', [], [], SGaduGadu, cfGaduGadu, dtString);
    // Social
    with AddNew('X-TWITTER', [], [], STwitter, cfTwitter, dtString) do
      AddAlternative('X-SOCIALPROFILE', ['TWITTER'], []);
    with AddNew('X-FACEBOOK', [], [], SFacebook, cfFacebook, dtString) do
      AddAlternative('X-SOCIALPROFILE', ['FACEBOOK'], []);
    with AddNew('X-MASTODON', [], [], SMastodon, cfMastodon, dtString) do
      AddAlternative('X-SOCIALPROFILE', ['MASTODON'], []);
    with AddNew('X-YOUTUBE', [], [], SYouTube, cfYouTube, dtString) do
      AddAlternative('X-SOCIALPROFILE', ['YOUTUBE'], []);
    with AddNew('X-PEERTUBE', [], [], SPeerTube, cfPeerTube, dtString) do
      AddAlternative('X-SOCIALPROFILE', ['PEERTUBE'], []);
    with AddNew('X-LINKEDIN', [], [], SLinkedIn, cfLinkedIn, dtString) do
      AddAlternative('X-SOCIALPROFILE', ['LINKEDIN'], []);
    with AddNew('X-SNAPCHAT', [], [], SSnapchat, cfSnapchat, dtString) do
      AddAlternative('X-SOCIALPROFILE', ['SNAPCHAT'], []);
    with AddNew('X-INSTAGRAM', [], [], SInstagram, cfInstagram, dtString) do
      AddAlternative('X-SOCIALPROFILE', ['INSTAGRAM'], []);
    with AddNew('X-REDDIT', [], [], SReddit, cfReddit, dtString) do
      AddAlternative('X-SOCIALPROFILE', ['REDDIT'], []);
    with AddNew('X-MYSPACE', [], [], SMySpace, cfMySpace, dtString) do
      AddAlternative('X-SOCIALPROFILE', ['MYSPACE'], []);
    end;
  end;
  Result := FFields;
end;

function TContact.GetField(Index: TContactFieldIndex): string;
var
  Prop: TContactProperty;
  Field: TContactField;
begin
  if not Assigned(ContactsFile) then raise Exception.Create(SContactHasNoParent);
  Field := GetFields.GetByIndex(Index);
  if Assigned(Field) then begin
    Prop := GetProperty(Field);
    if Assigned(Prop) then begin
      if Field.ValueIndex <> -1 then begin
        Result := DecodeEscaped(Prop.ValueItem[Field.ValueIndex])
      end else Result := Prop.Value;
    end else Result := '';
  end else raise Exception.Create(SFieldIndexNotDefined);
end;

procedure TContact.SetField(Index: TContactFieldIndex; AValue: string);
var
  Prop: TContactProperty;
  Field: TContactField;
  I: Integer;
begin
  if not Assigned(ContactsFile) then raise Exception.Create(SContactHasNoParent);
  Field := GetFields.GetByIndex(Index);
  if Assigned(Field) then begin
    Prop := GetProperty(Field);
    if (not Assigned(Prop)) and (AValue <> '') then begin
      Prop := TContactProperty.Create;
      Prop.Name := Field.SysName;
      for I := 0 to Length(Field.Groups) - 1 do
        Prop.Attributes.Add(Field.Groups[I]);
      Properties.Add(Prop);
    end;
    if Assigned(Prop) then begin
      if Field.ValueIndex <> -1 then begin
        Prop.ValueItem[Field.ValueIndex] := EncodeEscaped(AValue);
      end else Prop.Value := AValue;

      // Remove if empty
      if Prop.Value = '' then begin
        Properties.Remove(Prop);
      end;
    end;
    Modified := True;
  end else raise Exception.Create(SFieldIndexNotDefined);
end;

procedure TContact.SetModified(AValue: Boolean);
begin
  if FModified = AValue then Exit;
  FModified := AValue;
  DoOnModify;
end;

procedure TContact.DoOnModify;
begin
  if Assigned(FOnModify) then FOnModify(Self);
end;

function TContact.HasField(FieldIndex: TContactFieldIndex): Boolean;
var
  Field: TContactField;
begin
  if not Assigned(ContactsFile) then raise Exception.Create(SContactHasNoParent);
  Field := GetFields.GetByIndex(FieldIndex);
  if Assigned(Field) then begin
    Result := Assigned(GetProperty(Field));
  end else raise Exception.Create(SFieldIndexNotDefined);
end;

function TContact.FullNameToFileName: string;
var
  I: Integer;
begin
  Result := Fields[cfFullName];
  for I := 1 to Length(Result) do begin
    if Result[I] in [':', '/', '\', '.', '"', '*', '|', '?', '<', '>'] then
      Result[I] := '_';
  end;
end;

function TContact.GetProperty(Field: TContactField): TContactProperty;
var
  I: Integer;
begin
  Result := Properties.GetByNameGroups(Field.SysName, Field.Groups, Field.NoGroups);
  I := 0;
  while (not Assigned(Result)) and (I < Field.Alternatives.Count) do begin
    Result := Properties.GetByNameGroups(Field.Alternatives[I].SysName,
      Field.Alternatives[I].Groups, Field.Alternatives[I].NoGroups);
    if Assigned(Result) then Break;
    Inc(I);
  end;
end;

function TContact.GetProperty(FieldIndex: TContactFieldIndex): TContactProperty;
var
  Field: TContactField;
begin
  if not Assigned(ContactsFile) then raise Exception.Create(SContactHasNoParent);
  Field := GetFields.GetByIndex(FieldIndex);
  if Assigned(Field) then begin
    Result := GetProperty(Field);
  end else Result := nil;
end;

procedure TContact.Assign(Source: TContact);
begin
  Properties.Assign(Source.Properties);
  FModified := Source.FModified;
end;

function TContact.UpdateFrom(Source: TContact): Boolean;
var
  I: Integer;
begin
  if not Assigned(ContactsFile) then raise Exception.Create(SContactHasNoParent);
  Result := False;
  for I := 0 to GetFields.Count - 1 do begin
    if (Source.Fields[GetFields[I].Index] <> '') and
      (Source.Fields[GetFields[I].Index] <>
      Fields[GetFields[I].Index]) then begin
        Result := True;
        Fields[GetFields[I].Index] := Source.Fields[GetFields[I].Index];
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

class destructor TContact.Destroy2;
begin
  FreeAndNil(FFields);
end;

procedure TContact.SaveToStrings(Output: TStrings);
var
  I: Integer;
  NameText: string;
  Value2: string;
  LineIndex: Integer;
  OutText: string;
  LinePrefix: string;
  CutLength: Integer;
const
  MaxLineLength = 73;
begin
    with Output do begin
      Add(VCardBegin);
      for I := 0 to Properties.Count - 1 do
      with Properties[I] do begin
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
              CutLength := MaxLineLength;
              if Encoding = 'QUOTED-PRINTABLE' then begin
                // Do not cut encoded items
                if ((CutLength - 2) >= 1) and (OutText[CutLength - 2] = '=') then
                  Dec(CutLength, 2)
                else if ((CutLength - 1) >= 1) and (OutText[CutLength - 1] = '=') then
                  Dec(CutLength, 1);
              end;
              Add(LinePrefix + Copy(OutText, 1, CutLength));
              LinePrefix := ' ';
              System.Delete(OutText, 1, CutLength);
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
      Add(VCardEnd);
    end;
end;

function TContact.LoadFromStrings(Lines: TStrings; StartLine: Integer = 0): Integer;
type
  TParseState = (psNone, psInside, psFinished);
var
  ParseState: TParseState;
  Line: string;
  Line2: string;
  Value: string;
  I: Integer;
  NewProperty: TContactProperty;
  CommandPart: string;
  Names: string;
begin
  ParseState := psNone;
  I := StartLine;
  while I < Lines.Count do begin
    Line := Trim(Lines[I]);
    if Line = '' then begin
      // Skip empty lines
    end else
    if ParseState = psNone then begin
      if Line = VCardBegin then begin
        ParseState := psInside;
      end else begin
        ContactsFile.Error(SExpectedVCardBegin, I + 1);
        I := -1;
        Break;
      end;
    end else
    if ParseState = psInside then begin
      if Line = VCardEnd then begin
        ParseState := psFinished;
        Inc(I);
        Break;
      end else
      if Pos(':', Line) > 0 then begin
        CommandPart := GetNext(Line, ':');
        Names := CommandPart;
        Value := Line;
        while True do begin
          Inc(I);
          Line2 := Lines[I];
          if (Length(Lines[I]) > 0) and (Line2[1] = ' ') then begin
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
        NewProperty := Properties.GetByName(Names);
        if not Assigned(NewProperty) then begin
          NewProperty := TContactProperty.Create;
          Properties.Add(NewProperty);
        end;
        NewProperty.Attributes.DelimitedText := Names;
        if NewProperty.Attributes.Count > 0 then begin
          NewProperty.Name := NewProperty.Attributes[0];
          NewProperty.Attributes.Delete(0);
        end;
        NewProperty.Value := Value;
        NewProperty.EvaluateAttributes;
      end else begin
        ContactsFile.Error(SExpectedProperty, I + 1);
        I := -1;
        Break;
      end;
    end;
    Inc(I);
  end;
  Result := I;
end;

procedure TContact.SaveToFile(FileName: string);
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    SaveToStrings(Lines);
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end;
end;

procedure TContact.LoadFromFile(FileName: string);
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
    {$IF FPC_FULLVERSION>=30200}
    if (Length(Lines.Text) > 0) and (Pos(VCardBegin, Lines.Text) = 0) then begin
      Lines.LoadFromFile(FileName, TEncoding.Unicode);
      if (Length(Lines.Text) > 0) and (Pos(VCardBegin, Lines.Text) = 0) then begin
        Lines.LoadFromFile(FileName, TEncoding.BigEndianUnicode);
      end;
    end;
    {$ENDIF}
    LoadFromStrings(Lines);
  finally
    Lines.Free;
  end;
end;

{ TContactsFile }

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
  Result := VCardFileExt;
end;

function TContactsFile.GetFileFilter: string;
begin
  Result := GetFileName + ' (' + GetFileExt + ')|*' + GetFileExt + '|' + inherited;
end;

procedure TContactsFile.SaveToStrings(Output: TStrings);
var
  I: Integer;
begin
  for I := 0 to Contacts.Count - 1 do
    Contacts[I].SaveToStrings(Output);
end;

procedure TContactsFile.LoadFromStrings(Lines: TStrings);
var
  Contact: TContact;
  I: Integer;
  NewI: Integer;
begin
  Contacts.Clear;

  I := 0;
  while I < Lines.Count do begin
    Contact := TContact.Create;
    Contact.ContactsFile := Self;
    NewI := Contact.LoadFromStrings(Lines, I);
    if NewI <= Lines.Count then begin
      if NewI <> -1 then begin
        Contacts.Add(Contact);
        I := NewI;
      end else begin
        FreeAndNil(Contact);
        Inc(I);
      end;
    end else begin
      FreeAndNil(Contact);
      Break;
    end;
  end;
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
  Lines: TStringList;
begin
  inherited;
  Lines := TStringList.Create;
  try
    SaveToStrings(Lines);
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end
end;

procedure TContactsFile.LoadFromFile(FileName: string);
var
  Lines: TStringList;
begin
  inherited;
  Lines := TStringList.Create;
  Lines.LoadFromFile(FileName);
  {$IF FPC_FULLVERSION>=30200}
  if (Length(Lines.Text) > 0) and (Pos(VCardBegin, Lines.Text) = 0) then begin
    Lines.LoadFromFile(FileName, TEncoding.Unicode);
    if (Length(Lines.Text) > 0) and (Pos(VCardBegin, Lines.Text) = 0) then begin
      Lines.LoadFromFile(FileName, TEncoding.BigEndianUnicode);
    end;
  end;
  {$ENDIF}
  try
    LoadFromStrings(Lines);
  finally
    Lines.Free;
  end;
end;

constructor TContactsFile.Create;
begin
  inherited;
  Contacts := TContacts.Create;
  Contacts.ContactsFile := Self;
end;

destructor TContactsFile.Destroy;
begin
  FreeAndNil(Contacts);
  inherited;
end;

end.

