unit UFormFindDuplicity;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Contnrs, UContact;

type

  { TFoundItem }

  TFoundItem = class
    Field: string;
    Contacts: TContacts;
    constructor Create;
    destructor Destroy; override;
  end;

  { TFoundItems }

  TFoundItems = class(TObjectList)
    function SearchByField(Field: string): TFoundItem;
  end;

  { TFormFindDuplicity }

  TFormFindDuplicity = class(TForm)
    ComboBoxField: TComboBox;
    Label1: TLabel;
    ListView1: TListView;
    Panel1: TPanel;
    procedure ComboBoxFieldChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
  private
    FContacts: TContacts;
    procedure SetContacts(AValue: TContacts);

  public
    FoundItems: TFoundItems;
    ContactField: TContactFieldIndex;
    procedure Find;
    procedure ReloadList;
    property Contacts: TContacts read FContacts write SetContacts;
  end;

var
  FormFindDuplicity: TFormFindDuplicity;

implementation

{$R *.lfm}

uses
  UCore;

{ TFoundItems }

function TFoundItems.SearchByField(Field: string): TFoundItem;
var
  Item: TFoundItem;
begin
  Result := nil;
  for Item in Self do
    if Item.Field = Field then begin
      Result := Item;
      Break;
    end;
end;

{ TFoundItem }

constructor TFoundItem.Create;
begin
  Contacts := TContacts.Create(False);
end;

destructor TFoundItem.Destroy;
begin
  Contacts.Free;
  inherited Destroy;
end;

{ TFormFindDuplicity }

procedure TFormFindDuplicity.ListView1Data(Sender: TObject; Item: TListItem);
begin
  if Item.Index < FoundItems.Count then
  with TFoundItem(FoundItems[Item.Index]) do begin
    Item.Caption := Field;
    Item.SubItems.Add(Contacts.ToString);
    Item.SubItems.Add(IntToStr(Contacts.Count));
  end;
end;

procedure TFormFindDuplicity.SetContacts(AValue: TContacts);
begin
  if FContacts = AValue then Exit;
  FContacts := AValue;
  if Assigned(FContacts) then begin
    Contacts.ContactsFile.Fields.LoadToStrings(ComboBoxField.Items);
    ComboBoxField.ItemIndex := Integer(ContactField);
    if (ComboBoxField.Items.Count > 0) and (ComboBoxField.ItemIndex = -1) then
      ComboBoxField.ItemIndex := 0;
  end else ComboBoxField.Clear;
end;

function FoundItemsSort(Item1, Item2: Pointer): Integer;
begin
  if TFoundItem(Item1).Contacts.Count < TFoundItem(Item2).Contacts.Count then Result := 1
  else if TFoundItem(Item1).Contacts.Count > TFoundItem(Item2).Contacts.Count then Result := -1
  else Result := 0;
end;

procedure TFormFindDuplicity.Find;
var
  I: Integer;
  Item: TFoundItem;
  FieldName: string;
begin
  FoundItems.Clear;
  for I := 0 to Contacts.Count - 1 do begin
    FieldName := TContact(Contacts[I]).Fields[ContactField];
    if FieldName <> '' then begin
      Item := FoundItems.SearchByField(FieldName);
      if not Assigned(Item) then begin
        Item := TFoundItem.Create;
        Item.Field := FieldName;
        FoundItems.Add(Item);
      end;
      Item.Contacts.Add(Contacts[I]);
    end;
  end;
  FoundItems.Sort(FoundItemsSort);
  ReloadList;
end;

procedure TFormFindDuplicity.FormCreate(Sender: TObject);
begin
  FoundItems := TFoundItems.Create;
  Core.CoolTranslator1.TranslateComponentRecursive(Self);
  Core.ThemeManager1.UseTheme(Self);
  ContactField := cfTelCell;
end;

procedure TFormFindDuplicity.ComboBoxFieldChange(Sender: TObject);
begin
  ContactField := TContactFieldIndex(ComboBoxField.ItemIndex);
  Find;
end;

procedure TFormFindDuplicity.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  Core.PersistentForm1.Save(Self);
end;

procedure TFormFindDuplicity.FormDestroy(Sender: TObject);
begin
  FoundItems.Free;
end;

procedure TFormFindDuplicity.FormShow(Sender: TObject);
begin
  Core.PersistentForm1.Load(Self);
  Find;
end;

procedure TFormFindDuplicity.ReloadList;
begin
  ListView1.Items.Count := FoundItems.Count;
  ListView1.Refresh;
end;

end.
