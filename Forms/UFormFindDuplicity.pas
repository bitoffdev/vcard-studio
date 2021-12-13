unit UFormFindDuplicity;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, ActnList, Menus, fgl, UContact;

type

  { TFoundItem }

  TFoundItem = class
    Field: string;
    Contacts: TContacts;
    constructor Create;
    destructor Destroy; override;
  end;

  { TFoundItems }

  TFoundItems = class(TFPGObjectList<TFoundItem>)
    function SearchByField(Field: string): TFoundItem;
  end;

  { TFormFindDuplicity }

  TFormFindDuplicity = class(TForm)
    AShowContacts: TAction;
    ActionList1: TActionList;
    ButtonMerge: TButton;
    ComboBoxField: TComboBox;
    Label1: TLabel;
    ListView1: TListView;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    procedure AShowContactsExecute(Sender: TObject);
    procedure ButtonMergeClick(Sender: TObject);
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
    ContactFieldIndex: TContactFieldIndex;
    procedure Find;
    procedure ReloadList;
    property Contacts: TContacts read FContacts write SetContacts;
  end;

var
  FormFindDuplicity: TFormFindDuplicity;


implementation

{$R *.lfm}

uses
  UCore, UFormContacts;

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
  FreeAndNil(Contacts);
  inherited;
end;

{ TFormFindDuplicity }

procedure TFormFindDuplicity.ListView1Data(Sender: TObject; Item: TListItem);
begin
  if Item.Index < FoundItems.Count then
  with TFoundItem(FoundItems[Item.Index]) do begin
    Item.Caption := Field;
    Item.Data := FoundItems[Item.Index];
    Item.SubItems.Add(Contacts.ToString);
    Item.SubItems.Add(IntToStr(Contacts.Count));
  end;
end;

procedure TFormFindDuplicity.SetContacts(AValue: TContacts);
var
  ContactField: TContactField;
  Items: TStringList;
  I: Integer;
begin
  if FContacts = AValue then Exit;
  FContacts := AValue;
  if Assigned(FContacts) then begin
    Items := TStringList.Create;
    try
      Contacts.ContactsFile.Fields.LoadToStrings(Items);

      // Remove fields which are not used in contacts
      for I := Items.Count - 1 downto 0 do
        if Contacts.CountByField(TContactField(Items.Objects[I]).Index) = 0 then
          Items.Delete(I);

      ComboBoxField.Items.Assign(Items);
    finally
      Items.Free;
    end;
    ContactField := Contacts.ContactsFile.Fields.GetByIndex(ContactFieldIndex);
    ComboBoxField.ItemIndex := ComboBoxField.Items.IndexOfObject(ContactField);
    if (ComboBoxField.Items.Count > 0) and (ComboBoxField.ItemIndex = -1) then
      ComboBoxField.ItemIndex := 0;
  end else ComboBoxField.Clear;
end;

function FoundItemsSort(const Item1, Item2: TFoundItem): Integer;
begin
  if Item1.Contacts.Count < Item2.Contacts.Count then Result := 1
  else if Item1.Contacts.Count > Item2.Contacts.Count then Result := -1
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
    FieldName := Contacts[I].Fields[ContactFieldIndex];
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
  Core.Translator.TranslateComponentRecursive(Self);
  Core.ThemeManager1.UseTheme(Self);
  ContactFieldIndex := cfFullName;
end;

procedure TFormFindDuplicity.ComboBoxFieldChange(Sender: TObject);
begin
  if ComboBoxField.ItemIndex <> -1 then
    ContactFieldIndex := TContactField(ComboBoxField.Items.Objects[ComboBoxField.ItemIndex]).Index
    else ContactFieldIndex := cfTelCell;
  Find;
end;

procedure TFormFindDuplicity.AShowContactsExecute(Sender: TObject);
var
  Form: TFormContacts;
  I: Integer;
begin
  if Assigned(ListView1.Selected) then begin
    Form := TFormContacts.Create(nil);
    Form.Contacts := TContacts.Create(False);
    Form.Contacts.ContactsFile := Contacts.ContactsFile;
    with TFoundItem(ListView1.Selected.Data) do
      for I := 0 to Contacts.Count - 1 do
        Form.Contacts.Add(Contacts[I]);
    Form.ShowModal;
    with TFoundItem(ListView1.Selected.Data) do begin
      // Remove all deleted
      for I := 0 to Contacts.Count - 1 do
        if Form.Contacts.IndexOf(Contacts[I]) = -1 then begin
          Form.Contacts.Remove(Contacts[I]);
          Self.Contacts.Remove(Contacts[I]);
          Self.Contacts.ContactsFile.Modified := True;
        end;

      // Add newly added
      for I := 0 to Form.Contacts.Count - 1 do
        if Contacts.IndexOf(Form.Contacts[I]) = -1 then begin
          Form.Contacts.Add(Form.Contacts[I]);
          Self.Contacts.Add(Form.Contacts[I]);
          Self.Contacts.ContactsFile.Modified := True;
        end;
    end;
    Form.Contacts.Free;
    Form.Free;
    Find;
  end;
end;

procedure TFormFindDuplicity.ButtonMergeClick(Sender: TObject);
var
  TempContacts: TContactsFile;
  I: Integer;
begin
  TempContacts := TContactsFile.Create;
  try
    for I := 0 to Contacts.Count - 1 do
      TempContacts.Contacts.Merge(Contacts[I], TContactField(ComboBoxField.Items.Objects[ComboBoxField.ItemIndex]).Index);
    Contacts.Assign(TempContacts.Contacts);
    Find;
  finally
    TempContacts.Free;
  end;
end;

procedure TFormFindDuplicity.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  Core.PersistentForm1.Save(Self);
end;

procedure TFormFindDuplicity.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FoundItems);
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

