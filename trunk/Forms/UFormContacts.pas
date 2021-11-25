unit UFormContacts;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Menus, ActnList, UContact, UListViewSort, fgl, LazUTF8;

type

  { TFormContacts }

  TFormContacts = class(TForm)
    AAdd: TAction;
    ASelectAll: TAction;
    ARemove: TAction;
    AModify: TAction;
    ActionList1: TActionList;
    ListView1: TListView;
    ListViewFilter1: TListViewFilter;
    ListViewSort1: TListViewSort;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PopupMenuContact: TPopupMenu;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure AAddExecute(Sender: TObject);
    procedure AModifyExecute(Sender: TObject);
    procedure ARemoveExecute(Sender: TObject);
    procedure ASelectAllExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListViewFilter1Change(Sender: TObject);
    procedure ListViewSort1ColumnWidthChanged(Sender: TObject);
    function ListViewSort1CompareItem(Item1, Item2: TObject): Integer;
    procedure ListViewSort1Filter(ListViewSort: TListViewSort);
  private
    FContacts: TContacts;
    procedure FilterList(List: TFPGObjectList<TObject>);
    procedure SetContacts(AValue: TContacts);
  public
    property Contacts: TContacts read FContacts write SetContacts;
    procedure ReloadList;
    procedure UpdateInterface;
  end;

var
  FormContacts: TFormContacts;


implementation

{$R *.lfm}

uses
  UFormContact, UCore;

resourcestring
  SRemoveContacts = 'Remove contacts';
  SRemoveContactsQuery = 'Do you want to remove selected contacts?';
  STotal = 'Total';
  SFiltered = 'Filtered';
  SSelected = 'Selected';

{ TFormContacts }

procedure TFormContacts.ListView1Data(Sender: TObject; Item: TListItem);
begin
  if Item.Index < ListViewSort1.List.Count then
  with TContact(ListViewSort1.List[Item.Index]) do begin
    Item.Caption := Fields[cfFullName];
    Item.SubItems.Add(Fields[cfFirstName]);
    Item.SubItems.Add(Fields[cfMiddleName]);
    Item.SubItems.Add(Fields[cfLastName]);
    Item.SubItems.Add(Fields[cfTelCell]);
    Item.SubItems.Add(Fields[cfTelHome]);
    Item.Data := ListViewSort1.List[Item.Index];
  end;
end;

procedure TFormContacts.ListView1DblClick(Sender: TObject);
begin
  AModify.Execute;
end;

procedure TFormContacts.ListView1SelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  UpdateInterface;
end;

procedure TFormContacts.ListViewFilter1Change(Sender: TObject);
begin
  ReloadList;
  UpdateInterface;
end;

procedure TFormContacts.ListViewSort1ColumnWidthChanged(Sender: TObject);
begin
  ListViewFilter1.UpdateFromListView(ListView1);
end;

function TFormContacts.ListViewSort1CompareItem(Item1, Item2: TObject): Integer;
begin
  Result := 0;
  if Assigned(Item1) and Assigned(Item2) and (ListViewSort1.Order <> soNone) then begin
    with ListViewSort1 do
    case Column of
      0: Result := CompareString(TContact(Item1).Fields[cfFullName], TContact(Item2).Fields[cfFullName]);
      1: Result := CompareString(TContact(Item1).Fields[cfFirstName], TContact(Item2).Fields[cfFirstName]);
      2: Result := CompareString(TContact(Item1).Fields[cfMiddleName], TContact(Item2).Fields[cfMiddleName]);
      3: Result := CompareString(TContact(Item1).Fields[cfLastName], TContact(Item2).Fields[cfLastName]);
      4: Result := CompareString(TContact(Item1).Fields[cfTelCell], TContact(Item2).Fields[cfTelCell]);
      5: Result := CompareString(TContact(Item1).Fields[cfTelHome], TContact(Item2).Fields[cfTelHome]);
    end;
    if ListViewSort1.Order = soDown then Result := -Result;
  end else Result := 0;
end;

procedure TFormContacts.ListViewSort1Filter(ListViewSort: TListViewSort);
begin
  if Assigned(Contacts) then Contacts.AssignToList(ListViewSort1.List)
    else ListViewSort1.List.Clear;
  FilterList(ListViewSort1.List);
end;

procedure TFormContacts.FilterList(List: TFPGObjectList<TObject>);
var
  I: Integer;
  FoundCount: Integer;
  EnteredCount: Integer;
begin
  EnteredCount := ListViewFilter1.TextEnteredCount;
  for I := List.Count - 1 downto 0 do begin
    if List.Items[I] is TContact then begin
      with TContact(List.Items[I]) do begin
         with ListViewFilter1 do
         if Visible and (EnteredCount > 0) then begin
           FoundCount := 0;
           if Pos(UTF8LowerCase(StringGrid.Cells[0, 0]),
             UTF8LowerCase(TContact(List.Items[I]).Fields[cfFullName])) > 0 then Inc(FoundCount);
           if Pos(UTF8LowerCase(StringGrid.Cells[1, 0]),
             UTF8LowerCase(TContact(List.Items[I]).Fields[cfFirstName])) > 0 then Inc(FoundCount);
           if Pos(UTF8LowerCase(StringGrid.Cells[2, 0]),
             UTF8LowerCase(TContact(List.Items[I]).Fields[cfMiddleName])) > 0 then Inc(FoundCount);
           if Pos(UTF8LowerCase(StringGrid.Cells[3, 0]),
             UTF8LowerCase(TContact(List.Items[I]).Fields[cfLastName])) > 0 then Inc(FoundCount);
           if Pos(UTF8LowerCase(StringGrid.Cells[4, 0]),
             UTF8LowerCase(TContact(List.Items[I]).Fields[cfTelCell])) > 0 then Inc(FoundCount);
           if Pos(UTF8LowerCase(StringGrid.Cells[5, 0]),
             UTF8LowerCase(TContact(List.Items[I]).Fields[cfTelHome])) > 0 then Inc(FoundCount);
           if FoundCount <> EnteredCount then List.Delete(I);
         end;
      end;
    end else
    if TContact(List.Items[I]) is TContact then begin
      List.Delete(I);
    end;
  end;
end;

procedure TFormContacts.SetContacts(AValue: TContacts);
begin
  if FContacts = AValue then Exit;
  FContacts := AValue;
  ReloadList;
  UpdateInterface;
end;

procedure TFormContacts.FormShow(Sender: TObject);
begin
  Core.PersistentForm1.Load(Self);
  Core.ThemeManager1.UseTheme(Self);
  Core.Translator.TranslateComponentRecursive(Self);
  ReloadList;
  UpdateInterface;
  ListViewFilter1.UpdateFromListView(ListView1);
end;

procedure TFormContacts.AAddExecute(Sender: TObject);
var
  FormContact: TFormContact;
  Contact: TContact;
begin
  FormContact := TFormContact.Create(nil);
  try
  if FormContact.ShowModal = mrOK then begin
    Contact := TContact.Create;
    Contact.Parent := Contacts.ContactsFile;
    FormContact.SaveData(Contact);
    Contacts.Add(Contact);
    Core.DataFile.Modified := True;
    ReloadList;
    UpdateInterface;
  end;
  finally
    FormContact.Free;
  end;
end;

procedure TFormContacts.AModifyExecute(Sender: TObject);
var
  FormContact: TFormContact;
begin
  FormContact := TFormContact.Create(nil);
  try
    FormContact.LoadData(TContact(ListView1.Selected.Data));
    if FormContact.ShowModal = mrOK then begin
      FormContact.SaveData(TContact(ListView1.Selected.Data));
      Core.DataFile.Modified := True;
      ReloadList;
      UpdateInterface;
    end;
  finally
    FormContact.Free;
  end;
end;

procedure TFormContacts.ARemoveExecute(Sender: TObject);
var
  I: Integer;
begin
  if Assigned(ListView1.Selected) then
  if MessageDlg(SRemoveContacts, SRemoveContactsQuery,
    TMsgDlgType.mtConfirmation, [mbCancel, mbOk], 0) = mrOk then begin
    for I := ListView1.Items.Count - 1 downto 0 do
      if ListView1.Items[I].Selected then begin
        Contacts.Delete(I);
      end;
    Core.DataFile.Modified := True;
    ReloadList;
    UpdateInterface;
  end;
end;

procedure TFormContacts.ASelectAllExecute(Sender: TObject);
begin
  ListView1.SelectAll;
  UpdateInterface;
end;

procedure TFormContacts.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  Core.PersistentForm1.Save(Self);
end;

procedure TFormContacts.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  FContacts := nil;
  for I := 0 to ToolBar1.ButtonCount - 1 do begin
    ToolBar1.Buttons[I].ShowHint := True;
    ToolBar1.Buttons[I].Hint := ToolBar1.Buttons[I].Caption;
  end;
end;

procedure TFormContacts.ReloadList;
begin
  ListViewSort1.Refresh;
end;

procedure TFormContacts.UpdateInterface;
var
  Text: string;
  SelectedCount: Integer;
begin
  AAdd.Enabled := Assigned(Contacts);
  AModify.Enabled := Assigned(Contacts) and Assigned(ListView1.Selected);
  ARemove.Enabled := Assigned(Contacts) and Assigned(ListView1.Selected);

  Text := '';
  if Assigned(Contacts) then begin
    Text := STotal + ': ' + IntToStr(Contacts.Count);
    if ListView1.Items.Count < Contacts.Count then
      Text := Text + ', ' + SFiltered + ': ' + IntToStr(ListView1.Items.Count);
    SelectedCount := ListView1.SelCount;
    if SelectedCount > 0 then
      Text := Text + ', ' + SSelected + ': ' + IntToStr(SelectedCount);
  end;
  StatusBar1.Panels[0].Text := Text;
end;

end.

