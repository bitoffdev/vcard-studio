unit UFormContacts;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Menus, ActnList, UContact, UListViewSort, fgl, LazUTF8, Clipbrd;

type

  { TFormContacts }

  TFormContacts = class(TForm)
    AAdd: TAction;
    AClone: TAction;
    ACopy: TAction;
    ACut: TAction;
    APaste: TAction;
    ALoadFromFile: TAction;
    ASaveToFile: TAction;
    ASelectAll: TAction;
    ARemove: TAction;
    AModify: TAction;
    ActionList1: TActionList;
    ListView1: TListView;
    ListViewFilter1: TListViewFilter;
    ListViewSort1: TListViewSort;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupMenuContact: TPopupMenu;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    procedure AAddExecute(Sender: TObject);
    procedure ACloneExecute(Sender: TObject);
    procedure ACopyExecute(Sender: TObject);
    procedure ACutExecute(Sender: TObject);
    procedure ALoadFromFileExecute(Sender: TObject);
    procedure AModifyExecute(Sender: TObject);
    procedure APasteExecute(Sender: TObject);
    procedure ARemoveExecute(Sender: TObject);
    procedure ASaveToFileExecute(Sender: TObject);
    procedure ASelectAllExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
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
    FUpdateCount: Integer;
    procedure FilterList(List: TFPGObjectList<TObject>);
    procedure SetContacts(AValue: TContacts);
    function GetPreviousContact(Contact: TContact): TContact;
    function GetNextContact(Contact: TContact): TContact;
    procedure DoUpdateInterface;
    procedure UpdateColumns;
  public
    ListViewColumns: TContactFieldIndexes;
    FilterItems: TContactFilterItems;
    property Contacts: TContacts read FContacts write SetContacts;
    procedure ReloadList;
    procedure BeginUpdate;
    procedure EndUpdate;
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
  SEndUpdateTooLow = 'Update counter error';

{ TFormContacts }

procedure TFormContacts.ListView1Data(Sender: TObject; Item: TListItem);

  procedure AddItem(Text: string; IsCaption: Boolean = False);
  begin
    if IsCaption then begin
      if Text <> '' then Item.Caption := Text
        else Item.Caption := ' ';
    end else begin
      if Text <> '' then Item.SubItems.Add(Text)
        else Item.SubItems.Add(' ');
    end;
  end;

var
  I: Integer;
begin
  if Item.Index < ListViewSort1.List.Count then
  with TContact(ListViewSort1.List[Item.Index]) do begin
    for I := 0 to ListViewColumns.Count - 1 do begin
      AddItem(Fields[ListViewColumns[I]], I = 0);
    end;
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
var
  I: Integer;
begin
  // Load filter StringGrid cells into filter
  FilterItems.Clear;
  for I := 0 to ListViewColumns.Count - 1 do
    if I < ListViewFilter1.StringGrid.ColCount then
      if ListViewFilter1.StringGrid.Cells[I, 0] <> '' then
        FilterItems.AddNew(ListViewColumns[I], ListViewFilter1.StringGrid.Cells[I, 0]);

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
    Result := CompareString(TContact(Item1).Fields[ListViewColumns[Column]], TContact(Item2).Fields[ListViewColumns[Column]]);
    if ListViewSort1.Order = soDown then Result := -Result;
  end else Result := 0;
end;

procedure TFormContacts.ListViewSort1Filter(ListViewSort: TListViewSort);
begin
  if Assigned(Contacts) then Contacts.AssignToList(ListViewSort1.List)
    else begin
      ListViewSort1.List.Clear;
    end;
  FilterList(ListViewSort1.List);
end;

procedure TFormContacts.FilterList(List: TFPGObjectList<TObject>);
var
  I: Integer;
  J: Integer;
  K: Integer;
  FoundCount: Integer;
begin
  for I := List.Count - 1 downto 0 do begin
    if List.Items[I] is TContact then begin
      with TContact(List.Items[I]) do begin
        FoundCount := 0;
        for J := 0 to FilterItems.Count - 1 do begin
          if FilterItems[J].FieldIndex = cfNone then begin
            for K := 0 to TContact(List.Items[I]).Parent.Fields.Count - 1 do begin
              if Pos(UTF8LowerCase(FilterItems[J].Value),
                UTF8LowerCase(TContact(List.Items[I]).Fields[TContact(List.Items[I]).Parent.Fields[K].Index])) > 0 then begin
                  Inc(FoundCount);
                  Break;
                end;
            end;
          end else begin
            if Pos(UTF8LowerCase(FilterItems[J].Value),
              UTF8LowerCase(TContact(List.Items[I]).Fields[FilterItems[J].FieldIndex])) > 0 then
                Inc(FoundCount);
          end;
        end;
        if FoundCount <> FilterItems.Count then List.Delete(I);
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
  ListViewFilter1.Reset;
end;

function TFormContacts.GetPreviousContact(Contact: TContact): TContact;
var
  I: Integer;
begin
  I := ListViewSort1.List.IndexOf(Contact);
  if (I <> -1) and (I > 0) then
    Result := TContact(ListViewSort1.List[I - 1])
    else Result := nil;
end;

function TFormContacts.GetNextContact(Contact: TContact): TContact;
var
  I: Integer;
begin
  I := ListViewSort1.List.IndexOf(Contact);
  if (I <> -1) and (I < ListViewSort1.List.Count - 1) then
    Result := TContact(ListViewSort1.List[I + 1])
    else Result := nil;
end;

procedure TFormContacts.DoUpdateInterface;
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

procedure TFormContacts.UpdateColumns;
var
  I: Integer;
  Field: TContactField;
begin
  while ListView1.Columns.Count < ListViewColumns.Count do
    ListView1.Columns.Add;
  while ListView1.Columns.Count > ListViewColumns.Count do
    ListView1.Columns.Delete(ListView1.Columns.Count - 1);
  for I := 0 to ListView1.Columns.Count - 1 do begin
    if Assigned(Contacts) and Assigned(Contacts.ContactsFile) then begin
      Field := Contacts.ContactsFile.Fields.GetByIndex(ListViewColumns[I]);
      if Assigned(Field) then
        ListView1.Columns[I].Caption := Field.Title;
    end;
  end;
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
    Contact := TContact.Create;
    try
      Contact.Parent := Contacts.ContactsFile;
      FormContact.Contact := Contact;
      FormContact.OnGetPrevious := GetPreviousContact;
      FormContact.OnGetNext := GetNextContact;
      if FormContact.ShowModal = mrOK then begin
        Contacts.Add(Contact);
        Core.DataFile.Modified := True;
        ReloadList;
        UpdateInterface;
        Contact := nil;
      end;
    finally
      if Assigned(Contact) then
        Contact.Free;
    end;
  finally
    FormContact.Free;
  end;
end;

procedure TFormContacts.ACloneExecute(Sender: TObject);
var
  FormContact: TFormContact;
  Contact: TContact;
begin
  FormContact := TFormContact.Create(nil);
  try
    Contact := TContact.Create;
    try
      Contact.Parent := Contacts.ContactsFile;
      Contact.Assign(TContact(ListView1.Selected.Data));
      FormContact.Contact := Contact;
      FormContact.OnGetPrevious := GetPreviousContact;
      FormContact.OnGetNext := GetNextContact;
      if FormContact.ShowModal = mrOK then begin
        Contacts.Add(Contact);
        Contact := nil;
        Core.DataFile.Modified := True;
        ReloadList;
        UpdateInterface;
      end;
    finally
      if Assigned(Contact) then
        Contact.Free;
    end;
  finally
    FormContact.Free;
  end;
end;

procedure TFormContacts.ACopyExecute(Sender: TObject);
var
  Text: string;
  Strings: TStringList;
  I: Integer;
begin
  Strings := TStringList.Create;
  try
  Text := '';
  for I := 0 to ListView1.Items.Count - 1 do
    if ListView1.Items[I].Selected then begin
      Strings.Clear;
      TContact(ListView1.Items[I].Data).SaveToStrings(Strings);
      Text := Text + Strings.Text;
    end;
    Clipboard.AsText := Text;
  finally
    Strings.Free;
  end;
end;

procedure TFormContacts.ACutExecute(Sender: TObject);
var
  Text: string;
  Strings: TStringList;
  I: Integer;
begin
  Strings := TStringList.Create;
  try
    Text := '';
    for I := 0 to ListView1.Items.Count - 1 do
      if ListView1.Items[I].Selected then begin
        Strings.Clear;
        TContact(ListView1.Items[I].Data).SaveToStrings(Strings);
        Text := Text + Strings.Text;
      end;
    Clipboard.AsText := Text;
    for I := 0 to ListView1.Items.Count - 1 do
      if ListView1.Items[I].Selected then begin
        Contacts.Delete(Contacts.IndexOf(ListView1.Items[I].Data));
      end;
    ReloadList;
    ListView1.ClearSelection;
    UpdateInterface;
  finally
    Strings.Free;
  end;
end;

procedure TFormContacts.ALoadFromFileExecute(Sender: TObject);
var
  TempFile: TContactsFile;
begin
  if Assigned(ListView1.Selected) then begin
    TempFile := TContactsFile.Create;
    try
      OpenDialog1.Filter := TempFile.GetFileFilter;
      OpenDialog1.DefaultExt := TempFile.GetFileExt;
    finally
      TempFile.Free;
    end;
    OpenDialog1.InitialDir := ExtractFileDir(Core.LastContactFileName);
    OpenDialog1.FileName := ExtractFileName(Core.LastContactFileName);
    if OpenDialog1.Execute then begin
      TContact(ListView1.Selected.Data).LoadFromFile(OpenDialog1.FileName);
      Core.LastContactFileName := OpenDialog1.FileName;
      ReloadList;
    end;
  end;
end;

procedure TFormContacts.AModifyExecute(Sender: TObject);
var
  FormContact: TFormContact;
  Contact: TContact;
begin
  FormContact := TFormContact.Create(nil);
  try
    Contact := TContact.Create;
    try
      Contact.Parent := Contacts.ContactsFile;
      Contact.Assign(TContact(ListView1.Selected.Data));
      FormContact.Contact := Contact;
      FormContact.OnGetPrevious := GetPreviousContact;
      FormContact.OnGetNext := GetNextContact;
      if FormContact.ShowModal = mrOK then begin
        TContact(ListView1.Selected.Data).Assign(Contact);
        Core.DataFile.Modified := True;
        ReloadList;
        UpdateInterface;
      end;
    finally
      Contact.Free;
    end;
  finally
    FormContact.Free;
  end;
end;

procedure TFormContacts.APasteExecute(Sender: TObject);
var
  PasteContacts: TContactsFile;
  Lines: TStringList;
begin
  PasteContacts := TContactsFile.Create;
  Lines := TStringList.Create;
  try
    Lines.Text := Clipboard.AsText;
    PasteContacts.LoadFromStrings(Lines);
    if PasteContacts.Contacts.Count > 0 then begin
      if Assigned(ListView1.Selected) then begin
        Contacts.InsertContacts(Contacts.IndexOf(ListView1.Selected.Data),
          PasteContacts.Contacts);
      end else Contacts.AddContacts(PasteContacts.Contacts);
      Core.DataFile.Modified := True;
      ReloadList;
      UpdateInterface;
    end;
  finally
    Lines.Free;
    PasteContacts.Free;
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
        Contacts.Delete(Contacts.IndexOf(ListView1.Items[I].Data));
      end;
    Core.DataFile.Modified := True;
    ReloadList;
    UpdateInterface;
  end;
end;

procedure TFormContacts.ASaveToFileExecute(Sender: TObject);
var
  TempFile: TContactsFile;
begin
  if Assigned(ListView1.Selected) then begin
    TempFile := TContactsFile.Create;
    try
      SaveDialog1.Filter := TempFile.GetFileFilter;
      SaveDialog1.DefaultExt := TempFile.GetFileExt;
    finally
      TempFile.Free;
    end;
    SaveDialog1.InitialDir := ExtractFileDir(Core.LastContactFileName);
    SaveDialog1.FileName := TContact(ListView1.Selected.Data).FullNameToFileName +
      VCardFileExt;
    if SaveDialog1.Execute then begin
      TContact(ListView1.Selected.Data).SaveToFile(SaveDialog1.FileName);
      Core.LastContactFileName := SaveDialog1.FileName;
    end;
  end;
end;

procedure TFormContacts.ASelectAllExecute(Sender: TObject);
var
  I: Integer;
begin
  BeginUpdate;
  try
    ListView1.BeginUpdate;
    try
      for I := 0 to ListView1.Items.Count - 1 do
        ListView1.Items[I].Selected := True;
      //ListView1.SelectAll;
    finally
      ListView1.EndUpdate;
    end;
  finally
    EndUpdate;
  end;
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
  FilterItems := TContactFilterItems.Create;

  ListViewColumns := TContactFieldIndexes.Create;
  ListViewColumns.Add(cfFullName);
  ListViewColumns.Add(cfFirstName);
  ListViewColumns.Add(cfMiddleName);
  ListViewColumns.Add(cfLastName);
  ListViewColumns.Add(cfTel);
  ListViewColumns.Add(cfTelCell);
  ListViewColumns.Add(cfTelHome);
  ListViewColumns.Add(cfTelWork);
  ListViewColumns.Add(cfEmailWork);

  FContacts := nil;
  for I := 0 to ToolBar1.ButtonCount - 1 do begin
    ToolBar1.Buttons[I].ShowHint := True;
    ToolBar1.Buttons[I].Hint := ToolBar1.Buttons[I].Caption;
  end;
end;

procedure TFormContacts.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ListViewColumns);
  FreeAndNil(FilterItems);
end;

procedure TFormContacts.FormResize(Sender: TObject);
begin
  ListViewFilter1.UpdateFromListView(ListView1);
end;

procedure TFormContacts.ReloadList;
begin
  ListViewSort1.Refresh;
end;

procedure TFormContacts.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TFormContacts.EndUpdate;
begin
  if FUpdateCount <= 0 then raise Exception(SEndUpdateTooLow);
  Dec(FUpdateCount);
  if FUpdateCount = 0 then DoUpdateInterface;
end;

procedure TFormContacts.UpdateInterface;
begin
  if FUpdateCount = 0 then DoUpdateInterface;
  AAdd.Enabled := Assigned(Contacts);
  AModify.Enabled := Assigned(Contacts) and Assigned(ListView1.Selected);
  AClone.Enabled := Assigned(Contacts) and Assigned(ListView1.Selected);
  ARemove.Enabled := Assigned(Contacts) and Assigned(ListView1.Selected);
  ASelectAll.Enabled := ListView1.Items.Count > 0;
  ALoadFromFile.Enabled := Assigned(Contacts) and Assigned(ListView1.Selected);
  ASaveToFile.Enabled := Assigned(Contacts) and Assigned(ListView1.Selected);
  ACopy.Enabled := Assigned(Contacts) and Assigned(ListView1.Selected);
  ACut.Enabled := Assigned(Contacts) and Assigned(ListView1.Selected);
  APaste.Enabled := Assigned(Contacts) and (Clipboard.AsText <> '');

  UpdateColumns;
end;

end.

