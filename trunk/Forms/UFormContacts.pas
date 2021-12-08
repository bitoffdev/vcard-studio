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
    AClone: TAction;
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
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
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
    procedure ALoadFromFileExecute(Sender: TObject);
    procedure AModifyExecute(Sender: TObject);
    procedure ARemoveExecute(Sender: TObject);
    procedure ASaveToFileExecute(Sender: TObject);
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
    FUpdateCount: Integer;
    procedure FilterList(List: TFPGObjectList<TObject>);
    procedure SetContacts(AValue: TContacts);
    procedure FormContactPrevious(Sender: TObject);
    procedure FormContactNext(Sender: TObject);
    procedure DoUpdateInterface;
  public
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

begin
  if Item.Index < ListViewSort1.List.Count then
  with TContact(ListViewSort1.List[Item.Index]) do begin
    AddItem(Fields[cfFullName], True);
    AddItem(Fields[cfFirstName]);
    AddItem(Fields[cfMiddleName]);
    AddItem(Fields[cfLastName]);
    AddItem(Fields[cfTelCell]);
    AddItem(Fields[cfTelHome]);
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
    else begin
      ListViewSort1.List.Clear;
    end;
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
  ListViewFilter1.Reset;
end;

procedure TFormContacts.FormContactPrevious(Sender: TObject);
var
  I: Integer;
begin
  I := ListViewSort1.List.IndexOf(TFormContact(Sender).Contact);
  if (I <> -1) and (I > 0) then
    TFormContact(Sender).Contact := TContact(ListViewSort1.List[I - 1]);
end;

procedure TFormContacts.FormContactNext(Sender: TObject);
var
  I: Integer;
begin
  I := ListViewSort1.List.IndexOf(TFormContact(Sender).Contact);
  if (I <> -1) and (I < ListViewSort1.List.Count - 1) then
    TFormContact(Sender).Contact := TContact(ListViewSort1.List[I + 1]);
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
      FormContact.OnPrevious := FormContactPrevious;
      FormContact.OnNext := FormContactNext;
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
      FormContact.OnPrevious := FormContactPrevious;
      FormContact.OnNext := FormContactNext;
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
      FormContact.OnPrevious := FormContactPrevious;
      FormContact.OnNext := FormContactNext;
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
    SaveDialog1.FileName := TContact(ListView1.Selected.Data).Fields[cfFullName] +
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
  ListView1.BeginUpdate;
  for I := 0 to ListView1.Items.Count - 1 do
    ListView1.Items[I].Selected := True;
  //ListView1.SelectAll;
  ListView1.EndUpdate;
  EndUpdate;
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
  ALoadFromFile.Enabled := Assigned(ListView1.Selected);
  ASaveToFile.Enabled := Assigned(ListView1.Selected);
  AModify.Enabled := Assigned(ListView1.Selected);
  AClone.Enabled := Assigned(ListView1.Selected);
  ARemove.Enabled := Assigned(ListView1.Selected);
  ASelectAll.Enabled := ListView1.Items.Count > 0;
end;

end.

