unit UFormProperties;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Menus, ActnList, UContact, UListViewSort, fgl, LazUTF8;

type

  { TFormProperties }

  TFormProperties = class(TForm)
    AAdd: TAction;
    AClone: TAction;
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
    PopupMenuField: TPopupMenu;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure AAddExecute(Sender: TObject);
    procedure ACloneExecute(Sender: TObject);
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
    FProperties: TContactProperties;
    procedure FilterList(List: TFPGObjectList<TObject>);
    procedure SetProperties(AValue: TContactProperties);
  public
    property Properties: TContactProperties read FProperties write SetProperties;
    procedure ReloadList;
    procedure UpdateInterface;
  end;

var
  FormProperties: TFormProperties;


implementation

{$R *.lfm}

uses
  UFormProperty, UCore;

resourcestring
  SRemovePropertites = 'Remove fields';
  SRemovePropertiesQuery = 'Do you want to remove selected fields?';
  STotal = 'Total';
  SFiltered = 'Filtered';
  SSelected = 'Selected';

{ TFormProperties }

procedure TFormProperties.ListView1Data(Sender: TObject; Item: TListItem);

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
  with TContactProperty(ListViewSort1.List[Item.Index]) do begin
    AddItem(Name, True);
    AddItem(Attributes.DelimitedText);
    AddItem(Value);
    Item.Data := ListViewSort1.List[Item.Index];
  end;
end;

procedure TFormProperties.ListView1DblClick(Sender: TObject);
begin
  AModify.Execute;
end;

procedure TFormProperties.ListView1SelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  UpdateInterface;
end;

procedure TFormProperties.ListViewFilter1Change(Sender: TObject);
begin
  ReloadList;
  UpdateInterface;
end;

procedure TFormProperties.ListViewSort1ColumnWidthChanged(Sender: TObject);
begin
  ListViewFilter1.UpdateFromListView(ListView1);
end;

function TFormProperties.ListViewSort1CompareItem(Item1, Item2: TObject): Integer;
begin
  Result := 0;
  if Assigned(Item1) and Assigned(Item2) and (ListViewSort1.Order <> soNone) then begin
    with ListViewSort1 do
    case Column of
      0: Result := CompareString(TContactProperty(Item1).Name, TContactProperty(Item2).Name);
      1: Result := CompareString(TContactProperty(Item1).Attributes.DelimitedText, TContactProperty(Item2).Attributes.DelimitedText);
      2: Result := CompareString(TContactProperty(Item1).Value, TContactProperty(Item2).Value);
    end;
    if ListViewSort1.Order = soDown then Result := -Result;
  end else Result := 0;
end;

procedure TFormProperties.ListViewSort1Filter(ListViewSort: TListViewSort);
begin
  if Assigned(Properties) then Properties.AssignToList(ListViewSort1.List)
    else ListViewSort1.List.Clear;
  FilterList(ListViewSort1.List);
end;

procedure TFormProperties.FilterList(List: TFPGObjectList<TObject>);
var
  I: Integer;
  FoundCount: Integer;
  EnteredCount: Integer;
begin
  EnteredCount := ListViewFilter1.TextEnteredCount;
  for I := List.Count - 1 downto 0 do begin
    if List.Items[I] is TContactProperty then begin
      with TContactProperty(List.Items[I]) do begin
         with ListViewFilter1 do
         if Visible and (EnteredCount > 0) then begin
           FoundCount := 0;
           if Pos(UTF8LowerCase(StringGrid.Cells[0, 0]),
             UTF8LowerCase(TContactProperty(List.Items[I]).Name)) > 0 then Inc(FoundCount);
           if Pos(UTF8LowerCase(StringGrid.Cells[1, 0]),
             UTF8LowerCase(TContactProperty(List.Items[I]).Attributes.DelimitedText)) > 0 then Inc(FoundCount);
           if Pos(UTF8LowerCase(StringGrid.Cells[2, 0]),
             UTF8LowerCase(TContactProperty(List.Items[I]).Value)) > 0 then Inc(FoundCount);
           if FoundCount <> EnteredCount then List.Delete(I);
         end;
      end;
    end else
    if TContactProperty(List.Items[I]) is TContactProperty then begin
      List.Delete(I);
    end;
  end;
end;

procedure TFormProperties.SetProperties(AValue: TContactProperties);
begin
  if FProperties = AValue then Exit;
  FProperties := AValue;
  ReloadList;
  UpdateInterface;
end;

procedure TFormProperties.FormShow(Sender: TObject);
begin
  Core.PersistentForm1.Load(Self);
  Core.ThemeManager1.UseTheme(Self);
  Core.Translator.TranslateComponentRecursive(Self);
  ReloadList;
  UpdateInterface;
  ListViewFilter1.UpdateFromListView(ListView1);
end;

procedure TFormProperties.AAddExecute(Sender: TObject);
var
  FormProperty: TFormProperty;
  ContactProperty: TContactProperty;
begin
  FormProperty := TFormProperty.Create(nil);
  try
    ContactProperty := TContactProperty.Create;
    FormProperty.ContactProperty := ContactProperty;
    try
      if FormProperty.ShowModal = mrOK then begin
        Properties.Add(ContactProperty);
        ContactProperty := nil;
        Core.DataFile.Modified := True;
        ReloadList;
        UpdateInterface;
      end;
    finally
      if Assigned(ContactProperty) then
        ContactProperty.Free;
    end;
  finally
    FormProperty.Free;
  end;
end;

procedure TFormProperties.ACloneExecute(Sender: TObject);
var
  FormProperty: TFormProperty;
  ContactProperty: TContactProperty;
begin
  FormProperty := TFormProperty.Create(nil);
  try
    ContactProperty := TContactProperty.Create;
    ContactProperty.Assign(TContactProperty(ListView1.Selected.Data));
    FormProperty.ContactProperty := ContactProperty;
    try
      if FormProperty.ShowModal = mrOK then begin
        Properties.Add(ContactProperty);
        ContactProperty := nil;
        Core.DataFile.Modified := True;
        ReloadList;
        UpdateInterface;
      end;
    finally
      if Assigned(ContactProperty) then
        ContactProperty.Free;
    end;
  finally
    FormProperty.Free;
  end;
end;

procedure TFormProperties.AModifyExecute(Sender: TObject);
var
  FormProperty: TFormProperty;
  ContactProperty: TContactProperty;
begin
  FormProperty := TFormProperty.Create(nil);
  try
    ContactProperty := TContactProperty.Create;
    try
      ContactProperty.Assign(TContactProperty(ListView1.Selected.Data));
      FormProperty.ContactProperty := ContactProperty;
      if FormProperty.ShowModal = mrOK then begin
        TContactProperty(ListView1.Selected.Data).Assign(ContactProperty);
        Core.DataFile.Modified := True;
        ReloadList;
        UpdateInterface;
      end;
    finally
      ContactProperty.Free;
    end;
  finally
    FormProperty.Free;
  end;
end;

procedure TFormProperties.ARemoveExecute(Sender: TObject);
var
  I: Integer;
begin
  if Assigned(ListView1.Selected) then
  if MessageDlg(SRemovePropertites, SRemovePropertiesQuery,
    TMsgDlgType.mtConfirmation, [mbCancel, mbOk], 0) = mrOk then begin
    for I := ListView1.Items.Count - 1 downto 0 do
      if ListView1.Items[I].Selected then begin
        Properties.Delete(I);
      end;
    Core.DataFile.Modified := True;
    ReloadList;
    UpdateInterface;
  end;
end;

procedure TFormProperties.ASelectAllExecute(Sender: TObject);
begin
  ListView1.SelectAll;
  UpdateInterface;
end;

procedure TFormProperties.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  Core.PersistentForm1.Save(Self);
end;

procedure TFormProperties.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  FProperties := nil;
  for I := 0 to ToolBar1.ButtonCount - 1 do begin
    ToolBar1.Buttons[I].ShowHint := True;
    ToolBar1.Buttons[I].Hint := ToolBar1.Buttons[I].Caption;
  end;
end;

procedure TFormProperties.ReloadList;
begin
  ListViewSort1.Refresh;
end;

procedure TFormProperties.UpdateInterface;
var
  Text: string;
  SelectedCount: Integer;
begin
  AAdd.Enabled := Assigned(Properties);
  AModify.Enabled := Assigned(Properties) and Assigned(ListView1.Selected);
  ARemove.Enabled := Assigned(Properties) and Assigned(ListView1.Selected);

  Text := '';
  if Assigned(Properties) then begin
    Text := STotal + ': ' + IntToStr(Properties.Count);
    if ListView1.Items.Count < Properties.Count then
      Text := Text + ', ' + SFiltered + ': ' + IntToStr(ListView1.Items.Count);
    SelectedCount := ListView1.SelCount;
    if SelectedCount > 0 then
      Text := Text + ', ' + SSelected + ': ' + IntToStr(SelectedCount);
  end;
  StatusBar1.Panels[0].Text := Text;
end;

end.

