unit UFormTest;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ActnList, Menus, UTest;

type

  { TFormTest }

  TFormTest = class(TForm)
    ARun: TAction;
    AShow: TAction;
    ActionList1: TActionList;
    ButtonRun: TButton;
    ListViewTestCases: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenuTest: TPopupMenu;
    procedure ARunExecute(Sender: TObject);
    procedure AShowExecute(Sender: TObject);
    procedure ButtonRunClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewTestCasesData(Sender: TObject; Item: TListItem);
    procedure ListViewTestCasesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    procedure ReloadList;
    procedure UpdateInterface;
  public
    TestCases: TTestCases;
  end;

var
  FormTest: TFormTest;


implementation

{$R *.lfm}

uses
  UCore, UFormTestCase;

{ TFormTest }

procedure TFormTest.ListViewTestCasesData(Sender: TObject; Item: TListItem);
begin
  if Item.Index < TestCases.Count then
  with TestCases[Item.Index] do begin
    Item.Caption := Name;
    Item.Data := TestCases[Item.Index];
    Item.SubItems.Add(ResultText[Result]);
  end;
end;

procedure TFormTest.ListViewTestCasesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdateInterface;
end;

procedure TFormTest.ReloadList;
begin
  ListViewTestCases.Items.Count := TestCases.Count;
  ListViewTestCases.Refresh;
end;

procedure TFormTest.UpdateInterface;
begin
  ARun.Enabled := Assigned(ListViewTestCases.Selected);
  AShow.Enabled := Assigned(ListViewTestCases.Selected);
end;

procedure TFormTest.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Core.PersistentForm1.Save(Self);
end;

procedure TFormTest.ButtonRunClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to TestCases.Count - 1 do
    TestCases[I].Run;
  ReloadList;
end;

procedure TFormTest.AShowExecute(Sender: TObject);
begin
  if Assigned(ListViewTestCases.Selected) then
  with TFormTestCase.Create(nil) do
  try
    MemoLog.Text := TTestCase(ListViewTestCases.Selected.Data).Log;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TFormTest.ARunExecute(Sender: TObject);
begin
  if Assigned(ListViewTestCases.Selected) then begin
    TTestCase(ListViewTestCases.Selected.Data).Run;
    ReloadList;
  end;
end;

procedure TFormTest.FormCreate(Sender: TObject);
begin
  TestCases := TTestCases.Create;
  with TestCases do begin
    with TTestCaseLoadSave(AddNew('Load and save', TTestCaseLoadSave)) do begin
      Input := 'BEGIN:VCARD' + LineEnding +
        'VERSION:2.1' + LineEnding +
        'N:Surname;Name' + LineEnding +
        'FN:Name Surname' + LineEnding +
        'END:VCARD' + LineEnding;
      Output := Input;
    end;
    with TTestCaseLoadSave(AddNew('Multi-line', TTestCaseLoadSave)) do begin
      Input := 'BEGIN:VCARD' + LineEnding +
        'VERSION:2.1' + LineEnding +
        'NOTE:This is some long test which is really multi-lined\neach line\nis on' + LineEnding +
        ' different\nline so it is on multiple\nlines.'  + LineEnding +
        'END:VCARD' + LineEnding;
      Output := Input;
    end;
    AddNew('Encoding base64', TTestCaseLoadSave);
    AddNew('Encoding quoted-printable', TTestCaseLoadSave);
    AddNew('Image format', TTestCaseLoadSave);
    with TTestCaseLoadSave(AddNew('Empty', TTestCaseLoadSave)) do begin
      Input := '';
      Output := '';
    end;
    with TTestCaseLoadSave(AddNew('Begin only', TTestCaseLoadSave)) do begin
      Input := 'BEGIN:VCARD';
      Output := '';
    end;
    with TTestCaseLoadSave(AddNew('Missing end', TTestCaseLoadSave)) do begin
      Input := 'BEGIN:VCARD' + LineEnding +
        'VERSION:2.1' + LineEnding +
        'N:Surname;Name' + LineEnding +
        'FN:Name Surname' + LineEnding;
      Output := '';
    end;
  end;
end;

procedure TFormTest.FormDestroy(Sender: TObject);
begin
  FreeAndNil(TestCases);
end;

procedure TFormTest.FormShow(Sender: TObject);
begin
  Core.PersistentForm1.Load(Self);
  ReloadList;
  UpdateInterface;
end;

end.

