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
    LabelResult: TLabel;
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
    procedure InitTestCases;
  public
    TestCases: TTestCases;
  end;

var
  FormTest: TFormTest;


implementation

{$R *.lfm}

uses
  UCore, UFormTestCase, UContact;

resourcestring
  SPassed = 'Passed';
  SFailed = 'Failed';

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
var
  Selected: Boolean;
  Passed: Integer;
  Failed: Integer;
  I: Integer;
begin
  Selected := Assigned(ListViewTestCases.Selected);
  ARun.Enabled := Selected;
  AShow.Enabled := Selected;

  Passed := 0;
  Failed := 0;
  for I := 0 to TestCases.Count - 1 do begin
    case TestCases[I].Result of
      trPassed: Inc(Passed);
      trFailed: Inc(Failed);
    end;
  end;
  LabelResult.Caption := SPassed + ' ' + IntToStr(Passed) + ', ' +
    SFailed + ' ' + IntToStr(Failed);
end;

procedure TFormTest.InitTestCases;
const
  VCardVersion = 'VERSION:2.1';
begin
  TestCases := TTestCases.Create;
  with TestCases do begin
    with TTestCaseLoadSave(AddNew('Load and save', TTestCaseLoadSave)) do begin
      Input := VCardBegin + LineEnding +
        VCardVersion + LineEnding +
        'N:Surname;Name' + LineEnding +
        'FN:Name Surname' + LineEnding +
        VCardEnd + LineEnding;
      Output := Input;
    end;
    with TTestCaseLoadSave(AddNew('Long text', TTestCaseLoadSave)) do begin
      Input := VCardBegin + LineEnding +
        VCardVersion + LineEnding +
        'NOTE:This is some long test which is really multi-lined each line is on dif' + LineEnding +
        ' ferent line so it is on multiple lines.' + LineEnding +
        VCardEnd + LineEnding;
      Output := Input;
    end;
    with TTestCaseLoadSave(AddNew('Multi-line', TTestCaseLoadSave)) do begin
      Input := VCardBegin + LineEnding +
        VCardVersion + LineEnding +
        'NOTE:First line\nsecond line\nempty line\n\nlast line' + LineEnding +
        VCardEnd + LineEnding;
     Output := Input;
    end;
    with TTestCaseLoadSave(AddNew('Quoted-printable load-save', TTestCaseLoadSave)) do begin
      Input := VCardBegin + LineEnding +
        VCardVersion + LineEnding +
        'FN;ENCODING=QUOTED-PRINTABLE:Jm=C3=A9no=20P=C5=99=C3=ADjmen=C3=AD' + LineEnding +
        VCardEnd + LineEnding;
      Output := Input;
    end;
    with TTestCaseLoadSave(AddNew('Quoted-printable load-save multi-line', TTestCaseLoadSave)) do begin
      Input := VCardBegin + LineEnding +
        VCardVersion + LineEnding +
        'FN;ENCODING=QUOTED-PRINTABLE:Jm=C3=A9no=20P=C5=99=C3=ADjmen=C3=ADJm=C3=A9n=' + LineEnding +
        'o=20P=C5=99=C3=ADjmen=C3=AD' + LineEnding +
        VCardEnd + LineEnding;
      Output := Input;
    end;
    with TTestCaseLoadSave(AddNew('Base64 load-save (encoding=base64)', TTestCaseLoadSave)) do begin
      Input := VCardBegin + LineEnding +
        VCardVersion + LineEnding +
        'FN;ENCODING=BASE64:VGVzdCBzdHJpbmc=' + LineEnding +
        VCardEnd + LineEnding;
      Output := Input;
    end;
    with TTestCaseLoadSave(AddNew('Base64 load-save (base64)', TTestCaseLoadSave)) do begin
      Input := VCardBegin + LineEnding +
        VCardVersion + LineEnding +
        'FN;BASE64:VGVzdCBzdHJpbmc=' + LineEnding +
        VCardEnd + LineEnding;
      Output := Input;
    end;
    with TTestCaseLoadSave(AddNew('Base64 load-save (encoding=b)', TTestCaseLoadSave)) do begin
      Input := VCardBegin + LineEnding +
        VCardVersion + LineEnding +
        'FN;ENCODING=B:VGVzdCBzdHJpbmc=' + LineEnding +
        VCardEnd + LineEnding;
      Output := Input;
    end;
    with TTestCaseLoadSave(AddNew('Base64 load-save multi-line', TTestCaseLoadSave)) do begin
      Input := VCardBegin + LineEnding +
        VCardVersion + LineEnding +
        'FN;ENCODING=BASE64:U29tZSB2ZXJ5IGxvbmcgc3RyaW5nIFNvbWUgdmVyeSBsb25nIHN0cmlu' + LineEnding +
        ' ZyBTb21lIHZlcnkgbG9uZyBzdHJpbmcgU29tZSB2ZXJ5IGxvbmcgc3RyaW5n' + LineEnding +
        VCardEnd + LineEnding;
      Output := Input;
    end;
    //AddNew('Image format', TTestCaseLoadSave);
    with TTestCaseLoadSave(AddNew('Empty', TTestCaseLoadSave)) do begin
      Input := '';
      Output := '';
    end;
    with TTestCaseLoadSave(AddNew('Begin only', TTestCaseLoadSave)) do begin
      Input := VCardBegin;
      Output := '';
    end;
    with TTestCaseLoadSave(AddNew('Missing end', TTestCaseLoadSave)) do begin
      Input := VCardBegin + LineEnding +
        VCardVersion + LineEnding +
        'N:Surname;Name' + LineEnding +
        'FN:Name Surname' + LineEnding;
      Output := '';
    end;
    with TTestCaseLoadSave(AddNew('Missing start', TTestCaseLoadSave)) do begin
      Input := VCardVersion + LineEnding +
        'N:Surname;Name' + LineEnding +
        'FN:Name Surname' + LineEnding +
        VCardEnd + LineEnding;
      Output := '';
    end;
    with TTestCaseCheckProperty(AddNew('Property FN', TTestCaseCheckProperty)) do begin
      Index := cfFullName;
      Value := 'Name Surname';
      Input := VCardBegin + LineEnding +
        VCardVersion + LineEnding +
        'FN:' + Value + LineEnding +
        VCardEnd + LineEnding;
    end;
    with TTestCaseCheckProperty(AddNew('Escaped new lines in text', TTestCaseCheckProperty)) do begin
      Index := cfNote;
      Value := 'Line' + #13#10 + 'Line';
      Input := VCardBegin + LineEnding +
        VCardVersion + LineEnding +
        'NOTE:Line\nLine' + LineEnding +
        VCardEnd + LineEnding;
    end;
    with TTestCaseCheckProperty(AddNew('Compound value', TTestCaseCheckProperty)) do begin
      Index := cfFirstName;
      Value := 'FirstName';
      Input := VCardBegin + LineEnding +
        VCardVersion + LineEnding +
        'N:Surname;FirstName;;;' + LineEnding +
        VCardEnd + LineEnding;
    end;
    with TTestCaseCheckProperty(AddNew('Quoted-printable special symbols', TTestCaseCheckProperty)) do begin
      Index := cfFullName;
      Value := 'Jméno Příjmení';
      Input := VCardBegin + LineEnding +
        VCardVersion + LineEnding +
        'FN;ENCODING=QUOTED-PRINTABLE:Jm=C3=A9no=20P=C5=99=C3=ADjmen=C3=AD' + LineEnding +
        VCardEnd + LineEnding;
    end;
    with TTestCaseCheckProperty(AddNew('Base64 special symbols', TTestCaseCheckProperty)) do begin
      Index := cfFullName;
      Value := 'Jméno Příjmení';
      Input := VCardBegin + LineEnding +
        VCardVersion + LineEnding +
        'FN;ENCODING=BASE64:Sm3DqW5vIFDFmcOtam1lbsOt' + LineEnding +
        VCardEnd + LineEnding;
    end;
  end;
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
  UpdateInterface;
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
    UpdateInterface;
  end;
end;

procedure TFormTest.FormCreate(Sender: TObject);
begin
  Core.Translator.TranslateComponentRecursive(Self);
  Core.ThemeManager1.UseTheme(Self);

  InitTestCases;
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

