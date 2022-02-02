unit UFormTest;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  UTest;

type

  { TFormTest }

  TFormTest = class(TForm)
    ButtonRun: TButton;
    ListViewTestCases: TListView;
    procedure ButtonRunClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewTestCasesData(Sender: TObject; Item: TListItem);
  private
    procedure ReloadList;
  public
    TestCases: TTestCases;
  end;

var
  FormTest: TFormTest;


implementation

{$R *.lfm}

uses
  UCore;

{ TFormTest }

procedure TFormTest.ListViewTestCasesData(Sender: TObject; Item: TListItem);
begin
  if Item.Index < TestCases.Count then
  with TestCases[Item.Index] do begin
    Item.Caption := Name;
  end;
end;

procedure TFormTest.ReloadList;
begin
  ListViewTestCases.Items.Count := TestCases.Count;
  ListViewTestCases.Refresh;
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
end;

procedure TFormTest.FormCreate(Sender: TObject);
begin
  TestCases := TTestCases.Create;
  with TestCases do begin
    AddNew('Load and save');
    AddNew('Multi-line');
    AddNew('Encoding base64');
    AddNew('Encoding quoted-printable');
    AddNew('Image format');
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
end;

end.

