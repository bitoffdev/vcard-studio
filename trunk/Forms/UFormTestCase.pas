unit UFormTestCase;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormTestCase }

  TFormTestCase = class(TForm)
    MemoLog: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  FormTestCase: TFormTestCase;

implementation

{$R *.lfm}

uses
  UCore;

{ TFormTestCase }

procedure TFormTestCase.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  Core.PersistentForm1.Save(Self);
end;

procedure TFormTestCase.FormCreate(Sender: TObject);
begin
  Core.Translator.TranslateComponentRecursive(Self);
  Core.ThemeManager1.UseTheme(Self);
end;

procedure TFormTestCase.FormShow(Sender: TObject);
begin
  Core.PersistentForm1.Load(Self);
end;

end.

