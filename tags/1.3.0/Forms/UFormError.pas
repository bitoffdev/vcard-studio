unit UFormError;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormError }

  TFormError = class(TForm)
    MemoErrors: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  end;

var
  FormError: TFormError;


implementation

{$R *.lfm}

uses
  UCore;

{ TFormError }

procedure TFormError.FormCreate(Sender: TObject);
begin
  Core.Translator.TranslateComponentRecursive(Self);
  Core.ThemeManager1.UseTheme(Self);
end;

procedure TFormError.FormShow(Sender: TObject);
begin
  Core.PersistentForm1.Load(Self);
end;

procedure TFormError.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Core.PersistentForm1.Save(Self);
end;

end.

