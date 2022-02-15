unit UFormNameDetails;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormNameDetails }

  TFormNameDetails = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    EditFirstName: TEdit;
    EditLastName: TEdit;
    EditMiddleName: TEdit;
    EditTitleAfter: TEdit;
    EditTitleBefore: TEdit;
    Label1: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label5: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  FormNameDetails: TFormNameDetails;

implementation

{$R *.lfm}

uses
  UCore;

{ TFormNameDetails }

procedure TFormNameDetails.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  Core.PersistentForm1.Save(Self);
end;

procedure TFormNameDetails.FormCreate(Sender: TObject);
begin
  Core.Translator.TranslateComponentRecursive(Self);
  Core.ThemeManager1.UseTheme(Self);
end;

procedure TFormNameDetails.FormShow(Sender: TObject);
begin
  Core.PersistentForm1.Load(Self);
end;

end.

