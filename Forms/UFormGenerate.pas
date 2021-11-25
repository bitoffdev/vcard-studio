unit UFormGenerate;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, UContact;

type

  { TFormGenerate }

  TFormGenerate = class(TForm)
    ButtonGenerate: TButton;
    Label1: TLabel;
    SpinEditCount: TSpinEdit;
    procedure ButtonGenerateClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    Contacts: TContacts;
    procedure UpdateInterface;
  end;

var
  FormGenerate: TFormGenerate;


implementation

{$R *.lfm}

uses
  UCore;

{ TFormGenerate }

procedure TFormGenerate.ButtonGenerateClick(Sender: TObject);
var
  I: Integer;
  Contact: TContact;
begin
  for I := 1 to SpinEditCount.Value do begin
    Contact := Contacts.AddNew;
    Contact.Fields[cfFirstName] := 'First ' + IntToStr(Random(10000));
    Contact.Fields[cfLastName] := 'Last ' + IntToStr(Random(10000));
    Contact.Fields[cfFullName] := 'FullName ' + IntToStr(Random(100));
    Contact.Fields[cfTelCell] := IntToStr(Random(1000000000));
    Contact.Fields[cfTelHome] := IntToStr(Random(1000000000));
  end;
  Close;
end;

procedure TFormGenerate.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  Core.PersistentForm1.Save(Self);
end;

procedure TFormGenerate.FormCreate(Sender: TObject);
begin
  Core.Translator.TranslateComponentRecursive(Self);
  Core.ThemeManager1.UseTheme(Self);
end;

procedure TFormGenerate.FormShow(Sender: TObject);
begin
  Core.PersistentForm1.Load(Self);
  UpdateInterface;
end;

procedure TFormGenerate.UpdateInterface;
begin
  ButtonGenerate.Enabled := Assigned(Contacts);
end;

end.

