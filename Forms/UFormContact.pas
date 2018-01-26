unit UFormContact;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, UContact;

type

  { TFormContact }

  TFormContact = class(TForm)
    ButtonCancel: TButton;
    ButtonOk: TButton;
    EditName: TEdit;
    Label1: TLabel;
    PageControlContact: TPageControl;
    TabSheetGeneral: TTabSheet;
    TabSheetDetails: TTabSheet;
    TabSheetAll: TTabSheet;
  private

  public
    procedure LoadData(Contact: TContact);
    procedure SaveData(Contact: TContact);
  end;

var
  FormContact: TFormContact;

implementation

{$R *.lfm}

{ TFormContact }

procedure TFormContact.LoadData(Contact: TContact);
begin
  EditName.Text := Contact.FirstName;
end;

procedure TFormContact.SaveData(Contact: TContact);
begin
  Contact.FirstName := EditName.Text;
end;

end.

