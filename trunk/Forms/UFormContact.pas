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
    EditSurname: TEdit;
    EditEmail: TEdit;
    EditPhone: TEdit;
    EditName: TEdit;
    EditCellPhone: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ListView1: TListView;
    MemoNotes: TMemo;
    PageControlContact: TPageControl;
    TabSheetGeneral: TTabSheet;
    TabSheetDetails: TTabSheet;
    TabSheetAll: TTabSheet;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    procedure LoadData(Contact: TContact);
    procedure SaveData(Contact: TContact);
  end;

var
  FormContact: TFormContact;

implementation

{$R *.lfm}

uses
  UCore;

{ TFormContact }

procedure TFormContact.FormShow(Sender: TObject);
begin
  Core.PersistentForm1.Load(Self);
end;

procedure TFormContact.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  Core.PersistentForm1.Save(Self);
end;

procedure TFormContact.FormCreate(Sender: TObject);
begin
  Core.CoolTranslator1.TranslateComponentRecursive(Self);
end;

procedure TFormContact.LoadData(Contact: TContact);
begin
  EditName.Text := Contact.FirstName;
  EditSurname.Text := Contact.LastName;
  EditCellPhone.Text := Contact.TelCell;
  EditPhone.Text := Contact.TelHome;
  EditEmail.Text := Contact.EmailHome;
  MemoNotes.Lines.Text := Contact.Note;
end;

procedure TFormContact.SaveData(Contact: TContact);
begin
  Contact.FirstName := EditName.Text;
  Contact.LastName := EditSurname.Text;
  Contact.TelCell := EditCellPhone.Text;
  Contact.TelHome := EditPhone.Text;
  Contact.EmailHome := EditEmail.Text;
  Contact.Note := MemoNotes.Lines.Text;
end;

end.

