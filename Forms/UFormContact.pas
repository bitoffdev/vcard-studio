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
    procedure ListView1Data(Sender: TObject; Item: TListItem);
  private
    Contact: TContact;
  public
    procedure ReloadFields;
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
  PageControlContact.TabIndex := 0;
  ReloadFields;
end;

procedure TFormContact.ListView1Data(Sender: TObject; Item: TListItem);
begin
  if Item.Index < Contact.Parent.Fields.Count then
  with TContactField(Contact.Parent.Fields[Item.Index]) do begin
    Item.Caption := Name;
    Item.SubItems.Add(Contact.Fields[Index]);
  end;
end;

procedure TFormContact.ReloadFields;
begin
  if Assigned(Contact) then begin
    ListView1.Items.Count := Contact.Parent.Fields.Count;
  end else ListView1.Items.Count := 0;
  ListView1.Refresh;
end;

procedure TFormContact.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  Core.PersistentForm1.Save(Self);
end;

procedure TFormContact.FormCreate(Sender: TObject);
begin
  Core.CoolTranslator1.TranslateComponentRecursive(Self);
  Contact := nil;
end;

procedure TFormContact.LoadData(Contact: TContact);
begin
  Self.Contact := Contact;
  ReloadFields;
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

