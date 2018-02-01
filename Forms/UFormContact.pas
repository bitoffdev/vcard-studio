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
    EditWebPage: TEdit;
    EditWebPageHome: TEdit;
    EditWebPageWork: TEdit;
    EditEmailHome: TEdit;
    EditEmailWork: TEdit;
    EditBirthday: TEdit;
    EditTitle: TEdit;
    EditAddress: TEdit;
    EditOrganization: TEdit;
    EditPhoneHome: TEdit;
    EditPhoneWork: TEdit;
    EditCellPhoneHome: TEdit;
    EditCellPhoneWork: TEdit;
    EditFax: TEdit;
    EditFaxHome: TEdit;
    EditFaxWork: TEdit;
    EditPager: TEdit;
    EditSurname: TEdit;
    EditEmail: TEdit;
    EditPhone: TEdit;
    EditName: TEdit;
    EditCellPhone: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    LabelOrganization: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
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
    procedure TabSheetAllShow(Sender: TObject);
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
end;

procedure TFormContact.ListView1Data(Sender: TObject; Item: TListItem);
begin
  if Item.Index < Contact.Parent.Fields.Count then
  with TContactField(Contact.Parent.Fields[Item.Index]) do begin
    Item.Caption := Name;
    Item.SubItems.Add(Contact.Fields[Index]);
  end;
end;

procedure TFormContact.TabSheetAllShow(Sender: TObject);
begin
  ReloadFields;
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
  EditName.Text := Contact.FirstName;
  EditSurname.Text := Contact.LastName;
  EditCellPhone.Text := Contact.TelCell;
  EditPhoneHome.Text := Contact.TelHome;
  EditPhoneWork.Text := Contact.TelWork;
  EditEmail.Text := Contact.EmailHome;
  MemoNotes.Lines.Text := Contact.Note;
  EditTitle.Text := Contact.Title;
  EditOrganization.Text := Contact.Organization;
  EditAddress.Text := Contact.AdrHome;
  EditEmailHome.Text := Contact.EmailHome;
end;

procedure TFormContact.SaveData(Contact: TContact);
begin
  Contact.FirstName := EditName.Text;
  Contact.LastName := EditSurname.Text;
  Contact.TelCell := EditCellPhone.Text;
  Contact.TelHome := EditPhoneHome.Text;
  Contact.TelWork := EditPhoneWork.Text;
  Contact.EmailHome := EditEmail.Text;
  Contact.Note := MemoNotes.Lines.Text;
  Contact.Title := EditTitle.Text;
  Contact.Organization := EditOrganization.Text;
  Contact.AdrHome := EditAddress.Text;
  Contact.EmailHome := EditEmailHome.Text;
end;

end.

