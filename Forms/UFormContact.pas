unit UFormContact;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ActnList, Menus, ExtCtrls, UContact, base64;

type

  { TFormContact }

  TFormContact = class(TForm)
    AEditField: TAction;
    ActionList1: TActionList;
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
    ImagePhoto: TImage;
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
    Label23: TLabel;
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
    MenuItem1: TMenuItem;
    PageControlContact: TPageControl;
    PopupMenu1: TPopupMenu;
    TabSheetGeneral: TTabSheet;
    TabSheetDetails: TTabSheet;
    TabSheetAll: TTabSheet;
    procedure AEditFieldExecute(Sender: TObject);
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

resourcestring
  SFieldEdit = 'Field edit';
  SEditFieldValue = 'Edit field value';

{ TFormContact }

procedure TFormContact.FormShow(Sender: TObject);
begin
  Core.PersistentForm1.Load(Self);
  PageControlContact.TabIndex := 0;
end;

procedure TFormContact.ListView1Data(Sender: TObject; Item: TListItem);
begin
  if Item.Index < Contact.Properties.Count then
  with Contact.Properties[Item.Index] do begin
    Item.Caption := Contact.Properties[Item.Index].Name;
    Item.SubItems.Add(Attributes.DelimitedText);
    Item.SubItems.Add(Contact.Properties[Item.Index].Values.DelimitedText);
    Item.Data := Contact.Properties[Item.Index];
  end;
end;

procedure TFormContact.TabSheetAllShow(Sender: TObject);
begin
  ReloadFields;
end;

procedure TFormContact.ReloadFields;
begin
  if Assigned(Contact) then begin
    ListView1.Items.Count := Contact.Properties.Count;
  end else ListView1.Items.Count := 0;
  ListView1.Refresh;
end;

procedure TFormContact.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  Core.PersistentForm1.Save(Self);
end;

procedure TFormContact.AEditFieldExecute(Sender: TObject);
begin
  if Assigned(ListView1.Selected) then begin
    TContactProperty(ListView1.Selected.Data).Values.DelimitedText :=
      InputBox(SFieldEdit, SEditFieldValue, TContactProperty(ListView1.Selected.Data).Values.DelimitedText);
  end;
end;

procedure TFormContact.FormCreate(Sender: TObject);
begin
  Core.Translator.TranslateComponentRecursive(Self);
  Core.ThemeManager1.UseTheme(Self);
  Contact := nil;
end;

procedure TFormContact.LoadData(Contact: TContact);
var
  Photo: string;
  JpegImage: TJpegImage;
  Stream: TMemoryStream;
  PhotoProperty: TContactProperty;
begin
  Self.Contact := Contact;
  EditName.Text := Contact.Fields[cfFirstName];
  EditSurname.Text := Contact.Fields[cfLastName];
  EditCellPhone.Text := Contact.Fields[cfTelCell];
  EditPhoneHome.Text := Contact.Fields[cfTelHome];
  EditPhoneWork.Text := Contact.Fields[cfTelWork];
  EditEmail.Text := Contact.Fields[cfEmail];
  MemoNotes.Lines.Text := Contact.Fields[cfNote];
  EditTitle.Text := Contact.Fields[cfTitle];
  EditOrganization.Text := Contact.Fields[cfOrganization];
  EditAddress.Text := Contact.Fields[cfAdrHome];
  EditEmailHome.Text := Contact.Fields[cfEmailHome];

  ImagePhoto.Picture.Bitmap.Clear;
  PhotoProperty := Contact.GetProperty(cfPhoto);
  if Assigned(PhotoProperty) then begin
    Photo := Contact.Fields[cfPhoto];
    if (Photo <> '') and (PhotoProperty.Encoding <> '') then begin
      Photo := PhotoProperty.GetDecodedValue;
      Stream := TMemoryStream.Create;
      Stream.Write(Photo[1], Length(Photo));
      Stream.Position := 0;
      JpegImage := TJPEGImage.Create;
      JpegImage.LoadFromStream(Stream);
      ImagePhoto.Picture.Bitmap.SetSize(JpegImage.Width, JpegImage.Height);
      ImagePhoto.Picture.Bitmap.Canvas.Draw(0, 0, JpegImage);
      JpegImage.Free;
      Stream.Free;
    end;
  end;
end;

procedure TFormContact.SaveData(Contact: TContact);
begin
  Contact.Fields[cfFirstName] := EditName.Text;
  Contact.Fields[cfLastName] := EditSurname.Text;
  Contact.Fields[cfTelCell] := EditCellPhone.Text;
  Contact.Fields[cfTelHome] := EditPhoneHome.Text;
  Contact.Fields[cfTelWork] := EditPhoneWork.Text;
  Contact.Fields[cfEmail] := EditEmail.Text;
  Contact.Fields[cfNote] := MemoNotes.Lines.Text;
  Contact.Fields[cfTitle] := EditTitle.Text;
  Contact.Fields[cfOrganization] := EditOrganization.Text;
  Contact.Fields[cfAdrHome] := EditAddress.Text;
  Contact.Fields[cfEmailHome] := EditEmailHome.Text;
end;

end.
