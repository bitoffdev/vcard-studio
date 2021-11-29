unit UFormContact;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ActnList, Menus, ExtCtrls, UContact, base64, UFormProperties;

type

  { TFormContact }

  TFormContact = class(TForm)
    AEditField: TAction;
    ActionList1: TActionList;
    ButtonCancel: TButton;
    ButtonOk: TButton;
    EditAddressHomeCity: TEdit;
    EditAddressHomeCountry: TEdit;
    EditAddressHomePostalCode: TEdit;
    EditAddressHomePostOfficeBox: TEdit;
    EditAddressHomeRegion: TEdit;
    EditAddressHomeStreetExtended: TEdit;
    EditAddressWorkStreet: TEdit;
    EditAddressWorkCity: TEdit;
    EditAddressWorkRegion: TEdit;
    EditAddressWorkCountry: TEdit;
    EditAddressHomeStreet: TEdit;
    EditWebWork: TEdit;
    EditAddressWorkPostalCode: TEdit;
    EditAddressWorkPostOfficeBox: TEdit;
    EditAddressWorkStreetExtended: TEdit;
    EditDepartment: TEdit;
    EditFullName: TEdit;
    EditOrganization: TEdit;
    EditTitle: TEdit;
    EditTitleBefore: TEdit;
    EditMiddleName: TEdit;
    EditTitleAfter: TEdit;
    EditEmailHome: TEdit;
    EditEmailWork: TEdit;
    EditBirthday: TEdit;
    EditPhoneHome: TEdit;
    EditPhoneWork: TEdit;
    EditCellPhoneHome: TEdit;
    EditCellPhoneWork: TEdit;
    EditFax: TEdit;
    EditFaxHome: TEdit;
    EditFaxWork: TEdit;
    EditPager: TEdit;
    EditLastName: TEdit;
    EditEmail: TEdit;
    EditPhone: TEdit;
    EditFirstName: TEdit;
    EditCellPhone: TEdit;
    EditWebHome: TEdit;
    ImagePhoto: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelOrganization: TLabel;
    LabelOrganization1: TLabel;
    MemoNotes: TMemo;
    MenuItem1: TMenuItem;
    PageControlContact: TPageControl;
    PopupMenu1: TPopupMenu;
    TabSheetWork: TTabSheet;
    TabSheetGeneral: TTabSheet;
    TabSheetPrivate: TTabSheet;
    TabSheetAll: TTabSheet;
    procedure ButtonOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure TabSheetAllShow(Sender: TObject);
    procedure TabSheetGeneralShow(Sender: TObject);
  private
    FContact: TContact;
    FormProperties: TFormProperties;
    procedure SetContact(AValue: TContact);
  public
    procedure LoadData;
    procedure SaveData;
    property Contact: TContact read FContact write SetContact;
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

  FormProperties.ManualDock(TabSheetAll, nil, alClient);
  FormProperties.Align := alClient;
  FormProperties.Show;
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
  SaveData;
  FormProperties.ReloadList;
  FormProperties.UpdateInterface;
end;

procedure TFormContact.TabSheetGeneralShow(Sender: TObject);
begin
  LoadData;
end;

procedure TFormContact.SetContact(AValue: TContact);
begin
  if FContact = AValue then Exit;
  FContact := AValue;
  LoadData;
end;

procedure TFormContact.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  Core.PersistentForm1.Save(Self);
end;

procedure TFormContact.ButtonOkClick(Sender: TObject);
begin
  SaveData;
end;

procedure TFormContact.FormCreate(Sender: TObject);
begin
  Core.Translator.TranslateComponentRecursive(Self);
  Core.ThemeManager1.UseTheme(Self);
  FContact := nil;
  FormProperties := TFormProperties.Create(nil);
end;

procedure TFormContact.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FormProperties);
end;

procedure TFormContact.LoadData;
var
  Photo: string;
  JpegImage: TJpegImage;
  Stream: TMemoryStream;
  PhotoProperty: TContactProperty;
begin
  FormProperties.Properties := Contact.Properties;

  EditFullName.Text := Contact.Fields[cfFullName];
  EditFirstName.Text := Contact.Fields[cfFirstName];
  EditLastName.Text := Contact.Fields[cfLastName];
  EditMiddleName.Text := Contact.Fields[cfMiddleName];
  EditTitleAfter.Text := Contact.Fields[cfTitleAfter];
  EditTitleBefore.Text := Contact.Fields[cfTitleBefore];
  EditCellPhone.Text := Contact.Fields[cfTelCell];
  EditPhoneHome.Text := Contact.Fields[cfTelHome];
  EditPhoneWork.Text := Contact.Fields[cfTelWork];
  EditEmail.Text := Contact.Fields[cfEmail];
  MemoNotes.Lines.Text := Contact.Fields[cfNote];

  // Work
  EditTitle.Text := Contact.Fields[cfTitle];
  EditOrganization.Text := Contact.Fields[cfOrganization];
  EditDepartment.Text := Contact.Fields[cfDepartment];
  EditAddressWorkPostOfficeBox.Text := Contact.Fields[cfWorkAddressPostOfficeBox];
  EditAddressWorkStreet.Text := Contact.Fields[cfWorkAddressStreet];
  EditAddressWorkStreetExtended.Text := Contact.Fields[cfWorkAddressStreetExtended];
  EditAddressWorkCity.Text := Contact.Fields[cfWorkAddressCity];
  EditAddressWorkRegion.Text := Contact.Fields[cfWorkAddressRegion];
  EditAddressWorkCountry.Text := Contact.Fields[cfWorkAddressCountry];
  EditAddressWorkPostalCode.Text := Contact.Fields[cfWorkAddressPostalCode];
  EditWebHome.Text := Contact.Fields[cfUrlWork];

  // Private
  EditAddressHomePostOfficeBox.Text := Contact.Fields[cfHomeAddressPostOfficeBox];
  EditAddressHomeStreet.Text := Contact.Fields[cfHomeAddressStreet];
  EditAddressHomeStreetExtended.Text := Contact.Fields[cfHomeAddressStreetExtended];
  EditAddressHomeCity.Text := Contact.Fields[cfHomeAddressCity];
  EditAddressHomeRegion.Text := Contact.Fields[cfHomeAddressRegion];
  EditAddressHomeCountry.Text := Contact.Fields[cfHomeAddressCountry];
  EditAddressHomePostalCode.Text := Contact.Fields[cfHomeAddressPostalCode];
  EditWebHome.Text := Contact.Fields[cfUrlHome];
  EditBirthday.Text := Contact.Fields[cfDayOfBirth];

  EditEmailHome.Text := Contact.Fields[cfEmail];
  ImagePhoto.Picture.Bitmap.Clear;
  PhotoProperty := Contact.GetProperty(cfPhoto);
  if Assigned(PhotoProperty) then begin
    Photo := Contact.Fields[cfPhoto];
    if (Photo <> '') and (PhotoProperty.Encoding <> '') then begin
      Stream := TMemoryStream.Create;
      try
        Stream.Write(Photo[1], Length(Photo));
        Stream.Position := 0;
        if PhotoProperty.Attributes.IndexOf('JPEG') <> -1 then begin
          JpegImage := TJPEGImage.Create;
          try
            try
              JpegImage.LoadFromStream(Stream);
              ImagePhoto.Picture.Bitmap.SetSize(JpegImage.Width, JpegImage.Height);
              ImagePhoto.Picture.Bitmap.Canvas.Draw(0, 0, JpegImage);
            except
            end;
          finally
            JpegImage.Free;
          end;
        end else begin
          try
            ImagePhoto.Picture.LoadFromStream(Stream);
          except
          end;
        end;
      finally
        Stream.Free;
      end;
    end;
  end;
end;

procedure TFormContact.SaveData;
begin
  Contact.Fields[cfFullName] := EditFullName.Text;
  Contact.Fields[cfMiddleName] := EditMiddleName.Text;
  Contact.Fields[cfFirstName] := EditFirstName.Text;
  Contact.Fields[cfLastName] := EditLastName.Text;
  Contact.Fields[cfTitleAfter] := EditTitleAfter.Text;
  Contact.Fields[cfTitleBefore] := EditTitleBefore.Text;
  Contact.Fields[cfTelCell] := EditCellPhone.Text;
  Contact.Fields[cfTelHome] := EditPhoneHome.Text;
  Contact.Fields[cfTelWork] := EditPhoneWork.Text;
  Contact.Fields[cfEmail] := EditEmail.Text;
  Contact.Fields[cfNote] := MemoNotes.Lines.Text;
  Contact.Fields[cfEmail] := EditEmailHome.Text;
  Contact.Fields[cfDayOfBirth] := EditBirthday.Text;

  // Work
  Contact.Fields[cfTitle] := EditTitle.Text;
  Contact.Fields[cfOrganization] := EditOrganization.Text;
  Contact.Fields[cfDepartment] := EditDepartment.Text;
  Contact.Fields[cfWorkAddressPostOfficeBox] := EditAddressWorkPostOfficeBox.Text;
  Contact.Fields[cfWorkAddressStreet] := EditAddressWorkStreet.Text;
  Contact.Fields[cfWorkAddressStreetExtended] := EditAddressWorkStreetExtended.Text;
  Contact.Fields[cfWorkAddressCity] := EditAddressWorkCity.Text;
  Contact.Fields[cfWorkAddressRegion] := EditAddressWorkRegion.Text;
  Contact.Fields[cfWorkAddressCountry] := EditAddressWorkCountry.Text;
  Contact.Fields[cfWorkAddressPostalCode] := EditAddressWorkPostalCode.Text;
  Contact.Fields[cfUrlWork] := EditWebWork.Text;

  // Private
  Contact.Fields[cfHomeAddressPostOfficeBox] := EditAddressHomePostOfficeBox.Text;
  Contact.Fields[cfHomeAddressStreet] := EditAddressHomeStreet.Text;
  Contact.Fields[cfHomeAddressStreetExtended] := EditAddressHomeStreetExtended.Text;
  Contact.Fields[cfHomeAddressCity] := EditAddressHomeCity.Text;
  Contact.Fields[cfHomeAddressRegion] := EditAddressHomeRegion.Text;
  Contact.Fields[cfHomeAddressCountry] := EditAddressHomeCountry.Text;
  Contact.Fields[cfHomeAddressPostalCode] := EditAddressHomePostalCode.Text;
  Contact.Fields[cfUrlHome] := EditWebHome.Text;
end;

end.

