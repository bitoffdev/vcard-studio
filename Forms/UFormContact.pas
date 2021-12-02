unit UFormContact;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ActnList, Menus, ExtCtrls, UContact, UFormProperties;

type

  { TFormContact }

  TFormContact = class(TForm)
    AEditField: TAction;
    ActionList1: TActionList;
    ButtonCancel: TButton;
    ButtonNext: TButton;
    ButtonOk: TButton;
    ButtonPrevious: TButton;
    EditHomeAddressCity: TEdit;
    EditHomeAddressCountry: TEdit;
    EditHomeAddressPostalCode: TEdit;
    EditHomeAddressPostOfficeBox: TEdit;
    EditHomeAddressRegion: TEdit;
    EditHomeAddressStreet: TEdit;
    EditHomeAddressStreetExtended: TEdit;
    EditAddressWorkCity: TEdit;
    EditAddressWorkCountry: TEdit;
    EditAddressWorkPostalCode: TEdit;
    EditAddressWorkPostOfficeBox: TEdit;
    EditAddressWorkRegion: TEdit;
    EditAddressWorkStreet: TEdit;
    EditAddressWorkStreetExtended: TEdit;
    EditAniversary: TEdit;
    EditWorkEmail: TEdit;
    EditPager: TEdit;
    EditFax: TEdit;
    EditWeb: TEdit;
    EditHomeFax: TEdit;
    EditWorkFax: TEdit;
    EditHomeMobile: TEdit;
    EditWorkMobile: TEdit;
    EditWorkPager: TEdit;
    EditHomePhone: TEdit;
    EditNickName: TEdit;
    EditHomePager: TEdit;
    EditHomeEmail: TEdit;
    EditHomeWeb: TEdit;
    EditWorkWeb: TEdit;
    EditDepartment: TEdit;
    EditFullName: TEdit;
    EditOrganization: TEdit;
    EditTitle: TEdit;
    EditTitleBefore: TEdit;
    EditMiddleName: TEdit;
    EditTitleAfter: TEdit;
    EditBirthday: TEdit;
    EditLastName: TEdit;
    EditEmail: TEdit;
    EditPhone: TEdit;
    EditFirstName: TEdit;
    EditMobile: TEdit;
    EditWorkPhone: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
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
    Label22: TLabel;
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
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
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
    TabSheetOthers: TTabSheet;
    TabSheetHome: TTabSheet;
    TabSheetWork: TTabSheet;
    TabSheetGeneral: TTabSheet;
    TabSheetAll: TTabSheet;
    procedure ButtonNextClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure ButtonPreviousClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure TabSheetAllShow(Sender: TObject);
    procedure TabSheetGeneralShow(Sender: TObject);
  private
    FContact: TContact;
    FOnNext: TNotifyEvent;
    FOnPrevious: TNotifyEvent;
    FormProperties: TFormProperties;
    procedure SetContact(AValue: TContact);
  public
    procedure LoadData;
    procedure SaveData;
    property Contact: TContact read FContact write SetContact;
    property OnPrevious: TNotifyEvent read FOnPrevious write FOnPrevious;
    property OnNext: TNotifyEvent read FOnNext write FOnNext;
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
    Item.SubItems.Add(Contact.Properties[Item.Index].Value);
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

procedure TFormContact.ButtonNextClick(Sender: TObject);
begin
  if Assigned(FOnNext) then FOnNext(Self);
end;

procedure TFormContact.ButtonPreviousClick(Sender: TObject);
begin
  if Assigned(FOnPrevious) then FOnPrevious(Self);
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

  // General
  EditFullName.Text := Contact.Fields[cfFullName];
  EditFirstName.Text := Contact.Fields[cfFirstName];
  EditLastName.Text := Contact.Fields[cfLastName];
  EditMiddleName.Text := Contact.Fields[cfMiddleName];
  EditTitleAfter.Text := Contact.Fields[cfTitleAfter];
  EditTitleBefore.Text := Contact.Fields[cfTitleBefore];
  EditNickName.Text := Contact.Fields[cfNickName];
  EditEmail.Text := Contact.Fields[cfEmail];
  EditPhone.Text := Contact.Fields[cfTel];
  EditMobile.Text := Contact.Fields[cfTelCell];
  EditFax.Text := Contact.Fields[cfTelFax];
  EditPager.Text := Contact.Fields[cfTelPager];
  EditBirthday.Text := Contact.Fields[cfDayOfBirth];
  EditAniversary.Text := Contact.Fields[cfAnniversary];
  EditWeb.Text := Contact.Fields[cfUrl];

  // Home
  EditHomeEmail.Text := Contact.Fields[cfEmailHome];
  EditHomePhone.Text := Contact.Fields[cfTelHome];
  EditHomeMobile.Text := Contact.Fields[cfTelCellHome];
  EditHomeFax.Text := Contact.Fields[cfTelFaxHome];
  EditHomePager.Text := Contact.Fields[cfTelPagerHome];
  EditHomeAddressPostOfficeBox.Text := Contact.Fields[cfHomeAddressPostOfficeBox];
  EditHomeAddressStreet.Text := Contact.Fields[cfHomeAddressStreet];
  EditHomeAddressStreetExtended.Text := Contact.Fields[cfHomeAddressStreetExtended];
  EditHomeAddressCity.Text := Contact.Fields[cfHomeAddressCity];
  EditHomeAddressRegion.Text := Contact.Fields[cfHomeAddressRegion];
  EditHomeAddressCountry.Text := Contact.Fields[cfHomeAddressCountry];
  EditHomeAddressPostalCode.Text := Contact.Fields[cfHomeAddressPostalCode];
  EditHomeWeb.Text := Contact.Fields[cfUrlHome];

  // Work
  EditWorkEmail.Text := Contact.Fields[cfEmailWork];
  EditWorkPhone.Text := Contact.Fields[cfTelWork];
  EditWorkMobile.Text := Contact.Fields[cfTelCellWork];
  EditWorkFax.Text := Contact.Fields[cfTelFaxWork];
  EditWorkPager.Text := Contact.Fields[cfTelPagerWork];
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
  EditWorkWeb.Text := Contact.Fields[cfUrlWork];

  // Others
  MemoNotes.Lines.Text := Contact.Fields[cfNote];

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
  // General
  Contact.Fields[cfFullName] := EditFullName.Text;
  Contact.Fields[cfMiddleName] := EditMiddleName.Text;
  Contact.Fields[cfFirstName] := EditFirstName.Text;
  Contact.Fields[cfLastName] := EditLastName.Text;
  Contact.Fields[cfTitleAfter] := EditTitleAfter.Text;
  Contact.Fields[cfTitleBefore] := EditTitleBefore.Text;
  Contact.Fields[cfNickName] := EditNickName.Text;
  Contact.Fields[cfEmail] := EditEmail.Text;
  Contact.Fields[cfTel] := EditPhone.Text;
  Contact.Fields[cfTelCell] := EditMobile.Text;
  Contact.Fields[cfTelFax] := EditFax.Text;
  Contact.Fields[cfTelPager] := EditPager.Text;
  Contact.Fields[cfDayOfBirth] := EditBirthday.Text;
  Contact.Fields[cfAnniversary] := EditAniversary.Text;
  Contact.Fields[cfUrl] := EditWeb.Text;

  // Home
  Contact.Fields[cfEmailHome] := EditHomeEmail.Text;
  Contact.Fields[cfTelHome] := EditHomePhone.Text;
  Contact.Fields[cfTelCellHome] := EditHomeMobile.Text;
  Contact.Fields[cfTelFaxHome] := EditHomeFax.Text;
  Contact.Fields[cfTelPagerHome] := EditHomePager.Text;
  Contact.Fields[cfHomeAddressPostOfficeBox] := EditHomeAddressPostOfficeBox.Text;
  Contact.Fields[cfHomeAddressStreet] := EditHomeAddressStreet.Text;
  Contact.Fields[cfHomeAddressStreetExtended] := EditHomeAddressStreetExtended.Text;
  Contact.Fields[cfHomeAddressCity] := EditHomeAddressCity.Text;
  Contact.Fields[cfHomeAddressRegion] := EditHomeAddressRegion.Text;
  Contact.Fields[cfHomeAddressCountry] := EditHomeAddressCountry.Text;
  Contact.Fields[cfHomeAddressPostalCode] := EditHomeAddressPostalCode.Text;
  Contact.Fields[cfUrlHome] := EditHomeWeb.Text;

  // Work
  Contact.Fields[cfEmailWork] := EditWorkEmail.Text;
  Contact.Fields[cfTelWork] := EditWorkPhone.Text;
  Contact.Fields[cfTelCellWork] := EditWorkMobile.Text;
  Contact.Fields[cfTelFaxWork] := EditWorkFax.Text;
  Contact.Fields[cfTelPagerWork] := EditWorkPager.Text;
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
  Contact.Fields[cfUrlWork] := EditWorkWeb.Text;

  // Others
  Contact.Fields[cfNote] := MemoNotes.Lines.Text;
end;

end.

