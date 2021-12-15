unit UFormContact;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ActnList, Menus, ExtCtrls, ExtDlgs, UContact, UFormProperties;

type

  { TFormContact }

  TFormContact = class(TForm)
    APhotoClear: TAction;
    APhotoSave: TAction;
    APhotoLoad: TAction;
    ActionList1: TActionList;
    ButtonCancel: TButton;
    ButtonNext: TButton;
    ButtonOk: TButton;
    ButtonPrevious: TButton;
    EditAim: TEdit;
    EditCategories: TEdit;
    EditMatrix: TEdit;
    EditGaduGadu: TEdit;
    EditGroupWise: TEdit;
    EditYouTube: TEdit;
    EditGender: TEdit;
    EditLinkedIn: TEdit;
    EditFacebook: TEdit;
    EditInstagram: TEdit;
    EditMySpace: TEdit;
    EditTwitter: TEdit;
    EditReddit: TEdit;
    EditMastodon: TEdit;
    EditWindowsLive: TEdit;
    EditSnapchat: TEdit;
    EditYahoo: TEdit;
    EditGoogleTalk: TEdit;
    EditMsn: TEdit;
    EditIrc: TEdit;
    EditSkype: TEdit;
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
    EditJabber: TEdit;
    EditIcq: TEdit;
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
    EditQq: TEdit;
    EditPeerTube: TEdit;
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
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label5: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelOrganization: TLabel;
    LabelOrganization1: TLabel;
    LabelOrganization10: TLabel;
    LabelOrganization11: TLabel;
    LabelOrganization12: TLabel;
    LabelOrganization2: TLabel;
    LabelOrganization3: TLabel;
    LabelOrganization4: TLabel;
    LabelOrganization5: TLabel;
    LabelOrganization6: TLabel;
    LabelOrganization7: TLabel;
    LabelOrganization8: TLabel;
    LabelOrganization9: TLabel;
    MemoNotes: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    PageControlContact: TPageControl;
    PopupMenuPhoto: TPopupMenu;
    SavePictureDialog1: TSavePictureDialog;
    TabSheetSocial: TTabSheet;
    TabSheetChat: TTabSheet;
    TabSheetOthers: TTabSheet;
    TabSheetHome: TTabSheet;
    TabSheetWork: TTabSheet;
    TabSheetGeneral: TTabSheet;
    TabSheetAll: TTabSheet;
    procedure APhotoClearExecute(Sender: TObject);
    procedure APhotoLoadExecute(Sender: TObject);
    procedure APhotoSaveExecute(Sender: TObject);
    procedure ButtonNextClick(Sender: TObject);
    procedure ButtonPreviousClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TabSheetAllShow(Sender: TObject);
    procedure TabSheetChatHide(Sender: TObject);
    procedure TabSheetChatShow(Sender: TObject);
    procedure TabSheetGeneralHide(Sender: TObject);
    procedure TabSheetGeneralShow(Sender: TObject);
    procedure TabSheetHomeHide(Sender: TObject);
    procedure TabSheetHomeShow(Sender: TObject);
    procedure TabSheetOthersHide(Sender: TObject);
    procedure TabSheetOthersShow(Sender: TObject);
    procedure TabSheetSocialHide(Sender: TObject);
    procedure TabSheetSocialShow(Sender: TObject);
    procedure TabSheetWorkHide(Sender: TObject);
    procedure TabSheetWorkShow(Sender: TObject);
  private
    FProfilePhotoActive: Boolean;
    procedure SetProfilePhotoActive(AValue: Boolean);
  private
    FContact: TContact;
    FOnGetNext: TGetContactEvent;
    FOnGetPrevious: TGetContactEvent;
    FormProperties: TFormProperties;
    procedure SetContact(AValue: TContact);
    procedure ReloadAllPropertiesTab;
    property ProfilePhotoActive: Boolean read FProfilePhotoActive
      write SetProfilePhotoActive;
  public
    procedure UpdateInterface;
    property Contact: TContact read FContact write SetContact;
    property OnGetPrevious: TGetContactEvent read FOnGetPrevious write FOnGetPrevious;
    property OnGetNext: TGetContactEvent read FOnGetNext write FOnGetNext;
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

  FormProperties.ManualDock(TabSheetAll, nil, alClient);
  FormProperties.Align := alClient;
  FormProperties.Show;

  // Force to load default profile image
  ProfilePhotoActive := True;
  ProfilePhotoActive := False;

  PageControlContact.TabIndex := Core.LastContactTabIndex;
  UpdateInterface;
end;

procedure TFormContact.TabSheetAllShow(Sender: TObject);
begin
  FormProperties.Properties := Contact.Properties;
  FormProperties.ReloadList;
  FormProperties.UpdateInterface;
end;

procedure TFormContact.TabSheetChatHide(Sender: TObject);
begin
  Contact.Fields[cfMatrix] := EditMatrix.Text;
  Contact.Fields[cfJabber] := EditJabber.Text;
  Contact.Fields[cfIcq] := EditIcq.Text;
  Contact.Fields[cfMsn] := EditMsn.Text;
  Contact.Fields[cfSkype] := EditSkype.Text;
  Contact.Fields[cfQq] := EditQq.Text;
  Contact.Fields[cfGoogleTalk] := EditGoogleTalk.Text;
  Contact.Fields[cfWindowsLive] := EditWindowsLive.Text;
  Contact.Fields[cfYahoo] := EditYahoo.Text;
  Contact.Fields[cfAim] := EditAim.Text;
  Contact.Fields[cfIrc] := EditIrc.Text;
  Contact.Fields[cfGroupWise] := EditGroupWise.Text;
  Contact.Fields[cfGaduGadu] := EditGaduGadu.Text;

  ReloadAllPropertiesTab;
end;

procedure TFormContact.TabSheetChatShow(Sender: TObject);
begin
  EditMatrix.Text := Contact.Fields[cfMatrix];
  EditJabber.Text := Contact.Fields[cfJabber];
  EditIcq.Text := Contact.Fields[cfIcq];
  EditMsn.Text := Contact.Fields[cfMsn];
  EditSkype.Text := Contact.Fields[cfSkype];
  EditQq.Text := Contact.Fields[cfQq];
  EditGoogleTalk.Text := Contact.Fields[cfGoogleTalk];
  EditWindowsLive.Text := Contact.Fields[cfWindowsLive];
  EditYahoo.Text := Contact.Fields[cfYahoo];
  EditAim.Text := Contact.Fields[cfAim];
  EditIrc.Text := Contact.Fields[cfIrc];
  EditGroupWise.Text := Contact.Fields[cfGroupWise];
  EditGaduGadu.Text := Contact.Fields[cfGaduGadu];
end;

procedure TFormContact.TabSheetGeneralHide(Sender: TObject);
var
  Photo: string;
  PhotoProperty: TContactProperty;
  Stream: TMemoryStream;
  JpegImage: TJpegImage;
  GifImage: TGIFImage;
  PngImage: TPortableNetworkGraphic;
begin
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
  Contact.Fields[cfGender] := EditGender.Text;
  Contact.Fields[cfCategories] := EditCategories.Text;

  // Photo
  if ProfilePhotoActive then begin
    PhotoProperty := Contact.GetProperty(cfPhoto);
    if not Assigned(PhotoProperty) then begin
      PhotoProperty := TContactProperty.Create;
      PhotoProperty.Name := 'PHOTO';
      PhotoProperty.Attributes.DelimitedText := 'JPEG';
      Contact.Properties.Add(PhotoProperty);
    end;
    PhotoProperty.Encoding := 'BASE64';
    Stream := TMemoryStream.Create;
    try
      if PhotoProperty.Attributes.IndexOf('JPEG') <> -1 then begin
        JpegImage := TJPEGImage.Create;
        try
          try
            JpegImage.SetSize(ImagePhoto.Picture.Bitmap.Width, ImagePhoto.Picture.Bitmap.Height);
            JpegImage.Canvas.Draw(0, 0, ImagePhoto.Picture.Bitmap);
            JpegImage.SaveToStream(Stream);
          except
          end;
        finally
          JpegImage.Free;
        end;
      end else
      if PhotoProperty.Attributes.IndexOf('PNG') <> -1 then begin
        PngImage := TPortableNetworkGraphic.Create;
        try
          try
            PngImage.SetSize(ImagePhoto.Picture.Bitmap.Width, ImagePhoto.Picture.Bitmap.Height);
            PngImage.Canvas.Draw(0, 0, ImagePhoto.Picture.Bitmap);
            PngImage.SaveToStream(Stream);
          except
          end;
        finally
          PngImage.Free;
        end;
      end else
      if PhotoProperty.Attributes.IndexOf('GIF') <> -1 then begin
        GifImage := TGIFImage.Create;
        try
          try
            GifImage.SetSize(ImagePhoto.Picture.Bitmap.Width, ImagePhoto.Picture.Bitmap.Height);
            GifImage.Canvas.Draw(0, 0, ImagePhoto.Picture.Bitmap);
            GifImage.SaveToStream(Stream);
          except
          end;
        finally
          GifImage.Free;
        end;
      end else begin
        try
          ImagePhoto.Picture.SaveToStream(Stream);
        except
        end;
      end;

      SetLength(Photo, Stream.Size);
      Stream.Position := 0;
      Stream.Read(Photo[1], Length(Photo));
      Contact.Fields[cfPhoto] := Photo;
    finally
      Stream.Free;
    end;
  end else begin
    PhotoProperty := Contact.GetProperty(cfPhoto);
    if Assigned(PhotoProperty) then
       Contact.Properties.Remove(PhotoProperty);
  end;

  ReloadAllPropertiesTab;
end;

procedure TFormContact.TabSheetGeneralShow(Sender: TObject);
var
  Photo: string;
  JpegImage: TJpegImage;
  PngImage: TPortableNetworkGraphic;
  GifImage: TGIFImage;
  Stream: TMemoryStream;
  PhotoProperty: TContactProperty;
begin
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
  EditGender.Text := Contact.Fields[cfGender];
  EditCategories.Text := Contact.Fields[cfCategories];

  // Photo
  PhotoProperty := Contact.GetProperty(cfPhoto);
  if Assigned(PhotoProperty) then begin
    Photo := Contact.Fields[cfPhoto];
    if (Photo <> '') and (PhotoProperty.Encoding <> '') then begin
      Stream := TMemoryStream.Create;
      try
        Stream.Write(Photo[1], Length(Photo));
        Stream.Position := 0;
        if (PhotoProperty.Attributes.IndexOf('JPEG') <> -1) or
        (PhotoProperty.Attributes.IndexOf('jpeg') <> -1) then begin
          JpegImage := TJPEGImage.Create;
          try
            try
              JpegImage.LoadFromStream(Stream);
              with ImagePhoto.Picture.Bitmap do begin
                PixelFormat := pf24bit;
                SetSize(JpegImage.Width, JpegImage.Height);
                Canvas.Draw(0, 0, JpegImage);
              end;
              ProfilePhotoActive := True;
            except
              ProfilePhotoActive := False;
            end;
          finally
            JpegImage.Free;
          end;
        end else
        if (PhotoProperty.Attributes.IndexOf('PNG') <> -1) or
        (PhotoProperty.Attributes.IndexOf('png') <> -1) then begin
          PngImage := TPortableNetworkGraphic.Create;
          try
            try
              PngImage.LoadFromStream(Stream);
              with ImagePhoto.Picture.Bitmap do begin
                PixelFormat := pf24bit;
                SetSize(PngImage.Width, PngImage.Height);
                Canvas.Draw(0, 0, PngImage);
              end;
              ProfilePhotoActive := True;
            except
              ProfilePhotoActive := False;
            end;
          finally
            PngImage.Free;
          end;
        end else
        if (PhotoProperty.Attributes.IndexOf('GIF') <> -1) or
        (PhotoProperty.Attributes.IndexOf('gif') <> -1) then begin
          GifImage := TGIFImage.Create;
          try
            try
              GifImage.LoadFromStream(Stream);
              with ImagePhoto.Picture.Bitmap do begin
                PixelFormat := pf24bit;
                SetSize(GifImage.Width, GifImage.Height);
                Canvas.Draw(0, 0, GifImage);
              end;
              ProfilePhotoActive := True;
            except
              ProfilePhotoActive := False;
            end;
          finally
            GifImage.Free;
          end;
        end else begin
          try
            ImagePhoto.Picture.LoadFromStream(Stream);
            ProfilePhotoActive := True;
          except
            ProfilePhotoActive := False;
          end;
        end;
      finally
        Stream.Free;
      end;
    end else ProfilePhotoActive := False;
  end else ProfilePhotoActive := False;
end;

procedure TFormContact.TabSheetHomeHide(Sender: TObject);
begin
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

  ReloadAllPropertiesTab;
end;

procedure TFormContact.TabSheetHomeShow(Sender: TObject);
begin
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
end;

procedure TFormContact.TabSheetOthersHide(Sender: TObject);
begin
  Contact.Fields[cfNote] := MemoNotes.Lines.Text;

  ReloadAllPropertiesTab;
end;

procedure TFormContact.TabSheetOthersShow(Sender: TObject);
begin
  MemoNotes.Lines.Text := Contact.Fields[cfNote];
end;

procedure TFormContact.TabSheetSocialHide(Sender: TObject);
begin
  Contact.Fields[cfFacebook] := EditFacebook.Text;
  Contact.Fields[cfTwitter] := EditTwitter.Text;
  Contact.Fields[cfInstagram] := EditInstagram.Text;
  Contact.Fields[cfYouTube] := EditYouTube.Text;
  Contact.Fields[cfPeerTube] := EditPeerTube.Text;
  Contact.Fields[cfMastodon] := EditMastodon.Text;
  Contact.Fields[cfLinkedIn] := EditLinkedIn.Text;
  Contact.Fields[cfSnapchat] := EditSnapchat.Text;
  Contact.Fields[cfReddit] := EditReddit.Text;
  Contact.Fields[cfMySpace] := EditMySpace.Text;

  ReloadAllPropertiesTab;
end;

procedure TFormContact.TabSheetSocialShow(Sender: TObject);
begin
  EditFacebook.Text := Contact.Fields[cfFacebook];
  EditTwitter.Text := Contact.Fields[cfTwitter];
  EditInstagram.Text := Contact.Fields[cfInstagram];
  EditYouTube.Text := Contact.Fields[cfYouTube];
  EditPeerTube.Text := Contact.Fields[cfPeerTube];
  EditMastodon.Text := Contact.Fields[cfMastodon];
  EditLinkedIn.Text := Contact.Fields[cfLinkedIn];
  EditSnapchat.Text := Contact.Fields[cfSnapchat];
  EditReddit.Text := Contact.Fields[cfReddit];
  EditMySpace.Text := Contact.Fields[cfMySpace];
end;

procedure TFormContact.TabSheetWorkHide(Sender: TObject);
begin
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

  ReloadAllPropertiesTab;
end;

procedure TFormContact.TabSheetWorkShow(Sender: TObject);
begin
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
end;

procedure TFormContact.SetProfilePhotoActive(AValue: Boolean);
begin
  if FProfilePhotoActive = AValue then Exit;
  FProfilePhotoActive := AValue;
  if not FProfilePhotoActive then begin
    ImagePhoto.Picture.Assign(Core.GetProfileImage.Picture);
  end;
  UpdateInterface;
end;

procedure TFormContact.SetContact(AValue: TContact);
begin
  if FContact = AValue then Exit;
  FContact := AValue;
end;

procedure TFormContact.ReloadAllPropertiesTab;
begin
  if TabSheetAll.Visible then begin
    TabSheetAllShow(nil);
  end;
end;

procedure TFormContact.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  // Hide PageControl to fire TabSheet OnHide event on form close
  PageControlContact.ActivePage.Hide;

  Core.LastContactTabIndex := PageControlContact.TabIndex;
  Core.PersistentForm1.Save(Self);
end;

procedure TFormContact.ButtonNextClick(Sender: TObject);
begin
  if Assigned(FOnGetNext) then
     Contact := FOnGetNext(Contact);
end;

procedure TFormContact.APhotoLoadExecute(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then begin
    ImagePhoto.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    ProfilePhotoActive := True;
  end;
end;

procedure TFormContact.APhotoClearExecute(Sender: TObject);
begin
  ProfilePhotoActive := False;
end;

procedure TFormContact.APhotoSaveExecute(Sender: TObject);
begin
  if SavePictureDialog1.Execute then begin
    ImagePhoto.Picture.SaveToFile(SavePictureDialog1.FileName);
  end;
end;

procedure TFormContact.ButtonPreviousClick(Sender: TObject);
begin
  if Assigned(FOnGetPrevious) then
    Contact := FOnGetPrevious(Contact);
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

procedure TFormContact.UpdateInterface;
begin
  APhotoSave.Enabled := ProfilePhotoActive;
  APhotoClear.Enabled := ProfilePhotoActive;
  //ButtonNext.Enabled := Assigned(FOnGetNext) and Assigned(FOnGetNext(Contact));
  //ButtonPrevious.Enabled := Assigned(FOnGetPrevious) and Assigned(FOnGetPrevious(Contact));
end;

end.

