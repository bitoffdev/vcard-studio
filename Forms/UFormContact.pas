unit UFormContact;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ActnList, Menus, ExtCtrls, ExtDlgs, Buttons, UContact, LCLIntf,
  UFormProperties, DateUtils{$IFDEF LCLGTK2}, Gtk2Globals{$ENDIF}, UContactImage;

type

  { TFormContact }

  TFormContact = class(TForm)
    APhotoSetUrl: TAction;
    APhotoShow: TAction;
    APhotoClear: TAction;
    APhotoSave: TAction;
    APhotoLoad: TAction;
    ActionList1: TActionList;
    ButtonHomeAddressShow: TButton;
    ButtonCancel: TButton;
    ButtonWorkAddressShow: TButton;
    ButtonNext: TButton;
    ButtonOk: TButton;
    ButtonPrevious: TButton;
    CalendarDialog1: TCalendarDialog;
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
    EditWorkAddressCity: TEdit;
    EditWorkAddressCountry: TEdit;
    EditWorkAddressPostalCode: TEdit;
    EditWorkAddressPostOfficeBox: TEdit;
    EditWorkAddressRegion: TEdit;
    EditWorkAddressStreet: TEdit;
    EditWorkAddressStreetExtended: TEdit;
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
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    PageControlContact: TPageControl;
    PopupMenuPhoto: TPopupMenu;
    SavePictureDialog1: TSavePictureDialog;
    SpeedButtonBirthDay: TSpeedButton;
    SpeedButtonAniversary: TSpeedButton;
    SpeedButtonHomeEmail: TSpeedButton;
    SpeedButtonEmail: TSpeedButton;
    SpeedButtonWorkEmail: TSpeedButton;
    SpeedButtonWorkWeb: TSpeedButton;
    SpeedButtonWeb: TSpeedButton;
    SpeedButtonHomeWeb: TSpeedButton;
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
    procedure APhotoSetUrlExecute(Sender: TObject);
    procedure APhotoShowExecute(Sender: TObject);
    procedure ButtonHomeAddressShowClick(Sender: TObject);
    procedure ButtonNextClick(Sender: TObject);
    procedure ButtonPreviousClick(Sender: TObject);
    procedure ButtonWorkAddressShowClick(Sender: TObject);
    procedure EditFullNameChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImagePhotoClick(Sender: TObject);
    procedure SpeedButtonHomeWebClick(Sender: TObject);
    procedure SpeedButtonAniversaryClick(Sender: TObject);
    procedure SpeedButtonBirthDayClick(Sender: TObject);
    procedure SpeedButtonEmailClick(Sender: TObject);
    procedure SpeedButtonHomeEmailClick(Sender: TObject);
    procedure SpeedButtonWebClick(Sender: TObject);
    procedure SpeedButtonWorkEmailClick(Sender: TObject);
    procedure SpeedButtonWorkWebClick(Sender: TObject);
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
    FPhoto: TContactImage;
    procedure PhotoChange(Sender: TObject);
  private
    FContact: TContact;
    FOnGetNext: TGetContactEvent;
    FOnGetPrevious: TGetContactEvent;
    FormProperties: TFormProperties;
    procedure SetContact(AValue: TContact);
    procedure ReloadAllPropertiesTab;
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
  UCore, UCommon, UFormImage;

resourcestring
  SContact = 'Contact';
  SPhotoUrl = 'Photo URL';
  SPhotoUrlQuery = 'Enter URL for profile photo';

function DateToISO(Date: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd', Date);
end;

function URLEncode(Text: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Text) do begin
    if not (Text[I] in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '~', '.', ':', '/']) then
      Result := Result + '%' + IntToHex(Ord(Text[I]), 2)
    else
      Result := Result + Text[I];
  end;
end;

{$IF FPC_FULLVERSION<30200}
function TryISOStrToDate(const aString: string; out OutDate: TDateTime): Boolean;
var
  xYear, xMonth, xDay: LongInt;
begin
  case Length(aString) of
    8: Result :=
      TryStrToInt(Copy(aString, 1, 4), xYear) and
      TryStrToInt(Copy(aString, 5, 2), xMonth) and
      TryStrToInt(Copy(aString, 7, 2), xDay) and
      TryEncodeDate(xYear, xMonth, xDay, OutDate);
    10: Result :=
      TryStrToInt(Copy(aString, 1, 4), xYear) and
      TryStrToInt(Copy(aString, 6, 2), xMonth) and
      TryStrToInt(Copy(aString, 9, 2), xDay) and
      TryEncodeDate(xYear, xMonth, xDay, OutDate);
  else
    Result := False;
  end;
  if not Result then
    OutDate := 0;
end;
{$ENDIF}

{ TFormContact }

procedure TFormContact.FormShow(Sender: TObject);
begin
  {$IFDEF LCLGTK2}
  // GTK2 bug workaround https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/35720
  if Visible then LastMouse.WinControl := PageControlContact.ActivePage;
  {$ENDIF}
  Core.PersistentForm1.Load(Self);

  FormProperties.ManualDock(TabSheetAll, nil, alClient);
  FormProperties.Align := alClient;
  FormProperties.Show;

  PhotoChange(nil);

  PageControlContact.TabIndex := Core.LastContactTabIndex;
  UpdateInterface;
end;

procedure TFormContact.ImagePhotoClick(Sender: TObject);
begin
  APhotoShow.Execute;
end;

procedure TFormContact.SpeedButtonHomeWebClick(Sender: TObject);
begin
  if EditHomeWeb.Text <> '' then OpenURL(EditHomeWeb.Text);
end;

procedure TFormContact.SpeedButtonAniversaryClick(Sender: TObject);
var
  Date: TDateTime;
begin
  if TryISOStrToDate(EditAniversary.Text, Date) then
    CalendarDialog1.Date := Date
    else CalendarDialog1.Date := Now;
  if CalendarDialog1.Execute then
    EditAniversary.Text := DateToISO(CalendarDialog1.Date);
end;

procedure TFormContact.SpeedButtonBirthDayClick(Sender: TObject);
var
  Date: TDateTime;
begin
  if TryISOStrToDate(EditBirthday.Text, Date) then
    CalendarDialog1.Date := Date
    else CalendarDialog1.Date := Now;
  if CalendarDialog1.Execute then
    EditBirthday.Text := DateToISO(CalendarDialog1.Date);
end;

procedure TFormContact.SpeedButtonEmailClick(Sender: TObject);
begin
  if EditEmail.Text <> '' then OpenEmail(EditEmail.Text);
end;

procedure TFormContact.SpeedButtonHomeEmailClick(Sender: TObject);
begin
  if EditHomeEmail.Text <> '' then OpenEmail(EditHomeEmail.Text);
end;

procedure TFormContact.SpeedButtonWebClick(Sender: TObject);
begin
  if EditWeb.Text <> '' then OpenURL(EditWeb.Text);
end;

procedure TFormContact.SpeedButtonWorkEmailClick(Sender: TObject);
begin
  if EditWorkEmail.Text <> '' then OpenEmail(EditWorkEmail.Text);
end;

procedure TFormContact.SpeedButtonWorkWebClick(Sender: TObject);
begin
  if EditWorkWeb.Text <> '' then OpenURL(EditWorkWeb.Text);
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

  FPhoto.Contact := Contact;
  FPhoto.Save;

  ReloadAllPropertiesTab;
end;

procedure TFormContact.TabSheetGeneralShow(Sender: TObject);
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

  FPhoto.Contact := Contact;
  FPhoto.Load;
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
  Contact.Fields[cfWorkAddressPostOfficeBox] := EditWorkAddressPostOfficeBox.Text;
  Contact.Fields[cfWorkAddressStreet] := EditWorkAddressStreet.Text;
  Contact.Fields[cfWorkAddressStreetExtended] := EditWorkAddressStreetExtended.Text;
  Contact.Fields[cfWorkAddressCity] := EditWorkAddressCity.Text;
  Contact.Fields[cfWorkAddressRegion] := EditWorkAddressRegion.Text;
  Contact.Fields[cfWorkAddressCountry] := EditWorkAddressCountry.Text;
  Contact.Fields[cfWorkAddressPostalCode] := EditWorkAddressPostalCode.Text;
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
  EditWorkAddressPostOfficeBox.Text := Contact.Fields[cfWorkAddressPostOfficeBox];
  EditWorkAddressStreet.Text := Contact.Fields[cfWorkAddressStreet];
  EditWorkAddressStreetExtended.Text := Contact.Fields[cfWorkAddressStreetExtended];
  EditWorkAddressCity.Text := Contact.Fields[cfWorkAddressCity];
  EditWorkAddressRegion.Text := Contact.Fields[cfWorkAddressRegion];
  EditWorkAddressCountry.Text := Contact.Fields[cfWorkAddressCountry];
  EditWorkAddressPostalCode.Text := Contact.Fields[cfWorkAddressPostalCode];
  EditWorkWeb.Text := Contact.Fields[cfUrlWork];
end;

procedure TFormContact.PhotoChange(Sender: TObject);
begin
  if FPhoto.Used and (FPhoto.Url = '') then
    ImagePhoto.Picture.Bitmap.Assign(FPhoto.Bitmap)
    else ImagePhoto.Picture.Assign(Core.GetProfileImage.Picture);
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
  OpenPictureDialog1.FileName := Core.LastPhotoFileName;
  if OpenPictureDialog1.Execute then begin
    FPhoto.LoadFromFile(OpenPictureDialog1.FileName);
    Core.LastPhotoFileName := OpenPictureDialog1.FileName;
  end;
end;

procedure TFormContact.APhotoClearExecute(Sender: TObject);
begin
  FPhoto.Clear;
end;

procedure TFormContact.APhotoSaveExecute(Sender: TObject);
begin
  SavePictureDialog1.FileName := Core.LastPhotoFileName;
  if SavePictureDialog1.Execute then begin
    ImagePhoto.Picture.SaveToFile(SavePictureDialog1.FileName);
    Core.LastPhotoFileName := SavePictureDialog1.FileName;
  end;
end;

procedure TFormContact.APhotoSetUrlExecute(Sender: TObject);
begin
  FPhoto.Url := InputBox(SPhotoUrl, SPhotoUrlQuery, FPhoto.Url);
end;

procedure TFormContact.APhotoShowExecute(Sender: TObject);
begin
  with TFormImage.Create(nil) do
  try
    Image.Assign(FPhoto);
    if ShowModal = mrOK then begin
      FPhoto.Assign(Image);
      UpdateInterface;
    end;
  finally
    Free;
  end;
end;

procedure TFormContact.ButtonHomeAddressShowClick(Sender: TObject);
var
  Address: string;
begin
  Address := '';
  if EditHomeAddressStreet.Text <> '' then Address := Address + ' ' + EditHomeAddressStreet.Text;
  if EditHomeAddressStreetExtended.Text <> '' then Address := Address + ' ' + EditHomeAddressStreetExtended.Text;
  if EditHomeAddressPostOfficeBox.Text <> '' then Address := Address + ' ' + EditHomeAddressPostOfficeBox.Text;
  if EditHomeAddressCity.Text <> '' then Address := Address + ' ' + EditHomeAddressCity.Text;
  if EditHomeAddressCountry.Text <> '' then Address := Address + ' ' + EditHomeAddressCountry.Text;
  if Trim(Address) <> '' then
    OpenURL(Core.MapUrl + URLEncode(Trim(Address)));
end;

procedure TFormContact.ButtonPreviousClick(Sender: TObject);
begin
  if Assigned(FOnGetPrevious) then
    Contact := FOnGetPrevious(Contact);
end;

procedure TFormContact.ButtonWorkAddressShowClick(Sender: TObject);
var
  Address: string;
begin
  Address := '';
  if EditWorkAddressStreet.Text <> '' then Address := Address + ' ' + EditWorkAddressStreet.Text;
  if EditWorkAddressStreetExtended.Text <> '' then Address := Address + ' ' + EditWorkAddressStreetExtended.Text;
  if EditWorkAddressPostOfficeBox.Text <> '' then Address := Address + ' ' + EditWorkAddressPostOfficeBox.Text;
  if EditWorkAddressCity.Text <> '' then Address := Address + ' ' + EditWorkAddressCity.Text;
  if EditWorkAddressCountry.Text <> '' then Address := Address + ' ' + EditWorkAddressCountry.Text;
  if Trim(Address) <> '' then
    OpenURL(Core.MapUrl + URLEncode(Trim(Address)));
end;

procedure TFormContact.EditFullNameChange(Sender: TObject);
begin
  UpdateInterface;
end;

procedure TFormContact.FormCreate(Sender: TObject);
begin
  Core.Translator.TranslateComponentRecursive(Self);
  Core.ThemeManager1.UseTheme(Self);
  FContact := nil;
  FormProperties := TFormProperties.Create(nil);
  FPhoto := TContactImage.Create;
  FPhoto.FieldIndex := cfPhoto;
  FPhoto.OnChange := PhotoChange;
end;

procedure TFormContact.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPhoto);
  FreeAndNil(FormProperties);
end;

procedure TFormContact.UpdateInterface;
begin
  Caption := EditFullName.Text + ' - ' + SContact;
  APhotoSave.Enabled := FPhoto.Used;
  APhotoClear.Enabled := FPhoto.Used;
  //ButtonNext.Enabled := Assigned(FOnGetNext) and Assigned(FOnGetNext(Contact));
  //ButtonPrevious.Enabled := Assigned(FOnGetPrevious) and Assigned(FOnGetPrevious(Contact));
end;

end.

