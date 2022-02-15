unit UFormImage;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ExtDlgs, UContactImage;

type

  { TFormImage }

  TFormImage = class(TForm)
    ButtonCancel: TButton;
    ButtonOk: TButton;
    ButtonLoad: TButton;
    ButtonSave: TButton;
    ButtonClear: TButton;
    EditUrl: TEdit;
    ImagePhoto: TImage;
    Label1: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    SavePictureDialog1: TSavePictureDialog;
    procedure uttonClearClick(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure EditUrlChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure ImageChange(Sender: TObject);
  public
    Image: TContactImage;
    procedure UpdateInterface;
  end;

var
  FormImage: TFormImage;


implementation

{$R *.lfm}

uses
  UCore;

{ TFormImage }

procedure TFormImage.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Core.PersistentForm1.Save(Self);
end;

procedure TFormImage.FormCreate(Sender: TObject);
begin
  Core.Translator.TranslateComponentRecursive(Self);
  Core.ThemeManager1.UseTheme(Self);

  Image := TContactImage.Create;
  Image.OnChange := ImageChange;
end;

procedure TFormImage.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Image);
end;

procedure TFormImage.ButtonLoadClick(Sender: TObject);
begin
  OpenPictureDialog1.FileName := Core.LastPhotoFileName;
  if OpenPictureDialog1.Execute then begin
    Image.LoadFromFile(OpenPictureDialog1.FileName);
    Core.LastPhotoFileName := OpenPictureDialog1.FileName;
  end;
end;

procedure TFormImage.uttonClearClick(Sender: TObject);
begin
  Image.Clear;
end;

procedure TFormImage.ButtonSaveClick(Sender: TObject);
begin
  SavePictureDialog1.FileName := Core.LastPhotoFileName;
  if SavePictureDialog1.Execute then begin
    Image.SaveToFile(SavePictureDialog1.FileName);
    Core.LastPhotoFileName := SavePictureDialog1.FileName;
  end;
end;

procedure TFormImage.EditUrlChange(Sender: TObject);
begin
  Image.Url := EditUrl.Text;
end;

procedure TFormImage.FormShow(Sender: TObject);
begin
  Core.PersistentForm1.Load(Self);

  ImageChange(nil);
end;

procedure TFormImage.ImageChange(Sender: TObject);
begin
  if Image.Used and (Image.Url = '') then ImagePhoto.Picture.Bitmap.Assign(Image.Bitmap)
    else ImagePhoto.Picture.Assign(Core.GetProfileImage.Picture);
  EditUrl.Text := Image.Url;
  UpdateInterface;
end;

procedure TFormImage.UpdateInterface;
begin
  ButtonSave.Enabled := Image.Used;
  ButtonClear.Enabled := Image.Used;
end;

end.

