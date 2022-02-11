unit UContactImage;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Graphics, UContact, ExtCtrls;

type
  TContactImageFormat = (ifNone, ifBmp, ifJpeg, ifPng, ifGif);

  { TContactImage }

  TContactImage = class
  private
    FOnChange: TNotifyEvent;
    FUrl: string;
    FUsed: Boolean;
    function GetImageFormat(ContactProperty: TContactProperty): TContactImageFormat;
    procedure SetUrl(AValue: string);
    procedure SetUsed(AValue: Boolean);
    procedure DoOnChange;
    procedure SaveImageToStream(ImageFormat: TContactImageFormat;
      Stream: TStream);
    procedure LoadImageFromStream(ImageFormat: TContactImageFormat;
      Stream: TStream);
  public
    Bitmap: TBitmap;
    Format: TContactImageFormat;
    Loaded: Boolean;
    Modified: Boolean;
    Contact: TContact;
    FieldIndex: TContactFieldIndex;
    procedure Load;
    procedure Save;
    procedure Clear;
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    procedure Assign(Source: TContactImage);
    constructor Create;
    destructor Destroy; override;
    property Url: string read FUrl write SetUrl;
    property Used: Boolean read FUsed write SetUsed;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


implementation


{ TContactImage }

function TContactImage.GetImageFormat(ContactProperty: TContactProperty
  ): TContactImageFormat;
begin
  if (ContactProperty.Attributes.IndexOf('JPEG') <> -1) or
  (ContactProperty.Attributes.IndexOf('jpeg') <> -1) then Result := ifJpeg
  else
  if (ContactProperty.Attributes.IndexOf('GIF') <> -1) or
    (ContactProperty.Attributes.IndexOf('gif') <> -1) then Result := ifGif
  else
  if (ContactProperty.Attributes.IndexOf('PNG') <> -1) or
    (ContactProperty.Attributes.IndexOf('png') <> -1) then Result := ifPng
  else
    if (ContactProperty.Attributes.IndexOf('BMP') <> -1) or
      (ContactProperty.Attributes.IndexOf('bmp') <> -1) then Result := ifBmp
  else
    Result := ifNone;
end;

procedure TContactImage.SetUrl(AValue: string);
begin
  if FUrl = AValue then Exit;
  FUrl := AValue;
  Modified := True;
  Used := FUrl <> '';
  DoOnChange;
end;

procedure TContactImage.SetUsed(AValue: Boolean);
begin
  if FUsed = AValue then Exit;
  FUsed := AValue;
  DoOnChange;
end;

procedure TContactImage.DoOnChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TContactImage.SaveImageToStream(ImageFormat: TContactImageFormat;
  Stream: TStream);
var
  JpegImage: TJpegImage;
  PngImage: TPortableNetworkGraphic;
  GifImage: TGIFImage;
begin
  if ImageFormat = ifJpeg then begin
    JpegImage := TJPEGImage.Create;
    try
      try
        JpegImage.SetSize(Bitmap.Width, Bitmap.Height);
        JpegImage.Canvas.Draw(0, 0, Bitmap);
        JpegImage.SaveToStream(Stream);
      except
      end;
    finally
      JpegImage.Free;
    end;
  end else
  if ImageFormat = ifPng then begin
    PngImage := TPortableNetworkGraphic.Create;
    try
      try
        PngImage.SetSize(Bitmap.Width, Bitmap.Height);
        PngImage.Canvas.Draw(0, 0, Bitmap);
        PngImage.SaveToStream(Stream);
      except
      end;
    finally
      PngImage.Free;
    end;
  end else
  if ImageFormat = ifGif then begin
    GifImage := TGIFImage.Create;
    try
      try
        GifImage.SetSize(Bitmap.Width, Bitmap.Height);
        GifImage.Canvas.Draw(0, 0, Bitmap);
        GifImage.SaveToStream(Stream);
      except
      end;
    finally
      GifImage.Free;
    end;
  end else
  if ImageFormat = ifBmp then begin
    try
      Bitmap.SaveToStream(Stream);
    except
    end;
  end else begin
    // Use default type
    SaveImageToStream(ifJpeg, Stream);
  end;
end;

procedure TContactImage.LoadImageFromStream(ImageFormat: TContactImageFormat;
  Stream: TStream);
var
  JpegImage: TJpegImage;
  PngImage: TPortableNetworkGraphic;
  GifImage: TGIFImage;
  BmpImage: TBitmap;
begin
  if ImageFormat = ifJpeg then begin
    try
      JpegImage := TJPEGImage.Create;
      try
        JpegImage.LoadFromStream(Stream);
        with Bitmap do begin
          PixelFormat := pf24bit;
          SetSize(JpegImage.Width, JpegImage.Height);
          Canvas.Draw(0, 0, JpegImage);
        end;
      finally
        JpegImage.Free;
      end;
      Used := True;
    except
      Used := False;
    end;
  end else
  if ImageFormat = ifPng then begin
    try
      PngImage := TPortableNetworkGraphic.Create;
      try
        PngImage.LoadFromStream(Stream);
        with Bitmap do begin
          PixelFormat := pf24bit;
          SetSize(PngImage.Width, PngImage.Height);
          Canvas.Draw(0, 0, PngImage);
        end;
      finally
        PngImage.Free;
      end;
      Used := True;
    except
      Used := False;
    end;
  end else
  if ImageFormat = ifGif then begin
    try
      GifImage := TGIFImage.Create;
      try
        GifImage.LoadFromStream(Stream);
        with Bitmap do begin
          PixelFormat := pf24bit;
          SetSize(GifImage.Width, GifImage.Height);
          Canvas.Draw(0, 0, GifImage);
        end;
      finally
        GifImage.Free;
      end;
      Used := True;
    except
      Used := False;
    end;
  end else
  if ImageFormat = ifBmp then begin
    try
      BmpImage := TBitmap.Create;
      try
        BmpImage.LoadFromStream(Stream);
        with Bitmap do begin
          PixelFormat := pf24bit;
          SetSize(BmpImage.Width, BmpImage.Height);
          Canvas.Draw(0, 0, BmpImage);
        end;
      finally
        BmpImage.Free;
      end;
      Used := True;
    except
      Used := False;
    end;
  end else begin
    // Unknown image type, let TPicture guess what it is
    try
      with TImage.Create(nil) do
      try
        Picture.LoadFromStream(Stream);
        with Bitmap do begin
          PixelFormat := pf24bit;
          SetSize(Picture.Bitmap.Width, Picture.Bitmap.Height);
          Canvas.Draw(0, 0, Picture.Bitmap);
        end;
      finally
        Free;
      end;
      Used := True;
    except
      Used := False;
    end;
  end;
end;

procedure TContactImage.Load;
var
  PhotoProperty: TContactProperty;
  Photo: string;
  Stream: TMemoryStream;
begin
  PhotoProperty := Contact.GetProperty(FieldIndex);
  if not Loaded then begin
    if Assigned(PhotoProperty) then begin
      Loaded := True;
      Modified := True;
      Photo := Contact.Fields[FieldIndex];
      if (Photo <> '') and (PhotoProperty.Encoding <> '') then begin
        Stream := TMemoryStream.Create;
        try
          Stream.Write(Photo[1], Length(Photo));
          Stream.Position := 0;
          LoadImageFromStream(GetImageFormat(PhotoProperty), Stream);
        finally
          Stream.Free;
        end;
      end else begin
        Url := Photo;
        Used := True;
      end;
    end else Used := False;
  end;
end;

procedure TContactImage.Save;
var
  PhotoProperty: TContactProperty;
  Photo: string;
  Stream: TMemoryStream;
begin
  if Modified then begin
    if Used then begin
      PhotoProperty := Contact.GetProperty(FieldIndex);
      if not Assigned(PhotoProperty) then begin
        PhotoProperty := TContactProperty.Create;
        PhotoProperty.Name := 'PHOTO';
        PhotoProperty.Attributes.DelimitedText := 'JPEG';
        Contact.Properties.Add(PhotoProperty);
      end;
      if Url <> '' then begin
        Contact.Fields[FieldIndex] := Url;
      end else begin
        PhotoProperty.Encoding := VCardBase64;
        Stream := TMemoryStream.Create;
        try
          SaveImageToStream(GetImageFormat(PhotoProperty), Stream);
          Photo := '';
          SetLength(Photo, Stream.Size);
          Stream.Position := 0;
          Stream.Read(Photo[1], Length(Photo));
          Contact.Fields[FieldIndex] := Photo;
        finally
          Stream.Free;
        end;
      end;
    end else begin
      PhotoProperty := Contact.GetProperty(FieldIndex);
      if Assigned(PhotoProperty) then
         Contact.Properties.Remove(PhotoProperty);
    end;
    Modified := False;
  end;
end;

procedure TContactImage.Clear;
begin
  Url := '';
  Used := False;
end;

procedure TContactImage.LoadFromFile(FileName: string);
begin
  with TImage.Create(nil) do
  try
    Picture.LoadFromFile(FileName);
    Bitmap.Assign(Picture.Bitmap);
    Url := '';
    Used := True;
  finally
    Free;
  end;
end;

procedure TContactImage.SaveToFile(FileName: string);
begin
  with TImage.Create(nil) do
  try
    Picture.Bitmap.Assign(Bitmap);
    Picture.SaveToFile(FileName);
  finally
    Free;
  end;
end;

procedure TContactImage.Assign(Source: TContactImage);
begin
  Bitmap.Assign(Source.Bitmap);
  Url := Source.Url;
  Loaded := Source.Loaded;
  Modified := Source.Modified;
  Format := Source.Format;
  FieldIndex := Source.FieldIndex;
  Contact := Source.Contact;
  Used := Source.Used;
end;

constructor TContactImage.Create;
begin
  Bitmap := TBitmap.Create;
end;

destructor TContactImage.Destroy;
begin
  FreeAndNil(Bitmap);
  inherited;
end;


end.

