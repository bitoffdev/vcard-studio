unit UFormSource;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, Menus,
  StdCtrls, SynEdit, SynHighlighterAny, UVCardHighlighter;

type

  { TFormSource }

  TFormSource = class(TForm)
    APaste: TAction;
    ACopy: TAction;
    ACut: TAction;
    ASelectAll: TAction;
    ActionList1: TActionList;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PopupMenu1: TPopupMenu;
    SynEditSource: TSynEdit;
    procedure ACopyExecute(Sender: TObject);
    procedure ACutExecute(Sender: TObject);
    procedure APasteExecute(Sender: TObject);
    procedure ASelectAllExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    VCardHighlighter: TSynVCardHighlighter;
    function GetSource: string;
    procedure SetSource(AValue: string);
    procedure UpdateTheme;
  public
    property Source: string read GetSource write SetSource;
  end;

var
  FormSource: TFormSource;

implementation

{$R *.lfm}

uses
  UCore, UTheme, UContact;

{ TFormSource }

procedure TFormSource.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Core.PersistentForm1.Save(Self);
end;

procedure TFormSource.ASelectAllExecute(Sender: TObject);
begin
  SynEditSource.SelectAll;
end;

procedure TFormSource.APasteExecute(Sender: TObject);
begin
  SynEditSource.PasteFromClipboard;
end;

procedure TFormSource.ACopyExecute(Sender: TObject);
begin
  SynEditSource.CopyToClipboard;
end;

procedure TFormSource.ACutExecute(Sender: TObject);
begin
  SynEditSource.CutToClipboard;
end;

procedure TFormSource.FormCreate(Sender: TObject);
var
  ContactFields: TContactFields;
  I: Integer;
begin
  Core.Translator.TranslateComponentRecursive(Self);
  Core.ThemeManager1.UseTheme(Self);

  VCardHighlighter := TSynVCardHighlighter.Create(nil);
  ContactFields := TContact.GetFields;
  SetLength(VCardHighlighter.Properties, ContactFields.Count);
  for I := 0 to ContactFields.Count - 1 do
    VCardHighlighter.Properties[I] := LowerCase(ContactFields[I].SysName);
  SynEditSource.Highlighter := VCardHighlighter;
end;

procedure TFormSource.FormDestroy(Sender: TObject);
begin
  SynEditSource.Highlighter := nil;
  FreeAndNil(VCardHighlighter);
end;

procedure TFormSource.FormShow(Sender: TObject);
begin
  Core.PersistentForm1.Load(Self);
  UpdateTheme;
end;

function TFormSource.GetSource: string;
begin
  Result := SynEditSource.Lines.Text;
end;

procedure TFormSource.SetSource(AValue: string);
begin
  SynEditSource.Lines.Text := AValue;
end;

procedure TFormSource.UpdateTheme;
const
  clLightBlue = TColor($FF8080);
  clLightGreen = TColor($80FF80);
  clLightRed = TColor($8080FF);
var
  C: TColor;
begin
  if Core.ThemeManager1.Theme.Name = ThemeNameDark then begin
    SynEditSource.Color := clBlack;
    VCardHighlighter.IdentAttri.Foreground := clWhite;
    VCardHighlighter.KeywordAttri.Foreground := clLightBlue;
    VCardHighlighter.NumberAttri.Foreground := clLightGreen;
    VCardHighlighter.PropertyAttri.Foreground := clLightRed;
  end else
  if Core.ThemeManager1.Theme.Name = ThemeNameLight then begin
    SynEditSource.Color := clWhite;
    VCardHighlighter.IdentAttri.Foreground := clBlack;
    VCardHighlighter.KeywordAttri.Foreground := clBlue;
    VCardHighlighter.NumberAttri.Foreground := clGreen;
    VCardHighlighter.PropertyAttri.Foreground := clRed;
  end else begin
    SynEditSource.Color := clWindow;
    VCardHighlighter.IdentAttri.Foreground := clWindowText;
    C := ColorToRGB(clWindow);
    if (((C and $ff) + ((C shr 8) and $ff) + ((C shr 16) and $ff)) div 3) > $7f then begin
      VCardHighlighter.KeywordAttri.Foreground := clBlue;
      VCardHighlighter.NumberAttri.Foreground := clGreen;
      VCardHighlighter.PropertyAttri.Foreground := clRed;
    end else begin
      VCardHighlighter.KeywordAttri.Foreground := clLightBlue;
      VCardHighlighter.NumberAttri.Foreground := clLightGreen;
      VCardHighlighter.PropertyAttri.Foreground := clLightRed;
    end;
  end;
end;

end.

