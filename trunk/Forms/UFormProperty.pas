unit UFormProperty;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UContact;

type

  { TFormProperty }

  TFormProperty = class(TForm)
    ButtonCancel: TButton;
    ButtonOk: TButton;
    EditName: TEdit;
    EditAttributes: TEdit;
    EditValues: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure ButtonOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FContactProperty: TContactProperty;
    procedure SetContactProperty(AValue: TContactProperty);
    procedure LoadData;
    procedure SaveData;
  public
    property ContactProperty: TContactProperty read FContactProperty write SetContactProperty;
  end;

var
  FormProperty: TFormProperty;

implementation

{$R *.lfm}

uses
  UCore;

{ TFormProperty }

procedure TFormProperty.ButtonOkClick(Sender: TObject);
begin
  SaveData;
end;

procedure TFormProperty.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Core.PersistentForm1.Save(Self);
end;

procedure TFormProperty.FormCreate(Sender: TObject);
begin
  Core.Translator.TranslateComponentRecursive(Self);
  Core.ThemeManager1.UseTheme(Self);
  FContactProperty := nil;
end;

procedure TFormProperty.FormShow(Sender: TObject);
begin
  Core.PersistentForm1.Load(Self);
end;

procedure TFormProperty.SetContactProperty(AValue: TContactProperty);
begin
  if FContactProperty = AValue then Exit;
  FContactProperty := AValue;
  LoadData;
end;

procedure TFormProperty.LoadData;
begin
  if Assigned(FContactProperty) then begin
    EditName.Text := FContactProperty.Name;
    EditAttributes.Text := FContactProperty.Attributes.DelimitedText;
    EditValues.Text := FContactProperty.Value;
  end else begin
    EditName.Text := '';
    EditAttributes.Text := '';
    EditValues.Text := '';
  end;
  EditName.Enabled := Assigned(FContactProperty);
  EditAttributes.Enabled := Assigned(FContactProperty);
  EditValues.Enabled := Assigned(FContactProperty);
end;

procedure TFormProperty.SaveData;
begin
  FContactProperty.Name := EditName.Text;
  FContactProperty.Attributes.DelimitedText := EditAttributes.Text;
  FContactProperty.Value := EditValues.Text;
end;

end.

