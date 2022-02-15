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
    ComboBoxField: TComboBox;
    EditName: TEdit;
    EditAttributes: TEdit;
    EditValues: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure ButtonOkClick(Sender: TObject);
    procedure ComboBoxFieldChange(Sender: TObject);
    procedure EditAttributesChange(Sender: TObject);
    procedure EditNameChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FContactProperty: TContactProperty;
    procedure UpdateField;
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

procedure TFormProperty.ComboBoxFieldChange(Sender: TObject);
var
  Field: TContactField;
  Attributes: TStringList;
  I: Integer;
begin
  if ComboBoxField.ItemIndex <> -1 then begin
    Field := TContactField(ComboBoxField.Items.Objects[ComboBoxField.ItemIndex]);
    if Assigned(Field) then begin
      EditName.Text := Field.SysName;
      Attributes := TStringList.Create;
      try
        Attributes.NameValueSeparator := '=';
        Attributes.Delimiter := ';';
        Attributes.StrictDelimiter := True;
        for I := 0 to Length(Field.Groups) - 1 do
          Attributes.Add(Field.Groups[I]);
        EditAttributes.Text := Attributes.DelimitedText;
      finally
        Attributes.Free;
      end;
    end;
  end;
end;

procedure TFormProperty.EditAttributesChange(Sender: TObject);
begin
  UpdateField;
end;

procedure TFormProperty.EditNameChange(Sender: TObject);
begin
  UpdateField;
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
  TContact.GetFields.LoadToStrings(ComboBoxField.Items);
end;

procedure TFormProperty.FormShow(Sender: TObject);
begin
  Core.PersistentForm1.Load(Self);
end;

procedure TFormProperty.UpdateField;
var
  Field: TContactField;
  Groups: TStringList;
  GroupsArray: TStringArray;
  I: Integer;
begin
  Groups := TStringList.Create;
  try
    Groups.NameValueSeparator := '=';
    Groups.Delimiter := ';';
    Groups.StrictDelimiter := True;
    Groups.DelimitedText := EditAttributes.Text;
    GroupsArray := Default(TStringArray);
    SetLength(GroupsArray, Groups.Count);
    for I := 0 to Groups.Count - 1 do
      GroupsArray[I] := Groups[I];
  finally
    Groups.Free;
  end;
  Field := TContact.GetFields.GetBySysNameGroups(EditName.Text,
    GroupsArray);
  if Assigned(Field) then
    ComboBoxField.ItemIndex := ComboBoxField.Items.IndexOfObject(Field);
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

