unit UFormSettings;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, Spin, ExtCtrls, ULanguages;

type

  { TFormSettings }

  TFormSettings = class(TForm)
    Bevel1: TBevel;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    CheckBoxReopenLastFileOnStart: TCheckBox;
    CheckBoxAutomaticDPI: TCheckBox;
    ComboBoxLanguage: TComboBox;
    ComboBoxTheme: TComboBox;
    EditDefaultVcardVersion: TEdit;
    EditMapUrl: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelDPI: TLabel;
    SpinEditDPI: TSpinEdit;
    procedure ButtonOkClick(Sender: TObject);
    procedure CheckBoxAutomaticDPIChange(Sender: TObject);
    procedure CheckBoxStartOnLogonChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    procedure LoadData;
    procedure SaveData;
    procedure UpdateInterface;
  end;

var
  FormSettings: TFormSettings;


implementation

{$R *.lfm}

uses
  UCore, UTheme;

{ TFormSettings }

procedure TFormSettings.FormShow(Sender: TObject);
begin
  Core.PersistentForm1.Load(Self);

  Core.Translator.LanguageListToStrings(ComboBoxLanguage.Items);
  ComboBoxLanguage.ItemIndex := ComboBoxLanguage.Items.IndexOfObject(Core.Translator.Language);
  if ComboBoxLanguage.ItemIndex = -1 then ComboBoxLanguage.ItemIndex := 0;

  Core.ThemeManager1.Themes.LoadToStrings(ComboBoxTheme.Items);
  ComboBoxTheme.ItemIndex := ComboBoxTheme.Items.IndexOfObject(Core.ThemeManager1.Theme);
  if ComboBoxTheme.ItemIndex = -1 then ComboBoxTheme.ItemIndex := 0;
end;

procedure TFormSettings.ButtonOkClick(Sender: TObject);
begin
  if ComboBoxLanguage.ItemIndex <> -1 then
    Core.Translator.Language := TLanguage(ComboBoxLanguage.Items.Objects[ComboBoxLanguage.ItemIndex]);
  if ComboBoxTheme.ItemIndex <> -1 then
    Core.ThemeManager1.Theme := TTheme(ComboBoxTheme.Items.Objects[ComboBoxTheme.ItemIndex]);
end;

procedure TFormSettings.CheckBoxAutomaticDPIChange(Sender: TObject);
begin
  UpdateInterface;
end;

procedure TFormSettings.CheckBoxStartOnLogonChange(Sender: TObject);
begin
  UpdateInterface;
end;

procedure TFormSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  Core.PersistentForm1.Save(Self);
end;

procedure TFormSettings.FormCreate(Sender: TObject);
begin
  Core.Translator.TranslateComponentRecursive(Self);
  Core.ThemeManager1.UseTheme(Self);
end;

procedure TFormSettings.LoadData;
begin
  CheckBoxAutomaticDPI.Checked := Core.ScaleDPI1.AutoDetect;
  SpinEditDPI.Value := Core.ScaleDPI1.DPI.X;
  CheckBoxReopenLastFileOnStart.Checked := Core.ReopenLastFileOnStart;
  EditDefaultVcardVersion.Text := Core.DefaultVcardVersion;
  EditMapUrl.Text := Core.MapUrl;
  UpdateInterface;
end;

procedure TFormSettings.SaveData;
begin
  Core.ScaleDPI1.AutoDetect := CheckBoxAutomaticDPI.Checked;
  Core.ScaleDPI1.DPI := Point(SpinEditDPI.Value, SpinEditDPI.Value);
  Core.ReopenLastFileOnStart := CheckBoxReopenLastFileOnStart.Checked;
  Core.DefaultVcardVersion := EditDefaultVcardVersion.Text;
  Core.MapUrl := EditMapUrl.Text;
end;

procedure TFormSettings.UpdateInterface;
begin
  SpinEditDPI.Enabled := not CheckBoxAutomaticDPI.Checked;
  LabelDPI.Enabled := not CheckBoxAutomaticDPI.Checked;
end;

end.

