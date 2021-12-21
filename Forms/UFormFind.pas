unit UFormFind;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, ActnList, Menus, fgl, UContact, UFormContacts;

type

  { TFormFind }

  TFormFind = class(TForm)
    ButtonFind: TButton;
    ComboBoxField: TComboBox;
    EditValue: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    procedure ButtonFindClick(Sender: TObject);
    procedure ComboBoxFieldChange(Sender: TObject);
    procedure EditValueKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FContacts: TContacts;
    FormContacts: TFormContacts;
    procedure SetContacts(AValue: TContacts);
  public
    ContactFieldIndex: TContactFieldIndex;
    procedure Find;
    property Contacts: TContacts read FContacts write SetContacts;
  end;

var
  FormFind: TFormFind;


implementation

{$R *.lfm}

uses
  UCore;

resourcestring
  SAny = 'Any';

{ TFormFind }

procedure TFormFind.SetContacts(AValue: TContacts);
var
  ContactField: TContactField;
  Items: TStringList;
  I: Integer;
begin
  if FContacts = AValue then Exit;
  FContacts := AValue;
  if Assigned(FContacts) then begin
    Items := TStringList.Create;
    try
      TContact.GetFields.LoadToStrings(Items);

      // Remove fields which are not used in contacts
      for I := Items.Count - 1 downto 0 do
        if Contacts.CountByField(TContactField(Items.Objects[I]).Index) = 0 then
          Items.Delete(I);

      Items.InsertObject(0, SAny, nil);
      ComboBoxField.Items.Assign(Items);
    finally
      Items.Free;
    end;
    ContactField := TContact.GetFields.GetByIndex(ContactFieldIndex);
    ComboBoxField.ItemIndex := ComboBoxField.Items.IndexOfObject(ContactField);
    if (ComboBoxField.Items.Count > 0) and (ComboBoxField.ItemIndex = -1) then
      ComboBoxField.ItemIndex := 0;
  end else begin
    ComboBoxField.Clear;
  end;
  FormContacts.Contacts := Contacts;
end;

procedure TFormFind.Find;
begin
  with FormContacts.FilterItems do begin
    Clear;
    if EditValue.Text <> '' then
      AddNew(ContactFieldIndex, EditValue.Text);
  end;
  FormContacts.ReloadList;
  FormContacts.UpdateInterface;
end;

procedure TFormFind.FormCreate(Sender: TObject);
begin
  Core.Translator.TranslateComponentRecursive(Self);
  Core.ThemeManager1.UseTheme(Self);
  ContactFieldIndex := cfNone;

  FormContacts := TFormContacts.Create(nil);
  FormContacts.ListViewFilter1.Visible := False;
end;

procedure TFormFind.ComboBoxFieldChange(Sender: TObject);
var
  ContactField: TContactField;
begin
  if ComboBoxField.ItemIndex <> -1 then begin
      ContactField := TContactField(ComboBoxField.Items.Objects[ComboBoxField.ItemIndex]);
      if Assigned(ContactField) then
        ContactFieldIndex := ContactField.Index
        else ContactFieldIndex := cfNone;
    end else ContactFieldIndex := cfNone;
  Find;
end;

procedure TFormFind.EditValueKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then Find;
end;

procedure TFormFind.ButtonFindClick(Sender: TObject);
begin
  Find;
end;

procedure TFormFind.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  Core.PersistentForm1.Save(Self);
end;

procedure TFormFind.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FormContacts);
end;

procedure TFormFind.FormShow(Sender: TObject);
begin
  Core.PersistentForm1.Load(Self);

  FormContacts.ManualDock(Self, nil, alClient);
  FormContacts.Align := alClient;
  FormContacts.Show;

  Find;
end;

end.

