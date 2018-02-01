unit UFormContacts;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Menus, ActnList, UContact;

type

  { TFormContacts }

  TFormContacts = class(TForm)
    AAdd: TAction;
    ARemove: TAction;
    AModify: TAction;
    ActionList1: TActionList;
    ListView1: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PopupMenuContact: TPopupMenu;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure AAddExecute(Sender: TObject);
    procedure AModifyExecute(Sender: TObject);
    procedure ARemoveExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FContacts: TContacts;
    procedure SetContacts(AValue: TContacts);

  public
    property Contacts: TContacts read FContacts write SetContacts;
    procedure ReloadList;
    procedure UpdateInterface;
  end;

var
  FormContacts: TFormContacts;


implementation

{$R *.lfm}

uses
  UFormContact, UCore;

resourcestring
  SRemoveContacts = 'Remove contacts';
  SRemoveContactsQuery = 'Do you want to remove selected contacts?';

{ TFormContacts }

procedure TFormContacts.ListView1Data(Sender: TObject; Item: TListItem);
begin
  if Assigned(Contacts) and (Item.Index < Contacts.Count) then
  with TContact(Contacts[Item.Index]) do begin
    Item.Caption := FirstName;
    Item.SubItems.Add(MiddleName);
    Item.SubItems.Add(LastName);
    Item.Data := Contacts[Item.Index];
  end;
end;

procedure TFormContacts.ListView1DblClick(Sender: TObject);
begin
  AModify.Execute;
end;

procedure TFormContacts.ListView1SelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  UpdateInterface;
end;

procedure TFormContacts.SetContacts(AValue: TContacts);
begin
  if FContacts = AValue then Exit;
  FContacts := AValue;
  ReloadList;
  UpdateInterface;
end;

procedure TFormContacts.FormShow(Sender: TObject);
begin
  Core.PersistentForm1.Load(Self);
  ReloadList;
  UpdateInterface;
end;

procedure TFormContacts.AAddExecute(Sender: TObject);
var
  FormContact: TFormContact;
  Contact: TContact;
begin
  FormContact := TFormContact.Create(nil);
  try
  if FormContact.ShowModal = mrOK then begin
    Contact := TContact.Create;
    Contact.Parent := Contacts.ContactsFile;
    FormContact.SaveData(Contact);
    Contacts.Add(Contact);
    Core.DataFile.Modified := True;
    ReloadList;
    UpdateInterface;
  end;
  finally
    FormContact.Free;
  end;
end;

procedure TFormContacts.AModifyExecute(Sender: TObject);
var
  FormContact: TFormContact;
begin
  FormContact := TFormContact.Create(nil);
  try
    FormContact.LoadData(TContact(ListView1.Selected.Data));
    if FormContact.ShowModal = mrOK then begin
      FormContact.SaveData(TContact(ListView1.Selected.Data));
      Core.DataFile.Modified := True;
      ReloadList;
      UpdateInterface;
    end;
  finally
    FormContact.Free;
  end;
end;

procedure TFormContacts.ARemoveExecute(Sender: TObject);
var
  I: Integer;
begin
  if Assigned(ListView1.Selected) then
  if MessageDlg(SRemoveContacts, SRemoveContactsQuery,
    TMsgDlgType.mtConfirmation, [mbCancel, mbOk], 0) = mrOk then begin
    for I := ListView1.Items.Count - 1 downto 0 do
      if ListView1.Items[I].Selected then begin
        Contacts.Delete(I);
      end;
    Core.DataFile.Modified := True;
    ReloadList;
    UpdateInterface;
  end;
end;

procedure TFormContacts.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  Core.PersistentForm1.Save(Self);
end;

procedure TFormContacts.FormCreate(Sender: TObject);
begin
  FContacts := nil;
end;

procedure TFormContacts.ReloadList;
begin
  if Assigned(Contacts) then
    ListView1.Items.Count := Contacts.Count
    else ListView1.Items.Count := 0;
  ListView1.Refresh;
end;

procedure TFormContacts.UpdateInterface;
begin
  AAdd.Enabled := Assigned(Contacts);
  AModify.Enabled := Assigned(Contacts) and Assigned(ListView1.Selected);
  ARemove.Enabled := Assigned(Contacts) and Assigned(ListView1.Selected);
end;

end.

