unit UFormContacts;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, UContact;

type

  { TFormContacts }

  TFormContacts = class(TForm)
    ListView1: TListView;
    procedure FormShow(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
  private

  public
    Contacts: TContacts;
    procedure ReloadList;
  end;

var
  FormContacts: TFormContacts;

implementation

{$R *.lfm}

{ TFormContacts }

procedure TFormContacts.ListView1Data(Sender: TObject; Item: TListItem);
begin
  if Assigned(Contacts) and (Item.Index < Contacts.Count) then
  with TContact(Contacts[Item.Index]) do begin
    Item.Caption := FullName;
    Item.Data := Contacts[Item.Index];
  end;
end;

procedure TFormContacts.FormShow(Sender: TObject);
begin
  ReloadList;
end;

procedure TFormContacts.ReloadList;
begin
  if Assigned(Contacts) then
    ListView1.Items.Count := Contacts.Count
    else ListView1.Clear;
  ListView1.Refresh;
end;

end.

