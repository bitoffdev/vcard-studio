unit USynEditEx;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, LMessages, Messages, LCLType, LCLIntf, SynEdit;

type

  { TSynEditEx }

  TSynEditEx = class(TSynEdit)
  private
    FOnScroll: TNotifyEvent;
    procedure WMVScroll(var Msg: TLMScroll); message WM_VSCROLL;
  published
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
  end;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('Common', [TSynEditEx]);
end;


{ TSynEditEx }

procedure TSynEditEx.WMVScroll(var Msg: TLMScroll);
begin
  if Assigned(FOnScroll) then
    FOnScroll(Self);
  inherited;
end;


end.

