unit UQuotedPrintable;

{$mode delphi}

interface

uses
  Classes, SysUtils;

function DecodeQuotedPrintable(Text: string; IgnoreErrors: Boolean = False): string;
function EncodeQuotedPrintable(Text: string; EncodeSpaces: Boolean = False): string;

const
  QuotedPrintableEscapeCharacter = '=';

implementation

uses
  UCommon;

resourcestring
  SDecodeError = 'Decode error';

function DecodeQuotedPrintable(Text: string; IgnoreErrors: Boolean = False): string;
var
  I: Integer;
  J: Integer;
  C: Char;
  IntValue: Integer;
begin
  Result := '';
  SetLength(Result, Length(Text));
  I := 1;
  J := 1;
  while I <= Length(Text) do begin
    C := Text[I];
    if C = QuotedPrintableEscapeCharacter then begin
      if ((I + 2) <= Length(Text)) and TryHexToInt(Text[I + 1] + Text[I + 2], IntValue) then begin
        Result[J] := Chr(IntValue);
        Inc(I, 2);
        Inc(J);
        SetLength(Result, Length(Result) - 2);
      end else begin
        if not IgnoreErrors then raise Exception.Create(SDecodeError)
        else begin
          Result[J] := '?';
          Inc(J);
        end;
      end;
    end else begin
      Result[J] := C;
      Inc(J);
    end;
    Inc(I);
  end;
end;

function EncodeQuotedPrintable(Text: string; EncodeSpaces: Boolean): string;
var
  I: Integer;
  J: Integer;
  C: Char;
  LowerLimit: Char;
const
  HexDigits : array[0..$F] of AnsiChar = '0123456789ABCDEF';
begin
  if EncodeSpaces then LowerLimit := #33
    else LowerLimit := #32;
  Result := '';
  SetLength(Result, Length(Text));
  I := 1;
  J := 1;
  while I <= Length(Text) do begin
    C := Text[I];
    if (C = QuotedPrintableEscapeCharacter) or (C < LowerLimit) or (C > #126) then begin
      SetLength(Result, Length(Result) + 2);
      Result[J] := QuotedPrintableEscapeCharacter;
      Result[J + 1] := HexDigits[Ord(C) shr 4];
      Result[J + 2] := HexDigits[Ord(C) and $f];
      Inc(J, 3);
    end else begin
      Result[J] := C;
      Inc(J);
    end;
    Inc(I);
  end;
end;

end.

