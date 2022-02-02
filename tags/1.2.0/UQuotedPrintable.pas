unit UQuotedPrintable;

{$mode delphi}

interface

uses
  Classes, SysUtils;

function DecodeQuotedPrintable(Text: string): string;
function EncodeQuotedPrintable(Text: string): string;


implementation

resourcestring
  SLineLengthErr = 'Invalid line length for encoded text';

const
  MaxLine = 1000;

function DecodeQuotedPrintable(Text: string): string;
var
  O, Count, WS: Integer;
  I: integer;
  InBuf: array[0..Pred(MaxLine)] of Byte;
  OutBuf: array[0..Pred(MaxLine)] of Byte;
  Decoding: Boolean;
  Keeper: Boolean;
  Abort: Boolean;
  InStream: TMemoryStream;
  OutStream: TMemoryStream;
begin
  Result := '';
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  try
  if Text <> '' then begin
    InStream.Write(Text[1], Length(Text));
    InStream.Position := 0;
  end;
  Abort := False;
  FillChar(InBuf, SizeOf(InBuf), #0);
  WS := $FF;
  Decoding := True;
  Keeper := False;

  { Skip any CR/LF's to get to the encoded stuff }
  while True do begin
    if InStream.Read(Char(InBuf[0]), 1) = 0then
      Exit;
    if ((InBuf[0] <> $0D) and (InBuf[0] <> $0A)) then begin
      Keeper := True;
      Break;
    end;
  end;

  while Decoding and not Abort do begin
    { Initialize }
    if Keeper then begin
      I := 1;
      Keeper := False;
    end else begin
      I := 0;
    end;
    O := 0;

    { Read in one line at a time - skipping over bad characters }
    while True do begin
      if (I > High(InBuf)) then
        raise Exception.Create(SLineLengthErr);
      if InStream.Read(Char(InBuf[I]), 1) = 0 then
        Break;
      case InBuf[I] of
        $0A : Continue;
        $0D : begin
                Inc(I);
                Break;
              end;
       { Test for potential end of data }
       { '--' is probably the next Mime boundary }
       { $2D : if (I = 1) and (InBuf[0] = $2D) then Exit;}
      end;
      Inc(I);
    end;

    if I = 0 then Break;
    Count := I;
    I := 0;

    { Decode data to output stream }
    while I < Count do begin
      case InBuf[I] of
        9       : begin
                    if WS = $FF then
                      WS := O;
                    OutBuf[O] := InBuf[I];
                    Inc(O);
                    Inc(I);
                  end;
        13      : if WS = $FF then begin
                    OutBuf[O] := 13;
                    OutBuf[O+1] := 10;
                    Inc(O, 2);
                    Inc(I);
                  end else begin
                    OutBuf[WS] := 13;
                    OutBuf[WS+1] := 10;
                    O := WS+2;
                    Inc(I);
                  end;
        32      : begin
                    if WS = $FF then
                      WS := O;
                    OutBuf[O] := InBuf[I];
                    Inc(O);
                    Inc(I);
                  end;
        33..60  : begin
                    WS := $FF;
                    OutBuf[O] := InBuf[I];
                    Inc(O);
                    Inc(I);
                  end;
        61      : begin
                    WS := $FF;
                    if I+2 >= Count then Break;
                    case InBuf[I+1] of
                      48 : OutBuf[O] := 0;    {0}
                      49 : OutBuf[O] := 16;   {1}
                      50 : OutBuf[O] := 32;   {2}
                      51 : OutBuf[O] := 48;   {3}
                      52 : OutBuf[O] := 64;   {4}
                      53 : OutBuf[O] := 80;   {5}
                      54 : OutBuf[O] := 96;   {6}
                      55 : OutBuf[O] := 112;  {7}
                      56 : OutBuf[O] := 128;  {8}
                      57 : OutBuf[O] := 144;  {9}
                      65 : OutBuf[O] := 160;  {A}
                      66 : OutBuf[O] := 176;  {B}
                      67 : OutBuf[O] := 192;  {C}
                      68 : OutBuf[O] := 208;  {D}
                      69 : OutBuf[O] := 224;  {E}
                      70 : OutBuf[O] := 240;  {F}
                      97 : OutBuf[O] := 160;  {a}
                      98 : OutBuf[O] := 176;  {b}
                      99 : OutBuf[O] := 192;  {c}
                     100 : OutBuf[O] := 208;  {d}
                     101 : OutBuf[O] := 224;  {e}
                     102 : OutBuf[O] := 240;  {f}
                    end;
                    case InBuf[I+2] of
                      48 : ;                             {0}
                      49 : OutBuf[O] := OutBuf[O] + 1;   {1}
                      50 : OutBuf[O] := OutBuf[O] + 2;   {2}
                      51 : OutBuf[O] := OutBuf[O] + 3;   {3}
                      52 : OutBuf[O] := OutBuf[O] + 4;   {4}
                      53 : OutBuf[O] := OutBuf[O] + 5;   {5}
                      54 : OutBuf[O] := OutBuf[O] + 6;   {6}
                      55 : OutBuf[O] := OutBuf[O] + 7;   {7}
                      56 : OutBuf[O] := OutBuf[O] + 8;   {8}
                      57 : OutBuf[O] := OutBuf[O] + 9;   {9}
                      65 : OutBuf[O] := OutBuf[O] + 10;  {A}
                      66 : OutBuf[O] := OutBuf[O] + 11;  {B}
                      67 : OutBuf[O] := OutBuf[O] + 12;  {C}
                      68 : OutBuf[O] := OutBuf[O] + 13;  {D}
                      69 : OutBuf[O] := OutBuf[O] + 14;  {E}
                      70 : OutBuf[O] := OutBuf[O] + 15;  {F}
                      97 : OutBuf[O] := OutBuf[O] + 10;  {a}
                      98 : OutBuf[O] := OutBuf[O] + 11;  {b}
                      99 : OutBuf[O] := OutBuf[O] + 12;  {c}
                     100 : OutBuf[O] := OutBuf[O] + 13;  {d}
                     101 : OutBuf[O] := OutBuf[O] + 14;  {e}
                     102 : OutBuf[O] := OutBuf[O] + 15;  {f}
                    end;
                    Inc(I, 3);
                    Inc(O);
                  end;
        62..126 : begin
                    WS := $FF;
                    OutBuf[O] := InBuf[I];
                    Inc(O);
                    Inc(I);
                  end;
        else
          Inc(I);
      end;
    end;

    if O > 0 then
      OutStream.Write(OutBuf, O)
    else
      Break;   { OutBuf is empty }
  end;
  SetLength(Result, OutStream.Size);
  OutStream.Position := 0;
  if OutStream.Size > 0 then
    OutStream.Read(Result[1], Length(Result));
  finally
    OutStream.Free;
    InStream.Free;
  end;
end;

function EncodeQuotedPrintable(Text: string): string;
var
  O, W: Integer;
  WordBuf, OutBuf: array[0..80] of AnsiChar;
  CurChar: AnsiChar;
  Abort: Boolean;
  InStream: TStream;
  OutStream: TMemoryStream;

  procedure SendLine;
  begin
    if (OutBuf[O - 1] = #9) or (OutBuf[O - 1] = #32) then begin
      OutBuf[O] := '=';
      Inc(O);
    end;
    OutStream.Write(OutBuf, O);
    FillChar(OutBuf, SizeOf(OutBuf), #0);
    O := 0;
  end;

  procedure AddWordToOutBuf;
  var
    J : Integer;
  begin
    if (O + W) > 74 then SendLine;
    for J := 0 to (W - 1) do begin
      OutBuf[O] := WordBuf[J];
      Inc(O);
    end;
    W := 0;
  end;

  procedure AddHexToWord(B : Byte);
  const
    HexDigits : array[0..$F] of AnsiChar = '0123456789ABCDEF';
  begin
    if W > 73 then AddWordToOutBuf;
    WordBuf[W] := '=';
    WordBuf[W + 1] := HexDigits[B shr 4];
    WordBuf[W + 2] := HexDigits[B and $F];
    Inc(W, 3)
  end;

begin
  Result := '';
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  try
    if Text <> '' then begin
      InStream.Write(Text[1], Length(Text));
      InStream.Position := 0;
    end;

    Abort := False;
    O := 0;
    W := 0;
    FillChar(OutBuf, SizeOf(OutBuf), #0);
    while (InStream.Read(CurChar, 1) = 1) and not Abort do begin
      if (Ord(CurChar) in [33..60, 62..126]) then begin
        WordBuf[W] := CurChar;
        Inc(W);
        if W > 74 then AddWordToOutBuf;
      end else if (CurChar = ' ') or (CurChar = #9) then begin
        WordBuf[W] := CurChar;
        Inc(W);
        AddWordToOutBuf;
      end else if (CurChar = #13) then begin
        AddWordToOutBuf;
        SendLine;
      end else if (CurChar = #10) then begin
        { Do nothing }
      end else begin
        AddHexToWord(Byte(CurChar));
      end;
    end;
    AddWordToOutBuf;
    OutStream.Write(OutBuf, O);
    SetLength(Result, OutStream.Size);
    OutStream.Position := 0;
    if OutStream.Size > 0 then
      OutStream.Read(Result[1], Length(Result));
  finally
    OutStream.Free;
    InStream.Free;
  end;
end;

end.
