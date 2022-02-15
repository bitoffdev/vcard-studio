unit UVCardHighlighter;
(*
  This is an example how to implement your own highlighter.

  This example does allow to specify different colors for
  - text (defaults to not-highlighted)
  - spaces  (defaults to silver frame)
  - words, separated by spaces, that start with a,e,i,o,u  (defaults to bold)
  - the word "not"  (defaults to red background)

  See comments below and http://wiki.lazarus.freepascal.org/SynEdit_Highlighter

  How it works:

  - Creation
    The Highlighter creates Attributes that it can return the Words and Spaces.

  - SetLine
    Is called by SynEdit before a line gets painted (or before highlight info is needed)
    This is also called, each time the text changes fol *all* changed lines
    and may even be called for all lines after the change up to the end of text.

    After SetLine was called "GetToken*" should return information about the
    first token on the line.
    Note: Spaces are token too.

  - Next
    Scan to the next token, on the line that was set by "SetLine"
    "GetToken*"  should return info about that next token.

  - GetEOL
    Returns True, if "Next" was called while on the last token of the line.

  - GetTokenEx, GetTokenAttribute
    Provide info about the token found by "Next"

  - Next, GetEOL. GetToken*
    Are used by SynEdit to iterate over the Line.
    Important: The tokens returned for each line, must represent the original
    line-text (mothing added, nothing left out), and be returned in the correct order.

    They are called very often and should perform ath high speed.

  - GetToken, GetTokenPos, GetTokenKind
    SynEdit uses them e.g for finding matching brackets. If GetTokenKind returns different values per Attribute, then brackets only match, if they are of the same kind (e.g, if there was a string attribute, brackets outside a string would not match brackets inside a string)


*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEditHighlighter;

type

  { TSynVCardHighlighter }


  TSynVCardHighlighter = class(TSynCustomHighlighter)
  private
    FSpaceAttri: TSynHighlighterAttributes;
    FKeywordAttri: TSynHighlighterAttributes;
    FIdentAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FPropertyAttri: TSynHighlighterAttributes;
    procedure SetIdentAttri(AValue: TSynHighlighterAttributes);
    procedure SetKeywordAttri(AValue: TSynHighlighterAttributes);
    procedure SetNumberAttri(AValue: TSynHighlighterAttributes);
    procedure SetPropertyAttri(AValue: TSynHighlighterAttributes);
    procedure SetSpaceAttri(AValue: TSynHighlighterAttributes);
    function IsDigit(C: Char): Boolean;
    function IsWhiteSpace(C: Char): Boolean;
    function IsNumber(Text: string): Boolean;
    function IsProperty(Text: string): Boolean;
  protected
    // accesible for the other examples
    FTokenPos: Integer;
    FTokenEnd: Integer;
    FLineText: String;
  public
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;
    function  GetEol: Boolean; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function  GetTokenAttribute: TSynHighlighterAttributes; override;
  public
    Properties: array of string;
    function GetToken: String; override;
    function GetTokenPos: Integer; override;
    function GetTokenKind: integer; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    constructor Create(AOwner: TComponent); override;
  published
    property KeywordAttri: TSynHighlighterAttributes read FKeywordAttri
      write SetKeywordAttri;
    property IdentAttri: TSynHighlighterAttributes read FIdentAttri
      write SetIdentAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write SetSpaceAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write SetNumberAttri;
    property PropertyAttri: TSynHighlighterAttributes read FPropertyAttri
      write SetPropertyAttri;
  end;

implementation

constructor TSynVCardHighlighter.Create(AOwner: TComponent);
begin
  inherited;

  (* Create and initialize the attributes *)
  FKeywordAttri := TSynHighlighterAttributes.Create('keyword', 'keyword');
  AddAttribute(FKeywordAttri);
  FKeywordAttri.Style := [fsBold];
  FKeywordAttri.Foreground := clBlue;

  FIdentAttri := TSynHighlighterAttributes.Create('ident', 'ident');
  AddAttribute(FIdentAttri);
  FIdentAttri.Style := [];

  FNumberAttri := TSynHighlighterAttributes.Create('number', 'number');
  AddAttribute(FNumberAttri);
  FNumberAttri.Style := [];

  FPropertyAttri := TSynHighlighterAttributes.Create('property', 'property');
  AddAttribute(FPropertyAttri);
  FPropertyAttri.Style := [fsBold];

  FSpaceAttri := TSynHighlighterAttributes.Create('space', 'space');
  AddAttribute(FSpaceAttri);

  // Ensure the HL reacts to changes in the attributes. Do this once, if all attributes are created
  SetAttributesOnChange(@DefHighlightChange);
end;

(* Setters for attributes / This allows using in Object inspector*)
procedure TSynVCardHighlighter.SetKeywordAttri(AValue: TSynHighlighterAttributes);
begin
  FKeywordAttri.Assign(AValue);
end;

procedure TSynVCardHighlighter.SetIdentAttri(AValue: TSynHighlighterAttributes);
begin
  FIdentAttri.Assign(AValue);
end;

function TSynVCardHighlighter.IsWhiteSpace(C: Char): Boolean;
begin
  Result := C in [#9, ' '];
end;

function TSynVCardHighlighter.IsNumber(Text: string): Boolean;
var
  I: Integer;
begin
  if Length(Text) > 0 then begin
    Result := True;
    for I := 1 to Length(Text) do begin
      if not IsDigit(Text[I]) then begin
        Result := False;
        Break;
      end;
    end;
  end else Result := False;
end;

function TSynVCardHighlighter.IsProperty(Text: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(Properties) - 1 do
    if Properties[I] = Text then begin
      Result := True;
      Break;
    end;
end;

procedure TSynVCardHighlighter.SetNumberAttri(AValue: TSynHighlighterAttributes
  );
begin
  FNumberAttri.Assign(AValue);
end;

procedure TSynVCardHighlighter.SetPropertyAttri(
  AValue: TSynHighlighterAttributes);
begin
  FPropertyAttri.Assign(AValue);
end;

procedure TSynVCardHighlighter.SetSpaceAttri(AValue: TSynHighlighterAttributes);
begin
  FSpaceAttri.Assign(AValue);
end;

function TSynVCardHighlighter.IsDigit(C: Char): Boolean;
begin
  Result := C in ['0'..'9'];
end;

procedure TSynVCardHighlighter.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited;
  FLineText := NewValue;
  // Next will start at "FTokenEnd", so set this to 1
  FTokenEnd := 1;
  Next;
end;

procedure TSynVCardHighlighter.Next;
var
  L: Integer;
begin
  // FTokenEnd should be at the start of the next Token (which is the Token we want)
  FTokenPos := FTokenEnd;
  // assume empty, will only happen for EOL
  FTokenEnd := FTokenPos;

  // Scan forward
  // FTokenEnd will be set 1 after the last char. That is:
  // - The first char of the next token
  // - or past the end of line (which allows GetEOL to work)

  L := Length(FLineText);
  If FTokenPos > L then
    // At line end
    Exit
  else
  if FLineText[FTokenEnd] in [#9, ' '] then begin
    // At Space? Find end of spaces
    while (FTokenEnd <= L) and (FLineText[FTokenEnd] in [#0..#32]) do
      Inc(FTokenEnd)
  end else
  if FLineText[FTokenEnd] in [':', ',', ';'] then begin
    Inc(FTokenEnd);
  end else
  if IsDigit(FLineText[FTokenEnd]) then begin
    while (FTokenEnd <= L) and IsDigit(FLineText[FTokenEnd]) do
      Inc(FTokenEnd);
  end else begin
    // At None-Space? Find end of None-spaces
    while (FTokenEnd <= L) and not (FLineText[FTokenEnd] in [#9, ' ', ':', ',', ';']) do
      Inc(FTokenEnd);
  end;
end;

function TSynVCardHighlighter.GetEol: Boolean;
begin
  Result := FTokenPos > Length(FLineText);
end;

procedure TSynVCardHighlighter.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenStart := @FLineText[FTokenPos];
  TokenLength := FTokenEnd - FTokenPos;
end;

function TSynVCardHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
var
  Token: string;
begin
  Token := LowerCase(Copy(FLineText, FTokenPos, FTokenEnd - FTokenPos));

  // Match the text, specified by FTokenPos and FTokenEnd
  if IsWhiteSpace(FLineText[FTokenPos]) then
    Result := SpaceAttri
  else
  if IsNumber(Token) then
    Result := NumberAttri
  else
  if IsProperty(Token) then
    Result := PropertyAttri
  else
  if (Token = 'begin') or (Token = 'end') or (Token = 'vcard') then
    Result := KeywordAttri
  else
    Result := IdentAttri;
end;

function TSynVCardHighlighter.GetToken: String;
begin
  Result := Copy(FLineText, FTokenPos, FTokenEnd - FTokenPos);
end;

function TSynVCardHighlighter.GetTokenPos: Integer;
begin
  Result := FTokenPos - 1;
end;

function TSynVCardHighlighter.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  // Some default attributes
  case Index of
    SYN_ATTR_IDENTIFIER: Result := FIdentAttri;
    SYN_ATTR_KEYWORD: Result := FKeywordAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_NUMBER: Result := FNumberAttri;
    SYN_ATTR_SYMBOL: Result := FPropertyAttri;
    else Result := nil;
  end;
end;

function TSynVCardHighlighter.GetTokenKind: integer;
var
  Attr: TSynHighlighterAttributes;
begin
  // Map Attribute into Attribute unique number
  Attr := GetTokenAttribute;
  Result := 0;
  if Attr = FSpaceAttri then Result := 1
  else if Attr = FKeywordAttri then Result := 2
  else if Attr = FIdentAttri then Result := 3
  else if Attr = FNumberAttri then Result := 4
  else if Attr = FPropertyAttri then Result := 5;
end;

end.

