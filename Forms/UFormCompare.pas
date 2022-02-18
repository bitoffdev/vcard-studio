unit UFormCompare;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  UContact, Diff, LCLType, LCLIntf, ComCtrls, SynEdit;

type

  { TFormCompare }

  TFormCompare = class(TForm)
    EditLeftFileName: TEdit;
    EditRightFileName: TEdit;
    PanelLeft: TPanel;
    PanelRight: TPanel;
    Splitter1: TSplitter;
    SynEdit1: TSynEdit;
    SynEdit2: TSynEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBoxLeftPaint(Sender: TObject);
  private
    FLeftSide: TContactsFile;
    FRightSide: TContactsFile;
    S1, S2: string;
    Diff: TDiff;
    procedure SetLeftSide(AValue: TContactsFile);
    procedure SetRightSide(AValue: TContactsFile);
    procedure ReloadContent;
    procedure UpdateInterface;
  public
    property LeftSide: TContactsFile read FLeftSide write SetLeftSide;
    property RightSide: TContactsFile read FRightSide write SetRightSide;
  end;

var
  FormCompare: TFormCompare;

implementation

{$R *.lfm}

uses
  UCore;

{ TFormCompare }

procedure TFormCompare.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  Core.PersistentForm1.Save(Self);
end;

procedure TFormCompare.FormCreate(Sender: TObject);
begin
  Core.Translator.TranslateComponentRecursive(Self);
  Core.ThemeManager1.UseTheme(Self);
  Diff := TDiff.Create(Self);
end;

procedure TFormCompare.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Diff);
end;

procedure TFormCompare.FormShow(Sender: TObject);
begin
  Core.PersistentForm1.Load(Self);
  UpdateInterface;
  ReloadContent;
end;

procedure MarkupTextOut(Canvas: TCanvas; X, Y: Integer; Text: string);
var
  I: Integer;
  Len: Integer;
  Clr: Integer;
  SavedTextAlign: Cardinal;
  SavedBkColor: Cardinal;
  SavedTextColor: Cardinal;
  SavedPt: TPoint;
begin
  I := Pos('<', Text);
  if I = 0 then begin
    Canvas.TextOut(X, Y, Text);
    Exit;
  end;

  SavedTextColor := GetTextColor(Canvas.Handle);
  SavedBkColor := GetBkColor(Canvas.Handle);
  //SavedTextAlign := GetTextAlign(Canvas.Handle);
  //SetTextAlign(Canvas.Handle, SavedTextAlign or TA_UPDATECP);
  MoveToEx(Canvas.Handle, X, Y, @SavedPt);

  repeat
    if I > 1 then TextOut(Canvas.Handle, 0, 0, PChar(Text), I - 1);
    Delete(Text, 1, I);
    Len := Length(Text);
    if Len < 3 then Break
    else if (Text[1] = 'F') and (Text[2] = 'C') and (Text[3] = ':') and
      (Len > 9) and (Text[10] = '>') then begin
      Clr := StrToIntDef('$' + Copy(Text, 4, 6), 0);
      SetTextColor(Canvas.Handle, Clr);
      Delete(Text, 1, 10);
      Dec(Len, 10);
    end
    else if (Text[1] = 'B') and (Text[2] = 'C') and (Text[3] = ':') and
      (Len > 9) and (Text[10] = '>') then
    begin
      Clr := StrToIntDef('$' + Copy(Text, 4, 6), $1FFFFFF);
      if Clr > $FFFFFF then
        SetBkColor(Canvas.Handle, SavedBkColor) else
        SetBkColor(Canvas.Handle, Clr);
      Delete(Text, 1, 10);
      Dec(Len, 10);
    end
    else Break;
    I := Pos('<', Text);
  until (I = 0);
  TextOut(Canvas.Handle, 0, 0, PChar(Text), Len);

  SetTextColor(Canvas.Handle, SavedTextColor);
  SetBkColor(Canvas.Handle, SavedBkColor);
  //SetTextAlign(Canvas.Handle, SavedTextAlign);
  with SavedPt do MoveToEx(Canvas.Handle, X, Y, nil);
end;

procedure TFormCompare.PaintBoxLeftPaint(Sender: TObject);
begin
  with TPaintBox(Sender) do begin
    MarkupTextOut(Canvas, 0, 5, S1);
    MarkupTextOut(Canvas, 0, 25, S2);
    Canvas.TextOut(0, 55, 'Compare Statistics ...');
    with Diff.DiffStats do begin
      MarkupTextOut(Canvas, 0, 75, '  Matches : ' + IntToStr(Matches));
      MarkupTextOut(Canvas, 0, 95, '  <BC:AAFFAA>Modifies:<BC:------> ' + IntToStr(Modifies));
      MarkupTextOut(Canvas, 0, 115, '  <BC:FFAAAA>Adds    :<BC:------> ' + IntToStr(Adds));
      MarkupTextOut(Canvas, 0, 135, '  <BC:AAAAFF>Deletes :<BC:------> ' + IntToStr(Deletes));
    end;
  end;
end;

procedure TFormCompare.SetLeftSide(AValue: TContactsFile);
begin
  if FLeftSide = AValue then Exit;
  FLeftSide := AValue;
  ReloadContent;
  UpdateInterface;
end;

procedure TFormCompare.SetRightSide(AValue: TContactsFile);
begin
  if FRightSide = AValue then Exit;
  FRightSide := AValue;
  ReloadContent;
  UpdateInterface;
end;

procedure TFormCompare.ReloadContent;
var
  LeftText: string;
  RightText: string;
  I: Integer;
  LastKind: TChangeKind;

  //AddCharToStr() adds color markup to strings which will be parsed later by
  //my MarkupTextOut() function where diffs (additions, modifications and
  //deletions) will be displayed in Paintbox1 with different colors ...
  //<BC:------> change background color to original (transparent) color
  //<BC:AAFFAA> change background color to pale green
  //<BC:AAAAFF> change background color to pale red
  //<BC:FFAAAA> change background color to pale blue
  procedure AddCharToStr(var s: string; c: char; Kind, LastKind: TChangeKind);
  begin
    if (Kind = LastKind) then
      s := s + c //no need to change colors
    else
    case Kind of
      ckNone: s := s + '<BC:------>' + c;
      ckAdd: s := s + '<BC:FFAAAA>' + c;
      ckDelete: s := s + '<BC:AAAAFF>' + c;
      ckModify: s := s + '<BC:AAFFAA>' + c;
    end;
  end;

begin
  if Assigned(FLeftSide) then begin
    FLeftSide.Sort;
    LeftText := FLeftSide.AsString;
  end else LeftText := '';
  if Assigned(FRightSide) then begin
    FRightSide.Sort;
    RightText := FRightSide.AsString;
  end else RightText := '';

  Diff.Execute(PChar(LeftText), PChar(RightText), Length(LeftText), Length(RightText));

  //now, display the diffs ...
  LastKind := ckNone;
  S1 := '';
  S2 := '';
  for I := 0 to Diff.Count - 1 do
    with Diff.Compares[I] do begin
      //show changes to first string (with spaces for adds to align with second string)
      if Kind = ckAdd then AddCharToStr(S1, ' ', Kind, LastKind)
      else AddCharToStr(S1, Chr1, Kind, LastKind);

      //show changes to second string (with spaces for deletes to align with first string)
      if Kind = ckDelete then AddCharToStr(S2, ' ', Kind, LastKind)
      else AddCharToStr(S2, Chr2, Kind, LastKind);

      LastKind := Kind;
    end;
end;

procedure TFormCompare.UpdateInterface;
begin
  if Assigned(FLeftSide) then EditLeftFileName.Text := FLeftSide.FileName
    else EditLeftFileName.Text := '';
  if Assigned(FRightSide) then EditRightFileName.Text := FRightSide.FileName
    else EditRightFileName.Text := '';
end;

end.

