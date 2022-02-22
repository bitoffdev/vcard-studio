unit UFormCompare;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  UContact, Diff, LCLType, LCLIntf, ComCtrls, Buttons, Menus, ActnList, SynEdit,
  SynEditMiscClasses, SynHighlighterPosition,
  SynEditHighlighter, UCommon, USynEditEx;

type

  { TFormCompare }

  TFormCompare = class(TForm)
    ASwitchSides: TAction;
    AReloadFiles: TAction;
    AFileOpenLeft: TAction;
    AFileOpenRight: TAction;
    ActionList1: TActionList;
    EditLeftFileName: TEdit;
    EditRightFileName: TEdit;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItemClose: TMenuItem;
    OpenDialogSide: TOpenDialog;
    PanelLeft: TPanel;
    PanelRight: TPanel;
    SpeedButtonOpenLeft: TSpeedButton;
    SpeedButtonOpenRight: TSpeedButton;
    Splitter1: TSplitter;
    SynEditLeft: TSynEditEx;
    SynEditRight: TSynEditEx;
    procedure AFileOpenLeftExecute(Sender: TObject);
    procedure AFileOpenRightExecute(Sender: TObject);
    procedure AReloadFilesExecute(Sender: TObject);
    procedure ASwitchSidesExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItemCloseClick(Sender: TObject);
    procedure SynEditLeftChange(Sender: TObject);
    procedure SynEditLeftScroll(Sender: TObject);
    procedure SynEditRightChange(Sender: TObject);
    procedure SynEditRightScroll(Sender: TObject);
  private
    FLeftSide: string;
    FRightSide: string;
    Diff: TDiff;
    HighlighterLeft: TSynPositionHighlighter;
    HighlighterRight: TSynPositionHighlighter;
    AttrAdded: TtkTokenKind;
    AttrDeleted: TtkTokenKind;
    AttrModified: TtkTokenKind;
    LastWidth: Integer;
    procedure SetLeftSide(AValue: string);
    procedure SetRightSide(AValue: string);
    procedure ReloadContent;
    procedure UpdateInterface;
    procedure UpdateHighlight;
    function LoadFile(AFileName: string): string;
  public
    procedure LoadFileLeft(FileName: string);
    procedure LoadFileRight(FileName: string);
    property LeftSide: string read FLeftSide write SetLeftSide;
    property RightSide: string read FRightSide write SetRightSide;
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

procedure TFormCompare.ASwitchSidesExecute(Sender: TObject);
var
  TempFileName: string;
  TempContent: string;
begin
  TempContent := SynEditLeft.Text;
  SynEditLeft.Text := SynEditRight.Text;
  SynEditRight.Text := TempContent;

  TempFileName := EditLeftFileName.Text;
  EditLeftFileName.Text := EditRightFileName.Text;
  EditRightFileName.Text := TempFileName;

  UpdateInterface;
  UpdateHighlight;
end;

procedure TFormCompare.FormActivate(Sender: TObject);
begin
  if LastWidth = -1 then LastWidth := Width;
end;

procedure TFormCompare.AReloadFilesExecute(Sender: TObject);
begin
  LoadFileLeft(EditLeftFileName.Text);
  LoadFileRight(EditRightFileName.Text);
  UpdateHighlight;
  UpdateInterface;
end;

procedure TFormCompare.AFileOpenLeftExecute(Sender: TObject);
begin
  OpenDialogSide.InitialDir := ExtractFileDir(EditLeftFileName.Text);
  OpenDialogSide.FileName := ExtractFileName(EditLeftFileName.Text);
  if OpenDialogSide.Execute then begin
    EditLeftFileName.Text := OpenDialogSide.FileName;
    SynEditLeft.Text := LoadFileToStr(OpenDialogSide.FileName);
  end;
end;

procedure TFormCompare.AFileOpenRightExecute(Sender: TObject);
begin
  OpenDialogSide.InitialDir := ExtractFileDir(EditRightFileName.Text);
  OpenDialogSide.FileName := ExtractFileName(EditRightFileName.Text);
  if OpenDialogSide.Execute then begin
    EditRightFileName.Text := OpenDialogSide.FileName;
    SynEditRight.Text := LoadFileToStr(OpenDialogSide.FileName);
  end;
  UpdateHighlight;
end;

procedure TFormCompare.FormCreate(Sender: TObject);
begin
  Core.Translator.TranslateComponentRecursive(Self);
  Core.ThemeManager1.UseTheme(Self);
  Diff := TDiff.Create(Self);

  HighlighterLeft := TSynPositionHighlighter.Create(Self);
  with HighlighterLeft do begin
    AttrAdded := CreateTokenID('Added', clNone, clLightGreen, []);
    AttrDeleted := CreateTokenID('Deleted', clNone, clLightBlue, []);
    AttrModified := CreateTokenID('Modified', clNone, clLightRed, []);
  end;
  SynEditLeft.Highlighter := HighlighterLeft;

  HighlighterRight := TSynPositionHighlighter.Create(Self);
  with HighlighterRight do begin
    AttrAdded := CreateTokenID('Added', clNone, clLightGreen, []);
    AttrDeleted := CreateTokenID('Deleted', clNone, clLightBlue, []);
    AttrModified := CreateTokenID('Modified', clNone, clLightRed, []);
  end;
  SynEditRight.Highlighter := HighlighterRight;

  LastWidth := -1;
end;

procedure TFormCompare.FormDestroy(Sender: TObject);
begin
  FreeAndNil(HighlighterLeft);
  FreeAndNil(HighlighterRight);
  FreeAndNil(Diff);
end;

procedure TFormCompare.FormResize(Sender: TObject);
var
  LastHandler: TNotifyEvent;
  NewPanelWidth: Integer;
const
  MaxRatio = 0.8;
begin
  if LastWidth <> -1 then begin
    LastHandler := PanelLeft.OnResize;
    try
      PanelLeft.OnResize := nil;
      NewPanelWidth := Round((PanelLeft.Width / LastWidth) * Width);
      if NewPanelWidth > Round(Width * MaxRatio) then NewPanelWidth := Round(Width * MaxRatio);
      PanelLeft.Width := NewPanelWidth;
    finally
      PanelLeft.OnResize := LastHandler;
    end;
    LastWidth := Width;
  end;
end;

procedure TFormCompare.FormShow(Sender: TObject);
begin
  Core.PersistentForm1.Load(Self);
  UpdateInterface;
  ReloadContent;
end;

procedure TFormCompare.MenuItemCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormCompare.SynEditLeftChange(Sender: TObject);
begin
  UpdateHighlight;
end;

procedure TFormCompare.SynEditLeftScroll(Sender: TObject);
begin
  SynEditRight.TopLine := SynEditLeft.TopLine;
end;

procedure TFormCompare.SynEditRightChange(Sender: TObject);
begin
  UpdateHighlight;
end;

procedure TFormCompare.SynEditRightScroll(Sender: TObject);
begin
  SynEditLeft.TopLine := SynEditRight.TopLine;
end;

procedure TFormCompare.SetLeftSide(AValue: string);
begin
  if FLeftSide = AValue then Exit;
  FLeftSide := AValue;
end;

procedure TFormCompare.SetRightSide(AValue: string);
begin
  if FRightSide = AValue then Exit;
  FRightSide := AValue;
end;

procedure TFormCompare.ReloadContent;
begin
  UpdateHighlight;
end;

procedure TFormCompare.UpdateInterface;
begin
end;

procedure TFormCompare.UpdateHighlight;
var
  LeftText: string;
  RightText: string;
  I: Integer;
  LastKind: TChangeKind;
  P1: TPoint;
  P2: TPoint;
  Rec: TCompareRec;
  NextToken1: TtkTokenKind;
  NextToken2: TtkTokenKind;
begin
  LeftText := SynEditLeft.Lines.Text;
  RightText := SynEditRight.Lines.Text;

  Diff.Execute(PChar(LeftText), PChar(RightText), Length(LeftText), Length(RightText));

  HighlighterLeft.ClearAllTokens;
  HighlighterRight.ClearAllTokens;
  LeftText := '';
  RightText := '';
  LastKind := ckNone;
  P1 := Point(1, 0);
  P2 := Point(1, 0);
  NextToken1 := tkText;
  NextToken2 := tkText;
  for I := 0 to Diff.Count - 1 do
    with Diff.Compares[I] do begin
      Rec := Diff.Compares[I];
      if Rec.Chr1 = LineEnding then begin
        if NextToken1 <> tkText then begin
          HighlighterLeft.AddToken(P1.Y, 0, NextToken1);
          NextToken1 := tkText;
        end;
        Inc(P1.Y);
        P1.X := 0;
        LeftText := LeftText + Rec.Chr1;
      end else begin
        if Kind = ckAdd then LeftText := LeftText + ' '
          else LeftText := LeftText + Rec.chr1;
        if Kind <> LastKind then begin
          HighlighterLeft.AddToken(P1.Y, P1.X, NextToken1);
          if Kind = ckNone then NextToken1 := tkText
          //else if Kind = ckAdd then NextToken1 := AttrAdded
          else if Kind = ckDelete then NextToken1 := AttrDeleted
          else if Kind = ckModify then NextToken1 := AttrModified;
        end;
        Inc(P1.X);
      end;

      if Rec.Chr2 = LineEnding then begin
        if NextToken2 <> tkText then begin
          HighlighterRight.AddToken(P2.Y, 0, NextToken2);
          NextToken2 := tkText;
        end;
        Inc(P2.Y);
        P2.X := 0;
        RightText := RightText + Rec.Chr2;
      end else begin
        if Kind = ckDelete then RightText := RightText + ' '
          else RightText := RightText + Rec.Chr2;
        if Kind <> LastKind then begin
          HighlighterRight.AddToken(P2.Y, P2.X, NextToken2);
          if Kind = ckNone then NextToken2 := tkText
          else if Kind = ckAdd then NextToken2 := AttrAdded
          //else if Kind = ckDelete then NextToken2 := AttrDeleted
          else if Kind = ckModify then NextToken2 := AttrModified;
        end;
        Inc(P2.X);
      end;

      LastKind := Kind;
    end;

  //SynEditLeft.Lines.Text := LeftText;
  //SynEditRight.Lines.Text := RightText;
end;

function TFormCompare.LoadFile(AFileName: string): string;
var
  Ext: string;
begin
  Ext := ExtractFileExt(AFileName);
  if Ext = VCardFileExt then begin
    with TContactsFile.Create do
    try
      LoadFromFile(AFileName);
      Result := AsString;
    finally
      Free;
    end;
  end else Result := LoadFileToStr(AFileName);
end;

procedure TFormCompare.LoadFileLeft(FileName: string);
begin
  EditLeftFileName.Text := FileName;
  LeftSide := LoadFile(FileName);
  SynEditLeft.Text := LeftSide;
end;

procedure TFormCompare.LoadFileRight(FileName: string);
begin
  EditRightFileName.Text := FileName;
  RightSide := LoadFile(FileName);
  SynEditRight.Text := RightSide;
end;

end.

