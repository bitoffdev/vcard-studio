unit UTest;

{$mode Delphi}

interface

uses
  Classes, SysUtils, fgl;

type
  TTestResult = (trNone, trPassed, trFailed);

  { TTestCase }

  TTestCase = class
  public
    Name: string;
    Result: TTestResult;
    Log: string;
    procedure Run; virtual;
  end;

  TTestCaseClass = class of TTestCase;

  { TTestCases }

  TTestCases = class(TFPGObjectList<TTestCase>)
    function AddNew(Name: string; TestClass: TTestCaseClass): TTestCase;
  end;

  { TTestCaseLoadSave }

  TTestCaseLoadSave = class(TTestCase)
    Input: string;
    Output: string;
    procedure Run; override;
    procedure Evaluate(Passed: Boolean);
  end;

const
  ResultText: array[TTestResult] of string = ('None', 'Passed', 'Failed');


implementation

uses
  UContact;

{ TTestCaseLoadSave }

procedure TTestCaseLoadSave.Run;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    with TContactsFile.Create do
    try
      Lines.Text := Input;
      LoadFromStrings(Lines);
      Lines.Text := '';
      SaveToStrings(Lines);
      Evaluate(Lines.Text = Output);
      if Result <> trPassed then begin
        Log := 'Expected:' + LineEnding +
          '"' + Output + '"' + LineEnding + LineEnding +
          'Output:' + LineEnding +
          '"' + Lines.Text + '"';
      end;
    finally
      Free;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TTestCaseLoadSave.Evaluate(Passed: Boolean);
begin
  if Passed then Result := trPassed
    else Result := trFailed;
end;

{ TTestCase }

procedure TTestCase.Run;
begin

end;

{ TTestCases }

function TTestCases.AddNew(Name: string; TestClass: TTestCaseClass): TTestCase;
begin
  Result := TestClass.Create;
  Result.Name := Name;
  Add(Result);
end;

end.

