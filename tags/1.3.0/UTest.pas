unit UTest;

{$mode Delphi}

interface

uses
  Classes, SysUtils, fgl, UContact;

type
  TTestResult = (trNone, trPassed, trFailed);

  { TTestCase }

  TTestCase = class
  public
    Name: string;
    Result: TTestResult;
    Log: string;
    procedure Run; virtual;
    procedure Evaluate(Passed: Boolean);
    procedure Pass;
    procedure Fail;
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
  end;

  { TTestCaseCheckProperty }

  TTestCaseCheckProperty = class(TTestCase)
    Input: string;
    ContactIndex: Integer;
    Index: TContactFieldIndex;
    Value: string;
    procedure Run; override;
  end;

const
  ResultText: array[TTestResult] of string = ('None', 'Passed', 'Failed');


implementation

{ TTestCaseCheckProperty }

procedure TTestCaseCheckProperty.Run;
var
  Lines: TStringList;
  PropertyValue: string;
begin
  Lines := TStringList.Create;
  try
    with TContactsFile.Create do
    try
      Lines.Text := Input;
      LoadFromStrings(Lines);
      if ContactIndex < Contacts.Count then begin
        PropertyValue := Contacts[ContactIndex].Fields[Index];
        Evaluate(PropertyValue = Value);
      end else Fail;
      Log := 'Expected:' + LineEnding +
        '"' + Value + '"' + LineEnding + LineEnding +
        'Output:' + LineEnding +
        '"' + PropertyValue + '"';
    finally
      Free;
    end;
  finally
    Lines.Free;
  end;
end;

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
      Log := 'Expected:' + LineEnding +
        '"' + Output + '"' + LineEnding + LineEnding +
        'Output:' + LineEnding +
        '"' + Lines.Text + '"';
    finally
      Free;
    end;
  finally
    Lines.Free;
  end;
end;

{ TTestCase }

procedure TTestCase.Run;
begin
end;

procedure TTestCase.Evaluate(Passed: Boolean);
begin
  if Passed then Result := trPassed
    else Result := trFailed;
end;

procedure TTestCase.Pass;
begin
  Result := trPassed;
end;

procedure TTestCase.Fail;
begin
  Result := trFailed;
end;

{ TTestCases }

function TTestCases.AddNew(Name: string; TestClass: TTestCaseClass): TTestCase;
begin
  Result := TestClass.Create;
  Result.Name := Name;
  Add(Result);
end;

end.

