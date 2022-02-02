unit UTest;

{$mode Delphi}

interface

uses
  Classes, SysUtils, fgl;

type
  TTestResult = (trNone, trPassed, trFailed);

  { TTestCase }

  TTestCase = class
    Name: string;
    Result: TTestResult;
    procedure Run;
  end;

  { TTestCases }

  TTestCases = class(TFPGObjectList<TTestCase>)
    function AddNew(Name: string): TTestCase;
  end;

implementation

{ TTestCase }

procedure TTestCase.Run;
begin

end;

{ TTestCases }

function TTestCases.AddNew(Name: string): TTestCase;
begin
  Result := TTestCase.Create;
  Result.Name := Name;
  Add(Result);
end;

end.

