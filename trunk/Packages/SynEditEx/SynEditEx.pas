{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SynEditEx;

{$warn 5023 off : no warning about unused units}
interface

uses
  USynEditEx, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('USynEditEx', @USynEditEx.Register);
end;

initialization
  RegisterPackage('SynEditEx', @Register);
end.
