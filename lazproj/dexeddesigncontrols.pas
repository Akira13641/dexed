{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DexedDesignControls;

{$warn 5023 off : no warning about unused units}
interface

uses
  u_dsgncontrols, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('u_dsgncontrols', @u_dsgncontrols.Register);
end;

initialization
  RegisterPackage('DexedDesignControls', @Register);
end.
