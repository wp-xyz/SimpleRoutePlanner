unit srpDatamodule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  { TGeneralDataModule }

  TGeneralDataModule = class(TDataModule)
    GeneralImages: TImageList;
  private

  public

  end;

var
  GeneralDataModule: TGeneralDataModule;

implementation

{$R *.lfm}

end.

