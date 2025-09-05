unit srpGlobals;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo,
  Graphics, Controls, StdCtrls,
  mvTypes;

const
  NO_POINT: TRealPoint = (Lon:999.0; Lat:999.0);
  DEFAULT_GPS_FORMAT = '0.000000';

procedure BoldGroupbox(AGroupbox: TCustomGroupbox);
function TextHeight(AFont: TFont; AFontStyle: TFontStyles; AText: String): Integer;


implementation

procedure BoldGroupbox(AGroupbox: TCustomGroupbox);
var
  i: Integer;
  propinfo: PPropInfo;
  cntrl: TControl;
begin
  for i:=0 to AGroupbox.ControlCount-1 do begin
    cntrl := AGroupbox.Controls[i];
    // Does the control have a ParentFont property?
    propinfo := GetPropInfo(cntrl, 'ParentFont');
    if propinfo <> nil then
      // If yes, turn it off .
      SetOrdProp(cntrl, propinfo, Longint(false));
  end;
  AGroupbox.Font.Style := [fsBold];
end;

function TextHeight(AFont: TFont; AFontStyle: TFontStyles; AText: String): Integer;
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    bmp.SetSize(10, 10);
    bmp.Canvas.Font.Assign(AFont);
    bmp.Canvas.Font.Style := AFontStyle;
    Result := bmp.Canvas.TextHeight(AText);
  finally
    bmp.Free;
  end;
end;

end.

