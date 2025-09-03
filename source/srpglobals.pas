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

end.

