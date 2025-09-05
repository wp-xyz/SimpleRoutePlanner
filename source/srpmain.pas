unit srpMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, IniFiles, fgl,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, SynEdit,
  LCLIntf, LCLType, Buttons, Types,
  mvMapViewer, mvTypes, mvEngine, mvPluginCommon, mvMarkerPlugins, mvPlugins, mvGpsObj,
  fpHttpClient, fpJSON,
  srpGlobals, srpAPIKey, srpSettings;

const
  // ImageIndices in ImageList32
  IMGINDEX_START = 0;
  IMGINDEX_VIA = 0;
  IMGINDEX_END = 4;    // Flag
  IMGINDEX_CAR = 1;
  IMGINDEX_PEDESTRIAN = 2;

  DEFAULT_POLYLINE_PRECISION = 6;

var
  LengthUnits: String = 'kilometers';  // or 'miles'

type
  TManeuverType = (
    mtNone,                                                       // 0
    mtStart, mtStartRight, mtStartLeft,                           // 1..3
    mtDestination, mtDestinationRight, mtDestinationLeft,         // 4..6
    mtBecomes, mtContinue, mtSlighRight, mtRight, mtSharpRight,   // 7..11
    mtUTurnRight, mtUTurnLeft, mtSharpLeft, mtLeft, mtSlightLeft, // 12..16
    mtRampStraight, mtRampRight, mtRampLeft,                      // 17..19
    mtExitRight, mtExitLeft,                                      // 20..21
    mtStayStraight, mtStayRight, mtStayLeft,                      // 22..24
    mtMerge, mtEnterRoundabout, mtExitRoundabout,                 // 25..27
    mtEnterFerry, mtExitFerry,                                    // 28..29
    mtTransit, mtTransitTransfer, mtTransitRemainOn,              // 30..32
    mtTransitConnectionStart, mtTransitConnectionTransfer,        // 33..34
    mtTransitConnectionDestination, mtPostTransitConnectionDestination,  // 35..36
    mtMergeRight, mtMergeLeft                                     // 37, 38
  );

  TRouteManeuver = class
    ManeuverType: TManeuverType;
    Instruction: String;
//    Instruction_Before: String;
//    Instruction_After: String;
    Length: Double;
    Time: Double;
    AccumLength: Double;
    AccumTime: Double;
    ShapeIndex_Begin: Integer;
    ShapeIndex_End: Integer;
  end;

  TRouteManeuvers = specialize TFPGObjectList <TRouteManeuver>;

  TRoute = class
  private
    FManeuvers: TRouteManeuvers;
    FPolyLine: TRealPointArray;
  protected
    function ExtractManeuvers(AJson: TJsonData): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function JsonGetSummary(AJson: TJsonData; out ATotalLength, ATotalTime: Double): Boolean;
    procedure JsonToPolyLine(AJson: TJsonData);
    procedure JsonToStrings(AJson: TJsonData; AList: TStrings);
    property Maneuvers: TRouteManeuvers read FManeuvers;
    property PolyLine: TRealPointArray read FPolyLine;
  end;


  { TMainForm }

  TMainForm = class(TForm)
    ImageList24: TImageList;
    lblTotalLength: TLabel;
    lblTotalTime: TLabel;
    infoTotalLength: TLabel;
    infoTotalTime: TLabel;
    lbManeuvers: TListBox;
    lbLocations: TListBox;
    LocationsPanel: TPanel;
    ManeuverPanel: TPanel;
    tbClearLocations: TToolButton;
    tbVehicleAuto: TToolButton;
    tbVehiclePedestrian: TToolButton;
    tbSettings: TToolButton;
    ToolButton2: TToolButton;
    VehicleToolBar: TToolBar;
    ToolButton1: TToolButton;
    TotalInfoPanel: TPanel;
    LocationsToolbar: TToolBar;
    tbAddLocation: TToolButton;
    tbDeleteLocation: TToolButton;
    tbPickLocation: TToolButton;
    Splitter1: TSplitter;
    btnSave: TButton;
    btnLoad: TButton;
    ImageList32: TImageList;
    MapView: TMapView;
    MvPluginManager: TMvPluginManager;
    LegalNoticePlugin: TLegalNoticePlugin;
    MarkerEditorPlugin: TMarkerEditorPlugin;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    Panel2: TPanel;
    SaveDialog: TSaveDialog;
    SynEdit1: TSynEdit;
    pgJSON: TTabSheet;
    pgRoute: TTabSheet;
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbLocationsClick(Sender: TObject);
    procedure lbLocationsDblClick(Sender: TObject);
    procedure lbLocationsDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure lbLocationsMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, Y: Integer);
    procedure lbManeuversClick(Sender: TObject);
    procedure lbManeuversDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure lbManeuversMeasureItem(Control: TWinControl; Index: Integer;
      var AHeight: Integer);
    procedure MapViewMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure MapViewMouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure MarkerEditorPluginCanClick({%H-}AMapView: TMapView; APoint: TGPSPoint;
      var CanClick: Boolean);
    procedure MarkerEditorPluginEndDrag(Sender: TObject);
    procedure MarkerEditorPluginSelectionChange(Sender: TObject);
    procedure tbAddLocationClick(Sender: TObject);
    procedure tbClearLocationsClick(Sender: TObject);
    procedure tbDeleteLocationClick(Sender: TObject);
    procedure tbPickLocationClick(Sender: TObject);
    procedure tbSettingsClick(Sender: TObject);
    procedure tbVehicleAutoClick(Sender: TObject);
    procedure tbVehiclePedestrianClick(Sender: TObject);
  private
    FApiKey: String;
    FMapProvider: String;
    FRoute: TRoute;
    FLanguage: String;
    FActivated: Boolean;
    procedure AddRouteToMap(ALine: TRealPointArray);
    function GetCostingAsString: String;
    function GetLayer: TMapLayer;
    function GetLocationPOI(AIndex: Integer): TMapPointOfInterest;
    function GetLocationPoints: TRealPointArray;
    function GetManeuverMarkerLayer: TMapLayer;
    procedure GetRoute;
    procedure InitLocations;
    procedure NewLocation(AIndex: Integer; ACaption: String; AImageIndex: Integer);
    procedure RequestApiKey;
    procedure ShowRoute(AJson: TJSONData);
    procedure ShowManeuverMarker(AManeuver: TRouteManeuver);
    procedure UpdateCmds;
    procedure UpdateViaCaptions;

    function CalcIniFileName: String;
    procedure ReadIni;
    procedure WriteIni;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  opensslsockets;

// https://github.com/mapbox/polyline/blob/master/src/polyline.js
function DecodePolyLine(AStr: String; APrecision: Integer): TRealPointArray;
const
  BLOCK_SIZE = 1024;
var
  index: Integer = 1;
  lat: Double = 0.0;
  lon: Double = 0.0;
  shift: Integer = 0;
  res: Integer = 0;
  b: Byte;
  lat_change, lon_change: Double;
  factor: Double;
  n: Integer = 0;
begin
  Result := nil;

  SetLength(Result, BLOCK_SIZE);
  factor := IntPower(10.0, APrecision);

  while index < Length(AStr) do
  begin
    // Latitude
    shift := 1;
    res := 0;
    repeat
      b := ord(AStr[index]) - 63;
      inc(index);
      res := res + (b and $1F) * shift;
      shift := shift * 32;
    until (b < $20);
    if (res and $01) <> 0 then
      lat_change := (-res - 1) div 2
    else
      lat_change := res div 2;

    // Longitude
    shift := 1;
    res := 0;
    repeat
      b := ord(AStr[index])  - 63;
      inc(index);
      res := res + (b and $1F) * shift;
      shift := shift * 32;
    until (b < $20);
    if (res and $01 <> 0) then
      lon_change := (-res - 1) div 2
    else
      lon_change := res div 2;

    lat := lat + lat_change;
    lon := lon + lon_change;
    Result[n].Lat := lat / factor;
    Result[n].Lon := lon / factor;
    inc(n);
    if n = Length(Result) then
      SetLength(Result, Length(Result) + BLOCK_SIZE);
  end;

  SetLength(result, n);
end;

function LengthAsString(ALength: Double): String;
begin
  if ALength = 0 then
    Result := ''
  else
    case LengthUnits of
      'kilometers':
        if ALength < 1 then
          Result := FormatFloat('0', ALength*1000) + ' m'
        else
          Result := FormatFloat('0.0', ALength) + ' km';
      'miles':
        if ALength < 1 then
          Result := FormatFloat('0', ALength * 1760) + ' yd'
        else
          Result := FormatFloat('0.0', ALength) + ' mi';
    end;
end;

function TimeAsString(ATime: Double): String;
begin
  if ATime < 60 then
    Result := FormatFloat('0', ATime) + ' sec'
  else
  if ATime < 3600 then
    Result := FormatFloat('0', ATime/60) + ' min'
  else
    Result := FormatDateTime('h:nn', ATime/(24*60*60)) + ' hours';
end;


{ TRoute }

constructor TRoute.Create;
begin
  inherited;
  FManeuvers := TRouteManeuvers.Create;
end;

destructor TRoute.Destroy;
begin
  FManeuvers.Free;
  inherited;
end;

procedure TRoute.Clear;
begin
  FManeuvers.Clear;
  SetLength(FPolyLine, 0);
end;

function TRoute.ExtractManeuvers(AJson: TJsonData): Boolean;
(*
  "maneuvers" : [
    {
      "type" : 3,
      "instruction" : "--- some text ---",
      "verbal_succinct_transition_instruction" : "--- some text ---",
      "verbal_pre_transition_instruction" : "--- some text ---",
      "verbal_post_transition_instruction" : "--- some text ",
      "street_names" : [
        "--- some street name ---"
      ],
      "time" : 3.6356000000000002E+001,
      "length" : 4.8699999999999999E-001,
      "cost" : 6.4665999999999997E+001,
      "begin_shape_index" : 0,
      "end_shape_index" : 20,
      "verbal_multi_cue" : true,
      "travel_mode" : "drive",
      "travel_type" : "car"
    },
    ...
*)
var
  i: Integer;
  maneuver: TRouteManeuver;
  prevManeuver: TRouteManeuver;
  jLegs: TJSONArray;
  jManeuvers: TJSONArray;
  jManeuver: TJSONObject;
begin
  Result := false;

  jLegs := AJson.FindPath('trip.legs') as TJSONArray;
  if jLegs <> nil then
  begin
    jManeuvers := jLegs[0].FindPath('maneuvers') as TJSONArray;
    if jManeuvers = nil then
      exit;
    prevManeuver := nil;
    for i := 0 to jManeuvers.Count-1 do
    begin
      jManeuver := jManeuvers[i] as TJSONObject;
      maneuver := TRouteManeuver.Create;
      maneuver.ManeuverType := TManeuverType(jManeuver.Integers['type']);
      maneuver.Instruction := jManeuver.Strings['instruction'];
      maneuver.Time := jManeuver.Floats['time'];
      maneuver.Length := jManeuver.Floats['length'];
      if i > 0 then begin
        maneuver.AccumTime := prevManeuver.AccumTime + maneuver.Time;
        maneuver.AccumLength := prevManeuver.AccumLength + maneuver.Length;
      end;
      maneuver.ShapeIndex_Begin := jManeuver.Integers['begin_shape_index'];
      maneuver.ShapeIndex_End := jManeuver.Integers['end_shape_index'];
      prevManeuver := maneuver;
      FManeuvers.Add(maneuver);
    end;
    Result := True;
  end;
end;

function TRoute.JsonGetSummary(AJson: TJsonData; out ATotalLength, ATotalTime: Double): Boolean;
var
  json: TJsonData;
  jSummary: TJsonObject;
begin
  Result := false;

  json := AJson.FindPath('trip.legs');
  if json <> nil then
  begin
    jSummary := TJsonArray(json)[0].FindPath('summary') as TJsonObject;
    if jSummary <> nil then
    begin
      ATotalLength := jSummary.Floats['length'];
      ATotalTime := jSummary.Floats['time'];
      Result := true;
    end;
  end;
end;

procedure TRoute.JsonToPolyLine(AJson: TJsonData);
var
  json: TJSONData;
begin
  FPolyLine := nil;
  if AJson = nil then
    exit;
  json := AJson.FindPath('trip.legs');
  if json <> nil then
  begin
    json := TJSONArray(json)[0].FindPath('shape');
    if json <> nil then
      FPolyLine := DecodePolyline(json.AsString, DEFAULT_POLYLINE_PRECISION);
  end;
end;

procedure TRoute.JsonToStrings(AJson: TJsonData; AList: TStrings);
var
  i: Integer;
  rm: TRouteManeuver;
begin
  AList.BeginUpdate;
  try
    AList.Clear;
    if not ExtractManeuvers(AJson) then
      exit;
    for i := 0 to FManeuvers.Count-1 do
    begin
      rm := FManeuvers[i];
      if rm.Length = 0 then
        AList.AddObject(rm.Instruction, rm)
      else
        AList.AddObject(Format('%s'+LineEnding+'%s (%s)', [
          rm.Instruction, LengthAsString(rm.Length), TimeAsString(rm.Time)
        ]), rm);
    end;
  finally
    AList.EndUpdate;
  end;
end;


{ TMainForm }

procedure TMainForm.AddRouteToMap(ALine: TRealPointArray);
var
  i: Integer;
  layer: TMapLayer;
  track: TMapTrack;
  point: TMapTrackPoint;
begin
  layer := GetLayer;
  layer.Tracks.Clear;
  if Length(ALine) = 0 then
    exit;

  track := layer.Tracks.Add as TMapTrack;
  for i:= 0 to High(ALine) do
  begin
    point := track.Points.Add as TMapTrackPoint;
    point.RealPoint := ALine[i];
  end;
end;

procedure TMainForm.btnLoadClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    SynEdit1.Lines.LoadFromFile(OpenDialog.Filename);
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    SynEdit1.Lines.SaveToFile(SaveDialog.FileName);
end;

function TMainForm.CalcIniFileName: String;
begin
  Result := ChangeFileExt(Application.ExeName, '.ini');
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CanClose then
    try
      WriteIni;
    except
    end;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    FActivated := true;
    lbLocations.ItemHeight :=
      TextHeight(lbLocations.Font, [fsBold], 'Tg') +
      TextHeight(lbLocations.Font, [], 'Tg')*2 +
      Scale96ToFont(4);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  infoTotalLength.Caption := '-';
  infoTotalTime.Caption := '-';
  SynEdit1.Lines.Clear;

  lbLocations.ItemHeight :=
    TextHeight(lbLocations.Font, [fsBold], 'Tg') +
    TextHeight(lbLocations.Font, [], 'Tg')*2 +
    Scale96ToFont(4);

  InitLocations;

  ReadIni;
  if FApiKey = '' then
    RequestApiKey;
  if FApiKey = '' then
  begin
    MessageDlg(
      'This application cannot run without an API key. ' + LineEnding +
      'Register at https://client.stadiamaps.com/accounts/login to get an API Key. ' +
      'It is free for personal, non-commercial use.',
      mtError, [mbOK], 0);
  end;

  StadiaMaps_ApiKey := FApiKey;
  MapView.Engine.RegisterProviders;
  MapView.MapProvider := FMapProvider;

  MapView.Active := FApiKey <> '';
  LocationsPanel.Enabled := FApiKey <> '';
  Panel2.Enabled := FApiKey <> '';

  UpdateCmds;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FRoute);
end;

function TMainForm.GetCostingAsString: String;
begin
  if tbVehicleAuto.Down then
    Result := 'auto'
  else
  if tbVehiclePedestrian.Down then
    Result := 'pedestrian'
  else
    raise Exception.Create('Selected costing model not implemented.');
end;

function TMainForm.GetLayer: TMaplayer;
begin
  if MapView.Layers.Count = 0 then
    Result := MapView.Layers.Add as TMapLayer
  else
    Result := MapView.Layers[0];
end;

{ Determines the point-of-interest stored a the given index in the Locations list }
function TMainForm.GetLocationPOI(AIndex: Integer): TMapPointOfInterest;
begin
  Result := TMapPointOfInterest(lbLocations.Items.Objects[AIndex]);
end;

{ Counts the number of locations, but only those which really contain GPS
  coordinates. }
function TMainForm.GetLocationPoints: TRealPointArray;
var
  i, n: Integer;
  poi: TMapPointOfInterest;
begin
  Result := nil;
  n := 0;
  SetLength(Result, lbLocations.Items.Count);
  for i := 0 to lbLocations.Items.Count-1 do
  begin
    poi := GetLocationPOI(i);
    if Assigned(poi) and not poi.RealPoint.Equal(NO_POINT) then
    begin
      Result[n] := poi.RealPoint;
      inc(n);
    end;
  end;
  SetLength(Result, n);
end;

function TMainForm.GetManeuverMarkerLayer: TMapLayer;
var
  i: Integer;
begin
  if MapView.Layers.Count < 2 then
    for i := MapView.Layers.Count to 1 do
      MapView.Layers.Add;
  Result := MapView.Layers[1];
end;

// code from: https://wiki.freepascal.org/fphttpclient#Posting_JSON
// json: https://docs.stadiamaps.com/routing/standard-routing/?utm_content=explore_api_docs&utm_campaign=products_routing_navigation_turn_by_turn_directions&utm_source=marketing_site#__tabbed_1_6
// details: https://docs.stadiamaps.com/api-reference/
(* Example taken from this site:
curl -X POST -H "Content-Type: application/json" -d '{
    "locations": [
        {
            "lon": -76.306572,
            "lat": 40.042072,
            "type": "break"
        },
        {
            "lon": -76.781559,
            "lat": 39.992115,
            "type": "break"
        }
    ],
    "costing": "auto",
    "costing_options": {
        "auto": {
            "use_highways": 0.3
        }
    },
    "units": "miles"
}' "https://api.stadiamaps.com/route/v1?api_key=YOUR-API-KEY"
*)
procedure TMainForm.GetRoute;
const
  LocationParamsMask =
    '    { '+ LineEnding +
    '       "lon": %.6f, ' + LineEnding +
    '       "lat": %.6f, ' + LineEnding +
    '       "type": "%s" ' + LineEnding +    // "break", "through", "via" or "break_through"
    '    }';
var
  fs: TFormatSettings;
  Client: TFPHttpClient;
  Response: TStringStream;
  Params: string = '{ ' + LineEnding +
      '"locations": [ ' + LineEnding +
      '%s' +      // start point
//      '%s' +      // via point(s)
//      '%s' +      // end point
      '], ' + LineEnding +
      '"costing": "%s", ' + LineEnding +       // "car", "bicycle", "bus", "pedestrians" (+ some more)
      '"costing_options": { ' + LineEnding +
      '    "auto": {' + LineEnding +
//      '       "use_tolls": 0, ' + LineEnding +
      '       "use_highways": 1.0 ' + LineEnding +
      '    }, ' + LineEnding +
      '    "pedestrians": {' + LineEnding +
      '       "use_tracks": 1.0, ' + LineEnding +
      '       "use_highways": 0.0, ' + LineEnding +
      '       "ignore_oneways": true ' + LineEnding +
      '    } ' + LineEnding +
      '}, ' + LineEnding +
//      '"format": "osrm", ' + LineEnding +   // <--- THE CODE HERE IS NOT PREPARED FOR OSRM!
      '"units": "%s", ' + LineEnding +     // or "kilometers" or "miles"
      '"language": "%s" ' + LineEnding +
  '}';
  URL: string = 'https://api.stadiamaps.com/route/v1?api_key=%s';
var
  json: TJSONData;
  i: Integer;
  pts: TRealPointArray;
  locationParams: String;
  locationType: String;
begin
  if FApiKey = '' then
  begin
    MessageDlg('An API-Key is needed for this application.', mtError, [mbOK], 0);
    exit;
  end;

  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  // Gets the location points (start, via, end)
  pts := GetLocationPoints;
  if Length(pts) < 2 then
    exit;

  // Prepares the "locations" node for the json
  locationParams := '';
  for i := 0 to High(pts) do
  begin
    if i > 0 then
    begin
      locationParams := locationParams + ',' + LineEnding;
      locationType := 'via';
    end;
    if (i = 0) or (i = High(pts)) then
      locationType := 'break';
    locationParams := locationParams +
      Format(LocationParamsMask, [
        pts[i].Lon, pts[i].Lat, locationType
      ], fs);
  end;

  // Creates the full json, posts it to the server and reads and analyses the
  // server' return stream.
  Screen.BeginWaitCursor;
  Client := TFPHttpClient.Create(nil);
  try
    Client.AddHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
    Client.AddHeader('Content-Type', 'application/json; charset=UTF-8');
    Client.AddHeader('Accept', 'application/json');
    Client.AllowRedirect := true;
    Client.RequestBody := TRawByteStringStream.Create(Format(Params, [
      locationParams, GetCostingAsString,
      LengthUnits, FLanguage
    ]));
    Response := TStringStream.Create('');
    try
      try
        Client.Post(Format(URL, [FApiKey]), Response);
        Response.Position := 0;
        json := GetJSON(Response, false);
        try
          SynEdit1.Lines.Text := json.FormatJSON([], 2);
          ShowRoute(json);
          if Client.ResponseStatusCode <> 200 then
            SynEdit1.Lines.AddText(TRawByteStringStream(Client.RequestBody).DataString);
        finally
          json.Free;
        end;
      except on E: Exception do
        SynEdit1.Lines.Add('Something bad happened: ' + E.Message);
      end;
    finally
      Response.Free;
      Client.RequestBody.Free;
    end;
  finally
    Client.Free;
    Screen.EndWaitCursor;
  end;
end;

{ Called when the locations listbox should be cleared. After clearing it
  adds the start and end entries locations agains which are needed permanently. }
procedure TMainForm.InitLocations;
begin
  lbLocations.Clear;
  NewLocation(0, 'Start point', IMGINDEX_START);
  NewLocation(1, 'End point'+LineEnding+'(Destination)', IMGINDEX_END);
  lbLocations.ItemIndex := -1;
end;

procedure TMainForm.lbLocationsClick(Sender: TObject);
begin
  UpdateCmds;
end;

procedure TMainForm.lbLocationsDblClick(Sender: TObject);
begin
  tbPickLocation.Down := true;
end;

procedure TMainForm.lbLocationsDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  listbox: TListbox;
  poi: TMapPointOfInterest;
  pt: TRealPoint;
  h, w, wLon, wLat, px2: Integer;
  x, y: Integer;
  hasCoord: Boolean;
begin
  listbox := TListBox(Control);
  if listbox.Items.Count = 0 then
    exit;

  listbox.Canvas.Font.Assign(listbox.Font);
  if [odSelected, odFocused] * State <> [] then
  begin
    listbox.Canvas.Brush.Color := clHighlight;
    listbox.Canvas.Font.Color := clHighlightText;
  end else
  begin
    listbox.Canvas.Brush.Color := clWindow;
    listbox.Canvas.Font.Color := clWindowText;
  end;
  listbox.Canvas.FillRect(ARect);

  listbox.Canvas.Font.Style := [fsBold];
  px2 := Scale96ToFont(2);
  x := ARect.Left + px2;
  y := ARect.Top + px2;
  listbox.Canvas.TextOut(x, y, listbox.Items[Index]);
  inc(y, listbox.Canvas.TextHeight('Tg'));
  listbox.Canvas.Font.Style := [];
  h := listbox.Canvas.TextHeight('Tg');
  wLat := listbox.Canvas.TextWidth('Latitude: ');
  wLon := listbox.Canvas.TextWidth('Longitude: ');
  w := Max(wLon, wLat);
  x := x + listbox.Canvas.TextWidth('M');
  hasCoord := false;
  poi := GetLocationPOI(Index);
  if poi <> nil then
  begin
    pt := poi.RealPoint;
    if not pt.Equal(NO_POINT) then
    begin
      listbox.Canvas.TextOut(x, y, 'Latitude:');
      listbox.Canvas.TextOut(x + w, y, FormatFloat('0.000000', pt.Lat) + '°');
      inc(y, h);
      listbox.Canvas.TextOut(x, y, 'Longitude:');
      listbox.Canvas.TextOut(x + w, y, FormatFloat('0.000000', pt.Lon) + '°');
      hasCoord := True;
    end;
  end;
  if not hasCoord then
  begin
    listbox.Canvas.TextOut(x, y, 'Latitude:');
    listbox.Canvas.TextOut(x + w, y, '-');
    inc(y, h);
    listbox.Canvas.TextOut(x, y, 'Longitude:');
    listbox.Canvas.TextOut(x + w, y, '-');
  end;
end;

procedure TMainForm.lbLocationsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
  listbox: TListbox;
begin
  listbox := TListbox(Sender);
  if listbox.ItemIndex >= 0 then
  begin
    R := listbox.ItemRect(listbox.ItemIndex);
    if Y > R.Bottom then
    begin
      listbox.ItemIndex := -1;
      listbox.Invalidate;
      UpdateCmds;
    end;
  end;
end;

procedure TMainForm.lbManeuversClick(Sender: TObject);
var
  rm: TRouteManeuver;
begin
  if lbManeuvers.ItemIndex = -1 then
    rm := nil
  else
    rm := TRouteManeuver(lbManeuvers.Items.Objects[lbManeuvers.ItemIndex]);
  ShowManeuverMarker(rm);
end;

procedure TMainForm.lbManeuversDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  ts: TTextStyle;
  listbox: TListbox;
  txt: String;
  dx: Integer;
begin
  listBox := TListBox(Control);
  if [odSelected, odFocused] * State <> [] then
  begin
    listbox.Canvas.Brush.Color := clHighlight;
    listbox.Canvas.Font.Color := clHighlightText;
  end else
  begin
    listbox.Canvas.Brush.Color := clWindow;
    listbox.canvas.Font.Color := clWindowText;
  end;
  listbox.Canvas.FillRect(ARect);

  ts := listbox.Canvas.TextStyle;
  ts.Wordbreak := true;
  ts.SingleLine := false;
  ts.Layout := tlCenter;
  txt := listbox.Items[Index];
  dx := Scale96ToFont(3);
  OffsetRect(ARect, dx, 0);
  listbox.Canvas.TextRect(ARect, ARect.Left, ARect.Top, txt, ts);
end;

procedure TMainForm.lbManeuversMeasureItem(Control: TWinControl;
  Index: Integer; var AHeight: Integer);
var
  R: TRect;
  txt: String;
  listbox: TListBox;
begin
  listbox := TListbox(Control);
  R := Rect(0, 0, listbox.ClientWidth - GetSystemMetrics(SM_CYVSCROLL), 10000);
    // Strange: Listbox.ClientWidth seems to be WITH scrollbar (on Windows).
  txt := listbox.Items[Index] + LineEnding;
  DrawText(listbox.Canvas.Handle, PChar(txt), Length(txt), R, DT_CALCRECT or DT_WORDBREAK);
  AHeight := R.Height;
end;

procedure TMainForm.MapViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if tbPickLocation.Down and
    ((lbLocations.ItemIndex = 0) or (lbLocations.ItemIndex = lbLocations.Items.Count-1))
  then
    MarkerEditorPlugin.Selection.Clear;
end;

{ The user released a mouse button. When the app is is "pick mode" and a
  location item is selected in the listbox, the geo point is assigned to the
  point-of-view of the selected item, and the route is recalculated. }
procedure TMainForm.MapViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  poi: TMapPointOfInterest;
  pt: TRealPoint;
  locIdx: Integer;
begin
  if Button <> mbLeft then
    exit;

  if tbPickLocation.Down and (lbLocations.ItemIndex > -1) then
  begin
    pt := MapView.ScreenToLatLon(Point(X, Y));
    locIdx := lbLocations.ItemIndex;
    poi := GetLocationPOI(locIdx);
    poi.RealPoint := pt;
    poi.Visible := true;
    lbLocations.ItemIndex := locIdx;
    lbLocations.Invalidate;
    GetRoute;
    MarkerEditorPlugin.Selection.Clear;
    tbPickLocation.Down := false;
    UpdateCmds;
  end;
end;

{ When CanClick returns true the MarkerEditorPlugin allows the user to click on
  the specified point and to drag it to a different location. }
procedure TMainForm.MarkerEditorPluginCanClick(AMapView: TMapView;
  APoint: TGPSPoint; var CanClick: Boolean);
var
  i: Integer;
  poi: TMapPointOfInterest;
  pt: TRealPoint;
begin
  for i := 0 to lbLocations.Items.Count-1 do
  begin
    poi := GetLocationPOI(i);
    if Assigned(poi) then
    begin
      pt := poi.RealPoint;
      CanClick := APoint.RealPoint.Equal(pt);
      if CanClick then exit;
    end;
  end;
  CanClick := false;
end;

{ The user has dragged a location point in the map. The method updates the
  locations listbox and queries a new route from the server. }
procedure TMainForm.MarkerEditorPluginEndDrag(Sender: TObject);
begin
  lbLocations.Invalidate;
  GetRoute;
end;

{ The user has selected another location point in the map. This methods selects
  the corresponding item in the locations listbox. }
procedure TMainForm.MarkerEditorPluginSelectionChange(Sender: TObject);
var
  poi: TMapPointOfInterest;
  selPt: TRealPoint;
  i: Integer;
begin
  if MarkerEditorPlugin.Selection.Count > 0 then
  begin
    selPt := MarkerEditorPlugin.Selection[0].RealPoint;
    for i := 0 to lbLocations.Items.Count-1 do
    begin
      poi := GetLocationPOI(i);
      if selPt.Equal(poi.RealPoint) then
      begin
        lbLocations.ItemIndex := i;
        lbLocations.Invalidate;
        exit;
      end;
    end;
  end;
end;

{ Adds a point-of-interest to the map. Since the geo coordiates are not yet
  known the point is hidden.
  Adds also a new item to the locations listbox which stores the
  point-of-interest in its Objects property. }
procedure TMainForm.NewLocation(AIndex: Integer; ACaption: String; AImageIndex: Integer);
var
  poi: TMapPointOfInterest;
begin
  poi := GetLayer.PointsOfInterest.Add as TMapPointOfInterest;
  poi.RealPoint := NO_POINT;
  poi.Caption := ACaption;
  poi.ImageIndex := AImageIndex;
  poi.Visible := false;   // Will be made visible when the poi gets its coordinates
  if AImageIndex = IMGINDEX_END then  // Assuming that this is the flag icon
  begin
    poi.ImageAnchorX := 0;
    poi.ImageAnchorY := 100;
  end;

  if lbLocations.Count < 2 then
    AIndex := lbLocations.Items.AddObject(ACaption, poi)
  else
    lbLocations.Items.InsertObject(AIndex, ACaption, poi);
  lbLocations.ItemIndex := AIndex;
  lbLocations.Invalidate;

  UpdateCmds;
end;

procedure TMainForm.ReadIni;
var
  ini: TCustomIniFile;
  Pt: TRealPoint;
  R: TRect;
  L, T, W, H: Integer;
  n: Integer;
begin
  ini := TIniFile.Create(CalcIniFileName);
  try
    ini.Options := ini.Options + [ifoFormatSettingsActive];

    W := Scale96ToFont(ini.ReadInteger('MainForm', 'Width', ScaleFontTo96(Width)));
    H := Scale96ToFont(ini.ReadInteger('MainForm', 'Height', ScaleFontTo96(Height)));
    L := Scale96ToFont(ini.ReadInteger('MainForm', 'Left', ScaleFontTo96(Left)));
    T := Scale96ToFont(ini.ReadInteger('MainForm', 'Top', Scale96ToFont(Top)));
    R := Screen.DesktopRect;
    if W > R.Width then W := R.Width;
    if H > R.Height then H := R.Height;
    if L < R.Left then L := R.Left;
    if T < R.Top then T := R.Top;
    if L + W > R.Right then L := R.Left;
    if T + H > R.Bottom then T := R.Top;
    SetBounds(L, T, W, H);

    PageControl.ActivePageIndex := ini.ReadInteger('MainForm', 'PageIndex', 0);

    FApiKey := ini.ReadString('Map', 'API_Key', FApiKey);
    FMapProvider := ini.ReadString('Map', 'MapProvider', 'OpenStreetmap Standard');
    pt.Lat := ini.ReadFloat('Map', 'MapCenter_Latitude', MapView.Center.Lat);
    pt.Lon := ini.ReadFloat('Map', 'MapCenter_Longitude', MapView.Center.Lon);
    MapView.MapCenter.RealPt := pt;
    MapView.Zoom := ini.ReadInteger('Map', 'Zoom', MapView.Zoom);

    n := ini.ReadInteger('Routing', 'Vehicle', 0);
    case n of
      0: tbVehicleAuto.Down := true;
      1: tbVehiclePedestrian.Down := true;
    end;
    FLanguage := StringReplace(ini.ReadString('Routing', 'Language', 'en-EN'), '_', '-', []);
    LengthUnits := ini.ReadString('Routing', 'LengthUnits', LengthUnits);

  finally
    ini.Free;
  end;
end;

procedure TMainForm.RequestApiKey;
var
  F: TApiKeyForm;
begin
  F := TApiKeyForm.Create(nil);
  try
    if F.ShowModal = mrOK then
      FApiKey := F.ApiKey;
  finally
    F.Free;
  end;
end;

procedure TMainForm.ShowRoute(AJson: TJSONData);
var
  totalTime, totalLength: Double;
begin
  if FRoute = nil then
    FRoute := TRoute.Create
  else
    FRoute.Clear;

  FRoute.JsonToStrings(AJson, lbManeuvers.Items);
  FRoute.JsonToPolyLine(AJson);

  if FRoute.JsonGetSummary(AJson, totalLength, totalTime) then
  begin
    infoTotalLength.Caption := LengthAsString(totalLength);
    infoTotalTime.Caption := TimeAsString(totalTime);
  end else
  begin
    infoTotalLength.Caption := '-';
    infoTotalTime.Caption := '-';
  end;

  AddRouteToMap(FRoute.PolyLine);

  ShowManeuverMarker(nil);
end;

procedure TMainForm.ShowManeuverMarker(AManeuver: TRouteManeuver);
var
  layer: TMapLayer;
  pt: TRealPoint;
  marker: TMapPointOfInterest;
begin
  if AManeuver <> nil then
    lbManeuvers.ItemIndex := lbManeuvers.Items.IndexOfObject(AManeuver);
  layer := GetManeuverMarkerLayer;
  if layer.PointsOfInterest.Count = 0 then
    marker := layer.PointsOfInterest.Add as TMapPointOfInterest
  else
    marker := layer.PointsOfInterest[0];

  if AManeuver = nil then
    marker.Visible := false
  else
  begin
    pt := FRoute.PolyLine[AManeuver.ShapeIndex_Begin];
    if tbVehicleAuto.Down then
      marker.ImageIndex := IMGINDEX_CAR
    else
    if tbVehiclePedestrian.Down then
      marker.ImageIndex := IMGINDEX_PEDESTRIAN;
    marker.RealPoint := pt;
    marker.Visible := true;
  end;
end;

{ The + button was clicked to add a "via" location to the listbox. }
procedure TMainForm.tbAddLocationClick(Sender: TObject);
var
  locIdx: Integer;
begin
  locIdx := lbLocations.ItemIndex;
  if (locIdx < 1) then    // cannot add a via point before the start point
    exit;

  MarkerEditorPlugin.Selection.Clear;
  NewLocation(locIdx, '', IMGINDEX_VIA);
  UpdateViaCaptions;

  PageControl.ActivePage := pgRoute;
end;

procedure TMainForm.tbClearLocationsClick(Sender: TObject);
begin
  MapView.Layers.Clear;
  InitLocations;
  lbManeuvers.Items.Clear;
  SynEdit1.Lines.Clear;
  infoTotalLength.Caption := '-';
  infoTotalTime.Caption := '-';
  lbLocations.ItemIndex := -1;
  UpdateCmds;
end;

procedure TMainForm.tbDeleteLocationClick(Sender: TObject);
var
  locIdx: Integer;
  poi: TMapPointOfInterest;
begin
  locIdx := lbLocations.ItemIndex;

  // Cannot delete the start and end points.
  if (locIdx = 0) or (locIdx = lbLocations.Items.Count-1) then
    exit;

  poi := GetLocationPOI(locIdx);
  poi.Layer.PointsOfInterest.Delete(poi.Index);
  lbLocations.Items.Delete(locIdx);
  UpdateViaCaptions;
  GetRoute;
  UpdateCmds;
end;

procedure TMainForm.tbPickLocationClick(Sender: TObject);
begin
  PageControl.ActivePage := pgRoute;
end;

procedure TMainForm.tbSettingsClick(Sender: TObject);
var
  F: TSettingsForm;
begin
  F := TSettingsForm.Create(nil);
  try
    F.Position := poMainFormCenter;
    F.Language := FLanguage;
    F.LengthUnits := LengthUnits;
    if F.ShowModal = mrOK then
    begin
      FLanguage := F.Language;
      LengthUnits := F.LengthUnits;
      GetRoute;
    end;
  finally
    F.Free;
  end;
end;

procedure TMainForm.tbVehicleAutoClick(Sender: TObject);
begin
  GetRoute;
end;

procedure TMainForm.tbVehiclePedestrianClick(Sender: TObject);
begin
  GetRoute;
end;

procedure TMainForm.UpdateCmds;
var
  locIdx: Integer;
  i: Integer;
  poi: TMapPointOfInterest;
  enab: Boolean;
begin
  locIdx := lbLocations.ItemIndex;
  tbAddLocation.Enabled := locIdx > 0;
  tbDeleteLocation.Enabled := (locIdx > 0) and (locIdx < lbLocations.Items.Count -1 );
  enab := false;
  for i := 0 to lbLocations.Items.Count-1 do
  begin
    poi := GetLocationPOI(i);
    if not poi.RealPoint.Equal(NO_POINT) then
    begin
      enab := true;
      break;
    end;
  end;
  tbClearLocations.Enabled := enab;
  tbPickLocation.Enabled := locIdx <> -1;
end;

{ Makes sure that all via locations are named in order. }
procedure TMainForm.UpdateViaCaptions;
var
  i: Integer;
  poi: TMapPointOfInterest;
begin
  for i := 1 to lbLocations.Items.Count-2 do
  begin
    lbLocations.Items[i] := 'Via point #' + IntToStr(i);
    poi := TMapPointOfInterest(lbLocations.Items.Objects[i]);
    poi.Caption := lbLocations.Items[i];
  end;
end;

procedure TMainForm.WriteIni;
var
  ini: TCustomIniFile;
  n: Integer;
begin
  ini := TIniFile.Create(CalcIniFileName);
  try
    ini.Options := ini.Options + [ifoFormatSettingsActive];

    ini.WriteInteger('MainForm', 'Left', ScaleFormTo96(Left));
    ini.WriteInteger('MainForm', 'Top', ScaleFormTo96(Top));
    ini.WriteInteger('MainForm', 'Width', ScaleFormTo96(Width));
    ini.WriteInteger('MainForm', 'Height', ScaleFormTo96(Height));
    ini.WriteInteger('MainForm', 'PageIndex', PageControl.ActivePageIndex);

    ini.WriteString('Map', 'API_Key', FApiKey);
    ini.WriteString('Map', 'MapProvider', MapView.MapProvider);
    ini.WriteFloat('Map', 'MapCenter_Latitude', MapView.Center.Lat);
    ini.WriteFloat('Map', 'MapCenter_Longitude', MapView.Center.Lon);
    ini.WriteInteger('Map', 'Zoom', MapView.Zoom);

    if tbVehicleAuto.Down then
      n := 0
    else if tbVehiclePedestrian.Down then
      n := 1;
    ini.WriteInteger('Routing', 'Vehicle', n);
    ini.WriteString('Routing', 'Language', FLanguage);
    ini.WriteString('Routing', 'LengthUnits', LengthUnits);
  finally
    ini.Free;
  end;
end;

end.

