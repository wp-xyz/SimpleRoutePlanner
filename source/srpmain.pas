unit srpMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, IniFiles, fgl,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, SynEdit,
  LCLIntf, LCLType, Buttons, Types,
  mvMapViewer, mvTypes, mvEngine, mvPluginCommon, mvMarkerPlugins, mvPlugins, mvGpsObj,
  fpHttpClient, fpJSON,
  srpGlobals, srpAPIKey, srpSettings, srpViaFrame;

const
  // ImageIndices in ImageList32
  IMGINDEX_START = 0;
  IMGINDEX_VIA = 0;
  IMGINDEX_END = 0;
  IMGINDEX_CAR = 1;
  IMGINDEX_PEDESTRIAN = 2;

  DEFAULT_POLYLINE_PRECISION = 6;

var
  LengthUnits: String = 'kilometers';  // or 'miles'

type
  TPickMode = (pmNone, pmStart, pmVia, pmEnd);

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

  TViaFrames = specialize TFPGObjectList <TViaFrame>;

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    btnClear: TButton;
    btnAddViaPoint: TBitBtn;
    lblTotalLength: TLabel;
    lblTotalTime: TLabel;
    infoTotalLength: TLabel;
    infoTotalTime: TLabel;
    lbManeuvers: TListBox;
    Panel3: TPanel;
    Panel4: TPanel;
    ViaPointsPanel: TPanel;
    ToolbarPanel: TPanel;
    sbAuto: TSpeedButton;
    sbPedestrian: TSpeedButton;
    sbSettings: TSpeedButton;
    Splitter1: TSplitter;
    tbPickEndPt: TToggleBox;
    tbPickStartPt: TToggleBox;
    btnSave: TButton;
    btnLoad: TButton;
    infoEndLon: TLabel;
    infoEndLat: TLabel;
    infoStartLat: TLabel;
    infoStartLon: TLabel;
    gbStart: TGroupBox;
    gbEnd: TGroupBox;
    ImageList32: TImageList;
    lblStartLat: TLabel;
    lblStartLon: TLabel;
    lblEndLat: TLabel;
    lblEndLon: TLabel;
    MapView: TMapView;
    MvPluginManager: TMvPluginManager;
    LegalNoticePlugin: TLegalNoticePlugin;
    MarkerEditorPlugin: TMarkerEditorPlugin;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    LeftPanel: TPanel;
    Panel2: TPanel;
    SaveDialog: TSaveDialog;
    SynEdit1: TSynEdit;
    pgJSON: TTabSheet;
    pgRoute: TTabSheet;
    procedure btnClearClick(Sender: TObject);
//    procedure btnClearViaClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnAddViaPointClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
    procedure sbSettingsClick(Sender: TObject);
    procedure sbAutoClick(Sender: TObject);
    procedure sbPedestrianClick(Sender: TObject);
    procedure tbPickEndPtClick(Sender: TObject);
    procedure tbPickStartPtClick(Sender: TObject);
    procedure tbPickViaPtClick(Sender: TObject);
  private
    FPickMode: TPickMode;
    FPickVia: Integer;
    FSelected: TPickMode;
    FSelectedViaIndex: Integer;
    FStartPt: TRealPoint;
    FViaPt: TRealPoint;
    FEndPt: TRealPoint;
    FApiKey: String;
    FMapProvider: String;
    FRoute: TRoute;
    FLanguage: String;
    FViaFrames: TViaFrames;
    procedure AddRouteToMap(ALine: TRealPointArray);
    procedure CloseViaFrameHandler(Sender: TObject);
    function GetCostingAsString: String;
    function GetLayer: TMapLayer;
    procedure GetRoute;
    function GetManeuverMarkerLayer: TMapLayer;
    function GetPickMode(out AViaIndex: Integer): TPickMode;
    procedure RequestApiKey;
    procedure SetEndPt(APoint: TRealPoint);
    procedure SetStartPt(APoint: TRealPoint);
//    procedure SetViaPt(APoint: TRealPoint);
    procedure ShowRoute(AJson: TJSONData);
    procedure ShowManeuverMarker(AManeuver: TRouteManeuver);

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
    point.RealPoint := RealPoint(ALine[i].Lat, ALine[i].Lon);
  end;
end;

procedure TMainForm.btnAddViaPointClick(Sender: TObject);
var
  frame: TViaFrame;
begin
  frame := TViaFrame.Create(self);
  frame.Name := '';
  frame.Index := FViaFrames.Count;
  frame.ViaPoint := NO_POINT;
  frame.Parent := ViaPointsPanel;
  frame.Align := alTop;
  frame.Top := 9999;
  frame.OnClose := @CloseViaFrameHandler;
  FViaFrames.Add(frame);
end;

procedure TMainForm.CloseViaFrameHandler(Sender: TObject);
var
  i, idx: Integer;
  layer: TMapLayer;
begin
  idx := FViaFrames.IndexOf(TViaFrame(Sender));
  if idx <> -1 then
  begin
    // Delete the via frame
    FViaFrames.Delete(idx);

    // Update the caption of the remaining via frames
    for i := 0 to FViaFrames.Count-1 do
      FViaFrames[i].Index := i;

    // Delete the via point from the map
    layer := GetLayer;
    inc(idx, 2);  // Keep start and end points
    if idx < layer.PointsOfInterest.Count then
      layer.PointsOfInterest.Delete(idx);

    // Update the caption of the remaining via points in the map
    for i := 2 to layer.PointsOfInterest.Count-1 do
      layer.PointsOfInterest[i].Caption := 'Via #' + IntToStr(i-1);
  end;
  GetRoute;
end;

procedure TMainForm.btnClearClick(Sender: TObject);
begin
  SetStartPt(NO_POINT);
  SetEndPt(NO_POINT);
  FViaFrames.Clear;
  MapView.Layers.Clear;
  lbManeuvers.Items.Clear;
  SynEdit1.Lines.Clear;
  infoTotalLength.Caption := '-';
  infoTotalTime.Caption := '-';
end;

procedure TMainForm.btnLoadClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    SynEdit1.Lines.LoadFromFile(OpenDialog.Filename);
end;
                                     (*
procedure TMainForm.btnClearViaClick(Sender: TObject);
var
  layer: TMapLayer;
begin
  FViaFrames.Clear;
  GetRoute;
  layer := GetLayer;
  while layer.PointsOfInterest.Count > 2 then
    layer.PointsOfInterest.Delete(2);
end;                               *)

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

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FViaFrames := TViaFrames.Create;

  BoldGroupBox(gbStart);
  BoldGroupBox(gbEnd);

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
  LeftPanel.Enabled := FApiKey <> '';
  Panel2.Enabled := FApiKey <> '';

  btnClearClick(nil);
  SynEdit1.Lines.Clear;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FRoute);
  FreeAndNil(FViaFrames);
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
  dx := Scale96ToFont(2);
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

function TMainForm.GetCostingAsString: String;
begin
  if sbAuto.Down then
    Result := 'auto'
  else
  if sbPedestrian.Down then
    Result := 'pedestrian'
  else
    raise Exception.Create('Selected costing not implemented.');
end;

function TMainForm.GetLayer: TMaplayer;
begin
  if MapView.Layers.Count = 0 then
    Result := MapView.Layers.Add as TMapLayer
  else
    Result := MapView.Layers[0];
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

function TMainForm.GetPickMode(out AViaIndex: Integer): TPickMode;
var
  i: Integer;
begin
  Result := pmNone;
  AViaIndex := -1;
  if tbPickStartPt.Checked then
    Result := pmStart
  else
  if tbPickEndPt.Checked then
    Result := pmEnd
  else
  begin
    Result := pmVia;
    for i := 0 to FViaFrames.Count-1 do
      if FViaFrames[i].Selected then
      begin
        AViaIndex := i;
        exit;
      end;
  end;
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
  StartParams: String = '';
  ViaParams: String = '';
  EndParams: String = '';
  Params: string = '{ ' + LineEnding +
      '"locations": [ ' + LineEnding +
      '%s' +      // start point
      '%s' +      // via point(s)
      '%s' +      // end point
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
  viaPt: TRealPoint;
begin
  if FStartPt.Equal(NO_POINT) then exit;
  if FEndPt.Equal(NO_POINT) then exit;

  if FApiKey = '' then
  begin
    MessageDlg('API-Key needed for this application.', mtError, [mbOK], 0);
    exit;
  end;

  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  StartParams := Format(LocationParamsMask + ',' + LineEnding, [
    FStartPt.Lon, FStartPt.Lat, 'break'
  ], fs);

  ViaParams := '';
  for i := 0 to FViaFrames.Count-1 do
  begin
    viaPt := FViaFrames[i].ViaPoint;
    if not viaPt.Equal(NO_POINT) then
      ViaParams := ViaParams + //',' + LineEnding +
        Format(LocationParamsMask + ',' + LineEnding, [
          viaPt.Lon, viaPt.Lat, 'via'
        ], fs);
  end;
  //if ViaParams <> '' then Delete(ViaParams, 1, Length(','+LineEnding));

  EndParams := Format(LocationParamsMask + LineEnding, [
    FEndPt.Lon, FEndPt.Lat, 'break'
  ], fs);

  Screen.BeginWaitCursor;
  Client := TFPHttpClient.Create(nil);
  try
    Client.AddHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
    Client.AddHeader('Content-Type', 'application/json; charset=UTF-8');
    Client.AddHeader('Accept', 'application/json');
    Client.AllowRedirect := true;
    Client.RequestBody := TRawByteStringStream.Create(Format(Params, [
      StartParams, ViaParams, EndParams, GetCostingAsString,
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

procedure TMainForm.MapViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FPickMode in [pmStart, pmEnd] then
    MarkerEditorPlugin.Selection.Clear;
end;

procedure TMainForm.MapViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  layer: TMapLayer;
  poi: TMapPointOfInterest;
  i: Integer;
  viaIdx: Integer;
begin
  if Button <> mbLeft then
    exit;

  case GetPickMode(viaIdx) of
    pmStart:
      begin
        SetStartPt(MapView.ScreenToLatLon(Point(X, Y)));
        layer := GetLayer;
        if layer.PointsOfInterest.Count < 1 then
          layer.PointsOfInterest.Add;
        poi := layer.PointsOfInterest[0];               // 0 = route start
        poi.RealPoint := FStartPt;
        poi.ImageIndex := IMGINDEX_START;
        poi.Caption := 'Start';
        GetRoute;
        MarkerEditorPlugin.Selection.Clear;
        tbPickStartPt.Checked := false;
        FPickMode := pmNone;
      end;

    pmVia:
      if viaIdx > -1 then
      begin
        FViaFrames[viaIdx].ViaPoint := MapView.ScreenToLatLon(Point(X, Y));
        layer := GetLayer;
        if layer.PointsOfInterest.Count < 3 + viaIdx then
          for i := layer.PointsOfInterest.Count to 2 + viaIdx do
            layer.PointsOfInterest.Add;
        poi := layer.PointsOfInterest[2 + viaIdx];               // 2 = via point
        poi.RealPoint := FViaFrames[viaIdx].ViaPoint;
        poi.ImageIndex := IMGINDEX_VIA;
        poi.Caption := 'Via #' + IntToStr(viaIdx+1);
        GetRoute;
        MarkerEditorPlugin.Selection.Clear;
        FViaFrames[viaIdx].UnselectVia;
        FPickMode := pmNone;
      end;

    pmEnd:
      begin
        SetEndPt(MapView.ScreenToLatLon(Point(X, Y)));
        layer := GetLayer;
        if layer.PointsOfInterest.Count < 2 then
          for i := layer.PointsOfInterest.Count to 1 do
            layer.PointsOfInterest.Add;
        poi := layer.PointsOfInterest[1];               // 1 = route end point (destination)
        poi.RealPoint := FEndPt;
        poi.ImageIndex := IMGINDEX_END;
        poi.Caption := 'Destination';
        GetRoute;
        tbPickEndPt.Checked := false;
        FPickMode := pmNone;
        MarkerEditorPlugin.Selection.Clear;
      end;
  end;
end;

procedure TMainForm.MarkerEditorPluginCanClick(AMapView: TMapView;
  APoint: TGPSPoint; var CanClick: Boolean);
var
  i: Integer;
  pt: TRealPoint;
begin
  CanClick := APoint.RealPoint.Equal(FStartPt) or APoint.RealPoint.Equal(FEndPt);
  if not CanClick then
    for i := 0 to FViaFrames.Count-1 do
    begin
      pt := FViaFrames[i].ViaPoint;
      CanClick := APoint.RealPoint.Equal(pt) and not (pt.Equal(NO_POINT));
      if CanClick then exit;
    end;
end;

procedure TMainForm.MarkerEditorPluginEndDrag(Sender: TObject);
var
  pt: TRealPoint;
  i: Integer;
begin
  if MarkerEditorPlugin.Selection.Count > 0 then
  begin
    pt := MarkerEditorPlugin.Selection[0].RealPoint;
    case FSelected of
      pmStart:
        SetStartPt(pt);
      pmVia:
        FViaFrames[FSelectedViaIndex].ViaPoint := pt;
      pmEnd:
        SetEndPt(pt);
      else
        exit;
    end;
    GetRoute;
  end;
end;

procedure TMainForm.MarkerEditorPluginSelectionChange(Sender: TObject);
var
  selPt: TRealPoint;
  i: Integer;
begin
  FSelected := pmNone;
  if MarkerEditorPlugin.Selection.Count > 0 then
  begin
    selPt := MarkerEditorPlugin.Selection[0].RealPoint;
    if selPt.Equal(FStartPt) then
      FSelected := pmStart
    else
    if selPt.Equal(FEndPt) then
      FSelected := pmEnd
    else
    if selPt.Equal(FViaPt) then
      FSelected := pmVia
    else
      for i := 0 to FViaFrames.Count-1 do
        if selPt.Equal(FViaFrames[i].ViaPoint) then
        begin
          FSelected := pmVia;
          FSelectedViaIndex := i;
          break;
        end;
  end;
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
      0: sbAuto.Down := true;
      1: sbPedestrian.Down := true;
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

procedure TMainForm.sbAutoClick(Sender: TObject);
begin
  GetRoute;
end;

procedure TMainForm.sbPedestrianClick(Sender: TObject);
begin
  GetRoute;
end;

procedure TMainForm.sbSettingsClick(Sender: TObject);
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

procedure TMainForm.SetEndPt(APoint: TRealPoint);
begin
  FEndPt := APoint;
  if APoint.Equal(NO_POINT) then
  begin
    infoEndLat.Caption := '';
    infoEndLon.Caption := '';
  end else
  begin
    infoEndLat.Caption := FormatFloat('0.000000', FEndPt.Lat) + '°';
    infoEndLon.Caption := FormatFloat('0.000000', FEndPt.Lon) + '°';
  end;
end;

procedure TMainForm.SetStartPt(APoint: TRealPoint);
begin
  FStartPt := APoint;
  if APoint.Equal(NO_POINT) then
  begin
    infoStartLat.Caption := '';
    infoStartLon.Caption := '';
  end else
  begin
    infoStartLat.Caption := FormatFloat('0.000000', FStartPt.Lat) + '°';
    infoStartLon.Caption := FormatFloat('0.000000', FStartPt.Lon) + '°';
  end;
end;
               (*
procedure TMainForm.SetViaPt(APoint: TRealPoint);
begin
  FViaPt := APoint;
  if APoint.Equal(NO_POINT) then
  begin
    infoViaLat.Caption := '';
    infoViaLon.Caption := '';
  end else
  begin
    infoViaLat.Caption := FormatFloat('0.000000', FViaPt.Lat) + '°';
    infoViaLon.Caption := FormatFloat('0.000000', FViaPt.Lon) + '°';
  end;
end;         *)

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
    if sbAuto.Down then
      marker.ImageIndex := IMGINDEX_CAR
    else
    if sbPedestrian.Down then
      marker.ImageIndex := IMGINDEX_PEDESTRIAN;
    marker.RealPoint := pt;
    marker.Visible := true;
  end;
end;

procedure TMainForm.tbPickEndPtClick(Sender: TObject);
begin
  FPickMode := pmEnd;
  PageControl.ActivePage := pgRoute;
end;

procedure TMainForm.tbPickStartPtClick(Sender: TObject);
begin
  FPickMode := pmStart;
  PageControl.ActivePage := pgRoute;
end;

procedure TMainForm.tbPickViaPtClick(Sender: TObject);
begin
  FPickMode := pmVia;
  PageControl.ActivePage := pgRoute;
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

    if sbAuto.Down then n := 0;
    if sbPedestrian.Down then n := 1;
    ini.WriteInteger('Routing', 'Vehicle', n);
    ini.WriteString('Routing', 'Language', FLanguage);
    ini.WriteString('Routing', 'LengthUnits', LengthUnits);
  finally
    ini.Free;
  end;
end;

end.

