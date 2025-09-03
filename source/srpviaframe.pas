unit srpViaFrame;

{$mode ObjFPC}{$H+}

interface

uses                LazLoggerBase,
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls,
  mvTypes,
  srpGlobals;

type

  { TViaFrame }

  TCloseViaFrameEvent = TNotifyEvent;

  TViaFrame = class(TFrame)
    btnClearVia: TButton;
    gbVia: TGroupBox;
    infoViaLat: TLabel;
    infoViaLon: TLabel;
    lblViaLat: TLabel;
    lblViaLon: TLabel;
    Panel1: TPanel;
    tbPickViaPt: TToggleBox;
    procedure btnClearViaClick(Sender: TObject);
  private
    FIndex: Integer;
    FViaPoint: TRealPoint;
    FOnClose: TCloseViaFrameEvent;
    procedure SetIndex(AValue: Integer);
    procedure SetViaPoint(APoint: TRealPoint);

  public
    constructor Create(AOwner: TComponent); override;
    function Selected: Boolean;
    procedure SelectVia;
    procedure UnselectVia;
    property Index: Integer read FIndex write SetIndex;
    property ViaPoint: TRealPoint read FViaPoint write SetViaPoint;
    property OnClose: TCloseViaFrameEvent read FOnClose write FOnClose;
  end;

implementation

{$R *.lfm}

constructor TViaFrame.Create(AOwner: TComponent);
begin
  inherited;
  BoldGroupBox(gbVia);
  ViaPoint := NO_POINT;
end;

procedure TViaFrame.btnClearViaClick(Sender: TObject);
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

function TViaFrame.Selected: Boolean;
begin
  Result := tbPickViaPt.Checked;
end;

procedure TViaFrame.SelectVia;
begin
  tbPickViaPt.Checked := true;
end;

procedure TViaFrame.SetIndex(AValue: Integer);
begin
  FIndex := AValue;
  gbVia.Caption := 'Via point #' + IntToStr(FIndex + 1);
end;

procedure TViaFrame.SetViaPoint(APoint: TRealPoint);
begin
  FViaPoint := APoint;
  if APoint.Equal(NO_POINT) then
  begin
    infoViaLat.Caption := '';
    infoViaLon.Caption := '';
  end else
  begin
    infoViaLat.Caption := FormatFloat(DEFAULT_GPS_FORMAT, FViaPoint.Lat);
    infoViaLon.Caption := FormatFloat(DEFAULT_GPS_FORMAT, FViaPoint.Lon);
  end;
end;

procedure TViaFrame.UnselectVia;
begin
  tbPickViaPt.Checked := false;
end;

end.

