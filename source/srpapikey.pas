unit srpAPIKey;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls, LCLIntf, Buttons,
  srpDatamodule;

type

  { TApiKeyForm }

  TApiKeyForm = class(TForm)
    ButtonPanel: TButtonPanel;
    edApiKey: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lblURL: TLabel;
    btnPasteAPIKey: TSpeedButton;
    procedure FormActivate(Sender: TObject);
    procedure lblURLClick(Sender: TObject);
    procedure lblURLMouseEnter(Sender: TObject);
    procedure lblURLMouseLeave(Sender: TObject);
    procedure btnPasteAPIKeyClick(Sender: TObject);
  private
    FActivated: Boolean;
    function GetApiKey: String;
    procedure SetApiKey(const AValue: String);

  public
    property ApiKey: String read GetApiKey write SetApiKey;

  end;

var
  ApiKeyForm: TApiKeyForm;

implementation

{$R *.lfm}

uses
  ClipBrd;

{ TApiKeyForm }

procedure TApiKeyForm.btnPasteAPIKeyClick(Sender: TObject);
begin
  edApiKey.Text := Clipboard.AsText;
end;

procedure TApiKeyForm.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    FActivated := true;
    Constraints.MinHeight := edApiKey.Top + edApiKey.Height + edApiKey.BorderSpacing.Bottom +
      ButtonPanel.Height + ButtonPanel.BorderSpacing.Around;
  end;
end;

function TApiKeyForm.GetApiKey: String;
begin
  Result := edApiKey.Text;
end;

procedure TApiKeyForm.lblURLClick(Sender: TObject);
begin
  OpenURL(lblURL.Caption);
end;

procedure TApiKeyForm.lblURLMouseEnter(Sender: TObject);
begin
  Screen.Cursor := crHandPoint;
  lblURL.Font.Style := [fsUnderline];
end;

procedure TApiKeyForm.lblURLMouseLeave(Sender: TObject);
begin
  Screen.Cursor := Cursor;
  lblURL.Font.Style := [];
end;

procedure TApiKeyForm.SetApiKey(const AValue: String);
begin
  edApiKey.Text := AValue;
end;


end.

