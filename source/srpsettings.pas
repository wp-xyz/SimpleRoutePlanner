unit srpSettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    ButtonPanel: TButtonPanel;
    cmbLanguage: TComboBox;
    cmbLengthUnits: TComboBox;
    lblLengthUnits: TLabel;
    lblLanguage: TLabel;
    procedure FormActivate(Sender: TObject);
  private
    FActivated: Boolean;
    function GetLanguage: String;
    function GetLengthUnits: String;
    procedure SetLanguage(AValue: String);
    procedure SetLengthUnits(AValue: String);
  public
    property Language: String read GetLanguage write SetLanguage;
    property LengthUnits: String read GetLengthUnits write SetLengthUnits;
  end;

var
  SettingsForm: TSettingsForm;

implementation

{$R *.lfm}

procedure TSettingsForm.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    FActivated := true;
    Constraints.MinHeight := cmbLengthUnits.Top + cmbLengthUnits.Height +
      cmbLengthUnits.borderSpacing.Bottom + ButtonPanel.Height;
  end;
end;

function TSettingsForm.GetLanguage: String;
begin
  Result := cmbLanguage.Items[cmbLanguage.ItemIndex];
end;

function TSettingsForm.GetLengthUnits: String;
begin
  Result := cmbLengthUnits.Items[cmbLengthUnits.ItemIndex];
end;

procedure TSettingsForm.SetLanguage(AValue: String);
begin
  cmbLanguage.ItemIndex := cmbLanguage.Items.IndexOf(AValue);
end;

procedure TSettingsForm.SetLengthUnits(AValue: String);
begin
  cmbLengthUnits.ItemIndex := cmbLengthUnits.Items.IndexOf(AValue);
end;

end.

