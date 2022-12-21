unit U_FormatConverter;

interface

uses
  System.SysUtils,
  System.Classes,
  U_JSON.XML,
  U_XML.JSON;

type
  TFormatConverter = class(TComponent)
  private
    fJSONtoXML : TJSONtoXML;
    fXMLtoJSON : TXMLtoJSON;
    fDammyComp : TComponent;

  protected
    { Protected declarations }

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;

  published
    property JSONtoXML : TJSONtoXML read fJSONtoXML write fJSONtoXML;
    property XMLtoJSON : TXMLtoJSON read fXMLtoJSON write fXMLtoJSON;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Ridge Dynamics', [TFormatConverter]);
end;

{ TFormatConverter }

constructor TFormatConverter.Create(AOwner: TComponent);
begin
  if not Assigned(AOwner) then
  begin
    fDammyComp := TComponent.Create(nil);
    inherited Create(fDammyComp);
  end
  else
    inherited Create(AOwner);

  fJSONtoXML := TJSONtoXML.Create();
  fXMLtoJSON := TXMLtoJSON.Create();
end;

destructor TFormatConverter.Destroy;
begin
  if Assigned(fDammyComp) then
    FreeAndNil(fDammyComp);
end;

end.
