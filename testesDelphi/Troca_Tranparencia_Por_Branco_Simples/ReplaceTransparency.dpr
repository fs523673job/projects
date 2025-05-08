program ReplaceTransparency;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows,
  Vcl.Graphics,
  Vcl.Imaging.pngimage;

function TemTransparencia(APng: TPngImage): Boolean;
var
  X, Y: Integer;
  AlphaLine: PByteArray;
begin
  Result := False;

  if not (APng.Header.ColorType in [COLOR_RGBALPHA, COLOR_GRAYSCALEALPHA]) then
    Exit(False);

  for Y := 0 to APng.Height - 1 do
  begin
    AlphaLine := APng.AlphaScanline[Y];
    if Assigned(AlphaLine) then
    begin
      for X := 0 to APng.Width - 1 do
        if AlphaLine^[X] < 255 then
          Exit(True);
    end;
  end;
end;

procedure RemoverTransparencia(APng: TPngImage; CorFundo: TColor);
var
  X, Y: Integer;
  AlphaLine: PByteArray;
  RGBLine: PRGBLine;
  FundoR, FundoG, FundoB: Byte;
  Alpha: Single;
begin
  FundoR := GetRValue(ColorToRGB(CorFundo));
  FundoG := GetGValue(ColorToRGB(CorFundo));
  FundoB := GetBValue(ColorToRGB(CorFundo));

  APng.Canvas.Lock;
  try
    for Y := 0 to APng.Height - 1 do
    begin
      AlphaLine := APng.AlphaScanline[Y];
      RGBLine := APng.Scanline[Y];

      if Assigned(AlphaLine) then
      begin
        for X := 0 to APng.Width - 1 do
        begin
          if AlphaLine^[X] < 255 then
          begin
            Alpha := AlphaLine^[X] / 255;

            RGBLine^[X].rgbtRed := Round(RGBLine^[X].rgbtRed * Alpha + FundoR * (1 - Alpha));
            RGBLine^[X].rgbtGreen := Round(RGBLine^[X].rgbtGreen * Alpha + FundoG * (1 - Alpha));
            RGBLine^[X].rgbtBlue := Round(RGBLine^[X].rgbtBlue * Alpha + FundoB * (1 - Alpha));

            AlphaLine^[X] := 255;
          end;
        end;
      end;
    end;

    APng.RemoveTransparency;
  finally
    APng.Canvas.Unlock;
  end;
end;

var
  Png: TPngImage;
  ArquivoEntrada, ArquivoSaida: String;
begin
  try
    Write('Informe o arquivo PNG de entrada: ');
    Readln(ArquivoEntrada);

    if not FileExists(ArquivoEntrada) then
      raise Exception.Create('Arquivo n�o encontrado.');

    Png := TPngImage.Create;
    try
      Png.LoadFromFile(ArquivoEntrada);

      if TemTransparencia(Png) then
      begin
        Writeln('Imagem possui transpar�ncia. Removendo...');

        RemoverTransparencia(Png, clWhite);

        ArquivoSaida := ChangeFileExt(ArquivoEntrada, '_SemTransparencia.png');
        Png.SaveToFile(ArquivoSaida);

        Writeln('Transpar�ncia removida e salvo em: ', ArquivoSaida);
      end
      else
        Writeln('Imagem n�o possui transpar�ncia.');
    finally
      Png.Free;
    end;
  except
    on E: Exception do
      Writeln('Erro: ', E.Message);
  end;

  Writeln('Pressione ENTER para sair.');
  Readln;
end.

