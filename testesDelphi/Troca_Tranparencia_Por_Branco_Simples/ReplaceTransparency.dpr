program ReplaceTransparency;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows,
  Types,
  Vcl.Graphics,
  Vcl.Imaging.pngimage;

const
  COR_MAGICA = $00FF00FF;

function CoresSemelhantes(C1, C2: TColor; Tolerancia: Byte = 10): Boolean;
var
  RGB1, RGB2: COLORREF;
begin
  RGB1 := ColorToRGB(C1);
  RGB2 := ColorToRGB(C2);

  Result :=
    (Abs(Integer(GetRValue(RGB1)) - Integer(GetRValue(RGB2))) <= Integer(Tolerancia)) and
    (Abs(Integer(GetGValue(RGB1)) - Integer(GetGValue(RGB2))) <= Integer(Tolerancia)) and
    (Abs(Integer(GetBValue(RGB1)) - Integer(GetBValue(RGB2))) <= Integer(Tolerancia));
end;


procedure SubstituirCorPorBranco(ABitmap: TBitmap; CorAlvo: TColor; Tolerancia: Byte = 10);
var
  X, Y: Integer;
  PixelColor: TColor;
begin
  for Y := 0 to ABitmap.Height - 1 do
  begin
    for X := 0 to ABitmap.Width - 1 do
    begin
      PixelColor := ABitmap.Canvas.Pixels[X, Y];
      if CoresSemelhantes(PixelColor, CorAlvo, Tolerancia) then
        ABitmap.Canvas.Pixels[X, Y] := clWhite;
    end;
  end;
end;

procedure PngSubstituirCorPorBranco(APng: TPngImage; CorAlvo: TColor; Tolerancia: Byte = 10);
var
  X, Y: Integer;
  PixelColor: TColor;
begin

  var Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf32bit;
    Bitmap.AlphaFormat := afIgnored;
    Bitmap.SetSize(APng.Width, APng.Height);

    Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));

    var RectImg: TRect := Rect(0, 0, APng.Width, APng.Height);
    APng.Draw(Bitmap.Canvas, RectImg);

    for Y := 0 to Bitmap.Height - 1 do
    begin
      for X := 0 to Bitmap.Width - 1 do
      begin
        PixelColor := Bitmap.Canvas.Pixels[X, Y];
        if CoresSemelhantes(PixelColor, CorAlvo, Tolerancia) then
          Bitmap.Canvas.Pixels[X, Y] := clWhite;
      end;
    end;

    APng.Assign(Bitmap);
    APng.RemoveTransparency;
  finally
    Bitmap.Free;
  end;
end;

function PngContemCorAlvo(APng: TPngImage; CorAlvo: TColor; Tolerancia: Byte = 10): Boolean;
var
  X, Y: Integer;
  PixelColor: TColor;
  Bitmap: TBitmap;
  RectImg: TRect;
begin
  Result := False;

  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf32bit;
    Bitmap.AlphaFormat := afIgnored;
    Bitmap.SetSize(APng.Width, APng.Height);

    RectImg := Rect(0, 0, APng.Width, APng.Height);
    APng.Draw(Bitmap.Canvas, RectImg);

    for Y := 0 to Bitmap.Height - 1 do
    begin
      for X := 0 to Bitmap.Width - 1 do
      begin
        PixelColor := ColorToRGB(Bitmap.Canvas.Pixels[X, Y]);
        if CoresSemelhantes(PixelColor, CorAlvo, Tolerancia) then
          Exit(True);
      end;
    end;
  finally
    Bitmap.Free;
  end;
end;

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

procedure RemoverTransparenciaCorretamente(APng: TPngImage; CorFundo: TColor);
var
  X, Y: Integer;
  AlphaLine: PByteArray;
  RGBLine: PRGBLine;
  FundoR, FundoG, FundoB: Byte;
  Alfa: Byte;
  AlfaNormalizado: Single;
begin
  // Componentes RGB da cor de fundo
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
          Alfa := AlphaLine^[X];

          // Se houver transparência parcial
          if Alfa < 255 then
          begin
            AlfaNormalizado := Alfa / 255;

            // Composição correta sobre o fundo
            RGBLine^[X].rgbtRed := Round((RGBLine^[X].rgbtRed * AlfaNormalizado) + (FundoR * (1 - AlfaNormalizado)));
            RGBLine^[X].rgbtGreen := Round((RGBLine^[X].rgbtGreen * AlfaNormalizado) + (FundoG * (1 - AlfaNormalizado)));
            RGBLine^[X].rgbtBlue := Round((RGBLine^[X].rgbtBlue * AlfaNormalizado) + (FundoB * (1 - AlfaNormalizado)));

            // Remove a transparência totalmente
            AlphaLine^[X] := 255;
          end;
        end;
      end;
    end;

    // Remove a informação de canal alfa da imagem
    APng.RemoveTransparency;
  finally
    APng.Canvas.Unlock;
  end;
end;

procedure RemoverTransparenciaPNG(APng: TPngImage; const ArquivoEntrada, ArquivoSaida: String; CorFundo: TColor);
var
  Bitmap: TBitmap;
  RectImg: TRect;
begin
  if not FileExists(ArquivoEntrada) then
    raise Exception.Create('Arquivo não encontrado.');

  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf32bit;
    Bitmap.AlphaFormat := afIgnored; // Sem alfa
    Bitmap.SetSize(APng.Width, APng.Height);

    Bitmap.Canvas.Brush.Color := CorFundo;
    Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));

    RectImg := Rect(0, 0, APng.Width, APng.Height);
    APng.Draw(Bitmap.Canvas, RectImg);

    APng.Assign(Bitmap);
    APng.RemoveTransparency;
  finally
    Bitmap.Free;
  end;
end;

procedure ExRemoverTransparenciaPNG(APng: TPngImage; const ArquivoEntrada, ArquivoSaida: String; CorFundoTemporario: TColor);
var
  Bitmap: TBitmap;
  RectImg: TRect;
begin
  if not FileExists(ArquivoEntrada) then
    raise Exception.Create('Arquivo não encontrado: ' + ArquivoEntrada);

  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf32bit;
    Bitmap.AlphaFormat := afIgnored;
    Bitmap.SetSize(APng.Width, APng.Height);

    Bitmap.Canvas.Brush.Color := CorFundoTemporario;
    Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));

    RectImg := Rect(0, 0, APng.Width, APng.Height);
    APng.Draw(Bitmap.Canvas, RectImg);

    SubstituirCorPorBranco(Bitmap, CorFundoTemporario, 10);

    APng.Assign(Bitmap);
    APng.RemoveTransparency;
    APng.SaveToFile(ArquivoSaida);
  finally
    Bitmap.Free;
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
      raise Exception.Create('Arquivo não encontrado.');

    Png := TPngImage.Create;
    try
      Png.LoadFromFile(ArquivoEntrada);

      if TemTransparencia(Png) then
      begin
        Writeln('Imagem possui transparência. Removendo...');

        ArquivoSaida := ChangeFileExt(ArquivoEntrada, '_SemTransparencia.png');

        //RemoverTransparenciaCorretamente(Png, clBlack);
        //RemoverTransparencia(Png, clBlack);
        //RemoverTransparenciaPNG(Png, ArquivoEntrada, ArquivoSaida, clBlack);
        ExRemoverTransparenciaPNG(Png, ArquivoEntrada, ArquivoSaida, COR_MAGICA);


        while PngContemCorAlvo(Png, COR_MAGICA, 10) do
          PngSubstituirCorPorBranco(Png, COR_MAGICA, 20);

        Png.SaveToFile(ArquivoSaida);

        Writeln('Transparência removida e salvo em: ', ArquivoSaida);
      end
      else
        Writeln('Imagem não possui transparência.');
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

