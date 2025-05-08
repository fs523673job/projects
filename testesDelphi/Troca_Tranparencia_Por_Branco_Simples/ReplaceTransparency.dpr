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
  Line: PRGBLine;
  AlphaLine: PByteArray;
  R, G, B: Byte;
  TargetR, TargetG, TargetB: Byte;
begin
  TargetR := GetRValue(ColorToRGB(CorAlvo));
  TargetG := GetGValue(ColorToRGB(CorAlvo));
  TargetB := GetBValue(ColorToRGB(CorAlvo));
  APng.Canvas.Lock;
  try
    for Y := 0 to APng.Height - 1 do
    begin
      Line := APng.Scanline[Y];
      AlphaLine := APng.AlphaScanline[Y];
      for X := 0 to APng.Width - 1 do
      begin
        R := Line^[X].rgbtRed;
        G := Line^[X].rgbtGreen;
        B := Line^[X].rgbtBlue;
        if (Abs(R - TargetR) <= Tolerancia) and
           (Abs(G - TargetG) <= Tolerancia) and
           (Abs(B - TargetB) <= Tolerancia) then
        begin
          Line^[X].rgbtRed   := 255;
          Line^[X].rgbtGreen := 255;
          Line^[X].rgbtBlue  := 255;
          if Assigned(AlphaLine) then
            AlphaLine^[X] := 255;
        end;
      end;
    end;
  finally
    APng.Canvas.Unlock;
  end;
end;

procedure ExPngSubstituirTransparenciaPorBranco(APng: TPngImage; CorAlvo: TColor);
var
  X, Y: Integer;
  Line: PRGBLine;
  AlphaLine: PByteArray;
  R, G, B: Byte;
begin
  APng.Canvas.Lock;
  try
    for Y := 0 to APng.Height - 1 do
    begin
      Line := APng.Scanline[Y];
      AlphaLine := APng.AlphaScanline[Y];
      for X := 0 to APng.Width - 1 do
      begin
        if Assigned(AlphaLine) then
        begin
          if AlphaLine^[X] < 200 then
          begin
            AlphaLine^[X]      := 255;
            Line^[X].rgbtRed   := GetRValue(ColorToRGB(CorAlvo));
            Line^[X].rgbtGreen := GetGValue(ColorToRGB(CorAlvo));
            Line^[X].rgbtBlue  := GetBValue(ColorToRGB(CorAlvo));
          end;
        end;
      end;
    end;
  finally
    APng.Canvas.Unlock;
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

    for var c := 1 to 3 do
      PngSubstituirCorPorBranco(APng, CorFundoTemporario, 30); //30
  finally
    Bitmap.Free;
  end;
end;

procedure RemoverResiduosMagicos(APng: TPngImage; CorMagica: TColor; ToleranciaMax: Integer = 40);
var
  X, Y: Integer;
  Line: PRGBLine;
  AlphaLine: PByteArray;
  TargetR, TargetG, TargetB: Byte;
  Distancia: Integer;
begin
  TargetR := GetRValue(ColorToRGB(CorMagica));
  TargetG := GetGValue(ColorToRGB(CorMagica));
  TargetB := GetBValue(ColorToRGB(CorMagica));
  APng.Canvas.Lock;
  try
    for Y := 0 to APng.Height - 1 do
    begin
      Line := APng.Scanline[Y];
      AlphaLine := APng.AlphaScanline[Y];
      for X := 0 to APng.Width - 1 do
      begin
        // Calcula a distância euclidiana (mais precisa que tolerância por canal)
        Distancia := Round(Sqrt(
          Sqr(Line^[X].rgbtRed - TargetR) +
          Sqr(Line^[X].rgbtGreen - TargetG) +
          Sqr(Line^[X].rgbtBlue - TargetB)
        ));
        // Se estiver dentro da tolerância (ajuste conforme necessário)
        if Distancia <= ToleranciaMax then
        begin
          Line^[X].rgbtRed := 255;
          Line^[X].rgbtGreen := 255;
          Line^[X].rgbtBlue := 255;
          if Assigned(AlphaLine) then
            AlphaLine^[X] := 255;
        end;
      end;
    end;
  finally
    APng.Canvas.Unlock;
  end;
end;

procedure RemoverResiduosComDilatacao(APng: TPngImage; CorMagica: TColor; Tolerancia: Integer = 40);
var
  X, Y, I, J: Integer;
  Line: PRGBLine;
  TargetR, TargetG, TargetB: Byte;
  Mask: array of array of Boolean;
  VizinhoEncontrado: Boolean;
begin
  TargetR := GetRValue(ColorToRGB(CorMagica));
  TargetG := GetGValue(ColorToRGB(CorMagica));
  TargetB := GetBValue(ColorToRGB(CorMagica));
  // Cria máscara de pixels a serem tratados
  SetLength(Mask, APng.Height, APng.Width);
  // Primeira passagem: identifica apenas pixels que são claramente da cor mágica
  for Y := 0 to APng.Height - 1 do
  begin
    Line := APng.Scanline[Y];
    for X := 0 to APng.Width - 1 do
    begin
      Mask[Y][X] := (Abs(Line^[X].rgbtRed - TargetR) < 15) and  // Tolerância mais estreita aqui
                   (Abs(Line^[X].rgbtGreen - TargetG) < 15) and
                   (Abs(Line^[X].rgbtBlue - TargetB) < 15);
    end;
  end;
  // Segunda passagem: dilatação controlada apenas para pixels vizinhos similares
  for Y := 1 to APng.Height - 2 do
  begin
    Line := APng.Scanline[Y];
    for X := 1 to APng.Width - 2 do
    begin
      if not Mask[Y][X] then
      begin
        VizinhoEncontrado := False;
        // Verifica vizinhos
        for I := -1 to 1 do
        begin
          for J := -1 to 1 do
          begin
            if Mask[Y+I][X+J] then
            begin
              // Só marca se o pixel atual estiver dentro da tolerância maior
              if (Abs(Line^[X].rgbtRed - TargetR) < Tolerancia) and
                 (Abs(Line^[X].rgbtGreen - TargetG) < Tolerancia) and
                 (Abs(Line^[X].rgbtBlue - TargetB) < Tolerancia) then
              begin
                VizinhoEncontrado := True;
                Break;
              end;
            end;
          end;
          if VizinhoEncontrado then Break;
        end;
        Mask[Y][X] := VizinhoEncontrado;
      end;
    end;
  end;
  // Terceira passagem: substitui apenas os pixels marcados
  APng.Canvas.Lock;
  try
    for Y := 0 to APng.Height - 1 do
    begin
      Line := APng.Scanline[Y];
      for X := 0 to APng.Width - 1 do
      begin
        if Mask[Y][X] then
        begin
          Line^[X].rgbtRed := 255;
          Line^[X].rgbtGreen := 255;
          Line^[X].rgbtBlue := 255;
        end;
      end;
    end;
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
        //RemoverResiduosComDilatacao(Png, COR_MAGICA, 100);
        //RemoverResiduosMagicos(Png, COR_MAGICA, 100);
        //if PngContemCorAlvo(Png, COR_MAGICA, 100) then
        //  PngSubstituirCorPorBranco(Png, COR_MAGICA, 100);

        //ExPngSubstituirTransparenciaPorBranco(Png, clWhite);

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

