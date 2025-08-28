unit unFunctions;

interface

uses
  System.Classes,
  System.Math,
  Vcl.Graphics,
  JPeg,
  cxGraphics


  ;

function ConvertAndResizeToJpeg(const ASource, ADest: TStream; const AMaxWidth, AMaxHeight: Integer; const AOnlyReduce: Boolean = False; const AQuality: Cardinal = 90): Double;

implementation

function ConvertAndResizeToJpeg;
var
  Pict: TPicture;
  FactorW, FactorH: Double;
  NewW, NewH: Integer;
  newBmp: TBitmap;
  oldBmp: TBitmap;
  jpg: TJPEGImage;
begin

  Pict := TPicture.Create;
  try
    Pict.LoadFromStream(ASource);
    FactorW := IfThen(AMaxWidth > 0, Pict.Width / AMaxWidth, 1.0);
    FactorH := IfThen(AMaxHeight > 0, Pict.Height / AMaxHeight, 1.0);
    Result := IfThen(FactorW > FactorH, FactorW, FactorH);
    if AOnlyReduce and (Result < 1.0) then
      Result := 1.0;
    NewW := Round(Pict.Width / Result);
    NewH := Round(Pict.Height / Result);
    newBmp := Vcl.Graphics.TBitmap.Create;
    oldBmp := Vcl.Graphics.TBitMap.Create;
    try
      oldBmp.PixelFormat := pf32bit;
      oldBmp.Transparent := True;
      oldBmp.Assign(Pict.Graphic);
      newBmp.PixelFormat := pf32bit;
      newBmp.Transparent := True;
      newBmp.SetSize(NewW, NewH);
      cxSmoothResizeBitmap(oldBmp, newBmp, True);
      jpg := TJPEGImage.Create;
      try
        jpg.CompressionQuality := AQuality;
        jpg.Scale := jsFullSize;
        jpg.Performance := jpBestQuality;
        jpg.PixelFormat := jf24Bit;
        jpg.ProgressiveEncoding := False;
        jpg.Smoothing := True;
        jpg.Assign(newBmp);
        jpg.Compress;
        ADest.Size := 0;
        jpg.SaveToStream(ADest);
      finally
        jpg.Free;
      end;
    finally
      newBmp.Free;
      oldBmp.Free;
    end;
  finally
    Pict.Free;
  end;
end;


end.
