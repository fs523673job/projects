unit unVerticalSignatureImageImp;

interface

uses
  unSignatureImage, unSignatureDTO, Vcl.Graphics, Vcl.Imaging.jpeg, System.Classes, Vcl.Imaging.pngimage, cxGraphics;

type
  TVerticalSignatureImage = class(TInterfacedObject, ISignatureImage)
  public
    function TreatSignatureImage(const ASignatureDTO: TSignatureDTO): TBitmap;
    procedure AdjustImageSize(const AImage, ANewImage: TJPEGImage; const AFieldHeight, AFieldWidth: Integer; const AHeighPercent, AWidthPercent: Double);
  end;

implementation

uses
  System.Math, unFunctions
  ;


{ TVerticalSignatureAndStamp }

procedure TVerticalSignatureImage.AdjustImageSize(const AImage, ANewImage: TJPEGImage;
  const AFieldHeight, AFieldWidth: Integer; const AHeighPercent, AWidthPercent: Double);
var
  NewWidth, NewHeight, HPos, VPos, NewFieldHeight, NewFieldWidth: Integer;
  AdjustWidth, AdjustHeight, AdjustFactor: Extended;
  NewJPG, OldJPG: TMemoryStream;
  Bitmap: TBitmap;
begin
  NewFieldHeight := Round(AFieldHeight * AHeighPercent);
  NewFieldWidth := Round(AFieldWidth * AWidthPercent);
  // Nao precisa redimensionar
  if (AImage.Width = NewFieldWidth) and (AImage.Height = NewFieldHeight) then
  begin
    ANewImage.Assign(AImage);
    Exit;
  end;

  NewJPG := TMemoryStream.Create;
  try
    OldJPG := TMemoryStream.Create;
    try
      Bitmap := TBitmap.Create;
      try
        // Ajusta proporcionalmente o tamanho da imagem original para caber na
        // imagem destino verificando se a diferença de altura ou de largura deve
        // determinar o ajuste
        AdjustWidth := (AImage.Width / NewFieldWidth);
        AdjustHeight := (AImage.Height / NewFieldHeight);
        AdjustFactor := Max(AdjustWidth, AdjustHeight);
        NewWidth := Trunc((AImage.Width / AdjustFactor) + 0.99999);
        NewHeight := Trunc((AImage.Height / AdjustFactor) + 0.99999);

        AImage.SaveToStream(OldJPG);
        OldJPG.Position := 0;
        
        ConvertAndResizeToJpeg(OldJPG, NewJPG, NewWidth, NewHeight, False, 100);

        NewJPG.Position := 0;
        ANewImage.LoadFromStream(NewJPG);

        // Calcula a posicao onde a imagem devera ser posicionada na nova imagem
        HPos := Trunc(NewFieldWidth div 2) - Trunc((NewWidth div 2) + 0.99999);
        HPos := Max(0, HPos);
        VPos := NewFieldHeight - NewHeight + 1;

        // Cria uma nova imagem toda branca com o tamanho do campo e encaixa a
        // imagem original na nova imagem
        Bitmap.Canvas.Brush.Color := clWhite;
        Bitmap.Canvas.Brush.Style := bsSolid;
        Bitmap.SetSize(NewFieldWidth, NewFieldHeight);
        Bitmap.Canvas.CopyRect(Rect(0, VPos, HPos + NewWidth - 1, VPos + NewHeight - 1), ANewImage.Canvas, Rect(0, 0, NewWidth - 1, NewHeight - 1));

        ANewImage.Assign(Bitmap);
      finally
        Bitmap.Free;
      end;
    finally
      OldJPG.Free;
    end;
  finally
    NewJPG.Free;
  end;
end;

function TVerticalSignatureImage.TreatSignatureImage(const ASignatureDTO: TSignatureDTO): TBitmap;
var
  NewSignature: TJPEGImage;
  LeftPos: Integer;
begin
  Result := TBitmap.Create;
  NewSignature := TJPEGImage.Create;
  try
    AdjustImageSize(ASignatureDTO.SignatureImage, NewSignature, ASignatureDTO.FieldHeight, ASignatureDTO.FieldWidth, 1, 0.5);
    Result.SetSize(ASignatureDTO.FieldWidth, ASignatureDTO.FieldHeight);
    LeftPos := ASignatureDTO.FieldWidth - NewSignature.Width;
    Result.Canvas.CopyRect(Rect(LeftPos, 0, Result.Width, NewSignature.Height), NewSignature.Canvas, Rect(0, 0, NewSignature.Width, NewSignature.Height));
  finally
    NewSignature.Free;
  end;
end;

end.
