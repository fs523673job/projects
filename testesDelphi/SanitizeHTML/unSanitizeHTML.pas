unit unSanitizeHTML;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.RegularExpressions,
  System.NetEncoding,
  System.WideStrUtils,
  System.Character
  ;


type
  TPreventXSS = class
  private
    class function DecodeEntities(const AText: String): String;
    class function MatchEvaluator(const Match: TMatch): String;
    class function AdjustNBSPError(const AContent: String; const ADecharacterizesNBSP: Boolean = True): String; static;
  public
    class function IsProbablyHTML(const AContent: String): Boolean; overload; static;
    class function IsProbablyHTML(const AContentBytes: TBytes): Boolean; overload; static;
    class function NeedsDecoding(const AContentHTML: String): Boolean; static;
    class function SanitizeHTML(const AContentBytes: TBytes): TBytes; overload;
    class function SanitizeHTML(const AContentHTML: String): String; overload;
    class function SanitizeTag(const AContentHTML: String): String; overload;
    class function SanitizeTag(const AContentBytes: TBytes): TBytes; overload;
    class function SanitizeAll(const AContentHTML: String): String; overload;
    class function SanitizeAll(const AContentBytes: TBytes): TBytes; overload;
  end;


implementation

{ TPreventXSS }

class function TPreventXSS.SanitizeHTML(const AContentBytes: TBytes): TBytes;
var
  ContentString, SanitizedString: String;
  Encoding: TEncoding;
begin
  Encoding := TEncoding.UTF8;
  ContentString := Encoding.GetString(AContentBytes);
  SanitizedString := SanitizeHTML(ContentString);
  Exit(Encoding.GetBytes(SanitizedString));
end;

class function TPreventXSS.DecodeEntities(const AText: String): String;
var
  RegEx: TRegEx;
begin
  Result := AText;
  RegEx := TRegEx.Create('&#(x?)([0-9A-Fa-f]+);', [roIgnoreCase]);
  Result := RegEx.Replace(Result, MatchEvaluator);
end;

class function TPreventXSS.AdjustNBSPError(const AContent: String; const ADecharacterizesNBSP: Boolean = True): String;
begin
  if ADecharacterizesNBSP then
    Exit(AContent.Replace('&nbsp;', '#nbsp;', [rfIgnoreCase, rfReplaceAll]))
  else
    Exit(AContent.Replace('#nbsp;', '&nbsp;', [rfIgnoreCase, rfReplaceAll]));
end;

class function TPreventXSS.IsProbablyHTML(const AContent: String): Boolean;
var
  LocalContent: String;
begin
  LocalContent := TNetEncoding.HTML.Decode(TNetEncoding.HTML.Decode(TPreventXSS.AdjustNBSPError(AContent)));
  Exit(TRegEx.IsMatch(LocalContent, '<\/?[a-z][\s\S]*>|<\/?[a-z][\s\S]*|\/?[a-z][\s\S]*>', [roIgnoreCase]));
end;

class function TPreventXSS.IsProbablyHTML(const AContentBytes: TBytes): Boolean;
var
  ContentString, SanitizedString: String;
  Encoding: TEncoding;
begin
  Encoding := TEncoding.UTF8;
  ContentString := Encoding.GetString(AContentBytes);
  Exit(TPreventXSS.IsProbablyHTML(ContentString));
end;

class function TPreventXSS.MatchEvaluator(const Match: TMatch): String;
var
  IsHex: Boolean;
  CharCode: Integer;
begin
  IsHex := Match.Groups[1].Value = 'x';
  if IsHex then
    CharCode := StrToIntDef('$' + Match.Groups[2].Value, -1)
  else
    CharCode := StrToIntDef(Match.Groups[2].Value, -1);

  if (CharCode >= 0) and (CharCode <= $10FFFF) then
    Result := Char.ConvertFromUtf32(CharCode)
  else
    Result := Match.Value;
end;

class function TPreventXSS.NeedsDecoding(const AContentHTML: String): Boolean;
var
  DecodedContent: String;
begin
  DecodedContent := TNetEncoding.HTML.Decode(AContentHTML);
  Exit(not AContentHTML.Equals(DecodedContent));
end;

class function TPreventXSS.SanitizeHTML(const AContentHTML: String): String;
const
  SPECIAL_APDATA_BEGIN = '/*APDATABEGIN*/';
  SPECIAL_APDATA_END = '/*APDATAEND*/';
  AllowedTags: array[0..10] of string = ('b', 'i', 'u', 'p', 'br', 'img', 'div', 'font', 'span', 'a', 'strong');
  AllowedAttributes: array[0..4] of string = ('href', 'src', 'style', 'face', 'class');
var
  TagRegex, AttrRegex: TRegEx;
  Matches: TMatchCollection;
  TagMatch, AttrMatch: TMatch;
  Output, TagName, TagContent: string;
  IsEndTag: Boolean;
  i, LastIndex: Integer;
  AllowedTagList, AllowedAttrList: TDictionary<string, Boolean>;
  AttrName, AttrValue, AttrValueApdata: String;
  SanitizedOutput: TStringBuilder;
  IsSafeTag: Boolean;
  IsNeedEncoding: Boolean;

  function GetValueFromTagApdata(const AAttrValueApdata: String): String;
  var
    PosBegin, PosEnd: Integer;
  begin
    PosBegin := Pos(SPECIAL_APDATA_BEGIN, AAttrValueApdata);
    PosEnd := Pos(SPECIAL_APDATA_END, AAttrValueApdata);
    if (PosBegin > 0) and (PosEnd > 0) and (PosEnd > PosBegin) then
      Exit(TNetEncoding.HTML.Decode(Copy(AAttrValueApdata, PosBegin + Length(SPECIAL_APDATA_BEGIN), PosEnd - PosBegin - Length(SPECIAL_APDATA_BEGIN))).Replace('''', ''))
    else
      Exit(EmptyStr);
  end;

begin
  Output := AContentHTML;
  AllowedTagList := TDictionary<string, Boolean>.Create;
  AllowedAttrList := TDictionary<string, Boolean>.Create;
  SanitizedOutput := TStringBuilder.Create;
  try
    for i := Low(AllowedTags) to High(AllowedTags) do
      AllowedTagList.Add(AllowedTags[i], True);
    for i := Low(AllowedAttributes) to High(AllowedAttributes) do
      AllowedAttrList.Add(AllowedAttributes[i], True);
    TagRegex := TRegEx.Create('<[^>]*>', [roIgnoreCase]);
    Matches := TagRegex.Matches(Output);
    LastIndex := 1;
    for TagMatch in Matches do
    begin
      SanitizedOutput.Append(Copy(Output, LastIndex, TagMatch.Index - LastIndex));
      TagContent := TagMatch.Value;
      if TRegEx.IsMatch(TagContent, '^<\/\s*([a-z0-9]+)', [roIgnoreCase]) then
      begin
        IsEndTag := True;
        TagName := TRegEx.Match(TagContent, '^<\/\s*([a-z0-9]+)', [roIgnoreCase]).Groups[1].Value;
      end
      else if TRegEx.IsMatch(TagContent, '^<\s*([a-z0-9]+)', [roIgnoreCase]) then
      begin
        IsEndTag := False;
        TagName := TRegEx.Match(TagContent, '^<\s*([a-z0-9]+)', [roIgnoreCase]).Groups[1].Value;
      end
      else
        Continue;
      TagName := LowerCase(TagName);
      if AllowedTagList.ContainsKey(TagName) then
      begin
        IsSafeTag := True;
        if not IsEndTag then
        begin
          AttrRegex := TRegEx.Create('([a-z0-9\-]+)\s*=\s*(?:"([^"]*)"|''([^'']*)''|([^\s>]+))', [roIgnoreCase]);
          TagContent := '<' + TagName;
          for AttrMatch in AttrRegex.Matches(TagMatch.Value) do
          begin
            AttrName := LowerCase(AttrMatch.Groups[1].Value);
            if AttrMatch.Groups[2].Success then
              AttrValue := AttrMatch.Groups[2].Value
            else if AttrMatch.Groups[3].Success then
              AttrValue := AttrMatch.Groups[3].Value
            else if AttrMatch.Groups[4].Success then
              AttrValue := AttrMatch.Groups[4].Value
            else
              AttrValue := '';
            if AllowedAttrList.ContainsKey(AttrName) then
            begin
              if (AttrName = 'href') or (AttrName = 'src') then
              begin
                if (TRegEx.IsMatch(AttrValue, '(^http|https|mailto):', [roIgnoreCase])) then
                begin
                  TagContent := TagContent + Format(' %s="%s"', [AttrName, AttrValue]);
                end
                else if (Pos(SPECIAL_APDATA_BEGIN, AttrValue) > 0) and (Pos(SPECIAL_APDATA_END, AttrValue) > 0) then
                begin
                  AttrValueApdata := GetValueFromTagApdata(AttrValue);
                  if (TRegEx.IsMatch(AttrValueApdata, '(^http|https|mailto|www):|(^www).', [roIgnoreCase])) then
                    TagContent := TagContent + Format(' %s="%s"', [AttrName, AttrValue.Replace(SPECIAL_APDATA_BEGIN, '').Replace(SPECIAL_APDATA_END, '')]);
                end;
              end
              else
                TagContent := TagContent + Format(' %s="%s"', [AttrName, AttrValue]);
            end;
          end;
          TagContent := TagContent + '>';
          if IsSafeTag then
            SanitizedOutput.Append(TagContent);
        end
        else
        begin
          SanitizedOutput.Append('</' + TagName + '>');
        end;
      end;
      LastIndex := TagMatch.Index + TagMatch.Length;
    end;
    if LastIndex <= Length(Output) then
      SanitizedOutput.Append(Copy(Output, LastIndex, Length(Output) - LastIndex + 1));
    Exit(SanitizedOutput.ToString);
  finally
    AllowedTagList.Free;
    AllowedAttrList.Free;
    SanitizedOutput.Free;
  end;
end;

class function TPreventXSS.SanitizeTag(const AContentHTML: String): String;
const
  NotNeedEncoded: array[0..0] of string = ('&nbsp;');
var
  Input: String;
  Output: TStringBuilder;
  Regex: TRegEx;
  Matches: TMatchCollection;
  Match: TMatch;
  ProcessedContent: String;
  IsNeedEncoding: Boolean;
  c: Integer;
  NotNeedEncodedList: TDictionary<string, Boolean>;
begin
  Input := AContentHTML;
  Output := TStringBuilder.Create;
  try
    NotNeedEncodedList := TDictionary<string, Boolean>.Create;
    try
      for c := Low(NotNeedEncoded) to High(NotNeedEncoded) do
        NotNeedEncodedList.Add(NotNeedEncoded[c], True);

      Regex := TRegEx.Create('(<[^>]*>)|([^<>]+)', [roIgnoreCase]);
      Matches := Regex.Matches(Input);
      for Match in Matches do
      begin
        if (Match.Groups.Count - 1 = 1) and (Match.Groups[1].Success) then
          Output.Append(Match.Groups[1].Value);

        if (Match.Groups.Count > 2) and (Match.Groups[2].Success) then
        begin
          ProcessedContent := Match.Groups[2].Value;

          if (ProcessedContent.Trim <> '') and (not NotNeedEncodedList.ContainsKey(ProcessedContent)) then
          begin
            ProcessedContent :=  TPreventXSS.AdjustNBSPError(ProcessedContent);
            IsNeedEncoding := TPreventXSS.NeedsDecoding(ProcessedContent);

            if IsNeedEncoding then
              ProcessedContent := TNetEncoding.HTML.Decode(TNetEncoding.HTML.Decode(ProcessedContent));

            ProcessedContent := TPreventXSS.SanitizeHTML(ProcessedContent);

            if IsNeedEncoding then
              ProcessedContent := TNetEncoding.HTML.Encode(ProcessedContent);

            ProcessedContent :=  TPreventXSS.AdjustNBSPError(ProcessedContent, False);
          end;
          Output.Append(ProcessedContent);
        end;
      end;
      Result := Output.ToString;
    finally
      NotNeedEncodedList.Free;
    end;
  finally
    Output.Free;
  end;
end;

class function TPreventXSS.SanitizeTag(const AContentBytes: TBytes): TBytes;
var
  ContentString, SanitizedString: String;
  Encoding: TEncoding;
begin
  Encoding := TEncoding.UTF8;
  ContentString := Encoding.GetString(AContentBytes);
  SanitizedString := TPreventXSS.SanitizeTag(ContentString);
  Exit(Encoding.GetBytes(SanitizedString));
end;

class function TPreventXSS.SanitizeAll(const AContentHTML: String): String;
begin
  Exit(TPreventXSS.SanitizeHTML(TPreventXSS.SanitizeTag(AContentHTML)));
end;

class function TPreventXSS.SanitizeAll(const AContentBytes: TBytes): TBytes;
var
  ContentString, SanitizedString: String;
  Encoding: TEncoding;
begin
  Encoding := TEncoding.UTF8;
  ContentString := Encoding.GetString(AContentBytes);
  SanitizedString := TPreventXSS.SanitizeAll(ContentString);
  Exit(Encoding.GetBytes(SanitizedString));
end;

end.

{
Testes:
<div style=background-color:red onclick=alert('XSS')>
  Conteúdo perigoso
</div>
<img src=x onerror=alert("XSS Teste" )>
<a href="http://exemplo.com" href="javascript:alert('XSS')">Link</a>

<!--<script>Malicious code</script>-->
<p>Texto após comentário malicioso.</p>

<a href="&#106;&#97;&#118;&#97;&#115;&#99;&#114;&#105;&#112;&#116;&#58;&#97;&#108;&#101;&#114;&#116;&#40;&#39;XSS&#39;&#41;">Link Malicioso</a>

<foo:bar>Conteúdo com namespace desconhecido</foo:bar>

<a href="javascript:alert('XSS')">Clique aqui</a>

<script>alert('Este é um script malicioso');</script>

<form action="/submit" method="post">
  <label for="nome">Nome:</label>
  <input type="text" id="nome" name="nome"><br><br>
  <input type="submit" value="Enviar">
</form>

<table>
  <thead>
    <tr>
      <th>Nome</th>
      <th>Idade</th>
      <th>Cidade</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Ana</td>
      <td>28</td>
      <td>São Paulo</td>
    </tr>
    <tr>
      <td>Bruno</td>
      <td>35</td>
      <td>Rio de Janeiro</td>
    </tr>
  </tbody>
</table>

<img src="imagem.jpg" alt="Descrição da imagem">

<a href="http://exemplo.com" href="javascript:alert('XSS')">Link</a>

<div data-info="informação" data-test="testando">Conteúdo com data-attributes</div>

}
