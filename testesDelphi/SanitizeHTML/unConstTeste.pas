unit unConstTeste;

interface

const
  GENERAL_TESTE =
   '   <div> ' + sLineBreak +
   '   <u> ' + sLineBreak +
   '     <b> ' + sLineBreak +
   '       <span style="color:purple;font-family:''MS Sans Serif'';;">Descritivo</span> ' + sLineBreak +
   '     </b> ' + sLineBreak +
   '   </u> ' + sLineBreak +
   ' </div> ' + sLineBreak +
   ' <div>  ' + sLineBreak +
   '   <span style="color:navy;font-family:''MS Sans Serif'';;">Este cargo foi criado com objetivo de atender à demanda de atividades burocráticas das áreas administrativas da empresa, para que sejam as Chefias liberadas para atividades mais gerenciais</span> ' + sLineBreak +
   ' </div>' + sLineBreak +
   ' <div> ' + sLineBreak +
   '   <span style="color:navy;font-family:''MS Sans Serif'';;">do que operacionais.</span> ' + sLineBreak +
   ' </div> ' + sLineBreak +
   ' <div> ' + sLineBreak +
   '   <u> ' + sLineBreak +
   '     <b> ' + sLineBreak +
   '       <span style="color:purple;font-family:''MS Sans Serif'';;">Atividades</span> ' + sLineBreak +
   '     </b> ' + sLineBreak +
   '   </u> ' + sLineBreak +
   ' </div> ' + sLineBreak +
   ' <div> ' + sLineBreak +
   '   <span style="color:teal;font-family:''MS Sans Serif'';;">.Arquivar os documentos gerais, na frequência, local e forma, conforme norma estabelecida.</span> ' + sLineBreak +
   ' </div> ' + sLineBreak +
   ' <div> ' + sLineBreak +
   '   <span style="color:teal;font-family:''MS Sans Serif'';;">.Efetuar as cobranças de atividades das áreas correlatas</span> ' + sLineBreak +
   ' </div> ' + sLineBreak +
   ' <div> ' + sLineBreak +
   '   <span style="color:teal;font-family:''MS Sans Serif'';;">.Posicionar seu superior diariamente sobre as atividades pendentes</span> ' + sLineBreak +
   ' </div> ' + sLineBreak +
   ' <div> ' + sLineBreak +
   '   <span style="color:teal;font-family:''MS Sans Serif'';;">.Outras atividades, testes</span> ' + sLineBreak +
   ' </div> ' + sLineBreak +
   ' <div> ' + sLineBreak +
   '   <u> ' + sLineBreak +
   '     <b> ' + sLineBreak +
   '       <span style="color:olive;font-family:''MS Sans Serif'';;">Responsabilidades</span> ' + sLineBreak +
   '     </b> ' + sLineBreak +
   '   </u> ' + sLineBreak +
   ' </div> ' + sLineBreak +
   ' <div> ' + sLineBreak +
   '   <u> ' + sLineBreak +
   '     <b> ' + sLineBreak +
   '       <span style="color:olive;font-family:''Symbol'';;">· </span> ' + sLineBreak +
   '     </b> ' + sLineBreak +
   '   </u> ' + sLineBreak +
   '   <span style="color:maroon;font-family:''Comic Sans MS'';;">Responsável pelo cumprimento de seus prazos, conforme estabelece a norma em vigor</span> ' + sLineBreak +
   ' </div> ' + sLineBreak +
   ' <div> ' + sLineBreak +
   '   <span style="color:maroon;font-family:''Symbol'';;">· </span> ' + sLineBreak +
   '   <span style="color:maroon;font-family:''Comic Sans MS'';;">Responsável pela cobrança das atividades críticas relativas ao seu departamento, com relação a terceiros</span> ' + sLineBreak +
   ' </div> ' + sLineBreak +
   ' <div> ' + sLineBreak +
   '   <span style="color:maroon;font-family:''Symbol'';;">· </span> ' + sLineBreak +
   '   <span style="color:maroon;font-family:''Comic Sans MS'';;">Outras responsabilidades</span> ' + sLineBreak +
   ' </div> ' + sLineBreak +
   ' <div> ' + sLineBreak +
   '     <img src=x onerror=alert("XSS Teste" )> ' + sLineBreak +
   '     <img src="http://teste.teste.jpg" onerror=alert("XSS Teste" )> ' + sLineBreak +
   '     <img src=http://teste.teste.jpg onerror=alert("XSS Teste" )> ' + sLineBreak +
   '     <img src="https://teste.teste.jpg" onerror=alert("XSS Teste" )> ' + sLineBreak +
   '     <img src=https://teste.teste.jpg onerror=alert("XSS Teste" )> ' + sLineBreak +
   ' </div> ' + sLineBreak +
   ' <div> ' + sLineBreak +
   '   <span style="font-family:''Arial'';font-size:11.00pt;">&amp;lt;img src=x onerror=alert(&#39;teste&#39;)&amp;gt;</span> ' + sLineBreak +
   '   <span style="font-family:''Arial'';font-size:11.00pt;">&amp;lt;img src="http://teste.teste.jpg" onerror=alert(&#39;teste&#39;)&amp;gt;</span> ' + sLineBreak +
   '   <span style="font-family:''Arial'';font-size:11.00pt;">&amp;lt;img src=http://teste.teste.jpg onerror=alert(&#39;teste&#39;)&amp;gt;</span> ' + sLineBreak +
   '   <span style="font-family:''Arial'';font-size:11.00pt;">&amp;lt;img src="https://teste.teste.jpg" onerror=alert(&#39;teste&#39;)&amp;gt;</span> ' + sLineBreak +
   '   <span style="font-family:''Arial'';font-size:11.00pt;">&amp;lt;img src=https://teste.teste.jpg onerror=alert(&#39;teste&#39;)&amp;gt;</span> ' + sLineBreak +
   ' </div> ' + sLineBreak +
   '        ' + sLineBreak +
   ' ï»¿<img src=x onerror=alert(document.location) ' + sLineBreak +
   ' ï»¿<img src=x onerror="alert(document.location)" ' + sLineBreak +
   ' ï»¿<img src=x onerror="alert(document.location)" ' + sLineBreak +
   ' ï»¿&lt;img src=x onerror=alert(document.location) ' + sLineBreak +
   ' ï»¿<ï¼œï¹¤<img src=x onerror=alert(document.location>> ' + sLineBreak +
   ' ï»¿<<img src=x onerror="alert(document.location) ' + sLineBreak +
   ' ï»¿ï¹¤img src=x onerror="alert(document.location)" ' + sLineBreak +
   ' ï»¿aaa ' + sLineBreak +
   ' ï»¿<ï¼œï¹¤<img src=x onerror=alert(document.location)>> ' + sLineBreak +
   ' ï»¿<d<img src=x onerror=alert(document.location)>> ' + sLineBreak +
   ' ï»¿<<img src=x onerror=alert(document.location)>> ' + sLineBreak +
   ' ï»¿<img src=x onerror=alert(document.location)>> ' + sLineBreak +
   ' ï»¿<<img src=x onerror=alert(document.location)>> ' + sLineBreak +
   ' ï»¿<<img src=x onerror=alert(document.location)>';

  GENERAL_TEST_CONTENT_01 = 'javascript:alert(document.domain);/*" onload="alert(document.domain)';
  GENERAL_TEST_CONTENT_02 = 'teste" onmouseover="alert(1)" a ';
  GENERAL_TEST_CONTENT_03 = 'testeXPTO" sTyLe="width:1px; filter:glow" onfilterchange="javascript:alert(1)" onmouseover="alert(2)" onload="alert(3)"  autofocus onfocus="alert(4)" ';

implementation

end.
