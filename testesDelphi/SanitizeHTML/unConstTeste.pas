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
   '   <span style="color:navy;font-family:''MS Sans Serif'';;">Este cargo foi criado com objetivo de atender � demanda de atividades burocr�ticas das �reas administrativas da empresa, para que sejam as Chefias liberadas para atividades mais gerenciais</span> ' + sLineBreak +
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
   '   <span style="color:teal;font-family:''MS Sans Serif'';;">.Arquivar os documentos gerais, na frequ�ncia, local e forma, conforme norma estabelecida.</span> ' + sLineBreak +
   ' </div> ' + sLineBreak +
   ' <div> ' + sLineBreak +
   '   <span style="color:teal;font-family:''MS Sans Serif'';;">.Efetuar as cobran�as de atividades das �reas correlatas</span> ' + sLineBreak +
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
   '       <span style="color:olive;font-family:''Symbol'';;">� </span> ' + sLineBreak +
   '     </b> ' + sLineBreak +
   '   </u> ' + sLineBreak +
   '   <span style="color:maroon;font-family:''Comic Sans MS'';;">Respons�vel pelo cumprimento de seus prazos, conforme estabelece a norma em vigor</span> ' + sLineBreak +
   ' </div> ' + sLineBreak +
   ' <div> ' + sLineBreak +
   '   <span style="color:maroon;font-family:''Symbol'';;">� </span> ' + sLineBreak +
   '   <span style="color:maroon;font-family:''Comic Sans MS'';;">Respons�vel pela cobran�a das atividades cr�ticas relativas ao seu departamento, com rela��o a terceiros</span> ' + sLineBreak +
   ' </div> ' + sLineBreak +
   ' <div> ' + sLineBreak +
   '   <span style="color:maroon;font-family:''Symbol'';;">� </span> ' + sLineBreak +
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
   ' ﻿<img src=x onerror=alert(document.location) ' + sLineBreak +
   ' ﻿<img src=x onerror="alert(document.location)" ' + sLineBreak +
   ' ﻿<img src=x onerror="alert(document.location)" ' + sLineBreak +
   ' ﻿&lt;img src=x onerror=alert(document.location) ' + sLineBreak +
   ' ﻿<＜﹤<img src=x onerror=alert(document.location>> ' + sLineBreak +
   ' ﻿<<img src=x onerror="alert(document.location) ' + sLineBreak +
   ' ﻿﹤img src=x onerror="alert(document.location)" ' + sLineBreak +
   ' ﻿aaa ' + sLineBreak +
   ' ﻿<＜﹤<img src=x onerror=alert(document.location)>> ' + sLineBreak +
   ' ﻿<d<img src=x onerror=alert(document.location)>> ' + sLineBreak +
   ' ﻿<<img src=x onerror=alert(document.location)>> ' + sLineBreak +
   ' ﻿<img src=x onerror=alert(document.location)>> ' + sLineBreak +
   ' ﻿<<img src=x onerror=alert(document.location)>> ' + sLineBreak +
   ' ﻿<<img src=x onerror=alert(document.location)>';

  GENERAL_TEST_CONTENT_01 = 'javascript:alert(document.domain);/*" onload="alert(document.domain)';
  GENERAL_TEST_CONTENT_02 = 'teste" onmouseover="alert(1)" a ';
  GENERAL_TEST_CONTENT_03 = 'testeXPTO" sTyLe="width:1px; filter:glow" onfilterchange="javascript:alert(1)" onmouseover="alert(2)" onload="alert(3)"  autofocus onfocus="alert(4)" ';

implementation

end.
