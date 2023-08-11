import datetime
import re


def ler_modelo_chamado(nome_arquivo: str) -> str:
    with open(nome_arquivo, 'r', encoding='utf-8') as arquivo:
        modelo = arquivo.read()

    return modelo


def preencher_valor_automatico(campo: str, tag: str) -> str:
    from datetime import datetime
    if tag == "%dateNow":
        return datetime.now().strftime('%Y-%m-%d')
    else:
        # Se não for uma tag especial, solicite a entrada do usuário
        return input(f"Digite o valor para {campo}: ")


def preencher_campos(conteudo_modelo: str) -> str:
    campos_tags = [match for match in re.findall(
        r'(\w+=%\w+|\w+=%s)', conteudo_modelo)]

    for campo_tag in campos_tags:
        campo, tag = campo_tag.split('=')
        valor = preencher_valor_automatico(campo, tag)
        conteudo_modelo = conteudo_modelo.replace(
            campo_tag, f"{campo}={valor}", 1)

    return conteudo_modelo


def salvar_arquivo_preenchido(conteudo_preenchido: str, nome_arquivo_destino: str):
    caminho_destino = f"C:\\Users\\flsantos\\OneDrive - Apdata do Brasil Software Ltda\\Chamados\\NewChamados\\{nome_arquivo_destino}.txt"
    with open(caminho_destino, 'w', encoding='utf-8') as arquivo:
        arquivo.write(conteudo_preenchido)


def main():
    # Ler o arquivo de modelo
    conteudo_modelo = ler_modelo_chamado(
        "C:\\Users\\flsantos\\OneDrive - Apdata do Brasil Software Ltda\\Chamados\\Modelo_Chamado_Base.txt")

    # Verificar se o conteúdo foi lido corretamente
    print("Conteúdo do Modelo:")
    print(conteudo_modelo)
    print("-------------------------------")

    # Preencher os campos do modelo
    conteudo_preenchido = preencher_campos(conteudo_modelo)

    # Verificar quais campos foram detectados
    print("Campos detectados:")
    campos = [match.split('=')[0]
              for match in re.findall(r'(\w+=%s)', conteudo_modelo)]
    print(campos)
    print("-------------------------------")

    # Salvar o arquivo preenchido no diretório especificado
    nome_arquivo_destino = input(
        "Digite o nome para o arquivo de saída (ex: chamado_preenchido.txt): ")
    salvar_arquivo_preenchido(conteudo_preenchido, nome_arquivo_destino)


if __name__ == "__main__":
    main()
