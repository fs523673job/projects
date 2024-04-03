import json
import os

def ler_conteudo_arquivo(caminho_base, caminho_arquivo):
    caminho_completo = os.path.join(caminho_base, caminho_arquivo)
    try:
        with open(caminho_completo, 'r', encoding='utf-8') as arquivo:
            return arquivo.read()
    except Exception as e:
        print(f"Erro ao ler o arquivo {caminho_completo}: {e}")
        return None

def modificar_arquivo(caminho_base, caminho, arquivo_trecho_original, arquivo_trecho_modificado):
    trecho_original = ler_conteudo_arquivo(caminho_base, arquivo_trecho_original)
    trecho_modificado = ler_conteudo_arquivo(caminho_base, arquivo_trecho_modificado)
    if trecho_original is None or trecho_modificado is None:
        return

    if not os.path.isfile(caminho):
        print(f"O arquivo {caminho} não foi encontrado.")
        return

    try:
        with open(caminho, 'r', encoding='utf-8') as arquivo:
            conteudo = arquivo.read()
    except Exception as e:
        print(f"Erro ao ler o arquivo {caminho}: {e}")
        return

    if trecho_original not in conteudo:
        print("O trecho original não foi encontrado no arquivo:", caminho)
        return

    conteudo_modificado = conteudo.replace(trecho_original, trecho_modificado)

    try:
        with open(caminho, 'w', encoding='utf-8') as arquivo:
            arquivo.write(conteudo_modificado)
        print(f"Arquivo modificado com sucesso: {caminho}")
    except Exception as e:
        print(f"Erro ao escrever no arquivo {caminho}: {e}")

def carregar_configuracoes(caminho_base, arquivo_config):
    caminho_completo = os.path.join(caminho_base, arquivo_config)
    try:
        with open(caminho_completo, 'r', encoding='utf-8') as arquivo:
            return json.load(arquivo)
    except Exception as e:
        print(f"Erro ao carregar o arquivo de configuração {caminho_completo}: {e}")
        return None

# Define o caminho base para os arquivos de configuração e trechos de código
caminho_base = 'C:\\github\\fs523673job\\projects\\cmdPython\\auxiliar'

arquivo_config = 'updateAlterFontsForTests.json'
configuracoes = carregar_configuracoes(caminho_base, arquivo_config)

if configuracoes:
    for config in configuracoes:
        modificar_arquivo(caminho_base, config['caminho'], config['arquivo_trecho_original'], config['arquivo_trecho_modificado'])
