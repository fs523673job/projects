import webbrowser

def abrir_pagina_gitlab(numero_chamado: str):
    url_base = "https://gitlab.com/apdata/global-antares/-/merge_requests/new?merge_request%5Bsource_branch%5D=feature%2FPT_"
    url_completa = url_base + numero_chamado + "_MR"
    webbrowser.open(url_completa)

def main():
    numero_chamado = input("Digite o número do chamado: ")
    abrir_pagina_gitlab(numero_chamado)

if __name__ == "__main__":
    main()
