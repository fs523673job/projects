import os
import re
import uuid
from datetime import datetime
from pathlib import Path

PRE_NAME = "8001"

def detect_xml_encoding(raw: bytes, default: str = "utf-8") -> str:
    """
    Detecta encoding pelo cabeçalho XML (<?xml version="1.0" encoding="..."?>).
    Se não encontrar, retorna default (utf-8).
    """
    head = raw[:256]
    m = re.search(br'encoding="([^"]+)"', head, flags=re.IGNORECASE)
    if m:
        try:
            return m.group(1).decode("ascii")
        except Exception:
            return default
    return default

def xor8_bytes(data: bytes) -> bytes:
    """Aplica XOR 8 em cada byte (embaralhar/desembaralhar)."""
    return bytes(b ^ 8 for b in data)

def update_xml_datetime_to_now(xml_text: str) -> str:
    """
    Atualiza apenas o conteúdo de <Field_1 dataType="ap:dateTime">...</Field_1> para agora (UTC, Z).
    Não remove/insere tags, só troca o texto entre elas.
    """
    now_iso_z = datetime.utcnow().strftime("%Y-%m-%dT%H:%M:%SZ")

    # Função de substituição que preserva as bordas (grupo 1 = abertura, 3 = fechamento)
    def _repl(m):
        return f"{m.group(1)}{now_iso_z}{m.group(3)}"

    pattern = r'(<Field_1\s+[^>]*>)(.*?)(</Field_1>)'
    # DOTALL para atravessar quebras de linha; IGNORECASE para tolerar variações de caixa
    return re.sub(pattern, _repl, xml_text, flags=re.DOTALL | re.IGNORECASE)

def ask_yesno(prompt: str) -> bool:
    return input(f"{prompt} [yes/no]: ").strip().lower() == "yes"

def next_filename(output_dir: Path, encrypted_final: bool) -> Path:
    # Timestamp até milissegundos (mmm)
    ts = datetime.now().strftime("%Y%m%d%H%M%S%f")[:-3]
    guid = str(uuid.uuid4()).upper()
    suffix = "Encrypt" if encrypted_final else "Decrypt"
    name = f"{PRE_NAME}_{ts}_{guid}_{suffix}.off"
    return output_dir / name

def try_fix_field1_datetime(raw: bytes) -> bytes:
    """
    Recebe bytes já DESembaralhados (decrypt). Se conseguir decodificar como XML,
    atualiza o Field_1; senão, devolve os bytes intactos.
    """
    enc = detect_xml_encoding(raw, "utf-8")
    try:
        text = raw.decode(enc)  # sem errors="ignore"
    except UnicodeDecodeError:
        # Não conseguimos decodificar com o encoding declarado; não arriscar.
        return raw

    # Verifica presença explícita de Field_1 dateTime
    if not re.search(r'<Field_1\s+[^>]*dataType="ap:dateTime"', text, flags=re.IGNORECASE):
        return raw  # não alterar se o campo não existir

    new_text = update_xml_datetime_to_now(text)
    return new_text.encode(enc)

def main():
    src_path = Path(input("Informe o caminho do arquivo a ser duplicado: ").strip().strip('"'))
    if not src_path.is_file():
        print("Arquivo de origem não encontrado.")
        return

    out_dir_in = input("Informe a pasta de saída (vazio = mesma pasta do arquivo): ").strip().strip('"')
    output_dir = Path(out_dir_in) if out_dir_in else src_path.parent
    output_dir.mkdir(parents=True, exist_ok=True)

    do_decrypt = ask_yesno("Deseja desembaralhar (decrypt) o conteúdo?")
    do_encrypt_after = ask_yesno("Deseja encriptar (embaralhar) ANTES de salvar?")

    try:
        copies = int(input("Quantas cópias deseja criar? ").strip())
        if copies < 1:
            print("Quantidade inválida.")
            return
    except ValueError:
        print("Quantidade inválida.")
        return

    # Lê bytes
    data = src_path.read_bytes()

    # Se for para decryptar, aplica XOR 8
    if do_decrypt:
        data = xor8_bytes(data)
        data = try_fix_field1_datetime(data)
        # Tenta decodificar para texto e, se for XML com o campo Field_1, atualiza a data
        try:
            text = data.decode("utf-8", errors="ignore")
            if re.search(r'<Field_1\s+dataType="ap:dateTime">', text, flags=re.IGNORECASE):
                text = update_xml_datetime_to_now(text)
                data = text.encode("utf-8")
        except Exception:
            # Se não der para tratar como texto, segue em frente sem ajuste
            pass

    # Se for para encriptar antes de salvar, aplica XOR 8 no estado atual
    if do_encrypt_after:
        data = xor8_bytes(data)

    # Estado final dos bytes define o sufixo
    encrypted_final = do_encrypt_after or (not do_decrypt)

    # Gera as cópias
    for _ in range(copies):
        out_file = next_filename(output_dir, encrypted_final)
        out_file.write_bytes(data)
        print(f"Cópia criada: {out_file}")

if __name__ == "__main__":
    main()



