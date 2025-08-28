import os
import re
import uuid
import time
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
    now_iso_z = datetime.now().strftime("%Y-%m-%dT%H:%M:%SZ")

    def _repl(m):
        return f"{m.group(1)}{now_iso_z}{m.group(3)}"

    pattern = r'(<Field_1\s+[^>]*dataType="ap:dateTime"[^>]*>)(.*?)(</Field_1>)'
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

    # Pergunta o intervalo entre as cópias (em segundos)
    delay_sec = 0.0
    delay_in = input("Quantos segundos entre cada cópia? (ex.: 1, 0.5; vazio = 0): ").strip().replace(",", ".")
    if delay_in:
        try:
            delay_sec = max(0.0, float(delay_in))
        except ValueError:
            print("Valor de segundos inválido. Usando 0.")
            delay_sec = 0.0

    # Lê bytes
    original_data = src_path.read_bytes()

    # Preparação: se for para decryptar, gera uma versão de trabalho descriptografada
    # e tenta decodificar como XML (com encoding detectado).
    decrypted_bytes = None
    xml_enc = None
    xml_text = None
    has_field1 = False

    if do_decrypt:
        decrypted_bytes = xor8_bytes(original_data)
        # tenta decodificar com encoding do cabeçalho, sem ignorar erros
        enc_guess = detect_xml_encoding(decrypted_bytes, "utf-8")
        try:
            xml_text_candidate = decrypted_bytes.decode(enc_guess)
            # detecta se existe Field_1 com dataType ap:dateTime
            if re.search(r'<Field_1\s+[^>]*dataType="ap:dateTime"', xml_text_candidate, flags=re.IGNORECASE):
                xml_text = xml_text_candidate
                xml_enc = enc_guess
                has_field1 = True
            else:
                # não é o XML esperado; usaremos bytes decrypted tal como estão
                pass
        except UnicodeDecodeError:
            # não decodifica; seguimos com bytes
            pass

    # Estado final do arquivo (Encrypt/Decrypt) no nome
    encrypted_final = do_encrypt_after or (not do_decrypt)

    # Loop de geração das cópias
    for i in range(copies):
        # Gera o conteúdo desta cópia:
        if do_decrypt:
            if has_field1 and xml_text is not None and xml_enc is not None:
                # Atualiza Field_1 para o "agora" desta cópia
                updated_text = update_xml_datetime_to_now(xml_text)
                data_for_copy = updated_text.encode(xml_enc)
            else:
                # Sem XML/Field_1 reconhecido; usa os bytes descriptografados tal como estão
                data_for_copy = decrypted_bytes
        else:
            # Não foi pedido decrypt; usa bytes originais
            data_for_copy = original_data

        # (Opcional) Re-encrypt antes de salvar
        if do_encrypt_after:
            data_for_copy = xor8_bytes(data_for_copy)

        # Salva com nome no padrão e extensão .off
        out_file = next_filename(output_dir, encrypted_final)
        out_file.write_bytes(data_for_copy)
        print(f"Cópia criada: {out_file}")

        # Aguarda o intervalo, exceto após a última cópia
        if i < copies - 1 and delay_sec > 0:
            time.sleep(delay_sec)

if __name__ == "__main__":
    main()




