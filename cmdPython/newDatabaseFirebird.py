import os
import subprocess
import requests
import zipfile

def download_and_extract_database(directory, version):
    if version == '2':
        url = "https://www.firebirdsql.org/file/documentation/reference_manuals/fbdevgd-en/code/db_2_5.zip"
        zip_name = 'db_2_5.zip'
        db_name = 'TestDB_F25.fdb'
        extracted_dir = 'db_2_5'
    else:
        url = "https://www.firebirdsql.org/file/documentation/reference_manuals/fbdevgd-en/code/db_3_0.zip"
        zip_name = 'db_3_0.zip'
        db_name = 'TestDB_F30.fdb'
        extracted_dir = 'db_3_0'

    response = requests.get(url, stream=True)
    zip_path = os.path.join(directory, zip_name)
    
    with open(zip_path, 'wb') as zip_file:
        for chunk in response.iter_content(chunk_size=8192):
            zip_file.write(chunk)

    with zipfile.ZipFile(zip_path, 'r') as zip_ref:
        zip_ref.extractall(directory)

    os.remove(zip_path)
    
    # Rename and move the database file
    old_db_path = os.path.join(directory, extracted_dir, 'examples.fdb')
    new_db_path = os.path.join(directory, db_name)
    os.rename(old_db_path, new_db_path)
    os.rmdir(os.path.join(directory, extracted_dir))  # Remove the extracted directory

def main():
    # Default values
    default_directory = 'C:\github\\bases\\firebird'

    # Ask for user input
    directory = input(f"Please enter the directory for database creation or download (default: {default_directory}): ") or default_directory

    version_choice = '3'  # Default to version 3
    # Ask if the user wants to download the example database
    download_choice = input("Would you like to download an example database directly from the Firebird website? (y/n): ").lower()
    if download_choice == 'y':
        version_choice = input("Which version of Firebird would you like? (2 for version 2.5 or 3 for version 3.0): ").strip()
        while version_choice not in ['2', '3']:
            print("Invalid choice. Please enter 2 or 3.")
            version_choice = input("Which version of Firebird would you like? (2 for version 2.5 or 3 for version 3.0): ").strip()

        download_and_extract_database(directory, version_choice)
        user = 'SYSDBA'
        password = 'masterkey'
    else:
        default_user = 'SYSDBA'
        default_password = 'masterkey'
        
        user = input(f"Please enter the username for database creation (default: {default_user}): ") or default_user
        password = input(f"Please enter the password for database creation (default: {default_password}): ") or default_password
        
    db_path = os.path.join(directory, 'TestDB_F30.fdb' if version_choice == '3' else 'TestDB_F25.fdb')

    # Generate SQL scripts
    with open('script.sql', 'w') as script_file:
        script_file.write(f"CREATE DATABASE '{db_path}' page_size 8192 user '{user}' password '{password}';\n")
    
    with open('inserts.sql', 'w') as inserts_file:
        inserts_file.write("CREATE TABLE FEATURE_CRIADAS (ID INTEGER NOT NULL PRIMARY KEY, NAME_FEATURE VARCHAR(255), NAME_BRANCH VARCHAR(255), SHA_ORIGEM VARCHAR(255));\n")

    # Execute SQL scripts
    isql_path = "C:\\Program Files\\Firebird\\Firebird_3_0\\isql.exe"
    
    subprocess.run([isql_path, "-i", "script.sql", "-u", user, "-p", password])
    subprocess.run([isql_path, "-i", "inserts.sql", db_path, "-u", user, "-p", password])

    # Delete SQL files
    os.remove('script.sql')
    os.remove('inserts.sql')

if __name__ == "__main__":
    main()
