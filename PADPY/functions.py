import os.path
import tempfile
import urllib.request
import py7zlib
import xml.etree.ElementTree as ET
import os
import csv
import glob
import sqlite3
import pandas as pd


def download_unpack_7z_to_catalog(url, catalog):
    """
    Funkcja pobiera plik *.7z z podanego adresu (url)
    i go rozpakowuje do podanego folderu (catalog)
    """
    path = os.path.join("..", "serwisy_xml", catalog)
    
    if not os.path.exists(path):
        os.makedirs(path)
    
    _, file = tempfile.mkstemp()
    urllib.request.urlretrieve(url, file)

    with open(file, 'rb') as f:
        archive = py7zlib.Archive7z(f)
        for name in archive.getnames():
            outfilename = os.path.join(path, name)
            outfile = open(outfilename, 'wb')
            outfile.write(archive.getmember(name).read())
            outfile.close()
    
    os.remove(file)


ROOT_PATH = 'serwisy_xml'
CSV_PATH = 'serwisy_csv'


def get_list_of_files_from_directory(directory, extension):
    """
    Funkcja pobiera liste plikow z podanej sciezki (directory)
    o podanym rozszerzeniu (extension)
    """
    files_list = []
    for file in glob.glob(os.path.join(directory, "*." + extension)):
        files_list.append(file)
    return files_list


def get_elem_from_path(path, index):
    return path.split("/")[index]


def get_name_of_file_without_extension(file):
    return file.split(".")[0]


def xml_to_csv_file(catalog, name):
    """
    Funkcja tworzy plik name.csv na podstawie podanego pliku name.xml 
    z podanego folderu (catalog)
    """
    xml_path = os.path.join("..", ROOT_PATH, catalog, name) + ".xml"
    csv_path = os.path.join("..", CSV_PATH, catalog, name) + ".csv"
    path_to_csv = os.path.join("..", CSV_PATH, catalog)
    
    if not os.path.exists(path_to_csv):
        os.makedirs(path_to_csv)

    tree = ET.parse(xml_path)
    root = tree.getroot()
        
    f = open(csv_path, 'w')
    csvwriter = csv.writer(f)
    
    head = []
    for row in root:
        for key in row.attrib:
            if key not in head:
                head.append(key)

    csvwriter.writerow(head)

    for row in root.findall('row'):
        _row = []
        for key in row.attrib:
            value = row.attrib.get(key)
            _row.append(value)
        csvwriter.writerow(_row) 
        
    f.close()


def convert_xml_files_to_csv_files(catalog):
    """
    Funkcja tworzy pliki *.csv na podstawie plikow *.xml 
    z podanego folderu (catalog)
    """
    directory = os.path.join("..", ROOT_PATH, catalog)
    files_list = get_list_of_files_from_directory(directory, "xml")
    for file in files_list:
        f = get_elem_from_path(file, 3)
        name = get_name_of_file_without_extension(f)
        xml_to_csv_file(catalog, name)


def read_csv_files_and_add_to_data_base(catalog, letter, connection):
    """
    Funkcja wczytuje ramki w formacie csv z podanego folderu (catalog)
    a następnie dodaje je do bazy danych (connection)
    
    letter - pomocnicza zmienna do rozróżniania nazw tabel
    """
    tables = {}
    directory = os.path.join("..", CSV_PATH, catalog)
    files_list = get_list_of_files_from_directory(directory, "csv")
    for file in files_list:
        f = get_elem_from_path(file, 3)
        name = get_name_of_file_without_extension(f)
        table_name = name + letter
        table = pd.read_csv(file, comment = "#")
        tables[table_name] = table
        table.to_sql(table_name, connection)
    return tables