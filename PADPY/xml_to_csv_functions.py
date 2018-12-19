import xml.etree.ElementTree as ET
import os
import csv
import glob

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
    Funkcja tworzy plik *.csv na podstawie podanego pliku *.xml 
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
    directory = os.path.join("..", ROOT_PATH, catalog)
    files_list = get_list_of_files_from_directory(directory, "xml")
    for file in files_list:
        f = get_elem_from_path(file, 3)
        name = get_name_of_file_without_extension(f)
        xml_to_csv_file(catalog, name)