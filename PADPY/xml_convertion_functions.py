import xml.etree.ElementTree as ET
import numpy as np
import pandas as pd

# zmienna reprezentujaca katalog bazowy
ROOT_PATH = '../serwisy_xml/'

def get_xml(path):
    """
    Funkcja, ktora pobiera plik xml z podanej sciezki
    
    path - sciezka do pliku xml
    """
    tree = ET.parse(ROOT_PATH + path)
    return tree.getroot()

def get_dataframe_from_xml(root):
    """
    Funkcja przerabiajaca kod xml na ramke danych
    
    root - 'korzen' kodu xml
    """
    row_counter = 0
    dict = {}
    for row in root:
        unused_keys = list(dict)
        for key, value in row.attrib.items():
            if key in dict:
                dict[key].append(value)
                unused_keys.remove(key)
            else:
                column = [None] * row_counter
                column.append(value)
                dict[key] = column
        for key in unused_keys:
            dict[key].append(None)
        row_counter += 1
    df = pd.DataFrame.from_dict(dict, orient='index').transpose()
        
    return df

def get_dataframe_from_xml_file(path):
    """
    Funkcja przerabiajaca plik xml na ramke danych
    
    path - sciezka do pliku xml
    """
    root = get_xml(path)
    return get_dataframe_from_xml(root)