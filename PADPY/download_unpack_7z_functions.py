import os.path
import tempfile
import urllib.request
import py7zlib

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