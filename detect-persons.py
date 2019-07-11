import io
from pdfminer.converter import TextConverter
from pdfminer.pdfinterp import PDFPageInterpreter
from pdfminer.pdfinterp import PDFResourceManager
from pdfminer.layout import LAParams
from pdfminer.pdfpage import PDFPage
import spacy
import csv
from spacy import displacy
from collections import Counter
import en_core_web_sm
from io import BytesIO  
from io import StringIO 
import sys

filename = sys.argv[1]

##tokinizesetups
from spacy.tokenizer import Tokenizer
from spacy.lang.en import English
nlp = English()
tokenizer = Tokenizer(nlp.vocab)
tokenizer = nlp.Defaults.create_tokenizer(nlp)

##nltk stop word setups
import nltk
nltk.download('stopwords')
from nltk.corpus import stopwords

en_stops = set(stopwords.words('english'))
de_stops = set(stopwords.words('german'))

##Convert PDF file to Text 
def convert(fname, pages=None):
    if not pages:
        pagenums = set()
    else:
        pagenums = set(pages)
 
    output = StringIO()
    manager = PDFResourceManager()
    converter = TextConverter(manager, output, laparams=LAParams())
    interpreter = PDFPageInterpreter(manager, converter)
 
    infile = open(fname, 'rb')
    for page in PDFPage.get_pages(infile, pagenums):
        interpreter.process_page(page)
    infile.close()
    converter.close()
    text = output.getvalue()
    output.close
    return text

def tokenize(text):
    tokens = tokenizer(text)
    tokenedText = " "
    en_stops = set(stopwords.words('english'))
    de_stops = set(stopwords.words('german'))

    all_words = tokens
    for word in all_words: 
        if (word not in en_stops and word not in de_stops):
            tokenedText += word.text
            tokenedText +=' '
    ##print(tokenedText)
    return tokenedText 

def string_edit(strn):
    strn = strn.strip()
    strn = strn.replace('\n', ' ')
    strn = strn.replace('  ',' ')
    strn = strn.replace('#','')
    strn = strn.replace('!','')
    strn = strn.replace('"','')
    return strn

##Extract names from the text
def extract_names(text):
    
    nlp = en_core_web_sm.load()
    doc = nlp(text)
    allentities = doc.ents
    output = []
    
    for X in allentities: 
    
        if X.label_ == 'PERSON':
            
            output.append(string_edit(X.text))
           

    output = set(output)
    return output
 
if __name__ == '__main__':
    
    text = convert(filename)
    names = extract_names(tokenize(text))
    with open('persons-names.csv', 'w') as csvFile: 
        writer = csv.writer(csvFile)
        writer.writerow(names)

    csvFile.close()

    file_name= filename.split('.')[0]
    file1 = open(file_name + '.txt',"w") 
    file1.writelines(text)
    file1.close()
    sys.stdout.flush()
    print("jhvfnvifj")
    sys.stdout.flush()
    




