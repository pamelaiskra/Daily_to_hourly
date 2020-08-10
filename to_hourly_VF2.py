import csv
import numpy as np
from numpy import array
import numbers
from collections import OrderedDict

def isfloat(value):
  try:
    float(value)
    return True
  except ValueError:
    return False

#Change name of file accordingly
titel=('22006')
file_to_analyse=('%s.csv' % titel)

with open(file_to_analyse) as CSVdatei:
    Datei = list(csv.reader(CSVdatei))

#Number of rows in the file
anzahl_der_zeilen = sum(1 for line in open(file_to_analyse))

#This is to determine the first day that has complete data (i.e. starts in ':15') so that erste_zeil is the first row analysed
if ':15' in Datei[21][0]:
    erste_zeil= 21
else:
    for h in range (0, 4):
        check_line= 21+h
        if ':15' in Datei[check_line][0]:
            erste_zeil= check_line

#This is to determine the last hour that has complete data (i.e. that has ':00:00') so that letzte_zeil is the last row analysed
if ':00:00' in Datei[anzahl_der_zeilen-1][0]:
    letzte_zeil= anzahl_der_zeilen-1
else:
    for h in range (1, 5):
        check_line= anzahl_der_zeilen-1-h   
        if ':00:00' in Datei[check_line][0]:
            letzte_zeil= check_line

#Subset to a matrix with all the data to be analysed (zeitfolgen), where first row is zeitfolgen[0,0] and last row is zeitfolgen[len(zeitfolgen)-1,0]
#print('First line is %i and reads %s' % (erste_zeil, Datei[erste_zeil][0]))
#print('Last line is %i and reads %s' % (letzte_zeil, Datei[letzte_zeil][0]))
#For some reason a "+1" needs to be added to consider all rows.
subset=np.array((Datei)[erste_zeil:letzte_zeil+1])
zeitfolgen=subset[:,0:2]
MyArray=np.array([])

for i in range (0, len(zeitfolgen)-2):
    if ':15' in zeitfolgen[i,0]:
        for j in range (0, 4):
            if isfloat(zeitfolgen[i+j,1])==True:
                fluss=float(zeitfolgen[i+j,1])/4
                if j==0:
                    flussStunde=fluss
                else:
                    flussStunde=flussStunde+fluss
            else:
                flussStunde='NaN'
                break
        Datum=zeitfolgen[i-1,0]
        DatumCols=Datum.replace('/', ' ')
        Fluss=flussStunde
        if i==0:
        #first row to be analysed (:15) corresponds to previous line from Datei
            Datum=Datei[erste_zeil-1][0]   
            DatumCols=Datum.replace('/', ' ')
            MyArray=np.hstack((MyArray, np.array([DatumCols,flussStunde])))
        else:
            MyArray=np.vstack((MyArray, np.array([DatumCols,flussStunde])))
        print(DatumCols)
        print(Fluss)

print(MyArray)

DateiName=('%s_to_HOURLY.csv' % titel)
np.savetxt(DateiName, MyArray, delimiter="\t", fmt="%s") 




  
