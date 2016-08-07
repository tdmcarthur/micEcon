TFPIP Version 1.0
*****************

A TFP INDEX PROGRAM

by
Tim Coelli
Centre for Efficiency and Productivity Analysis
School of Economics
University of Queensland
Brisbane, QLD 4072
Australia.
email: t.coelli@economics.uq.edu.au
Web: http://www.uq.edu.au/economics/cepa/


GETTING STARTED:

1. Open the WINDOWS EXPLORER program on your PC.

2. Create a new folder on your hard drive, for example called TFPIP, by 
using the File/New/Folder option.

3. Copy ALL the files associated with TFPIP into this folder, using
the File/Copy option or by "drag-and-drop".

4. If the files are stored in a TFPIP.ZIP file, you will then need
to unzip (extract) the files by double-clicking on the zip file and 
extracting the contents of the zip file into the TFPIP folder.


FILES USED BY TFPIP

TFPIP uses three text files when it conducts an analysis.  These are:
	- a data file (eg. named eg1-dta.txt)
	- an instruction file (eg. named eg1-ins.txt)
	- an output file (eg. named eg1-out.txt)

All of these files are text files.  They can be edited using many programs.
For example, NOTEPAD, WORDPAD, WORD, WORD PERFECT, etc.  
We suggest you use WORDPAD to view and edit these files.


HOW TO RUN THE PROGRAM

To practice running TFPIP double-click on the TFPIP.EXE file name.  
The program will then ask for an instruction file name.  Type in 
	eg1-ins.txt 
(and hit the RETURN key).  TFPIP will only take a few seconds to complete
this small example.  To look at the output file (eg1-out.txt) you then
simply double-click on the eg1-out.txt file name.   


FILE NAMES IN DOS

TFPIP is a DOS computer program.  In DOS all file names must satisfy certain
restrictions:
	- no more than 12 characters
	- no more than 3 characters after the period (".")
	- no more than 8 characters before the period
That is, the largest possible file name has the form:
	XXXXXXXX.XXX
Since we use text files, the file name will always have "txt" after the 
period.  That is:
	XXXXXXXX.txt



DATA FILE STRUCTURE

All data, instruction and output files are text files.  In the data file
the output quantity columns are listed first followed by the input quantity
columns and then the output price columns and the input price columns 
(left to right across the file).  There should be no column names in the data
file, only numbers.


CREATING A NEW INSTRUCTION FILE

The easiest way to create a new instruction/command file is to open 
an existing instruction file (eg. eg1-ins.txt) using WORDPAD and then save
it under a new name (eg. abc2-ins.txt).  This is done by using the File/SaveAs
options in WORDPAD.  Then you can edit the contents of this new file to suit
your new analysis.


CREATING DATA FILES USING EXCEL

Many people store their data in Excel files.  To construct a text file from
an Excel file, open the file in Excel and then use the File/SaveAs option in 
Excel and save it as a text file.  However, be sure to remove any column names 
first - the data file should only contain numbers.


MANUAL

The manual for this program is not yet completed.  The methods used by the 
program are the Tornqvist and Fisher index number methods.  The program can
calculate both transitive and non-transitive index numbers.  The transitive index
numbers are obtained by applying the EKS transformation to the non-transitive 
index numbers.  For more on these index numbers see Chapters 4 and 5 in Coelli, 
Rao and Battese (1998), An Introduction to Efficiency and Productivity Analysis, 
Kluwer Academic Publishers, Boston.  Note that the example data listed in the
eg1-dta.txt file is taken from Chapter 4 in this book.


LIABILITY

Every effort has been made to ensure that this software is free of errors.  
However, the software is supplied without any warranty as to the 
performance or fitness of the program for any particular purpose.  All 
risk relating to the results and performance of this program is assumed by
the user.  CEPA shall not be liable for any damages resulting from the 
use of this product.



