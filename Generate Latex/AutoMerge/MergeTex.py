import glob, os, re, subprocess

date = "February 9, 2018"

fileText = "\\documentclass{article}\n\\usepackage{subfig}\n\\usepackage{graphicx}\n\\usepackage{multirow}\n\\usepackage{float}\n\\usepackage[landscape, margin=0.5in, tmargin=0.5in, bmargin=0.5in]{geometry}\n\\begin{document}"
fileText += "\n\\begin{center}{\LARGE Elite Law Analysis}\n\\\\\n\\begin{tabular}{rl}\\\\Professor Joseph Grundfest, Professor Laurie Hodrick, Noam Habot \\\\"+date+"\\end{tabular}\\end{center}"


filesWritten = []


def regressionsTable(f):
    # read the file
    thisFile = open(f, "r")

    text = thisFile.read()
    thisFile.close()

    # get rid of the lines with tStat...
    text = re.sub('tStat (.*?)&', r' &', text)
    text = re.sub('LawyersLog', r'log(Lawyers)', text)
    text = re.sub('Lawyers2', r'Lawyers^2', text)
    #text = re.sub('MnARevenue', r'MnANumDeals', text)

    # extract the title then delete that line
    title = re.search('\\caption\{(.*?)\}', text).group(1)
    text = re.sub('\\\\caption\{(.*?)\}', r'', text)

    # replace the header for each table
    replaceHeader = """
\\begin{table}[H]\n\\centering\n\\begin{tabular}{|clllllllll|}
\hline
\multirow{3}{*}{Coefficients} & \multicolumn{9}{c|}{\\textbf{"""+ title +"""}} \\\\
\cline{2-10}
& \multicolumn{4}{c}{FirmFE} & \multicolumn{4}{c}{NoFirmFE} & \multirow{2}{*}{Lawyers} \\\\
\cline{2-9}
& FE3 & FE1 & FEYear & NoFE & FE3 & FE1 & FEYear & NoFE &  \\\\
\\hline
 """
    body = "\\hline".join(text.split("\\hline")[2:3])
    footer = '\\hline' + "\\hline".join(text.split("\\hline")[3:])

    # if this is the table without lawyers, put another & before each \\ (endline)
    # to draw the line on the right side of the table.
    if 'without Lawyers' in title:
        body = re.sub(' \\\\', r' & \\', body)


    body = re.sub('Regression Num', 'Regression \\#', body)
    body = re.sub('MaxVIF', 'Max VIF', body)
    body = re.sub('Observations', '\hline \n Observations', body)
    body = re.sub('R2', 'R^2', body)

    text = replaceHeader+body+footer

    return text



def addMethods(categoryName):
    global filesWritten

    addition = "\n{\\large \\textbf{"+categoryName+"} }\n"
    addition += "\\begin{figure}[H]\n\\centering\n\\begin{tabular}{cccc}\n"


    counter = 0
    for f in glob.glob(os.path.join(os.getcwd(), "../IndivTexOutput/*")):
        base = os.path.basename(f)
        if "methods" in base and base not in filesWritten:
            filesWritten.append(base)

            imagePath = "../IndivTexOutput/" + str(base)
            addition += "\\subfloat[]{\\includegraphics[width=0.42\\textwidth]{" + imagePath + "}}"
            if counter == 0 or counter == 2:
                addition += " &\n"
            elif counter == 1:
                addition += " \\\\\n"

            counter += 1


    addition += "\\end{tabular}\n\\end{figure}\n"

    if categoryName == "Model Selection":
        addition += "We have also generated this analysis when breaking up the data into 3 or more tiers, but they do not appear to show the differences as clearly as the two-tier models. We conjecture that if as we increase the number of tiers by which we factorize the data, there is a higher proportion of zeros in the resulting lowest tier. This causes hightened sensitivity in the signal of the data and deems the plot uninterpretable."

    addition += "\n\\newpage\n"
    return addition


def addCategory(category, categoryName):
    global filesWritten

    thisText = "{\\large \\textbf{"+categoryName+"} }"
    for f in sorted(glob.glob(os.path.join(os.getcwd(), "../IndivTexOutput/*"))):
        base = os.path.basename(f)
        if category in base and base not in filesWritten:
            print base
            filesWritten.append(base)
            if ".tex" in base:

                # we want to handle the regression table files differently
                if "regressions" in base:
                    thisText += regressionsTable(f) + '\n'
                    thisText = thisText.replace("MnA", "M\&A")
                    continue


                if category == "MnAGDP":
                    thisFile = open(f)
                    text = thisFile.read()
                    thisFile.close()
                    title = "Correlations with AggMnA and GDP (by Rank)"
                    # replace the header for the table
                    replaceHeader = """
                    \\begin{table}[H]\n\\centering\n\\begin{tabular}{ccccccccccccc}

                    \multirow{2}{*}{} & \multicolumn{12}{c}{\\textbf{"""+ title +"""}} \\\\
                    \\hline
                    & \multicolumn{2}{c}{Gross Rev} & \multicolumn{2}{c}{Gross Rev/Lawyer} & \multicolumn{2}{c}{Gross Rev/Eq Partner} & \multicolumn{2}{c}{NOI}& \multicolumn{2}{c}{NOI/Lawyer}& \multicolumn{2}{c}{NOI/Eq Partner}\\\\
                    \cline{2-13}
                    & Agg MnA & GDP & Agg MnA & GDP & Agg MnA & GDP & Agg MnA & GDP & Agg MnA & GDP & Agg MnA & GDP\\\\
                    \\hline
                     """


                    body = "\\hline".join(text.split("\\hline")[2:3])
                    footer = '\\hline' + "\\hline".join(text.split("\\hline")[3:])
                    text = replaceHeader+body+footer
                    text = text.replace("MnA", "M\&A")
                    thisText += text
                    continue

                with open(f) as file:
                    for line in file:
                        line = line.replace("\\begin{table}[ht]", "\\begin{table}[H]")
                        line = line.replace("MnA", "M\&A")
                        line = line.replace("Lawyers2", "Lawyers^2")
                        thisText += line



            elif ".jpg" in base:

                imagePath = "../IndivTexOutput/" + str(base)
                thisText += "\n\\begin{figure}[H]\\centering\n\\includegraphics[width=0.85\\textwidth]{"+imagePath+"}\\end{figure}\n"



    thisText += "\n\\newpage\n"
    return thisText



# first add the summary statistics
fileText += addCategory("summary", "Summary Statistics")
# # now add correlation tables
fileText += addCategory("cor", "Correlations")

# # now add the correlations with AggMnA and GDP
fileText += addCategory("MnAGDP", "Correlations with AggM\&A and GDP")

# # now add all the regression files:
fileText += addCategory("regressions", "Regressions")

# # now add all the regression Performance files:
fileText += addCategory("performance", "Regression Performance")

# # now add all the regression Performance files:
fileText += addCategory("pvaltable", "P-Value Summary")

# # now add all the files for model averaging:
fileText += addCategory("ModelAveraging", "Model Averaging")

# now write the rest of the files that we might have forgotten:
fileText += addCategory("Breakpoints", "Breakpoint Analysis")

# now add a square of GrossRev, NOI x lasso, lm
fileText += addMethods("Model Selection")

# now write the rest of the files that we might have forgotten:
fileText += addCategory("", "More Plots")



fileText += "\n\\end{document}"

with open("AutoMerged.tex", 'w') as file:
    file.write(fileText)

#os.system("pdflatex -interaction=nonstopmode AutoMerged.tex")
subprocess.call(["pdflatex", "-interaction=nonstopmode", "AutoMerged.tex"], stdout=open(os.devnull, 'wb'))
subprocess.call(["rm", "AutoMerged.aux"], stdout=open(os.devnull, 'wb'))
subprocess.call(["rm", "AutoMerged.log"], stdout=open(os.devnull, 'wb'))
subprocess.call(["mv", "AutoMerged.pdf", "SlideDeck2016.pdf"], stdout=open(os.devnull, 'wb'))
