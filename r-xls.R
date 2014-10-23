## reading an Excel file into R (one of several ways)

# easiest way of all: open the spreadsheet using Excel, highlight
# the desired area, and copy it to the clipboard using Ctrl-c

bank <- read.delim("clipboard")
head(bank,5)

# also easy to do the reverse: writing an R table to Excel

T <- summary(bank)     # from earlier
T
write.table(T,"clipboard",sep="\t",col.names=NA)

# now Ctrl-c into an XLS spreadsheet - all done!


# alternatively, save bank.xls as bank.csv when in Excel 
# (CSV means "comma separated value", a text file format)

# can read from your hard drive, or straight off a website

BANK <- read.csv("http://www.stat.harvard.edu/Research/r/bank.csv")
head(BANK,5)

r

