#!bin/bash
    
	FILES="../DataFiles/Cleaned2/patent/*"
    for f in $FILES
    do
        mongoimport -d sotonproject -c patents --type csv --file "$f" --headerline
    done
	
	FILES="../DataFiles/Cleaned2/citation/*"
	for f in $FILES
    do
        mongoimport -d sotonproject -c citations --type csv --file "$f" --headerline
    done
	
	
