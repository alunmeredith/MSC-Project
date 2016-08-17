#!bin/bash
    
	FILES="../DataFiles/Cleaned2/patent/*"
    for f in $FILES
    do
        mongoimport -d sotonproject -c patents2 --type csv --file "$f" --headerline
    done
	
	FILES="../DataFiles/Cleaned2/citation/*"
	for f in $FILES
    do
        mongoimport -d sotonproject -c citations --type csv --file "$f" --headerline
    done
	
	FILES="../DataFiles/Cleaned/patent/*"
    for f in $FILES
    do
        echo "$f"
        mongoimport -d sotonproject -c patentsOld --type csv --file "$f" --headerline
    done
	
	FILES="../DataFiles/Cleaned/citation/*"
	for f in $FILES
    do
        mongoimport -d sotonproject -c citationsOld --type csv --file "$f" --headerline
    done
	
