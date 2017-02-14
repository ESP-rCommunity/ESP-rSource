#!/bin/sh
echo "use: ./search_word.sh word"
echo "and it searches 8 levels of folders for all files which include"
echo "the character string and generates file which_word"
echo " "
WORD=$1
echo "starting search for "$WORD
grep -li $WORD * > which_$WORD
echo "starting.."
grep -li $WORD */* >> which_$WORD
echo "starting..."
grep -li $WORD */*/* >> which_$WORD
echo "starting...."
grep -li $WORD */*/*/* >> which_$WORD
echo "starting....."
grep -li $WORD */*/*/*/* >> which_$WORD
echo "starting......"
grep -li $WORD */*/*/*/*/* >> which_$WORD
echo "starting......."
grep -li $WORD */*/*/*/*/*/* >> which_$WORD
echo "starting........"
grep -li $WORD */*/*/*/*/*/*/* >> which_$WORD
echo "Done. Please look in which_"$WORD

