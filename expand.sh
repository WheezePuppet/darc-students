
if test $# -lt 1
then
    echo "Usage: expand.sh [.csvFile]."
else
    if ! test -e $1
    then
        echo "No such file: $1."
    else 
        sed -i 's/\<obi\>/Obi-wan/g' $1
        sed -i 's/\<dv\>/Vader/g' $1
        sed -i 's/\<tar\>/Tarkin/g' $1
        sed -i 's/\<luke\>/Luke/g' $1
        sed -i 's/\<leia\>/Leia/g' $1
        sed -i 's/\<han\>/Han/g' $1
        sed -i 's/\<3p\>/C3PO/g' $1
        sed -i 's/\<r2\>/R2D2/g' $1
        sed -i 's/\<chew\>/Chewie/g' $1
        sed -i 's/\<snag\>/Snaggletooth/g' $1
        sed -i 's/\<walrus\>/Walrusman/g' $1
        sed -i 's/\<biggs\>/Biggs/g' $1
        sed -i 's/\<mf\>/Falcon/g' $1
        sed -i 's/\<yav\>/Yavin/g' $1
        sed -i 's/\<mf\>/Falcon/g' $1
        sed -i 's/\<ds\>/Death Star/g' $1
        sed -i 's/\<tat\>/Tatooine/g' $1
        sed -i 's/\<mos\>/Mos Eisley/g' $1
    fi
fi
