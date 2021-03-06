
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
        sed -i 's/\<DV\>/Vader/g' $1
        sed -i 's/\<tar\>/Tarkin/g' $1
        sed -i 's/\<luke\>/Luke/g' $1
        sed -i 's/\<leia\>/Leia/g' $1
        sed -i 's/\<han\>/Han/g' $1
        sed -i 's/\<3p\>/C3PO/g' $1
        sed -i 's/\<r2\>/R2D2/g' $1
        sed -i 's/\<ozzel\>/Ozzel/g' $1
        sed -i 's/\<piett\>/Piett/g' $1
        sed -i 's/\<veers\>/Veers/g' $1
        sed -i 's/\<needa\>/Needa/g' $1
        sed -i 's/\<chew\>/Chewie/g' $1
        sed -i 's/\<dack\>/Dack/g' $1
        sed -i 's/\<wedge\>/Wedge/g' $1
        sed -i 's/\<yoda\>/Yoda/g' $1
        sed -i 's/\<jab\>/Jabba/g' $1
        sed -i 's/\<bib\>/Bib Fortuna/g' $1
        sed -i 's/\<oola\>/Oola/g' $1
        sed -i 's/\<droidBoss\>/Droid Boss/g' $1
        sed -i 's/\<emp\>/Emperor/g' $1
        sed -i 's/\<lobot\>/Lobot/g' $1
        sed -i 's/\<snag\>/Snaggletooth/g' $1
        sed -i 's/\<walrus\>/Walrusman/g' $1
        sed -i 's/\<riek\>/Rieekan/g' $1
        sed -i 's/\<biggs\>/Biggs/g' $1
        sed -i 's/\<mf\>/Falcon/g' $1
        sed -i 's/\<bes\>/Bespin/g' $1
        sed -i 's/\<yav\>/Yavin/g' $1
        sed -i 's/\<dag\>/Dagobah/g' $1
        sed -i 's/\<hoth\>/Hoth/g' $1
        sed -i 's/\<mf\>/Falcon/g' $1
        sed -i 's/\<ds\>/Death Star/g' $1
        sed -i 's/\<DS\>/Death Star/g' $1
        sed -i 's/\<tat\>/Tatooine/g' $1
        sed -i 's/\<mos\>/Mos Eisley/g' $1
        sed -i 's/\<lando\>/Lando/g' $1
        sed -i 's/\<abity\>/Nien Nunb/g' $1
        sed -i 's/\<boba\>/Boba Fett/g' $1
        sed -i 's/\<palace\>/Jabbas Palace/g' $1
        sed -i 's/\<mothma\>/Mon Mothma/g' $1
        sed -i 's/\<madine\>/Madine/g' $1
        sed -i 's/\<ackbar\>/Ackbar/g' $1
        sed -i 's/\<ack\>/Ackbar/g' $1
        sed -i 's/\<wicket\>/Wicket/g' $1
        sed -i 's/\<endor\>/Endor/g' $1
    fi
fi
