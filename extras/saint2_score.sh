### SAINT2 SCORE ###

TARGET=$1
MODEL=$2
CONTACT=NA      ## The contact component of the SAINT2 score
COMBINED=NA     ## The overall SAINT2 score

SAINT2=./        ## path to SAINT2 installation, if you have one

$SAINT2/bin/saint2 config_"$TARGET"* -- "$MODEL"  > "$MODEL"_scores
if [ -s "$MODEL"_scores ]
then
  CONTACT=`cat "$MODEL"_scores | awk '/^Saulo =/ { print $NF; }'`
  COMBINED=`cat "$MODEL"_scores | awk '/^Combined score =/ { print $NF; }'`
fi
echo $TARGET $MODEL $CONTACT $COMBINED 
