#!/bin/bash

#gnuplot -e "namefilenya='dos.all.m${1}t${1}.svg';batas=$1;judul='$2'" /home/aku/kanazawa/work/calcMic/templates/bin/pdos.all.glt 

#inkscape -z -e dos.all.m${1}t${1}.jpg -w 420 -h 610 dos.all.m${1}t${1}.svg 

echo "dosplotVertikal.sh xr         yr    over/parallel invStat   poskey       orbitalList      folder "
echo "dosplotVertikal.sh int:int   int    o/p            1/-1     top:left  Atom:Judul:3:4:5 folder{0.1.4}* "
echo "dosplotVertikal.sh $1         $2    $3              $4        $5                $6          $7"

# for k in {0..6};
# do for j in {d,p} ;
# do for i in {1..14}; 
# do echo $i $j $k;
#   namaf=$(ls|grep nico2o4invB.ferri.$k);
#   dosplotVertical.sh $namaf $i $j -10:2 2.5 1 "";
#   convert plots/hasil.jpg label:"$namaf.${j}.$(printf %02f $i)" -font Monofur +swap -gravity SouthWest -append tempor/${namaf}.${j}.$(printf %02f $i).jpg;
# done;
#done;
#done


#dirs=("$4"*)
dirs=("$@")
dirs=("${dirs[@]:7}")

for i in "${dirs[@]}"; do
  echo $i;
  echo ==============================================
done
rm -f temp.glt

poskey=$(echo $5 | sed -e 's/:/ /g')
total=$6

daftarOrbital=$7
mkdir -p plots
xr=$1 # xrange , dimasukkan sebagai "bawah:atas" mis "-10:6" 
xticsnya="-100,2,100"
yr=$2 # yrange, dimasukkan sebagai "yr" untuk mewakili "[-yr:yr]"
yticsnya=$(echo $yr | awk '{print "-"$1"+5,5,"$1"-5"}')
invStat=1
if [[ "$4" == "-1" ]]; then invStat="$4" ;fi

tailer="$(ls "${dirs[0]}" |grep dos.tot | awk -F '.' '{print $NF}' )"
over=$3
mainTitle=""
cat >> temp.glt << EOF
#!/home/aku/bin/gnuplot -persist
reset
set term post  portrait enhanced color font "Times-Roman"
set output "plots/hasil.eps"

if (!exists("MP_LEFT"))   MP_LEFT = .1
if (!exists("MP_RIGHT"))  MP_RIGHT = .91
if (!exists("MP_BOTTOM")) MP_BOTTOM = .1
if (!exists("MP_TOP"))    MP_TOP = .9
if (!exists("MP_GAP"))    MP_GAP = 0.0

set tmargin 0
set bmargin 0
set lmargin 8
set rmargin 6

set multiplot layout 5,2 columnsfirst title "{/:Bold=15 $mainTitle}" \
 margins screen MP_LEFT, MP_RIGHT, MP_BOTTOM, MP_TOP spacing screen MP_GAP

set border lw 0.2

set key $poskey font "Times New Roman Bold,8"
set key spacing 1
set key samplen 0
set xrange [$xr]
set yrange [-$yr:$yr]
set mxtics 2 
set mytics 2 
rydberg=13.605

unset grid

#set arrow from 0,-$yr to 0,$yr nohead lc rgb 'navy'

$(genLineType.hs)

f(x) = 0

set xzeroaxis lw 1 lt 1 lc rgb 'black'
set yzeroaxis lw 1 lt 1 lc rgb 'black'

set style data boxes
unset ylabel
unset xtics
set ytics $yticsnya font ",10" nomirror offset .7 
set xtics $xticsnya font ",10" nomirror offset 0,.6 
set format x ""
#############################################################################################
EOF

ender=$'

unset multiplot

#dikalikan 2 karena satuannya /cell eV

system "cd plots && epstopdf hasil.eps && pdftocairo -r 150 -singlefile -jpeg hasil.pdf tmp && convert tmp.jpg -rotate 0 hasil.png && rm -f tmp.jpg"
'

pre="plot "

akhiran="$ender"'
system "cd plots && rm -f tmp*jpg"
'

totalHeader="
###########Total Header#############
set label 'Total' at 2.5,15 font 'Bold,8'
set label 'spin-up' at 2.5,7.5 font ',8'
set label 'spin-down' at 2.5,-7.5 font ',8'

"

urutan=0
    plotplate="$plotplate ; set format x '% h'; set xtics format '' nomirror ; unset xlabel; unset ylabel "
    for foldernya in "${dirs[@]}"; do
      urutan=$((1+$urutan))

      topTitle=$(echo $foldernya|awk -F '.' '{print $NF}')
      if [ "$topTitle" != "0GGA" ]; then topTitle="$(echo $topTitle|sed -e 's/^.*G/QSGW_{/g' -e 's/$/}/g' )"; else topTitle="GGA(PBE)";fi
      #generator="f1.genPDOSAtomicOrbital.hs $topTitle $xr $yr $invStat $tailer $foldernya  Ni:Ni_{eg}:8:10 Ni:Ni_{t2g}:6:7:9 CoTd:CoTd_{eg}:8:10 CoTd:CoTd_{t2g}:6:7:9"
      generator="f1.genPDOSAtomicOrbitalTot.hs $topTitle $xr $yr $total $over $invStat $tailer $foldernya $daftarOrbital"
      echo $generator
      echo ====through------------------
#      plotplate="$plotplate
#      plot '$foldernya/dos.tot.$tailer' u (\$1*rydberg):(\$2*($invStat)/rydberg) w l lc rgb 'black' notitle "
#      plotplate="$plotplate ,\\
#      '$foldernya/dos.tot.$tailer' u (\$1*rydberg):(\$3*(-1)*($invStat)/rydberg) w l lc rgb 'black' title ''; plot $($generator)"
      plotplate="$plotplate 
      $($generator)"
      echo =============================
      echo $plotplate
      echo =============================
    done
    echo "$plotplate $akhiran" >> temp.glt
    echo ====done loop====
    gnuplot temp.glt
    #---#dimensi=$(convert plots/hasil.jpg -fuzz 5% -transparent white sparse-color:-|sed -e 's/ /\n/g'|awk -F ',' 'BEGIN{a=0; b=0;aa=10000;bb=10000}{if (a<$1) a=$1; if ($1<aa) aa=$1;  if (b<$2) b=$2; if (bb>$2) bb=$2 }END{print a-(10-a%10)"x"b-bb+(10-b%10)"+"aa-(30+aa%10)"+"bb-(10-aa%10)}')
    #---#convert plots/hasil.jpg -crop $dimensi plots/hasil.jpg
    #---##convert plots/hasil.jpg -pointsize 24 -font "monofur" label:'Energy (eV)' -gravity Center -append plots/hasil.jpg
    #---##convert plots/hasil.jpg -gravity West -font monofur -pointsize 24 -draw 'rotate -90 text 0,20 "DOS (states/eV)"' plots/hasil.jpg 
    exit
