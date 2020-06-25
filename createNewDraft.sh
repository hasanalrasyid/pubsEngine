#!/bin/sh
target=$1
if [ -z "$target" ]; then
  echo "Error: jalankan dengan cara"
  echo "./createNewDraft.sh target"
  exit
fi
mkdir -p $target
cd $target
backupHD="/home/aku/Documents/kerja/kanazawa/report/drafts/$target"
if [ -d "$backupHD" ]; then
  echo "Error: anda harus menghapus $backupHD secara manual"
  exit
fi

git init
cat > .gitignore << EOF
build
EOF
git add -A
git commit -m "Inisiasi $target"
cp -a ../$target $backupHD
cat >> $backupHD/.git/config << EOF
[receive]
  denyCurrentBranch = updateInstead
EOF

git remote add origin "$backupHD"
cat > .git/hooks/post-commit << EOF
#!/bin/bash
git push --all
EOF
chmod +x .git/hooks/post-commit
mkdir Figures
cat > Figures/README << EOF
Please put all additional files over here
EOF
cat > Figures/.gitignore << EOF
temp
EOF
ln -s ../../pubsEngine
ln -s pubsEngine/Makefile.forDraft Makefile
mkdir reported
cat > reported/README << EOF
Please put all finished files here
EOF
cat > manuscript.md << EOF
HIT ME
EOF
cat > manuscript.bib << EOF
@article{obata_improving_2015,
        title = {Improving the {Description} of {Nonmagnetic} and {Magnetic} {Molecular} {Crystals} via the van der {Waals} {Density} {Functional}},
        volume = {84},
        issn = {0031-9015},
        url = {http://journals.jps.jp/doi/abs/10.7566/JPSJ.84.024715},
        doi = {10.7566/JPSJ.84.024715},
        abstract = {We have derived and implemented a stress tensor formulation for the van der Waals density functional (vdW-DF) with spin-polarization-dependent gradient c
orrection (GC) recently proposed by the authors [J. Phys. Soc. Jpn. 82, 093701 (2013)] and applied it to nonmagnetic and magnetic molecular crystals under ambient condition.
 We found that the cell parameters of the molecular crystals obtained with vdW-DF show an overall improvement compared with those obtained using local density and generalize
d gradient approximations. In particular, the original vdW-DF with GC gives the equilibrium structural parameters of solid oxygen in the α-phase, which are in good agreement
 with the experiment.},
        number = {2},
        urldate = {2017-07-29},
        journal = {J. Phys. Soc. Jpn.},
        author = {Obata, Masao and Oda, Tatsuki and Nakamura, Makoto and Hamada, Ikutaro},
        month = jan,
        year = {2015},
        pages = {024715},
        file = {Full Text PDF:/home/aku/.zotero/zotero/cj95rjne.default/zotero/storage/8E9JFEFK/Obata et al. - 2015 - Improving the Description of Nonmagnetic and Magne.pdf:
application/pdf;Snapshot:/home/aku/.zotero/zotero/cj95rjne.default/zotero/storage/ZJNEANHK/JPSJ.84.html:text/html}
}
EOF

cat > localStyle.sty << EOF
\ProvidesPackage{localStyle}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\usepackage{xspace}

\newcommand{\defLocalStyle}{This is local style need to be copied per manuscript}
EOF

git add -A
git commit -m "Inisiasi $target"

