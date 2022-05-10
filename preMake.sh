if [ -z "$1" ]; then
  echo ./preMake.sh article
  exit
fi
dir=$1
for i in $(find templates/common|grep map$); do teckit_compile $i;done
rm -rf /tmp/extraTemplate
mkdir -p /tmp/extraTemplate
touch templates/$dir/extra.7z
for i in $(find templates/$dir/extra templates/common/fontmap/*.tec templates/common/files ! -type d); do echo $i; name=$(basename $i); echo $name;cat $i|7za a /tmp/extraTemplate/extra.7z -si${name} ;done
if [ "$(stat -c%s templates/$dir/extra.7z)" != "$(stat -c%s /tmp/extraTemplate/extra.7z)" ] ; then
  echo "new extra.7z detected"
  cp -f /tmp/extraTemplate/extra.7z templates/$dir/extra.7z
else
  echo "extra.7z not updated"
fi
