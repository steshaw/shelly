# From https://raw.githubusercontent.com/Cyber-Duck/fish-wakatime/master/fish-wakatime.sh

set -l project

if echo (pwd) | grep -qEi "$HOME/Code/.*/"
    set project (echo (pwd) | sed "s-$HOME/Code/.*/\\([^/]*\\).*-\\1-")
else
    set project "Terminal"
end

set -l entity
if true
    set entity "$TERM.fish"
else
    set entity (echo $history[1] | cut -d ' ' -f1)
end

echo $project:$entity >>/tmp/wakatime.fish.log

#wakatime --write --plugin "fish-wakatime/0.0.1" --entity-type app --project "$project" --entity (echo $history[1] | cut -d ' ' -f1) 2>&1 >> /tmp/wakatime.fish.log &
wakatime \
   --write \
   --plugin "fish-wakatime/0.0.2" \
   --project "$project" \
   --entity-type 'app' \
   --entity 'Fish' \
   --language 'fish' \
   --verbose \
   2>&1 >> /tmp/wakatime.fish.log &
