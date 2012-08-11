guard :shell do
  watch /.*\.l?hs$/ do |m|
    `uhc -tjs -i 'lib/uhc-js/uhc-js/src;src' -o /dev/null src/App.hs && sh combine_dependencies.sh`
  end
end
