function diff2diff --description 'Turn diff -qr output into per-file diff commands'
	fgrep " differ" | sed sed -e "s/Files/diff -u/" -e "s/and //" -e "s/ differ//"
end
