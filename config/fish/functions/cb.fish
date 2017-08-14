# Defined in /var/folders/kw/qdk542sj7gn_1zr1nb57nyzm0000gn/T//fish.eG465L/cb.fish @ line 2
function cb
	find . \( -type d -name .git -o -name node_modules \) -prune -o  -type f \( -name "*.pyc" -o -name "*.pyo" -o -name "*~" -o -name ".*~" -o -name ".#*" -o -name "*.class" -o -name "*.qc" -o -name "*.qp" \) -print0 | xargs -0 rm -f
end
