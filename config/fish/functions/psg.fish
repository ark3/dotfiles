# Defined in /var/folders/kw/qdk542sj7gn_1zr1nb57nyzm0000gn/T//fish.VZIDFh/psg.fish @ line 1
function psg --description 'pgrep followed by useful ps'
	pgrep -u $USER $argv | xargs ps -o pid,stat,time,%cpu,%mem,command -p
end
