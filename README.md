# dotfiles

Personal configuration files, managed with [dotbot](https://github.com/anishathalye/dotbot).

## New machine setup

```bash
# Clone via HTTPS (SSH isn't set up yet)
git clone https://github.com/ark3/dotfiles.git ~/dotfiles
cd ~/dotfiles

# Create symlinks
./install

# Set up GitHub SSH access
./setup-github-ssh <github-username>
```

After this, the dotfiles remote is updated to use SSH, and you can push/pull normally.

## Updating GitHub SSH config

To pick up the latest security settings from github-keygen, run the setup script again:

```bash
~/dotfiles/setup-github-ssh <github-username>
```
