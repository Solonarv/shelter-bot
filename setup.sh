#!/bin/env bash

# Check dependencies (just stack, actually)
echo "Checking for 'stack'."
if ! [ -x "$(command -v stack)" ]; then
  echo "'stack' not found, aborting. Please install it and make sure it is on the path." >&2
  exit 1;
else
  echo "Found stack on your path. Checking version:"
  stack --version
fi

if [ -d local ]; then
  echo "Found local/ directory, all good."
else 
  echo "Didn't find local/ directory, creating it now."
  mkdir local
fi

tokenfile="local/token.txt"

echo "Checking for auth token file"
if [ -s tokenfile ]; then
  echo "Found auth token file."
else
  echo "Did not find auth token file, creating it now."
  echo "TOKEN" > "$tokenfile"
  echo "Please enter a valid oauth2 bot token in '$tokenfile' before launching the bot."
fi

cmdfile="local/commands.txt"
echo "Checking for command file"
if [ -f cmdfile ]; then
  echo "Found command file."
else
  echo "Did not find command file. Creating sample command file now."
  echo '!ping = pong!' > "$cmdfile"
fi

echo "Setup complete. Beginning build process. This may take a while."
echo "If the build crashes or stops for any other reason, simply start it again"
echo "by running this script. It will pick up roughly where it left off."

rm -f ./run

if stack build; then
  echo -e '#!/bin/env bash\nstack exec shelter-bot' > ./run
  chmod +x ./run
  echo "Build complete. You can start the bot by running 'stack exec shelter-bot',"
  echo "or by running the 'run' file."
else
  echo "Build failed or aborted. Try starting it again; if issues persist, contact the maintainer."
fi