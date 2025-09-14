# Task-06 (LyricLounge)

While working on this task, I created a Discord bot called LyricLounge. The bot connects with the Last.fm API and lrclib API to provide features like:

Searching and displaying track information (artist, album, duration, tags).

Fetching song lyrics.

Showing trending tracks.

Recommending songs based on genre or mood.

Managing a personal playlist (add, remove, view, clear).

A help command that lists all available commands.


##What I did

1. Set up environment

Installed required libraries (discord.py, aiohttp, python-dotenv).

Created a .env file to securely store my Discord token and API key.



2. Wrote the bot

Used the discord slash commands (/lyrics, /track, /playlist, etc.).

Learned how to use async/await with aiohttp for API requests.



3. Tested on Discord

Ran python main.py to start the bot.

Invited the bot to my server and checked each command (lyrics, playlist, trending, etc.).




What I understood / learned

How Discord bots are structured using discord.py.

The importance of environment variables (.env) for sensitive data.

Basics of making API calls and parsing JSON responses.

How to organize a project in fewer lines of code without losing readability.

Debugging errors and confirming that only the current version of main.py is what the bot runs.
