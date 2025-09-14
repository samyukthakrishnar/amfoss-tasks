import discord, aiohttp, os
from discord.ext import commands
from dotenv import load_dotenv

load_dotenv()
TOKEN, KEY = os.getenv("DISCORD_BOT_TOKEN"), os.getenv("LASTFM_API_KEY")

intents = discord.Intents.default()
intents.message_content = True
bot = commands.Bot(command_prefix='/', intents=intents)
playlists = {}

class API:
    def __init__(self): self.sess=None
    async def get(self,url,**p):
        if not self.sess: self.sess=aiohttp.ClientSession()
        r=await self.sess.get(url,params=p); 
        return await r.json() if r.status==200 else {}
    async def lyrics(self,song,artist):
        d=await self.get("https://lrclib.net/api/search",track_name=song,artist_name=artist)
        return d[0].get("plainLyrics","Not found") if d else "Not found"
    async def call(self,method,**p):
        return await self.get("http://ws.audioscrobbler.com/2.0/",method=method,api_key=KEY,format="json",**p)
    async def track(self,song,artist):
        t=(await self.call("track.getInfo",artist=artist,track=song)).get("track")
        if not t: return
        return {"name":t.get("name"),"artist":t.get("artist",{}).get("name"),
                "album":t.get("album",{}).get("title"),"duration":t.get("duration"),
                "playcount":t.get("playcount"),
                "tags":[x["name"] for x in t.get("toptags",{}).get("tag",[])]}
    async def trending(self): return (await self.call("chart.gettoptracks",limit=10)).get("tracks",{}).get("track",[])
    async def rec(self,g): return (await self.call("tag.gettoptracks",tag=g,limit=10)).get("tracks",{}).get("track",[])
    async def close(self): 
        if self.sess: await self.sess.close()

api=API()
def fmt(tr): return "\n".join(f"{i+1}. {t.get('name')} - {t.get('artist',{}).get('name')}" for i,t in enumerate(tr[:10]))

@bot.event
async def on_ready():
    print("Ready:",bot.user); 
    try: await bot.tree.sync()
    except: pass

@bot.tree.command(name="lyrics")
async def _lyrics(i,song:str,artist:str):
    await i.response.defer()
    l=await api.lyrics(song,artist)
    if len(l)>4096:l=l[:4093]+"..."
    await i.followup.send(embed=discord.Embed(title=f"Lyrics: {song}",description=f"```{l}```"))

@bot.tree.command(name="track")
async def _track(i,song:str,artist:str):
    await i.response.defer()
    t=await api.track(song,artist)
    e=discord.Embed(title="Track Info")
    if t:
        for k,v in t.items():
            if k!="tags": e.add_field(name=k,value=v or "?",inline=True)
        if t["tags"]: e.add_field(name="Tags",value=", ".join(t["tags"][:5]))
    else: e.description="Not found"
    await i.followup.send(embed=e)

@bot.tree.command(name="trending")
async def _trend(i):
    await i.response.defer()
    tr=await api.trending()
    await i.followup.send(embed=discord.Embed(title="Trending",description=fmt(tr) if tr else "None"))

@bot.tree.command(name="recommend")
async def _rec(i,genre:str):
    await i.response.defer()
    tr=await api.rec(genre)
    await i.followup.send(embed=discord.Embed(title=f"{genre.title()} Recs",description=fmt(tr) if tr else "None"))

@bot.tree.command(name="playlist")
async def _pl(i,action:str,song:str=None):
    u=i.user.id; playlists.setdefault(u,[])
    p=playlists[u]; e=discord.Embed(title="Playlist")
    if action=="add" and song: p.append(song); e.description=f"Added {song}"
    elif action=="remove" and song: 
        if song in p: p.remove(song); e.description=f"Removed {song}"
        else: e.description="Not in list"
    elif action=="view": e.description="\n".join(p) if p else "Empty"
    elif action=="clear": p.clear(); e.description="Cleared"
    else: e.description="Use add/remove/view/clear"
    await i.response.send_message(embed=e)

@bot.tree.command(name="mood")
async def _mood(i,m:str):
    await i.response.defer()
    moods={"happy":"pop","sad":"melancholy","energetic":"electronic","relaxed":"ambient",
           "romantic":"love","angry":"metal","chill":"chillout","party":"dance"}
    tr=await api.rec(moods.get(m.lower(),m))
    await i.followup.send(embed=discord.Embed(title=f"{m.title()} Mood",description=fmt(tr) if tr else "None"))

@bot.tree.command(name="help")
async def _help(i):
    cmds=["/lyrics","/track","/playlist","/trending","/recommend","/mood","/help"]
    await i.response.send_message(embed=discord.Embed(title="Commands",description="\n".join(cmds)))

@bot.event
async def on_disconnect(): await api.close()

if __name__=="__main__":
    if not TOKEN or not KEY: exit("Missing env")
    bot.run(TOKEN)
