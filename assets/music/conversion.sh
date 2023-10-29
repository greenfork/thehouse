ffmpeg -t '1:45' -i source.ogg -af afade=in:0:d=5,afade=out:st=100:d=5 dream_2.ogg
