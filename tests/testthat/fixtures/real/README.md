# Real tracker exports

Unmodified leading rows of files written by the tracking programs themselves,
kept small. Tests that use them check the loaders against what the programs
actually write, not against a hand-built table.

| File | Written by | Source | Licence |
|---|---|---|---|
| `idtrackerai_trajectories.csv` | idtracker.ai 6.0.0a0 (first 11 frames, 8 animals; missing positions are `nan`) | `data/idtrackerai/trajectories_csv/trajectories.csv` in <https://github.com/animovement/movement-data>, shared by Jordi Torrents (Champalimaud Foundation) | CC BY 4.0 |
| `trackmate_v7_spots.csv` | TrackMate 7 "all spots" CSV export (four header rows, then 6 spots) | `1-analyses/tissue-cuts/1-tracking/cONTROL3_allspots.csv` in <https://github.com/bruvellu/cephalic-furrow> (commit f425269), doi:10.5281/zenodo.7781947 | GPL-3.0 |
