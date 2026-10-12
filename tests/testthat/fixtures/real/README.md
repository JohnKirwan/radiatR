# Real tracker exports

Unmodified leading rows of files written by the tracking programs themselves,
kept small. Tests that use them check the loaders against what the programs
actually write, not against a hand-built table.

| File | Written by | Source | Licence |
|---|---|---|---|
| `idtrackerai_trajectories.csv` | idtracker.ai 6.0.0a0 (first 11 frames, 8 animals; missing positions are `nan`) | `data/idtrackerai/trajectories_csv/trajectories.csv` in <https://github.com/animovement/movement-data>, shared by Jordi Torrents (Champalimaud Foundation) | CC BY 4.0 |
| `trackmate_v7_spots.csv` | TrackMate 7 "all spots" CSV export (four header rows, then 6 spots) | `1-analyses/tissue-cuts/1-tracking/cONTROL3_allspots.csv` in <https://github.com/bruvellu/cephalic-furrow> (commit f425269), doi:10.5281/zenodo.7781947 | GPL-3.0 |
| `trackmate_v6_spots.csv` | TrackMate <= 6 "all spots" CSV (one header row): three spots in no track (`TRACK_ID` `None`) and the spots of tracks 1 and 100 | `Tutorials/example_tracks.csv` in <https://github.com/vanTeeffelenLab/ExTrack> | MIT |
| `trex_beetle.csv` | TRex (first 5 frames; `frame` appears twice in the header) | `data/trex/beetle.csv` in <https://github.com/animovement/movement-data> | CC BY 4.0 |
| `toxtrac_tracking.txt` | ToxTrac 2026 `Tracking.txt` layout, 11 columns, tab-separated: the rows printed as Table 3 of the manual, with the table's heading as the first line (the files ToxTrac writes have no header row; the tests also read it with that line removed) | Rodriguez, Molares-Ulloa, Andersson & Brodin (2026), *ToxTrac 2026 User and Technical Overview Manual*, <https://doi.org/10.5281/zenodo.20397164> | CC BY 4.0 |
| `toxtrac_tracking_realspace.txt` | ToxTrac 2026 `Tracking_RealSpace.txt`: the rows printed as Table 6 of the manual, tab-separated | as above | CC BY 4.0 |

The two ToxTrac files are transcribed from the manual's tables, not copied from
a recording. The loaders were also run against real ToxTrac output (the
`Tracking_0.txt` and `Tracking_RealSpace_1.txt` samples in
<https://github.com/AmanRathoreP/ToxTrac-Data-Analyser>, older 6-column layout),
which is not bundled because that repository states no licence.
