# Real tracker exports

Unmodified leading rows of files written by the tracking programs themselves,
kept small. Tests that use them check the loaders against what the programs
actually write, not against a hand-built table.

| File | Written by | Source | Licence |
|---|---|---|---|
| `idtrackerai_trajectories.csv` | idtracker.ai 6.0.0a0 (first 11 frames, 8 animals; missing positions are `nan`) | `data/idtrackerai/trajectories_csv/trajectories.csv` in <https://github.com/animovement/movement-data>, shared by Jordi Torrents (Champalimaud Foundation) | CC BY 4.0 |
| `trackmate_v7_spots.csv` | TrackMate 7 "all spots" CSV export (four header rows, then 6 spots) | `1-analyses/tissue-cuts/1-tracking/cONTROL3_allspots.csv` in <https://github.com/bruvellu/cephalic-furrow> (commit f425269), doi:10.5281/zenodo.7781947 | GPL-3.0 |
| `trackmate_v6_spots.csv` | TrackMate <= 6 "all spots" CSV (one header row): three spots in no track (`TRACK_ID` `None`) and the spots of tracks 1 and 100 | `Tutorials/example_tracks.csv` in <https://github.com/vanTeeffelenLab/ExTrack> | MIT, Copyright (c) 2022 van Teeffelen Lab |
| `trex_beetle.csv` | TRex (first 5 frames; `frame` appears twice in the header) | `data/trex/beetle.csv` in <https://github.com/animovement/movement-data> | CC BY 4.0 |
| `toxtrac_tracking.txt` | ToxTrac 2026 `Tracking.txt` layout, 11 columns, tab-separated: the rows printed as Table 3 of the manual, with the table's heading as the first line (the files ToxTrac writes have no header row; the tests also read it with that line removed) | Rodriguez, Molares-Ulloa, Andersson & Brodin (2026), *ToxTrac 2026 User and Technical Overview Manual*, <https://doi.org/10.5281/zenodo.20397164> | CC BY 4.0 |
| `toxtrac_tracking_realspace.txt` | ToxTrac 2026 `Tracking_RealSpace.txt`: the rows printed as Table 6 of the manual, tab-separated | as above | CC BY 4.0 |
| `dlc_single_wasp_predictions.csv` | DeepLabCut predictions for one animal (three header rows; bodyparts `head` and `stinger`): the first 40 frames and the last 12, the middle cut. The last 9 frames have x = y = 0 and likelihood 0 | `poses/DLC_single-wasp.predictions.csv` in <https://gin.swc.ucl.ac.uk/neuroinformatics/movement-sample-data>, shared by Sanna Titus (Sainsbury Wellcome Centre, UCL); 40 fps | CC BY 4.0 |
| `dlc_two_mice_predictions.csv` | DeepLabCut multi-animal predictions (four header rows, two individuals, 12 bodyparts): the first 20 frames. Header rows end in CRLF, data rows in LF, as in the source | `poses/DLC_two-mice.predictions.csv` in the same repository, shared by Dimokratis Karamanlis (University of Geneva); 100 fps | CC BY 4.0 |
| `dlc_CollectedData_Pranav.csv` | DeepLabCut *labelled training data* (rows indexed by image, no likelihoods), kept as the case the `deeplabcut` dialect must refuse | `examples/openfield-Pranav-2018-10-30/labeled-data/m4s1/CollectedData_Pranav.csv` in <https://github.com/DeepLabCut/DeepLabCut> (commit 9a129d4946323799cf4328ec567f237ecde8c24d; (c) A. & M.W. Mathis Labs, see `inst/COPYRIGHTS`) | LGPL-3.0 |
| `sleap_three_nodes.analysis.csv` | SLEAP analysis CSV (`track`, `frame_idx`, `instance.score`, then `<node>.x` / `.y` / `.score` for `head`, `torso`, `tail_base`): the first 59 rows, two tracks. Nodes SLEAP did not find are blank with score 0.0 | `data/sleap/new_video.v002.000_mice_new.analysis.csv` in <https://github.com/animovement/movement-data>, from `new_data/results.zip` in <https://github.com/talmolab/cosyne-tutorial-data> (the SLEAP tutorial at Cosyne 2024) | BSD-3-Clause, Copyright (c) 2024, Talmo Lab at the Salk Institute |
| `sleap_three_mice.analysis.csv` | SLEAP analysis CSV layout, one node (`centroid`), three tracks, 20 frames, scores above 1. Written by sleap-io from a SLEAP `.h5`, not by SLEAP's own exporter | `data/sleap/SLEAP_three-mice_Aeon_mixed-labels.analysis.csv` in <https://github.com/animovement/movement-data>, derived from `poses/SLEAP_three-mice_Aeon_mixed-labels.analysis.h5` in <https://gin.swc.ucl.ac.uk/neuroinformatics/movement-sample-data>, shared by Chang Huan Lo (Sainsbury Wellcome Centre) | CC BY 4.0 |
| `ethovision_trial.txt` | EthoVision XT text export of one tracked termite (latin-1, CRLF, `;`-separated with a trailing `;`): the 33 header lines, then data rows 1 to 20 and 1600 to 1650 of the source, the rows between cut. 71 rows; the 31 in which the subject was not found (`"-"`, source rows 1607 to 1626 and 1634 to 1644) are every such row of the source | `data/ethovision/S22-Track-Cupim02SeptTarde-Trial.txt` in <https://github.com/animovement/movement-data> (commit `09a5fc3`), redistributed there from doi:10.5281/zenodo.3628061 by Julieth Castiblanco, Paulo Fellipe Cristaldo, Leticia Ribeiro Paiva and Og DeSouza | CC BY 4.0 |
| `ethovision_truncated.txt` | `ethovision_trial.txt` followed by the last line of the source file, which is cut off after three fields and has no line ending: the case the `ethovision` dialect must refuse | as above | CC BY 4.0 |

`sleap_three_nodes.analysis.csv` is redistributed under the BSD 3-Clause
licence of talmolab/cosyne-tutorial-data. `sleap_three_nodes.LICENSE.txt`
beside it is the notice, conditions and disclaimer as movement-data
distributes them with the source file (its "frames 0 to 199" describes that
file; the fixture is its first 59 rows).

The two ToxTrac files are transcribed from the manual's tables, not copied from
a recording. The loaders were also run against real ToxTrac output (the
`Tracking_0.txt` and `Tracking_RealSpace_1.txt` samples in
<https://github.com/AmanRathoreP/ToxTrac-Data-Analyser>, older 6-column layout),
which is not bundled because that repository states no licence.

The two EthoVision files are cut with `sed -n '1,53p;1633,1683p'` (and
`tail -n 1` for the cut-off line), so their bytes are the source's. Without
its cut-off last line the whole source file loads under
`read_tracks(dialect = "ethovision", on_invalid = "drop")`: 1,876 rows, 31
without a position.

No ANY-maze export is bundled: the real file the `anymaze` dialect was checked
against (`MWM/MWMSimplified/N14Months/NData/Madhavan14m_C1_Days1-4 - Test
6.csv` in <https://github.com/CowenLab/Ovariectomy_and_development>, 460 rows,
1 without a position) is in a repository that states no licence.
`../anymaze_synthetic.csv` has that file's layout (the header, `H:MM:SS.fff`
times, an empty first position, `In <zone>` columns) with invented values.
