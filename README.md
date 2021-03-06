
![](https://images.unsplash.com/photo-1599409636295-e3cf3538f212?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=750&q=80%20750w)

# Mario Kart 64 World Records

The data this week comes from [Mario Kart World
Records](https://mkwrs.com/) and contains world records for the classic
(if you’re a 90’s kid) racing game on the Nintendo 64.

This
[Video](https://www.youtube.com/watch?v=D6cpa-TvKn8&ab_channel=SummoningSalt)
talks about the history of Mario Kart 64 World Records in greater
detail. Despite it’s release back in 1996 (1997 in Europe and North
America), it is still actiely played by many and new world records are
achieved every month.

The game consists of 16 individual tracks and world records can be
achieved for the fastes **single lap** or the fastest completed race
(**three laps**). Also, through the years, players discovered
**shortcuts** in many of the tracks. Fortunately, shortcut and
non-shortcut world records are listed separately.

Furthermore, the Nintendo 64 was released for NTSC- and PAL-systems. On
PAL-systems, the game runs a little slower. All times in this dataset
are PAL-times, but they can be converted back to NTSC-times.

## Data Dictionary

### `world-records.rds`

> Current world records in Mario Kart 64 with date achieved and player’s
> name

| variable         | class     | description                     |
|:-----------------|:----------|:--------------------------------|
| track            | character | Track name                      |
| type             | factor    | Single or three lap record      |
| shortcut         | factor    | Shortcut or non-shortcut record |
| player           | character | Player’s name                   |
| system\_played   | character | Used system (NTSC or PAL)       |
| date             | date      | World record date               |
| time\_period     | period    | Time as `hms` period            |
| time             | double    | Time in seconds                 |
| record\_duration | double    | Record duration in days         |

### `drivers.rds`

> Player’s data. Except nationality, this could be constructed with the
> above dataset.

| variable | class     | description                            |
|:---------|:----------|:---------------------------------------|
| position | integer   | Player’s current leader board position |
| player   | character | Player’s name                          |
| total    | integer   | Total world records                    |
| year     | double    | Year                                   |
| records  | integer   | Number of world records                |
| nation   | character | Player’s nationality                   |

## Some fun questions to explore

-   How did the world records develop over time?
-   Which track is the fastest?
-   For which track did the world record improve the most?
-   For how many tracks have shortcuts been discovered?
-   When were shortcuts discovered?
-   On which track does the shortcut save the most time?
-   Which is the longest standing world record?
-   Who is the player with the most world records?
-   Who are recent players?

Credit: [Benedikt Claus](https://github.com/benediktclaus)
