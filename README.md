# streamWriter

A windows application for recording/playing internet-radio streams.

Visit [streamwriter.org](https://streamwriter.org) for more information.

<p align="center">
  <img src="./.github/streamwriter-main.png" alt="streamWriter main window">
</p>

### :zap: Features
- Record as many streams as you want at the same time (MP3/AAC)
- Automatically record a wishlist's song when it's playing on a stream
- Player for streams and recorded files
- Track splitting with silence detection
- Function for manual cutting of saved titles
- Tracks are named by a given pattern
- Short songs (ads) can be skipped
- Writing of tags to recorded files
- Script-based postprocessing
- Applying of effects to recorded songs (SoX)
- Integration of different audio encoders
- Scheduled recordings
- Stream browser
- Multilingual
- Can be installed or used in portable mode
- Built using Lazarus :fire:

### :computer: Installation
Downloads are available at [streamwriter.org](https://streamwriter.org/downloads).

### :gear: Building
- Install Lazarus IDE
- Clone repository and init submodules
- Install package `SubModules\fpc-common\controls\mcontrols.lpk`
- Set variables in `Scripts\SetEnvironment.bat`
- Run `Scripts\Build.bat` to build a release build