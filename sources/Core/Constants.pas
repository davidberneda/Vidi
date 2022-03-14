unit Constants;

interface

type
  TVidiConstants=record
  const
    Vidi_Name        = 'Vidi';

    NoDot_Extension  = 'vidi';
    Extension        = '.'+NoDot_Extension;

    Vidi_Version     = 'v0.0.20-alpha';

    BinaryExtension  = '.vidic';

    // Auto-update
    LatestRelease    = 'Latest_Release.txt';
    DownloadURL      = 'davidberneda/Vidi/raw/master/download';
    ReleasePrefix    = 'Vidi_';
  end;

implementation

end.
