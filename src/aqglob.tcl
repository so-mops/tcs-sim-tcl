# globals for acqusition code

# settable camera parameters

global gcamexposuretime gcamsettemp gcamXbin gcamYbin

# variables that describe status to user

global gcamerastatus gcameratemp gcameravac gProgramStatus

# semaphores used to control allowable actions at the user interface

global gsicamstarted gcamerainitialized gcoolerrunning
global gtelemetrystarted gfocuscalibrated gpausetelemetry

# variables for receiving and managing TCS telemetry

global gSerialportNum gtelreaderrunning
global grawtelemetry gTCSport gTCSStat gTCScmdcode gTCSlastcmdcode gTCScmdstatus
global gTCSbusy gTCSmovestatus gTCSRAlimit gTCSDEClimit gTCSHorzlimit gTCSDisabled
global gTCSFocUplimit gTCSFocDnlimit gTCSSoftlimit gTCSDRlimit
global gTCSFocus gSlewListener gSettleListener

# variables relating to observing mode and cadence

global gObservingMode gNumpasses gDetection gNminus 

# camera command in progress?

global gcambusy gCamstatdisable

# expect buffer from last camera command

global gexpectbuf

# decimal latitude and longitude of observatory

global gobslat gobslong
global azalt

# Additional globals to support TCS-ng

global gTCSnglog gCurrentTCSngCommandID guseTCSng
global gTCSngtelid gTCSinterface
