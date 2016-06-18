#define	kFrmNavHeaderFlagsObjectFocusStartState  0x00000001
#define	kFrmNavHeaderFlagsAppFocusStartState     0x00000002

#define RomIncompatibleAlert           1001

#define MainMenu		1590
#define FlightPlanMenu 	1591
#define GPSMenu			1592
#define DiversionMenu	1593
#define MapMenu			1594
#define TimerMenu   	1595

#define MnPreferences	  1501
#define MnAbout			  1502
#define MnSimulation	  1503
#define MnNextSegment     1504
#define MnPreviousSegment 1505
#define MnImportBase      1506
#define MnImportTFR       1507

#define MnGotoGPS		1510
#define MnGotoPrx		1511
#define MnGotoMap		1512
#define MnGotoHSI		1513
#define MnGotoPlan		1514
#define MnGotoTimer     1515

#define MnCommandPopup  1516

#define MnNewWaypoint	1520
#define MnFlight		1521
#define MnInformation	1522
#define MnOBS			1523
#define MnMapConfig    	1524
#define MnNextLeg		1525
#define MnPreviousLeg	1526
#define MnEditPlan		1527
#define MnDivertTo		1528
#define MnDivertEmergency 1529
#define MnDivertCancel 1530
#define MnDivertCancelConfirmed 1539
#define MnNextPage      1531
#define MnPreviousPage  1532
#define MnShowAlarm     1533
#define MnVNAV        	1534
#define MnDayNight      1535
#define MnClearTrackLog 1536
#define MnMonitoring    1537
#define MnUndoEdit      1538

#define MnObstacleMap   1550
#define MnObstacleOff   1551
#define MnTurnAnticipationOff 1552
#define MnFlightSave    1553
#define MnFlightSaveAs  1554
#define MnFlightFlipFlop 1555

#define MnExit          1599

#define ReadoutHeight		23
#define MainForm		2000	
#define SatReadout		2004
#define ETAReadout		2005
#define WaypointField		2014
#define WarningNotStarted	2015
#define WarningNoFix		2016
#define WarningGPSLost		2017

#define GPSForm			3000
#define StopPushButton	3001
#define StartPushButton	3002
#define SimPushButton   3003
#define LatField		3004
#define LonField		3005
#define GPSUTCField    	3006
#define AltField		3007
#define GPSLocalField   3008
#define MagField                3010
#define GPSTimeZoneField        3011
#define GPSTimeZonePlusButton   3012
#define GPSTimeZoneMinusButton  3013
#define GPSHDOPField            3014
#define GPSVDOPField            3015
#define GPSPDOPField            3016
#define GPSStatusField          3017
#define GPSPushButtonGroup	30

#define VNAVDialog 		3100
#define VnAltSelector 	3101
#define VnRateSelector	3102
#define VnBySelector	3103
#define VnOKButton      3104
#define VnCancelButton  3105
#define VnClearButton   3106
#define VnGPSAltSelector 3107

#define SimulationDialog 3500
#define SimHeadingField  3501
#define SimAltitudeField 3502
#define SimSpeedField    3503
#define SimOKButton      3504
#define SimCancelButton  3505

#define ImportDialog      3520
#define ImportStatusField 3521

#define FlightPlanForm  	4000
#define FPEditButton		4001
#define FPCoPilotButton		4002
#define FPUpButton		4003
#define FPDnButton		4004
#define FPList			4006
#define FPNewButton		4008
#define FPLegList               4010
#define FPTotalTime             4011
#define FPTotalDistance         4012
#define FPAlternateButton       4013
#define FPLegPushButton         4015
#define FPCumulativePushButton  4016
#define FPETEPushButton         4017
#define FPETAPushButton         4018
#define FPStatusField           4019

#define FPLegPushButtonGroup    40
#define FPTimePushButtonGroup   41

#define FPLegWidth              36
#define FPAltWidth              32
#define FPTrkWidth				21
#define FPDistWidth             26

#define EditWaypointForm	5000
#define IdField			5001
#define LatDegreesField		5002
#define LatMinutesField		5003
#define LonDegreesField		5004
#define LonMinutesField		5005
#define LonPopUpTrigger		5007
#define DescriptionField	5008
#define OKButton		5009
#define CancelButton		5010
#define DeleteButton		5011
#define MagVarnField		5014
#define NorthPushButton		5015
#define SouthPushButton		5016
#define WestPushButton		5017
#define EastPushButton		5018

#define TimerForm           5100
#define Timer1Selector      5101
#define Timer2Selector      5102
#define Timer3Selector      5103
#define Timer4Selector      5104
#define Timer1Button        5105
#define Timer2Button        5106
#define Timer3Button        5107
#define Timer4Button        5108
#define Timer1LabelSelector 5109
#define Timer2LabelSelector 5110
#define Timer3LabelSelector 5111
#define Timer4LabelSelector 5112
#define Timer1Seconds       5113
#define Timer2Seconds       5114
#define Timer3Seconds       5115
#define Timer4Seconds       5116

#define LatPushButtonGroup	2
#define LonPushButtonGroup	3

#define SCFFlightDBGroup    60
#define SelectCopilotFlightDialog 6000
#define FlightListBox		6001
#define SCFOKButton		    6002
#define SCFCancelButton		6003
#define SCFBlankButton   	6004
#define SCFInfoField        6005
#define SCFNextSegmentButton 6006
#define SCFDeleteButton     6007
#define SCFCoPilotButton    6008
#define SCFFMButton         6009

#define MonitorDialog 		  6100
#define MdTurnAnticipation    6101
#define MdObstacleWarnings    6103
#define MdTrackLogOff         6105
#define MdTrackLog1s          6106
#define MdTrackLog5s          6107
#define MdTrackLog10s         6108
#define MdOKButton            6109
#define MdCancelButton        6110
#define MdVoiceVolume         6111
#define MdTrackLogGroup         61

#define SaveFlightDialog          6200
#define SaveFlightFileNameField   6201
#define SaveFlightOKButton	      6202
#define SaveFlightCancelButton    6203


#define PreferencesDialog         7000
#define NmPushButton              7001
#define MiPushButton              7002
#define KmPushButton              7003
#define CpPushButton              7004
#define TruePushButton            7005
#define MagneticPushButton        7006
#define AutoStartGPSCheckbox      7008
#define UseCopilotFlightsCheckbox 7009
#define PromptForFlightCheckbox   7010
#define RegistrationCodeField     7011
#define PrefOKButton              7012
#define PrefCancelButton          7013
#define PrefOneSecUpdateCheckbox  7014
#define PrefGPSPopup              7015
#define PrefGPSSourceList         7016
#define MorePrefsButton           7020

#define UnitsPushButtonGroup      70
#define HeadingsPushButtonGroup   71

#define MessageDialog       7400
#define MDField				7401
#define MDButton1			7402
#define MDButton2			7403
#define MDButton3			7404
#define MDButton4			7405

#define MapForm              7500
#define PanButton            7501
#define MapCursorLeftButton  7507
#define MapCursorRightButton 7508
#define MapPanToWaypointButton 7509
#define MapPanToAircraftButton 7510
#define MapPanToSelectionButton 7511
#define MapAltitudeSlider    7512

/*
 * code in MapDialog depends on the ordering of the definitions below
 *
 */

#define MapDialog            7600
#define McAfButton           7601
#define McLargeAfButton      7602
#define McVORButton          7603
#define McNDBButton          7604
#define McOtherButton        7605
#define McIntersectButton    7606
#define McObstacleButton     7607
#define McClassAButton       7608
#define McClassBButton       7609
#define McClassCButton       7610
#define McClassDButton       7611
#define McClassEButton       7612
#define McClassFButton       7613
#define McClassGButton       7614
#define McSUASButton         7615
#define McUpperAirwayButton  7616
#define McLowerAirwayButton  7617
#define McUpperAltButton     7618
#define McLowerAltButton     7619
#define McTerrainButton      7620

#define McTerrainLevelLabel  7621
#define McTerrainLevelSelector 7622
#define McTracklineCheckbox  7623
#define McATZCheckbox        7624
#define McTrackupCheckbox    7625
#define McHeadingArcCheckbox 7626
#define McRouteLineCheckbox  7627
#define McTrackLogCheckbox   7628


#define DvItemListTop        36
#define DvItemListLeft       16
#define DvItemHeight         64
#define DvItemWidth          250
#define DiversionForm        8000
#define DvSearchStr          8001
#define DvScrollbar          8002
#define DvIdentCheckbox      8003
#define DvAirfieldPushbutton 8005
#define DvVORPushbutton      8006
#define DvNDBPushbutton      8007
#define DvIntPushbutton      8008
#define DvAnyPushbutton      8009
#define DvPushbuttonGroup    81

#define EmergencyDiversionForm 8100

#define AlphaPadDialog       8200
#define AlphaPadA            8201
#define AlphaPad0            8227
#define AlphaPadDash         8237
#define AlphaPadSpace        8238
#define AlphaPadBS           8250
#define AlphaPadOK           8251
#define AlphaPadCancel       8252
#define AlphaPadField        8253
#define AlphaPadClear  		 8254

#define MorePreferencesDialog    8500
#define MpGrassPushbutton        8501
#define MpAsphaltPushbutton      8502
#define MpBothPushbutton         8503
#define MpLengthField            8504
#define MpWidthField             8505
#define MpAfSelector             8506
#define MpVORSelector            8507
#define MpNDBSelector            8508
#define MpOtherSelector          8509
#define MpATZRadiusField         8522
#define MpOKButton               8523
#define MpCancelButton           8524
#define MpSurfacePushbuttonGroup 85

#define RangeSettingDialog         8550
#define RsIconPopup              8551
#define RsIconPopupList          8552
#define RsLabelPopup             8553
#define RsLabelPopupList         8554
#define RsOKButton               8555
#define RsCancelButton           8556

#define IndexDialog              8650

#define KeypadDialog   8700
#define KPButton0      8701
#define KPButton1      8702
#define KPButton2      8703
#define KPButton3      8704
#define KPButton4      8705
#define KPButton5      8706
#define KPButton6      8707
#define KPButton7      8708
#define KPButton8      8709
#define KPButton9      8710
#define KPDeleteButton 8711
#define KPUser1        8712
#define KPUser2		   8713
#define KPUser3   	   8714
#define KPUser4   	   8715
#define KPDecimalButton 8716
#define KPInputField   8717

#define CommandDialog        8800
#define CDNavButton          8801
#define CDPlanButton         8802
#define CDDivButton          8803
#define CDGPSButton          8804
#define CDEnrAppButton       8805
#define CDWPButton           8806
#define CDCRSButton          8807
#define CDCoPilotButton      8808
#define CDWPInfoButton       8809
#define CDResumeButton       8810
#define CDMapConfigButton    8811
#define CDMapButton          8812
#define CDNextWaypointButton 8813
#define CDPrevWaypointButton 8814
#define CDUndoButton         8815
#define CDCloseButton        8820
#define CDVNAVButton		 8821
#define CDDayNightButton     8822
#define CDMonitorButton      8823
#define CDTimerButton        8824
#define CDFunction1			 8825
#define CDFunction2			 8826
#define CDFunction3			 8827
#define CDFunction4			 8828
#define CDFunction5			 8829
#define CDFunction6			 8830
#define CDFunction7			 8831
#define CDFunction8			 8832

#define WPInfoDialog          8900
#define WPInfoListbox         8901
#define WPInfoIdElevField     8904
#define WPInfoDescField       8905
#define WPInfoAllPushbutton   8907
#define WPInfoFreqPushbutton  8908
#define WPInfoRwyPushbutton   8909
#define WPInfoILSPushbutton   8910
#define WPOKButton			  8911
#define WPGotoButton		  8912
#define WPPanMapButton        8915


#define WPInfoPushbuttonGroup 89

#define TextDialog            8920
#define TextDialogField       8921

#define ConfirmDeleteDialogue       9000
#define DataInputAlert              9001
#define MathLibAlert                9002
#define RegistrationSucceededAlert  9003
#define RegistrationFailedAlert     9004
#define RegistrationNagAlert        9005
#define CopilotNotFoundAlert        9006
#define AboutBox                    9007
#define DisclaimerAlert             9008
#define EnableCopilotAlert          9009
#define GenericWarningAlert         9011
#define SerialPortAlert             9015
#define WpLatErrorAlert             9016
#define WpLonErrorAlert             9017
#define WpIdErrorAlert              9018
#define WpMagVarAlert               9019
#define TzAlert                     9020
#define InvalidRunwayDimensionAlert 9021
#define DatabaseScanResultsAlert    9022
#define ScanningAlert               9023
#define ExitAlert                   9024
#define BluetoothAlert              9025
#define CardInputAlert              9026
#define GarminInputAlert            9027
#define IndexingAlert               9028
#define InvalidATZRadiusAlert       9029
#define EmergencyDivertFailedAlert  9030
#define FlightPlanAlert             9031
#define IndexingErrorAlert          9032
#define SimulationAlert             9033
#define CoPilotWaypointsAlert       9044
#define AsImportFailedAlert         9045
#define ConfirmImportAlert          9046
#define DemoExpiredAlert            9047
#define DemoLeftAlert               9048
#define LoadAlternateAlert			9049
#define FlightExistsAlert           9050
#define FlightDeleteAlert           9051

#define RegcodeTesterAlert          9099


#define SCFHelpString			501
#define PrefsHelpString			502
#define EditWaypointFormHelpString	503
#define MorePrefsHelpString             504

#define DialBitmap		1700
#define AircraftBitmaps         1707
#define IconBitmaps             1708
#define IconMaskBitmaps         1709
#define AIDialBitmap            1710

#define McAirfieldBmp 		1711
#define McLargeAirfieldBmp 	1712
#define McVORBmp 			1713
#define McNDBBmp 			1714
#define McOtherBmp 			1715
#define McIntersectionBmp 	1716
#define McObstacleBmp 		1717
#define McClassABmp 		1718
#define McClassBBmp 		1719
#define McSUASBmp 			1720
#define McUpperAirwayBmp 	1721
#define McLowerAirwayBmp 	1722
#define McFilterAboveBmp 	1723
#define McFilterBelowBmp 	1724
#define McOffBmp            1725
#define McLabelBmp          1726

#define FivewayPopupBmp		1727

#define IconBitmapsLow			1730
#define IconBitmapsLowMask      1731


#define McTerrainNormalBmp 	1800
#define McTerrainNormalWarnBmp 1801
#define McTerrainWarnBmp		1802

