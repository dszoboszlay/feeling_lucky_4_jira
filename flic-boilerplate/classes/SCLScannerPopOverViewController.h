//
//  FLCScannerPopOverViewController.h
//  Flic
//
//  Created by Oskar Öberg on 2015-04-29.
//  Copyright (c) 2015 Shortcut Labs. All rights reserved.
//

#import "FLCPopOverViewController.h"

typedef enum SCLScannerPopOverViewControllerPane
{
	SCLScannerPopOverViewControllerPaneScanning,
	SCLScannerPopOverViewControllerPaneConnecting,
	SCLScannerPopOverViewControllerPaneConnected,
	SCLScannerPopOverViewControllerPaneConnectionError,
	SCLScannerPopOverViewControllerPaneNotFound,
	SCLScannerPopOverViewControllerPanePrivate,
	SCLScannerPopOverViewControllerPaneNoBluetooth,
	SCLScannerPopOverViewControllerPaneNoInternet,
	SCLScannerPopOverViewControllerPaneButtonForbidden
}SCLScannerPopOverViewControllerPane;

@class SCLButtonScanner, SCLFlicButton, SCLFlicManager;

@interface SCLScannerPopOverViewController : FLCPopOverViewController

@property (nonatomic, strong) SCLButtonScanner *buttonScanner;
@property (nonatomic) SCLScannerPopOverViewControllerPane pane;
@property (nonatomic, copy) void (^verifyButton)(SCLFlicButton *flicButton, void(^verified)(NSError *error));
@property (nonatomic, copy) NSArray *allowedButtons;

- (instancetype)initWithFlicManager:(SCLFlicManager *)flicManager scanInterval:(NSTimeInterval)scanInterval;

@end