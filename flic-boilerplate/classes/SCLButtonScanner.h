//
//  FLCScanResult.h
//  Flic
//
//  Created by Oskar Öberg on 2015-04-29.
//  Copyright (c) 2015 Shortcut Labs. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "fliclib/fliclib.h"

extern NSString * const SCLButtonScannerErrorDomain;

typedef NS_ENUM(NSInteger, SCLButtonScannerErrorCode)
{
	SCLButtonScannerErrorCodeUserDidCancel,
	SCLButtonScannerErrorCodeButtonNotFound,
	SCLButtonScannerErrorCodeCodeVerificationFailed
};

@interface SCLButtonScanner : NSObject <SCLFlicButtonDelegate, SCLFlicManagerDelegate>

@property (nonatomic, strong) SCLFlicButton *button;
@property (nonatomic) BOOL connecting;
@property (nonatomic) BOOL scanning;
@property (nonatomic, copy) void (^stateChangeHandler)();
@property (nonatomic, copy) void (^verifyButton)(SCLFlicButton *flicButton, void(^verified)(NSError *error));
@property (nonatomic, copy) void (^completionHandler)(SCLFlicButton *flicButton, NSError *error);
@property (nonatomic, readonly) NSError *error;


- (instancetype)initWithFlicManager:(SCLFlicManager *)flicManager;
- (void)startScanning:(NSTimeInterval)timeInterval;
- (void)stopScan;
- (void)abort;

@end