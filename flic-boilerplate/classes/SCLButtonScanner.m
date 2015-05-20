//
//  FLCScanResult.m
//  Flic
//
//  Created by Oskar Öberg on 2015-04-29.
//  Copyright (c) 2015 Shortcut Labs. All rights reserved.
//

#import "SCLButtonScanner.h"

#define MAX_CONNECTING_TIME 10

NSString * const SCLButtonScannerErrorDomain = @"FLCButtonScannerErrorDomain";

@interface SCLButtonScanner ()

@property (nonatomic, strong) NSMutableArray *pendingButtons;
@property (nonatomic, strong) NSMutableArray *failedButtons;
@property (nonatomic, strong) SCLFlicManager *flicManager;
@property (nonatomic, strong) NSMutableArray *errors;

@end

#pragma clang diagnostic ignored "-Wprotocol"

@implementation SCLButtonScanner

@synthesize scanning=_scanning;

- (instancetype)initWithFlicManager:(SCLFlicManager *)flicManager;
{
	self = [super init];
	if(self)
	{
		_pendingButtons = [NSMutableArray new];
		_failedButtons = [NSMutableArray new];
		_errors = [NSMutableArray new];
		_flicManager = flicManager;
	}
	return self;
}

- (void)startScanning:(NSTimeInterval)timeInterval;
{
	[self.errors removeAllObjects];
	self.button = nil;
	self.connecting = NO;
	self.scanning = YES;
	
	[self.flicManager startScan];
	
	if(self.stateChangeHandler)
	{
		self.stateChangeHandler();
	}
	
	__weak typeof(self) weakSelf = self;
	
	dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(timeInterval * NSEC_PER_SEC)), dispatch_get_main_queue(), ^
	{
		if(weakSelf.scanning)
		{
			[weakSelf stopScan];
			if(weakSelf.pendingButtons.count == 0)
			{
				weakSelf.connecting = NO;
			}
			[weakSelf checkState];
		}
   });
	
	dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(MAX_CONNECTING_TIME * NSEC_PER_SEC)), dispatch_get_main_queue(), ^{
		
	});
}

- (void)done;
{
	[self clear];
	
	if(!self.button && self.errors.count == 0)
	{
		[self.errors addObject:[NSError errorWithDomain:SCLButtonScannerErrorDomain code:SCLButtonScannerErrorCodeButtonNotFound userInfo:nil]];
	}
	if(self.completionHandler)
	{
		self.completionHandler(self.button, !self.button ? self.error : nil);
		
	}
	
}

- (void)stopScan;
{
	[self.flicManager stopScan];
	self.scanning = NO;
}

- (void)removePendingButton:(SCLFlicButton *)flicButton;
{
	[self.pendingButtons removeObject:flicButton];
	if(self.pendingButtons.count == 0 && !self.scanning)
	{
		self.connecting = NO;
	}
}

- (void)flicButtonIsReady:(SCLFlicButton *)flicButton;
{
	[self stopScan];
	
	__weak typeof(self) weakSelf = self;
	
	void(^next)(NSError *error) = ^(NSError *error)
	{
		[weakSelf removePendingButton:flicButton];
		if(error)
		{
			NSError *internalError = [NSError errorWithDomain:SCLButtonScannerErrorDomain code:SCLButtonScannerErrorCodeCodeVerificationFailed userInfo:@{@"originalError":error}];
			[weakSelf.errors addObject:internalError];
			
			[self.flicManager forgetButton:flicButton];
			
			[weakSelf checkState];
		}
		else
		{
			
			weakSelf.button = flicButton;
			[weakSelf done];
		}
	};
	
	if(self.verifyButton)
	{
		self.verifyButton(flicButton, ^void(NSError *error)
						  {
							  next(error);
						  });
	}
	else
	{
		next(nil);
	}
	
}

- (void)flicManager:(SCLFlicManager *)manager didDiscoverButton:(SCLFlicButton *)button withRSSI:(NSNumber *)RSSI;
{
	if(self.isInterestedInButton && !self.isInterestedInButton(button))
	{
		[self.failedButtons addObject:button];
		return;
	}
	[button connect];
	[self.pendingButtons addObject:button];
	
	_connecting = YES;
	[self checkState];
	
}

- (void)flicButton:(SCLFlicButton *)button didFailToConnectWithError:(NSError *)error;
{
	[self.errors addObject:error];
	[self removePendingButton:button];
	[self.failedButtons addObject:button];

	
	NSLog(@"failed: %@ pending: %i", error, (int)self.pendingButtons.count);
	[self checkState];
}

- (void)checkState;
{
	if(self.scanning || self.connecting)
	{
		if(self.stateChangeHandler)
		{
			self.stateChangeHandler();
		}
	}
	else
	{
		[self done];
	}
}

- (NSInteger)priorityForError:(NSError *)error;
{
	if([error.domain isEqualToString:SCLButtonScannerErrorDomain] && error.code == SCLButtonScannerErrorCodeCodeVerificationFailed)
	{
		return 1;
	}
	else if([error.domain isEqualToString:SCLErrorDomain] && error.code == SCLFlicErrorButtonIsPrivate)
	{
		return 2;
	}
	else if([error.domain isEqualToString:SCLErrorDomain] && error.code == SCLFlicErrorBackendUnreachable)
	{
		return 3;
	}
	else if([error.domain isEqualToString:SCLErrorDomain] && error.code == SCLFlicErrorNoInternetConnection)
	{
		return 4;
	}
	return 5;
}

- (NSError *)error;
{
	NSArray *sorted = [self.errors sortedArrayUsingComparator:^NSComparisonResult(NSError *obj1, NSError *obj2)
					   {
						   NSInteger prio1 = [self priorityForError:obj1];
						   NSInteger prio2 = [self priorityForError:obj2];
						   
						   if(prio1 == prio2)
						   {
							   return NSOrderedSame;
						   }
						   return prio1 < prio2 ? NSOrderedAscending : NSOrderedDescending;
					   }];
	return sorted.firstObject;
}

- (IBAction)scan:(id)sender;
{
	
}

- (void)clear;
{
	[self stopScan];
	for(SCLFlicButton *button in self.pendingButtons)
	{
		[self.flicManager forgetButton:button];
	}
	for(SCLFlicButton *button in self.failedButtons)
	{
		[self.flicManager forgetButton:button];
	}
	[self.failedButtons removeAllObjects];
	[self.pendingButtons removeAllObjects];
	
}

@end