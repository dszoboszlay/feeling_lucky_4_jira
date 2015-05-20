//
//  AppDelegate.m
//  flic-boilerplate
//
//  Created by Oskar Öberg on 2015-05-18.
//  Copyright (c) 2015 Shortcut Labs AB. All rights reserved.
//

#import "KLCAppDelegate.h"

#define SCL_APP_ID @"cfe4b5bb-ba39-4e51-808a-f9dcaad86341"
#define SCL_APP_SECRET @"bf6b88da-3c46-46be-bfdb-b3135819183a"

@interface KLCAppDelegate ()


@end

@implementation KLCAppDelegate

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
	#if !(TARGET_IPHONE_SIMULATOR)
	self.flicManager = [[SCLFlicManager alloc] initWithDelegate:self appID:SCL_APP_ID appSecret:SCL_APP_SECRET andRestoreState:YES];
	#endif
	return YES;
}

#pragma mark - SCLFlicManagerDelegate

- (void) flicManager:(SCLFlicManager *)manager didChangeBluetoothState: (SCLFlicManagerBluetoothState) state;
{
	NSLog(@"flicManagerDidChangeBluetoothState:%i", (int)manager.bluetoothState);
	if([self.buttonDelegate respondsToSelector:@selector(flicManager: didChangeBluetoothState:)])
	{
		[self.buttonDelegate flicManager:manager didChangeBluetoothState:state];
	}
}

- (void) flicManagerDidRestoreState:(SCLFlicManager *)manager;
{
	NSLog(@"flicManagerDidRestoreState:%@", manager);
	
	for(id key in self.flicManager.knownButtons)
	{
		SCLFlicButton *button = self.flicManager.knownButtons[key];
		button.delegate = self;
	}
	
	if([self.buttonDelegate respondsToSelector:@selector(flicManagerDidRestoreState:)])
	{
		[self.buttonDelegate flicManagerDidRestoreState:manager];
	}
}

- (void) flicManager:(SCLFlicManager *)manager didDiscoverButton:(SCLFlicButton *)button withRSSI:(NSNumber *)RSSI;
{
	NSLog(@"flicManager:didDiscoverButton:%@", button.name);
	button.delegate = self;
	if([self.buttonDelegate respondsToSelector:@selector(flicManager: didDiscoverButton:withRSSI:)])
	{
		[self.buttonDelegate flicManager:manager didDiscoverButton:button withRSSI:RSSI];
	}
}

- (void) flicManager:(SCLFlicManager *)manager didForgetButton:(NSUUID *)buttonIdentifier error:(NSError *)error;
{
	NSLog(@"flicManager:didForgetButton:%@ error:%@", buttonIdentifier, error);
	if([self.buttonDelegate respondsToSelector:@selector(flicManager: didForgetButton:error:)])
	{
		[self.buttonDelegate flicManager:manager didForgetButton:buttonIdentifier error:error];
	}
}

#pragma mark - SCLFlicButtonDelegate

- (void) flicButton:(SCLFlicButton *) button didReceiveButtonDown:(BOOL) queued age: (NSInteger) age;
{
	NSLog(@"flicButton:%@ didReceiveButtonDown:%i age:%li", button, (int)queued, (long)age);
	if([self.buttonDelegate respondsToSelector:@selector(flicButton: didReceiveButtonDown:age:)])
	{
		[self.buttonDelegate flicButton:button didReceiveButtonDown:queued age:age];
	}
}

- (void) flicButton:(SCLFlicButton *) button didReceiveButtonUp:(BOOL) queued age: (NSInteger) age;
{
	NSLog(@"flicButton:%@ didReceiveButtonUp:%i age:%li", button, (int)queued, (long)age);
	if([self.buttonDelegate respondsToSelector:@selector(flicButton: didReceiveButtonUp:age:)])
	{
		[self.buttonDelegate flicButton:button didReceiveButtonUp:queued age:age];
	}
}

- (void) flicButton:(SCLFlicButton *) button didReceiveButtonClick:(BOOL) queued age: (NSInteger) age;
{
	NSLog(@"flicButton:%@ didReceiveButtonClick:%i age:%li", button, (int)queued, (long)age);
	if([self.buttonDelegate respondsToSelector:@selector(flicButton: didReceiveButtonClick:age:)])
	{
		[self.buttonDelegate flicButton:button didReceiveButtonClick:queued age:age];
	}
}

- (void) flicButton:(SCLFlicButton *) button didReceiveButtonDoubleClick:(BOOL) queued age: (NSInteger) age;
{
	NSLog(@"flicButton:%@ didReceiveButtonDoubleClick:%i age:%li", button, (int)queued, (long)age);
	if([self.buttonDelegate respondsToSelector:@selector(flicButton: didReceiveButtonDoubleClick:age:)])
	{
		[self.buttonDelegate flicButton:button didReceiveButtonDoubleClick:queued age:age];
	}
}

- (void) flicButton:(SCLFlicButton *) button didReceiveButtonHold:(BOOL) queued age: (NSInteger) age;
{
	NSLog(@"flicButton:%@ didReceiveButtonHold:%i age:%li", button, (int)queued, (long)age);
	if([self.buttonDelegate respondsToSelector:@selector(flicButton:didReceiveButtonHold:age:)])
	{
		[self.buttonDelegate flicButton:button didReceiveButtonHold:queued age:age];
	}
}

- (void) flicButtonDidConnect:(SCLFlicButton *)button;
{
	NSLog(@"flicButtonDidConnect:%@", button);
	
	[button setMode:SCLFlicButtonModeActiveKeepAlive withOptions:nil];
	button.triggerBehavior = SCLFlicButtonTriggerBehaviorClickAndDoubleClickAndHold;

	if([self.buttonDelegate respondsToSelector:@selector(flicButtonDidConnect:)])
	{
		[self.buttonDelegate flicButtonDidConnect:button];
	}
}

- (void) flicButtonIsReady:(SCLFlicButton *)button;
{
	NSLog(@"flicButtonDidConnect:%@", button);
	if([self.buttonDelegate respondsToSelector:@selector(flicButtonIsReady:)])
	{
		[self.buttonDelegate flicButtonIsReady:button];
	}
}

- (void) flicButton:(SCLFlicButton *) button didDisconnectWithError:(NSError *)error;
{
	NSLog(@"flicButton:%@ didDisconnectWithError:%@", button, error);
	if([self.buttonDelegate respondsToSelector:@selector(flicButton: didDisconnectWithError:)])
	{
		[self.buttonDelegate flicButton:button didDisconnectWithError:error];
	}
}

- (void) flicButton:(SCLFlicButton *)button didFailToConnectWithError:(NSError *)error;
{
	NSLog(@"flicButton:%@ didFailToConnectWithError:%@", button, error);
	if([self.buttonDelegate respondsToSelector:@selector(flicButton: didFailToConnectWithError:)])
	{
		[self.buttonDelegate flicButton:button didFailToConnectWithError:error];
	}
}

- (void) flicButton:(SCLFlicButton *) button didReceiveBatteryUpdate:(NSInteger) level;
{
	NSLog(@"flicButton:%@ didReceiveBatteryUpdate:%li", button, (long)level);
	if([self.buttonDelegate respondsToSelector:@selector(flicButton: didReceiveBatteryUpdate:)])
	{
		[self.buttonDelegate flicButton:button didReceiveBatteryUpdate:level];
	}
}

- (void) flicButton:(SCLFlicButton *)button didUpdateRSSI:(NSNumber *) RSSI error:(NSError *)error;
{
	NSLog(@"flicButton:%@ didUpdateRSSI:%@ error:%@", button, RSSI, error);
	if([self.buttonDelegate respondsToSelector:@selector(flicButton: didUpdateRSSI:error:)])
	{
		[self.buttonDelegate flicButton:button didUpdateRSSI:RSSI error:error];
	}
}

@end
