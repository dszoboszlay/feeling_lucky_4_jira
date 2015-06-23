//
//  AppDelegate.m
//  flic-boilerplate
//
//  Created by Oskar Öberg on 2015-05-18.
//  Copyright (c) 2015 Shortcut Labs AB. All rights reserved.
//

#import "KLCAppDelegate.h"
#import "UIAlertView+Blocks.h"
#import "Issue.h"

#define API_URL @"http://192.168.1.18:8001"
#define SCL_APP_ID @"cfe4b5bb-ba39-4e51-808a-f9dcaad86341"
#define SCL_APP_SECRET @"bf6b88da-3c46-46be-bfdb-b3135819183a"

@interface KLCAppDelegate ()


@end

@implementation KLCAppDelegate

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
	#if !(TARGET_IPHONE_SIMULATOR)
	self.flicManager = [[SCLFlicManager alloc] initWithDelegate:self appID:SCL_APP_ID appSecret:SCL_APP_SECRET andRestoreState:YES];
	#endif
	
	[application registerUserNotificationSettings:[UIUserNotificationSettings settingsForTypes:UIUserNotificationTypeAlert|UIUserNotificationTypeBadge|UIUserNotificationTypeSound categories:nil]];
	[application registerForRemoteNotifications];
	
	return YES;
}

- (void)application:(UIApplication *)application didReceiveLocalNotification:(UILocalNotification *)notification;
{
	if([notification.userInfo[@"action"] isEqualToString:@"showcurrentissue"])
	{
		[self getCurrentIssueWithCompletionHandler:^(Issue *issue, NSError *error) {
			[self showIssue:issue];
		}];
	}
}

- (void)taskFromPath:(NSString *)path method:(NSString *)method completionHandler:(void (^)(NSData *data, NSURLResponse *response, NSError *error))completionHandler;
{
	NSString *url = [NSString stringWithFormat:@"%@/%@", API_URL, path];
	NSMutableURLRequest *request = [NSMutableURLRequest requestWithURL:[NSURL URLWithString:url]];
	request.HTTPMethod = method;
	
	NSURLSession *session = [NSURLSession sharedSession];
	NSURLSessionDataTask *task = [session dataTaskWithRequest:request completionHandler:^(NSData *data, NSURLResponse *response, NSError *error) {

		dispatch_async(dispatch_get_main_queue(), ^{
			completionHandler(data, response, error);
		});
		
	}];
	
	[task resume];
}

- (void)showIssue:(Issue *)issue;
{
	
	RIButtonItem *cancel = [RIButtonItem itemWithLabel:@"Close"];
	RIButtonItem *view = [RIButtonItem itemWithLabel:@"View" action:^{

		[[UIApplication sharedApplication] openURL:[NSURL URLWithString:issue.url]];
	}];
	
	RIButtonItem *complete = [RIButtonItem itemWithLabel:@"Complete" action:^{
		[self completeIssue:issue completionHandler:^(Issue *issue, NSTimeInterval completionTime, NSError *error) {

		}];
	}];
	RIButtonItem *reject = [RIButtonItem itemWithLabel:@"Reject" action:^{
		[self rejectIssue:issue completionHandler:^(NSError *error) {
			
		}];
	}];
	RIButtonItem *returnBtn = [RIButtonItem itemWithLabel:@"Return" action:^{
		[self returnIssue:issue completionHandler:^(NSError *error) {
		}];
	}];
	
	
	UIAlertView *alert = [[UIAlertView alloc] initWithTitle:issue.title message:issue.body cancelButtonItem:cancel otherButtonItems:view, complete, reject, returnBtn, nil];
	[alert show];
}


- (void)getCurrentIssueWithCompletionHandler:(void (^)(Issue *issue, NSError *error))completionHandler;
{
	[self taskFromPath:@"issue" method:@"GET" completionHandler:^(NSData *data, NSURLResponse *response, NSError *error)
	{
		NSDictionary *responseDict = [NSJSONSerialization JSONObjectWithData:data options:kNilOptions error:&error];
		Issue *issue = [Issue issueFromDictionary:responseDict];
		
		completionHandler(issue, error);
		 
	}];
}

- (void)getNewIssueWithCompletionHandler:(void (^)(NSError *error))completionHandler;
{
	[self taskFromPath:@"issue" method:@"POST" completionHandler:^(NSData *data, NSURLResponse *response, NSError *error)
	 {
		 if(error)
		 {
			 NSLog(@"%@", error);
			 return;
		 }
		 
		 NSDictionary *responseDict = [NSJSONSerialization JSONObjectWithData:data options:kNilOptions error:&error];
		 
		 

		UILocalNotification *n = [[UILocalNotification alloc] init];
		n.alertTitle = responseDict[@"title"];
		n.alertBody = responseDict[@"body"];
		n.fireDate = [NSDate date];
		n.userInfo = @{@"action": @"showcurrentissue"};
			 
		[[UIApplication sharedApplication] scheduleLocalNotification:n];
			 
		[[NSNotificationCenter defaultCenter] postNotificationName:NOTIFICATION_ISSUES_UPDATED object:nil];
			 
		completionHandler(error);
		 
		 

		 
	 }];
}

- (void)completeIssue:(Issue*)issue completionHandler:(void (^)(Issue *issue, NSTimeInterval completionTime, NSError *error))completionHandler;
{
	NSString *url = issue ? [NSString stringWithFormat:@"complete/%@", issue.issueId] : @"complete";
	
	[self taskFromPath:url method:@"POST" completionHandler:^(NSData *data, NSURLResponse *response, NSError *error) {
		
		NSDictionary *responseDict = [NSJSONSerialization JSONObjectWithData:data options:kNilOptions error:&error];
		Issue *issue = [Issue issueFromDictionary:responseDict];
		NSTimeInterval completionTime = [responseDict[@"time"] doubleValue];
		
		NSString *title = @"Issue completed!";
		NSString *body = [NSString stringWithFormat:@"You resolved this issue in %i seconds, good job!", (int)completionTime];
		
		if([[UIApplication sharedApplication] applicationState] == UIApplicationStateActive)
		{
			UIAlertView *alert = [[UIAlertView alloc] initWithTitle:title message:body delegate:nil cancelButtonTitle:@"OK" otherButtonTitles:nil];
			[alert show];
		}
		else
		{
			UILocalNotification * n = [[UILocalNotification alloc] init];
			n.alertTitle = title;
			n.alertBody = body;
			n.fireDate = [NSDate date];
			[[UIApplication sharedApplication] scheduleLocalNotification:n];
		}
	
		
		
		
		[[NSNotificationCenter defaultCenter] postNotificationName:NOTIFICATION_ISSUES_UPDATED object:nil];
		
		completionHandler(issue, completionTime, error);
	}];
}

- (void)rejectIssue:(Issue*)issue completionHandler:(void (^)(NSError *error))completionHandler;
{
	NSString *url = issue ? [NSString stringWithFormat:@"reject/%@", issue.issueId] : @"reject";
	
	[self taskFromPath:url method:@"POST" completionHandler:^(NSData *data, NSURLResponse *response, NSError *error) {
		
		UILocalNotification * n = [[UILocalNotification alloc] init];
		n.alertTitle = @"Issue rejected";
		n.alertBody = @"You have rejected the current issue";
		n.fireDate = [NSDate date];
		[[UIApplication sharedApplication] scheduleLocalNotification:n];
		
		
		[[NSNotificationCenter defaultCenter] postNotificationName:NOTIFICATION_ISSUES_UPDATED object:nil];
		
		completionHandler(error);
	}];
}

- (void)returnIssue:(Issue*)issue completionHandler:(void (^)(NSError *error))completionHandler;
{
	NSString *url = issue ? [NSString stringWithFormat:@"return/%@", issue.issueId] : @"return";
	
	[self taskFromPath:url method:@"POST" completionHandler:^(NSData *data, NSURLResponse *response, NSError *error) {
		[[NSNotificationCenter defaultCenter] postNotificationName:NOTIFICATION_ISSUES_UPDATED object:nil];
		completionHandler(error);
	}];
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
