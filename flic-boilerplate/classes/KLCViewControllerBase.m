//
//  ViewController.m
//  flic-boilerplate
//
//  Created by Oskar Öberg on 2015-05-18.
//  Copyright (c) 2015 Shortcut Labs AB. All rights reserved.
//

#import "KLCViewControllerBase.h"
#import "FlicTableViewCell.h"
#import "SCLScannerPopOverViewController.h"
#import "SCLButtonScanner.h"


@interface KLCViewControllerBase ()

@end

@implementation KLCViewControllerBase

- (void)viewDidLoad;
{
	[super viewDidLoad];
	self.allowedButtons = @[@"ARcA"];
}

- (void)startScanningWithCompletionHandler:(void (^)(FLCPopOverViewController *popOverViewController, BOOL cancelled, id value , NSError *error))completionHandler;
{
	SCLScannerPopOverViewController *scannerVC = [[SCLScannerPopOverViewController alloc] initWithFlicManager:AppDelegate.flicManager scanInterval:5.0];
	
	scannerVC.allowedButtons = self.allowedButtons;
	
	UIView *view = [[UIApplication sharedApplication] windows][0];
	
	AppDelegate.buttonDelegate = scannerVC.buttonScanner;
	[scannerVC showInView:view completionHandler:completionHandler];
	
}

- (IBAction)reloadData;
{
	[self.tableView reloadData];
}


# pragma mark UITableViewDataSource

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section;
{
	return AppDelegate.flicManager.knownButtons.count;
}

- (UITableViewCell*)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath;
{
	static NSString *cellId = @"FlicTableViewCell";
	FlicTableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:cellId forIndexPath:indexPath];
	SCLFlicButton *button = AppDelegate.flicManager.knownButtons[AppDelegate.flicManager.knownButtons.allKeys[indexPath.row]];
	cell.title.text = button.name.length > 4 ? [button.name substringFromIndex:4] : button.name;
	return cell;
}

- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath;
{
	return 44.0;
}

#pragma mark UITableViewDelegate

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath;
{
	UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Flic" message:nil delegate:self cancelButtonTitle:@"Close" otherButtonTitles:@"Connect", @"Disconnect", @"Forget button", nil];
	alert.tag = indexPath.row;
	[alert show];
	[self.tableView deselectRowAtIndexPath:indexPath animated:YES];
}

- (void)alertView:(UIAlertView *)alertView clickedButtonAtIndex:(NSInteger)buttonIndex;
{
	SCLFlicButton *button = AppDelegate.flicManager.knownButtons[AppDelegate.flicManager.knownButtons.allKeys[alertView.tag]];
	if(buttonIndex == alertView.firstOtherButtonIndex)
	{
		[button connect];
	}
	else if(buttonIndex == alertView.firstOtherButtonIndex + 1)
	{
		[button disconnect];
	}
	else if(buttonIndex == alertView.firstOtherButtonIndex + 2)
	{
		[button disconnect];
		[AppDelegate.flicManager forgetButton:button];
		[self.tableView reloadData];
	}
}

@end
