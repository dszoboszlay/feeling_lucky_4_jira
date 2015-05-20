//
//  ViewController.m
//  flic-boilerplate
//
//  Created by Oskar Öberg on 2015-05-18.
//  Copyright (c) 2015 Shortcut Labs AB. All rights reserved.
//

#import "KLCViewController.h"
#import "SCLScannerPopOverViewController.h"
#import "SCLButtonScanner.h"
#import "FlicTableViewCell.h"

@interface KLCViewController ()

@property (nonatomic, strong) IBOutlet UITableView *tableView;

@end

@implementation KLCViewController

- (void)viewDidLoad;
{
	[super viewDidLoad];
	// you code here!
}

- (IBAction)scan:(id)sender;
{
	SCLScannerPopOverViewController *scannerVC = [[SCLScannerPopOverViewController alloc] initWithFlicManager:AppDelegate.flicManager scanInterval:5.0];
	
	UIView *view = [[UIApplication sharedApplication] windows][0];

	AppDelegate.buttonDelegate = scannerVC.buttonScanner;
	[scannerVC showInView:view completionHandler:^(FLCPopOverViewController *popOverViewController, BOOL cancelled, SCLFlicButton *button, NSError *error)
	{
		// if the scan was successful, you now have the button and you can also access it in flicManager.knownButtons
		// if anything went wrong, check the error
		// don't forget to set AppDelegate.buttonDelegate accordingly to recieve button events
		[self.tableView reloadData];
	}];
	
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
		[AppDelegate.flicManager forgetButton:button];
		[self.tableView reloadData];
	}
}

@end
