//
//  ViewController.h
//  flic-boilerplate
//
//  Created by Oskar Öberg on 2015-05-18.
//  Copyright (c) 2015 Shortcut Labs AB. All rights reserved.
//

#import <UIKit/UIKit.h>

@class FLCPopOverViewController;

@interface KLCViewControllerBase : UIViewController <UITableViewDelegate, UITableViewDataSource, UIAlertViewDelegate>

@property (nonatomic, strong) IBOutlet UITableView *tableView;
@property (nonatomic, strong) NSArray *allowedButtons;

- (void)startScanningWithCompletionHandler:(void (^)(FLCPopOverViewController *popOverViewController, BOOL cancelled, id value , NSError *error))completionHandler;
- (IBAction)reloadData;

@end