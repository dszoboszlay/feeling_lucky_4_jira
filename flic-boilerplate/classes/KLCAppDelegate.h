//
//  AppDelegate.h
//  flic-boilerplate
//
//  Created by Oskar Öberg on 2015-05-18.
//  Copyright (c) 2015 Shortcut Labs AB. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <fliclib/fliclib.h>

@class Issue;

@interface KLCAppDelegate : UIResponder <UIApplicationDelegate, SCLFlicButtonDelegate, SCLFlicManagerDelegate, UIAlertViewDelegate>

@property (strong, nonatomic) UIWindow *window;
@property (nonatomic, strong) SCLFlicManager *flicManager;
@property (nonatomic, weak) id<SCLFlicButtonDelegate, SCLFlicManagerDelegate> buttonDelegate;

- (void)taskFromPath:(NSString *)path method:(NSString *)method completionHandler:(void (^)(NSData *data, NSURLResponse *response, NSError *error))completionHandler;

- (void)showIssue:(Issue *)issue;
- (void)getCurrentIssueWithCompletionHandler:(void (^)(Issue *issue, NSError *error))completionHandler;
- (void)getNewIssueWithCompletionHandler:(void (^)(NSError *error))completionHandler;
- (void)completeIssue:(Issue*)issue completionHandler:(void (^)(Issue *issue, NSTimeInterval completionTime, NSError *error))completionHandler;
- (void)rejectIssue:(Issue*)issue completionHandler:(void (^)(NSError *error))completionHandler;
- (void)returnIssue:(Issue*)issue completionHandler:(void (^)(NSError *error))completionHandler;

@end

