//
//  AppDelegate.h
//  flic-boilerplate
//
//  Created by Oskar Öberg on 2015-05-18.
//  Copyright (c) 2015 Shortcut Labs AB. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <fliclib/fliclib.h>

@interface KLCAppDelegate : UIResponder <UIApplicationDelegate, SCLFlicButtonDelegate, SCLFlicManagerDelegate>

@property (strong, nonatomic) UIWindow *window;
@property (nonatomic, strong) SCLFlicManager *flicManager;
@property (nonatomic, weak) id<SCLFlicButtonDelegate, SCLFlicManagerDelegate> buttonDelegate;

@end

