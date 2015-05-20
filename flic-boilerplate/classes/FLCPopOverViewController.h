//
//  FLCPopOverViewController.h
//  Flic
//
//  Created by Oskar Öberg on 2015-03-11.
//  Copyright (c) 2015 Shortcut Labs. All rights reserved.
//

#import <UIKit/UIKit.h>

@class FLCPopOverViewController;

typedef enum FLCPopOverViewControllerAnimationStyle
{
	FLCPopOverViewControllerAnimationStyleZoom,
	FLCPopOverViewControllerAnimationStyleSlideFromBottom
}FLCPopOverViewControllerAnimationStyle;

@interface FLCPopOverViewController : UIViewController

@property (nonatomic, strong) IBOutlet UIView *contentView;
@property (nonatomic, strong) id value;
@property (nonatomic, strong) NSError *error;
@property (nonatomic) FLCPopOverViewControllerAnimationStyle animationStyle;
@property (nonatomic) BOOL dismissOnTouchOutside;

- (void)showInView:(UIView*)view completionHandler:(void (^)(FLCPopOverViewController *popOverViewController, BOOL cancelled, id value , NSError *error))completionHandler;
- (void)dismissWithCancelled:(BOOL)cancel;

@end
