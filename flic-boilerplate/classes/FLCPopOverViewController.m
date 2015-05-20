//
//  FLCPopOverViewController.m
//  Flic
//
//  Created by Oskar Öberg on 2015-03-11.
//  Copyright (c) 2015 Shortcut Labs. All rights reserved.
//

#import "FLCPopOverViewController.h"

@interface FLCPopOverViewController ()

@property (nonatomic, copy) void (^completionHandler)(FLCPopOverViewController *popOverViewController, BOOL cancelled, id value, NSError *error);
@property (nonatomic, strong) id strongSelf;
@property (nonatomic) BOOL hasAnimated;

@end

@implementation FLCPopOverViewController

- (void)viewDidLoad;
{
	self.dismissOnTouchOutside = YES;
	
	UITapGestureRecognizer *tapOutside = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(tapOutside:)];
	tapOutside.cancelsTouchesInView = NO;
	
	[self.view addGestureRecognizer:tapOutside];
}

- (void)tapOutside:(UITapGestureRecognizer *)gestureRecognizer;
{
	if(!self.dismissOnTouchOutside)
	{
		return;
	}
	
	if(!CGRectContainsPoint(self.contentView.frame, [gestureRecognizer locationInView:self.view]))
	{
		[self dismissWithCancelled:YES];
	}
}

- (void)showInView:(UIView*)view completionHandler:(void (^)(FLCPopOverViewController *popOverViewController, BOOL cancelled, id value , NSError *error))completionHandler;
{
	self.hasAnimated = NO;
	self.strongSelf = self;
	self.completionHandler = completionHandler;
	[view addSubview:self.view];
	self.view.frame = CGRectMake(0.0, 0.0, view.frame.size.width, view.frame.size.height);
	[self.view setNeedsUpdateConstraints];
}

- (void)viewDidLayoutSubviews;
{
	if(self.hasAnimated)
	{
		return;
	}
	
	self.hasAnimated = YES;
	
	[CATransaction begin];
	[CATransaction setCompletionBlock:^{
		
	}];
	
	CABasicAnimation *fadeAnimation = [CABasicAnimation animation];
	fadeAnimation.keyPath = @"opacity";
	fadeAnimation.fromValue = @(0.0);
	fadeAnimation.duration = 0.2;
	fadeAnimation.removedOnCompletion = YES;
	fadeAnimation.timingFunction = [CAMediaTimingFunction functionWithName:kCAMediaTimingFunctionEaseOut];
	
	if(self.contentView)
	{
		if(self.animationStyle == FLCPopOverViewControllerAnimationStyleZoom)
		{
			CABasicAnimation *scaleAnimation = [CABasicAnimation animation];
			scaleAnimation.keyPath = @"transform.scale";
			scaleAnimation.fromValue = @(0.8);
			scaleAnimation.duration = 0.2;
			scaleAnimation.removedOnCompletion = YES;
			scaleAnimation.timingFunction = [CAMediaTimingFunction functionWithControlPoints:0.00 :0.00 :0.00 :1.00];;
			[self.contentView.layer addAnimation:scaleAnimation forKey:@"scale"];
		}
		else if(self.animationStyle == FLCPopOverViewControllerAnimationStyleSlideFromBottom)
		{
			CABasicAnimation *slideAnimation = [CABasicAnimation animation];
			slideAnimation.keyPath = @"position.y";
			slideAnimation.fromValue = @(self.contentView.layer.position.y + self.contentView.frame.size.height);
			slideAnimation.duration = 0.5;
			slideAnimation.removedOnCompletion = YES;
			slideAnimation.timingFunction = [CAMediaTimingFunction functionWithControlPoints:0.00 :0.00 :0.00 :1.00];
			[self.contentView.layer addAnimation:slideAnimation forKey:@"slide"];
		}
	}
	[self.view.layer addAnimation:fadeAnimation forKey:@"fadein"];
	
	[CATransaction commit];
}

- (void)dismissWithCancelled:(BOOL)cancelled;
{
	if(self.completionHandler)
	{
		self.completionHandler(self, cancelled, self.value, self.error);
	}
	
	[CATransaction begin];
	[CATransaction setCompletionBlock:^{
		self.strongSelf = nil;
		[self.view removeFromSuperview];
	}];
	
	CABasicAnimation *fadeAnimation = [CABasicAnimation animation];
	fadeAnimation.keyPath = @"opacity";
	fadeAnimation.toValue = @(0.0);
	fadeAnimation.duration = 0.2;
	fadeAnimation.removedOnCompletion = NO;
	fadeAnimation.fillMode = kCAFillModeForwards;
	fadeAnimation.timingFunction = [CAMediaTimingFunction functionWithName:kCAMediaTimingFunctionEaseIn];
	
	if(self.contentView)
	{
		if(self.animationStyle == FLCPopOverViewControllerAnimationStyleZoom)
		{
			CABasicAnimation *scaleAnimation = [CABasicAnimation animation];
			scaleAnimation.keyPath = @"transform.scale";
			scaleAnimation.toValue = cancelled ? @(0.8) : @(1.2);
			scaleAnimation.duration = 0.2;
			scaleAnimation.timingFunction = [CAMediaTimingFunction functionWithControlPoints:1.00 :0.00 :1.00 :1.00];
			scaleAnimation.fillMode = kCAFillModeForwards;
			scaleAnimation.removedOnCompletion = NO;
			[self.contentView.layer addAnimation:scaleAnimation forKey:@"scale"];
		}
		else if(self.animationStyle == FLCPopOverViewControllerAnimationStyleSlideFromBottom)
		{
			CABasicAnimation *slideAnimation = [CABasicAnimation animation];
			slideAnimation.keyPath = @"position.y";
			slideAnimation.toValue = @(self.contentView.layer.position.y + self.contentView.frame.size.height);
			slideAnimation.duration = 0.3;
			slideAnimation.removedOnCompletion = NO;
			slideAnimation.fillMode = kCAFillModeForwards;
			slideAnimation.timingFunction = [CAMediaTimingFunction functionWithControlPoints:1.00 :0.00 :1.00 :1.00];
			[self.contentView.layer addAnimation:slideAnimation forKey:@"slide"];
		}
	}
	
	[self.view.layer addAnimation:fadeAnimation forKey:@"fadein"];
	
	[CATransaction commit];
}

@end
