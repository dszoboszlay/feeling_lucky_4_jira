//
//  FlicTableViewCell.m
//  flic-boilerplate
//
//  Created by Oskar Öberg on 2015-05-20.
//  Copyright (c) 2015 Shortcut Labs AB. All rights reserved.
//

#import "FlicTableViewCell.h"
#import "SKBounceAnimation.h"

@implementation FlicTableViewCell

- (void)boing;
{
	CATransform3D transform = self.icon.layer.transform;
	id finalValue = [NSValue valueWithCATransform3D:
					 CATransform3DScale(transform, .8, 1.2, .9)
					 ];
	
	SKBounceAnimation *bounceAnimation = [SKBounceAnimation animationWithKeyPath:@"transform"];
	bounceAnimation.fromValue = finalValue;
	bounceAnimation.toValue = [NSValue valueWithCATransform3D:transform];
	bounceAnimation.duration = 0.15f;
	bounceAnimation.numberOfBounces = 2;
	bounceAnimation.shouldOvershoot = YES;
	
	[self.icon.layer addAnimation:bounceAnimation forKey:@"boink"];
	
	/*
	 CABasicAnimation *animation = [CABasicAnimation animation];
	 animation.keyPath = @"transform.scale";
	 animation.fromValue = @(1.1);
	 animation.toValue = @(1.0);
	 animation.duration = 0.3;
	 animation.timingFunction = [CAMediaTimingFunction functionWithName:kCAMediaTimingFunctionEaseIn];
	 animation.removedOnCompletion = YES;
	 
	 [self.buttonImage.layer addAnimation:animation forKey:@"boink"];
	 */
}

@end
