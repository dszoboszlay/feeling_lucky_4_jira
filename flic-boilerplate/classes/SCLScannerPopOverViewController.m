//
//  FLCScannerPopOverViewController.m
//  Flic
//
//  Created by Oskar Öberg on 2015-04-29.
//  Copyright (c) 2015 Shortcut Labs. All rights reserved.
//

#import "SCLScannerPopOverViewController.h"
#import "SCLButtonScanner.h"

@interface SCLScannerPopOverViewController ()

@property (nonatomic, strong) UIView *scanningPane;
@property (nonatomic, strong) UIView *connectingPane;
@property (nonatomic, strong) UIView *connectedPane;
@property (nonatomic, strong) UIView *connectionErrorPane;
@property (nonatomic, strong) UIView *notFoundPane;
@property (nonatomic, strong) UIView *privatePane;
@property (nonatomic, strong) UIView *noBluetoothPane;
@property (nonatomic, strong) UIView *noInternetPane;
@property (nonatomic, strong) UIView *buttonForbidden;

@property (nonatomic, strong) UIView *currentPane;

@property (nonatomic, readonly) NSArray *paneViews;

@property (nonatomic) NSTimeInterval scanInterval;

@end

@implementation SCLScannerPopOverViewController

- (instancetype)initWithFlicManager:(SCLFlicManager *)flicManager scanInterval:(NSTimeInterval)scanInterval;
{
	if(self)
	{
		CGFloat height = 330.0;
		self.contentView = [[UIView alloc] initWithFrame:CGRectMake(0.0, self.view.frame.size.height-height, self.view.frame.size.width, height)];
		self.contentView.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleTopMargin;
		_buttonScanner = [[SCLButtonScanner alloc] initWithFlicManager:flicManager];
		_scanInterval = scanInterval;
		[self createPaneViews];
		[self.view addSubview:self.contentView];
		[self viewDidLoad];
	}
	return self;
}

- (void)viewDidLoad;
{
	[super viewDidLoad];
	__weak typeof(self) weakSelf = self;
	self.buttonScanner.stateChangeHandler = ^()
	{
		if(weakSelf.buttonScanner.connecting)
		{
			weakSelf.pane = SCLScannerPopOverViewControllerPaneConnecting;
		}
		else if(weakSelf.buttonScanner.scanning)
		{
			weakSelf.pane = SCLScannerPopOverViewControllerPaneScanning;
		}
		else
		{
			weakSelf.pane = SCLScannerPopOverViewControllerPaneConnectionError;
		}
	};
	
	self.buttonScanner.verifyButton = ^void(SCLFlicButton *flicButton, void(^verified)(NSError *))
	{
		if(weakSelf.verifyButton)
		{
			weakSelf.verifyButton(flicButton, verified);
		}
		else
		{
			verified(nil);
		}
		
	};
	
	self.buttonScanner.completionHandler = ^(SCLFlicButton *flicButton, NSError *error)
	{
		weakSelf.error = error;
		weakSelf.value = flicButton;
		if(error)
		{
			[weakSelf setPaneFromError:error];
		}
		else
		{
			weakSelf.pane = SCLScannerPopOverViewControllerPaneConnected;
		}
	};
	
	UITapGestureRecognizer *paneTapGR = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(paneTapped:)];
	[self.contentView addGestureRecognizer:paneTapGR];
	
}

- (UIView *)createViewWithDictionary:(NSDictionary *)dictionary;
{
	UIView *view = [[UIView alloc] initWithFrame:CGRectMake(0.0, 0.0, 320.0, 330.0)];
	
	UIImageView *icon = [[UIImageView alloc] initWithFrame:CGRectMake(115.0, 0.0, 90.0, 62.0)];
	icon.contentMode = UIViewContentModeCenter;
	
	UIView *background = [[UIView alloc] initWithFrame:CGRectMake(0.0, 62.0, 320.0, 268.0)];
	background.backgroundColor = [UIColor whiteColor];
	
	UIImageView *mainImage = [[UIImageView alloc] initWithFrame:CGRectMake(0.0, 111.0, 320.0, 172.0)];
	mainImage.contentMode = UIViewContentModeScaleAspectFit;
	
	UIView *bar = [[UIView alloc] initWithFrame:CGRectMake(0.0, 61.0, 320.0, 50.0)];
	
	UILabel *title = [[UILabel alloc] initWithFrame:CGRectMake(8.0, 75.0, 304.0, 21.0)];
	title.textAlignment = NSTextAlignmentCenter;
	title.textColor = [UIColor whiteColor];
	title.font = [UIFont fontWithName:@"HelveticaNeue-Bold" size:14.0];
	
	UILabel *bottomText = [[UILabel alloc] initWithFrame:CGRectMake(8.0, 271.0, 304.0, 59.0)];
	bottomText.font = [UIFont systemFontOfSize:14.0];
	bottomText.textAlignment = NSTextAlignmentCenter;
	
	UITextView *mainText = [[UITextView alloc] initWithFrame:CGRectMake(40.0, 169.0, 240.0, 89.0)];
	mainText.backgroundColor = [UIColor clearColor];
	mainText.editable = NO;
	mainText.userInteractionEnabled = NO;
	
	UIButton *topRightButton = [UIButton buttonWithType:UIButtonTypeCustom];
	topRightButton.frame = CGRectMake(270.0, 61.0, 50.0, 50.0);
	topRightButton.imageView.contentMode = UIViewContentModeCenter;
	[topRightButton addTarget:self action:@selector(topRightButtonTouched) forControlEvents:UIControlEventTouchUpInside];
	
	bar.autoresizingMask = UIViewAutoresizingFlexibleWidth;
	title.autoresizingMask = UIViewAutoresizingFlexibleWidth;
	icon.autoresizingMask = UIViewAutoresizingFlexibleLeftMargin | UIViewAutoresizingFlexibleRightMargin;
	background.autoresizingMask = UIViewAutoresizingFlexibleWidth;
	mainImage.autoresizingMask = UIViewAutoresizingFlexibleWidth;
	mainText.autoresizingMask = UIViewAutoresizingFlexibleWidth;
	bottomText.autoresizingMask = UIViewAutoresizingFlexibleWidth;
	topRightButton.autoresizingMask = UIViewAutoresizingFlexibleLeftMargin;
	
	if(dictionary[@"icon"])
	{
		icon.image = dictionary[@"icon"];
	}
	
	if(dictionary[@"mainImage"])
	{
		mainImage.image = dictionary[@"mainImage"];
	}
	
	if(dictionary[@"barBackground"])
	{
		bar.backgroundColor = dictionary[@"barBackground"];
	}
	
	if(dictionary[@"title"])
	{
		title.text = dictionary[@"title"];
	}
	
	if(dictionary[@"bottomText"])
	{
		bottomText.text = dictionary[@"bottomText"];
	}
	
	if(dictionary[@"mainText"])
	{
		mainText.attributedText = dictionary[@"mainText"];
	}
	
	if(dictionary[@"topRightButton"])
	{
		[topRightButton setImage:dictionary[@"topRightButton"] forState:UIControlStateNormal];
	}
	
	if([dictionary[@"iconAnimation"] isEqualToString:@"grow"])
	{
		CABasicAnimation *searchingAnimationScale = [CABasicAnimation animationWithKeyPath:@"transform.scale"];
		searchingAnimationScale.duration = 1.0;
		searchingAnimationScale.toValue = @1.2;
		searchingAnimationScale.repeatCount = HUGE_VALF;
		searchingAnimationScale.autoreverses = YES;
		searchingAnimationScale.timingFunction = [CAMediaTimingFunction functionWithControlPoints:0.32 :0.00 :0.08 :1.00];
		
		CABasicAnimation *searchingAnimationY = [CABasicAnimation animationWithKeyPath:@"position.y"];
		searchingAnimationY.duration = 1.0;
		searchingAnimationY.toValue = @(icon.layer.position.y-5.0);
		searchingAnimationY.repeatCount = HUGE_VALF;
		searchingAnimationY.autoreverses = YES;
		searchingAnimationY.timingFunction = [CAMediaTimingFunction functionWithControlPoints:0.33 :0.18 :0.13 :1.00];
		
		[icon.layer addAnimation:searchingAnimationScale forKey:@"scale"];
		[icon.layer addAnimation:searchingAnimationY forKey:@"y"];
	}
	
	[view addSubview:icon];
	[view addSubview:background];
	[view addSubview:mainImage];
	[view addSubview:bar];
	[view addSubview:title];
	[view addSubview:bottomText];
	[view addSubview:mainText];
	[view addSubview:topRightButton];
	
	return view;
}

- (void)createPaneViews;
{
	self.animationStyle = FLCPopOverViewControllerAnimationStyleSlideFromBottom;
	
	// scanning pane
	
	self.scanningPane = [self createViewWithDictionary:@{
														 @"title": @"Searching for Flics...",
														 @"bottomText": @"Click the Flic you want to add once",
														 @"mainImage": [UIImage imageNamed:@"main_click_flic"],
														 @"barBackground": FLC_COLOR_PINK,
														 @"icon": [UIImage imageNamed:@"main_add_flic_searching_icon"],
														 @"topRightButton": [UIImage imageNamed:@"main_add_flic_cancel_icon_low"],
														 @"iconAnimation": @"grow"
														 }];
	
	// connecting pane
	
	self.connectingPane = [self createViewWithDictionary:@{
														   @"title": @"Found new Flic, connecting...",
														   @"mainImage": [UIImage imageNamed:@"main_add_flic_mint_connecting"],
														   @"bottomText": @"This can take a few seconds",
														   @"barBackground": FLC_COLOR_PINK,
														   @"icon": [UIImage imageNamed:@"main_add_flic_connecting_icon"],
														   @"topRightButton": [UIImage imageNamed:@"main_add_flic_cancel_icon_low"]
														   }];
	
	// connected pane
	
	self.connectedPane = [self createViewWithDictionary:@{
														  @"title": @"Connected",
														  @"mainImage": [UIImage imageNamed:@"main_add_flic_mint_connected"],
														  @"bottomText": @"Tap to set up",
														  @"barBackground": FLC_COLOR_PRIMARY,
														  @"icon": [UIImage imageNamed:@"main_add_flic_connected_icon"],
														  @"topRightButton": [UIImage imageNamed:@"main_add_flic_cancel_icon_low"]
														  }];
	
	// connection error pane
	
	NSMutableParagraphStyle *paragraphStyle = [[NSMutableParagraphStyle alloc]init];
	[paragraphStyle setAlignment:NSTextAlignmentCenter];
	NSDictionary *attributes = @{NSFontAttributeName: [UIFont systemFontOfSize:18.0], NSParagraphStyleAttributeName: paragraphStyle, NSForegroundColorAttributeName: [UIColor colorWithWhite:0.2 alpha:1.0]};
	
	NSMutableAttributedString *errorMessage = [[NSMutableAttributedString alloc] initWithString:@"Something went wrong. Please try again or contact us at support@flic.io." attributes:attributes];
	
	[errorMessage addAttribute:NSForegroundColorAttributeName value:FLC_COLOR_PRIMARY_TEXT range:[errorMessage.string rangeOfString:@"support@flic.io"]];
	
	self.connectionErrorPane = [self createViewWithDictionary:@{
																@"title": @"Couldn’t connect to Flic",
																@"barBackground": FLC_COLOR_RED,
																@"topRightButton": [UIImage imageNamed:@"main_add_flic_cancel_icon_low"],
																@"mainText": errorMessage,
																@"icon": [UIImage imageNamed:@"main_add_flic_error_icon"]
																
																}];
	
	// not found pane
	
	self.notFoundPane = [self createViewWithDictionary:@{
														 @"icon": [UIImage imageNamed:@"main_add_flic_error_icon"],
														 @"barBackground": FLC_COLOR_CERISE,
														 @"title": @"Couldn’t find any new Flics",
														 @"mainText": [[NSAttributedString alloc] initWithString:@"Make sure your Flic is within reach when you click it." attributes:attributes],
														 @"topRightButton": [UIImage imageNamed:@"main_add_flic_cancel_icon_low"]
														 }];
	
	// private pane
	
	self.privatePane = [self createViewWithDictionary:@{
														@"icon": [UIImage imageNamed:@"main_add_flic_private_mode_icon"],
														@"title": @"Your Flic is locked",
														@"barBackground": [UIColor colorWithWhite:66.0/255.0 alpha:1.0],
														@"mainImage": [UIImage imageNamed:@"setup-unlock"],
														@"bottomText": @"Click & hold for 6 seconds to unlock",
														@"topRightButton": [UIImage imageNamed:@"main_add_flic_cancel_icon_low"]
														}];
	
	// no bluetooth pane
	
	self.noBluetoothPane = [self createViewWithDictionary:@{
															@"icon": [UIImage imageNamed:@"main_add_flic_bluetooth_off_icon"],
															@"barBackground": FLC_COLOR_RED,
															@"title": @"Bluetooth is OFF",
															@"mainText": [[NSAttributedString alloc] initWithString:@"\nTurn on Bluetooth and try again." attributes:attributes],
															@"topRightButton": [UIImage imageNamed:@"main_add_flic_cancel_icon_low"]
															}];
	
	
	// no internet pane
	
	self.noInternetPane = [self createViewWithDictionary:@{
														   @"icon": [UIImage imageNamed:@"main_add_flic_no_internet_icon"],
														   @"barBackground": FLC_COLOR_RED,
														   @"title": @"No internet connection",
														   @"mainText": [[NSAttributedString alloc] initWithString:@"\nMake sure you are connected to Wi-Fi or mobile data and try again." attributes:attributes],
														   @"topRightButton": [UIImage imageNamed:@"main_add_flic_cancel_icon_low"]
														   }];
	
	// forbidden
	
	self.buttonForbidden = [self createViewWithDictionary:@{
															@"icon": [UIImage imageNamed:@"main_add_flic_error_icon"],
															@"barBackground": FLC_COLOR_RED,
															@"title": @"Forbidden",
															@"mainText": [[NSAttributedString alloc] initWithString:@"You don’t have permisson to add this Flic. Please ask the owner of the button to share it with you!" attributes:attributes],
															@"topRightButton": [UIImage imageNamed:@"main_add_flic_cancel_icon_low"]
															}];
	
	self.pane = SCLScannerPopOverViewControllerPaneScanning;
}

- (void)paneTapped:(UITapGestureRecognizer *)gestureRecognizer;
{
	if(self.pane == SCLScannerPopOverViewControllerPanePrivate)
	{
		[self.buttonScanner startScanning:self.scanInterval];
	}
	else if(self.pane == SCLScannerPopOverViewControllerPaneConnected)
	{
		[self dismissWithCancelled:NO];
	}
}

- (void)viewDidLayoutSubviews;
{
	[super viewDidLayoutSubviews];
	for(UIView *paneView in self.paneViews)
	{
		CGRect frame = paneView.frame;
		frame.size.width = self.contentView.frame.size.width;
		paneView.frame = frame;
	}
}

- (void)showInView:(UIView*)view completionHandler:(void (^)(FLCPopOverViewController *popOverViewController, BOOL cancelled, id value , NSError *error))completionHandler;
{
	[self.buttonScanner startScanning:self.scanInterval];
	[super showInView:view completionHandler:completionHandler];
}

- (NSArray *)paneViews;
{
	return @[
			 self.scanningPane,
			 self.connectingPane,
			 self.connectedPane,
			 self.connectionErrorPane,
			 self.notFoundPane,
			 self.privatePane,
			 self.noBluetoothPane,
			 self.noInternetPane,
			 self.buttonForbidden
			 ];
}

- (UIView *)paneViewForPane:(SCLScannerPopOverViewControllerPane)pane;
{
	return [[self paneViews] objectAtIndex:pane];
}

- (void)setPaneFromError:(NSError *)error;
{
	if(!error) return;
	
	if([error.domain isEqualToString:SCLErrorDomain] && (error.code == SCLFlicErrorBackendUnreachable || error.code == SCLFlicErrorNoInternetConnection))
	{
		self.pane = SCLScannerPopOverViewControllerPaneNoInternet;
	}
	else if([error.domain isEqualToString:SCLErrorDomain] && error.code == SCLFlicErrorButtonIsPrivate)
	{
		self.pane = SCLScannerPopOverViewControllerPanePrivate;
	}
	else if([error.domain isEqualToString:SCLButtonScannerErrorDomain] && error.code == SCLButtonScannerErrorCodeButtonNotFound)
	{
		self.pane = SCLScannerPopOverViewControllerPaneNotFound;
	}
	else if([error.domain isEqualToString:SCLButtonScannerErrorDomain] && error.code == SCLButtonScannerErrorCodeCodeVerificationFailed)
	{
		self.pane = SCLScannerPopOverViewControllerPaneButtonForbidden;
	}
	else
	{
		self.pane = SCLScannerPopOverViewControllerPaneConnectionError;
	}
}

- (void)setPane:(SCLScannerPopOverViewControllerPane)pane;
{
	NSLog(@"set pane %i", pane);
	if(pane == _pane && self.currentPane)
	{
		return;
	}
	_pane = pane;
	
	if(self.currentPane)
	{
		UIView *oldPane = self.currentPane;
		[CATransaction begin];
		[CATransaction setCompletionBlock:^
		 {
			 [oldPane removeFromSuperview];
		 }];
		
		CABasicAnimation *slideOut = [CABasicAnimation animationWithKeyPath:@"position.x"];
		slideOut.duration = 0.8;
		slideOut.toValue = @(-90.0/2.0);
		slideOut.timingFunction = [CAMediaTimingFunction functionWithControlPoints:0.60 :0.00 :0.40 :1.00];
		slideOut.fillMode = kCAFillModeForwards;
		slideOut.removedOnCompletion = NO;
		
		CABasicAnimation *slideIn = [CABasicAnimation animationWithKeyPath:@"position.x"];
		slideIn.duration = 0.5;
		slideIn.fromValue = @(self.currentPane.layer.position.x + self.currentPane.frame.size.width);
		slideIn.timingFunction = [CAMediaTimingFunction functionWithControlPoints:0.60 :0.00 :0.40 :1.00];
		slideIn.removedOnCompletion = YES;
		
		[self.currentPane.layer addAnimation:slideOut forKey:@"slideOut"];
		
		self.currentPane = [self paneViewForPane:pane];
		self.currentPane.hidden = NO;
		[self.contentView addSubview:self.currentPane];
		[self.currentPane.layer removeAllAnimations];
		[self.currentPane.layer addAnimation:slideIn forKey:@"slideIn"];
		
		[CATransaction commit];
	}
	else
	{
		self.currentPane = [self paneViewForPane:pane];
		[self.contentView addSubview:self.currentPane];
	}
	
	_pane = pane;
}

- (void)topRightButtonTouched;
{
	[self dismissWithCancelled:YES];
}

- (void)cancel;
{
	[self dismissWithCancelled:YES];
}

- (void)dismissWithCancelled:(BOOL)cancel;
{
	[self.buttonScanner abort];
	[super dismissWithCancelled:cancel];
}

@end