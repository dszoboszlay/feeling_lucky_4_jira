//
//  FlicTableViewCell.h
//  flic-boilerplate
//
//  Created by Oskar Öberg on 2015-05-20.
//  Copyright (c) 2015 Shortcut Labs AB. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface FlicTableViewCell : UITableViewCell

@property (nonatomic, strong) IBOutlet UILabel *title;
@property (nonatomic, strong) IBOutlet UIImageView *icon;

@end
