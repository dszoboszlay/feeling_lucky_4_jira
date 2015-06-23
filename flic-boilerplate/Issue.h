//
//  Issue.h
//  flic-boilerplate
//
//  Created by Oskar Öberg on 2015-05-21.
//  Copyright (c) 2015 Shortcut Labs AB. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface Issue : NSObject

@property (nonatomic, strong) NSString *title;
@property (nonatomic, strong) NSString *issueId;
@property (nonatomic, strong) NSString *url;
@property (nonatomic, strong) NSString *body;

+ (instancetype)issueFromDictionary:(NSDictionary *)dictionary;

@end
