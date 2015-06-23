//
//  Issue.m
//  flic-boilerplate
//
//  Created by Oskar Öberg on 2015-05-21.
//  Copyright (c) 2015 Shortcut Labs AB. All rights reserved.
//

#import "Issue.h"

@implementation Issue

+ (instancetype)issueFromDictionary:(NSDictionary *)dictionary;
{
	Issue *issue = [[Issue alloc] init];
	
	issue.title = dictionary[@"title"];
	issue.issueId = dictionary[@"id"];
	issue.url = dictionary[@"url"];
	issue.body = dictionary[@"body"];
	
	if(issue.title.length > 60)
	{
		issue.title = [issue.title substringToIndex:59];
	}
	if(issue.body.length > 200)
	{
		issue.body = [issue.body substringToIndex:199];
	}
	return issue;
}

@end
