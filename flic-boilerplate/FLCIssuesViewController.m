//
//  FLCIssuesViewController.m
//  flic-boilerplate
//
//  Created by Oskar Öberg on 2015-05-21.
//  Copyright (c) 2015 Shortcut Labs AB. All rights reserved.
//

#import "FLCIssuesViewController.h"
#import "KLCIssueTableViewCell.h"
#import "Issue.h"
#import "UIAlertView+Blocks.h"

@interface FLCIssuesViewController ()

@property (nonatomic, strong) IBOutlet UITableView *tableView;
@property (nonatomic, strong) IBOutlet UIActivityIndicatorView *activityIndicator;
@property (nonatomic, strong) NSArray *issues;

@end

@implementation FLCIssuesViewController

- (void)viewDidLoad;
{
	[super viewDidLoad];
	
	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(fetchIssues) name:NOTIFICATION_ISSUES_UPDATED object:nil];
	
	[self fetchIssues];
}

- (void)viewWillAppear:(BOOL)animated;
{
	[self fetchIssues];
}

- (void)fetchIssues;
{
	[self.activityIndicator startAnimating];
	[AppDelegate taskFromPath:@"issues" method:@"GET" completionHandler:^(NSData *data, NSURLResponse *response, NSError *error)
	{
		[self.activityIndicator stopAnimating];
		NSString *responseString = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
		NSDictionary *responseDict = [NSJSONSerialization JSONObjectWithData:data options:kNilOptions error:&error];
		
		NSMutableArray *issues = [NSMutableArray new];
		
		for(NSDictionary *issueDict in responseDict[@"issues"])
		{
			Issue *issue = [Issue issueFromDictionary:issueDict];
			[issues addObject:issue];
		}
		
		self.issues = issues;
		[self.tableView reloadData];
	}];
}

# pragma mark UITableViewDataSource

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section;
{
	return self.issues.count;
}

- (UITableViewCell*)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath;
{
	static NSString *cellId = @"KLCIssueTableViewCell";
	Issue *issue = self.issues[indexPath.row];
	KLCIssueTableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:cellId forIndexPath:indexPath];
	cell.title.text = issue.title;
	cell.subtitle.text = issue.body;
	return cell;
}

- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath;
{
	return 44.0;
}

#pragma mark UITableViewDelegate

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath;
{
	[self.tableView deselectRowAtIndexPath:indexPath animated:YES];
	
	Issue *issue = self.issues[indexPath.row];
	[AppDelegate showIssue:issue];
	
}

@end
