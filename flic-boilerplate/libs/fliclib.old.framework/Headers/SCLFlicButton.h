//
//  @file SCLFlicButton.h
//  @framework fliclib
//
//  Created by Anton Meier on 2014-06-18.
//  Copyright (c) 2015 Shortcut Labs. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CoreBluetooth/CoreBluetooth.h>
#import <sqlite3.h>

/*!
 *  @enum SCLFlicButtonConnectionState
 *
 *  @discussion Represents the diffrent connection states that a flic can be in at any given time.
 *
 */
typedef NS_ENUM(NSInteger, SCLFlicButtonConnectionState) {
    /**
     * The flic is currently connected. This means that the iOS device has an active link with the flic and that data can be sent in both
     * directions provided that it has also been verified, notified by the <code>flicButtonIsReady:</code> callback.
     */
    SCLFlicButtonConnectionStateConnected = 0,
    /**
     * The flic is currently connecting. This means that the iOS device is either in the process of establishing a link with flic or
     * that it is waiting for flic to come within proximity so that such process can be initiated.
     */
    SCLFlicButtonConnectionStateConnecting,
    /**
     * The flic is currently disconnected.
     */
    SCLFlicButtonConnectionStateDisconnected,
    /**
     * The flic is currently disconnecting.
     */
    SCLFlicButtonConnectionStateDisconnecting,
};

/*!
 *  @enum SCLFlicButtonMode
 *
 *  @discussion Represents the different modes that a flic can be configured to operate in. It is very important that you choose a mode
 *              that fits your application the best. Always try to choose the mode that consumes the least amount of battery that works
 *              with your application. For more detailed information about these different modes see the general documentation available at
 *              our website.
 *
 */
typedef NS_ENUM(NSInteger, SCLFlicButtonMode) {
    /**
     * With Active mode the flic will be in a constant connection to the iOS device whenever a connection has been established. It will always
     * try to reconnect in case the connection is lost but the advertisement on flic will timeout after a short period of time. If this occurs
     * then the flic has to be pressed manually in order to start the advertisement again. This timeout is added in order to preserve energy.
     * If the connection however is canceled using the <code>disconnect</code> method then no pending connection will remain and the advertisement
     * on flic will not start. It will have a maximum response time of 280ms whenever connected. This is the most energy conservative version of
     * the different active modes.
     */
    SCLFlicButtonModeActive = 0,
    /**
     * This mode is very similar to Active mode with the difference that the connection will re-establish whenever flic is brought back within
     * range of the iOS device after being away for a longer period of time. This will be the case on all scenarios except when the button has
     * been manually disconnected using the <code>disconnect</code> method. This mode is particularily useful when you want to have a seamless
     * persistent user experience with lower latency than on the passive mode (described below). This mode will unfortunately consume more energy
     * since it requires that flic advertises during longer periods of time. Keep this in mind when developing your app.
     */
    SCLFlicButtonModeActiveKeepAlive = 1,
    /**
     * The SuperActive mode will behave ecactly like the regular Active mode with the exception that it instead has a maximum response time of
     * 45ms whenever connected. This is good for applications where response time is crucial. The downside is that it consumes more energy than
     * any other mode so use with caution.
     */
    SCLFlicButtonModeSuperActive = 2,
    /**
     * Flic will be in a passive state for most of the time. It will only connect to the iOS device and send data whenever a button press
     * has been made. This is the most energy conservative mode, but may have a negative effect on response time. This is the recommended mode
     * for all applications where latency is not an issue.
     *
     * While in this mode you will not get any button disconnect/connect events since all that will be handled internally.
     * Update: You will now get these events! It might be removed at a later point, to be decided.
     */
    SCLFlicButtonModePassive = 3,
};

/*!
 *  @enum SCLFlicButtonModeOptions
 *
 *  @discussion Different options that can be added when setting the mode of the flic.
 *
 */
typedef NS_ENUM(NSInteger, SCLFlicButtonModeOptions) {
    /**
     * Not available at this time! This option specifies that the LED should light up on the flic whenever pressed.
     */
    SCLFlicButtonModeOptionsLEDWhenPress = 1,
    /**
     * Not available at this time! This option specifies that the LED should blink for 5 seconds when advertisement starts.
     */
    SCLFlicButtonModeOptionsLEDWhenAdv = 2,
};

/*!
 *  @enum SCLFlicButtonLEDIntensity
 *
 *  @discussion Represents the different LED intensity options available on the flic.
 *              It is strongly recommended that you keep battery consumption in mind when choosing intensity.
 *
 */
typedef NS_ENUM(NSInteger, SCLFlicButtonLEDIntensity) {
    /**
     * LED intensity low.
     */
    SCLFlicButtonLEDIntensityLow = 0,
    /**
     * LED intensity medium.
     */
    SCLFlicButtonLEDIntensityMedium,
    /**
     * LED intensity high.
     */
    SCLFlicButtonLEDIntensityHigh,
    /* Temporary */
    SCLFlicButtonLEDIntensityFade,
};

/*!
 *  @enum SCLFlicButtonLEDType
 *
 *  @discussion Represents the different LED type options available on the flic.
 *
 */
typedef NS_ENUM(NSInteger, SCLFlicButtonLEDType) {
    /**
     * LED will be turned on with a constant light.
     */
    SCLFlicButtonLEDTypeConstant = 0,
    /**
     * LED will be turned on with a blinking light
     */
    SCLFlicButtonLEDTypeBlink,
};


/*!
 *  @enum SCLFlicButtonLEDSpeed
 *
 *  @discussion Represents the different LED speeds available when using a led pattern as the LED type.
 *              Notice, this will not have any affect if you are using a constant LED type.
 *
 */
typedef NS_ENUM(NSInteger, SCLFlicButtonLEDSpeed) {
    /**
     * LED intensity medium.
     */
    SCLFlicButtonLEDSpeedSlow = 0,
    /**
     * LED intensity high.
     */
    SCLFlicButtonLEDSpeedFast,
};

/*!
 *  @enum SCLFlicButtonLEDDuration
 *
 *  @discussion Represents the different LED intensity options available on the flic.
 *
 */
typedef NS_ENUM(NSInteger, SCLFlicButtonLEDDuration) {
    /**
     * LED will stay on until you actively turn it off. This setting is not recommended since it can have a drastic effect on battery consumption.
     */
    SCLFlicButtonLEDDurationInf = 0,
    /**
     * LED will perform the chosen action for 1 second and then turn itself off.
     */
    SCLFlicButtonLEDDuration1,
    /**
     * LED will perform the chosen action for 5 seconds and then turn itself off.
     */
    SCLFlicButtonLEDDuration5,
    /**
     * LED will perform the chosen action for 10 seconds and then turn itself off.
     */
    SCLFlicButtonLEDDuration10,
    /**
     * LED will perform the chosen action for 20 seconds and then turn itself off.
     */
    SCLFlicButtonLEDDuration20,
    /**
     * LED will perform the chosen action for 30 seconds and then turn itself off.
     */
    SCLFlicButtonLEDDuration30,
    /**
     * LED will perform the chosen action for 45 seconds and then turn itself off.
     */
    SCLFlicButtonLEDDuration45,
    /**
     * LED will perform the chosen action for 60 seconds and then turn itself off.
     */
    SCLFlicButtonLEDDuration60,
};

/*!
 *  @enum SCLFlicButtonTriggerBehavior
 *
 *  @discussion Represents the different trigger event behaviors that can be configured on the flic.
 *              The buttonUp and buttonDown events will be active no matter which of these alternatives you choose.
 *
 */
typedef NS_ENUM(NSInteger, SCLFlicButtonTriggerBehavior) {
    /**
     * Used to distinguish between only click and hold.
     * <br/><br/>
     * Click will be fired when the button is released if it was pressed for maximum 1 second.
     * Otherwise, hold will be fired 1 second after the button was pressed. Click will then not be fired upon release.
     * Since this option will only distinguish between click and hold it does not have to take double click into consideration.
     * This means that the click event can be sent immediately on button release rather than to wait for a possible double click.
     * <br/><br/>
     * Note: this will be the default behavior.
     */
    SCLFlicButtonTriggerBehaviorClickAndHold = 0,
    /**
     * Used to distinguish between only single click and double click.
     * <br/><br/>
     * Double click will be registered if the time between two button down events was at most 0.5 seconds.
     * The double click event will then be fired upon button release.
     * If the time was more than 0.5 seconds, a single click event will be fired;
     * either directly upon button release if the button was down for more than 0.5 seconds, or after 0.5 seconds
     * if the button was down for less than 0.5 seconds.
     * <br/><br/>
     * To summarize this, a small delay will be added to the click event (if it was released within 0.5 s) in order to see if a
     * double click will happen next. This ensures that a click event will not be sent prior to the double click.
     */
    SCLFlicButtonTriggerBehaviorClickAndDoubleClick,
    /**
     * Used to distinguish between single click, double click and hold.
     * <br/><br/>
     * If the time between the first button down and button up event was more than 1 second, a hold event will be fired.
     * <br/><br/>
     * Else, double click will be fired if the time between two button down events was at most 0.5 seconds.
     * The double click event will then be fired upon button release.
     * If the time was more than 0.5 seconds, a single click event will be fired;
     * either directly upon button release if the button was down for more than 0.5 seconds,
     * or after 0.5 seconds if the button was down for less than 0.5 seconds.
     * <br/><br/>
     * Note: Three fast consecutive clicks means one double click and then one single click.
     * Four fast consecutive clicks means two double clicks.
     */
    SCLFlicButtonTriggerBehaviorClickAndDoubleClickAndHold,
};

/*!
 *  @enum SCLFlicError
 *
 *  @discussion These enums represents the different error codes that can be sent on both the SCLFlicButton and SCLFlicManager classes.
 *
 */
typedef NS_ENUM(NSInteger, SCLFlicError) {
    /**
     * An error has occurred.
     */
    SCLFlicErrorUnknown = 0,
    /**
     * General error code that can be sent to let you know that a started task did not complete.
     */
    SCLFlicErrorCouldNotCompleteTask,
    /**
     * If a connection to a flic button failed for unknown reasons. This could for example be if flic is brought out of range during a connection sequense.
     */
    SCLFlicErrorConnectionFailed,
    /**
     * The RSSI value could not be read.
     */
    SCLFlicErrorCouldNotUpdateRSSI,
    /**
     * The internal database that is used in order to keep track of the buttons is not available or can not be accessed.
     */
    SCLFlicErrorDatabaseError,
    /**
     * If the flic button sends unknown data to the iOS device that does not comply with our communication protocol specification.
     */
    SCLFlicErrorUnknownDataReceived,
    /**
     * If for some reason the button does not responde in time during the verification process.
     */
    SCLFlicErrorVerificationTimeOut,
    /**
     * The backend that is used for the initial verification sequence can not be reached.
     */
    SCLFlicErrorBackendUnreachable,
    /**
     * The iOS device does not have an internet connection that is required in order to complete the task.
     */
    SCLFlicErrorNoInternetConnection,
    /**
     * The response from the backend server is invalid.
     */
    SCLFlicErrorCredentialsNotMatching,
    /**
     * If you try to access a flic that is currently being used with another device or another app on the same iOS device.
     */
    SCLFlicErrorButtonIsPrivate,
    /**
     * A crypthographic error has occurred.
     */
    SCLFlicErrorCryptographicFailure,
    /**
     * For some reason the button was disconnected before the verification sequense had time to complete.
     */
    SCLFlicErrorButtonDisconnectedDuringVerification,
};

@protocol SCLFlicButtonDelegate;

/*!
 *  @class SCLFlicButton
 *
 *  @discussion An instance of this class represents a physical flic.
 *              All commands will be directly passed to the physical flic and any events occurring on the flic will be passed along
 *              using the specified delegate methods.
 *
 */
@interface SCLFlicButton : NSObject {
    
}

/*!
 *  @property delegate
 *
 *  @discussion The delegate object that will receive events related to this particular flic.
 *
 */
@property(weak, nonatomic) id<SCLFlicButtonDelegate> delegate;

/*!
 *  @property buttonIdentifier
 *
 *  @discussion The virtual ID of the flic.
 *
 */
@property (readonly, nonatomic, strong) NSUUID *buttonIdentifier;

/*!
 *  @property buttonPublicKey
 *
 *  @discussion The public key of the flic. This in the key that is used to identify the flic on our backend.
 *
 */
@property (readonly, nonatomic, strong) NSString *buttonPublicKey;

/*!
 *  @property name
 *
 *  @discussion The Bluetooth device name of the flic button.
 *
 */
@property (atomic, readonly, strong) NSString *name;


/*!
 *  @property state
 *
 *  @discussion The current state of the flic.
 *
 */
@property (atomic, readonly) SCLFlicButtonConnectionState connectionState;

/*!
 *  @property mode
 *
 *  @discussion The current mode of the flic.
 *
 */
@property (nonatomic, readonly) SCLFlicButtonMode mode;

/*!
 *  @property triggerBehavior
 *
 *  @discussion This property specifies how the flic press events should be handled. You are allowed to change this property any time you want.
 *              Take a look at SCLFlicButtonTriggerBehavior to see the options available.
 *
 */
@property (nonatomic, readwrite) SCLFlicButtonTriggerBehavior triggerBehavior;

/*!
 *  @property pressCount
 *
 *  @discussion This property specifies how many times the flic has been toggled at any given time. This will register all down events
 *              as well as the up events, which means that if you want to know how many times it has been clicked then you have to divide
 *              this number by two. Also, this property will always contain the last known registered value, meaning that if the flic
 *              has been pressed while not being within proximity then the property will of course not be up to date. It will be updated
 *              as soon as the flic connects the next time. Important notice: Even though this value is represented by an int the internal
 *              counter only consists of 3 bytes meaning that the counter will rollover at 0xffffff (16777215). However, it is highly unlikely
 *              that anyone will ever reach those numbers. The presscounter will be reset if a factory reset is done on the physical flic button.
 *
 */
@property (nonatomic, readonly) int pressCount;

/*!
 *  @method connect:
 *
 *  @discussion		Attempts to connect the flic. If the flic is not available, due to either being out of range or being in passive
 *                  mode, then the flic will be connected once it becomes available since this call will not time out, also called a
 *                  pending connection. It can be canceled by calling the <code>disconnect</code> method.
 *
 */
- (void) connect;

/*!
 *  @method disconnect:
 *
 *  @discussion		Disconnect a currently connected flic or cancel a pending connection.
 *
 */
- (void) disconnect;

/*!
 *  @method setLEDwithIntestity:type:speed:duration:andPriority:prio
 *
 *  @param intensity    The intensity of the LED light.
 *  @param type         The type of light.
 *  @param speed        The speed of the LED action (Will have no affect when using a constant LED type).
 *  @param duration     How long the LED action should be.
 *
 *  @discussion         Turn on the flic with a color and pattern.
 *
 */
- (void) setLEDwithIntestity:(SCLFlicButtonLEDIntensity)intensity type:(SCLFlicButtonLEDType)type
                       speed:(SCLFlicButtonLEDSpeed)speed duration:(SCLFlicButtonLEDDuration)duration andPriority:(int) prio;

/*!
 *  @method turnOffLED:
 *
 *  @discussion         Turn off the LED.
 *
 */
- (void) turnOffLED;

/*!
 *  @method setMode:withOptions:
 *
 *  @discussion         This method is the method to call when you wish to switch between the different available modes for the flic.
 *                      If the flic is not available, meaning that it is not connected to the iOS device, due to either being in a passive
 *                      mode or out of proximity then the mode will not change instantly. The mode will instead change once the flic becomes
 *                      available the next time and only then will the mode property be updated accordingly. However, if you change the mode
 *                      while the flic is disconnected and not having a pending connection to it, then you need to actively call the
 *                      <code>connect:</code> method in order for it to connect. This is unless you are using a <i>KeepAlive</i> or <i>Passive</i>
 *                      mode, in which case a <code>connect</code> will be sent automatically.
 *
 *  @param mode         The mode that you wish to switch to.
 *  @param options      SCLFlicButtonModeOptions
 *
 */
- (void) setMode:(SCLFlicButtonMode)mode withOptions:(NSDictionary *)options;

/*!
 *  @method readRSSI
 *
 *  @discussion         A call to this method will read the RSSI (received signal strength indication) of the flic while it is currently connected.
 *                      A callback will be sent to the SCLFlicButton delegate once the value has been updated. This value is represented
 *                      in decibels and has an effective range from -100 to 0.
 *
 */
- (void) readRSSI;

@end

/*!
 *  @protocol SCLFlicButtonDelegate
 *
 *  @discussion         The delegate of a SCLFlicButton object must adopt the <code>SCLFlicButtonDelegate</code> protocol. There are not
 *                      any required delegate methods, but all are recommended for proper use of the flic.
 *
 */
@protocol SCLFlicButtonDelegate <NSObject>

@required

@optional

/*!
 *  @method flicButton:didReceiveButtonDown:age:
 *
 *  @param button       The SCLFlicButton object that the event came from.
 *  @param queued       This lets you know if the event is a queued event that happened before the flic connected or if it is a real time
 *                      event. Keep in mind that this parameter will most likely be <code>YES</code> if the flic is in passive mode since
 *                      the flic will in that mode connect and disconnect on every button press.
 *  @param age          The age of the trigger event in seconds. This is particularily important when receiving events that might have been
 *                      queued up on flic while it was out of range. The units is in seconds and will be rounded to the nearest second.
 *
 *  @discussion         The flic registered a button down event.
 *
 */
- (void) flicButton:(SCLFlicButton *) button didReceiveButtonDown:(BOOL) queued age: (NSInteger) age;

/*!
 *  @method flicButton:didReceiveButtonUp:age:
 *
 *  @param button       The SCLFlicButton object that the event came from.
 *  @param queued       This lets you know if the event is a queued event that happened before the flic connected or if it is a real time
 *                      event. Keep in mind that this parameter will most likely be <code>YES</code> if the flic is in passive mode since
 *                      the flic will in that mode connect and disconnect on every button press.
 *  @param age          The age of the trigger event in seconds. This is particularily important when receiving events that might have been
 *                      queued up on flic while it was out of range. The units is in seconds and will be rounded to the nearest second.
 *
 *  @discussion         The flic registered a button up event.
 *
 */
- (void) flicButton:(SCLFlicButton *) button didReceiveButtonUp:(BOOL) queued age: (NSInteger) age;

/*!
 *  @method flicButton:didReceiveButtonClick:age:
 *
 *  @param button       The SCLFlicButton object that the event came from.
 *  @param queued       This lets you know if the event is a queued event that happened before the flic connected or if it is a real time
 *                      event. Keep in mind that this parameter will most likely be <code>YES</code> if the flic is in passive mode since
 *                      the flic will in that mode connect and disconnect on every button press.
 *  @param age          The age of the trigger event in seconds. This is particularily important when receiving events that might have been
 *                      queued up on flic while it was out of range. The units is in seconds and will be rounded to the nearest second.
 *
 *  @discussion         The flic registered a button click event.
 *                      The behavior of this event depends on what SCLFlicButtonTriggerBehavior the triggerBehavior property is set to.
 *
 */
- (void) flicButton:(SCLFlicButton *) button didReceiveButtonClick:(BOOL) queued age: (NSInteger) age;

/*!
 *  @method flicButton:didReceiveButtonDoubleClick:age:
 *
 *  @param button       The SCLFlicButton object that the event came from.
 *  @param queued       This lets you know if the event is a queued event that happened before the flic connected or if it is a real time
 *                      event. Keep in mind that this parameter will most likely be <code>YES</code> if the flic is in passive mode since
 *                      the flic will in that mode connect and disconnect on every button press.
 *  @param age          The age of the trigger event in seconds. This is particularily important when receiving events that might have been
 *                      queued up on flic while it was out of range. The units is in seconds and will be rounded to the nearest second.
 *
 *  @discussion         The flic registered a button double click event.
 *                      The behavior of this event depends on what SCLFlicButtonTriggerBehavior the triggerBehavior property is set to.
 *
 */
- (void) flicButton:(SCLFlicButton *) button didReceiveButtonDoubleClick:(BOOL) queued age: (NSInteger) age;

/*!
 *  @method flicButton:didReceiveButtonHold:age:
 *
 *  @param button       The SCLFlicButton object that the event came from.
 *  @param queued       This lets you know if the event is a queued event that happened before the flic connected or if it is a real time
 *                      event. Keep in mind that this parameter will most likely be <code>YES</code> if the flic is in passive mode since
 *                      the flic will in that mode connect and disconnect on every button press.
 *  @param age          The age of the trigger event in seconds. This is particularily important when receiving events that might have been
 *                      queued up on flic while it was out of range. The units is in seconds and will be rounded to the nearest second.
 *
 *  @discussion         The flic registered a button hold event.
 *                      The behavior of this event depends on what SCLFlicButtonTriggerBehavior the triggerBehavior property is set to.
 *
 */
- (void) flicButton:(SCLFlicButton *) button didReceiveButtonHold:(BOOL) queued age: (NSInteger) age;

/*!
 *  @method flicButtonDidConnect:
 *
 *  @param button       The SCLFlicButton object that the event came from.
 *
 *  @discussion         This delegate method is called every time the flic physically connectsto the iOS device, regardless of the reason for it.
 *                      Keep in mind that you also have to wait for the <code>flicButtonIsReady:</code> before the flic is ready for use.
 *                      The <code>connectionState</code> is not guaranteed to switch to <code>SCLFlicButtonConnectionStateConnected</code> until
 *                      after the <code>flicButtonIsReady:</code> callback has arrived.
 *
 */
- (void) flicButtonDidConnect:(SCLFlicButton *)button;

/*!
 *  @method flicButtonIsReady:
 *
 *  @param button       The SCLFlicButton object that the event came from.
 *
 *  @discussion         This delegate method is called every time the flic has sucessfully connected and the autheticity has been verified.
 *                      You will not receive any press events from the flic before this callback has been sent. Typically this event will be sent
 *                      immediately after the <code>flicButtonDidConnect:</code> event. The verification process will take longer time the very
 *                      first time that the flic is connected to the particular iOS device since a round-trip to the backend server has to
 *                      be completed. This also means that an active internet connection is needed during the first connect of the flic.
 *
 */
- (void) flicButtonIsReady:(SCLFlicButton *)button;

/*!
 *  @method flicButton:didDisconnectWithError:
 *
 *  @param button       The SCLFlicButton object that the event came from.
 *  @param error        The error that caused the disconnect. If the disconnect was intentional, then the parameter will be nil.
 *
 *  @discussion         This delegate method is called every time the flic has disconnected, regardless of the reason for it.
 *                      This can sometimes be called during a connection event that failed before the user was notified of the connection.
 *
 */
- (void) flicButton:(SCLFlicButton *) button didDisconnectWithError:(NSError *)error;

/*!
 *  @method flicButton:didFailToConnectWithError:
 *
 *  @param button       The SCLFlicButton object that the event came from.
 *  @param error        The error that caused the flic to fail to connect.
 *
 *  @discussion         The requested connection failed. Please note that depending on at what point in the connection process the connection
 *                      failed you might also receive a regular flicButtonDidDisconnect: as well. If the connection fails and this callback is
 *                      made then the flic will always cancel the pending connection, regardless of what mode the flic happens to be in.
 *                      This means that if you get a <code>flicButton:didFailToConnectWithError:</code> event and the flic is in passive mode then you
 *                      need to call the <code>connect:</code> yourself to activate the pending connection once again.
 *
 */
- (void) flicButton:(SCLFlicButton *)button didFailToConnectWithError:(NSError *)error;

/*!
 *  @method flicButtonDidReceiveBatteryUpdate:
 *
 *  @param button       The SCLFlicButton object that the event came from.
 *  @param level        The battery level in percent (0-100).
 *
 *  @discussion         This lets you know that the battery status of the flic has been updated.
 *
 */
- (void) flicButton:(SCLFlicButton *) button didReceiveBatteryUpdate:(NSInteger) level;

/*!
 *  @method flicButton:didUpdateRSSI:error:
 *
 *  @param button       The SCLFlicButton object that the event came from.
 *  @param RSSI         The RSSI value represented in decibels.
 *  @param error        In case there was a problem with updating the RSSI value then this parameter will describe the error.
 *                      This will in all other cases be nil.
 *
 *  @discussion         This callback verifies (unless an error occurred) that the RSSI value was updated.
 *
 */
- (void) flicButton:(SCLFlicButton *)button didUpdateRSSI:(NSNumber *) RSSI error:(NSError *)error;

//TODO: Remove Later
- (void) flicButton:(SCLFlicButton *)button logMessage:(NSString *)mess;

@end
