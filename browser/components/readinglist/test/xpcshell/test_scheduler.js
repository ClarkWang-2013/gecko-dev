/* Any copyright is dedicated to the Public Domain.
   http://creativecommons.org/publicdomain/zero/1.0/ */

XPCOMUtils.defineLazyModuleGetter(this, 'setTimeout',
  'resource://gre/modules/Timer.jsm');

// Setup logging prefs before importing the scheduler module.
Services.prefs.setCharPref("readinglist.log.appender.dump", "Trace");

let {createTestableScheduler} = Cu.import("resource:///modules/readinglist/Scheduler.jsm", {});
Cu.import("resource://gre/modules/Preferences.jsm");
Cu.import("resource://gre/modules/Timer.jsm");

// Log rotation needs a profile dir.
do_get_profile();

let prefs = new Preferences("readinglist.scheduler.");

function promiseObserver(topic) {
  return new Promise(resolve => {
    let obs = (subject, topic, data) => {
      Services.obs.removeObserver(obs, topic);
      resolve(data);
    }
    Services.obs.addObserver(obs, topic, false);
  });
}

function createScheduler(options) {
  // avoid typos in the test and other footguns in the options.
  let allowedOptions = ["expectedDelay", "expectNewTimer", "syncFunction"];
  for (let key of Object.keys(options)) {
    if (!allowedOptions.includes(key)) {
      throw new Error("Invalid option " + key);
    }
  }
  let scheduler = createTestableScheduler();
  // make our hooks
  let syncFunction = options.syncFunction || Promise.resolve;
  scheduler._engine.sync = syncFunction;
  // we expect _setTimeout to be called *twice* - first is the initial sync,
  // and there's no need to test the delay used for that. options.expectedDelay
  // is to check the *subsequent* timer.
  let numCalls = 0;
  scheduler._setTimeout = function(delay) {
    ++numCalls;
    print("Test scheduler _setTimeout call number " + numCalls + " with delay=" + delay);
    switch (numCalls) {
      case 1:
        // this is the first and boring schedule as it initializes - do nothing
        // other than return a timer that fires immediately.
        return setTimeout(() => scheduler._doSync(), 0);
        break;
      case 2:
        // This is the one we are interested in, so check things.
        if (options.expectedDelay) {
          // a little slop is OK as it takes a few ms to actually set the timer
          ok(Math.abs(options.expectedDelay * 1000 - delay) < 500, [options.expectedDelay * 1000, delay]);
        }
        // and return a timeout that "never" fires
        return setTimeout(() => scheduler._doSync(), 10000000);
        break;
      default:
        // This is unexpected!
        ok(false, numCalls);
    }
  };
  // And a callback made once we've determined the next delay. This is always
  // called even if _setTimeout isn't (due to no timer being created)
  scheduler._onAutoReschedule = () => {
    // Most tests expect a new timer, so this is "opt out"
    let expectNewTimer = options.expectNewTimer === undefined ? true : options.expectNewTimer;
    ok(expectNewTimer ? scheduler._timer : !scheduler._timer);
  }
  // calling .init fires things off...
  scheduler.init();
  return scheduler;
}

add_task(function* testSuccess() {
  // promises which resolve once we've got all the expected notifications.
  let allNotifications = [
    promiseObserver("readinglist:sync:start"),
    promiseObserver("readinglist:sync:finish"),
  ];
  // New delay should be "as regularly scheduled".
  prefs.set("schedule", 100);
  let scheduler = createScheduler({expectedDelay: 100});
  yield Promise.all(allNotifications);
  scheduler.finalize();
});

add_task(function* testOffline() {
  let scheduler = createScheduler({expectNewTimer: false});
  Services.io.offline = true;
  ok(!scheduler._canSync(), "_canSync is false when offline.")
  ok(!scheduler._timer, "there is no current timer while offline.")
  Services.io.offline = false;
  ok(scheduler._canSync(), "_canSync is true when online.")
  ok(scheduler._timer, "there is a new timer when back online.")
  scheduler.finalize();
});

add_task(function* testRetryableError() {
  let allNotifications = [
    promiseObserver("readinglist:sync:start"),
    promiseObserver("readinglist:sync:error"),
  ];
  prefs.set("retry", 10);
  let scheduler = createScheduler({
    expectedDelay: 10,
    syncFunction: () => Promise.reject("transient"),
  });
  yield Promise.all(allNotifications);
  scheduler.finalize();
});

add_task(function* testAuthError() {
  prefs.set("retry", 10);
  // We expect an auth error to result in no new timer (as it's waiting for
  // some indication it can proceed), but with the next delay being a normal
  // "retry" interval (so when we can proceed it is probably already stale, so
  // is effectively "immediate")
  let scheduler = createScheduler({
    expectedDelay: 10,
    syncFunction:  () => {
      return Promise.reject(ReadingListScheduler._engine.ERROR_AUTHENTICATION);
    },
    expectNewTimer: false
  });
  // XXX - TODO - send an observer that "unblocks" us and ensure we actually
  // do unblock.
  scheduler.finalize();
});

add_task(function* testBackoff() {
  let scheduler = createScheduler({expectedDelay: 1000});
  Services.obs.notifyObservers(null, "readinglist:backoff-requested", 1000);
  // XXX - this needs a little love as nothing checks createScheduler actually
  // made the checks we think it does.
  scheduler.finalize();
});

function run_test() {
  run_next_test();
}
