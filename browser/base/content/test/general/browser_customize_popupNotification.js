/*
Any copyright is dedicated to the Public Domain.
http://creativecommons.org/publicdomain/zero/1.0/
*/

function test() {
  waitForExplicitFinish();
  let newWin = OpenBrowserWindow();
  whenDelayedStartupFinished(newWin, function () {
    // Remove the URL bar
    newWin.gURLBar.parentNode.removeChild(newWin.gURLBar);

    waitForFocus(function () {
      let PN = newWin.PopupNotifications;
      PN.transitionsEnabled = false;
      try {
        let notification = PN.show(newWin.gBrowser.selectedBrowser, "some-notification", "Some message");
        ok(notification, "showed the notification");
        ok(PN.isPanelOpen, "panel is open");
        is(PN.panel.anchorNode, newWin.gBrowser.selectedTab, "notification is correctly anchored to the tab");
        PN.panel.hidePopup();
      } catch (ex) {
        ok(false, "threw exception: " + ex);
      }
      PN.transitionsEnabled = true;
      newWin.close();
      finish();
    }, newWin);
  });
}
