package org.mozilla.gecko.tests;

import org.mozilla.gecko.Actions;
import org.mozilla.gecko.PaintedSurface;

import android.net.Uri;

/**
 * A test to ensure that when an input field is focused, it is not obscured by the VKB.
 * - Loads a page with an input field past the bottom of the visible area.
 * - scrolls down to make the input field visible at the bottom of the screen.
 * - taps on the input field to bring up the VKB
 * - verifies that the input field is still visible.
 */
public class testVkbOverlap extends PixelTest {
    private static final int CURSOR_BLINK_PERIOD = 500;
    private static final int LESS_THAN_CURSOR_BLINK_PERIOD = CURSOR_BLINK_PERIOD - 50;
    private static final int PAGE_SETTLE_TIME = 5000;

    public void testVkbOverlap() {
        blockForGeckoReady();
        testSetup("initial-scale=1.0, user-scalable=no", false);
        testSetup("initial-scale=1.0", false);
        testSetup("", "phone".equals(mDevice.type));
    }

    private void testSetup(String viewport, boolean shouldZoom) {
        loadAndPaint(getAbsoluteUrl("/robocop/test_viewport.sjs?metadata=" + Uri.encode(viewport)));

        // scroll to the bottom of the page and let it settle
        Actions.RepeatedEventExpecter paintExpecter = mActions.expectPaint();
        MotionEventHelper meh = new MotionEventHelper(getInstrumentation(), mDriver.getGeckoLeft(), mDriver.getGeckoTop());
        meh.dragSync(10, 150, 10, 50);

        // the input field has a green background, so let's count the number of green pixels
        int greenPixelCount = 0;

        PaintedSurface painted = waitForPaint(paintExpecter);
        paintExpecter.unregisterListener();
        try {
            greenPixelCount = countGreenPixels(painted);
        } finally {
            painted.close();
        }

        mAsserter.ok(greenPixelCount > 0, "testInputVisible", "Found " + greenPixelCount + " green pixels after scrolling");

        paintExpecter = mActions.expectPaint();
        // the input field should be in the bottom-left corner, so tap thereabouts
        meh.tap(5, mDriver.getGeckoHeight() - 5);

        // After tapping in the input field, the page needs some time to do stuff, like draw and undraw the focus highlight
        // on the input field, trigger the VKB, process any resulting events generated by the system, and scroll the page. So
        // we give it a few seconds to do all that. We are sufficiently generous with our definition of "few seconds" to
        // prevent intermittent test failures.
        try {
            Thread.sleep(PAGE_SETTLE_TIME);
        } catch (InterruptedException ie) {
            ie.printStackTrace();
        }

        // now that the focus is in the text field we will repaint every 500ms as the cursor blinks, so we need to use a smaller
        // "no paints" threshold to consider the page painted
        paintExpecter.blockUntilClear(LESS_THAN_CURSOR_BLINK_PERIOD);
        paintExpecter.unregisterListener();
        painted = mDriver.getPaintedSurface();
        try {
            // if the vkb scrolled into view as expected, then the number of green pixels now visible should be about the
            // same as it was before, since the green pixels indicate the text input is in view. use a fudge factor of 0.9 to
            // account for borders and such of the text input which might still be out of view.
            int newCount = countGreenPixels(painted);

            // if zooming is allowed, the number of green pixels visible should have increased substatially
            if (shouldZoom) {
                mAsserter.ok(newCount > greenPixelCount * 1.5, "testVkbOverlap", "Found " + newCount + " green pixels after tapping; expected " + greenPixelCount);
            } else {
                mAsserter.ok((Math.abs(greenPixelCount - newCount) / greenPixelCount < 0.1), "testVkbOverlap", "Found " + newCount + " green pixels after tapping; expected " + greenPixelCount);
            }
        } finally {
            painted.close();
        }
    }

    private int countGreenPixels(PaintedSurface painted) {
        int count = 0;
        for (int y = painted.getHeight() - 1; y >= 0; y--) {
            for (int x = painted.getWidth() - 1; x >= 0; x--) {
                int pixel = painted.getPixelAt(x, y);
                int r = (pixel >> 16) & 0xFF;
                int g = (pixel >> 8) & 0xFF;
                int b = (pixel & 0xFF);
                if (g > (r + 0x30) && g > (b + 0x30)) {
                    // there's more green in this pixel than red or blue, so count it.
                    // the reason this is so hacky-looking is because even though green is supposed to
                    // be (r,g,b) = (0x00, 0x80, 0x00), the GL readback ends up coming back quite
                    // different.
                    count++;
                }
                // uncomment for debugging:
                // if (pixel != -1) mAsserter.dumpLog("Pixel at " + x + ", " + y + ": " + Integer.toString(pixel, 16));
            }
        }
        return count;
    }
}
