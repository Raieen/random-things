package xyz.raieen.flipbook;

import org.jcodec.api.FrameGrab;
import org.jcodec.api.JCodecException;
import org.jcodec.common.model.Picture;
import org.jcodec.scale.AWTUtil;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;
import java.io.File;
import java.io.IOException;

// TODO: Unit tests to be a good person.
public class Flipbook {

    // Constants
    private final int FRAME_ROWS = 3;
    private final int FRAME_COLS = 3;

    private File file;
    private int frameCount;

    public Flipbook(File file, int frameCount) {
        this.file = file;
        this.frameCount = frameCount;
    }

    /**
     * Creates sheets (frameCount mod FRAME_ROWS / FRAME_COLS) of FRAME_ROWS * FRAME_COLS in folder.
     *
     * @param folder      folder where the sheets saved to
     * @param printImages prints the images iff true
     */
    public void generateImages(String folder, boolean printImages) throws IOException, JCodecException, PrinterException {
        // Need to get the initial frame for image width/height
        BufferedImage frameZero = getImage(0);
        int frameWidth = frameZero.getWidth(), frameHeight = frameZero.getHeight();
        int imageSheet = 0; // The sheet which has FRAME_ROWS * FRAME_COLS number of frames on it

        // Go through frameCount frames [0, frameCount-1]
        for (int frame = 0; frame < frameCount; frame++) {
            BufferedImage bufferedImage = new BufferedImage(
                    frameWidth * FRAME_ROWS, frameHeight * FRAME_COLS, BufferedImage.TYPE_INT_RGB);
            Graphics2D graphics2D = bufferedImage.createGraphics();

            // Draw this image sheet of FRAME_ROWS * FRAME_COLS frames.
            for (int frameX = 0; frameX < FRAME_ROWS; frameX++) {
                for (int frameY = 0; frameY < FRAME_COLS; frameY++) {
                    System.out.println(String.format("Drawing frame %d.", frame));
                    BufferedImage frameImage = getImage(frame);
                    graphics2D.drawImage(frameImage, null, frameX * frameWidth, frameY * frameHeight);
                    frame++;
                }
            } // TODO: 14/08/19 Some check in here for frame < frameCount.

            ImageIO.write(bufferedImage, "png", new File(String.format("%s/sheet-%d.png", folder, imageSheet)));
            System.out.println(String.format("Generated sheet %d.", imageSheet++));

            // Print if requested
            if (printImages) {
                printImage(bufferedImage);
            }
        }
    }


    /**
     * Opens a print dialog and prints the image with the user's specifications from the dialog.
     *
     * @param bufferedImage Image to print
     */
    private void printImage(BufferedImage bufferedImage) throws PrinterException {
        PrinterJob printerJob = PrinterJob.getPrinterJob();

        printerJob.setPrintable((graphics, pageFormat, i) -> {
            if (i != 0) return Printable.NO_SUCH_PAGE;

            pageFormat.setOrientation(PageFormat.LANDSCAPE);

            graphics.translate((int) pageFormat.getImageableX(), (int) pageFormat.getImageableY());
            graphics.drawImage(bufferedImage, 0, 0, (int) printerJob.defaultPage().getImageableWidth(), (int) printerJob.defaultPage().getImageableHeight(), null);
            return Printable.PAGE_EXISTS;
        });
        if (printerJob.printDialog()) printerJob.print();
    }


    /**
     * Returns the given frame number's image frame from file.
     *
     * @param frame Frame of image.
     * @return Buffered image at frame in file
     */
    private BufferedImage getImage(int frame) throws IOException, JCodecException {
        Picture picture = FrameGrab.getFrameFromFile(file, frame);
        return AWTUtil.toBufferedImage(picture);
    }
}
