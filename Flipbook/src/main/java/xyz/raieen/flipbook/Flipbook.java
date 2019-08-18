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

public class Flipbook {

    // Constants
    private final File file;
    private int frameStart = 0;
    private final int frameEnd;
    private final int rows;
    private final int cols;

    /**
     * Flipbook which represents [0, x] frames where
     * x is frameEnd mod rows / cols (Don't waste paper, just print if you have a full sheet!!)
     *
     * @param file     Video file
     * @param frameEnd frames for the flip book.
     * @param rows     Rows of imgaes in the sheet
     * @param cols     Columns of images in the sheet
     */
    public Flipbook(File file, int frameEnd, int rows, int cols) {
        this.file = file;
        this.frameEnd = frameEnd;
        this.rows = rows;
        this.cols = cols;
    }

    /**
     * Flipbook which represents [frameStart, x] frames where
     * x is frameEnd mod rows / cols (Don't waste paper, just print if you have a full sheet!!)
     *
     * @param file       Video file
     * @param frameStart starting frame for the video
     * @param frameEnd   ending frame for the video
     * @param rows       Rows of imgaes in the sheet
     * @param cols       Columns of images in the sheet
     */
    public Flipbook(File file, int frameStart, int frameEnd, int rows, int cols) {
        this.file = file;
        this.frameStart = frameStart;
        this.frameEnd = frameEnd;
        this.rows = rows;
        this.cols = cols;
    }

    /**
     * Creates sheets (frameEnd mod rows / cols) of rows * cols in folder.
     *
     * @param folder      folder where the sheets saved to
     * @param printImages prints the images iff true
     */
    public void generateImages(String folder, boolean printImages) throws IOException, JCodecException, PrinterException {
        // Need to get the initial frame for image width/height
        BufferedImage frameZero = getImage(0);
        int frameWidth = frameZero.getWidth(), frameHeight = frameZero.getHeight();
        int imageSheet = 0; // The sheet which has rows * cols number of frames on it

        // Go through frameEnd frames [0, frameEnd-1]
        for (int frame = frameStart; frame < frameEnd; frame++) {
            BufferedImage bufferedImage = new BufferedImage(
                    frameWidth * rows, frameHeight * cols, BufferedImage.TYPE_INT_RGB);
            Graphics2D graphics2D = bufferedImage.createGraphics();

            // Draw this image sheet of rows * cols frames.
            for (int frameX = 0; frameX < rows; frameX++) {
                for (int frameY = 0; frameY < cols; frameY++) {
                    System.out.println(String.format("Drawing frame %d.", frame));
                    BufferedImage frameImage = getImage(frame);
                    graphics2D.drawImage(frameImage, null, frameX * frameWidth, frameY * frameHeight);
                    frame++;
                }
            }

            ImageIO.write(bufferedImage, "jpg", new File(String.format("%s/sheet-%d.png", folder, imageSheet)));
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
            graphics.drawImage(bufferedImage, 0, 0, (int) printerJob.defaultPage().getImageableWidth(),
                    (int) printerJob.defaultPage().getImageableHeight(), null);
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
