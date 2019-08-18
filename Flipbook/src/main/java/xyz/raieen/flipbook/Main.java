package xyz.raieen.flipbook;

import org.jcodec.api.JCodecException;

import java.awt.print.PrinterException;
import java.io.File;
import java.io.IOException;

/**
 * Generates a set of images that can be printed and turned into a flip book from a given mp4 file.
 * <p>
 * Usage: ./flipbooktool.jar INPUT_FILE FRAME_COUNT PRINT_IMAGES OUTPUT_FOLDER ROWS COLS
 * Usage: ./flipbooktool.jar INPUT_FILE FRAME_START FRAME_END PRINT_IMAGES OUTPUT_FOLDER ROWS COLS
 * <p>
 * Eg. ./flipbooktool.jar hello.mp4 20 false output/ 2 3
 */
public class Main {

    public static void main(String[] args) {
        // Arg checking
        if (args.length != 6 && args.length != 7) {
            System.err.println("Usage: ./flipbooktool.jar INPUT_FILE [FRAME_START] FRAME_COUNT PRINT_IMAGES OUTPUT_FOLDER ROWS COLS");
            System.exit(1);
        }

        int startFrameOffset = args.length == 7 ? 1 : 0; // use offset to avoid writing two cases (with/without frameStart)

        String inputFile = args[0], outputFolder = args[3 + startFrameOffset];
        int startFrame = args.length == 7 ? parseIntegerArgument(args[1]) : 0, endFrame = parseIntegerArgument(args[1 + startFrameOffset]);
        int rows = parseIntegerArgument(args[4 + startFrameOffset]), cols = parseIntegerArgument(args[5 + startFrameOffset]);
        boolean printImages = Boolean.parseBoolean(args[2 + startFrameOffset]);

        File output = new File(outputFolder);
        if (output.exists()) {
            if (output.isFile() || !output.canWrite()) {
                System.err.println(String.format("%s is an invalid directory.", outputFolder));
                System.exit(1);
            }
        } else {
            output.mkdirs();
        }

        Flipbook flipbook = new Flipbook(new File(inputFile), startFrame, endFrame, rows, cols);
        try {
            flipbook.generateImages(outputFolder, printImages);
        } catch (IOException | JCodecException e) {
            System.err.println(String.format("Something went wrong! %s", e.getMessage()));
        } catch (PrinterException e) {
            System.err.println(String.format("Unable to print images! %s", e.getMessage()));
        }
    }

    private static int parseIntegerArgument(String arg) {
        try {
            int x = Integer.parseInt(arg);
            if (x < 0) throw new NumberFormatException();
            return x;
        } catch (NumberFormatException e) {
            System.err.println(String.format("%s is not an integral number.", arg));
        }
        return 0;
    }
}