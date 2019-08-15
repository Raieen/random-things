package xyz.raieen.flipbook;

import org.jcodec.api.JCodecException;

import java.awt.print.PrinterException;
import java.io.File;
import java.io.IOException;

/**
 * Generates a set of images that can be printed and turned into a flip book from a given mp4 file.
 * Usage: java -jar flipbooktool.jar INPUT_FILE FRAME_COUNT PRINT_IMAGES OUTPUT_FOLDER
 */
public class Main {

    public static void main(String[] args) {
        // Arg checking
        if (args.length < 4) {
            System.err.println("Usage: java -jar flipbooktool.jar INPUT_FILE FRAME_COUNT PRINT_IMAGES OUTPUT_FOLDER");
            System.exit(1);
        }

        int frames = 0;
        try {
            frames = Integer.parseInt(args[1]);
            if (frames < 0) throw new NumberFormatException();
        } catch (NumberFormatException e) {
            System.err.println(String.format("%s is not an integral number.", args[1]));
            System.exit(1);
        }
        boolean print = Boolean.parseBoolean(args[2]);

        // Create OUTPUT_FOLDER if it doesn't exist.
        File output = new File(args[3]);
        if (output.exists() && output.isFile() || !output.canWrite()) {
            System.err.println(String.format("%s is an invalid directory.", args[3]));
            System.exit(1);
        }
        // TODO: 14/08/19 Create directory now.

        System.out.println(String.format("Input File %s", args[0]));
        System.out.println(String.format("Frame Count %s", frames));
        System.out.println(String.format("Printing Images? %b", print));
        System.out.println(String.format("Output Folder %s", args[3]));
        Flipbook flipbook = new Flipbook(new File(args[0]), frames);
        try {
            flipbook.generateImages(args[3], print);
        } catch (IOException | JCodecException e) {
            System.err.println(String.format("Something went wrong! %s", e.getMessage()));
        } catch (PrinterException e) {
            System.err.println(String.format("Unable to print images! %s", e.getMessage()));
        }
    }
}