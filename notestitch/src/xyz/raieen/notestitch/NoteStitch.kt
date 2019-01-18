package xyz.raieen.notestitch

import java.io.File
import java.io.FileWriter
import java.util.*

// Image Extensions
val imageExts = arrayOf("png", "jpg", "jpeg")

/**
 * @param args specify directory then output... ie. ~/Notes/Math notes.md
 */
fun main(args: Array<String>) {
    if(args.size != 2) {
        println("Directory and output required.")
        return
    }

    val directory = args[0]
    val output = args[1]

    if (directory.isNullOrEmpty() || output.isNullOrEmpty()) {
        println("Directory and output required.")
        return
    }

    if (!File(directory).exists()) {
        println("Directory not found.")
        return
    }

    // Preserve order
    val images: LinkedHashMap<String, ArrayList<String>> = linkedMapOf()

    // Get all files in directory
    File(directory).walkTopDown().sortedBy {
        it.name
    }.forEach {
        if (it.isFile) {
            if (imageExts.contains(it.extension)) {
                // Image tag ie. tag_0 => tag
                var imageTag = it.nameWithoutExtension;

                if (imageTag.contains("_")) {
                    imageTag = imageTag.split("_")[0]
                }

                var filePath = File(directory).toURI().relativize(it.toURI()).path

                if (images.containsKey(imageTag)) {
                    images[imageTag]?.add(filePath)
                } else {
                    images[imageTag] = arrayListOf(filePath)
                }
            }
        }
    }

    // Writing markdown to file
    val file = File(output)

    if (!file.exists()) {
        file.createNewFile()
    }

    var fileWriter = FileWriter(file)

    for (tag in images.keys) {
        fileWriter.append("## $tag\n\n")

        images[tag]?.forEach {
            fileWriter.append("![]($it)\n\n")
        }
    }

    fileWriter.close()
}