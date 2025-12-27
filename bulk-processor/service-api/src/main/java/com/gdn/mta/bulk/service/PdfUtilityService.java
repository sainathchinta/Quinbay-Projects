package com.gdn.mta.bulk.service;

public interface PdfUtilityService {
  /**
   * Creates a new PDF file named {@code mergedFileName}
   * by merging all the PDF files in the directory.
   *
   * @param gcsDirectory must not end with forward slash ( / )
   * @param mergedFilePath must not end with forward slash ( / )
   * @param mergedFileName must not contain forward slash ( / )
   */
  void generateMergedPdfFileFromPdfFilesAtGcsDirectory(String gcsDirectory, String mergedFilePath,
      String mergedFileName) throws Exception;
}
