package com.gdn.mta.bulk.service;

import com.gdn.partners.bulk.util.Constant;
import com.google.api.gax.paging.Page;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.BlobId;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.pdfbox.io.MemoryUsageSetting;
import org.apache.pdfbox.multipdf.PDFMergerUtility;
import org.assertj.core.util.Files;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

@Service
@Slf4j
public class PdfUtilityServiceImpl implements PdfUtilityService {
  @Autowired
  private FileStorageService fileStorageService;

  @Override
  public void generateMergedPdfFileFromPdfFilesAtGcsDirectory(String directory,
      String mergedFilePath, String mergedFileName) throws Exception {
    List<BlobId> pdfIdsToBeMerged = new ArrayList<>();
    PDFMergerUtility mergerUtility = new PDFMergerUtility();

    // fetch all PDFs
    Page<Blob> page = fileStorageService.listFilesAtGcsDirectory(directory);
    page.iterateAll().forEach((Blob blob) -> {
      if (MediaType.APPLICATION_PDF_VALUE.equals(blob.getContentType())
          && !blob.getName().equals(mergedFileName)) {
        mergerUtility.addSource(new ByteArrayInputStream(blob.getContent()));
        pdfIdsToBeMerged.add(blob.getBlobId());
      }
    });

    if (CollectionUtils.isEmpty(pdfIdsToBeMerged)) {
      Exception exception = new RuntimeException(
          String.format("no PDF files to merge at %s", directory));
      log.error("#mergePdfFilesAtGcsDirectory error: ", exception);
      throw exception;
    }

    // merge all PDFs
    mergerUtility.setDestinationFileName(mergedFilePath + File.separator + mergedFileName);
    mergerUtility.mergeDocuments(MemoryUsageSetting.setupTempFileOnly());
  }
}
