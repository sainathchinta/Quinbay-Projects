package com.gdn.partners.product.analytics.service.impl.helper;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.zip.GZIPInputStream;

import com.google.api.gax.paging.Page;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.Storage;
import com.google.cloud.storage.StorageBatch;
import lombok.RequiredArgsConstructor;
import org.apache.commons.io.FileUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.gdn.partners.product.analytics.model.enums.FileExtensions;
import com.gdn.partners.product.analytics.properties.FileHelperProperties;
import com.gdn.partners.product.analytics.service.helper.FileHelper;
import com.gdn.partners.product.analytics.service.impl.exception.FileException;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@RequiredArgsConstructor
public class FileHelperImpl implements FileHelper {

  private final FileHelperProperties fileHelperProperties;

  @Qualifier("imageGoogleCloudStorage")
  private final Storage imageGoogleCloudStorage;

  @Override
  public List<String> splitFileIntoSmallFiles(String fileName, String extension, int noOfRecordsPerFile,
    List<String> localFilePathList) {
    return splitFileEveryNLines(fileName, extension, noOfRecordsPerFile, localFilePathList);
  }

  @Override
  public List<String> unZipFile(String fileName, String extension, List<String> storageBlobToLocalByBlobNamePrefix) {
    List<String> uncompressedFilePathList = new ArrayList<>();
    int sequence = 0;
    for (String compressedFilePath : storageBlobToLocalByBlobNamePrefix) {
      String uncompressedFilePath = getFilePath(fileName, extension, sequence);
      try (GZIPInputStream gzipInputStream = new GZIPInputStream(
        new FileInputStream(compressedFilePath));
        FileOutputStream fileOutputStream = new FileOutputStream(uncompressedFilePath)) {
        byte[] buffer = new byte[fileHelperProperties.getBufferSize()];
        int bytesRead;
        while ((bytesRead = gzipInputStream.read(buffer)) > 0) {
          fileOutputStream.write(buffer, 0, bytesRead);
        }
      } catch (IOException e) {
        throw new FileException(e.getMessage(), e);
      }
      uncompressedFilePathList.add(uncompressedFilePath);
      sequence++;
    }
    return uncompressedFilePathList;
  }

  @Override
  @Async
  public void deleteFilesFromAutoQCDirectory() {
    try {
      FileUtils.cleanDirectory(new File(fileHelperProperties.getDirectory()));
    } catch (IOException e) {
      log.error("Exception while deleting files from filestore ", e);
    }
  }

  @Override
  public int deleteDirectoryFromGcs(String bucket, String folderPath) {
    StorageBatch batch = imageGoogleCloudStorage.batch();
    Page<Blob> blobs =
        imageGoogleCloudStorage.list(bucket, Storage.BlobListOption.prefix(folderPath));
    int counter = 0;
    int totalCountOfObjects = 0;
    for (Blob blob : blobs.getValues()) {
      try {
        batch.delete(blob.getBlobId());
        log.info("Deleting the files: {}", blob.getName());
        totalCountOfObjects++;
        counter++;
        if (counter == fileHelperProperties.getBatchSizeForImageDeletion()) {
          batch.submit();
          batch = imageGoogleCloudStorage.batch();
          counter = 0;
        }
      }
      catch (Exception e){
        log.error("Error deleting the file: {}, error- }", blob.getName(), e);
      }
    }
    if (counter != 0) {
      batch.submit();
    }
    return totalCountOfObjects;
  }

  private List<String> splitFileEveryNLines(String fileName, String extension, int numberOfLines, List<String> localFilePathList) {
    List<String> splitFileNames = new ArrayList<>();
    for (String filePath : localFilePathList) {
      try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
        int numberOfFiles = getNumberOfSplitFiles(numberOfLines, filePath);
        for (int i = 1; i <= numberOfFiles; i++) {
          String writeFilePath = getFilePath(fileName + i, extension);
          try (BufferedWriter writer = new BufferedWriter(new FileWriter(writeFilePath))) {
            readNLinesAndWrite(numberOfLines, reader, writer);
          }
          splitFileNames.add(writeFilePath);
        }
      } catch (IOException e) {
        log.error("Exception while splitting file ", e);
        throw new FileException(e.getMessage(), e);
      }
    }
    return splitFileNames;
  }

  private void readNLinesAndWrite(int numberOfLines, BufferedReader reader, BufferedWriter writer) throws IOException {
    for (int j = 1; j <= numberOfLines; j++) {
      String line = reader.readLine();
      if (Objects.nonNull(line)) {
        writer.write(line);
        if (j != numberOfLines) {
          writer.newLine();
        }
      }
    }
  }

  private int getNumberOfSplitFiles(int numberOfLines, String filePath) throws IOException {
    try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
      int numberOfFiles = (int) Math.ceil(reader.lines().count() / (double) numberOfLines);
      log.debug("Splitting file into {} files", numberOfFiles);
      return numberOfFiles;
    }
  }

  String getFilePath(String fileName, String extension) {
    return new StringBuilder(fileHelperProperties.getDirectory()).append(
      fileName.concat(String.valueOf(UUID.randomUUID()))).append(extension).toString();
  }

  private String getFilePath(String fileName, String extension, int sequence) {
    return new StringBuilder(fileHelperProperties.getDirectory()).append(fileName).append(extension)
      .append(sequence).append(FileExtensions.GZIP.getExtension()).toString();
  }
}
