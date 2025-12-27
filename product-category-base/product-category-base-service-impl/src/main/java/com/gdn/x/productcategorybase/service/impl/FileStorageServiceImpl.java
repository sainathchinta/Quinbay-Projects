package com.gdn.x.productcategorybase.service.impl;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.ImageFileCopyDTO;
import com.gdn.x.productcategorybase.executor.UploadFinalImageToGcsProcess;
import com.gdn.x.productcategorybase.executor.UploadImageToGcsProcess;
import com.gdn.x.productcategorybase.properties.GcsProperties;
import com.gdn.x.productcategorybase.service.FileStorageService;
import com.gdn.x.productcategorybase.service.GcsService;
import com.google.cloud.storage.Bucket;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class FileStorageServiceImpl implements FileStorageService {

  @Autowired
  private GcsProperties gcsProperties;

  @Autowired
  private GcsService gcsService;

  @Autowired
  private ExecutorService imageUploadExecutorService;

  @Autowired
  @Qualifier("sourceImageBucket")
  private Bucket sourceImageBucket;

  @Autowired
  @Qualifier("finalImageBucket")
  private Bucket finalImageBucket;

  @Value("${image.source.directory}")
  private String imageSourceDirectory;

  @Value("${full.image.final.directory}")
  private String fullImageDirectory;

  @Value("${medium.image.final.directory}")
  private String mediumImageDirectory;

  @Value("${thumbnail.image.final.directory}")
  private String thumbnailImageDirectory;

  @Value("${enable.migrated.final.image.deletion}")
  private boolean migratedFinalImageDeletionEnabled;

  @Value("${avoid.duplicate.calls.gcs.uploads}")
  private boolean avoidDuplicateCallsToGcsForImageUploads;

  private static final String CATALOG_IMAGE = "catalog-image";
  private static final String DOUBLE_SLASH = "//";

  @Override
  public Image updateLocationForImages(Image image, String productCode, String imageSourceDirectory,
    String fullImageSourceDirectory, Set<String> uploadedImageLocations, AtomicInteger imageUploadCounter) throws Exception {
    String filename = null;
    if (!image.getLocationPath().contains(productCode)) {
      String sourcePath = null;
      if (StringUtils.isEmpty(fullImageSourceDirectory)) {
        sourcePath = imageSourceDirectory;
      } else {
        sourcePath = image.isActive() ? fullImageSourceDirectory : imageSourceDirectory;
      }
      filename = getImageFilename(image, productCode, imageSourceDirectory,
        uploadedImageLocations, sourcePath, imageUploadCounter);
      if (gcsProperties.isSourceImageEnabled()) {
        image.setLocationPath(
          (gcsProperties.getPathPrefix() + File.separator + productCode + File.separator + filename).replaceAll("//",
            "/"));
      } else {
        image.setLocationPath((productCode + File.separator + filename).replaceAll("//", "/"));
      }
      image.setActive(false);
      uploadedImageLocations.add(getFileName(image));
    }
    return image;
  }

  private String getImageFilename(Image image, String productCode, String imageSourceDirectory,
    Set<String> uploadedImageLocations, String sourcePath, AtomicInteger imageUploadCounter) throws Exception {
    String filename;
    if (avoidDuplicateCallsToGcsForImageUploads && !uploadedImageLocations.contains(getFileName(image))) {
      filename = fetchImageFromGcs(image, productCode, imageSourceDirectory, sourcePath);
      imageUploadCounter.getAndIncrement();
      log.info("FileName : {} was uploaded for product : {} ", filename, productCode);
    } else {
      filename = avoidDuplicateCallsToGcsForImageUploads
        ? setFileNameForImageNotDownloadedFromGcs(sourcePath, image)
        : fetchImageFromGcs(image, productCode, imageSourceDirectory, sourcePath);
      log.info("skipped image upload to file for fileName : {}", filename);
    }
    return filename;
  }

  private String fetchImageFromGcs(Image image, String productCode, String imageSourceDirectory,
    String sourcePath) throws Exception {
    String filename;
    ImageFileCopyDTO imageFileCopyDTO = downloadImageFile(sourcePath, image, productCode);

    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(imageFileCopyDTO.getFilename()),
      ErrorMessage.FILE_NAME_WAS_EMPTY_WHILE_COPYING_IMAGE_FROM_SOURCE_TO_TARGET.getMessage());
    filename = uploadImageFile(image, productCode, imageSourceDirectory, imageFileCopyDTO.getSourceFile(),
      imageFileCopyDTO.getFilename(), imageFileCopyDTO.getImageDownloadFromGcsBytes(),
      imageFileCopyDTO.isImageDownloadedFromGcs());
    return filename;
  }

  private String setFileNameForImageNotDownloadedFromGcs(String sourcePath, Image image) {
    if(!image.isActive() && !image.getLocationPath().contains(gcsProperties.getPathPrefix())){
      return new File((sourcePath + File.separator + image.getLocationPath()).replaceAll(DOUBLE_SLASH, File.separator)).getName();
    }
    return getFileName(image);
  }

  private ImageFileCopyDTO downloadImageFile(String sourcePath, Image image, String productCode) {
    log.info("Downloading image for copy. sourcePath : {} , image : {} , productCode : {} ", sourcePath, image,
        productCode);
    ImageFileCopyDTO imageFileCopyDTO = new ImageFileCopyDTO();
    imageFileCopyDTO.setFilename(getFileName(image));
    if (!image.isActive() && image.getLocationPath().contains(gcsProperties.getPathPrefix())) {
      imageFileCopyDTO.setImageDownloadFromGcsBytes(downloadFileFromGcs(productCode, imageFileCopyDTO.getFilename()));
      imageFileCopyDTO.setImageDownloadedFromGcs(true);
    } else if (image.isActive()) {
      if (gcsProperties.isFileStoreToGcsMigrationCompleted()) {
        imageFileCopyDTO.setImageDownloadFromGcsBytes(
            downloadFileFromGcsFinalMigrationDone(image.getLocationPath()));
        imageFileCopyDTO.setImageDownloadedFromGcs(true);
      } else {
        downloadFileFromGcsFinalMigrationNotDone(imageFileCopyDTO, image,
            sourcePath);
      }
    } else {
      imageFileCopyDTO.setSourceFile(
          new File((sourcePath + File.separator + image.getLocationPath()).replaceAll(DOUBLE_SLASH, File.separator)));
      String path = (sourcePath + File.separator + image.getLocationPath()).replaceAll(DOUBLE_SLASH, File.separator);
      log.info("Getting file from filestore. path : {} ", path);
      imageFileCopyDTO.setSourceFile(new File(path));
      imageFileCopyDTO.setFilename(imageFileCopyDTO.getSourceFile().getName());
    }
    return imageFileCopyDTO;
  }

  private void downloadFileFromGcsFinalMigrationNotDone(ImageFileCopyDTO imageFileCopyDTO, Image image, String sourcePath) {
    String pathForDownloadImageFromGcs =
        (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalFullImageDirectory()
            + File.separator + image.getLocationPath()).replaceAll(
            DOUBLE_SLASH, File.separator);
    try {
      log.info("Downloading image from Gcs finalImageDirectory. pathForDownloadImageFromGcs : {} ",
          pathForDownloadImageFromGcs);
      imageFileCopyDTO.setImageDownloadFromGcsBytes(
          gcsService.downloadFile(gcsProperties.getFinalImageBucketName(), pathForDownloadImageFromGcs));
      imageFileCopyDTO.setImageDownloadedFromGcs(true);
    } catch (ApplicationRuntimeException e) {
      log.error("File not found. path : {} , error - ", pathForDownloadImageFromGcs, e);
      if (e.getErrorCodes().equals(ErrorCategory.DATA_NOT_FOUND)) {
        String path = (sourcePath + File.separator + image.getLocationPath()).replaceAll(DOUBLE_SLASH, File.separator);
        log.info("Getting file from filestore as image not found in Gcs. path : {} ", path);
        imageFileCopyDTO.setSourceFile(new File(path));
        imageFileCopyDTO.setFilename(imageFileCopyDTO.getSourceFile().getName());
      } else {
        throw e;
      }
    }
  }

  private byte[] downloadFileFromGcsFinalMigrationDone(String locationPath) {
    byte[] imageDownloadedFromGcsBytes;
    String pathForDownloadImageFromGcs =
        (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalFullImageDirectory()
            + File.separator + locationPath).replaceAll(DOUBLE_SLASH,
            File.separator);
    log.info("Downloading file from Gcs for copy. pathForDownloadImageFromGcs : {} ", pathForDownloadImageFromGcs);
    imageDownloadedFromGcsBytes = gcsService.downloadFile(gcsProperties.getFinalImageBucketName(),
        pathForDownloadImageFromGcs.replaceAll(DOUBLE_SLASH, File.separator));
    return imageDownloadedFromGcsBytes;
  }

  private byte[] downloadFileFromGcs(String productCode, String filename) {
    String pathForDownloadImageFromGcs =
        gcsProperties.getSourceImageDirectory() + File.separator + gcsProperties.getPathPrefix() + File.separator
            + productCode + File.separator + filename;
    log.info("Downloading file from Gcs. productCode : {} , pathForDownloadImageFromGcs : {} ", productCode,
        pathForDownloadImageFromGcs);
    return gcsService.downloadFile(gcsProperties.getSourceImageBucketName(),
        pathForDownloadImageFromGcs.replaceAll(DOUBLE_SLASH, File.separator));
  }

  private String uploadImageFile(Image image, String productCode, String imageSourceDirectory,
      File sourceFile, String filename, byte[] imageDownloadFromGcsBytes, boolean imageDownloadedFromGcs)
      throws Exception {
    if (gcsProperties.isSourceImageEnabled()) {
      if (imageDownloadedFromGcs) {
        uploadFileToGcs(productCode, filename, imageDownloadFromGcsBytes);
      } else {
        uploadFileToGcs(productCode, filename, Files.readAllBytes(sourceFile.toPath()));
      }
    } else {
      if (imageDownloadedFromGcs) {
        uploadFileToFileStoreFromBytes(productCode, imageSourceDirectory, filename, imageDownloadFromGcsBytes);
      } else {
        uploadFileToFileStore(image, productCode, imageSourceDirectory, sourceFile, filename);
      }
    }
    return filename;
  }

  @Override
  public void uploadFileToFileStore(Image image, String productCode, String imageSourceDirectory, File sourceFile,
      String filename) throws IOException {
    String path =
        (imageSourceDirectory + File.separator + productCode + File.separator + filename).replaceAll(DOUBLE_SLASH,
            File.separator);
    File targetFile = new File(path);
    try {
      targetFile.getParentFile().mkdirs();
      FileUtils.copyFile(sourceFile, targetFile);
      log.info("Copied the image {} from source location : {} to target location {}", filename,
          sourceFile.getAbsolutePath(), targetFile.getAbsoluteFile());
    } catch (Exception e) {
      log.error("error copying the image from source {} to target {}", image.getLocationPath(),
          productCode + File.separator + filename, e);
      throw e;
    }
  }

  public void uploadFileToFileStoreFromBytes(String productCode, String imageSourceDirectory, String filename,
      byte[] imageDownloadFromGcsBytes) throws IOException {
    String path =
        (imageSourceDirectory + File.separator + productCode + File.separator + filename).replaceAll(DOUBLE_SLASH,
            File.separator);
    File targetFile = new File(path);
    log.info("Writing file to filestore. targetFilePath : {} ", targetFile.getAbsoluteFile());
    FileUtils.writeByteArrayToFile(targetFile, imageDownloadFromGcsBytes);
  }

  private void uploadFileToGcs(String productCode, String filename, byte[] imageDownloadedFromGcsBytes)
      throws Exception {
    String pathForUploadImageToGcs =
        gcsProperties.getSourceImageDirectory() + File.separator + gcsProperties.getPathPrefix() + File.separator
            + productCode + File.separator + filename;
    GdnPreconditions.checkArgument(Objects.nonNull(imageDownloadedFromGcsBytes),
        ErrorMessage.IMAGE_DOWNLOADED_FROM_GCS_WAS_NULL.getMessage());
    log.info("Uploading the resized file to GCS. pathForUploadImageToGcs : {} ", pathForUploadImageToGcs);
    gcsService.uploadFile(sourceImageBucket, pathForUploadImageToGcs, imageDownloadedFromGcsBytes);
  }

  private String getFileName(Image image) {
    return image.getLocationPath().substring(image.getLocationPath().lastIndexOf(File.separator)+1);
  }

  @Override
  public void deleteImages(String locationPath) {
    if (locationPath.contains(CATALOG_IMAGE)) {
      deleteFromGcs(locationPath);
    } else {
      File image = new File(imageSourceDirectory + locationPath);
      if (image.exists()) {
        image.delete();
      }
    }
  }

  @Override
  public void deleteFromGcs(String locationPath) {
    String bucketName = gcsProperties.getSourceImageBucketName();
    boolean isDeleted = gcsService.deleteFile(bucketName,
        (gcsProperties.getSourceImageDirectory() + File.separator + locationPath).replaceAll("//", "/"));
    if (isDeleted) {
      log.info("Deleted image from GCS with location : {}", locationPath);
    } else {
      log.error("Failed to delete image from GCS with location : {}", locationPath);
    }
  }

  @Override
  public boolean isFinalImageFileExist(String imagePath) {
    boolean migrationCompleted = gcsProperties.isFileStoreToGcsMigrationCompleted();
    boolean fileFoundInGcs = gcsService.isFileExists(gcsProperties.getFinalImageBucketName(),
        (gcsProperties.getFinalImageDirectory() + Constants.DELIMITER_SLASH + gcsProperties.getFinalFullImageDirectory() + Constants.DELIMITER_SLASH
            + imagePath).replaceAll("//","/"));
    if (!fileFoundInGcs && !migrationCompleted) {
      return !(new File(fullImageDirectory + File.separator + imagePath).exists());
    } else {
      return !fileFoundInGcs;
    }
  }

  @Override
  public void migrateFinalImageFromGfsToGcs(Set<String> locationPathSet) throws Exception {
    log.info("Migrating finalImage from Gfs to Gcs. locationPathSet : {} , parallelUploadEnabled : {} ",
        locationPathSet, gcsProperties.isFinalImageParallelUploadEnabled());
    long startTime = System.currentTimeMillis();
    if (gcsProperties.isFinalImageParallelUploadEnabled()) {
      uploadFinalImageParallel(locationPathSet);
    } else {
      uploadFinalImageSequentially(locationPathSet);
    }
    long endTime = System.currentTimeMillis();
    log.info("Finishing migrating finalImage from Gfs to Gcs. locationPathSet : {} , timeTaken : {} ", locationPathSet,
        endTime - startTime);

    startTime = System.currentTimeMillis();

    if (migratedFinalImageDeletionEnabled) {
      deleteFinalImageFromGfs(locationPathSet);
    }

    endTime = System.currentTimeMillis();
    log.info("Deletion of filestore final Image is done. locationPathSet : {} , timeTaken : {} ", locationPathSet,
        endTime - startTime);
  }

  private void deleteFinalImageFromGfs(Set<String> locationPathSet) {
    for (String locationPath : locationPathSet) {
      File finalFullImageFile = new File(clearDoubleSlash(fullImageDirectory + File.separator + locationPath));
      if (finalFullImageFile.exists()) {
        finalFullImageFile.delete();
      }
      File finalMediumImageFile = new File(clearDoubleSlash(mediumImageDirectory + File.separator + locationPath));
      if (finalMediumImageFile.exists()) {
        finalMediumImageFile.delete();
      }
      File finalThumbnailImageFile =
          new File(clearDoubleSlash(thumbnailImageDirectory + File.separator + locationPath));
      if (finalThumbnailImageFile.exists()) {
        finalThumbnailImageFile.delete();
      }
    }
  }

  private void uploadFinalImageParallel(Set<String> locationPathSet) throws InterruptedException, ExecutionException {
    List<UploadFinalImageToGcsProcess> uploadFinalImageToGcsProcessList = new ArrayList<>();
    for (String locationPath : locationPathSet) {
      UploadFinalImageToGcsProcess uploadFinalImageToGcsProcess = new UploadFinalImageToGcsProcess();
      uploadFinalImageToGcsProcess.setLocationPath(locationPath);
      uploadFinalImageToGcsProcess.setFileStorageService(this);
      uploadFinalImageToGcsProcessList.add(uploadFinalImageToGcsProcess);
    }
    List<Future<Boolean>> finalImageUploadFutureResults =
        imageUploadExecutorService.invokeAll(uploadFinalImageToGcsProcessList);
    for (Future<Boolean> finalImageUploadFutureResult : finalImageUploadFutureResults) {
      finalImageUploadFutureResult.get();
    }
  }

  private void uploadFinalImageSequentially(Set<String> locationPathSet) throws Exception {
    for (String locationPath : locationPathSet) {
      uploadFinalImageFromGfsToGcs(locationPath);
    }
  }

  public boolean isAlreadyGcsImage(String path) {
    return path.contains(gcsProperties.getPathPrefix());
  }

  @Override
  public String getGcsPathWithPrefix(String path) {
    if (path.contains(gcsProperties.getResizePrefix())) {
      return clearDoubleSlash(
          gcsProperties.getResizePrefix() + File.separator + gcsProperties.getPathPrefix() + File.separator
              + path.replaceAll(gcsProperties.getResizePrefix(), StringUtils.EMPTY));
    } else {
      return clearDoubleSlash(gcsProperties.getPathPrefix() + File.separator + path);
    }
  }

  @Override
  public void migrateImagesFromGfsToGcs(String productCode, Set<Pair<String, String>> imagePaths) throws Exception {
    log.info("starting with image migration from gfs to gcs. productCode : {}, imagePaths : {} ", productCode,
        imagePaths);
    long startTime = System.currentTimeMillis();
    if (gcsProperties.isParallelUploadEnabled()) {
      uploadImagesParallel(imagePaths);
    } else {
      uploadImagesSequentially(imagePaths);
    }
    long endTime = System.currentTimeMillis();
    log.info("finished with image migration from gfs to gcs. productCode : {}, imagePaths : {} ", productCode,
        imagePaths);
    log.info("total time taken in migrating images for productCode : {}, timeTaken : {} ", productCode,
        endTime - startTime);

    startTime = System.currentTimeMillis();
    for (Pair<String, String> imagePath : imagePaths) {
      File file = new File(clearDoubleSlash(imageSourceDirectory + File.separator + imagePath.getLeft()));
      if (file.exists()) {
        file.delete();
      }
    }
    endTime = System.currentTimeMillis();
    log.info("total time taken in deletion of images for productCode : {}, timeTaken : {} ", productCode,
        endTime - startTime);
  }

  @Override
  public boolean uploadFileFromGfsToGcs(String fileStorePath, String gcsPath) throws Exception {
    byte[] resizedImageBytesFromFileStore = Files.readAllBytes(Paths.get(fileStorePath));
    gcsService.uploadFile(sourceImageBucket, gcsPath, resizedImageBytesFromFileStore);
    return true;
  }

  private void uploadImagesParallel(Set<Pair<String, String>> imagePaths) throws Exception {
    List<UploadImageToGcsProcess> uploadImageToGcsProcessList = new ArrayList<>();
    for (Pair<String, String> imagePath : imagePaths) {
      uploadImageToGcsProcessList.add(
          new UploadImageToGcsProcess(clearDoubleSlash(imageSourceDirectory + File.separator + imagePath.getLeft()),
              clearDoubleSlash(gcsProperties.getSourceImageDirectory() + File.separator + imagePath.getRight()), this));
    }
    List<Future<Boolean>> imageUploadFutureResults = imageUploadExecutorService.invokeAll(uploadImageToGcsProcessList);
    for (Future<Boolean> imageUploadFutureResult : imageUploadFutureResults) {
      imageUploadFutureResult.get();
    }
  }

  private void uploadImagesSequentially(Set<Pair<String, String>> imagePaths) throws Exception {
    for (Pair<String, String> imagePath : imagePaths) {
      uploadFileFromGfsToGcs(clearDoubleSlash(imageSourceDirectory + File.separator + imagePath.getLeft()),
          clearDoubleSlash(gcsProperties.getSourceImageDirectory() + File.separator + imagePath.getRight()));
    }
  }

  private String clearDoubleSlash(String path) {
    return path.replaceAll("//", "/");
  }

  @Override
  public void deleteFullFinalImagesFromGcs(String locationPath) {
    String bucketName = gcsProperties.getFinalImageBucketName();
    boolean isDeleted = gcsService.deleteFile(bucketName,
        (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalFullImageDirectory()
            + File.separator + locationPath).replaceAll("//", "/"));
    if (isDeleted) {
      log.info("Deleted full images from GCS with location : {}", locationPath);
    } else {
      File image = new File(fullImageDirectory + locationPath);
      if (image.exists()) {
        image.delete();
      }
    }
  }

  @Override
  public void uploadFinalImageFromGfsToGcs(String locationPath) throws Exception {
    uploadFullImageFromGfsToGcs(locationPath);
    uploadMediumImageFromGfsToGcs(locationPath);
    uploadThumbnailImageFromGfsToGcs(locationPath);
  }

  private void uploadThumbnailImageFromGfsToGcs(String locationPath) throws Exception {
    String fileStorePathThumbnail = thumbnailImageDirectory + File.separator + locationPath;
    File thumbnailImageFile = new File(clearDoubleSlash(fileStorePathThumbnail));
    if (thumbnailImageFile.exists()) {
      byte[] thumbnailImageBytesFromFileStore = Files.readAllBytes(Paths.get(thumbnailImageFile.getPath()));
      String gcsPathForThumbnailImage =
          gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalThumbnailImageDirectory()
              + File.separator + locationPath;
      gcsService.uploadFile(finalImageBucket, clearDoubleSlash(gcsPathForThumbnailImage),
          thumbnailImageBytesFromFileStore);
    } else {
      log.error("File not found while migrating finalImage from Gfs to Gcs. thumbnailImageFile : {} ",
          thumbnailImageFile);
    }
  }

  private void uploadMediumImageFromGfsToGcs(String locationPath) throws Exception {
    String fileStorePathMedium = mediumImageDirectory + File.separator + locationPath;
    File mediumImageFile = new File(clearDoubleSlash(fileStorePathMedium));
    if (mediumImageFile.exists()) {
      byte[] mediumImageBytesFromFileStore = Files.readAllBytes(Paths.get(mediumImageFile.getPath()));
      String gcsPathForMediumImage =
          gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalMediumImageDirectory()
              + File.separator + locationPath;
      gcsService.uploadFile(finalImageBucket, clearDoubleSlash(gcsPathForMediumImage), mediumImageBytesFromFileStore);
    } else {
      log.error("File not found while migrating finalImage from Gfs to Gcs. fileStorePathMedium : {} ",
          fileStorePathMedium);
    }
  }

  private void uploadFullImageFromGfsToGcs(String locationPath) throws Exception {
    String fileStorePathFull = fullImageDirectory + File.separator + locationPath;
    File fullImageFile = new File(clearDoubleSlash(fileStorePathFull));
    if (fullImageFile.exists()) {
      byte[] fullImageBytesFromFileStore = Files.readAllBytes(Paths.get(fullImageFile.getPath()));
      String gcsPathForFullImage =
          gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalFullImageDirectory()
              + File.separator + locationPath;
      gcsService.uploadFile(finalImageBucket, clearDoubleSlash(gcsPathForFullImage), fullImageBytesFromFileStore);
    } else {
      log.error("File not found while migrating finalImage from Gfs to Gcs. fileStorePathFull : {} ",
          fileStorePathFull);
    }
  }

  public void deleteFinalMediumImagesFromGcs(String locationPath) {
    String bucketName = gcsProperties.getFinalImageBucketName();
    boolean isDeleted = gcsService.deleteFile(bucketName,
        (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalMediumImageDirectory()
            + File.separator + locationPath).replaceAll("//", "/"));
    if (isDeleted) {
      log.info("Deleted medium images from GCS with location : {}", locationPath);
    } else {
      File image = new File(mediumImageDirectory + locationPath);
      if (image.exists()) {
        image.delete();
      }
    }
  }

  @Override
  public void deleteFinalThumbnailImagesFromGcs(String locationPath) {
    String bucketName = gcsProperties.getFinalImageBucketName();
    boolean isDeleted = gcsService.deleteFile(bucketName,
        (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalThumbnailImageDirectory()
            + File.separator + locationPath).replaceAll("//", "/"));
    if (isDeleted) {
      log.info("Deleted thumbnail images from GCS with location : {}", locationPath);
    } else {
      File image = new File(thumbnailImageDirectory + locationPath);
      if (image.exists()) {
        image.delete();
      }
    }
  }
}
