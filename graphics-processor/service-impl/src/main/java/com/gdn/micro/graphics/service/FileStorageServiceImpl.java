package com.gdn.micro.graphics.service;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import com.google.cloud.storage.Blob;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.FileCopyUtils;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.micro.graphics.AvailableSize;
import com.gdn.micro.graphics.domain.event.model.ImageResponse;
import com.gdn.micro.graphics.domain.event.model.ImageResultDetail;
import com.gdn.micro.graphics.model.GraphicDetailCommand;
import com.gdn.micro.graphics.model.GraphicImageDetail;
import com.gdn.micro.graphics.service.config.GcsProperties;
import com.gdn.micro.graphics.service.error.ErrorMessage;
import com.gdn.micro.graphics.util.Constants;
import com.gdn.micro.graphics.utils.GraphicsProcessorHelper;
import com.google.cloud.storage.Bucket;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class FileStorageServiceImpl implements FileStorageService {

  private static final String CATALOG = "catalog";
  private static final String DOUBLE_SLASH = "//";
  private static final String RESIZE = "resize";
  private static final String SCALE = "scale";
  private static final String SEOUL = "seoul";
  private static final String REMOVE_KEY_SUFFIX = ".remove";

  @Autowired
  private GcsService gcsService;

  @Autowired
  private GcsProperties gcsProperties;

  @Autowired
  private ImagePathConfiguration imagePathConfiguration;

  @Autowired
  @Qualifier("sourceImageBucket")
  private Bucket sourceImageBucket;

  @Autowired
  @Qualifier("finalImageBucket")
  private Bucket finalImageBucket;

  @Autowired
  @Qualifier("rmaImageBucket")
  private Bucket rmaImageBucket;

  @Autowired
  @Qualifier("oxfordImageBucket")
  private Bucket oxfordImageBucket;

  @Autowired
  @Qualifier("orderImageBucket")
  private Bucket orderImageBucket;

  @Value("${temp.directory.resize.image}")
  private String tempDirForResizeImage;

  @Value("${temp.directory.scale.image}")
  private String tempDirForScaleImage;

  @Value("${mta.source.directory}")
  private String sourcePrefixPath;

  @Override
  public String getLocalSourcePathOfFile(String sourcePath, String prefixPath, boolean resize,
      String productCodeFileName, boolean isEdited) throws IOException {
    if (sourcePath.contains(gcsProperties.getPathPrefix())) {
      long startTime = System.currentTimeMillis();
      String gcsFilePathForDownload = getPath(resize, sourcePath, productCodeFileName, isEdited);
      String fileStoreTempFilePathForResizeImage = localSourcePath(resize, prefixPath, productCodeFileName);
      File tempFileForResizeImage = new File(fileStoreTempFilePathForResizeImage);
      tempFileForResizeImage.getParentFile().mkdirs();
      if (resize && isEdited) {
        gcsService.downloadFileTo(gcsProperties.getFinalImageBucketName(), gcsFilePathForDownload,
            fileStoreTempFilePathForResizeImage);
      } else {
        gcsService.downloadFileTo(gcsProperties.getSourceImageBucketName(), gcsFilePathForDownload,
            fileStoreTempFilePathForResizeImage);
      }
      sourcePath = fileStoreTempFilePathForResizeImage;
      long endTime = System.currentTimeMillis();
      log.info("Time take to download file from gcs and save to filestore. sourcePath : {} , timeTake : {} ",
          sourcePath, endTime - startTime);
    }
    return sourcePath;
  }

  private String getPath(boolean isResize, String sourcePath, String productCodeFileName, boolean isEdited) {
    if (isResize) {
      if (isEdited) {
        return (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalFullImageDirectory()
            + File.separator + gcsProperties.getPathPrefix() + File.separator + productCodeFileName).replaceAll(
            DOUBLE_SLASH, File.separator);
      } else {
        return (gcsProperties.getSourceImageDirectory() + File.separator + gcsProperties.getPathPrefix()
            + File.separator + productCodeFileName).replaceAll(DOUBLE_SLASH, File.separator)
            .replaceAll(DOUBLE_SLASH, File.separator);
      }
    } else {
      return ((sourcePath.contains(gcsProperties.getResizeImagePrefix()) ?
          gcsProperties.getSourceResizeImageDirectory() :
          gcsProperties.getSourceImageDirectory()) + File.separator + gcsProperties.getPathPrefix() + File.separator
              + productCodeFileName).replaceAll(DOUBLE_SLASH, File.separator);
    }
  }

  private String localSourcePath(boolean resize, String prefixPath, String productCodeFileName) {
    if (resize) {
      return prefixPath + tempDirForResizeImage + UUID.randomUUID().toString() + Constants.SLASH + Constants.UNDERSCORE
          + productCodeFileName.replaceAll("/", "_");
    } else {
      return prefixPath + tempDirForScaleImage + UUID.randomUUID().toString() + Constants.SLASH + Constants.UNDERSCORE
          + productCodeFileName.replaceAll("/", "_");
    }
  }

  @Override
  public void saveImages(String destinationPath, ImageResponse resultDetail, boolean resize,
      String productCodeFileName, Boolean isEdited) throws Exception {
    byte[] resizedImageBytesFromFileStore = Files.readAllBytes(Paths.get(destinationPath));
    productCodeFileName = getProductCodeFileNameForDestinationPath(productCodeFileName);
    if (gcsProperties.isSourceImageEnabled() && resize) {
      long startTime = System.currentTimeMillis();
      String resizeImagePathForGcs =
          gcsProperties.getPathPrefix() + File.separator + productCodeFileName;
      log.info("Uploading image file of resize image to Gcs. destinationPath : {} , resizeImagePathForGcs : {} ",
          destinationPath, resizeImagePathForGcs);
      gcsService.uploadFile(sourceImageBucket,
          gcsProperties.getSourceResizeImageDirectory() + File.separator + resizeImagePathForGcs,
          resizedImageBytesFromFileStore);
      resultDetail.setImagePathLocation(gcsProperties.getResizeImagePrefix() + resizeImagePathForGcs);
      long endTime = System.currentTimeMillis();
      log.info("Time to upload resized image from filestore to gcs. path : {} , timeTaken : {} ", resizeImagePathForGcs,
          endTime - startTime);
      log.info("File downloaded from Gcs. destinationPath : {} , resultDetail : {} , productCodeFileName : {} ",
          destinationPath, resultDetail, productCodeFileName);
    } else if (gcsProperties.isFinalImageEnabled() && (!resize)) {
      if (isEdited) {
        uploadingEditedImageToGcs(destinationPath, resultDetail, productCodeFileName, resizedImageBytesFromFileStore);
      } else {
        String groupCode = getGroupCode(destinationPath);
        uploadingScaledImageToGcs(destinationPath, resultDetail, productCodeFileName, resizedImageBytesFromFileStore,
            groupCode);
      }
    }
  }

  private void uploadingEditedImageToGcs(String destinationPath, ImageResponse resultDetail, String productCodeFileName,
      byte[] resizedImageBytesFromFileStore) throws Exception {
    String editedImagePath = getEditedImagePath(destinationPath, productCodeFileName);
    log.info("Uploading image file of edited scaled image to Gcs. destinationPath : {} , editedImagePath : {} ",
        destinationPath, editedImagePath);
    gcsService.uploadFile(finalImageBucket, editedImagePath, resizedImageBytesFromFileStore);
    resultDetail.setImagePathLocation(getImagePathLocation(editedImagePath));
  }

  private void uploadingScaledImageToGcs(String destinationPath, ImageResponse resultDetail, String productCodeFileName,
      byte[] resizedImageBytesFromFileStore, String groupCode) throws Exception {
    String scaleImagePath = getScaleImagePath(destinationPath, productCodeFileName, groupCode);
    log.info("Uploading image file of scaled image to Gcs. destinationPath : {} , scaleImagePath : {} ",
        destinationPath, scaleImagePath);
    gcsService.uploadFile(finalImageBucket, scaleImagePath, resizedImageBytesFromFileStore);
    resultDetail.setImagePathLocation(getImagePathLocation(scaleImagePath));
  }

  private String getEditedImagePath(String destinationPath, String productCodeFileName) {
    String editedImagePath;
    if (destinationPath.contains(File.separator + AvailableSize.FULL.getName() + File.separator)) {
      editedImagePath =
          gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalFullImageDirectory()
              + File.separator + gcsProperties.getPathPrefix() + File.separator + productCodeFileName;
    } else if (destinationPath.contains(File.separator + AvailableSize.MEDIUM.getName() + File.separator)) {
      editedImagePath =
          gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalMediumImageDirectory()
              + File.separator + gcsProperties.getPathPrefix() + File.separator + productCodeFileName;
    } else if (destinationPath.contains(File.separator + AvailableSize.THUMBNAIL.getName() + File.separator)) {
      editedImagePath =
          gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalThumbnailImageDirectory()
              + File.separator + gcsProperties.getPathPrefix() + File.separator + productCodeFileName;
    } else {
      editedImagePath =
          gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalSeoulImageDirectory()
              + File.separator + gcsProperties.getPathPrefix() + File.separator + productCodeFileName;
    }
    return editedImagePath.replaceAll(DOUBLE_SLASH, File.separator);
  }

  private String getScaleImagePath(String destinationPath, String productCodeFileName, String groupCode) {
    String scaleImagePath;
    if (destinationPath.contains(File.separator + AvailableSize.FULL.getName() + File.separator)) {
      scaleImagePath =
          gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalFullImageDirectory()
              + File.separator + gcsProperties.getPathPrefix() + File.separator + groupCode + File.separator
              + productCodeFileName;
    } else if (destinationPath.contains(File.separator + AvailableSize.MEDIUM.getName() + File.separator)) {
      scaleImagePath =
          gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalMediumImageDirectory()
              + File.separator + gcsProperties.getPathPrefix() + File.separator + groupCode + File.separator
              + productCodeFileName;
    } else if (destinationPath.contains(File.separator + AvailableSize.THUMBNAIL.getName() + File.separator)) {
      scaleImagePath =
          gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalThumbnailImageDirectory()
              + File.separator + gcsProperties.getPathPrefix() + File.separator + groupCode + File.separator
              + productCodeFileName;
    } else {
      scaleImagePath =
          gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalSeoulImageDirectory()
              + File.separator + gcsProperties.getPathPrefix() + File.separator + groupCode + File.separator
              + productCodeFileName;
    }
    return scaleImagePath.replaceAll(DOUBLE_SLASH, File.separator);
  }

  private String getImagePathLocation(String outPutPath) {
    String pathRemoveFinalImage = outPutPath.substring(
        outPutPath.indexOf(gcsProperties.getFinalImageDirectory()) + gcsProperties.getFinalImageDirectory().length()
            + 1);
    return pathRemoveFinalImage.substring(pathRemoveFinalImage.indexOf(File.separator) + 1);
  }

  private static String getGroupCode(String destinationPath) {
    String pathFileNameRemoved = destinationPath.substring(0, destinationPath.lastIndexOf(File.separator));
    String pathProductCodeRemoved = pathFileNameRemoved.substring(0, pathFileNameRemoved.lastIndexOf(File.separator));
    String groupCode = pathProductCodeRemoved.substring(pathProductCodeRemoved.lastIndexOf(File.separator) + 1);
    GdnPreconditions.checkArgument(
        !(groupCode.equals(AvailableSize.FULL.getName()) || groupCode.equals(AvailableSize.MEDIUM.getName())
            || groupCode.equals(AvailableSize.THUMBNAIL.getName()) || groupCode.equals(CATALOG)),
        ErrorMessage.INVALID_GROUP_CODE);
    return groupCode;
  }

  @Override
  public void deletingTempFileUsedForResizeImageGcs(String sourcePath) {
    log.info("Deleting the temp folder for resize and scaling. sourcePath : {} ", sourcePath);
    if (sourcePath.contains(tempDirForResizeImage) || sourcePath.contains(tempDirForScaleImage)) {
      File tempFile = new File(sourcePath);
      if (tempFile.exists()) {
        tempFile.delete();
        log.info("Deletion happened successfully. {} ", sourcePath);
      }
    }
  }

  public void deletingFileAndDirectory(String productCode, String destinationPath) {
    log.info(
        "Deleting the unused Image file in destination path after uploading Image file to Gcs. productCode : {} , destinationPath : {} ",
        productCode, destinationPath);
    while (destinationPath.contains(productCode)) {
      File destinationFile = new File(destinationPath);
      if (destinationFile.exists()) {
        destinationFile.delete();
        destinationPath = destinationPath.substring(0, destinationPath.lastIndexOf(File.separator));
      } else {
        break;
      }
    }
  }

  @Override
  public void deleteTempCreatedImages(Collection<String> tempCreatedImages) {
    long startTime = System.currentTimeMillis();
    if (CollectionUtils.isNotEmpty(tempCreatedImages)) {
      for (String tempLocation : tempCreatedImages) {
        deletingTempFileUsedForResizeImageGcs(tempLocation);
      }
    }
    long endTime = System.currentTimeMillis();
    log.info("Time taken to dleete temporary images: paths : {}, timeTaken : {} ", tempCreatedImages, endTime - startTime);
  }

  @Override
  public Collection<String> downloadImagesAndSaveToTempLocation(List<GraphicImageDetail> graphicImageDetails,
      boolean isResize, boolean isEdited) throws IOException {
    Set<String> distinctImageSourcePath =
        graphicImageDetails.stream().map(GraphicImageDetail::getSourcePath).collect(Collectors.toSet());
    Map<String, String> originalAndModifiedSourceImageMap = new HashMap<>();
    for (String sourceImagePath : distinctImageSourcePath) {
      String modifiedSourcePath = getLocalSourcePathOfFile(sourceImagePath, sourcePrefixPath, isResize,
          getProductCodeFileName(sourceImagePath), isEdited);
      originalAndModifiedSourceImageMap.put(sourceImagePath, modifiedSourcePath);
    }

    for (GraphicImageDetail graphicImageDetail : graphicImageDetails) {
      graphicImageDetail.setSourcePath(originalAndModifiedSourceImageMap.get(graphicImageDetail.getSourcePath()));
    }
    return originalAndModifiedSourceImageMap.values();
  }

  @Override
  public String getProductCodeFileName(String sourcePath) {
    String pathWithoutName = sourcePath.substring(0, sourcePath.lastIndexOf(File.separator));
    String productCode = pathWithoutName.substring(pathWithoutName.lastIndexOf(File.separator) + 1);
    String fileName = sourcePath.substring(sourcePath.lastIndexOf(File.separator));
    return productCode + fileName;
  }

  @Override
  public void deleteDestinationPath(Collection<String> destinationLocationPaths, boolean isResize) {
    for (String destinationPath : destinationLocationPaths) {
      if (isRequestForResizeOrScale(destinationPath) && ((isResize && gcsProperties.isSourceImageEnabled()) || (!isResize
          && gcsProperties.isFinalImageEnabled()))) {
        File tempFile = new File(destinationPath);
        if (tempFile.exists()) {
          tempFile.delete();
          log.info("Deletion happened successfully. {} ", destinationPath);
        }
      }
    }
  }

  private boolean isRequestForResizeOrScale(String destinationPath) {
    return destinationPath.contains(imagePathConfiguration.getLocationPrefix(RESIZE)) || destinationPath.contains(
        imagePathConfiguration.getLocationPrefix(SCALE)) || destinationPath.contains(
        imagePathConfiguration.getLocationPrefix(SEOUL));
  }

  private String getProductCodeFileNameForDestinationPath(String productCodeFileName) {
    if (productCodeFileName.contains(tempDirForResizeImage.replaceAll("/", "")) || productCodeFileName.contains(
        tempDirForScaleImage.replaceAll("/", ""))) {
      String uniqueImagePath = productCodeFileName.substring(productCodeFileName.lastIndexOf(File.separator) + 1);
      String productCodeAndFileName = uniqueImagePath.substring(uniqueImagePath.indexOf(Constants.UNDERSCORE) + 1);
      String productCode = productCodeAndFileName.substring(0, productCodeAndFileName.indexOf(Constants.UNDERSCORE));
      String fileName = productCodeAndFileName.substring(productCodeAndFileName.indexOf(Constants.UNDERSCORE) + 1);
      return productCode + File.separator + fileName;
    }
    return productCodeFileName;
  }

  @Override
  public boolean uploadToGcsRMA(String clientId) {
    return gcsProperties.isScaleGcsEnabled() && Arrays.asList(gcsProperties.getRmaClientId().split(Constants.COMMA))
        .contains(clientId);
  }

  @Override
  public boolean uploadToGcsOxford(String clientId) {
    return gcsProperties.isStoreGcsEnabled() && Arrays.asList(gcsProperties.getOxfordClientId().split(Constants.COMMA))
        .contains(clientId);
  }

  @Override
  public File createTemporaryFile(InputStream inputStream, String fileExt, String clientId) throws Exception {
    if (uploadToGcsRMA(clientId)) {
      File temporaryFile = new File(
          gcsProperties.getRmaTemporaryImageSourcePath() + File.separator + UUID.randomUUID() + fileExt);
      temporaryFile.getParentFile().mkdirs();
      try(BufferedOutputStream stream = new BufferedOutputStream(new FileOutputStream(temporaryFile))) {
        FileCopyUtils.copy(inputStream, stream);
      }
      return temporaryFile;
    } else if (uploadToGcsOxford(clientId)) {
      File temporaryFile = new File(
          gcsProperties.getOxfordTemporaryImageSourcePath() + File.separator + UUID.randomUUID() + fileExt);
      temporaryFile.getParentFile().mkdirs();
      try(BufferedOutputStream stream = new BufferedOutputStream(new FileOutputStream(temporaryFile))) {
        FileCopyUtils.copy(inputStream, stream);
      }
      return temporaryFile;
    } else {
      return GraphicsProcessorHelper.generateTemporaryFileFromStream(inputStream, fileExt);
    }
  }

  @Override
  public Blob createTemporaryFileInGCSRmaScaling(byte[] bytes, String path) {
    try {
      String gcsDestinationPath = (gcsProperties.getRmaGcsPath() + File.separator + path);
      return gcsService.uploadFile(rmaImageBucket, gcsDestinationPath, bytes);
    }
    catch (Exception e){
      log.error("Error uploading file for RMA scaling to path:{}, error- ", path, e);
      return null;
    }
  }

  @Override
  public String getImageLocationPathPrefix(String clientId) {
    if (uploadToGcsRMA(clientId)) {
      return gcsProperties.getRmaTemporaryImageDestinationPath();
    } else if (uploadToGcsOxford(clientId)) {
      return gcsProperties.getOxfordTemporaryImageDestinationPath();
    } else {
      return imagePathConfiguration.getLocationPrefix(clientId);
    }
  }

  @Override
  public void uploadToGcs(ImageResultDetail imageResultDetail, String clientId) throws Exception {
    if (Arrays.asList(gcsProperties.getRmaClientId().split(Constants.COMMA)).contains(clientId)) {
      String gcsDestinationPath =
          (gcsProperties.getRmaGcsPath() + File.separator + imageResultDetail.getImagePathLocation()
              .replaceAll(gcsProperties.getRmaTemporaryImageDestinationPath(), StringUtils.EMPTY)).replaceAll(
              Constants.DOUBLE_SLASH, Constants.SLASH);
      gcsService.uploadFile(rmaImageBucket, gcsDestinationPath, FileUtils.readFileToByteArray(new File(
          (gcsProperties.getRmaTemporaryImageDestinationPath() + File.separator
              + imageResultDetail.getImagePathLocation()).replaceAll(Constants.DOUBLE_SLASH, Constants.SLASH))));
    } else if (Arrays.asList(gcsProperties.getOxfordClientId().split(Constants.COMMA)).contains(clientId)) {
      String gcsDestinationPath =
          (gcsProperties.getOxfordGcsPath() + File.separator + imageResultDetail.getImagePathLocation()
              .replaceAll(gcsProperties.getOxfordTemporaryImageDestinationPath(), StringUtils.EMPTY)).replaceAll(
              Constants.DOUBLE_SLASH, Constants.SLASH);
      gcsService.uploadFile(oxfordImageBucket, gcsDestinationPath, FileUtils.readFileToByteArray(new File(
          (gcsProperties.getOxfordTemporaryImageDestinationPath() + File.separator
              + imageResultDetail.getImagePathLocation()).replaceAll(Constants.DOUBLE_SLASH, Constants.SLASH))));
    } else if (Arrays.asList(gcsProperties.getOrderClientId().split(Constants.COMMA))
      .contains(clientId)) {
      gcsService.uploadFile(orderImageBucket, imageResultDetail.getImagePathLocation(),
        FileUtils.readFileToByteArray(new File(imageResultDetail.getImagePathLocation())));
    } else if (
      Arrays.asList(gcsProperties.getActiveProductNewImageClientId().split(Constants.COMMA))
        .contains(clientId) && imageResultDetail.isActive()) {
      gcsService.uploadFile(sourceImageBucket, imageResultDetail.getDestinationPath(),
        FileUtils.readFileToByteArray(new File(imageResultDetail.getImagePathLocation())));
    } else if (Arrays.asList(
      gcsProperties.getActiveProductNewImageClientId().split(Constants.COMMA)).contains(clientId)) {
      gcsService.uploadFile(finalImageBucket, imageResultDetail.getDestinationPath(),
        FileUtils.readFileToByteArray(new File(imageResultDetail.getImagePathLocation())));
    }
  }

  private void deleteFromTemporaryLocationRMA(String path) {
    File file = new File(path);
    if (file.exists()) {
      log.info("Deleting temporary file from memory with path:{}", path);
      file.delete();
    } else {
      log.error("Temporary file with path :{} for RMA Scaling not present for deletion", path);
    }
  }

  @Override
  public void deleteFromTemporaryLocation(String path, String clientId) {
    if (Arrays.asList(gcsProperties.getRmaClientId().split(Constants.COMMA)).contains(clientId)) {
      String tempPath = (gcsProperties.getRmaTemporaryImageDestinationPath() + File.separator + path.replace(
          gcsProperties.getRmaTemporaryImageDestinationPath(), StringUtils.EMPTY)).replace(Constants.DOUBLE_SLASH,
          Constants.SLASH);
      File file = new File(tempPath);
      if (file.exists()) {
        file.delete();
      }
    } else if (Arrays.asList(gcsProperties.getOxfordClientId().split(Constants.COMMA)).contains(clientId)) {
      String tempPath = (gcsProperties.getOxfordTemporaryImageDestinationPath() + File.separator + path.replace(
          gcsProperties.getOxfordTemporaryImageDestinationPath(), StringUtils.EMPTY)).replace(Constants.DOUBLE_SLASH,
          Constants.SLASH);
      File file = new File(tempPath);
      if (file.exists()) {
        file.delete();
      }
    }
  }

  @Override
  public void uploadToAndDeleteFromTempLocationGcs(boolean uploadToGcs, String clientId, List<GraphicDetailCommand> graphicDetailCommands,
      List<ImageResultDetail> resultList) throws Exception {
    if (uploadToGcs) {
      for (ImageResultDetail imageResultDetail : resultList) {
        uploadToGcs(imageResultDetail, clientId);
      }
      for (GraphicDetailCommand graphicDetailCommand : graphicDetailCommands) {
        deleteFromTemporaryLocationRMA(graphicDetailCommand.getSourcePath());
      }
      for (ImageResultDetail imageResultDetail : resultList) {
        deleteFromTemporaryLocation(imageResultDetail.getImagePathLocation(), clientId);
      }
    }
  }

  @Override
  public byte[] gcsToFileForXRMA(String imagePath, String clientId) throws IOException {
    byte[] content = null;
    if (gcsProperties.isDisplayGcsEnabled() && Arrays.asList(gcsProperties.getRmaClientId().split(Constants.COMMA))
        .contains(clientId)) {
      content = gcsService.downloadFile(gcsProperties.getRmaBucketName(),
          (gcsProperties.getRmaGcsPath() + File.separator + imagePath).replaceAll(Constants.DOUBLE_SLASH,
              Constants.SLASH));
      if (ArrayUtils.isNotEmpty(content)) {
        return content;
      } else if (!gcsProperties.isRMAImageMigrationDone()) {
        return getFileFromFileStore(imagePath, clientId);
      }
    }
      return getFileFromFileStore(imagePath, clientId);
  }


  private byte[] getFileFromFileStore(String imagePath, String clientId) throws IOException {
    String prefix = imagePathConfiguration.getLocationPrefix(clientId);
    File file = new File(prefix + imagePath);
    if (file.exists()) {
      return FileUtils.readFileToByteArray(file);
    }
    return null;
  }

  @Override
  public byte[] gcsRemoveForOxford(String imagePath, String clientId) throws Exception {
    byte[] content = null;
    if (gcsProperties.isRemoveGcsEnabled() && Arrays.asList(gcsProperties.getOxfordClientId().split(Constants.COMMA))
        .contains(clientId)) {
      content = gcsService.downloadFile(gcsProperties.getOxfordBucketName(),
          (gcsProperties.getOxfordGcsPath() + File.separator + imagePath).replaceAll(Constants.DOUBLE_SLASH,
              Constants.SLASH));
      if (ArrayUtils.isNotEmpty(content)) {
        gcsService.uploadFile(oxfordImageBucket,
            (gcsProperties.getOxfordGcsPath() + File.separator + gcsProperties.getRemoveGcsPathForOxford()
                + imagePath).replaceAll(Constants.DOUBLE_SLASH, Constants.SLASH), content);
        gcsService.deleteFile(gcsProperties.getOxfordBucketName(),
            gcsProperties.getOxfordGcsPath() + File.separator + imagePath);
        return content;
      } else if (!gcsProperties.isOxfordImageMigrationDone()) {
        content = deleteFileFromFileStore(imagePath, clientId);
        if (ArrayUtils.isNotEmpty(content)) {
          gcsService.uploadFile(oxfordImageBucket,
              (gcsProperties.getOxfordGcsPath() + File.separator + gcsProperties.getRemoveGcsPathForOxford()
                  + imagePath).replaceAll(Constants.DOUBLE_SLASH, Constants.SLASH), content);
        }
        return content;
      }
    }
    return deleteFileFromFileStore(imagePath, clientId);
  }

  private byte[] deleteFileFromFileStore(String imagePath, String clientId) throws Exception {
    String prefix = imagePathConfiguration.getLocationPrefix(clientId);
    File file = new File(prefix + imagePath);
    byte[] content = null;
    if (file.exists()) {
      String removePrefix = imagePathConfiguration.getLocationPrefix(clientId + REMOVE_KEY_SUFFIX);
      StringBuilder destinationPath = new StringBuilder(removePrefix).append(imagePath);
      GraphicsProcessorHelper.createDestinationDirectory(destinationPath.toString());
      File destinationFilePath = new File(destinationPath.toString());
      if (file.isDirectory()) {
        FileUtils.copyDirectory(file, destinationFilePath);
      } else {
        FileUtils.copyFile(file, destinationFilePath);
      }
      content = FileUtils.readFileToByteArray(file);
      FileUtils.deleteQuietly(file);
    }
    return content;
  }

}