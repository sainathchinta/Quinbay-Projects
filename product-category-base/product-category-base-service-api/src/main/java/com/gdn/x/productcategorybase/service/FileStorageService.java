package com.gdn.x.productcategorybase.service;

import java.io.File;
import java.io.IOException;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.lang3.tuple.Pair;

import com.gdn.x.productcategorybase.dto.Image;

public interface FileStorageService {

  /**
   * update location for images
   *
   * @param image
   * @param productCode
   * @param imageSourceDirectory
   * @param fullImageSourceDirectory
   * @param uploadedImageLocations
   * @param imageUploadCounter
   * @return
   * @throws Exception
   */
  Image updateLocationForImages(Image image, String productCode, String imageSourceDirectory,
      String fullImageSourceDirectory, Set<String> uploadedImageLocations, AtomicInteger imageUploadCounter) throws Exception;

  void uploadFileToFileStore(Image image, String productCode, String imageSourceDirectory, File sourceFile,
      String filename) throws IOException;

  /**
   * delete Images for rejected products
   *
   * @param locationPath
   */
  void deleteImages(String locationPath);

  /**
   * delete Images from GCS
   *
   * @param locationPath
   */
  void deleteFromGcs(String locationPath);

  /**
   * checking existing image files from fileStore or GCS
   * @param imagePath
   * @return
   */
  boolean isFinalImageFileExist(String imagePath);

  /**
   * Migrate FinalImage From Gfs To Gcs
   *
   * @param locationPathSet
   * @throws Exception
   */
  void migrateFinalImageFromGfsToGcs(Set<String> locationPathSet) throws Exception;

  /**
   * Upload FinalImage From Gfs To Gcs
   *
   * @param locationPath
   * @throws Exception
   */
  void uploadFinalImageFromGfsToGcs(String locationPath) throws Exception;

  /**
   * Checking is Already Gcs Image
   *
   * @param path
   * @return
   */
  boolean isAlreadyGcsImage(String path);

  /**
   *
   * @param path
   * @return
   */
  String getGcsPathWithPrefix(String path);

  /**
   *
   * @param productCode
   * @param imagePaths
   */
  void migrateImagesFromGfsToGcs(String productCode, Set<Pair<String, String>> imagePaths) throws Exception;

  /**
   *
   * @param fileStorePath
   * @param gcsPath
   * @return
   * @throws Exception
   */
  boolean uploadFileFromGfsToGcs(String fileStorePath, String gcsPath) throws Exception;

  /**
   * delete final full images from fileStore or GCS
   *
   * @param locationPath
   */
  void deleteFullFinalImagesFromGcs(String locationPath);

  /**
   * delete final medium images from GCS or fileStore
   * @param locationPath
   */
  void deleteFinalMediumImagesFromGcs(String locationPath);

  /**
   * delete final thumbnail images from GCS or fileStore
   * @param locationPath
   */
  void deleteFinalThumbnailImagesFromGcs(String locationPath);

}
