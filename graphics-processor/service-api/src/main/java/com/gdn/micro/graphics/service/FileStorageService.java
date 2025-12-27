package com.gdn.micro.graphics.service;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.List;

import com.gdn.micro.graphics.domain.event.model.ImageResponse;
import com.gdn.micro.graphics.domain.event.model.ImageResultDetail;
import com.gdn.micro.graphics.model.GraphicDetailCommand;
import com.gdn.micro.graphics.model.GraphicImageDetail;
import com.google.cloud.storage.Blob;

public interface FileStorageService {

  /**
   * @param sourcePath
   * @param prefixPath
   * @param resize
   * @param productCodeFileName
   * @param isEdited
   * @return
   * @throws IOException
   */
  String getLocalSourcePathOfFile(String sourcePath, String prefixPath, boolean resize,
      String productCodeFileName, boolean isEdited) throws IOException;

  /**
   *
   * @param sourcePath
   */
  void deletingTempFileUsedForResizeImageGcs(String sourcePath);

  /**
   * save resized image
   *
   * @param destinationPath
   * @param resultDetail
   * @param resize
   * @param productCodeFileName
   * @param isEdited
   * @throws Exception
   */
  void saveImages(String destinationPath, ImageResponse resultDetail, boolean resize,
      String productCodeFileName, Boolean isEdited) throws Exception;

  /**
   *
   * @param tempCreatedImages
   */
  void deleteTempCreatedImages(Collection<String> tempCreatedImages);

  /**
   * @param graphicImageDetails
   * @param isResize
   * @param isEdited
   * @return
   */
  Collection<String> downloadImagesAndSaveToTempLocation(List<GraphicImageDetail> graphicImageDetails,
      boolean isResize, boolean isEdited) throws IOException;

  /**
   *
   * @param sourcePath
   * @return
   */
  String getProductCodeFileName(String sourcePath);

  /**
   *
   * @param destinationLocationPaths
   * @param isResize
   */
  void deleteDestinationPath(Collection<String> destinationLocationPaths, boolean isResize);

  /**
   * check using client id if file can be uploaded to GCS
   * @param clientId
   * @return
   */
  boolean uploadToGcsRMA(String clientId);


  /**
   * check using client id if file can be uploaded to GCS
   * @param clientId
   * @return
   */
  boolean uploadToGcsOxford(String clientId);

  /**
   * create temporary file for scaling
   * @param inputStream
   * @param fileExt
   * @param clientId
   * @return
   * @throws Exception
   */
  File createTemporaryFile(InputStream inputStream, String fileExt, String clientId) throws Exception;


  /**
   * create temporary file in gcs for rma image scaling
   * @return
   */
  Blob createTemporaryFileInGCSRmaScaling(byte[] bytes, String path);


  /**
   * get location path from client id;
   * @param clientId
   * @return
   */
  String getImageLocationPathPrefix(String clientId);

  /**
   * upload images to GCS
   * @param imageResultDetail
   * @param clientId
   * @throws Exception
   */
  void uploadToGcs(ImageResultDetail imageResultDetail, String clientId) throws Exception;

  /**
   * delete file for xgp temp location
   * @param path
   * @param clientId
   */
  void deleteFromTemporaryLocation(String path, String clientId);

  /**
   *
   * @param uploadToGcs
   * @param clientId
   * @param graphicDetailCommands
   * @param resultList
   */
  void uploadToAndDeleteFromTempLocationGcs(boolean uploadToGcs, String clientId, List<GraphicDetailCommand> graphicDetailCommands,
      List<ImageResultDetail> resultList) throws Exception;

  /**
   * fetch from gcs for xrma
   *
   * @param imagePath
   * @param clientId
   * @return
   * @throws IOException
   */
  byte[] gcsToFileForXRMA(String imagePath,String clientId) throws IOException;

  /**
   * changing gcs location to removed gcs
   *
   * @param imagePath
   * @param clientId
   * @return
   * @throws Exception
   */
  byte[] gcsRemoveForOxford(String imagePath, String clientId) throws Exception;
}
