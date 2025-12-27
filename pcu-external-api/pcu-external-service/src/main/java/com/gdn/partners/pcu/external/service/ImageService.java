package com.gdn.partners.pcu.external.service;

import java.io.IOException;
import java.io.InputStream;

import com.gdn.partners.pcu.external.service.model.request.UploadImageRequest;
import org.springframework.web.multipart.MultipartFile;

public interface ImageService {

  /**
   * Get Image detail by file name
   *
   * @param fileName
   * @param active
   * @return
   * @throws Exception
   */
  byte[] getImageDetail(String fileName, boolean active) throws Exception;

  /**
   * upload image by upload image request
   *
   * @param request
   * @return
   * @throws IOException
   */
  boolean uploadImage(UploadImageRequest request) throws IOException;

  /**
   * Upload attribute image for size attribute
   *
   * @param image
   * @param imageFilename
   * @return
   * @throws Exception
   */
  String uploadAttributeImage(MultipartFile image, String imageFilename) throws Exception;

  /**
   * Get input stream from URL or base ^4 image
   *
   * @param imageData
   * @return
   * @throws IOException
   */
  InputStream getImageInputStream(String imageData) throws Exception;

  /**
   * To check of the image exists in the final path
   *
   * @param path
   * @return
   */
  boolean imageExistsAndValid(String path) throws Exception;

}
