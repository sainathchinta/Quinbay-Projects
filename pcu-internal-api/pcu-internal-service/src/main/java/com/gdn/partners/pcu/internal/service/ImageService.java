package com.gdn.partners.pcu.internal.service;

import com.gdn.partners.pcu.internal.web.model.request.UploadImageRequest;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;

public interface ImageService {

  /**
   * Check image size
   *
   * @param imageFileName
   * @param active
   * @return
   * @throws IOException
   */
  boolean checkImageSize(String imageFileName, boolean active) throws IOException;

  /**
   * Upload document
   *
   * @param documentFileName
   * @param fileBytes
   * @param sellerCode
   * @param brandCode
   * @return
   */
  void uploadDocument(String documentFileName, byte[] fileBytes, String sellerCode, String brandCode)
      throws Exception;

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
   * upload image by upload image request
   *
   * @param request
   * @return
   * @throws IOException
   */
  boolean uploadImage(UploadImageRequest request) throws IOException;
}
