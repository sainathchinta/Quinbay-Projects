package com.gdn.x.mta.distributiontask.service.api;

import java.util.List;

import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductImage;

public interface FileStorageService {
  /**
   * delete original images
   *
   * @param locationPath
   */
  void deleteOriginalImages(String locationPath);

  /**
   * delete from GCS
   *
   * @param locationPath
   */
  void deleteFromGcs(String locationPath);

  /**
   * validate if nonactive image exist or not
   * @param product
   */
  void validateProductImages(Product product) throws Exception;

  /**
   * update the location path for images
   * @param newProduct
   * @param existingProduct
   */
  void updateNewlyAddedImagePath(Product newProduct, Product existingProduct);

  /**
   * checking existing image files from fileStore or GCS
   * @param imagePath
   * @return
   */
  boolean isFinalImageFileExist(String imagePath);

  /**
   * checking image Location Path For Non-Active Product Images
   * @param locationPath non null
   * @return true/false
   */
  boolean isSourceImageFileExist(String locationPath);
}
