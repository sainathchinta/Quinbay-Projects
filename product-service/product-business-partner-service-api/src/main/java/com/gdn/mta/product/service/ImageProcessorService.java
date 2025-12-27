package com.gdn.mta.product.service;

import com.gdn.micro.graphics.web.model.BulkImagesProcessRequest;
import com.gdn.micro.graphics.web.model.ScaleEditedImageRequest;
import com.gdn.micro.graphics.web.model.BulkResizeImageRequest;

public interface ImageProcessorService {

  /**
   * scale all image for product code
   *
   * @param bulkImagesProcessRequest must not null
   * @throws Exception
   */
  void scaleImage(BulkImagesProcessRequest bulkImagesProcessRequest) throws Exception;

  /**
   * scale all image for product code
   *
   * @param scaleEditedImageRequest must not null
   * @throws Exception
   */
  void scaleEditedImages(ScaleEditedImageRequest scaleEditedImageRequest) throws Exception;


  /**
   * Resize all images for given product code
   *
   * @param storeId
   * @param productCode
   * @throws Exception
   */
  boolean resizeImage(String storeId, String productCode, int prioritySeller) throws Exception;

  /**
   * Resize all edited images for given product code
   *
   * @param bulkResizeImageRequest
   * @throws Exception
   */
  void resizeEditedImage(BulkResizeImageRequest bulkResizeImageRequest) throws Exception;

  /**
   * Resize all revised images for given product code
   * @param bulkResizeImageRequest
   * @throws Exception
   */
  void resizeRevisedImage(BulkResizeImageRequest bulkResizeImageRequest) throws Exception;

  /**
   * Resize all edited images for given product code in either gcs or filestore
   *
   * @param bulkResizeImageRequest
   * @throws Exception
   */
  void resizeEditedImageInFileStoreOrGcs(BulkResizeImageRequest bulkResizeImageRequest) throws Exception;

}
