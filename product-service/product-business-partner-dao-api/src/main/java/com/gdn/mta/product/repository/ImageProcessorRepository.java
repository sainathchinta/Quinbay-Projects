package com.gdn.mta.product.repository;


import com.gdn.micro.graphics.web.model.BulkImagesProcessRequest;
import com.gdn.micro.graphics.web.model.BulkResizeImageRequest;
import com.gdn.micro.graphics.web.model.ScaleEditedImageRequest;

public interface ImageProcessorRepository {

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
   * Resize all images for product code
   *
   * @param bulkResizeImageRequest
   * @throws Exception
   */
  void resizeImage(BulkResizeImageRequest bulkResizeImageRequest) throws Exception;

  /**
   * Resize all edited images for product code
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

}
