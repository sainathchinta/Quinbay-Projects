package com.gdn.mta.product.service;

import java.util.List;
import java.util.Map;

import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductImageEditRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.UpdateItemsPriceStockImagesRequest;
import com.gdn.micro.graphics.web.model.ImageRequest;
import com.gdn.mta.product.entity.ProductFieldHistory;
import com.gdn.mta.product.valueobject.CommonImagePathDto;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.entity.Product;

public interface FileStorageService {


  /**
   * @param filePath
   * @param isFinalImage
   * @return
   */
  boolean isFileExists(String filePath, boolean isFinalImage);

  /**
   * @param imageSourceDirectory
   * @param imageHashCodeLocationPathMap
   * @return
   */
  String checkImageAvailabilityAndGetUnavailableImages(String imageSourceDirectory,
    Map<String, CommonImagePathDto> imageHashCodeLocationPathMap);

  /**
   * @param imageSourceDirectory
   * @param imageHashCodeLocationPathMap
   * @return
   */
  String checkImageAvailabilityAndGetUnavailableImagesForEditedProducts(String imageSourceDirectory,
    Map<String, Image> imageHashCodeLocationPathMap);

  /**
   * @param productCreationRequest
   * @param productCreationType
   * @return
   */
  String checkImageAvailability(ProductCreationRequest productCreationRequest, String productCreationType);

  /**
   * @param imageRequests
   * @param isFinalImage
   * @return
   */
  String checkImageAvailability(List<ImageRequest> imageRequests, boolean isFinalImage);

  void editImageNameIfGcsEnabled(UpdateItemsPriceStockImagesRequest request);

  void editImageNameIfGcsEnabled(ProductCreationRequest productCreationRequest);

  void editImageNameIfGcsEnabled(ProductVariantUpdateRequest request);

  void editImageNameIfGcsEnabled(ProductImageEditRequest request);

  void editImageNameIfGcsEnabled(Product newProduct, Product savedProduct);

  /**
   * get complete source location url prefix
   * @return
   */
  String getCompleteSourceUrlPrefix();

  /**
   * get image path prefix
   * @return
   */
  String getImagePathPrefix();

  /**
   *
   * @param images
   * @param imageSourceDirectory
   * @return
   */
  List<ImageRequest> getRevisedImageRequests(List<Image> images, String imageSourceDirectory);

  /**
   *
   * @param productLevel3SummaryDetailsImage
   * @return
   */
  List<ImageRequest> getImageRequest(ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImage);

  /**
   *
   * @param imageLocationPath
   * @return
   */
  boolean checkFilePresentInGcsOrFileStoreAfterMigration(String imageLocationPath);

  /**
   *
   * @param imageRequests
   * @return
   */
  String checkImageAvailabilityInFileStoreOrGcs(List<ImageRequest> imageRequests);

  /**
   *
   * @param locationPath
   * @return
   */
  String generateFinalImageFullPath(String locationPath);

  /**
   *
   * @param changeFieldList
   */
  void validateImageExists(List<ProductFieldHistory> changeFieldList);
}
