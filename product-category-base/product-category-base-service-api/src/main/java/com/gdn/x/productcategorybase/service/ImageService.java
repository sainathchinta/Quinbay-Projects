package com.gdn.x.productcategorybase.service;

import java.util.List;
import java.util.Set;

import com.gdn.x.productcategorybase.dto.ProductActivateImageDTO;
import com.gdn.x.productcategorybase.dto.ProductImageDTO;
import com.gdn.x.productcategorybase.dto.ProductPublishUpdateDTO;
import com.gdn.x.productcategorybase.entity.ActivateImage;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductImageSingle;
import com.gdn.x.productcategorybase.entity.ProductItemImage;

public interface ImageService {
  
  ActivateImage updateImageName(String storeId, String productCode, String locationPath, String hashCode) throws Exception;

  /**
   * update all image path and activated for product and its product items
   *
   * @param storeId must not blank
   * @param dto product activation image activation request DTO , must not null
   * @return
   * @throws Exception
   */
  ProductPublishUpdateDTO activateAndUpdateImagesName(String storeId, ProductActivateImageDTO dto, boolean skipReview)
      throws Exception;

  /**
   * Find LocationPaths By ProductCodeList And StoreId
   *
   * @param productCodeList
   * @param storeId
   * @return
   */
  Set<String> findProductAndItemLocationPathsByProductCodeListAndStoreId(List<String> productCodeList, String storeId);

  ActivateImage isProductImagesActivated(String storeId, String productCode) throws Exception;
  
  void activateAndUpdateImageName(String storeId, String productCode,  String locationPath, String hashCode);
  
  /**
   * Filter product images by list of productId
   * @param productIds
   * @return
   */
  List<ProductImageSingle> filterProductImagesByProductIds(List<String> productIds);


  /**
   * Filter Product Images by productCodes
   * 
   * @param storeId
   * @param productCodes
   * @return
   */
  List<ProductImageDTO> filterProductImagesByProductCodes(String storeId, List<String> productCodes);

  /**
   *
   * @param storeId
   * @param productId
   * @return
   */
  List<ProductImage> getProductImagesByStoreIdAndProductIdCached(String storeId, String productId);

  /**
   *
   * @param storeId
   * @param productId
   * @return
   */
  List<ProductImage> getProductImagesByStoreIdAndProductId(String storeId, String productId);

  /**
   *
   * @param storeId
   * @param productItemIds
   * @return
   */
  List<ProductItemImage> getDetachedProductItemImagesByStoreIdAndProductItemIds(
      String storeId, List<String> productItemIds);

  /**
   * delete product images using id's
   * @param productImageIds
   */
  void deleteProductImagesByIds(List<String> productImageIds);

  /**
   * delete product item images using id's
   *
   * @param productItemImageIds
   */
  void deleteProductItemImagesByIds(List<String> productItemImageIds);
}
