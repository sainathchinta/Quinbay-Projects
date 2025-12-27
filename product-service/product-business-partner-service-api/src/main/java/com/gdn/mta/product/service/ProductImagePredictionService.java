package com.gdn.mta.product.service;

import java.util.List;

import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gda.mta.product.dto.ImageQcProcessedResponse;
import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingRequest;
import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingResponse;
import com.gda.mta.product.dto.ProductImagePredictionResponse;
import com.gda.mta.product.dto.response.PredictionTypeResponse;
import com.gdn.mta.product.entity.ProductImagePrediction;

public interface ProductImagePredictionService {

  /**
   * Insert new image prediction type
   *
   * @param productImagePrediction
   */
  void insert(ProductImagePrediction productImagePrediction);

  /**
   * Update existing image prediction type
   *
   * @param productImagePrediction
   */
  void update(ProductImagePrediction productImagePrediction);

  /**
   * update image prediction and category mapping
   *
   * @param storeId
   * @param request
   */
  ProductImagePrediction updateImagePredictionAndCategoryMapping(String storeId, ProductImagePredictionAndCategoryMappingRequest request)
      throws Exception;

  /**
   * cache evict for update image prediction and category mapping
   *
   * @param productImagePrediction
   * @throws Exception
   */
  void cacheEvictForUpdateImagePredictionAndCategoryMapping(ProductImagePrediction productImagePrediction) throws Exception;

  /**
   * Delete image prediction type
   *
   * @param storeId
   * @param predictionType
   */
  void delete(String storeId, String predictionType);

  /**
   * Find image prediction type by storeId and variable
   *
   * @param storeId
   * @param predictionType
   * @return
   */
  ProductImagePrediction findByStoreIdAndPredictionType(String storeId, String predictionType);

  /**
   * get List of ProductImagePrediction by storeId
   *
   * @param storeId
   * @return
   */
  List<ProductImagePredictionResponse> findByStoreId(String storeId);

  /**
   * Get List of ProductImagePrediction by storeId and markForDeleteFalse
   *
   * @param storeId
   * @return
   */
  List<ProductImagePrediction> findByStoreIdAndMarkForDeleteFalse(String storeId);

  /**
   * Find all predictions which are supposed to be considered
   *
   * @param storeId
   * @return
   */
  List<ProductImagePredictionResponse> findByStoreIdAndPredictionConsideredTrue(String storeId);

  /**
   * get ProductImagePredictionResponse by productCode
   *
   * @param storeId
   * @param productCode
   * @return
   */
  ImageQcProcessedResponse findProductImagePredictionResponseByStoreIdAndProductCode(String storeId,
      String productCode);

  /**
   * get ProductImagePredictionResponse and brand response by productCode
   *
   * @param storeId
   * @param productCode
   * @return
   */
  ImageQcProcessedAndBrandResponse findProductImagePredictionAndBrandResponseByStoreIdAndProductCode(String storeId,
      String productCode) throws Exception;

  /**
   * get product image prediction and category mapping from predictionTypeList
   *
   * @param storeId
   * @param predictionTypeList
   * @return
   * @throws Exception
   */
  List<ProductImagePredictionAndCategoryMappingResponse> getImagePredictionAndCategoryMapping(String storeId,
      List<String> predictionTypeList) throws Exception;

  /**
   * Get list of different prediction types present
   *
   * @param storeId
   * @return
   */
  List<PredictionTypeResponse> getDifferentPredictionType(String storeId);

  /**
   * evict prediction type cache
   *
   * @param storeId
   */
  void evictPredictionTypeCache(String storeId);

  /**
   * Get list of available predictions
   *
   * @param storeId
   * @return
   */
  List<String> getListOfActivePredictionTypes(String storeId);

  /**
   * @param storeId
   * @return
   */
  List<ProductImagePrediction> findByStoreIdAndForceReviewTrue(String storeId);

  /**
   * Find predictions which are considered for auto approval
   *
   * @param storeId
   * @return
   */
  List<ProductImagePrediction> findByStoreIdAndForceReviewTrueAndPredictionConsideredTrue(String storeId);
}
