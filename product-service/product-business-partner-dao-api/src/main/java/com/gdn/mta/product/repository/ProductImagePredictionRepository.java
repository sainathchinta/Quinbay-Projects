package com.gdn.mta.product.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gda.mta.product.dto.response.PredictionTypeResponse;
import com.gdn.mta.product.entity.ProductImagePrediction;

public interface ProductImagePredictionRepository extends JpaRepository<ProductImagePrediction, String> {

  ProductImagePrediction findByStoreIdAndPredictionType(String storeId, String predictionType);

  List<ProductImagePrediction> findByStoreIdAndPredictionTypeInAndMarkForDeleteFalse(String storeId, List<String> predictionTypes);

  @Query(value = "select new com.gda.mta.product.dto.response.PredictionTypeResponse (displayName, displayNameIn)"
      + " from ProductImagePrediction where " + "storeId =:storeId and markForDelete = false ")
  List<PredictionTypeResponse> findDistinctPredictionTypeNameWhereStoreIdAndMarkForDeleteFalse(
      @Param("storeId") String storeId);

  @Query(value = "select distinct predictionType from ProductImagePrediction pip where storeId = :storeId and "
      + "markForDelete = false")
  List<String> findDistinctPredictionTypeWhereStoreIdAndMarkForDeleteFalse(@Param("storeId") String storeId);

  /**
   * @param predictionType
   * @return
   */
  ProductImagePrediction findByPredictionType(String predictionType);

  /**
   * @param storeId
   * @return
   */
  List<ProductImagePrediction> findByStoreIdAndForceReviewTrue(String storeId);

  /**
   * Fetch all predictions which are supposed to be considered
   *
   * @param storeId
   * @return
   */
  List<ProductImagePrediction> findByStoreIdAndForceReviewTrueAndPredictionConsideredTrue(String storeId);

  /**
   * Find by store
   *
   * @param storeId
   * @return
   */
  List<ProductImagePrediction> findByStoreIdAndPredictionConsideredTrue(String storeId);

  /**
   * To find the image prediction by storeId
   * @param storeId
   * @return
   */
  List<ProductImagePrediction> findByStoreId(String storeId);

  /**
   * To find the image prediction by storeId
   * @param storeId
   * @return
   */
  List<ProductImagePrediction> findByStoreIdAndMarkForDeleteFalse(String storeId);

}