package com.gdn.x.productcategorybase.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.x.productcategorybase.entity.PredictionCategoryMapping;

public interface PredictionCategoryMappingRepository extends JpaRepository<PredictionCategoryMapping, String> {

  List<PredictionCategoryMapping> findByStoreIdAndPredictionIdAndCategoryCodeIn(String storeId, String predictionId,
      List<String> categoryCodes);

  List<PredictionCategoryMapping> findByStoreIdAndPredictionIdInAndMarkForDeleteFalse(String storeId,
      List<String> predictionIds);

  List<PredictionCategoryMapping> findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(String storeId,
      String categoryCode);
}
