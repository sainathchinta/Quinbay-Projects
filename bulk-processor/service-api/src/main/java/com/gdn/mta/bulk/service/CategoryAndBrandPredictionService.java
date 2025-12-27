package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.dto.BrandAndCategoryPredictionRequest;

public interface CategoryAndBrandPredictionService {
    /**
     * Predicts brand and category for external uploads
     * @param brandAndCategoryPredictionRequest
     */
    void process(BrandAndCategoryPredictionRequest brandAndCategoryPredictionRequest)
        throws Exception;
}
