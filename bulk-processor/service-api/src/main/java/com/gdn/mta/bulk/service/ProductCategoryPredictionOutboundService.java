package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.dto.ProductCategorySuggestionResponse;

import java.util.Map;

public interface ProductCategoryPredictionOutboundService {

    /**
     *
     * @param query - product name
     * @param externalCategory
     * @return
     */
    String predictProductCategoriesByProductName(String query,
        String externalCategory, Map<String, String> dsResponse) throws Exception;
}
