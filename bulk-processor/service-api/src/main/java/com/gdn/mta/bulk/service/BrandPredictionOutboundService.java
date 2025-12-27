package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.dto.BrandSuggestionResponse;
import com.gdn.mta.bulk.models.BrandPredictionRequest;

import java.util.Map;

public interface BrandPredictionOutboundService {
    /**
     * @param brandPredictionRequest - product name and description
     * @return
     */
    String predictProductBrand(
        BrandPredictionRequest brandPredictionRequest, Map<String, String> dsResponse) throws Exception;
}
