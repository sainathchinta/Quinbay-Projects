package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.dto.CategorySuggestionResponse;
import com.gdn.mta.bulk.dto.ProductCategorySuggestionResponse;
import com.gdn.mta.bulk.feignConfig.ProductCategoryPredictionFeign;
import com.gdn.partners.bulk.util.Constant;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Map;

@Service
@Slf4j
public class ProductCategoryPredictionOutboundServiceBean
    implements ProductCategoryPredictionOutboundService {

    @Autowired
    private ProductCategoryPredictionFeign productCategoryPredictionFeign;

    @Autowired
    private ObjectMapper objectMapper;

    @Override
    public String predictProductCategoriesByProductName(String query, String externalCategory,
        Map<String, String> dsResponse) throws Exception {
        ProductCategorySuggestionResponse response =
            productCategoryPredictionFeign.predictProductCategoriesByProductName(query,
                externalCategory);
        dsResponse.put(Constant.CATEGORY_RECOMMENDATION, objectMapper.writeValueAsString(response));
        if (ObjectUtils.isEmpty(response) || CollectionUtils.isEmpty(
            response.getPredictedCategories())) {
            log.error(
                "Error on mapping category prediction for productName {} : externalCategory : {} ",
                query, externalCategory);
            throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
                Constant.ERROR_IN_CATEGORY_PREDICTION);
        }
        int maxCategoryLevel = 0;
        String categoryCode = null;
        for (CategorySuggestionResponse category : response.getPredictedCategories().getFirst()
            .getCategories()) {
            if (maxCategoryLevel < Integer.parseInt(category.getCategoryLevel())) {
                maxCategoryLevel = Integer.parseInt(category.getCategoryLevel());
                categoryCode = category.getCategoryCode();
            }
        }
        return categoryCode;
    }
}
