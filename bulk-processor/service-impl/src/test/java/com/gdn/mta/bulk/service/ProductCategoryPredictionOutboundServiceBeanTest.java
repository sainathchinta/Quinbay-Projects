package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.dto.CategorySuggestionResponse;
import com.gdn.mta.bulk.dto.ProductCategorySuggestionResponse;
import com.gdn.mta.bulk.dto.SuggestedCategoriesResponse;
import com.gdn.mta.bulk.feignConfig.ProductCategoryPredictionFeign;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ProductCategoryPredictionOutboundServiceBeanTest {
    @InjectMocks
    private ProductCategoryPredictionOutboundServiceBean
        productCategoryPredictionOutboundServiceBean;

    @Mock
    private ProductCategoryPredictionFeign productCategoryPredictionFeign;

    @Mock
    private ObjectMapper objectMapper;

    private ProductCategorySuggestionResponse productCategorySuggestionResponse;
    private Map<String, String> DS_RESPONSE = new HashMap<>();
    private final String PRODUCT_NAME = "productName";
    private final String EXTERNAL_CATEGORY = "externalCategory";

    @BeforeEach
    public void setup() {
        MockitoAnnotations.initMocks(this);
    }

    @AfterEach
    public void tearDown() {
        Mockito.verifyNoMoreInteractions(productCategoryPredictionFeign);
        Mockito.verifyNoMoreInteractions(objectMapper);
    }

    @Test
    public void predictProductCategoriesByProductName_emptyResponse()
        throws JsonProcessingException {
        Mockito.when(productCategoryPredictionFeign.predictProductCategoriesByProductName("", ""))
            .thenReturn(productCategorySuggestionResponse);
        Map<String, String> dsResponse = new HashMap<>();
        try {
            Assertions.assertThrows(ApplicationRuntimeException.class,
                () -> productCategoryPredictionOutboundServiceBean.predictProductCategoriesByProductName(
                    PRODUCT_NAME, EXTERNAL_CATEGORY, dsResponse));
        } finally {
            Mockito.verify(productCategoryPredictionFeign)
                .predictProductCategoriesByProductName(PRODUCT_NAME, EXTERNAL_CATEGORY);
            Mockito.verify(objectMapper).writeValueAsString(productCategorySuggestionResponse);
        }
    }
    @Test
    public void predictProductCategoriesByProductName_emptyPredictedCategory()
        throws JsonProcessingException {
        productCategorySuggestionResponse = new ProductCategorySuggestionResponse();
        productCategorySuggestionResponse.setQuery(PRODUCT_NAME);
        productCategorySuggestionResponse.setExternalCategory(EXTERNAL_CATEGORY);
        Mockito.when(productCategoryPredictionFeign.predictProductCategoriesByProductName(PRODUCT_NAME, EXTERNAL_CATEGORY))
            .thenReturn(productCategorySuggestionResponse);
        Map<String, String> dsResponse = new HashMap<>();
        try {
            Assertions.assertThrows(ApplicationRuntimeException.class,
                () -> productCategoryPredictionOutboundServiceBean.predictProductCategoriesByProductName(
                    PRODUCT_NAME, EXTERNAL_CATEGORY, dsResponse));
        } finally {
            Mockito.verify(productCategoryPredictionFeign)
                .predictProductCategoriesByProductName(PRODUCT_NAME, EXTERNAL_CATEGORY);
            Mockito.verify(objectMapper).writeValueAsString(productCategorySuggestionResponse);
        }
    }

    @Test
    public void predictProductCategoriesByProductName_happyFlow() throws Exception {
        productCategorySuggestionResponse = new ProductCategorySuggestionResponse();
        CategorySuggestionResponse categorySuggestionResponse1 = new CategorySuggestionResponse();
        categorySuggestionResponse1.setCategoryLevel("1");
        categorySuggestionResponse1.setCategoryCode("code1");
        CategorySuggestionResponse categorySuggestionResponse2 = new CategorySuggestionResponse();
        categorySuggestionResponse2.setCategoryLevel("2");
        categorySuggestionResponse2.setCategoryCode("code2");
        SuggestedCategoriesResponse suggestedCategoriesResponse = new SuggestedCategoriesResponse();
        suggestedCategoriesResponse.setCategories(List.of(categorySuggestionResponse2,categorySuggestionResponse1));
        productCategorySuggestionResponse.setPredictedCategories(Collections.singletonList(suggestedCategoriesResponse));
        Mockito.when(productCategoryPredictionFeign.predictProductCategoriesByProductName(PRODUCT_NAME, EXTERNAL_CATEGORY))
            .thenReturn(productCategorySuggestionResponse);
        productCategoryPredictionOutboundServiceBean.predictProductCategoriesByProductName(PRODUCT_NAME, EXTERNAL_CATEGORY, DS_RESPONSE);

        Mockito.verify(productCategoryPredictionFeign).predictProductCategoriesByProductName(PRODUCT_NAME,EXTERNAL_CATEGORY);
        Mockito.verify(objectMapper).writeValueAsString(productCategorySuggestionResponse);
    }
}
