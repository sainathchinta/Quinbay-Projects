package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.dto.BrandDetailsResponse;
import com.gdn.mta.bulk.dto.BrandSuggestionResponse;
import com.gdn.mta.bulk.feignConfig.DsProtectedBrandFeign;
import com.gdn.mta.bulk.models.BrandPredictionRequest;
import com.gdn.partners.bulk.util.Constant;
import com.google.api.gax.rpc.ApiException;
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
import java.util.Map;

public class BrandPredictionOutboundServiceBeanTest {
    @InjectMocks
    private BrandPredictionOutboundServiceBean brandPredictionOutboundServiceBean;

    @Mock
    private DsProtectedBrandFeign dsProtectedBrandFeign;

    @Mock
    private ObjectMapper objectMapper;

    private final String PRODUCT_NAME = "productName";
    private final String PRODUCT_DESCRIPTION = "productDescripiton";
    private final String BRAND_NAME = "brandName";
    private final String RESPONSE = "testREsponse";
    private BrandPredictionRequest brandPredictionRequest = new BrandPredictionRequest();
    private Map<String, String> DS_RESPONSE = new HashMap<>();
    private BrandSuggestionResponse brandSuggestionResponse;
    private BrandDetailsResponse brandDetailsResponse = new BrandDetailsResponse();

    @BeforeEach
    public void setup() {
        MockitoAnnotations.initMocks(this);
        brandPredictionRequest.setProductName(PRODUCT_NAME);
        brandPredictionRequest.setDescription(PRODUCT_DESCRIPTION);

        brandDetailsResponse.setBrandName(BRAND_NAME);
        brandDetailsResponse.setSource(Constant.BRAND_SOURCE_FINAL_RECOMMENDATION);
    }

    @AfterEach
    public void teardown() {
        Mockito.verifyNoMoreInteractions(dsProtectedBrandFeign);
        Mockito.verifyNoMoreInteractions(objectMapper);
    }

    @Test
    public void predictProductBrand_happyTest() throws Exception {
        brandSuggestionResponse = new BrandSuggestionResponse();
        brandSuggestionResponse.setBrandRecommendation(Collections.singletonList(brandDetailsResponse));
        Mockito.when(dsProtectedBrandFeign.predictProductBrandByProductName(brandPredictionRequest)).thenReturn(brandSuggestionResponse);
        Mockito.when(objectMapper.writeValueAsString(brandSuggestionResponse)).thenReturn(RESPONSE);
        brandPredictionOutboundServiceBean.predictProductBrand(brandPredictionRequest, DS_RESPONSE);

        Mockito.verify(dsProtectedBrandFeign).predictProductBrandByProductName(brandPredictionRequest);
        Mockito.verify(objectMapper).writeValueAsString(brandSuggestionResponse);
    }

    @Test
    public void predictProductBrand_responseEmptyTest() throws Exception {
        Mockito.when(dsProtectedBrandFeign.predictProductBrandByProductName(brandPredictionRequest)).thenReturn(brandSuggestionResponse);
        Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(RESPONSE);
        try {
            Assertions.assertThrows(ApplicationRuntimeException.class,()->brandPredictionOutboundServiceBean.predictProductBrand(brandPredictionRequest,
                DS_RESPONSE));
        } finally {
        }
        Mockito.verify(dsProtectedBrandFeign).predictProductBrandByProductName(brandPredictionRequest);
        Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
    }

    @Test
    public void predictProductBrand_brandNameEmptyTest() throws Exception {
        brandSuggestionResponse = new BrandSuggestionResponse();
        Mockito.when(dsProtectedBrandFeign.predictProductBrandByProductName(brandPredictionRequest)).thenReturn(brandSuggestionResponse);
        Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(RESPONSE);
        try {
            Assertions.assertThrows(ApplicationRuntimeException.class,()->brandPredictionOutboundServiceBean.predictProductBrand(brandPredictionRequest,
                DS_RESPONSE));
        } finally {
        }
        Mockito.verify(dsProtectedBrandFeign).predictProductBrandByProductName(brandPredictionRequest);
        Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
    }
}
