package com.gdn.partners.product.analytics.web.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.product.analytics.service.ProductAttributeExtractionsService;
import com.gdn.partners.product.analytics.web.model.request.ProductAttributeExtractionsRequest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

class ProductAttributeExtractionsControllerTest {

    @Mock
    private ProductAttributeExtractionsService productAttributeExtractionsService;

    @InjectMocks
    private ProductAttributeExtractionsController productAttributeExtractionsController;

    private MockMvc mockMvc;

    private ObjectMapper objectMapper;

    private static final String STORE_ID = "10001";
    private static final int BATCH_SIZE = 100;

    @BeforeEach
    public void setup() {
        MockitoAnnotations.initMocks(this);
        this.mockMvc = MockMvcBuilders.standaloneSetup(this.productAttributeExtractionsController).build();
        objectMapper = new ObjectMapper();
    }

    @Test
    void publishEvents_WithAllParameters_Success() throws Exception {
        // When
        this.mockMvc.perform(
            MockMvcRequestBuilders.get("/api/product-attribute-extractions/publish")
                .param("storeId", STORE_ID)
                .param("batchSize", String.valueOf(BATCH_SIZE))
        ).andExpect(MockMvcResultMatchers.status().isOk());

        // Then
        verify(productAttributeExtractionsService).publishEventsForProductAttributeExtractions(STORE_ID, BATCH_SIZE);
        verifyNoMoreInteractions(productAttributeExtractionsService);
    }

    @Test
    void publishEvents_WithoutOptionalParameters_Success() throws Exception {
        // When
        this.mockMvc.perform(
            MockMvcRequestBuilders.get("/api/product-attribute-extractions/publish")
                .param("storeId", STORE_ID)
                .param("batchSize", String.valueOf(0))
        ).andExpect(MockMvcResultMatchers.status().isOk());

        // Then
        verify(productAttributeExtractionsService).publishEventsForProductAttributeExtractions(STORE_ID, 0);
        verifyNoMoreInteractions(productAttributeExtractionsService);
    }

    @Test
    void publishEvents_WithEmptyUsername_Success() throws Exception {
        // When
        this.mockMvc.perform(
            MockMvcRequestBuilders.get("/api/product-attribute-extractions/publish")
                .param("storeId", STORE_ID)
                .param("batchSize", String.valueOf(BATCH_SIZE))
        ).andExpect(MockMvcResultMatchers.status().isOk());

        // Then
        verify(productAttributeExtractionsService).publishEventsForProductAttributeExtractions(STORE_ID, BATCH_SIZE);
        verifyNoMoreInteractions(productAttributeExtractionsService);
    }

    @Test
    void publishEventsByProductSku_WithAllParameters_Success() throws Exception {
        // Given
        var productSkuList = List.of("sku1", "sku2");
        var request = new ProductAttributeExtractionsRequest();
        request.setProductSkuList(productSkuList);
        String json = objectMapper.writeValueAsString(request);

        // When
        this.mockMvc.perform(
            MockMvcRequestBuilders.post("/api/product-attribute-extractions/publish-by-product-sku")
                .param("storeId", STORE_ID)
                .contentType("application/json")
                .content(json)
        ).andExpect(MockMvcResultMatchers.status().isOk());

        // Then
        verify(productAttributeExtractionsService).publishEventsForProductAttributeExtractionsByProductSku(
            STORE_ID, productSkuList);
        verifyNoMoreInteractions(productAttributeExtractionsService);
    }

    @Test
    void publishEventsByProductSku_WithEmptyProductSkuList_Success() throws Exception {
        // Given
        var productSkuList = List.of(""); // or Collections.emptyList() if you want to test empty
        var request = new ProductAttributeExtractionsRequest();
        request.setProductSkuList(productSkuList);
        String json = objectMapper.writeValueAsString(request);

        // When
        this.mockMvc.perform(
            MockMvcRequestBuilders.post("/api/product-attribute-extractions/publish-by-product-sku")
                .param("storeId", STORE_ID)
                .contentType("application/json")
                .content(json)
        ).andExpect(MockMvcResultMatchers.status().isOk());

        // Then
        verify(productAttributeExtractionsService).publishEventsForProductAttributeExtractionsByProductSku(
            STORE_ID, productSkuList);
        verifyNoMoreInteractions(productAttributeExtractionsService);
    }
} 