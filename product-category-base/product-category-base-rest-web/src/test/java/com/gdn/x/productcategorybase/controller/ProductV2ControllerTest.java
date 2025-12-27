package com.gdn.x.productcategorybase.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;
import com.gdn.x.productcategorybase.dto.response.ProductL1AndL2CodeResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;
import com.gdn.x.productcategorybase.service.ProductService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

class ProductV2ControllerTest {

  private MockMvc mockMvc;

  @Mock
  private ProductService productService;

  @InjectMocks
  private ProductV2Controller productV2Controller;

  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

  @BeforeEach
  void setUp() {
    MockitoAnnotations.openMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(productV2Controller).build();
  }

  @Test
  void testCheckOmniChannelSkuExistsInSeller_success() throws Exception {
    String storeId = "store1";
    String requestId = "req1";
    String username = "user1";
    OmniChannelSkuRequest request =
        new OmniChannelSkuRequest("sellerA", Arrays.asList("SKU1", "SKU2"));
    ValidOmniChannelSkuResponse validOmniChannelSkuResponse = new ValidOmniChannelSkuResponse();
    Map<String, ProductL1AndL2CodeResponse> map = new HashMap<>();
    map.put("SKU1",
      new ProductL1AndL2CodeResponse("product1", "L1Code1", "omniChannelSku1", "itemName"));
    map.put("SKU2",
      new ProductL1AndL2CodeResponse("product2", "L1Code2", "omniChannelSku2", "itemName"));
    validOmniChannelSkuResponse.setExistingOmniChannelSkusAndProductDetailsMap(map);

    when(productService.checkOmniChannelSkuExistsInSeller(eq(storeId),
        any(OmniChannelSkuRequest.class), eq(false))).thenReturn(validOmniChannelSkuResponse);

    mockMvc.perform(
            post("/api/product/v2/checkOmniChannelSkuExistsOrNotBySellerCodeAndSkuList").param(
                    "storeId", storeId).param("requestId", requestId).param("username", username)
                .contentType(MediaType.APPLICATION_JSON)
                .content(OBJECT_MAPPER.writeValueAsString(request))).andExpect(status().isOk())
        .andExpect(
            jsonPath("$.value.existingOmniChannelSkusAndProductDetailsMap.SKU1.productCode").value(
                "product1")).andExpect(
            jsonPath("$.value.existingOmniChannelSkusAndProductDetailsMap.SKU1.skuCode").value(
                "L1Code1")).andExpect(
            jsonPath("$.value.existingOmniChannelSkusAndProductDetailsMap.SKU1.omniChannelSku").value(
                "omniChannelSku1"));
  }

  @Test
  void testCheckOmniChannelSkuExistsInSeller_emptyInput() throws Exception {
    String storeId = "store1";
    String requestId = "req1";
    OmniChannelSkuRequest request = new OmniChannelSkuRequest("sellerA", null);
    ValidOmniChannelSkuResponse validOmniChannelSkuResponse = new ValidOmniChannelSkuResponse();
    validOmniChannelSkuResponse.setExistingOmniChannelSkusAndProductDetailsMap(new HashMap<>());

    when(productService.checkOmniChannelSkuExistsInSeller(eq(storeId),
        any(OmniChannelSkuRequest.class), eq(false))).thenReturn(validOmniChannelSkuResponse);

    mockMvc.perform(
            post("/api/product/v2/checkOmniChannelSkuExistsOrNotBySellerCodeAndSkuList").param(
                    "storeId", storeId).param("requestId", requestId)
                .contentType(MediaType.APPLICATION_JSON)
                .content(OBJECT_MAPPER.writeValueAsString(request))).andExpect(status().isOk())
        .andExpect(jsonPath("$.value.existingOmniChannelSkusAndProductDetailsMap").isEmpty());
  }
} 