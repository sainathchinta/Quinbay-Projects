package com.gdn.partners.pcu.internal.web.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.enums.BrandApprovalStatus;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.VendorApiPath;
import com.gdn.partners.pcu.internal.properties.ClientParameterProperties;
import com.gdn.partners.pcu.internal.service.VendorService;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductDetailRequest;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

import jakarta.servlet.http.Cookie;
import jakarta.validation.ConstraintViolationException;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(controllers = VendorController.class)
@AutoConfigureMockMvc
public class VendorControllerIntegrationTest {

  private static final String sessionJson =
      "{\"code\":\"123\",\"username\":\"a@vendor.com\",\"name\":\"Vendor\",\"mode\":{\"code\":\"code\","
          + "\"type\":\"VENDOR\"},\"switchableMode\":true,\"modules\":[\"/fbb\"],\"roles\":[],"
          + "\"accessibilities\":[],\"links\":[{\"code\":\"PRODUCT\",\"type\":\"MENU\","
          + "\"name\":\"Produk\",\"url\":\"/url\"}],\"customFields\":{\"mapsGoogleApisBrowserKey\":\"map\"}}";
  private static final String VENDOR_CODE = "vendorCode";
  private static final String INVALID_TYPE = "invalid";
  private DistributionProductDetailRequest distributionProductDetailRequest;

  @Autowired
  private MockMvc mockMvc;

  @MockBean
  private ClientParameterHelper clientParameterHelper;


  @MockBean
  private ValueOperations valueOperations;

  @MockBean
  private VendorService vendorService;

  @MockBean
  private StringRedisTemplate redisTemplate;

  @MockBean
  private ClientParameterProperties clientParameterProperties;


  @BeforeEach
  public void setUp() {
    when(redisTemplate.opsForValue()).thenReturn(valueOperations);
    when(valueOperations.get(anyString())).thenReturn(sessionJson);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(clientParameterHelper.getChannelId()).thenReturn(Constants.CHANNEL_ID);
    when(clientParameterHelper.getClientId()).thenReturn(Constants.CLIENT_ID);
    when(clientParameterHelper.getVendorCode()).thenReturn(Constants.VENDOR_CODE);
    doNothing().when(vendorService)
        .updateProduct(anyString(), anyString(), any(DistributionProductDetailRequest.class));

    distributionProductDetailRequest = new DistributionProductDetailRequest();
    distributionProductDetailRequest.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(vendorService);
  }

  @Test
  public void updateVendorProductTest_WithIncorrectPathVariable() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        post(VendorApiPath.BASE_PATH + VendorApiPath.VENDOR_PRODUCT_UPDATE, INVALID_TYPE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON).content("{}")
            .cookie(new Cookie(Constants.SIGNATURE, Constants.SIGNATURE));
    final MvcResult mvcResult = mockMvc.perform(requestBuilder).andExpect(status().isBadRequest()).andReturn();
    Assertions.assertTrue(mvcResult.getResolvedException().getClass().equals(ConstraintViolationException.class));
  }

  @Test
  public void updateVendorProductTest_WithCorrectPathVariable() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        post(VendorApiPath.BASE_PATH + VendorApiPath.VENDOR_PRODUCT_UPDATE, Constants.VENDOR_TYPE_CONTENT)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON).content("{}")
            .cookie(new Cookie(Constants.SIGNATURE, Constants.SIGNATURE));
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(vendorService)
        .updateProduct(eq(Constants.VENDOR_TYPE_CONTENT), eq(VENDOR_CODE), any());
  }

  @Test
  public void approveVendorProductTest_WithCorrectPathVariable() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    MockHttpServletRequestBuilder requestBuilder =
        post(VendorApiPath.BASE_PATH + VendorApiPath.VENDOR_PRODUCT_APPROVE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(distributionProductDetailRequest))
            .cookie(new Cookie(Constants.SIGNATURE, Constants.SIGNATURE));
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(vendorService).approveVendorProduct(eq(VENDOR_CODE),
        any(DistributionProductDetailRequest.class));
  }

  @Test
  public void approveVendorProductDraftBrandTest() throws Exception {
    distributionProductDetailRequest.setBrandApprovalStatus(INVALID_TYPE);
    ObjectMapper objectMapper = new ObjectMapper();
    MockHttpServletRequestBuilder requestBuilder =
        post(VendorApiPath.BASE_PATH + VendorApiPath.VENDOR_PRODUCT_APPROVE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(distributionProductDetailRequest))
            .cookie(new Cookie(Constants.SIGNATURE, Constants.SIGNATURE));
    mockMvc.perform(requestBuilder).andExpect(status().is5xxServerError());
  }

  @Test
  public void approveImageVendorProductDraftBrandTest() throws Exception {
    distributionProductDetailRequest.setBrandApprovalStatus(INVALID_TYPE);
    ObjectMapper objectMapper = new ObjectMapper();
    MockHttpServletRequestBuilder requestBuilder =
        post(VendorApiPath.BASE_PATH + VendorApiPath.VENDOR_PRODUCT_APPROVE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(distributionProductDetailRequest))
            .cookie(new Cookie(Constants.SIGNATURE, Constants.SIGNATURE));
    try {
      mockMvc.perform(requestBuilder);
    }
    catch(ApplicationRuntimeException e) {
    }
  }
}
