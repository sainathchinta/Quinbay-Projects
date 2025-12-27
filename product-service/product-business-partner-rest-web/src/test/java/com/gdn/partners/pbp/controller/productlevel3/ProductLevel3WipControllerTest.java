package com.gdn.partners.pbp.controller.productlevel3;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.utils.URIBuilder;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.http.converter.ResourceHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import jakarta.servlet.ServletException;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.commons.constant.ProductLevel3WipSummaryCriteria;
import com.gdn.mta.product.service.ProductLevel3WipWrapper;
import com.gdn.mta.product.valueobject.ProductLevel3WipDTO;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipDetailResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipSummaryRequest;
import com.gdn.partners.pbp.entity.productlevel3.CountProductLevel3Wip;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Wip;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1CollectionService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3WipService;
import com.gdn.partners.pbp.web.model.ProductLevel3WipControllerPath;

public class ProductLevel3WipControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "API";
  private static final String DEFAULT_CLIENT_ID = "TEST";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_USERNAME = "DEVELOPER";
  private static final ProductLevel3WipSummaryCriteria DEFAULT_CRITERIA =
      ProductLevel3WipSummaryCriteria.ALL;
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLI-00001";
  private static final String DEFAULT_PRODUCT_NAME = "Product of Blibli.com";
  private static final String DEFAULT_PRODUCT_SKU = "BLI-00001-00001";
  private static final VerificationMode NEVER_CALLED = times(0);

  @Mock
  private ProductLevel3WipService productLevel3WipService;

  @Mock
  private ProductLevel3WipWrapper productLevel3WipWrapper;

  @Mock
  private ProductLevel1CollectionService productLevel1CollectionService;

  @InjectMocks
  private ProductLevel3WipController productLevel3WipController;

  private MockMvc mockMvc;
  private ObjectMapper objectMapper;

  @Captor
  private ArgumentCaptor<ProductLevel3WipSummaryRequest> argumentCaptor;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = MockMvcBuilders.standaloneSetup(this.productLevel3WipController)
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter())
        .build();
    this.objectMapper = new ObjectMapper(new JsonFactory());
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    verifyNoMoreInteractions(this.productLevel3WipService);
  }

  private ProductLevel3Wip generateProductLevel3Wip() throws Exception {
    ProductLevel3Wip productLevel3Wip = new ProductLevel3Wip();
    return productLevel3Wip;
  }

  private Page<ProductLevel3Wip> generateProductLevel3Wips() throws Exception {
    List<ProductLevel3Wip> productLevel3Wips = new ArrayList<ProductLevel3Wip>();
    productLevel3Wips.add(this.generateProductLevel3Wip());
    return new PageImpl<ProductLevel3Wip>(productLevel3Wips);
  }
  
  private ProductLevel3WipDTO generateProductLevel3WipDTO() throws Exception {
    ProductLevel3WipDTO productLevel3Wip = new ProductLevel3WipDTO();
    return productLevel3Wip;
  }

  private Page<ProductLevel3WipDTO> generateProductLevel3WipsDTO() throws Exception {
    List<ProductLevel3WipDTO> productLevel3Wips = new ArrayList<>();
    productLevel3Wips.add(this.generateProductLevel3WipDTO());
    return new PageImpl<ProductLevel3WipDTO>(productLevel3Wips);
  }

  private CountProductLevel3Wip generateCountProductLevel3Wip() throws Exception {
    CountProductLevel3Wip countProductLevel3Wip = new CountProductLevel3Wip();
    countProductLevel3Wip.getTotalItemsByCriterias().put(ProductLevel3WipSummaryCriteria.FAILED,
        0L);
    countProductLevel3Wip.getTotalItemsByCriterias()
        .put(ProductLevel3WipSummaryCriteria.IN_PROGRESS, 0L);
    countProductLevel3Wip.setTotalItems(0L);
    return countProductLevel3Wip;
  }

  private ProductLevel3WipSummaryRequest generateProductLevel3WipSummaryRequest() throws Exception {
    ProductLevel3WipSummaryRequest request = new ProductLevel3WipSummaryRequest();
    request.setCriteria(ProductLevel3WipControllerTest.DEFAULT_CRITERIA);
    request.setBusinessPartnerCode(ProductLevel3WipControllerTest.DEFAULT_BUSINESS_PARTNER_CODE);
    request.setProductName(ProductLevel3WipControllerTest.DEFAULT_PRODUCT_NAME);
    return request;
  }

  @Test
  public void filterSummaryWithStateTest() throws Exception {
    when(this.productLevel3WipService.findSummaryByFilterWithState(
        any(), any(Pageable.class)))
            .thenReturn(this.generateProductLevel3WipsDTO());
    URI uri = new URIBuilder()
        .setPath(ProductLevel3WipControllerPath.BASE_PATH
            + ProductLevel3WipControllerPath.FILTER_SUMMARY_WITH_STATE)
        .addParameter("storeId", ProductLevel3WipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3WipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel3WipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel3WipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel3WipControllerTest.DEFAULT_USERNAME).build();
    String requestBody =
        this.objectMapper.writeValueAsString(this.generateProductLevel3WipSummaryRequest());
    this.mockMvc
        .perform(MockMvcRequestBuilders.post(uri).content(requestBody)
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(this.productLevel3WipService).findSummaryByFilterWithState(
        argumentCaptor.capture(), any(Pageable.class));
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, argumentCaptor.getValue().getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_PRODUCT_NAME, argumentCaptor.getValue().getProductName());
  }

  @Test
  public void filterSummaryWithStateWithoutProductNameTest() throws Exception {
    when(this.productLevel3WipService.findSummaryByFilterWithState(
        any(),
        any(Pageable.class))).thenReturn(this.generateProductLevel3WipsDTO());
    URI uri = new URIBuilder()
        .setPath(ProductLevel3WipControllerPath.BASE_PATH
            + ProductLevel3WipControllerPath.FILTER_SUMMARY_WITH_STATE)
        .addParameter("storeId", ProductLevel3WipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3WipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel3WipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel3WipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel3WipControllerTest.DEFAULT_USERNAME).build();
    ProductLevel3WipSummaryRequest request = this.generateProductLevel3WipSummaryRequest();
    request.setProductName(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    this.mockMvc
        .perform(MockMvcRequestBuilders.post(uri).content(requestBody)
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(this.productLevel3WipService).findSummaryByFilterWithState(
        argumentCaptor.capture(),
        any(Pageable.class));
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, argumentCaptor.getValue().getBusinessPartnerCode());
    Assertions.assertEquals(null, argumentCaptor.getValue().getProductName());
  }

  @Test
  public void filterSummaryWithStateWithCriteriaExceptionTest() throws Exception {
    when(this.productLevel3WipService
        .findSummaryByFilterWithState(eq(generateProductLevel3WipSummaryRequest()), any(Pageable.class)))
        .thenReturn(this.generateProductLevel3WipsDTO());
    URI uri = new URIBuilder()
        .setPath(ProductLevel3WipControllerPath.BASE_PATH + ProductLevel3WipControllerPath.FILTER_SUMMARY_WITH_STATE)
        .addParameter("storeId", ProductLevel3WipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3WipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel3WipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel3WipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel3WipControllerTest.DEFAULT_USERNAME).build();
    ProductLevel3WipSummaryRequest request = this.generateProductLevel3WipSummaryRequest();
    request.setCriteria(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc
          .perform(MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      verify(this.productLevel3WipService, ProductLevel3WipControllerTest.NEVER_CALLED)
          .findSummaryByFilterWithState(eq(generateProductLevel3WipSummaryRequest()), any(Pageable.class));
    }
  }

  @Test
  public void filterSummaryWithStateWithBusinessPartnerCodeExceptionTest() throws Exception {
    when(this.productLevel3WipService.findSummaryByFilterWithState(
        eq(generateProductLevel3WipSummaryRequest()), any(Pageable.class)))
            .thenReturn(this.generateProductLevel3WipsDTO());
    URI uri = new URIBuilder()
        .setPath(ProductLevel3WipControllerPath.BASE_PATH
            + ProductLevel3WipControllerPath.FILTER_SUMMARY_WITH_STATE)
        .addParameter("storeId", ProductLevel3WipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3WipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel3WipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel3WipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel3WipControllerTest.DEFAULT_USERNAME).build();
    ProductLevel3WipSummaryRequest request = this.generateProductLevel3WipSummaryRequest();
    request.setBusinessPartnerCode(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc
          .perform(MockMvcRequestBuilders.post(uri).content(requestBody)
              .contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      verify(this.productLevel3WipService, ProductLevel3WipControllerTest.NEVER_CALLED)
          .findSummaryByFilterWithState(eq(generateProductLevel3WipSummaryRequest()),
              any(Pageable.class));
    }
  }

  @Test
  public void countSummaryWithStateTest() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ProductLevel3WipControllerPath.BASE_PATH
            + ProductLevel3WipControllerPath.COUNT_SUMMARY_WITH_STATE)
        .addParameter("storeId", ProductLevel3WipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3WipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel3WipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel3WipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel3WipControllerTest.DEFAULT_USERNAME)
        .addParameter("businessPartnerCode",
            ProductLevel3WipControllerTest.DEFAULT_BUSINESS_PARTNER_CODE)
        .build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(this.productLevel3WipWrapper)
        .countSummaryWithState(ProductLevel3WipControllerTest.DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void countSummaryWithStateNullCountTest() throws Exception {
    when(this.productLevel3WipWrapper.countSummaryWithState(anyString())).thenReturn(null);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3WipControllerPath.BASE_PATH
            + ProductLevel3WipControllerPath.COUNT_SUMMARY_WITH_STATE)
        .addParameter("storeId", ProductLevel3WipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3WipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel3WipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel3WipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel3WipControllerTest.DEFAULT_USERNAME)
        .addParameter("businessPartnerCode",
            ProductLevel3WipControllerTest.DEFAULT_BUSINESS_PARTNER_CODE)
        .build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(this.productLevel3WipWrapper)
        .countSummaryWithState(ProductLevel3WipControllerTest.DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void countSummaryByStateTest() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ProductLevel3WipControllerPath.BASE_PATH
            + ProductLevel3WipControllerPath.COUNT_SUMMARY_WITH_FILTER_STATE)
        .addParameter("storeId", ProductLevel3WipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3WipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel3WipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel3WipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel3WipControllerTest.DEFAULT_USERNAME)
        .addParameter("businessPartnerCode",
            ProductLevel3WipControllerTest.DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("type", Constants.PRIMARY)
        .build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(this.productLevel3WipWrapper).countSummaryByFilterType(
        ProductLevel3WipControllerTest.DEFAULT_BUSINESS_PARTNER_CODE, ProductLevel3WipControllerTest.DEFAULT_STORE_ID,
        Constants.PRIMARY);
  }

  @Test
  public void countSummaryByStateNullCountTest() throws Exception {
    when(this.productLevel3WipWrapper.countSummaryByFilterType(anyString(), anyString(), anyString())).thenReturn(null);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3WipControllerPath.BASE_PATH
            + ProductLevel3WipControllerPath.COUNT_SUMMARY_WITH_FILTER_STATE)
        .addParameter("storeId", ProductLevel3WipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3WipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel3WipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel3WipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel3WipControllerTest.DEFAULT_USERNAME)
        .addParameter("businessPartnerCode",
            ProductLevel3WipControllerTest.DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("type", Constants.SECONDARY.toLowerCase())
        .build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(this.productLevel3WipWrapper).countSummaryByFilterType(
        ProductLevel3WipControllerTest.DEFAULT_BUSINESS_PARTNER_CODE, ProductLevel3WipControllerTest.DEFAULT_STORE_ID,
        Constants.SECONDARY.toLowerCase());
  }

  @Test
  public void filterDetailByProductSKuTest() throws Exception {
    when(this.productLevel3WipWrapper.findByProductSku(eq(DEFAULT_STORE_ID), anyString(), Mockito.eq(false)))
        .thenReturn(new ProductLevel3WipDetailResponse());
    URI uri = new URIBuilder()
        .setPath(ProductLevel3WipControllerPath.BASE_PATH
            + ProductLevel3WipControllerPath.DETAIL_FILTER_PRODUCT_SKU)
        .addParameter("storeId", ProductLevel3WipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3WipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel3WipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel3WipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel3WipControllerTest.DEFAULT_USERNAME)
        .addParameter("productSku", ProductLevel3WipControllerTest.DEFAULT_PRODUCT_SKU).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(this.productLevel3WipWrapper).findByProductSku(eq(DEFAULT_STORE_ID), anyString(), Mockito.eq(false));
  }

  @Test
  public void filterDetailByProductSKuWithIsActiveTrueTest() throws Exception {
    when(this.productLevel3WipWrapper.findByProductSku(eq(DEFAULT_STORE_ID), anyString(), Mockito.eq(true)))
        .thenReturn(new ProductLevel3WipDetailResponse());
    URI uri = new URIBuilder()
        .setPath(ProductLevel3WipControllerPath.BASE_PATH
            + ProductLevel3WipControllerPath.DETAIL_FILTER_PRODUCT_SKU)
        .addParameter("storeId", ProductLevel3WipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3WipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel3WipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel3WipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel3WipControllerTest.DEFAULT_USERNAME)
        .addParameter("isActive", Boolean.TRUE.toString())
        .addParameter("productSku", ProductLevel3WipControllerTest.DEFAULT_PRODUCT_SKU).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri))
        .andExpect(MockMvcResultMatchers.status().isOk());
    verify(this.productLevel3WipWrapper).findByProductSku(eq(DEFAULT_STORE_ID), anyString(), Mockito.eq(true));
  }

  @Test
  public void filterDetailByProductSku_withNullProductLevel3() throws Exception {
    when(this.productLevel3WipWrapper.findByProductSku(eq(DEFAULT_STORE_ID), anyString(), Mockito.eq(true)))
        .thenReturn(null);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3WipControllerPath.BASE_PATH
            + ProductLevel3WipControllerPath.DETAIL_FILTER_PRODUCT_SKU)
        .addParameter("storeId", ProductLevel3WipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3WipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel3WipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel3WipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel3WipControllerTest.DEFAULT_USERNAME)
        .addParameter("productSku", ProductLevel3WipControllerTest.DEFAULT_PRODUCT_SKU).build();
    try {
      this.mockMvc.perform(MockMvcRequestBuilders.get(uri))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      verify(this.productLevel3WipWrapper).findByProductSku(eq(DEFAULT_STORE_ID), anyString(), Mockito.eq(true));
    }
  }

  @Test
  public void filterDetailByProductSku_withEmptyProductSku() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ProductLevel3WipControllerPath.BASE_PATH
            + ProductLevel3WipControllerPath.DETAIL_FILTER_PRODUCT_SKU)
        .addParameter("storeId", ProductLevel3WipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3WipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel3WipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel3WipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel3WipControllerTest.DEFAULT_USERNAME)
        .addParameter("productSku", "").build();
    try {
      this.mockMvc.perform(MockMvcRequestBuilders.get(uri))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
    }
  }

  @Test
  public void filterSummaryWithExpectationActivationDateTest() throws Exception {
    List<ProductLevel3WipDTO> dtos = Arrays.asList(new ProductLevel3WipDTO());
    Mockito.when(productLevel3WipService
        .findProductWipByExpectationActivationDateGreater(Mockito.eq(DEFAULT_STORE_ID),
            Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.any(Date.class))).thenReturn(dtos);
    URI uri = new URIBuilder().setPath(ProductLevel3WipControllerPath.BASE_PATH
        + ProductLevel3WipControllerPath.FILTER_SUMMARY_WITH_EXPECTATION_ACTIVATION_DATE)
        .addParameter("storeId", ProductLevel3WipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3WipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel3WipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel3WipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("businessPartnerCode",
            ProductLevel3WipControllerTest.DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("username", ProductLevel3WipControllerTest.DEFAULT_USERNAME).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    Mockito.verify(productLevel3WipService)
        .findProductWipByExpectationActivationDateGreater(Mockito.eq(DEFAULT_STORE_ID),
            Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.any(Date.class));
  }

  @Test
  public void filterSummaryWithExpectationActivationDateTest_withEmptyBpCode_failed()
      throws Exception {
    URI uri = new URIBuilder().setPath(ProductLevel3WipControllerPath.BASE_PATH
        + ProductLevel3WipControllerPath.FILTER_SUMMARY_WITH_EXPECTATION_ACTIVATION_DATE)
        .addParameter("storeId", ProductLevel3WipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3WipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel3WipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel3WipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("businessPartnerCode", StringUtils.EMPTY)
        .addParameter("username", ProductLevel3WipControllerTest.DEFAULT_USERNAME).build();
    try {
      Assertions.assertThrows(ServletException.class, () -> {
        this.mockMvc.perform(MockMvcRequestBuilders.get(uri).contentType(MediaType.APPLICATION_JSON))
            .andExpect(MockMvcResultMatchers.status().isOk())
            .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
      });
    } catch (Exception e) {
      if (e.getCause() instanceof ApplicationRuntimeException) {
        throw new ApplicationRuntimeException();
      }
    }
  }

  @Test
  public void filterSummaryWithExpectationActivationDateTest_withException_failed()
      throws Exception {
    Mockito.when(productLevel3WipService
        .findProductWipByExpectationActivationDateGreater(Mockito.eq(DEFAULT_STORE_ID),
            Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.any(Date.class)))
        .thenThrow(new ApplicationRuntimeException());
    URI uri = new URIBuilder().setPath(ProductLevel3WipControllerPath.BASE_PATH
        + ProductLevel3WipControllerPath.FILTER_SUMMARY_WITH_EXPECTATION_ACTIVATION_DATE)
        .addParameter("storeId", ProductLevel3WipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3WipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel3WipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel3WipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("businessPartnerCode",
            ProductLevel3WipControllerTest.DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("username", ProductLevel3WipControllerTest.DEFAULT_USERNAME).build();
    try {
      Assertions.assertThrows(ServletException.class, () -> {
        this.mockMvc.perform(MockMvcRequestBuilders.get(uri).contentType(MediaType.APPLICATION_JSON))
            .andExpect(MockMvcResultMatchers.status().isOk())
            .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
      });
    } finally {
      Mockito.verify(productLevel3WipService)
          .findProductWipByExpectationActivationDateGreater(Mockito.eq(DEFAULT_STORE_ID),
              Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.any(Date.class));
    }
  }

  @Test
  public void sendEmailForExceededActivationTest() throws Exception {
    Mockito.doNothing().when(productLevel3WipService)
        .sendMailForEmailExceededActivation(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE),
            Mockito.eq(ProductLevel3WipControllerTest.DEFAULT_USERNAME));

    URI uri = new URIBuilder().setPath(
        ProductLevel3WipControllerPath.BASE_PATH + ProductLevel3WipControllerPath.SEND_MAIL_FOR_EXCEEDED_ACTIVATION)
        .addParameter("storeId", ProductLevel3WipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3WipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel3WipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel3WipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("businessPartnerCode", ProductLevel3WipControllerTest.DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("username", ProductLevel3WipControllerTest.DEFAULT_USERNAME).build();
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());

    Mockito.verify(productLevel3WipService)
        .sendMailForEmailExceededActivation(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE),
            Mockito.eq(ProductLevel3WipControllerTest.DEFAULT_USERNAME));
  }

  @Test
  public void sendEmailForExceededActivationExceptionTest() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(productLevel3WipService)
        .sendMailForEmailExceededActivation(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_USERNAME));
    URI uri = new URIBuilder().setPath(
        ProductLevel3WipControllerPath.BASE_PATH + ProductLevel3WipControllerPath.SEND_MAIL_FOR_EXCEEDED_ACTIVATION)
        .addParameter("storeId", ProductLevel3WipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel3WipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel3WipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel3WipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("businessPartnerCode", ProductLevel3WipControllerTest.DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("username", ProductLevel3WipControllerTest.DEFAULT_USERNAME).build();
    try {
      Assertions.assertThrows(ServletException.class, () -> {
        this.mockMvc.perform(MockMvcRequestBuilders.post(uri).contentType(MediaType.APPLICATION_JSON))
            .andExpect(MockMvcResultMatchers.status().isOk())
            .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
      });
    } finally {
      Mockito.verify(productLevel3WipService)
          .sendMailForEmailExceededActivation(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.eq(DEFAULT_USERNAME));
    }
  }

}
