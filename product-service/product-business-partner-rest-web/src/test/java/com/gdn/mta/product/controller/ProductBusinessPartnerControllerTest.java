package com.gdn.mta.product.controller;

import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import com.gda.mta.product.dto.ProductL3RetryListRequest;
import com.gdn.mta.product.service.ProductLevel3Wrapper;
import com.gdn.partners.pbp.model.productlevel3.CreateProductLevel3Response;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.utils.URIBuilder;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
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
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.http.converter.ResourceHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import jakarta.servlet.ServletException;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductBusinessPartnerAttributeRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerAttributeResponse;
import com.gda.mta.product.dto.ProductBusinessPartnerRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerResponse;
import com.gda.mta.product.dto.ProductCopyRequest;
import com.gda.mta.product.dto.ProductItemBusinessPartnerRequest;
import com.gda.mta.product.dto.ProductItemBusinessPartnerResponse;
import com.gda.mta.product.dto.ProductItemWholesalePriceRequest;
import com.gda.mta.product.dto.RejectedSkuProductResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.PageableHelper;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerAttribute;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.RejectedSkuProductCollection;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.mta.product.service.ProductBusinessPartnerService;
import com.gdn.mta.product.service.util.WholesaleValidationUtil;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.mta.product.web.model.ProductBusinessPartnerControllerErrorMessage;
import com.gdn.mta.product.web.model.ProductBusinessPartnerControllerPath;
import com.gdn.mta.product.web.model.ProductControllerErrorMessage;
import com.gdn.partners.pbp.commons.constants.Constants;

public class ProductBusinessPartnerControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "channel-1";
  private static final String DEFAULT_CLIENT_ID = "client-1";
  private static final String DEFAULT_USERNAME = "developer";
  private static final Integer DEFAULT_PAGE = 0;
  private static final Integer DEFAULT_SIZE = 10;
  private static final VerificationMode AT_LEAST_ONE = Mockito.times(1);
  private static final VerificationMode NEVER_CALLED = Mockito.times(0);
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLI-00001";
  private static final String DEFAULT_PRODUCT_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_PRODUCT_ITEM_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_PICKUP_POINT_CODE = "PP-0000001";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_PRODUCT_BUSINESS_PARTNER_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_ATTRIBUTE_ID = UUID.randomUUID().toString();
  private static final String GDN_PRODUCT_SKU = "dummy-product-sku";
  private static final String MERCHANT_SKU = "dummy-merchant-sku";
  private static final String ITEM_SKU = "SOA-23423-00002-00001";
  private static final String SOURCE_PARTNER_CODE = "SOA-23423";
  private static final String ORDER_BY = "updatedDate";
  private static final String SORT_BY = "desc";
  private static final String CATEGORY_CODE = "code";

  private MockMvc mockMvc;
  private String requestId;
  private ObjectMapper objectMapper;

  @InjectMocks
  private ProductBusinessPartnerController productBusinessPartnerController;

  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Mock
  private WholesaleValidationUtil wholesaleValidationUtil;

  @Mock
  private ProductLevel3Wrapper productLevel3Wrapper;

  @Captor
  private ArgumentCaptor<ProductBusinessPartner> productBusinessPartnerArgumentCaptor;

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(getProductBusinessPartnerService());
    Mockito.verifyNoMoreInteractions(getWholesaleValidationUtil());
    Mockito.verifyNoMoreInteractions(productLevel3Wrapper);
  }

  public MockMvc getMockMvc() {
    return mockMvc;
  }

  public ObjectMapper getObjectMapper() {
    return objectMapper;
  }

  public ProductBusinessPartnerController getProductBusinessPartnerController() {
    return productBusinessPartnerController;
  }

  public ProductBusinessPartnerService getProductBusinessPartnerService() {
    return productBusinessPartnerService;
  }

  public WholesaleValidationUtil getWholesaleValidationUtil() {
    return wholesaleValidationUtil;
  }

  public void setWholesaleValidationUtil(WholesaleValidationUtil wholesaleValidationUtil) {
    this.wholesaleValidationUtil = wholesaleValidationUtil;
  }

  public String getRequestId() {
    return requestId;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    setMockMvc(MockMvcBuilders
        .standaloneSetup(getProductBusinessPartnerController())
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter()).build());
    setRequestId(UUID.randomUUID().toString());
    setObjectMapper(new ObjectMapper(new JsonFactory()));

    Mockito
        .when(this.productBusinessPartnerService.create(Mockito.anyString(),
            Mockito.any(ProductBusinessPartner.class), anyBoolean(),
            Mockito.anyList()))
        .thenReturn(CreateProductLevel3Response.builder().isLogisticsSaveSuccess(true).build());
    Mockito.doNothing().when(this.productBusinessPartnerService)
        .retryCreate(Mockito.anyString(), Mockito.anyString(), eq(null));
    Mockito.when(productBusinessPartnerService.getMinimumPrice(DEFAULT_STORE_ID)).thenReturn(1);
  }

  public void setMockMvc(MockMvc mockMvc) {
    this.mockMvc = mockMvc;
  }

  public void setObjectMapper(ObjectMapper objectMapper) {
    this.objectMapper = objectMapper;
  }

  public void setProductBusinessPartnerController(ProductBusinessPartnerController productBusinessPartnerController) {
    this.productBusinessPartnerController = productBusinessPartnerController;
  }

  public void setProductBusinessPartnerService(ProductBusinessPartnerService productBusinessPartnerService) {
    this.productBusinessPartnerService = productBusinessPartnerService;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  private ProductBusinessPartnerRequest generateProductBusinessPartnerRequest() throws Exception {
    ProductBusinessPartnerRequest request = new ProductBusinessPartnerRequest();
    request
        .setBusinessPartnerId(ProductBusinessPartnerControllerTest.DEFAULT_BUSINESS_PARTNER_CODE);
    request.setProductId(ProductBusinessPartnerControllerTest.DEFAULT_PRODUCT_ID);
    request.setProductName("Product Testing");
    request.setCategoryName("Category Testing");
    request.setBrand("Brand Testing");
    request.setProductItemBusinessPartners(new ArrayList<ProductItemBusinessPartnerRequest>());
    request.setProductBusinessPartnerAttributes(new ArrayList<ProductBusinessPartnerAttributeRequest>());
    ProductItemBusinessPartnerRequest itemRequest = new ProductItemBusinessPartnerRequest();
    itemRequest.setProductItemId(ProductBusinessPartnerControllerTest.DEFAULT_PRODUCT_ITEM_ID);
    itemRequest.setProductType(GdnBaseLookup.PRODUCT_TYPE_REGULAR);
    itemRequest.setPrice(10000D);
    itemRequest.setSalePrice(10000D);
    itemRequest.setStock(10);
    itemRequest.setMinimumStock(1);
    itemRequest.setPickupPointId(ProductBusinessPartnerControllerTest.DEFAULT_PICKUP_POINT_CODE);
    request.getProductItemBusinessPartners().add(itemRequest);
    ProductBusinessPartnerAttributeRequest attributeRequest = new ProductBusinessPartnerAttributeRequest();
    attributeRequest.setAttributeId(ProductBusinessPartnerControllerTest.DEFAULT_ATTRIBUTE_ID);
    attributeRequest.setValue("Attribute Value");
    attributeRequest.setMandatory(true);
    request.getProductBusinessPartnerAttributes().add(attributeRequest);
    return request;
  }

  private ProductBusinessPartnerRequest generateProductBusinessPartnerRequestWithWholesalePrice() throws Exception {
    ProductBusinessPartnerRequest request = new ProductBusinessPartnerRequest();
    request
        .setBusinessPartnerId(ProductBusinessPartnerControllerTest.DEFAULT_BUSINESS_PARTNER_CODE);
    request.setProductId(ProductBusinessPartnerControllerTest.DEFAULT_PRODUCT_ID);
    request.setProductName("Product Testing");
    request.setCategoryName("Category Testing");
    request.setCategoryCode(CATEGORY_CODE);
    request.setBrand("Brand Testing");
    request.setProductItemBusinessPartners(new ArrayList<ProductItemBusinessPartnerRequest>());
    request.setProductBusinessPartnerAttributes(new ArrayList<ProductBusinessPartnerAttributeRequest>());
    ProductItemBusinessPartnerRequest itemRequest = new ProductItemBusinessPartnerRequest();
    itemRequest.setProductItemId(ProductBusinessPartnerControllerTest.DEFAULT_PRODUCT_ITEM_ID);
    itemRequest.setProductType(GdnBaseLookup.PRODUCT_TYPE_REGULAR);
    itemRequest.setPrice(10D);
    itemRequest.setSalePrice(10D);
    itemRequest.setStock(5);
    itemRequest.setMinimumStock(1);
    itemRequest.setPickupPointId(ProductBusinessPartnerControllerTest.DEFAULT_PICKUP_POINT_CODE);
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest.setWholesaleDiscount(10.0);
    productItemWholesalePriceRequest.setQuantity(2);
    request.getProductItemBusinessPartners().add(itemRequest);
    itemRequest.setProductItemWholesalePriceRequests(new ArrayList<>(Arrays.asList(productItemWholesalePriceRequest)));
    ProductBusinessPartnerAttributeRequest attributeRequest = new ProductBusinessPartnerAttributeRequest();
    attributeRequest.setAttributeId(ProductBusinessPartnerControllerTest.DEFAULT_ATTRIBUTE_ID);
    attributeRequest.setValue("Attribute Value");
    attributeRequest.setMandatory(true);
    request.getProductBusinessPartnerAttributes().add(attributeRequest);
    return request;
  }

  @Test
  public void testDeleteProductBusinessPartner() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    ProductBusinessPartnerAttributeRequest productBusinessPartnerAttributeRequest =
        new ProductBusinessPartnerAttributeRequest();
    productBusinessPartnerAttributeRequest.setId(UUID.randomUUID().toString());
    productBusinessPartnerAttributeRequest.setValue("VALUE");
    productBusinessPartnerRequest
        .setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productBusinessPartnerRequest.getProductItemBusinessPartners()
        .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
            .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
            .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
            .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
            .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
            .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
            .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
            .pickupPointId("pickup-point-1").display(true).buyable(true)
            .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
        .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
            .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
            .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
            .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
            .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
            .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
            .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
            .pickupPointId("pickup-point-1").display(true).buyable(true)
            .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    for (ProductBusinessPartnerAttributeRequest productItemBusinessPartnerAttributeRequest : productBusinessPartnerRequest
        .getProductBusinessPartnerAttributes()) {
      ProductBusinessPartnerAttribute productBusinessPartnerAttribute = new ProductBusinessPartnerAttribute();
      BeanUtils.copyProperties(productItemBusinessPartnerAttributeRequest, productBusinessPartnerAttribute);
      productBusinessPartner.getProductBusinessPartnerAttributes().add(productBusinessPartnerAttribute);
    }
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, getRequestId());
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.DELETE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    MvcResult result =
        getMockMvc()
        .perform(
            post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
            .andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response), result.getResponse().getContentAsString());
    Mockito.verify(getProductBusinessPartnerService(), AT_LEAST_ONE).findById(productBusinessPartnerRequest.getId());
    Mockito.verify(getProductBusinessPartnerService(), AT_LEAST_ONE).update(productBusinessPartner);
    Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void testDeleteProductBusinessPartnerWithIdNull() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(null, DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    ProductBusinessPartnerAttributeRequest productBusinessPartnerAttributeRequest = new ProductBusinessPartnerAttributeRequest();
    productBusinessPartnerAttributeRequest.setId(null);
    productBusinessPartnerAttributeRequest.setValue("VALUE");
    productBusinessPartnerRequest
        .setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productBusinessPartnerRequest.getProductItemBusinessPartners()
        .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
            .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
            .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
            .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
            .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
            .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
            .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
            .pickupPointId("pickup-point-1").display(true).buyable(true)
            .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
        .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
            .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
            .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
            .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
            .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
            .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
            .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
            .pickupPointId("pickup-point-1").display(true).buyable(true)
            .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, getRequestId());
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.DELETE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    MvcResult result =
        getMockMvc()
            .perform(
                post(uri).accept(MediaType.APPLICATION_JSON)
                    .contentType(MediaType.APPLICATION_JSON)
                    .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
            .andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response), result.getResponse().getContentAsString());
    Mockito.verify(getProductBusinessPartnerService(), AT_LEAST_ONE).findById(productBusinessPartnerRequest.getId());
    Mockito.verify(getProductBusinessPartnerService(), AT_LEAST_ONE).update(productBusinessPartner);
    Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void testDeleteProductBusinessPartnerWithInvalidBrand() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
        .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
            .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
            .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
            .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
            .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
            .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
            .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
            .pickupPointId("pickup-point-1").display(true).buyable(true)
            .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
        .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
            .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
            .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
            .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
            .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
            .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
            .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
            .pickupPointId("pickup-point-1").display(true).buyable(true)
            .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.DELETE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.BRAND_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
    }
  }

  @Test
  public void testDeleteProductBusinessPartnerWithInvalidBusinessPartnerId() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false, null, "product-1",
            "BP1-00001-00001", true, new ArrayList<ProductItemBusinessPartnerRequest>(),
            new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.DELETE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.BUSINESS_PARTNER_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
    }
  }

  @Test
  public void testValidateProductSkuThrowException() throws Exception {
    when(productBusinessPartnerService.validateProductSku(eq(GDN_PRODUCT_SKU)))
        .thenThrow(Exception.class);

    mockMvc.perform(MockMvcRequestBuilders
        .get(ProductBusinessPartnerControllerPath.BASE_PATH
            + ProductBusinessPartnerControllerPath.VALIDATE_PRODUCT_SKU)
        .contentType(MediaType.APPLICATION_JSON).param("productSku", GDN_PRODUCT_SKU)
        .param("storeId", "dummy-store-id").param("channelId", "dummy-channel-id")
        .param("clientId", "dummy-client-id").param("requestId", "dummy-request-id")
        .param("username", "dummy-username")).andExpect(status().isOk());

    verify(productBusinessPartnerService, times(1)).validateProductSku(eq(GDN_PRODUCT_SKU));
  }

  @Test
  public void testValidateProductSku() throws Exception {
    when(productBusinessPartnerService.validateProductSku(eq(GDN_PRODUCT_SKU)))
        .thenReturn("dummy-result");

    mockMvc.perform(MockMvcRequestBuilders
        .get(ProductBusinessPartnerControllerPath.BASE_PATH
            + ProductBusinessPartnerControllerPath.VALIDATE_PRODUCT_SKU)
        .contentType(MediaType.APPLICATION_JSON).param("productSku", GDN_PRODUCT_SKU)
        .param("storeId", "dummy-store-id").param("channelId", "dummy-channel-id")
        .param("clientId", "dummy-client-id").param("requestId", "dummy-request-id")
        .param("username", "dummy-username")).andExpect(status().isOk());


    verify(productBusinessPartnerService,times(1)).validateProductSku(eq(GDN_PRODUCT_SKU));
  }

  @Test
  public void testDeleteProductBusinessPartnerWithInvalidCategoryName() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.DELETE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.CATEGORY_NAME_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
    }
  }

  @Test
  public void testDeleteProductBusinessPartnerWithInvalidMinimumPrice() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(0.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.DELETE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.MINIMUM_PRICE_VALUE_INVALID));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testDeleteProductBusinessPartnerWithInvalidMinimumSalePrice() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(0.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.DELETE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.MINIMUM_PRICE_VALUE_INVALID));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testDeleteProductBusinessPartnerWithInvalidMinimumStock() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(null)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.DELETE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.MINIMUM_STOCK_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testDeleteProductBusinessPartnerWithInvalidPickupPointId() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId(null).display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.DELETE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PICKUP_POINT_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testDeleteProductBusinessPartnerWithInvalidPrice() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(null)
        .salePrice(0.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners().add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(0.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.DELETE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PRICE_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testDeleteProductBusinessPartnerWithInvalidProductId() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", null, "BP1-00001-00001", true, new ArrayList<ProductItemBusinessPartnerRequest>(),
            new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.DELETE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PRODUCT_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
    }
  }

  @Test
  public void testDeleteProductBusinessPartnerWithInvalidProductItemBusinessPartners() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.DELETE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.PRODUCT_ITEM_BUSINESS_PARTNERS_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
    }
  }

  @Test
  public void testDeleteProductBusinessPartnerWithInvalidProductItemId() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId(null)
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId(null)
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.DELETE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PRODUCT_ITEM_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testDeleteProductBusinessPartnerWithInvalidProductName() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.DELETE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PRODUCT_NAME_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
    }
  }

  @Test
  public void testDeleteProductBusinessPartnerWithInvalidProductType() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(null).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners().add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.DELETE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PRODUCT_TYPE_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testDeleteProductBusinessPartnerWithInvalidSalePrice() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners().add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(null).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.DELETE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.SALE_PRICE_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testDeleteProductBusinessPartnerWithInvalidStock() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(null).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.DELETE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.STOCK_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testDeleteProductBusinessPartnerWithInvalidUpdatedBy() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), null, false, "business-partner-1",
            "product-1", "BP1-00001-00001", true, new ArrayList<ProductItemBusinessPartnerRequest>(),
            new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.DELETE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
    }
  }

  @Test
  public void testDeleteProductBusinessPartnerWithInvalidUpdatedDate() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, null, DEFAULT_USERNAME, false, "business-partner-1", "product-1",
            "BP1-00001-00001", true, new ArrayList<ProductItemBusinessPartnerRequest>(),
            new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.DELETE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
    }
  }

  @Test
  public void testFilterProductBusinessPartnerSummaryByActivatedTrue() throws Exception {
    List<ProductBusinessPartnerResponse> productBusinessPartnerResponses =
        new ArrayList<ProductBusinessPartnerResponse>();
    productBusinessPartnerResponses.add(new ProductBusinessPartnerResponse(UUID.randomUUID().toString(),
        DEFAULT_STORE_ID, Calendar.getInstance().getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(),
        DEFAULT_USERNAME, "business-partner-1", "product-1", "BP1-00001-00001", true,
        new ArrayList<ProductItemBusinessPartnerResponse>(), new ArrayList<ProductBusinessPartnerAttributeResponse>()));
    productBusinessPartnerResponses.add(new ProductBusinessPartnerResponse(UUID.randomUUID().toString(),
        DEFAULT_STORE_ID, Calendar.getInstance().getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(),
        DEFAULT_USERNAME, "business-partner-2", "product-1", "BP2-00001-00001", true,
        new ArrayList<ProductItemBusinessPartnerResponse>(), new ArrayList<ProductBusinessPartnerAttributeResponse>()));
    productBusinessPartnerResponses.add(new ProductBusinessPartnerResponse(UUID.randomUUID().toString(),
        DEFAULT_STORE_ID, Calendar.getInstance().getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(),
        DEFAULT_USERNAME, "business-partner-3", "product-2", "BP3-00001-00002", true,
        new ArrayList<ProductItemBusinessPartnerResponse>(), new ArrayList<ProductBusinessPartnerAttributeResponse>()));
    List<ProductBusinessPartner> productBusinessPartners = new ArrayList<ProductBusinessPartner>();
    for (ProductBusinessPartnerResponse productBusinessPartnerResponse : productBusinessPartnerResponses) {
      ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
      BeanUtils.copyProperties(productBusinessPartnerResponse, productBusinessPartner,
          "productItemBusinessPartners", "productBusinessPartnerAttributes");
      productBusinessPartners.add(productBusinessPartner);
    }
    Page<ProductBusinessPartner> page = new PageImpl<ProductBusinessPartner>(productBusinessPartners);
    Pageable pageable = PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE);
    GdnRestListResponse<ProductBusinessPartnerResponse> response =
        new GdnRestListResponse<ProductBusinessPartnerResponse>(null, null, true, productBusinessPartnerResponses,
            new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(), page.getTotalElements()), getRequestId());
    when(getProductBusinessPartnerService().findByActivatedTrue(DEFAULT_STORE_ID, pageable)).thenReturn(page);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.ACTIVATED)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    MvcResult result =
        getMockMvc()
        .perform(
            MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
            .andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response), result.getResponse().getContentAsString());
    Mockito.verify(getProductBusinessPartnerService(), AT_LEAST_ONE).findByActivatedTrue(DEFAULT_STORE_ID, pageable);
  }

  @Test
  public void testFilterProductBusinessPartnerSummaryByBusinessPartnerId() throws Exception {
    List<ProductBusinessPartnerResponse> productBusinessPartnerResponses =
        new ArrayList<ProductBusinessPartnerResponse>();
    productBusinessPartnerResponses.add(new ProductBusinessPartnerResponse(UUID.randomUUID().toString(),
        DEFAULT_STORE_ID, Calendar.getInstance().getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(),
        DEFAULT_USERNAME, "business-partner-1", "product-1", "BP1-00001-00001", true,
        new ArrayList<ProductItemBusinessPartnerResponse>(), new ArrayList<ProductBusinessPartnerAttributeResponse>()));
    productBusinessPartnerResponses.add(new ProductBusinessPartnerResponse(UUID.randomUUID().toString(),
        DEFAULT_STORE_ID, Calendar.getInstance().getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(),
        DEFAULT_USERNAME, "business-partner-1", "product-2", "BP1-00001-00002", true,
        new ArrayList<ProductItemBusinessPartnerResponse>(), new ArrayList<ProductBusinessPartnerAttributeResponse>()));
    List<ProductBusinessPartner> productBusinessPartners = new ArrayList<ProductBusinessPartner>();
    for (ProductBusinessPartnerResponse productBusinessPartnerResponse : productBusinessPartnerResponses) {
      ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
      BeanUtils.copyProperties(productBusinessPartnerResponse, productBusinessPartner,
          "productItemBusinessPartners", "productBusinessPartnerAttributes");
      productBusinessPartners.add(productBusinessPartner);
    }
    Page<ProductBusinessPartner> page = new PageImpl<ProductBusinessPartner>(productBusinessPartners);
    Pageable pageable = PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE);
    GdnRestListResponse<ProductBusinessPartnerResponse> response =
        new GdnRestListResponse<ProductBusinessPartnerResponse>(null, null, true, productBusinessPartnerResponses,
            new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(), page.getTotalElements()), getRequestId());
    when(
        getProductBusinessPartnerService().findByBusinessPartnerId(DEFAULT_STORE_ID, "business-partner-1", pageable))
        .thenReturn(page);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.FILTER_BUSINESS_PARTNER.replaceAll("\\{id\\}",
                        "business-partner-1")).addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    MvcResult result =
        getMockMvc()
        .perform(
            MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
            .andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response), result.getResponse().getContentAsString());
    Mockito.verify(getProductBusinessPartnerService(), AT_LEAST_ONE).findByBusinessPartnerId(DEFAULT_STORE_ID,
        "business-partner-1", pageable);
  }

  @Test
  public void testFilterProductBusinessPartnerSummaryByPickupPointId() throws Exception {
    List<ProductBusinessPartnerResponse> productBusinessPartnerResponses =
        new ArrayList<ProductBusinessPartnerResponse>();
    productBusinessPartnerResponses.add(new ProductBusinessPartnerResponse(UUID.randomUUID().toString(),
        DEFAULT_STORE_ID, Calendar.getInstance().getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(),
        DEFAULT_USERNAME, "business-partner-1", "product-1", "BP1-00001-00001", true,
        new ArrayList<ProductItemBusinessPartnerResponse>(), new ArrayList<ProductBusinessPartnerAttributeResponse>()));
    productBusinessPartnerResponses.add(new ProductBusinessPartnerResponse(UUID.randomUUID().toString(),
        DEFAULT_STORE_ID, Calendar.getInstance().getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(),
        DEFAULT_USERNAME, "business-partner-1", "product-2", "BP1-00001-00002", true,
        new ArrayList<ProductItemBusinessPartnerResponse>(), new ArrayList<ProductBusinessPartnerAttributeResponse>()));
    List<ProductBusinessPartner> productBusinessPartners = new ArrayList<ProductBusinessPartner>();
    for (ProductBusinessPartnerResponse productBusinessPartnerResponse : productBusinessPartnerResponses) {
      ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
      BeanUtils.copyProperties(productBusinessPartnerResponse, productBusinessPartner,
          "productItemBusinessPartners", "productBusinessPartnerAttributes");
      productBusinessPartners.add(productBusinessPartner);
    }
    Page<ProductBusinessPartner> page = new PageImpl<ProductBusinessPartner>(productBusinessPartners);
    Pageable pageable = PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE);
    GdnRestListResponse<ProductBusinessPartnerResponse> response =
        new GdnRestListResponse<ProductBusinessPartnerResponse>(null, null, true, productBusinessPartnerResponses,
            new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(), page.getTotalElements()), getRequestId());
    when(getProductBusinessPartnerService().findByPickupPointId(DEFAULT_STORE_ID, "pickup-point-1", pageable))
    .thenReturn(page);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.FILTER_PICKUP_POINT.replaceAll(
                        "\\{pickupPointId\\}", "pickup-point-1"))
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    MvcResult result =
        getMockMvc()
        .perform(
            MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
            .andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response), result.getResponse().getContentAsString());
    Mockito.verify(getProductBusinessPartnerService(), AT_LEAST_ONE).findByPickupPointId(DEFAULT_STORE_ID,
        "pickup-point-1", pageable);
  }

  @Test
  public void testGetProductBusinessPartner() throws Exception {
    ProductBusinessPartnerResponse productBusinessPartnerResponse =
        new ProductBusinessPartnerResponse(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, "business-partner-1",
            "product-1", "BP1-00001-00001", true, new ArrayList<ProductItemBusinessPartnerResponse>(),
            new ArrayList<ProductBusinessPartnerAttributeResponse>());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerResponse, productBusinessPartner);
    GdnRestSingleResponse<ProductBusinessPartnerResponse> response =
        new GdnRestSingleResponse<ProductBusinessPartnerResponse>(null, null, true, productBusinessPartnerResponse,
            getRequestId());
    when(getProductBusinessPartnerService().findById(productBusinessPartnerResponse.getId())).thenReturn(
        productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.DETAIL.replaceAll("\\{id\\}",
                        productBusinessPartnerResponse.getId()))
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    MvcResult result =
        getMockMvc()
        .perform(
            MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
            .andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response), result.getResponse().getContentAsString());
    Mockito.verify(getProductBusinessPartnerService(), AT_LEAST_ONE).findById(productBusinessPartnerResponse.getId());
  }

  @Test
  public void testGetProductBusinessPartnerSummary() throws Exception {
    List<ProductBusinessPartnerResponse> productBusinessPartnerResponses =
        new ArrayList<ProductBusinessPartnerResponse>();
    productBusinessPartnerResponses.add(new ProductBusinessPartnerResponse(UUID.randomUUID().toString(),
        DEFAULT_STORE_ID, Calendar.getInstance().getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(),
        DEFAULT_USERNAME, "business-partner-1", "product-1", "BP1-00001-00001", true,
        new ArrayList<ProductItemBusinessPartnerResponse>(), new ArrayList<ProductBusinessPartnerAttributeResponse>()));
    productBusinessPartnerResponses.add(new ProductBusinessPartnerResponse(UUID.randomUUID().toString(),
        DEFAULT_STORE_ID, Calendar.getInstance().getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(),
        DEFAULT_USERNAME, "business-partner-2", "product-1", "BP2-00001-00001", true,
        new ArrayList<ProductItemBusinessPartnerResponse>(), new ArrayList<ProductBusinessPartnerAttributeResponse>()));
    productBusinessPartnerResponses.add(new ProductBusinessPartnerResponse(UUID.randomUUID().toString(),
        DEFAULT_STORE_ID, Calendar.getInstance().getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(),
        DEFAULT_USERNAME, "business-partner-3", "product-2", "BP3-00001-00002", true,
        new ArrayList<ProductItemBusinessPartnerResponse>(), new ArrayList<ProductBusinessPartnerAttributeResponse>()));
    List<ProductBusinessPartner> productBusinessPartners = new ArrayList<ProductBusinessPartner>();
    for (ProductBusinessPartnerResponse productBusinessPartnerResponse : productBusinessPartnerResponses) {
      ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
      BeanUtils.copyProperties(productBusinessPartnerResponse, productBusinessPartner,
          "productItemBusinessPartners", "productBusinessPartnerAttributes");
      productBusinessPartners.add(productBusinessPartner);
    }
    Page<ProductBusinessPartner> page = new PageImpl<ProductBusinessPartner>(productBusinessPartners);
    Pageable pageable = PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE);
    GdnRestListResponse<ProductBusinessPartnerResponse> response =
        new GdnRestListResponse<ProductBusinessPartnerResponse>(null, null, true, productBusinessPartnerResponses,
            new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(), page.getTotalElements()), getRequestId());
    when(getProductBusinessPartnerService().findByStoreId(DEFAULT_STORE_ID, pageable)).thenReturn(page);
    URI uri =
        new URIBuilder().setPath(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.ROOT)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    MvcResult result =
        getMockMvc()
        .perform(
            MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
            .andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response), result.getResponse().getContentAsString());
    Mockito.verify(getProductBusinessPartnerService(), AT_LEAST_ONE).findByStoreId(DEFAULT_STORE_ID, pageable);
  }

  @Test
  public void testRetrySaveProductBusinessPartner() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest = new ProductBusinessPartnerRequest();
    productBusinessPartnerRequest.setUpdatedBy(DEFAULT_USERNAME);
    productBusinessPartnerRequest.setUpdatedDate(Calendar.getInstance().getTime());
    productBusinessPartnerRequest.setBusinessPartnerId("BP1-00001");
    productBusinessPartnerRequest.setGdnProductSku("BP1-00001-00001");
    productBusinessPartnerRequest.setId(UUID.randomUUID().toString());
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, getRequestId());
    Mockito.doNothing().when(getProductBusinessPartnerService())
    .retrySave(Mockito.any(ProductBusinessPartner.class));
    URI uri =
        new URIBuilder()
    .setPath(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.RETRY_CREATE)
    .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
    .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
    .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    MvcResult result =
        getMockMvc()
        .perform(
            post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
            .andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response), result.getResponse().getContentAsString());
    Mockito.verify(getProductBusinessPartnerService(), AT_LEAST_ONE)
        .retrySave(Mockito.any(ProductBusinessPartner.class));
  }

  @Test
  public void testGetProductBusinessPartnerWithAttribute() throws Exception {
    ProductBusinessPartnerResponse productBusinessPartnerResponse =
        new ProductBusinessPartnerResponse(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, "business-partner-1",
            "product-1", "BP1-00001-00001", true, new ArrayList<ProductItemBusinessPartnerResponse>(),
            new ArrayList<ProductBusinessPartnerAttributeResponse>());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerResponse, productBusinessPartner);
    ProductBusinessPartnerAttribute productBusinessPartnerAttribute = new ProductBusinessPartnerAttribute();
    productBusinessPartnerAttribute.setAttributeId(UUID.randomUUID().toString());
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setId(UUID.randomUUID().toString());
    productBusinessPartner.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttribute));
    productBusinessPartner.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    GdnRestSingleResponse<ProductBusinessPartnerResponse> response =
        new GdnRestSingleResponse<ProductBusinessPartnerResponse>(null, null, true, productBusinessPartnerResponse,
            getRequestId());
    when(getProductBusinessPartnerService().findById(productBusinessPartnerResponse.getId())).thenReturn(
        productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.DETAIL.replaceAll("\\{id\\}",
                    productBusinessPartnerResponse.getId()))
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    MvcResult result =
        getMockMvc()
            .perform(
                MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
                    .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
            .andReturn();
    Mockito.verify(getProductBusinessPartnerService(), AT_LEAST_ONE).findById(productBusinessPartnerResponse.getId());
  }

  @Test
  public void testRetrySaveProductBusinessPartnerWithInvalidProductBusinessPartnerId() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest = new ProductBusinessPartnerRequest();
    productBusinessPartnerRequest.setUpdatedBy(DEFAULT_USERNAME);
    productBusinessPartnerRequest.setUpdatedDate(Calendar.getInstance().getTime());
    productBusinessPartnerRequest.setGdnProductSku("BP1-00001-00001");
    Mockito.doNothing().when(getProductBusinessPartnerService())
    .retrySave(Mockito.any(ProductBusinessPartner.class));
    URI uri =
        new URIBuilder()
    .setPath(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.RETRY_CREATE)
    .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
    .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
    .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PRODUCT_BUSINESS_PARTNER_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).retrySave(
          Mockito.any(ProductBusinessPartner.class));
    }
  }

  @Test
  public void testRetrySaveProductBusinessPartnerWithInvalidUpdatedBy() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest = new ProductBusinessPartnerRequest();
    productBusinessPartnerRequest.setUpdatedDate(Calendar.getInstance().getTime());
    productBusinessPartnerRequest.setBusinessPartnerId("BP1-00001");
    productBusinessPartnerRequest.setGdnProductSku("BP1-00001-00001");
    Mockito.doNothing().when(getProductBusinessPartnerService())
    .retrySave(Mockito.any(ProductBusinessPartner.class));
    URI uri =
        new URIBuilder()
    .setPath(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.RETRY_CREATE)
    .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
    .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
    .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).retrySave(
          Mockito.any(ProductBusinessPartner.class));
    }
  }

  @Test
  public void testRetrySaveProductBusinessPartnerWithInvalidUpdatedDate() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest = new ProductBusinessPartnerRequest();
    productBusinessPartnerRequest.setUpdatedBy(DEFAULT_USERNAME);
    productBusinessPartnerRequest.setBusinessPartnerId("BP1-00001");
    productBusinessPartnerRequest.setGdnProductSku("BP1-00001-00001");
    Mockito.doNothing().when(getProductBusinessPartnerService())
    .retrySave(Mockito.any(ProductBusinessPartner.class));
    URI uri =
        new URIBuilder()
    .setPath(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.RETRY_CREATE)
    .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
    .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
    .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).retrySave(
          Mockito.any(ProductBusinessPartner.class));
    }
  }

  @Test
  public void testSaveProductBusinessPartner() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, getRequestId());
    when(getProductBusinessPartnerService().save(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    MvcResult result =
        getMockMvc()
        .perform(
            post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
            .andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response), result.getResponse().getContentAsString());
    Mockito.verify(getProductBusinessPartnerService(), AT_LEAST_ONE).save(productBusinessPartner);
    Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void testSaveProductBusinessPartnerWithActivatedFalse() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, getRequestId());
    when(getProductBusinessPartnerService().saveWithActivatedFalse(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_ACTIVATED_FALSE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    MvcResult result =
        getMockMvc()
        .perform(
            post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
            .andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response), result.getResponse().getContentAsString());
    Mockito.verify(getProductBusinessPartnerService(), AT_LEAST_ONE).saveWithActivatedFalse(productBusinessPartner);
    Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void testSaveProductBusinessPartnerWithActivatedFalseReturnID() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    GdnRestSimpleResponse<String> response = new GdnRestSimpleResponse<String>(getRequestId(),"");
    when(getProductBusinessPartnerService().saveWithActivatedFalse(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_ACTIVATED_FALSE_RETURN_ID)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    MvcResult result =
        getMockMvc()
        .perform(
            post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
            .andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response), result.getResponse().getContentAsString());
    Mockito.verify(getProductBusinessPartnerService(), AT_LEAST_ONE).saveWithActivatedFalse(productBusinessPartner);
    Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void testSaveProductBusinessPartnerWithActivatedFalseReturnIDCreatedByEmpty() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    GdnRestSimpleResponse<String> response = new GdnRestSimpleResponse<String>(getRequestId(),"");
    when(getProductBusinessPartnerService().saveWithActivatedFalse(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_ACTIVATED_FALSE_RETURN_ID)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    MvcResult result =
        getMockMvc()
        .perform(
            post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
            .andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response), result.getResponse().getContentAsString());
    Mockito.verify(getProductBusinessPartnerService(), AT_LEAST_ONE).saveWithActivatedFalse(productBusinessPartner);
    Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void testSaveProductBusinessPartnerWithActivatedFalseAndInvalidBrand() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().saveWithActivatedFalse(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_ACTIVATED_FALSE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.BRAND_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).saveWithActivatedFalse(productBusinessPartner);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithActivatedFalseAndInvalidBusinessPartnerId() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false, null, "product-1",
            "BP1-00001-00001", true, new ArrayList<ProductItemBusinessPartnerRequest>(),
            new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().saveWithActivatedFalse(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_ACTIVATED_FALSE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.BUSINESS_PARTNER_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).saveWithActivatedFalse(productBusinessPartner);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithActivatedFalseAndInvalidCategoryName() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().saveWithActivatedFalse(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_ACTIVATED_FALSE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.CATEGORY_NAME_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).saveWithActivatedFalse(productBusinessPartner);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithActivatedFalseAndInvalidCreatedBy() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), null, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false, "business-partner-1",
            "product-1", "BP1-00001-00001", true, new ArrayList<ProductItemBusinessPartnerRequest>(),
            new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().saveWithActivatedFalse(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_ACTIVATED_FALSE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).saveWithActivatedFalse(productBusinessPartner);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithActivatedFalseAndInvalidCreatedDate() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, null, DEFAULT_USERNAME,
            Calendar.getInstance().getTime(), DEFAULT_USERNAME, false, "business-partner-1", "product-1",
            "BP1-00001-00001", true, new ArrayList<ProductItemBusinessPartnerRequest>(),
            new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().saveWithActivatedFalse(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_ACTIVATED_FALSE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).saveWithActivatedFalse(productBusinessPartner);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithActivatedFalseAndInvalidMinimumStock() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(null)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().saveWithActivatedFalse(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_ACTIVATED_FALSE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.MINIMUM_STOCK_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).saveWithActivatedFalse(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithActivatedFalseAndInvalidPickupPointId() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId(null).display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().saveWithActivatedFalse(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_ACTIVATED_FALSE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PICKUP_POINT_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).saveWithActivatedFalse(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithActivatedFalseAndInvalidPrice() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(null)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(null)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().saveWithActivatedFalse(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_ACTIVATED_FALSE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PRICE_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).saveWithActivatedFalse(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithActivatedFalseAndInvalidProductId() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", null, "BP1-00001-00001", true, new ArrayList<ProductItemBusinessPartnerRequest>(),
            new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().saveWithActivatedFalse(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_ACTIVATED_FALSE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PRODUCT_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).saveWithActivatedFalse(productBusinessPartner);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithActivatedFalseAndInvalidProductItemBusinessPartners() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().saveWithActivatedFalse(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_ACTIVATED_FALSE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.PRODUCT_ITEM_BUSINESS_PARTNERS_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).saveWithActivatedFalse(productBusinessPartner);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithActivatedFalseAndInvalidProductItemId() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId(null)
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId(null)
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().saveWithActivatedFalse(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_ACTIVATED_FALSE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PRODUCT_ITEM_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).saveWithActivatedFalse(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithActivatedFalseAndInvalidProductName() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().saveWithActivatedFalse(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_ACTIVATED_FALSE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PRODUCT_NAME_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).saveWithActivatedFalse(productBusinessPartner);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithActivatedFalseAndInvalidProductType() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(null).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().saveWithActivatedFalse(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
    .setPath(
        ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.CREATE_ACTIVATED_FALSE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PRODUCT_TYPE_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).saveWithActivatedFalse(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithActivatedFalseAndInvalidSalePrice() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(null).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().saveWithActivatedFalse(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
    .setPath(
        ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.CREATE_ACTIVATED_FALSE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.SALE_PRICE_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).saveWithActivatedFalse(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithActivatedFalseAndInvalidStock() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(null).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(null).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().saveWithActivatedFalse(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
    .setPath(
        ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.CREATE_ACTIVATED_FALSE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.STOCK_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).saveWithActivatedFalse(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithInvalidBrand() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().save(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.BRAND_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).save(productBusinessPartner);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithInvalidBusinessPartnerId() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false, null, "product-1",
            "BP1-00001-00001", true, new ArrayList<ProductItemBusinessPartnerRequest>(),
            new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().save(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.BUSINESS_PARTNER_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).save(productBusinessPartner);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithInvalidCategoryName() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().save(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.CATEGORY_NAME_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).save(productBusinessPartner);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithInvalidCreatedBy() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), null, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false, "business-partner-1",
            "product-1", "BP1-00001-00001", true, new ArrayList<ProductItemBusinessPartnerRequest>(),
            new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().save(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).save(productBusinessPartner);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithInvalidCreatedDate() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, null, DEFAULT_USERNAME,
            Calendar.getInstance().getTime(), DEFAULT_USERNAME, false, "business-partner-1", "product-1",
            "BP1-00001-00001", true, new ArrayList<ProductItemBusinessPartnerRequest>(),
            new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().save(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).save(productBusinessPartner);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithInvalidMinimumPrice() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(0.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(0.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().save(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.MINIMUM_PRICE_VALUE_INVALID));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).save(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithInvalidMinimumSalePrice() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(0.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().save(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.MINIMUM_PRICE_VALUE_INVALID));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).save(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithInvalidMinimumStock() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(null)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(null)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().save(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.MINIMUM_STOCK_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).save(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithInvalidPickupPointId() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId(null).display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().save(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PICKUP_POINT_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).save(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithInvalidPrice() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(null)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners().add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().save(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PRICE_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).save(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithInvalidProductId() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", null, "BP1-00001-00001", true, new ArrayList<ProductItemBusinessPartnerRequest>(),
            new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().save(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PRODUCT_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).save(productBusinessPartner);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithInvalidProductItemBusinessPartners() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().save(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.PRODUCT_ITEM_BUSINESS_PARTNERS_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).save(productBusinessPartner);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithInvalidProductItemId() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId(null)
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId(null)
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().save(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PRODUCT_ITEM_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).save(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithInvalidProductName() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().save(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PRODUCT_NAME_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).save(productBusinessPartner);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithInvalidProductType() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(null).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().save(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PRODUCT_TYPE_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).save(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithInvalidSalePrice() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(null).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().save(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.SALE_PRICE_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).save(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testSaveProductBusinessPartnerWithInvalidStock() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(null).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().save(productBusinessPartner)).thenReturn("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.STOCK_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).save(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateProductBusinessPartner() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, getRequestId());
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    MvcResult result =
        getMockMvc()
        .perform(
            post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
            .andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response), result.getResponse().getContentAsString());
    Mockito.verify(getProductBusinessPartnerService(), AT_LEAST_ONE).findById(productBusinessPartnerRequest.getId());
    Mockito.verify(getProductBusinessPartnerService(), AT_LEAST_ONE).update(productBusinessPartner);
    Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void testUpdateProductBusinessPartnerWithInvalidBrand() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.BRAND_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
    }
  }

  @Test
  public void testUpdateProductBusinessPartnerWithInvalidBusinessPartnerId() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false, null, "product-1",
            "BP1-00001-00001", true, new ArrayList<ProductItemBusinessPartnerRequest>(),
            new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.BUSINESS_PARTNER_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
    }
  }

  @Test
  public void testUpdateProductBusinessPartnerWithInvalidCategoryName() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.CATEGORY_NAME_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
    }
  }

  @Test
  public void testUpdateProductBusinessPartnerWithInvalidMinimumPrice() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(0.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.MINIMUM_PRICE_VALUE_INVALID));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateProductBusinessPartnerWithInvalidMinimumSalePrice() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(0.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(0.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.MINIMUM_PRICE_VALUE_INVALID));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateProductBusinessPartnerWithInvalidMinimumStock() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(null)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.MINIMUM_STOCK_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateProductBusinessPartnerWithInvalidPickupPointId() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId(null).display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PICKUP_POINT_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateProductBusinessPartnerWithInvalidPrice() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(null)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PRICE_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateProductBusinessPartnerWithInvalidProductId() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", null, "BP1-00001-00001", true, new ArrayList<ProductItemBusinessPartnerRequest>(),
            new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PRODUCT_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
    }
  }

  @Test
  public void testUpdateProductBusinessPartnerWithInvalidProductItemBusinessPartners() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.PRODUCT_ITEM_BUSINESS_PARTNERS_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
    }
  }

  @Test
  public void testUpdateProductBusinessPartnerWithInvalidProductItemId() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId(null)
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PRODUCT_ITEM_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateProductBusinessPartnerWithInvalidProductName() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PRODUCT_NAME_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
    }
  }

  @Test
  public void testUpdateProductBusinessPartnerWithInvalidProductType() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(null).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(null).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PRODUCT_TYPE_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateProductBusinessPartnerWithInvalidSalePrice() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(null).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.SALE_PRICE_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateProductBusinessPartnerWithInvalidStock() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), DEFAULT_USERNAME, false,
            "business-partner-1", "product-1", "BP1-00001-00001", true,
            new ArrayList<ProductItemBusinessPartnerRequest>(), new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.setProductName("Produk 1");
    productBusinessPartnerRequest.setCategoryName("Kategori 1");
    productBusinessPartnerRequest.setBrand("Testing");
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(null).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.STOCK_MUST_NOT_BE_BLANK));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void testUpdateProductBusinessPartnerWithInvalidUpdatedBy() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, Calendar.getInstance().getTime(), null, false, "business-partner-1",
            "product-1", "BP1-00001-00001", true, new ArrayList<ProductItemBusinessPartnerRequest>(),
            new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
    }
  }

  @Test
  public void testUpdateProductBusinessPartnerWithInvalidUpdatedDate() throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest(UUID.randomUUID().toString(), DEFAULT_STORE_ID, Calendar.getInstance()
            .getTime(), DEFAULT_USERNAME, null, DEFAULT_USERNAME, false, "business-partner-1", "product-1",
            "BP1-00001-00001", true, new ArrayList<ProductItemBusinessPartnerRequest>(),
            new ArrayList<ProductBusinessPartnerAttributeRequest>());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-1")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00001").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    productBusinessPartnerRequest.getProductItemBusinessPartners()
    .add(new ProductItemBusinessPartnerRequest.Builder().id(UUID.randomUUID().toString())
        .storeId(DEFAULT_STORE_ID).createdDate(Calendar.getInstance().getTime())
        .createdBy(DEFAULT_USERNAME).updatedDate(Calendar.getInstance().getTime())
        .updatedBy(DEFAULT_USERNAME).markForDelete(false).productItemId("product-item-2")
        .productType(1).gdnProductItemSku("BP1-00001-00001-00002").price(10000.0)
        .salePrice(10000.0).saleStartDate(Calendar.getInstance().getTime())
        .saleEndDate(Calendar.getInstance().getTime()).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").display(true).buyable(true)
        .installation(false).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(productBusinessPartnerRequest, productBusinessPartner,
        "productItemBusinessPartners", "productBusinessPartnerAttributes");
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productBusinessPartnerRequest
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    when(getProductBusinessPartnerService().findById(productBusinessPartnerRequest.getId())).thenReturn(
        productBusinessPartner);
    Mockito.doNothing().when(getProductBusinessPartnerService()).update(productBusinessPartner);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      getMockMvc()
      .perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(productBusinessPartnerRequest)))
          .andExpect(status().isOk()).andReturn();
    } catch (ServletException e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE));
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).findById(productBusinessPartnerRequest.getId());
      Mockito.verify(getProductBusinessPartnerService(), NEVER_CALLED).update(productBusinessPartner);
    }
  }

  @Test
  public void createTest() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequest();
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    this.mockMvc.perform(
        post(uri).content(objectMapper.writeValueAsString(request))
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)).andExpect(
        status().isOk());
    Mockito.verify(this.productBusinessPartnerService).create(Mockito.anyString(),
        Mockito.any(ProductBusinessPartner.class), Mockito.eq(false), Mockito.anyList());
    Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
//        Mockito.any(ProductBusinessPartner.class), Mockito.eq(false), Mockito.anyList());
  }

  @Test
  public void createTest_logisticUpdateFailed() throws Exception {
    Mockito
        .when(this.productBusinessPartnerService.create(Mockito.anyString(),
            Mockito.any(ProductBusinessPartner.class), anyBoolean(),
            Mockito.anyList()))
        .thenReturn(CreateProductLevel3Response.builder().isLogisticsSaveSuccess(false).build());
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequest();
    URI uri = new URIBuilder()
        .setPath(ProductBusinessPartnerControllerPath.BASE_PATH
            + ProductBusinessPartnerControllerPath.CREATE_PBP)
        .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID).build();
    this.mockMvc
        .perform(post(uri).content(objectMapper.writeValueAsString(request))
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
    Mockito.verify(this.productBusinessPartnerService).create(Mockito.anyString(),
        Mockito.any(ProductBusinessPartner.class), Mockito.eq(false), Mockito.anyList());
    Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void createTestWithWholesalePrice() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequestWithWholesalePrice();
    ReflectionTestUtils.setField(productBusinessPartnerController, "maxWholesalePriceRequests", 5);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    this.mockMvc.perform(
        post(uri).content(objectMapper.writeValueAsString(request))
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)).andExpect(
        status().isOk());
    Mockito.verify(this.productBusinessPartnerService).create(Mockito.anyString(),
        Mockito.any(ProductBusinessPartner.class), Mockito.eq(false), Mockito.anyList());
    Mockito.verify(this.wholesaleValidationUtil)
        .validateWholesaleConfigOnFlow2(eq(CATEGORY_CODE), Mockito.anyList());
    Mockito.verify(this.productBusinessPartnerService).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void createTestWithWholesalePriceWithLessThan2Quantity() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequestWithWholesalePrice();
    request.getProductItemBusinessPartners().get(0).getProductItemWholesalePriceRequests().get(0)
        .setQuantity(1);
    ReflectionTestUtils.setField(productBusinessPartnerController, "maxWholesalePriceRequests", 5);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    try {
      this.mockMvc.perform(
          post(uri).content(objectMapper.writeValueAsString(request)).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
    } catch (Exception e) {
      Assertions.assertEquals(e.getCause().getMessage(),
          ProductControllerErrorMessage.MIN_QUANTITY_ALLOWED_FOR_WHOLESALE_PRICE_IS_2);
    } finally {
      Mockito.verify(this.productBusinessPartnerService).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void createTestWithWholesalePriceWithMoreThan5Requests() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequestWithWholesalePrice();
    request.getProductItemBusinessPartners().get(0).setProductItemWholesalePriceRequests(new ArrayList<>());
    request.getProductItemBusinessPartners().get(0).getProductItemWholesalePriceRequests().add(new ProductItemWholesalePriceRequest(1, 10));
    request.getProductItemBusinessPartners().get(0).getProductItemWholesalePriceRequests().add(new ProductItemWholesalePriceRequest(2, 12));
    request.getProductItemBusinessPartners().get(0).getProductItemWholesalePriceRequests().add(new ProductItemWholesalePriceRequest(3, 14));
    request.getProductItemBusinessPartners().get(0).getProductItemWholesalePriceRequests().add(new ProductItemWholesalePriceRequest(4, 15));
    request.getProductItemBusinessPartners().get(0).getProductItemWholesalePriceRequests().add(new ProductItemWholesalePriceRequest(5, 16));
    request.getProductItemBusinessPartners().get(0).getProductItemWholesalePriceRequests().add(new ProductItemWholesalePriceRequest(6, 17));
    ReflectionTestUtils.setField(productBusinessPartnerController, "maxWholesalePriceRequests", 5);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    try {
      this.mockMvc.perform(
          post(uri).content(objectMapper.writeValueAsString(request)).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
    } catch (Exception e) {
      Assertions.assertEquals(e.getCause().getMessage(), String.format(ErrorMessages.WHOLE_SETTING_GTE_5_CREATE, 5, 6));
    } finally {
      Mockito.verify(this.productBusinessPartnerService).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void createTestWithWholesalePriceWithDuplicateRequests() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequestWithWholesalePrice();
    request.getProductItemBusinessPartners().get(0).getProductItemWholesalePriceRequests().add(new ProductItemWholesalePriceRequest(1, 10));
    request.getProductItemBusinessPartners().get(0).getProductItemWholesalePriceRequests().add(new ProductItemWholesalePriceRequest(2, 12));
    request.getProductItemBusinessPartners().get(0).getProductItemWholesalePriceRequests().add(new ProductItemWholesalePriceRequest(3, 14));
    request.getProductItemBusinessPartners().get(0).getProductItemWholesalePriceRequests().add(new ProductItemWholesalePriceRequest(4, 15));
    request.getProductItemBusinessPartners().get(0).getProductItemWholesalePriceRequests().add(new ProductItemWholesalePriceRequest(5, 16));
    request.getProductItemBusinessPartners().get(0).getProductItemWholesalePriceRequests().add(new ProductItemWholesalePriceRequest(6, 17));
    ReflectionTestUtils.setField(productBusinessPartnerController, "maxWholesalePriceRequests", 5);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    try {
      this.mockMvc.perform(
          post(uri).content(objectMapper.writeValueAsString(request)).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
    } catch (Exception e) {
      Assertions.assertEquals(e.getCause().getMessage(), ErrorMessages.WHOLESALE_QUANTITY_IS_DUPLICATE);
    } finally {
      Mockito.verify(this.productBusinessPartnerService).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void createTestWithWholesalePriceLessThan1000Price() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequestWithWholesalePrice();
    ReflectionTestUtils.setField(productBusinessPartnerController, "maxWholesalePriceRequests", 5);
    request.getProductItemBusinessPartners().get(0).getProductItemWholesalePriceRequests().get(0)
        .setWholesaleDiscount(90.5);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    try {
      this.mockMvc.perform(
          post(uri).content(objectMapper.writeValueAsString(request)).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
    } catch (Exception e) {
      Assertions.assertEquals(e.getCause().getMessage(),
          String.format(ErrorMessages.WHOLESALE_PRICE_CANT_BE_LESS_THAN_MINIMUM_PRICE, 1));
    } finally {
      Mockito.verify(this.productBusinessPartnerService).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void createTestWithWholesalePriceTiersValidation() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequestWithWholesalePrice();
    ReflectionTestUtils.setField(productBusinessPartnerController, "maxWholesalePriceRequests", 5);
    request.getProductItemBusinessPartners().get(0).getProductItemWholesalePriceRequests()
        .add(new ProductItemWholesalePriceRequest(3, 5));
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    try {
      this.mockMvc.perform(
          post(uri).content(objectMapper.writeValueAsString(request)).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
    } catch (Exception e) {
      Assertions.assertEquals(e.getCause().getMessage(), ProductControllerErrorMessage.WHOLESALE_PRICE_TIER_RULE_VIOLATION);
    } finally {
      Mockito.verify(this.productBusinessPartnerService).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void createTestMandatoryFalse() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequest();
    request.getProductBusinessPartnerAttributes().get(0).setMandatory(false);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    this.mockMvc.perform(
        post(uri).content(objectMapper.writeValueAsString(request))
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)).andExpect(
        status().isOk());
    Mockito.verify(this.productBusinessPartnerService).create(Mockito.anyString(),
        Mockito.any(ProductBusinessPartner.class), Mockito.eq(false), Mockito.anyList());
    Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void createTestMandatoryFalseEmptyValue() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequest();
    request.getProductBusinessPartnerAttributes().get(0).setMandatory(false);
    request.getProductBusinessPartnerAttributes().get(0).setValue(StringUtils.EMPTY);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    this.mockMvc.perform(
        post(uri).content(objectMapper.writeValueAsString(request))
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)).andExpect(
        status().isOk());
    Mockito.verify(this.productBusinessPartnerService).create(Mockito.anyString(),
        productBusinessPartnerArgumentCaptor.capture(), Mockito.eq(false), Mockito.anyList());
    Assertions.assertEquals(Constants.DELIMITER_DASH,
        productBusinessPartnerArgumentCaptor.getValue().getProductBusinessPartnerAttributes().get(0).getValue());
    Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
  }

  @Test
  public void createTestException() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequest();
    request.getProductBusinessPartnerAttributes().get(0).setValue(StringUtils.EMPTY);
    URI uri = new URIBuilder()
        .setPath(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.CREATE_PBP)
        .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID).build();
    try {
      Assertions.assertThrows(Exception.class, () -> {
        this.mockMvc.perform(
            post(uri).content(objectMapper.writeValueAsString(request)).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
      });
    } finally {
      Mockito.verify(this.productBusinessPartnerService, ProductBusinessPartnerControllerTest.NEVER_CALLED)
          .create(Mockito.anyString(), Mockito.any(ProductBusinessPartner.class), Mockito.eq(false), eq(new ArrayList()));
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void createWithInvalidBusinessPartnerCodeTest() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequest();
    request.setBusinessPartnerId(null);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    try {
      this.mockMvc
          .perform(
              post(uri).content(objectMapper.writeValueAsString(request))
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.BUSINESS_PARTNER_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(this.productBusinessPartnerService,
          ProductBusinessPartnerControllerTest.NEVER_CALLED).create(Mockito.anyString(),
          Mockito.any(ProductBusinessPartner.class), Mockito.eq(false), eq(new ArrayList()));
    }
  }

  @Test
  public void createWithInvalidProductIdTest() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequest();
    request.setProductId(null);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    try {
      this.mockMvc
          .perform(
              post(uri).content(objectMapper.writeValueAsString(request))
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.PRODUCT_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(this.productBusinessPartnerService,
          ProductBusinessPartnerControllerTest.NEVER_CALLED).create(Mockito.anyString(),
          Mockito.any(ProductBusinessPartner.class), Mockito.eq(false), eq(new ArrayList()));
    }
  }

  @Test
  public void createWithInvalidProductNameTest() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequest();
    request.setProductName(null);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    try {
      this.mockMvc
          .perform(
              post(uri).content(objectMapper.writeValueAsString(request))
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.PRODUCT_NAME_MUST_NOT_BE_BLANK));
      Mockito.verify(this.productBusinessPartnerService,
          ProductBusinessPartnerControllerTest.NEVER_CALLED).create(Mockito.anyString(),
          Mockito.any(ProductBusinessPartner.class), Mockito.eq(false), eq(new ArrayList()));
    }
  }

  @Test
  public void createWithInvalidCategoryNameTest() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequest();
    request.setCategoryName(null);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    try {
      this.mockMvc
          .perform(
              post(uri).content(objectMapper.writeValueAsString(request))
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.CATEGORY_NAME_MUST_NOT_BE_BLANK));
      Mockito.verify(this.productBusinessPartnerService,
          ProductBusinessPartnerControllerTest.NEVER_CALLED).create(Mockito.anyString(),
          Mockito.any(ProductBusinessPartner.class), Mockito.eq(false), eq(new ArrayList()));
    }
  }

  @Test
  public void createWithInvalidBrandTest() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequest();
    request.setBrand(null);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    try {
      this.mockMvc
          .perform(
              post(uri).content(objectMapper.writeValueAsString(request))
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.BRAND_MUST_NOT_BE_BLANK));
      Mockito.verify(this.productBusinessPartnerService,
          ProductBusinessPartnerControllerTest.NEVER_CALLED).create(Mockito.anyString(),
          Mockito.any(ProductBusinessPartner.class), Mockito.eq(false), eq(new ArrayList()));
    }
  }

  @Test
  public void createWithEmptyProductItemBusinessPartnersTest() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequest();
    request.getProductItemBusinessPartners().clear();
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    try {
      this.mockMvc
          .perform(
              post(uri).content(objectMapper.writeValueAsString(request))
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.PRODUCT_ITEM_BUSINESS_PARTNERS_MUST_NOT_BE_BLANK));
      Mockito.verify(this.productBusinessPartnerService,
          ProductBusinessPartnerControllerTest.NEVER_CALLED).create(Mockito.anyString(),
          Mockito.any(ProductBusinessPartner.class), Mockito.eq(false), eq(new ArrayList()));
    }
  }

  @Test
  public void createWithInvalidProductItemIdTest() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequest();
    request.getProductItemBusinessPartners().get(0).setProductItemId(null);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    try {
      this.mockMvc
          .perform(
              post(uri).content(objectMapper.writeValueAsString(request))
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.PRODUCT_ITEM_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(this.productBusinessPartnerService,
          ProductBusinessPartnerControllerTest.NEVER_CALLED).create(Mockito.anyString(),
          Mockito.any(ProductBusinessPartner.class), Mockito.eq(false), eq(new ArrayList()));
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void createWithInvalidProductTypeTest() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequest();
    request.getProductItemBusinessPartners().get(0).setProductType(null);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    try {
      this.mockMvc
          .perform(
              post(uri).content(objectMapper.writeValueAsString(request))
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.PRODUCT_TYPE_MUST_NOT_BE_BLANK));
      Mockito.verify(this.productBusinessPartnerService,
          ProductBusinessPartnerControllerTest.NEVER_CALLED).create(Mockito.anyString(),
          Mockito.any(ProductBusinessPartner.class), Mockito.eq(false), eq(new ArrayList()));
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void createWithInvalidPriceTest() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequest();
    request.getProductItemBusinessPartners().get(0).setPrice(null);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    try {
      this.mockMvc
          .perform(
              post(uri).content(objectMapper.writeValueAsString(request))
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.PRICE_MUST_NOT_BE_BLANK));
      Mockito.verify(this.productBusinessPartnerService,
          ProductBusinessPartnerControllerTest.NEVER_CALLED).create(Mockito.anyString(),
          Mockito.any(ProductBusinessPartner.class), Mockito.eq(false), eq(new ArrayList()));
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void createWithInvalidSalePriceTest() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequest();
    request.getProductItemBusinessPartners().get(0).setSalePrice(null);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    try {
      this.mockMvc
          .perform(
              post(uri).content(objectMapper.writeValueAsString(request))
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.SALE_PRICE_MUST_NOT_BE_BLANK));
      Mockito.verify(this.productBusinessPartnerService,
          ProductBusinessPartnerControllerTest.NEVER_CALLED).create(Mockito.anyString(),
          Mockito.any(ProductBusinessPartner.class), Mockito.eq(false), eq(new ArrayList()));
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void createWithInvalidPriceMinimumValueTest() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequest();
    request.getProductItemBusinessPartners().get(0).setPrice(0.0);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    try {
      this.mockMvc
          .perform(
              post(uri).content(objectMapper.writeValueAsString(request))
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.MINIMUM_PRICE_VALUE_INVALID));
      Mockito.verify(this.productBusinessPartnerService,
          ProductBusinessPartnerControllerTest.NEVER_CALLED).create(Mockito.anyString(),
          Mockito.any(ProductBusinessPartner.class), Mockito.eq(false), eq(new ArrayList()));
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void createWithInvalidSalePriceMinimumValueTest() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequest();
    request.getProductItemBusinessPartners().get(0).setSalePrice(0.0);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    try {
      this.mockMvc
          .perform(
              post(uri).content(objectMapper.writeValueAsString(request))
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.MINIMUM_PRICE_VALUE_INVALID));
      Mockito.verify(this.productBusinessPartnerService,
          ProductBusinessPartnerControllerTest.NEVER_CALLED).create(Mockito.anyString(),
          Mockito.any(ProductBusinessPartner.class), Mockito.eq(false), eq(new ArrayList()));
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void createWithInvalidStockTest() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequest();
    request.getProductItemBusinessPartners().get(0).setStock(null);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    try {
      this.mockMvc
          .perform(
              post(uri).content(objectMapper.writeValueAsString(request))
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.STOCK_MUST_NOT_BE_BLANK));
      Mockito.verify(this.productBusinessPartnerService,
          ProductBusinessPartnerControllerTest.NEVER_CALLED).create(Mockito.anyString(),
          Mockito.any(ProductBusinessPartner.class), Mockito.eq(false), eq(new ArrayList()));
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void createWithInvalidMinimumStockTest() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequest();
    request.getProductItemBusinessPartners().get(0).setMinimumStock(null);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    try {
      this.mockMvc
          .perform(
              post(uri).content(objectMapper.writeValueAsString(request))
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.MINIMUM_STOCK_MUST_NOT_BE_BLANK));
      Mockito.verify(this.productBusinessPartnerService,
          ProductBusinessPartnerControllerTest.NEVER_CALLED).create(Mockito.anyString(),
          Mockito.any(ProductBusinessPartner.class), Mockito.eq(false), eq(new ArrayList()));
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void createWithInvalidPickupPointCodeTest() throws Exception {
    ProductBusinessPartnerRequest request = generateProductBusinessPartnerRequest();
    request.getProductItemBusinessPartners().get(0).setPickupPointId(null);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .build();
    try {
      this.mockMvc
          .perform(
              post(uri).content(objectMapper.writeValueAsString(request))
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductBusinessPartnerControllerErrorMessage.PICKUP_POINT_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(this.productBusinessPartnerService,
          ProductBusinessPartnerControllerTest.NEVER_CALLED).create(Mockito.anyString(),
          Mockito.any(ProductBusinessPartner.class), Mockito.eq(false), eq(new ArrayList()));
      Mockito.verify(getProductBusinessPartnerService()).getMinimumPrice(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void retryCreateTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.RETRY_CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("productBusinessPartnerId",
                ProductBusinessPartnerControllerTest.DEFAULT_PRODUCT_BUSINESS_PARTNER_ID).build();
    this.mockMvc.perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(
        status().isOk());
    Mockito.verify(this.productBusinessPartnerService).retryCreate(Mockito.anyString(),
        Mockito.anyString(), eq(null));
  }

  @Test
  public void retryCreateWithInvalidProductBusinessPartnerIdTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductBusinessPartnerControllerPath.BASE_PATH
                    + ProductBusinessPartnerControllerPath.RETRY_CREATE_PBP)
            .addParameter("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("productBusinessPartnerId", "").build();
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(
          status().isOk());
    } catch (Exception e) {
            Assertions.assertTrue(e
              .getMessage()
              .contains(
                  ProductBusinessPartnerControllerErrorMessage.PRODUCT_BUSINESS_PARTNER_ID_MUST_NOT_BE_BLANK));
      Mockito.verify(this.productBusinessPartnerService,
          ProductBusinessPartnerControllerTest.NEVER_CALLED).retryCreate(Mockito.anyString(),
          Mockito.anyString(), eq(null));
    }
  }

  @Test public void testFilterRejectedSkuProduct() throws Exception {
    List<RejectedSkuProductResponse> rejectedSkuProductResponses =
        new ArrayList<RejectedSkuProductResponse>();
    rejectedSkuProductResponses.add(
        new RejectedSkuProductResponse("product-1", "category-1", "brand-1",
            Calendar.getInstance().getTime(), DEFAULT_USERNAME, "Reason-1",
            Calendar.getInstance().getTime(), DEFAULT_PRODUCT_ID));
    rejectedSkuProductResponses.add(
        new RejectedSkuProductResponse("product-2", "category-2", "brand-2",
            Calendar.getInstance().getTime(), DEFAULT_USERNAME, "Reason-2",
            Calendar.getInstance().getTime(), DEFAULT_PRODUCT_ID));
    rejectedSkuProductResponses.add(
        new RejectedSkuProductResponse("product-3", "category-3", "brand-3",
            Calendar.getInstance().getTime(), DEFAULT_USERNAME, "Reason-3",
            Calendar.getInstance().getTime(), DEFAULT_PRODUCT_ID));

    List<RejectedSkuProductCollection> rejectedSkuProductCollections =
        new ArrayList<RejectedSkuProductCollection>();
    for (RejectedSkuProductResponse response : rejectedSkuProductResponses) {
      RejectedSkuProductCollection rejectedSkuProductCollection =
          new RejectedSkuProductCollection();
      BeanUtils.copyProperties(response, rejectedSkuProductCollection);
      rejectedSkuProductCollections.add(rejectedSkuProductCollection);
    }

    Page<RejectedSkuProductCollection> page =
        new PageImpl<RejectedSkuProductCollection>(rejectedSkuProductCollections);
    Pageable pageable = PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE);
    GdnRestListResponse<RejectedSkuProductResponse> response =
        new GdnRestListResponse<RejectedSkuProductResponse>(null, null, true,
            rejectedSkuProductResponses,
            new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
                page.getTotalElements()), getRequestId());
    when(getProductBusinessPartnerService()
        .findRejectedProductsByBusinessPartnerIdWithOrderByAndSortBy(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            pageable, null, ORDER_BY,SORT_BY)).thenReturn(page);
    URI uri = new URIBuilder().setPath(ProductBusinessPartnerControllerPath.BASE_PATH
        + ProductBusinessPartnerControllerPath.FILTER_REJECTED_SKU)
        .addParameter("businessPartnerId", DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("searchCriteria", null)
        .addParameter("orderBy", ORDER_BY)
        .addParameter("sortBy", SORT_BY)
        .addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
        .build();
    MvcResult result = getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());
    Mockito.verify(getProductBusinessPartnerService(), AT_LEAST_ONE)
        .findRejectedProductsByBusinessPartnerIdWithOrderByAndSortBy(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            pageable, null, ORDER_BY, SORT_BY);
  }

  @Test
  public void filterRejectedSkuByBusinessPartnerIdAndMerchantSkuTest() throws Exception {
    Pageable pageable = PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE);
    Mockito.when(this.productBusinessPartnerService.findRejectedProductsByBusinessPartnerIdAndMerchantSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_BUSINESS_PARTNER_ID, pageable, MERCHANT_SKU))
        .thenReturn(new PageImpl<RejectedSkuProductCollection>(new ArrayList<>()));
    this.mockMvc.perform(get(ProductBusinessPartnerControllerPath.BASE_PATH
            + ProductBusinessPartnerControllerPath.FILTER_REJECTED_SKU_BY_MERCHANT_SKU)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
            .param("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
            .param("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
            .param("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
            .param("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
            .param("page", ProductBusinessPartnerControllerTest.DEFAULT_PAGE.toString())
            .param("size", ProductBusinessPartnerControllerTest.DEFAULT_SIZE.toString())
            .param("businessPartnerId", ProductBusinessPartnerControllerTest.DEFAULT_PRODUCT_BUSINESS_PARTNER_ID)
            .param("merchantSku", ProductBusinessPartnerControllerTest.MERCHANT_SKU)
        ).andExpect(status().isOk());
    Mockito.verify(this.productBusinessPartnerService)
        .findRejectedProductsByBusinessPartnerIdAndMerchantSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_BUSINESS_PARTNER_ID,
            pageable, MERCHANT_SKU);
  }

  @Test
  public void filterRejectedSkuByBusinessPartnerIdAndMerchantSku_expectException() throws Exception {
    Pageable pageable = PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE);
    Mockito.when(this.productBusinessPartnerService.findRejectedProductsByBusinessPartnerIdAndMerchantSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_BUSINESS_PARTNER_ID, pageable, MERCHANT_SKU))
        .thenThrow(new ApplicationRuntimeException());
    try {
      this.mockMvc.perform(get(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.FILTER_REJECTED_SKU_BY_MERCHANT_SKU)
          .contentType(MediaType.APPLICATION_JSON).param("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
          .param("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
          .param("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
          .param("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
          .param("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
          .param("page", ProductBusinessPartnerControllerTest.DEFAULT_PAGE.toString())
          .param("size", ProductBusinessPartnerControllerTest.DEFAULT_SIZE.toString())
          .param("businessPartnerId", ProductBusinessPartnerControllerTest.DEFAULT_PRODUCT_BUSINESS_PARTNER_ID)
          .param("merchantSku", ProductBusinessPartnerControllerTest.MERCHANT_SKU)).andExpect(status().isOk());
    } catch (ApplicationRuntimeException e) {
    } finally {
      Mockito.verify(this.productBusinessPartnerService)
          .findRejectedProductsByBusinessPartnerIdAndMerchantSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_BUSINESS_PARTNER_ID,
              pageable, MERCHANT_SKU);
    }
  }

  @Test
  public void copyPartnerProductsTest() throws Exception {
    Mockito.doNothing()
      .when(this.productBusinessPartnerService)
      .copy(eq(DEFAULT_STORE_ID), eq(DEFAULT_USERNAME), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(DEFAULT_PICKUP_POINT_CODE),
        eq(false), Mockito.anyString(), eq(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(ITEM_SKU))), eq(SOURCE_PARTNER_CODE));

    ProductCopyRequest request = new ProductCopyRequest();
    request.setSourceBusinessPartnerCode(SOURCE_PARTNER_CODE);
    request.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    request.setPickupPointCode(DEFAULT_PICKUP_POINT_CODE);
    request.setGdnItemSkus(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(ITEM_SKU)));

    this.mockMvc
      .perform(post(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.COPY_PRODUCTS)
        .param("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
        .param("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
        .param("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
        .contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(request))
      ).andExpect(status().isOk());

    Mockito.verify(this.productBusinessPartnerService)
      .copy(eq(DEFAULT_STORE_ID), eq(DEFAULT_USERNAME), eq(DEFAULT_BUSINESS_PARTNER_CODE),
        eq(DEFAULT_PICKUP_POINT_CODE), eq(false), Mockito.anyString(), eq(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(ITEM_SKU))), eq(SOURCE_PARTNER_CODE));
  }

  @Test
  public void copyPartnerProductsTest_withRetryFlag() throws Exception {
    Mockito.doNothing()
      .when(this.productBusinessPartnerService)
      .copy(eq(DEFAULT_STORE_ID), eq(DEFAULT_USERNAME), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(DEFAULT_PICKUP_POINT_CODE),
        eq(true), Mockito.anyString(), eq(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(ITEM_SKU))), eq(SOURCE_PARTNER_CODE));

    ProductCopyRequest request = new ProductCopyRequest();
    request.setSourceBusinessPartnerCode(SOURCE_PARTNER_CODE);
    request.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    request.setPickupPointCode(DEFAULT_PICKUP_POINT_CODE);
    request.setGdnItemSkus(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(ITEM_SKU)));

    this.mockMvc
      .perform(post(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.COPY_PRODUCTS)
        .param("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
        .param("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
        .param("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
        .param("isRetryAttempt", "true")
        .contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(request))
      ).andExpect(status().isOk());

    Mockito.verify(this.productBusinessPartnerService)
      .copy(eq(DEFAULT_STORE_ID), eq(DEFAULT_USERNAME), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(DEFAULT_PICKUP_POINT_CODE)
        , eq(true), Mockito.anyString(), eq(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(ITEM_SKU))), eq(SOURCE_PARTNER_CODE));
  }

  @Test
  public void copyPartnerProductsTest_whenItemSKUDoesNotBelongToLinkedPartner() throws Exception {
    ProductCopyRequest request = new ProductCopyRequest();
    request.setSourceBusinessPartnerCode(SOURCE_PARTNER_CODE);
    request.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    request.setPickupPointCode(DEFAULT_PICKUP_POINT_CODE);
    request.setGdnItemSkus(Collections.singletonMap(GDN_PRODUCT_SKU, Arrays.asList("STS-34534-345-345")));

    try {
      this.mockMvc
        .perform(post(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.COPY_PRODUCTS)
          .param("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
          .param("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
          .param("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
          .param("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
          .param("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
          .contentType(MediaType.APPLICATION_JSON)
          .content(objectMapper.writeValueAsString(request))
        ).andExpect(status().isOk());
    } catch(Exception e) {
      Assertions.assertTrue(e.getMessage().contains("invalid item sku found"));
    }
  }

  @Test
  public void copyPartnerProductsTest_invalidPartnerCode() throws Exception {
    ProductCopyRequest request = new ProductCopyRequest();
    request.setSourceBusinessPartnerCode(SOURCE_PARTNER_CODE);
    request.setPickupPointCode(DEFAULT_PICKUP_POINT_CODE);
    request.setGdnItemSkus(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(ITEM_SKU)));

    try {
      this.mockMvc
        .perform(post(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.COPY_PRODUCTS)
          .param("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
          .param("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
          .param("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
          .param("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
          .param("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
          .contentType(MediaType.APPLICATION_JSON)
          .content(objectMapper.writeValueAsString(request))
        ).andExpect(status().isOk());
    } catch(Exception e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.BUSINESS_PARTNER_ID_MUST_NOT_BE_BLANK));
    }
  }

  @Test
  public void copyPartnerProductsTest_invalidSourcePartnerCode() throws Exception {
    ProductCopyRequest request = new ProductCopyRequest();
    request.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    request.setPickupPointCode(DEFAULT_PICKUP_POINT_CODE);
    request.setGdnItemSkus(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(ITEM_SKU)));

    try {
      this.mockMvc
        .perform(post(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.COPY_PRODUCTS)
          .param("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
          .param("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
          .param("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
          .param("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
          .param("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
          .contentType(MediaType.APPLICATION_JSON)
          .content(objectMapper.writeValueAsString(request))
        ).andExpect(status().isOk());
    } catch(Exception e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.MISSING_SOURCE_BUSINESS_PARTNER_ID));
    }
  }

  @Test
  public void copyPartnerProductsTest_invalidPickupPointCode() throws Exception {
    ProductCopyRequest request = new ProductCopyRequest();
    request.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    request.setSourceBusinessPartnerCode(SOURCE_PARTNER_CODE);
    request.setGdnItemSkus(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(ITEM_SKU)));

    try {
      this.mockMvc
        .perform(post(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.COPY_PRODUCTS)
          .param("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
          .param("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
          .param("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
          .param("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
          .param("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
          .contentType(MediaType.APPLICATION_JSON)
          .content(objectMapper.writeValueAsString(request))
        ).andExpect(status().isOk());
    } catch(Exception e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PICKUP_POINT_ID_MUST_NOT_BE_BLANK));
    }
  }

  @Test
  public void copyPartnerProductsTest_invalidItemSKUs() throws Exception {
    ProductCopyRequest request = new ProductCopyRequest();
    request.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    request.setSourceBusinessPartnerCode(SOURCE_PARTNER_CODE);
    request.setPickupPointCode(DEFAULT_PICKUP_POINT_CODE);

    try {
      this.mockMvc
        .perform(post(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.COPY_PRODUCTS)
          .param("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
          .param("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
          .param("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
          .param("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
          .param("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
          .contentType(MediaType.APPLICATION_JSON)
          .content(objectMapper.writeValueAsString(request))
        ).andExpect(status().isOk());
    } catch(Exception e) {
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.MISSING_PRODUCT_ITEMS));
    }
  }

  @Test
  public void copyPartnerProductsTest_WhenItemsNotEligibleForCopy() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, "NO_VALID_PRODUCTS_TO_COPY"))
      .when(this.productBusinessPartnerService)
      .copy(eq(DEFAULT_STORE_ID), eq(DEFAULT_USERNAME), eq(DEFAULT_BUSINESS_PARTNER_CODE),
        eq(DEFAULT_PICKUP_POINT_CODE),
        eq(true), Mockito.anyString(), eq(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(ITEM_SKU))), eq(SOURCE_PARTNER_CODE));

    ProductCopyRequest request = new ProductCopyRequest();
    request.setSourceBusinessPartnerCode(SOURCE_PARTNER_CODE);
    request.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    request.setPickupPointCode(DEFAULT_PICKUP_POINT_CODE);
    request.setGdnItemSkus(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(ITEM_SKU)));

    this.mockMvc
      .perform(post(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.COPY_PRODUCTS)
        .param("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
        .param("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
        .param("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
        .param("isRetryAttempt", "true")
        .contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(request))
      )
      .andExpect(status().isOk())
      .andExpect(jsonPath("$.errorCode", Matchers.equalTo("NO_VALID_PRODUCTS_TO_COPY")));

    Mockito.verify(this.productBusinessPartnerService)
      .copy(eq(DEFAULT_STORE_ID), eq(DEFAULT_USERNAME), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(DEFAULT_PICKUP_POINT_CODE)
        , eq(true), Mockito.anyString(), eq(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(ITEM_SKU))), eq(SOURCE_PARTNER_CODE));
  }

  @Test
  public void copyPartnerProductsTest_WhenCopyFails() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException(ErrorCategory.INVALID_STATE))
        .when(this.productBusinessPartnerService)
        .copy(eq(DEFAULT_STORE_ID), eq(DEFAULT_USERNAME), eq(DEFAULT_BUSINESS_PARTNER_CODE),
            eq(DEFAULT_PICKUP_POINT_CODE), eq(true), Mockito.anyString(), eq(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(ITEM_SKU))),
            eq(SOURCE_PARTNER_CODE));

    ProductCopyRequest request = new ProductCopyRequest();
    request.setSourceBusinessPartnerCode(SOURCE_PARTNER_CODE);
    request.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    request.setPickupPointCode(DEFAULT_PICKUP_POINT_CODE);
    request.setGdnItemSkus(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(ITEM_SKU)));

    try {
      this.mockMvc.perform(
          post(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.COPY_PRODUCTS)
              .param("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
              .param("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
              .param("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
              .param("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
              .param("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME).param("isRetryAttempt", "true")
              .contentType(MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(request)))
          .andExpect(status().is5xxServerError());
    } catch (Exception e) {
      Mockito.verify(this.productBusinessPartnerService)
          .copy(eq(DEFAULT_STORE_ID), eq(DEFAULT_USERNAME), eq(DEFAULT_BUSINESS_PARTNER_CODE),
              eq(DEFAULT_PICKUP_POINT_CODE), eq(true), Mockito.anyString(), eq(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(ITEM_SKU))),
              eq(SOURCE_PARTNER_CODE));
    }
  }

  @Test
  public void copyPartnerAllProductsTest() throws Exception {
    Mockito.doNothing()
      .when(this.productBusinessPartnerService)
      .copyAllProducts(DEFAULT_STORE_ID, DEFAULT_USERNAME, SOURCE_PARTNER_CODE, DEFAULT_BUSINESS_PARTNER_CODE,
        DEFAULT_PICKUP_POINT_CODE);

    ProductCopyRequest request = new ProductCopyRequest();
    request.setSourceBusinessPartnerCode(SOURCE_PARTNER_CODE);
    request.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    request.setPickupPointCode(DEFAULT_PICKUP_POINT_CODE);
    request.setGdnItemSkus(Collections.emptyMap());
    this.mockMvc
      .perform(post(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.COPY_ALL_PRODUCTS)
        .param("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
        .param("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
        .param("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
        .contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(request))
      ).andExpect(status().isOk());

    Mockito
      .verify(this.productBusinessPartnerService)
      .copyAllProducts(DEFAULT_STORE_ID, DEFAULT_USERNAME, SOURCE_PARTNER_CODE, DEFAULT_BUSINESS_PARTNER_CODE,
        DEFAULT_PICKUP_POINT_CODE);
  }

  @Test
  public void copyPartnerAllProductsTest_whenNoItemsAvailableToCopy() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, "NO_VALID_PRODUCTS_TO_COPY"))
      .when(this.productBusinessPartnerService)
      .copyAllProducts(DEFAULT_STORE_ID, DEFAULT_USERNAME, SOURCE_PARTNER_CODE, DEFAULT_BUSINESS_PARTNER_CODE,
        DEFAULT_PICKUP_POINT_CODE);

    ProductCopyRequest request = new ProductCopyRequest();
    request.setSourceBusinessPartnerCode(SOURCE_PARTNER_CODE);
    request.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    request.setPickupPointCode(DEFAULT_PICKUP_POINT_CODE);
    request.setGdnItemSkus(Collections.emptyMap());
    this.mockMvc
      .perform(post(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.COPY_ALL_PRODUCTS)
        .param("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
        .param("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
        .param("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
        .contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(request))
      ).andExpect(status().isOk())
      .andExpect(jsonPath("$.errorCode", Matchers.equalTo("NO_VALID_PRODUCTS_TO_COPY")));

    Mockito
      .verify(this.productBusinessPartnerService)
      .copyAllProducts(DEFAULT_STORE_ID, DEFAULT_USERNAME, SOURCE_PARTNER_CODE, DEFAULT_BUSINESS_PARTNER_CODE,
        DEFAULT_PICKUP_POINT_CODE);
  }

  @Test
  public void copyPartnerAllProductsTest_invalidPartnerCode() throws Exception {
    ProductCopyRequest request = new ProductCopyRequest();
    request.setSourceBusinessPartnerCode(SOURCE_PARTNER_CODE);
    request.setPickupPointCode(DEFAULT_PICKUP_POINT_CODE);
    request.setGdnItemSkus(Collections.emptyMap());

    try {
      this.mockMvc
        .perform(post(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.COPY_ALL_PRODUCTS)
          .param("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
          .param("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
          .param("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
          .param("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
          .param("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
          .contentType(MediaType.APPLICATION_JSON)
          .content(objectMapper.writeValueAsString(request))
        ).andExpect(status().isOk());
    } catch(Exception e){
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.BUSINESS_PARTNER_ID_MUST_NOT_BE_BLANK));
    }
  }

  @Test
  public void copyPartnerAllProductsTest_invalidSourcePartnerCode() throws Exception {
    ProductCopyRequest request = new ProductCopyRequest();
    request.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    request.setPickupPointCode(DEFAULT_PICKUP_POINT_CODE);
    request.setGdnItemSkus(Collections.emptyMap());

    try {
      this.mockMvc
        .perform(post(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.COPY_ALL_PRODUCTS)
          .param("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
          .param("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
          .param("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
          .param("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
          .param("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
          .contentType(MediaType.APPLICATION_JSON)
          .content(objectMapper.writeValueAsString(request))
        ).andExpect(status().isOk());
    } catch(Exception e){
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.MISSING_SOURCE_BUSINESS_PARTNER_ID));
    }
  }

  @Test
  public void copyPartnerAllProductsTest_invalidPickupPointCode() throws Exception {
    ProductCopyRequest request = new ProductCopyRequest();
    request.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    request.setSourceBusinessPartnerCode(DEFAULT_PICKUP_POINT_CODE);
    request.setGdnItemSkus(Collections.emptyMap());

    try {
      this.mockMvc
        .perform(post(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.COPY_ALL_PRODUCTS)
          .param("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
          .param("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
          .param("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
          .param("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
          .param("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
          .contentType(MediaType.APPLICATION_JSON)
          .content(objectMapper.writeValueAsString(request))
        ).andExpect(status().isOk());
    } catch(Exception e){
      Assertions.assertTrue(e.getMessage().contains(ProductBusinessPartnerControllerErrorMessage.PICKUP_POINT_ID_MUST_NOT_BE_BLANK));
    }
  }

  @Test
  public void NotificationForCopyPartnerProducts() throws Exception {
    Mockito.doNothing().when(this.productBusinessPartnerService).notifyForProductCopyingProcess(DEFAULT_STORE_ID);

    this.mockMvc.perform(post(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.COPY_PRODUCTS_NOTIFY,
        DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME)
      .param("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
      .param("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
      .param("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
      .param("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
      .param("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
      .contentType(MediaType.APPLICATION_JSON)
    ).andExpect(status().isOk());

    Mockito.verify(this.productBusinessPartnerService).notifyForProductCopyingProcess(DEFAULT_STORE_ID);

  }

  @Test
  public void resetProductItemSyncStatusForRetryTest() throws Exception {
    Mockito.doNothing().when(this.productBusinessPartnerService).resetProductItemSyncStatus(DEFAULT_STORE_ID);

    this.mockMvc.perform(post(ProductBusinessPartnerControllerPath.BASE_PATH + ProductBusinessPartnerControllerPath.UPDATE_SYNC_RETRY,
      DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME)
      .param("storeId", ProductBusinessPartnerControllerTest.DEFAULT_STORE_ID)
      .param("channelId", ProductBusinessPartnerControllerTest.DEFAULT_CHANNEL_ID)
      .param("clientId", ProductBusinessPartnerControllerTest.DEFAULT_CLIENT_ID)
      .param("requestId", ProductBusinessPartnerControllerTest.DEFAULT_REQUEST_ID)
      .param("username", ProductBusinessPartnerControllerTest.DEFAULT_USERNAME)
      .contentType(MediaType.APPLICATION_JSON)
    ).andExpect(status().isOk());

    Mockito.verify(this.productBusinessPartnerService).resetProductItemSyncStatus(DEFAULT_STORE_ID);

  }

  @Test
  public void isProductsMappedToBusinessPartnerCodeTest() throws Exception {
    when(productBusinessPartnerService.isProductMappedToMerchant(eq(DEFAULT_STORE_ID), eq(DEFAULT_BUSINESS_PARTNER_CODE)))
        .thenReturn(true);

    mockMvc.perform(MockMvcRequestBuilders
        .get(ProductBusinessPartnerControllerPath.BASE_PATH
            + ProductBusinessPartnerControllerPath.IS_PRODUCT_MAPPED_TO_MERCHANT, DEFAULT_BUSINESS_PARTNER_CODE)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", DEFAULT_STORE_ID).param("channelId", DEFAULT_CHANNEL_ID)
        .param("clientId", DEFAULT_CHANNEL_ID).param("requestId", DEFAULT_REQUEST_ID)
        .param("username", DEFAULT_USERNAME)).andExpect(status().isOk());

    verify(productBusinessPartnerService)
        .isProductMappedToMerchant(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void retryL3CreateJobTest() throws Exception {
    mockMvc.perform(MockMvcRequestBuilders
      .get(ProductBusinessPartnerControllerPath.BASE_PATH
        + ProductBusinessPartnerControllerPath.RETRY_L3_CREATE_JOB)
      .contentType(MediaType.APPLICATION_JSON)
      .param("storeId", DEFAULT_STORE_ID).param("channelId", DEFAULT_CHANNEL_ID)
      .param("clientId", DEFAULT_CLIENT_ID).param("requestId", DEFAULT_REQUEST_ID)
      .param("username", DEFAULT_USERNAME)).andExpect(status().isOk());
    verify(productLevel3Wrapper).retryL3CreationJob(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_CLIENT_ID,
      DEFAULT_CHANNEL_ID);
  }

  @Test
  public void overrideL3RetryEntryTest() throws Exception {
    ProductL3RetryListRequest productL3RetryListRequest = new ProductL3RetryListRequest();
    mockMvc.perform(MockMvcRequestBuilders
      .post(ProductBusinessPartnerControllerPath.BASE_PATH
        + ProductBusinessPartnerControllerPath.OVERRIDE_L3_RETRY_ENTRY)
      .contentType(MediaType.APPLICATION_JSON)
      .content(objectMapper.writeValueAsString(productL3RetryListRequest))
      .param("storeId", DEFAULT_STORE_ID).param("channelId", DEFAULT_CHANNEL_ID)
      .param("clientId", DEFAULT_CLIENT_ID).param("requestId", DEFAULT_REQUEST_ID)
      .param("username", DEFAULT_USERNAME)).andExpect(status().isOk());
    verify(productLevel3Wrapper).overrideL3Retry(eq(DEFAULT_STORE_ID), eq(DEFAULT_REQUEST_ID),
      eq(DEFAULT_USERNAME), Mockito.any(ProductL3RetryListRequest.class));
  }
}
