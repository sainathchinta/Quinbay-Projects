package com.gdn.mta.product.controller;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.BrandUpdateRequest;
import com.gda.mta.product.dto.CogsDataResponse;
import com.gda.mta.product.dto.CogsUpdateRequests;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.FbbCreatePickupPointRequest;
import com.gda.mta.product.dto.FbbCreatePickupPointResponse;
import com.gda.mta.product.dto.ItemL5ListingRequest;
import com.gda.mta.product.dto.ItemPriceStockQuickUpdateResponse;
import com.gda.mta.product.dto.ItemSkuPpCodeRequest;
import com.gda.mta.product.dto.NeedRevisionEligibilityRequest;
import com.gda.mta.product.dto.NeedRevisionEligibilityResponse;
import com.gda.mta.product.dto.PreOrderRequest;
import com.gda.mta.product.dto.ProductEditValidationDTO;
import com.gda.mta.product.dto.ProductL3ListingRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3AttributeRequest;
import com.gda.mta.product.dto.ProductLevel3QuickEditV2Request;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductLevel3UpdateRequest;
import com.gda.mta.product.dto.ProductMasterDataEditRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.QuickEditV2Request;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.response.ProductL3ListingResponse;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailsV2Response;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.commons.constant.UpdateProductAccessChannel;
import com.gdn.mta.product.entity.PreOrderDTO;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Attribute;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.service.ProductLevel3V2ServiceImpl;
import com.gdn.mta.product.service.ProductLevel3V2Wrapper;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.mta.product.service.exception.ApiDataNotFoundException;
import com.gdn.mta.product.web.model.ProductLevel3V2ControllerPath;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.businesspartner.dto.ProfileResponse;

import org.apache.http.client.utils.URIBuilder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.MediaType;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.http.converter.ResourceHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.net.URI;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Set;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class ProductLevel3V2ControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "web";
  private static final String DEFAULT_USERNAME = "username";
  private static final String DEFAULT_CLIENT_ID = "mta";
  private static final String DEFAULT_REQUEST_ID = "requestID";
  private static final String PRODUCT_SKU = "productSku";
  private static final String DEFAULT_PRODUCT_SKU = "BLI-00001";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLI-00001";
  private static final ObjectMapper DEFAULT_OBJECT_MAPPER = new ObjectMapper(new JsonFactory());
  private static final String DEFAULT_PRODUCT_NAME = "CODE-1000";
  private static final String DEFAULT_BRAND = "CODE-1000";
  private static final String DEFAULT_DESCRIPTION = "CODE-1000";
  private static final String DEFAULT_SPECIFICATION = "CODE-1000";
  private static final Double DEFAULT_PRICE = 1000000.0;
  private static final Double DEFAULT_SALE_PRICE = 900000.0;
  private static final Double DEFAULT_DISCOUNT = 10.0;
  private static final String DEFAULT_PROMOTION_NAME = "PROMO";
  private static final String DEFAULT_LOCATION_PATH = "Location";
  private static final Boolean DEFAULT_SKU_VALUE = true;
  private static final String YOUTUBE_INVALID_URL = "https://www.youtube.com/watch?v";
  private static final String REVIEW_TYPE = "pre-live";
  private static final String DEFAULT_UNIQUE_SELLING = "CODE-1000";
  private static final String DEFAULT_SPECIFICATION_DETAIL = "CODE-1000";
  private static final String DEFAULT_PRODUCT_STORY = "CODE-1000";
  private static final String YOUTUBE_URL = "https://www.youtube.com/watch?v=P1xAhgKTqDA";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String ITEM_SKU = "itemSku";
  private static final String PREORDER_DAYS_TYPE = "DAYS";
  private static final String PREORDER_WEEK_TYPE = "WEEK";
  private static final Integer PREORDER_VALUE = 10;

  private static final String PREORDER_DATE_TYPE = "DATE";
  private static final SimpleDateFormat PREORDER_DATE_FORMAT = new SimpleDateFormat("dd/MM/yyyy");
  private static final String UNSPECIFIED = "UNSPECIFIED";

  private MockMvc mockMvc;
  private static final ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
  private ProductLevel3 productLevel3;
  private ProductLevel3 productLevel3Request = new ProductLevel3();
  private List<ProductLevel3Attribute> attributesList;
  private ItemsPriceStockImagesUpdateResponse imagesUpdateResponse =
    new ItemsPriceStockImagesUpdateResponse();
  private ItemsPriceStockImagesUpdateResponse imagesUpdateResponse2 =
    new ItemsPriceStockImagesUpdateResponse();
  private EditProductResponse editProductResponse = new EditProductResponse();
  private ProductVariantUpdateRequest productVariantUpdateRequest =
    new ProductVariantUpdateRequest();
  private PreOrderRequest preOrderRequest;

  @Mock
  private ProductLevel3V2ServiceImpl productLevel3V2Service;

  @Mock
  private ProductLevel3V2Wrapper productLevel3V2Wrapper;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @InjectMocks
  private ProductLevel3V2Controller productLevel3V2Controller;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = MockMvcBuilders
      .standaloneSetup(productLevel3V2Controller)
      .setMessageConverters(new ByteArrayHttpMessageConverter(),
        new StringHttpMessageConverter(), new ResourceHttpMessageConverter(),
        new FormHttpMessageConverter(), new MappingJackson2HttpMessageConverter()).build();

    ProductLevel3Attribute attribute =
      new ProductLevel3Attribute(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BRAND,
        new ArrayList<String>(), DEFAULT_SKU_VALUE);

    attributesList = new ArrayList<>();
    attributesList.add(attribute);

    productLevel3 = new ProductLevel3();
    productLevel3.setDescription(DEFAULT_DESCRIPTION);
    productLevel3.setUniqueSellingPoint(DEFAULT_UNIQUE_SELLING);
    productLevel3.setSpecificationDetail(DEFAULT_SPECIFICATION_DETAIL);
    productLevel3.setProductName(DEFAULT_PRODUCT_NAME);
    productLevel3.setProductStory(DEFAULT_PRODUCT_STORY);
    productLevel3.setUrl(YOUTUBE_URL);
    productLevel3.setAttributes(attributesList);
    productLevel3.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);

    productLevel3Request.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);

    preOrderRequest =
      PreOrderRequest.builder().isPreOrder(true).preOrderType(PREORDER_DAYS_TYPE).preOrderValue(PREORDER_VALUE)
        .preOrderDate(new Date()).build();

    ReflectionTestUtils.setField(productLevel3V2Controller, "preOrderMaximumDays", 90);
    ReflectionTestUtils.setField(productLevel3V2Controller, "preOrderMaximumWeek", 13);
    ReflectionTestUtils.setField(productLevel3V2Controller, "combineL3AndL5Validation", true);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(), Mockito.any()))
        .thenReturn(productVariantUpdateRequest);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(productLevel3V2Service);
    verifyNoMoreInteractions(productLevel3V2Wrapper);
  }

  @Test
  public void itemListingUpdateV2Test() throws Exception {
    ProductLevel3QuickEditV2Request productLevel3QuickEditV2Request =
      new ProductLevel3QuickEditV2Request();
    Mockito.when(productLevel3V2Service.productQuickEditV2(DEFAULT_STORE_ID, PRODUCT_SKU, productLevel3QuickEditV2Request, false))
      .thenReturn(null);
    URI uri = new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH
        + ProductLevel3V2ControllerPath.ITEM_LISTING_UPDATE_V2.replaceAll("\\{productSku\\}",
        PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
      .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
      .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isExternalOnly","false")
      .build();
    mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(objectMapper.writeValueAsString(productLevel3QuickEditV2Request)))
      .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(
        jsonPath("$.success", equalTo(true)));
    Mockito.verify(productLevel3V2Service).productQuickEditV2(DEFAULT_STORE_ID, PRODUCT_SKU,
      productLevel3QuickEditV2Request, false);
  }

  @Test
  public void itemListingUpdateV2_exceptionTest() throws Exception {
    ProductLevel3QuickEditV2Request productLevel3QuickEditV2Request =
      new ProductLevel3QuickEditV2Request();
    Mockito.when(productLevel3V2Service.productQuickEditV2(DEFAULT_STORE_ID, PRODUCT_SKU,
      productLevel3QuickEditV2Request, false)).thenThrow(Exception.class);
    URI uri = new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH
        + ProductLevel3V2ControllerPath.ITEM_LISTING_UPDATE_V2.replaceAll("\\{productSku\\}",
        PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
      .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
      .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isExternalOnly","false")
      .build();
    mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(objectMapper.writeValueAsString(productLevel3QuickEditV2Request)))
      .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(
        jsonPath("$.success", equalTo(false)));
    Mockito.verify(productLevel3V2Service).productQuickEditV2(DEFAULT_STORE_ID, PRODUCT_SKU,
      productLevel3QuickEditV2Request, false);
  }

  @Test
  public void itemListingUpdateV2_withApiErrorCodeTest() throws Exception {
    ProductLevel3QuickEditV2Request productLevel3QuickEditV2Request =
      new ProductLevel3QuickEditV2Request();
    Mockito.when(productLevel3V2Service.productQuickEditV2(DEFAULT_STORE_ID, PRODUCT_SKU, productLevel3QuickEditV2Request, false))
      .thenReturn(ApiErrorCode.PRICE_UPDATE_FAILED);
    URI uri = new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH
        + ProductLevel3V2ControllerPath.ITEM_LISTING_UPDATE_V2.replaceAll("\\{productSku\\}",
        PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
      .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
      .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isExternalOnly","false")
      .build();
    mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(objectMapper.writeValueAsString(productLevel3QuickEditV2Request)))
      .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(
        jsonPath("$.errorMessage", equalTo(ApiErrorCode.PRICE_UPDATE_FAILED.getDesc())));
    Mockito.verify(productLevel3V2Service).productQuickEditV2(DEFAULT_STORE_ID, PRODUCT_SKU,
      productLevel3QuickEditV2Request, false);
  }

  @Test
  public void getL3ListingV2_Test() throws Exception {
    Page<ProductL3ListingResponse> productL3ListingResponses = new PageImpl<>(Arrays.asList(new ProductL3ListingResponse()));
    ProductL3ListingRequest productL3ListingRequest = new ProductL3ListingRequest();
    Mockito.when(productLevel3V2Service.getProductL3List(DEFAULT_STORE_ID, productL3ListingRequest, 0, 10))
        .thenReturn(productL3ListingResponses);
    URI uri = new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH
        + ProductLevel3V2ControllerPath.GET_L3_LISTING).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
        .build();
    mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(productL3ListingRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(
        jsonPath("$.success", equalTo(true)));
    Mockito.verify(productLevel3V2Service).getProductL3List(DEFAULT_STORE_ID, productL3ListingRequest, 0, 10);
  }

  @Test
  public void getL3ListingV2_exceptionTest() throws Exception {
    ProductL3ListingRequest productL3ListingRequest = new ProductL3ListingRequest();
    Mockito.when(productLevel3V2Service.getProductL3List(DEFAULT_STORE_ID, productL3ListingRequest, 0, 10))
        .thenThrow(Exception.class);
    URI uri = new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH
        + ProductLevel3V2ControllerPath.GET_L3_LISTING).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
        .build();
    mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(productL3ListingRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(
        jsonPath("$.success", equalTo(false)));
    Mockito.verify(productLevel3V2Service).getProductL3List(DEFAULT_STORE_ID, productL3ListingRequest, 0, 10);
  }

  @Test
  public void updateEditedProductInfoTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3V2Service, "maxProductDimensionLimit",
      100000);
    editProductResponse.setProductReview(true);
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setAction(1);
    imagesUpdateResponse.setApiErrorCode(null);
    ProductL3UpdateRequest product = generateProductLevel3Request();
    preOrderRequest.setIsPreOrder(true);
    product.setPreOrder(preOrderRequest);
    productLevel3.setPreOrder(new PreOrderDTO());
    productLevel3.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    product.setUrl(YOUTUBE_INVALID_URL);
    Mockito.when(productLevel3V2Service.editPriceStockVariantsInfo(Mockito.eq(DEFAULT_STORE_ID), Mockito.any(),
        Mockito.eq(productVariantUpdateRequest), Mockito.eq(editProductResponse))).thenReturn(imagesUpdateResponse);
    Mockito.when(productLevel3V2Service.generateProductLevel3(product)).thenReturn(productLevel3);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(), Mockito.any()))
        .thenReturn(productVariantUpdateRequest);
    Mockito.when(productLevel3V2Wrapper.editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID),
        Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true), Mockito.eq(false), Mockito.eq(false))).thenReturn(
        ProductEditValidationDTO.builder().editProductResponse(editProductResponse).productL3UpdateRequest(product)
            .productL3UpdateRequest(product).L5ValidationFailed(true).needCorrection(true).build());
    Mockito.when(productLevel3V2Service.updateLogistics(Mockito.any(),
        Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(null);
    URI uri =
      new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_EDIT_INFO
        .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
      MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
      .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Wrapper)
        .editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true),
            Mockito.eq(false), Mockito.eq(false));
  }

  @Test
  public void updateEditedProductInfo_Override_webp_image_path_Test() throws Exception {
    ReflectionTestUtils.setField(productLevel3V2Service, "maxProductDimensionLimit", 100000);
    ReflectionTestUtils.setField(productLevel3V2Controller, "webpConversionEnabled", true);
    editProductResponse.setProductReview(true);
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setAction(1);
    imagesUpdateResponse.setApiErrorCode(null);
    ProductL3UpdateRequest product = generateProductLevel3Request();
    ProductLevel3SummaryDetailsImageRequest commonImageRequest =
        new ProductLevel3SummaryDetailsImageRequest();
    commonImageRequest.setReviewType(Constants.NEW);
    commonImageRequest.setLocationPath("path/to/image.jpg");
    ProductLevel3SummaryDetailsImageRequest itemImageRequest =
        new ProductLevel3SummaryDetailsImageRequest();
    itemImageRequest.setReviewType(Constants.NEW);
    itemImageRequest.setLocationPath("path/to/item_image.jpg");
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setImages(List.of(itemImageRequest));
    product.setProductItems(List.of(productVariantPriceStockAndImagesRequest));
    product.setCommonImages(List.of(commonImageRequest));
    preOrderRequest.setIsPreOrder(true);
    product.setPreOrder(preOrderRequest);
    productLevel3.setPreOrder(new PreOrderDTO());
    productLevel3.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    product.setUrl(YOUTUBE_INVALID_URL);
    Mockito.when(productLevel3V2Service.editPriceStockVariantsInfo(Mockito.eq(DEFAULT_STORE_ID), Mockito.any(),
        Mockito.eq(productVariantUpdateRequest), Mockito.eq(editProductResponse))).thenReturn(imagesUpdateResponse);
    Mockito.when(productLevel3V2Service.generateProductLevel3(product)).thenReturn(productLevel3);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(), Mockito.any()))
        .thenReturn(productVariantUpdateRequest);
    Mockito.when(productLevel3V2Wrapper.editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID),
        Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true), Mockito.eq(false), Mockito.eq(false))).thenReturn(
        ProductEditValidationDTO.builder().editProductResponse(editProductResponse).productL3UpdateRequest(product)
            .productL3UpdateRequest(product).L5ValidationFailed(true).needCorrection(true).build());
    Mockito.when(productLevel3V2Service.updateLogistics(Mockito.any(),
        Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(null);
    URI uri =
        new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_EDIT_INFO
                .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Wrapper)
        .editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true),
            Mockito.eq(false), Mockito.eq(false));
  }

  @Test
  public void updateEditedProductInfo_Override_webp_image_path_NR_Flow_Test() throws Exception {
    ReflectionTestUtils.setField(productLevel3V2Service, "maxProductDimensionLimit", 100000);
    ReflectionTestUtils.setField(productLevel3V2Controller, "webpConversionEnabled", true);
    editProductResponse.setProductReview(true);
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setAction(1);
    imagesUpdateResponse.setApiErrorCode(null);
    ProductL3UpdateRequest product = generateProductLevel3Request();
    ProductLevel3SummaryDetailsImageRequest commonImageRequest = new ProductLevel3SummaryDetailsImageRequest();
    commonImageRequest.setReviewType(Constants.NEW);
    commonImageRequest.setLocationPath("path/to/image.jpg");
    ProductLevel3SummaryDetailsImageRequest itemImageRequest = new ProductLevel3SummaryDetailsImageRequest();
    itemImageRequest.setReviewType(Constants.NEW);
    itemImageRequest.setLocationPath("path/to/item_image.jpg");
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setImages(List.of(itemImageRequest));
    product.setProductItems(List.of(productVariantPriceStockAndImagesRequest));
    product.setCommonImages(List.of(commonImageRequest));
    preOrderRequest.setIsPreOrder(true);
    product.setPreOrder(preOrderRequest);
    productLevel3.setPreOrder(new PreOrderDTO());
    productLevel3.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    product.setUrl(YOUTUBE_INVALID_URL);
    product.setNeedCorrection(true);
    Mockito.when(productLevel3V2Service.editPriceStockVariantsInfo(Mockito.eq(DEFAULT_STORE_ID), Mockito.any(),
        Mockito.eq(productVariantUpdateRequest), Mockito.eq(editProductResponse))).thenReturn(imagesUpdateResponse);
    Mockito.when(productLevel3V2Service.generateProductLevel3(product)).thenReturn(productLevel3);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(), Mockito.any()))
        .thenReturn(productVariantUpdateRequest);
    Mockito.when(productLevel3V2Wrapper.editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID),
        Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true), Mockito.eq(false), Mockito.eq(false))).thenReturn(
        ProductEditValidationDTO.builder().editProductResponse(editProductResponse).productL3UpdateRequest(product)
            .productL3UpdateRequest(product).L5ValidationFailed(true).needCorrection(true).build());
    Mockito.when(productLevel3V2Service.updateLogistics(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(),
        Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(null);
    URI uri = new URIBuilder().setPath(
            ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_EDIT_INFO.replaceAll(
                "\\{productSku\\}", DEFAULT_PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Wrapper)
        .editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true),
            Mockito.eq(false), Mockito.eq(false));
  }

  @Test
  public void updateEditedProductInfoAndWrongPreOrderTypeTest() throws Exception {
    editProductResponse.setProductReview(true);
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setAction(1);
    imagesUpdateResponse.setApiErrorCode(null);
    ProductL3UpdateRequest product = generateProductLevel3Request();
    preOrderRequest.setIsPreOrder(true);
    preOrderRequest.setPreOrderValue(-1);
    preOrderRequest.setPreOrderType("WRONG");
    product.setPreOrder(preOrderRequest);
    productLevel3.setPreOrder(new PreOrderDTO());
    productLevel3.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    product.setUrl(YOUTUBE_INVALID_URL);
    Mockito.when(productLevel3V2Service
        .editPriceStockVariantsInfo(DEFAULT_STORE_ID, productLevel3Request, productVariantUpdateRequest,
            new EditProductResponse())).thenReturn(imagesUpdateResponse);
    Mockito.when(productLevel3V2Service.generateProductLevel3(product)).thenReturn(productLevel3);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(), Mockito.any()))
        .thenReturn(productVariantUpdateRequest);
    editProductResponse.setApiErrorCode(ApiErrorCode.INVALID_PREORDER_TYPE);
    Mockito.when(
        productLevel3V2Wrapper.editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(
                ProductL3UpdateRequest.class), Mockito.eq(true), Mockito.eq(false),
            Mockito.eq(false))).thenReturn(ProductEditValidationDTO.builder().editProductResponse(editProductResponse).productL3UpdateRequest(product).L5ValidationFailed(true).needCorrection(true).build());
    Mockito.when(productLevel3V2Service.updateLogistics(Mockito.any(),
        Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(null);
    URI uri =
        new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_EDIT_INFO
                .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Wrapper)
        .editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true), Mockito.eq(false),
            Mockito.eq(false));
  }

  @Test
  public void updateEditedProductInfoWithPreOrderSuccessTest() throws Exception {
    editProductResponse.setProductReview(true);
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setAction(1);
    imagesUpdateResponse.setApiErrorCode(null);
    ProductL3UpdateRequest product = generateProductLevel3Request();
    preOrderRequest.setPreOrderType(PREORDER_DATE_TYPE);
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, 5);
    preOrderRequest.setPreOrderDate(cal.getTime());
    product.setPreOrder(preOrderRequest);
    productLevel3.setPreOrder(new PreOrderDTO());
    productLevel3.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    product.setUrl(YOUTUBE_INVALID_URL);
    Mockito.when(productLevel3V2Service.editPriceStockVariantsInfo(Mockito.eq(DEFAULT_STORE_ID), Mockito.any(),
        Mockito.eq(productVariantUpdateRequest), Mockito.eq(editProductResponse))).thenReturn(imagesUpdateResponse);
    Mockito.when(productLevel3V2Service.generateProductLevel3(product)).thenReturn(productLevel3);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(), Mockito.any()))
      .thenReturn(productVariantUpdateRequest);
    Mockito.when(
        productLevel3V2Wrapper.editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(
                ProductL3UpdateRequest.class),Mockito.eq(true), Mockito.eq(false),
            Mockito.eq(false))).thenReturn(ProductEditValidationDTO.builder().editProductResponse(editProductResponse).productL3UpdateRequest(product).L5ValidationFailed(true).needCorrection(true).productL3UpdateRequest(product).build());
    Mockito.when(productLevel3V2Service.updateLogistics(Mockito.any(),
      Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(null);
    URI uri =
      new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_EDIT_INFO
          .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
      .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Wrapper)
        .editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true), Mockito.eq(false),
            Mockito.eq(false));
  }

  @Test
  public void updateEditedProductInfoWithPreOrderAndPreOrderTypeDateTypeDateWithExact90DaysTest() throws Exception {
    editProductResponse.setProductReview(true);
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setAction(1);
    imagesUpdateResponse.setApiErrorCode(null);
    ProductL3UpdateRequest product = generateProductLevel3Request();
    preOrderRequest.setPreOrderType(PREORDER_DATE_TYPE);
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, 90);
    preOrderRequest.setPreOrderDate(cal.getTime());
    product.setPreOrder(preOrderRequest);
    productLevel3.setPreOrder(new PreOrderDTO());
    productLevel3.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    product.setUrl(YOUTUBE_INVALID_URL);
    Mockito.when(productLevel3V2Service
      .editPriceStockVariantsInfo(DEFAULT_STORE_ID, productLevel3Request, productVariantUpdateRequest,
        new EditProductResponse())).thenReturn(imagesUpdateResponse);
    Mockito.when(productLevel3V2Service.generateProductLevel3(product)).thenReturn(productLevel3);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(), Mockito.any()))
      .thenReturn(productVariantUpdateRequest);
    Mockito.when(
        productLevel3V2Wrapper.editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(
                ProductL3UpdateRequest.class), Mockito.eq(true), Mockito.eq(false),
            Mockito.eq(false))).thenReturn(ProductEditValidationDTO.builder().editProductResponse(editProductResponse).productL3UpdateRequest(product).productL3UpdateRequest(product).L5ValidationFailed(true).needCorrection(true).build());
    Mockito.when(productLevel3V2Service.updateLogistics(Mockito.any(),
      Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(null);
    URI uri =
      new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_EDIT_INFO
          .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
      .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Wrapper)
        .editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true), Mockito.eq(false),
            Mockito.eq(false));
  }

  @Test
  public void updateEditedProductInfoWithPreOrderAndPreOrderTypeDateTypeDateWithPreOrderDaysAfterValueTest() throws Exception {
    editProductResponse.setProductReview(true);
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setAction(1);
    imagesUpdateResponse.setApiErrorCode(null);
    ProductL3UpdateRequest product = generateProductLevel3Request();
    preOrderRequest.setPreOrderType(PREORDER_DATE_TYPE);
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, 92);
    preOrderRequest.setPreOrderDate(cal.getTime());
    product.setPreOrder(preOrderRequest);
    productLevel3.setPreOrder(new PreOrderDTO());
    productLevel3.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    product.setUrl(YOUTUBE_INVALID_URL);
    Mockito.when(productLevel3V2Service
        .editPriceStockVariantsInfo(DEFAULT_STORE_ID, productLevel3Request, productVariantUpdateRequest,
            new EditProductResponse())).thenReturn(imagesUpdateResponse);
    Mockito.when(productLevel3V2Service.generateProductLevel3(product)).thenReturn(productLevel3);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(), Mockito.any()))
        .thenReturn(productVariantUpdateRequest);
    Mockito.when(
        productLevel3V2Wrapper.editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(
                ProductL3UpdateRequest.class), Mockito.eq(true), Mockito.eq(false),
            Mockito.eq(false))).thenReturn(ProductEditValidationDTO.builder().editProductResponse(editProductResponse).productL3UpdateRequest(product).L5ValidationFailed(true).needCorrection(true).productL3UpdateRequest(product).build());
    Mockito.when(productLevel3V2Service.updateLogistics(Mockito.any(),
        Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(null);
    URI uri =
        new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_EDIT_INFO
                .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Wrapper)
        .editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductL3UpdateRequest.class),
             Mockito.eq(true), Mockito.eq(false),
            Mockito.eq(false));
  }

  @Test
  public void updateEditedProductInfoResultantKywordsActionAutoNeedRevisionTest() throws Exception {
    editProductResponse = new EditProductResponse();
    editProductResponse.setProfileResponse(new ProfileResponse());
    editProductResponse.setProductReview(true);
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setAction(2);
    imagesUpdateResponse.setApiErrorCode(null);
    ProductL3UpdateRequest product = generateProductLevel3Request();
    preOrderRequest.setIsPreOrder(true);
    product.setPreOrder(preOrderRequest);
    productLevel3.setPreOrder(new PreOrderDTO());
    productLevel3.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    product.setUrl(YOUTUBE_INVALID_URL);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(), Mockito.any()))
        .thenReturn(productVariantUpdateRequest);
    Mockito.when(productLevel3V2Service.editPriceStockVariantsInfo(Mockito.eq(DEFAULT_STORE_ID),
            Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(imagesUpdateResponse);
    Mockito.when(productLevel3V2Service.generateProductLevel3(product)).thenReturn(productLevel3);
    Mockito.doNothing().when(productServiceWrapper)
        .performResultantActionBasedOnRestrictedKeywords(Mockito.any(), Mockito.any(), Mockito.anyInt(),
            Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyList(),
            Mockito.anyBoolean(), Mockito.any(), isNull(ProfileResponse.class), isNull(Set.class), Mockito.any(EditProductResponse.class));
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(product, new EditProductResponse())).thenReturn(productVariantUpdateRequest);
    Mockito.when(productLevel3V2Wrapper.editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID),
        Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true), Mockito.eq(false), Mockito.eq(false))).thenReturn(
        ProductEditValidationDTO.builder().editProductResponse(editProductResponse).productL3UpdateRequest(product).L5ValidationFailed(true).needCorrection(true).productL3UpdateRequest(product)
            .build()); Mockito.when(productLevel3V2Service.updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class),
        Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(null);
    URI uri =
        new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_EDIT_INFO
                .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Wrapper)
        .editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true),
            Mockito.eq(false), Mockito.eq(false));
  }

  @Test
  public void updateEditedProductInfoResultantKywordsActionAutoRejectTest() throws Exception {
    editProductResponse = new EditProductResponse();
    editProductResponse.setProductReview(true);
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setAction(3);
    editProductResponse.setProfileResponse(new ProfileResponse());
    imagesUpdateResponse.setApiErrorCode(null);
    ProductL3UpdateRequest product = generateProductLevel3Request();
    preOrderRequest.setIsPreOrder(true);
    product.setPreOrder(preOrderRequest);
    productLevel3.setPreOrder(new PreOrderDTO());
    productLevel3.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    product.setUrl(YOUTUBE_INVALID_URL);
    Mockito.when(productLevel3V2Service.editPriceStockVariantsInfo(Mockito.eq(DEFAULT_STORE_ID), Mockito.any(),
        Mockito.eq(productVariantUpdateRequest), Mockito.eq(editProductResponse))).thenReturn(imagesUpdateResponse);
    Mockito.when(productLevel3V2Service.generateProductLevel3(product)).thenReturn(productLevel3);
    Mockito.doNothing().when(productServiceWrapper)
        .performResultantActionBasedOnRestrictedKeywords(Mockito.any(), Mockito.any(), Mockito.anyInt(),
            Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyList(),
            Mockito.anyBoolean(), Mockito.any(), isNull(ProfileResponse.class), isNull(Set.class), Mockito.any(EditProductResponse.class));
    Mockito.when(productLevel3V2Wrapper.editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID),
        Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true), Mockito.eq(false), Mockito.eq(false))).thenReturn(
        ProductEditValidationDTO.builder().editProductResponse(editProductResponse).productL3UpdateRequest(product).L5ValidationFailed(true).needCorrection(true).productL3UpdateRequest(product)
            .build());Mockito.when(productLevel3V2Service.updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class),
        Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(null);
    URI uri =
        new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_EDIT_INFO
                .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Wrapper)
        .editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true),
            Mockito.eq(false), Mockito.eq(false));
  }


  @Test
  public void updateEditedProductInfo_ContextExceptionTest() throws Exception {
    editProductResponse.setApiErrorCode(ApiErrorCode.MANDATORY_FIELD_NOT_FOUND);
    imagesUpdateResponse.setApiErrorCode(null);
    ProductL3UpdateRequest product = generateProductLevel3Request();
    product.setUrl(YOUTUBE_INVALID_URL);
    Mockito.when(productLevel3V2Service.generateProductLevel3(product)).thenReturn(productLevel3);
    Mockito.when(productLevel3V2Wrapper.editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID),
        Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true), Mockito.eq(false), Mockito.eq(false))).thenReturn(
        ProductEditValidationDTO.builder().editProductResponse(editProductResponse).productL3UpdateRequest(product).L5ValidationFailed(true).needCorrection(true).productL3UpdateRequest(product)
            .build());
    URI uri =
      new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_EDIT_INFO
        .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
      MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
      .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Wrapper)
        .editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true),
            Mockito.eq(false), Mockito.eq(false));
  }

  @Test
  public void updateEditedProductInfo_ExceptionTest() throws Exception {
    ProductL3UpdateRequest product = generateProductLevel3Request();
    product.setUrl(YOUTUBE_INVALID_URL);
    preOrderRequest.setIsPreOrder(false);
    product.setPreOrder(preOrderRequest);
    Mockito.when(productLevel3V2Service.generateProductLevel3(product)).thenReturn(productLevel3);
    Mockito.when(productLevel3V2Wrapper.editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID),
        Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true), Mockito.eq(false), Mockito.eq(false))).thenReturn(
        ProductEditValidationDTO.builder().editProductResponse(editProductResponse).productL3UpdateRequest(product).L5ValidationFailed(true).needCorrection(true).productL3UpdateRequest(product)
            .build());Mockito.when(productLevel3V2Service.updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class),
      Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenThrow(Exception.class);
    URI uri =
      new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_EDIT_INFO
        .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
      MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
      .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Wrapper)
        .editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductL3UpdateRequest.class),
            Mockito.eq(true),
            Mockito.eq(false), Mockito.eq(false));
  }

  @Test
  public void updateEditedProductInfo_LogisticExceptionTest() throws Exception {
    editProductResponse.setApiErrorCode(null);
    imagesUpdateResponse.setApiErrorCode(null);
    ProductL3UpdateRequest product = generateProductLevel3Request();
    product.setUrl(YOUTUBE_INVALID_URL);
    Mockito.when(productLevel3V2Service.editPriceStockVariantsInfo(Mockito.eq(DEFAULT_STORE_ID), Mockito.any(),
        Mockito.eq(productVariantUpdateRequest), Mockito.eq(editProductResponse))).thenReturn(imagesUpdateResponse);
    Mockito.when(productLevel3V2Service.generateProductLevel3(product)).thenReturn(productLevel3);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(product, editProductResponse))
        .thenReturn(productVariantUpdateRequest);
    Mockito.when(productLevel3V2Wrapper.editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID),
        Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true), Mockito.eq(false), Mockito.eq(false))).thenReturn(
        ProductEditValidationDTO.builder().editProductResponse(editProductResponse).productL3UpdateRequest(product).L5ValidationFailed(true).needCorrection(true).productL3UpdateRequest(product)
            .build());
    Mockito.when(productLevel3V2Service.updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class),
      Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(),Mockito.anyBoolean())).thenReturn(ApiErrorCode.LOGISTICS_DATA_NOT_SAVED);
    URI uri =
      new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_EDIT_INFO
        .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
      MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
      .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Wrapper)
        .editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductL3UpdateRequest.class),
            Mockito.eq(true),
            Mockito.eq(false), Mockito.eq(false));
  }

  @Test
  public void updateEditedProductInfo_TakeDownTest() throws Exception {
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setToTakeDown(true);
    imagesUpdateResponse.setApiErrorCode(null);
    productLevel3.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    ProductL3UpdateRequest product = generateProductLevel3Request();
    product.setUrl(YOUTUBE_INVALID_URL);
    Mockito.when(productLevel3V2Service
        .editPriceStockVariantsInfo(DEFAULT_STORE_ID, productLevel3, productVariantUpdateRequest,
            editProductResponse)).thenReturn(imagesUpdateResponse);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(product, editProductResponse))
        .thenReturn(productVariantUpdateRequest);
    Mockito.when(productLevel3V2Service.generateProductLevel3(product)).thenReturn(productLevel3);
    Mockito.when(productLevel3V2Wrapper.editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID),
        Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true), Mockito.eq(false), Mockito.eq(false))).thenReturn(
        ProductEditValidationDTO.builder().editProductResponse(editProductResponse).productL3UpdateRequest(product).L5ValidationFailed(false).needCorrection(false).productL3UpdateRequest(product)
            .build());Mockito.when(productLevel3V2Service.updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class),
      Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(null);
    Mockito.doNothing().when(productLevel3V2Service)
      .takeDownProduct(Mockito.any(), Mockito.any(), Mockito.any());
    URI uri =
      new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_EDIT_INFO
        .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
      MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
      .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Wrapper)
        .editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductL3UpdateRequest.class),
            Mockito.eq(true),
            Mockito.eq(false), Mockito.eq(false));
  }

  @Test
  public void updateEditedProductInfoExceptionTest() throws Exception {
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setToTakeDown(true);
    imagesUpdateResponse.setApiErrorCode(null);
    productLevel3.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    ProductL3UpdateRequest product = generateProductLevel3Request();
    product.setUrl(YOUTUBE_INVALID_URL);
    Mockito.when(
        productLevel3V2Service.editPriceStockVariantsInfo(DEFAULT_STORE_ID, productLevel3, productVariantUpdateRequest,
            editProductResponse)).thenReturn(imagesUpdateResponse);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(product, editProductResponse))
        .thenReturn(productVariantUpdateRequest);
    Mockito.when(productLevel3V2Service.generateProductLevel3(product)).thenReturn(productLevel3);
    Mockito.when(productLevel3V2Wrapper.editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID),
            Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true), Mockito.eq(false), Mockito.eq(false)))
        .thenThrow(RuntimeException.class);
    Mockito.when(
        productLevel3V2Service.updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.anyBoolean(),
            Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(null);
    Mockito.doNothing().when(productLevel3V2Service).takeDownProduct(Mockito.any(), Mockito.any(), Mockito.any());
    URI uri = new URIBuilder().setPath(
            ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_EDIT_INFO.replaceAll(
                "\\{productSku\\}", DEFAULT_PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Wrapper)
        .editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true),
            Mockito.eq(false), Mockito.eq(false));
  }

  @Test
  public void updateEditedProductInfoApiExceptionTest() throws Exception {
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setToTakeDown(true);
    imagesUpdateResponse.setApiErrorCode(null);
    productLevel3.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    ProductL3UpdateRequest product = generateProductLevel3Request();
    product.setUrl(YOUTUBE_INVALID_URL);
    Mockito.when(productLevel3V2Service
        .editPriceStockVariantsInfo(DEFAULT_STORE_ID, productLevel3, productVariantUpdateRequest,
            editProductResponse)).thenReturn(imagesUpdateResponse);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(product, editProductResponse))
        .thenReturn(productVariantUpdateRequest);
    Mockito.when(productLevel3V2Service.generateProductLevel3(product)).thenReturn(productLevel3);
    Mockito.when(productLevel3V2Wrapper.editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID),
        Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true), Mockito.eq(false), Mockito.eq(false))).thenThrow(
        ApiDataNotFoundException.class);
    Mockito.when(productLevel3V2Service.updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class),
        Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(null);
    Mockito.doNothing().when(productLevel3V2Service)
        .takeDownProduct(Mockito.any(), Mockito.any(), Mockito.any());
    URI uri =
        new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_EDIT_INFO
                .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Wrapper)
        .editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductL3UpdateRequest.class),
            Mockito.eq(true),
            Mockito.eq(false), Mockito.eq(false));
  }

  @Test
  public void updateEditedProductInfoAlreadyTakenDown() throws Exception {
    ReflectionTestUtils.setField(productLevel3V2Controller, "combineL3AndL5Validation", false);
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setToTakeDown(true);
    editProductResponse.setProductReview(true);
    imagesUpdateResponse.setApiErrorCode(null);
    imagesUpdateResponse.setProductReview(false);
    productLevel3.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    ProductL3UpdateRequest product = generateProductLevel3Request();
    product.setUrl(YOUTUBE_INVALID_URL);
    imagesUpdateResponse.setTakeDown(true);
    Mockito.when(productLevel3V2Service
        .editPriceStockVariantsInfo(DEFAULT_STORE_ID, productLevel3, productVariantUpdateRequest, editProductResponse))
        .thenReturn(imagesUpdateResponse);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(product, editProductResponse))
        .thenReturn(productVariantUpdateRequest);
    Mockito.when(productLevel3V2Service.generateProductLevel3(product)).thenReturn(productLevel3);
    Mockito.when(productLevel3V2Wrapper.editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID),
        Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true), Mockito.eq(false), Mockito.eq(false))).thenReturn(
        ProductEditValidationDTO.builder().editProductResponse(editProductResponse).productL3UpdateRequest(product).L5ValidationFailed(true).needCorrection(false).productL3UpdateRequest(product)
            .build());Mockito.when(productLevel3V2Service.updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class),
        Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(null);
    Mockito.doNothing().when(productLevel3V2Service)
        .takeDownProduct(Mockito.any(), Mockito.any(), Mockito.any());
    URI uri =
        new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Wrapper)
        .editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductL3UpdateRequest.class),
            Mockito.eq(true),
            Mockito.eq(false), Mockito.eq(false));
  }

  @Test
  public void updateEditedProductInfoImageTakeDownFalse() throws Exception {
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setToTakeDown(false);
    editProductResponse.setProductReview(true);
    imagesUpdateResponse.setApiErrorCode(null);
    imagesUpdateResponse.setProductReview(false);
    imagesUpdateResponse.setTakeDown(false);
    productLevel3.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    ProductL3UpdateRequest product = generateProductLevel3Request();
    product.setUrl(YOUTUBE_INVALID_URL);
    Mockito.when(productLevel3V2Service
        .editPriceStockVariantsInfo(DEFAULT_STORE_ID, productLevel3, productVariantUpdateRequest, editProductResponse))
        .thenReturn(imagesUpdateResponse);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(product, editProductResponse))
        .thenReturn(productVariantUpdateRequest);
    Mockito.when(productLevel3V2Service.generateProductLevel3(product)).thenReturn(productLevel3);
    Mockito.when(productLevel3V2Wrapper.editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID),
        Mockito.any(ProductL3UpdateRequest.class),Mockito.eq(true), Mockito.eq(false), Mockito.eq(false))).thenReturn(
        ProductEditValidationDTO.builder().editProductResponse(editProductResponse).productL3UpdateRequest(product).L5ValidationFailed(false).needCorrection(true).productL3UpdateRequest(product)
            .build());Mockito.when(productLevel3V2Service.updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class),
        Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(null);
    Mockito.doNothing().when(productLevel3V2Service)
        .takeDownProduct(Mockito.any(), Mockito.any(), Mockito.any());
    URI uri =
        new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Wrapper)
        .editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductL3UpdateRequest.class),
            Mockito.eq(true),
            Mockito.eq(false), Mockito.eq(false));
  }

  @Test
  public void updateEditedProductInfoImageTakeDown() throws Exception {
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setToTakeDown(false);
    editProductResponse.setProductReview(true);
    imagesUpdateResponse.setApiErrorCode(null);
    imagesUpdateResponse.setProductReview(false);
    imagesUpdateResponse.setTakeDown(true);
    imagesUpdateResponse.setReviewType(DEFAULT_DESCRIPTION);
    productLevel3.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    ProductL3UpdateRequest product = generateProductLevel3Request();
    product.setUrl(YOUTUBE_INVALID_URL);
    Mockito.when(productLevel3V2Service
        .editPriceStockVariantsInfo(DEFAULT_STORE_ID, productLevel3, productVariantUpdateRequest, editProductResponse))
        .thenReturn(imagesUpdateResponse);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(product, editProductResponse))
        .thenReturn(productVariantUpdateRequest);
    Mockito.when(productLevel3V2Service.generateProductLevel3(product)).thenReturn(productLevel3);
    Mockito.when(productLevel3V2Wrapper.editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID),
        Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true), Mockito.eq(false), Mockito.eq(false))).thenReturn(
        ProductEditValidationDTO.builder().editProductResponse(editProductResponse).productL3UpdateRequest(product).L5ValidationFailed(true).needCorrection(true).productL3UpdateRequest(product)
            .build());Mockito.when(productLevel3V2Service.updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class),
        Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(),Mockito.anyBoolean() )).thenReturn(null);
    Mockito.doNothing().when(productLevel3V2Service)
        .takeDownProduct(Mockito.any(), Mockito.any(), Mockito.any());
    URI uri =
        new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_EDIT_INFO
            .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Wrapper)
        .editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true),
            Mockito.eq(false), Mockito.eq(false));

  }

  @Test
  public void fetchProductL3DetailsByProductSkuTest() throws Exception {
    Mockito.when(productLevel3V2Service
      .fetchL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, true, true))
      .thenReturn(new ProductLevel3DetailsV2Response());

    URI uri = new URIBuilder().setPath(
      ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.FETCH_L3_DETAILS
        .replaceAll("\\{productSku}", DEFAULT_PRODUCT_SKU))
      .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
      .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
      .addParameter("clientId", DEFAULT_CLIENT_ID)
      .addParameter("isNeedCorrection", Boolean.FALSE.toString()).build();
    mockMvc.perform(MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
      .contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Service)
      .fetchL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, true, true);
  }

  @Test
  public void fetchProductL3DetailsByProductSkuExceptionTest() throws Exception {
    Mockito.when(productLevel3V2Service
      .fetchL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, true, true))
      .thenThrow(Exception.class);
    URI uri = new URIBuilder().setPath(
      ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.FETCH_L3_DETAILS
        .replaceAll("\\{productSku}", DEFAULT_PRODUCT_SKU))
      .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
      .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
      .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(MockMvcRequestBuilders.get(uri))
      .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Service)
      .fetchL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, true, true);
  }

  @Test
  public void fetchProductL3DetailsByProductSkuApplicationRuntimeExceptionTest() throws Exception {
    Mockito.when(productLevel3V2Service
        .fetchL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, true, true))
        .thenThrow(new ApplicationRuntimeException());
    URI uri = new URIBuilder().setPath(
        ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.FETCH_L3_DETAILS
            .replaceAll("\\{productSku}", DEFAULT_PRODUCT_SKU))
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(MockMvcRequestBuilders.get(uri))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Service)
        .fetchL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, true, true);
  }

  @Test
  public void updateEditedProductInfoL5ErrorTest() throws Exception {
    imagesUpdateResponse2.setProductReview(true);
    editProductResponse.setApiErrorCode(null);
    imagesUpdateResponse.setApiErrorCode(null);
    imagesUpdateResponse2.setApiErrorCode(ApiErrorCode.PRICE_UPDATE_FAILED);
    ProductL3UpdateRequest product = generateProductLevel3Request();
    productLevel3.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    EditProductResponse editProductResponse1 = new EditProductResponse();
    editProductResponse1.setProductLevel3LogisticsRequest(product.getProductLevel3LogisticsRequest());
    product.setUrl(YOUTUBE_INVALID_URL);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(product, editProductResponse1))
        .thenReturn(productVariantUpdateRequest);
    Mockito.when(productLevel3V2Service
        .editPriceStockVariantsInfo(DEFAULT_STORE_ID, productLevel3Request, productVariantUpdateRequest,
            editProductResponse1)).thenReturn(imagesUpdateResponse2);
    Mockito.when(productLevel3V2Service.generateProductLevel3(product)).thenReturn(productLevel3);
    Mockito.when(productLevel3V2Wrapper.editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID),
        Mockito.any(ProductL3UpdateRequest.class),Mockito.eq(true), Mockito.eq(false), Mockito.eq(false))).thenReturn(
        ProductEditValidationDTO.builder().editProductResponse(editProductResponse).productL3UpdateRequest(product).L5ValidationFailed(true).needCorrection(true).productL3UpdateRequest(product)
            .build());
    Mockito.when(productLevel3V2Service.updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class),
      Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(null);
    URI uri =
      new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_EDIT_INFO
        .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
      MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(product)))
      .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Wrapper)
        .editProductDetails(Mockito.eq(DEFAULT_REQUEST_ID), Mockito.any(ProductL3UpdateRequest.class), Mockito.eq(true),
            Mockito.eq(false), Mockito.eq(false));
  }

  private ProductL3UpdateRequest generateProductLevel3Request() {
    ProductL3UpdateRequest product = new ProductL3UpdateRequest();
    product.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    product.setProductSku(DEFAULT_PRODUCT_SKU);
    product.setProductName(DEFAULT_PRODUCT_NAME);
    product.setBrand(DEFAULT_BRAND);
    product.setDescription(DEFAULT_DESCRIPTION);
    product.setSpecificationDetail(DEFAULT_SPECIFICATION);

    ProductLevel3AttributeRequest attribute =
      new ProductLevel3AttributeRequest(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BRAND,
        new ArrayList<String>(), DEFAULT_SKU_VALUE);
    String value = "value";
    attribute.getValues().add(value);

    product.setAccessChannel(UpdateProductAccessChannel.MTA_API_UPDATE_SINGLE.getDesc());
    return product;
  }

  @Test
  public void listingPartialUpdateTest() throws Exception {
    ProductLevel3QuickEditV2Request productLevel3QuickEditV2Request =
      new ProductLevel3QuickEditV2Request();
    ItemPriceStockQuickUpdateResponse response = new ItemPriceStockQuickUpdateResponse();
    response.setApiErrorCode(null);
    response.setVariantsErrorList(Collections.EMPTY_LIST);
    productLevel3QuickEditV2Request.setQuickEditV2Requests(Collections.singletonList(
      QuickEditV2Request.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).build()));
    Mockito.when(productLevel3V2Service.quickEditPatching(DEFAULT_STORE_ID, PRODUCT_SKU,
        productLevel3QuickEditV2Request))
      .thenReturn(response);
    URI uri = new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH
        + ProductLevel3V2ControllerPath.LISTING_UPDATE.replaceAll("\\{productSku\\}",
        PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
      .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
      .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
      .build();
    mockMvc.perform(
        MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(objectMapper.writeValueAsString(productLevel3QuickEditV2Request)))
      .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Service).quickEditPatching(DEFAULT_STORE_ID, PRODUCT_SKU,
      productLevel3QuickEditV2Request);
  }

  @Test
  public void listingPartialUpdateApiErrorTest() throws Exception {
    ProductLevel3QuickEditV2Request productLevel3QuickEditV2Request =
      new ProductLevel3QuickEditV2Request();
    ItemPriceStockQuickUpdateResponse response = new ItemPriceStockQuickUpdateResponse();
    response.setApiErrorCode(ApiErrorCode.INVALID_DATA_INPUT);
    response.setVariantsErrorList(Collections.EMPTY_LIST);
    productLevel3QuickEditV2Request.setQuickEditV2Requests(Collections.singletonList(
      QuickEditV2Request.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).build()));
    Mockito.when(productLevel3V2Service.quickEditPatching(DEFAULT_STORE_ID, PRODUCT_SKU,
      productLevel3QuickEditV2Request)).thenReturn(response);
    URI uri = new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH
        + ProductLevel3V2ControllerPath.LISTING_UPDATE.replaceAll("\\{productSku\\}", PRODUCT_SKU))
      .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
      .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
      .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(productLevel3QuickEditV2Request)))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.errorCode", equalTo(ApiErrorCode.INVALID_DATA_INPUT.getCode())))
      .andExpect(jsonPath("$.errorMessage", equalTo(ApiErrorCode.INVALID_DATA_INPUT.getDesc())));
    Mockito.verify(productLevel3V2Service)
      .quickEditPatching(DEFAULT_STORE_ID, PRODUCT_SKU, productLevel3QuickEditV2Request);
  }

  @Test
  public void listingUpdateExceptionTest() throws Exception{
    ProductLevel3QuickEditV2Request productLevel3QuickEditV2Request =
      new ProductLevel3QuickEditV2Request();
    Mockito.when(productLevel3V2Service.quickEditPatching(DEFAULT_STORE_ID, PRODUCT_SKU,
      productLevel3QuickEditV2Request)).thenThrow(new Exception("exception"));
    URI uri = new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH
        + ProductLevel3V2ControllerPath.LISTING_UPDATE.replaceAll("\\{productSku\\}",
        PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
      .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
      .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
      .build();
    mockMvc.perform(
        MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(objectMapper.writeValueAsString(productLevel3QuickEditV2Request)))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.VALIDATION.getCode())))
      .andExpect(jsonPath("$.errorMessage", equalTo("exception")));
    Mockito.verify(productLevel3V2Service).quickEditPatching(DEFAULT_STORE_ID, PRODUCT_SKU,
      productLevel3QuickEditV2Request);
  }

  @Test
  public void getItemSummaryL4ResponseTest() throws Exception {
    Mockito.when(productLevel3V2Service.getItemSummaryL4Response(DEFAULT_STORE_ID, PRODUCT_SKU, 0, 1))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    URI uri = new URIBuilder().setPath(
            ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.FETCH_L4_DETAILS.replaceAll(
                "\\{productSku\\}", PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("page", "0").addParameter("size", "1").build();
    mockMvc.perform(
            MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Service).getItemSummaryL4Response(DEFAULT_STORE_ID, PRODUCT_SKU, 0, 1);
  }

  @Test
  public void getItemSummaryL4ResponseApplicationRuntimeExceptionTest() throws Exception {
    Mockito.when(productLevel3V2Service.getItemSummaryL4Response(DEFAULT_STORE_ID, PRODUCT_SKU, 0, 1))
        .thenThrow(ApplicationRuntimeException.class);
    URI uri = new URIBuilder().setPath(
            ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.FETCH_L4_DETAILS.replaceAll(
                "\\{productSku\\}", PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("page", "0").addParameter("size", "1").build();
    mockMvc.perform(
            MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Service).getItemSummaryL4Response(DEFAULT_STORE_ID, PRODUCT_SKU, 0, 1);
  }

  @Test
  public void getItemSummaryL4ResponseExceptionTest() throws Exception {
    Mockito.when(productLevel3V2Service.getItemSummaryL4Response(DEFAULT_STORE_ID, PRODUCT_SKU, 0, 1))
        .thenThrow(Exception.class);
    URI uri = new URIBuilder().setPath(
            ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.FETCH_L4_DETAILS.replaceAll(
                "\\{productSku\\}", PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("page", "0").addParameter("size", "1").build();
    mockMvc.perform(
            MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Service).getItemSummaryL4Response(DEFAULT_STORE_ID, PRODUCT_SKU, 0, 1);
  }

  @Test
  public void createFbbPickupPointTest() throws Exception {
    FbbCreatePickupPointRequest fbbCreatePickupPointRequest = new FbbCreatePickupPointRequest();
    Mockito.when(productLevel3V2Service.createDefaultFbbPickupPoint(fbbCreatePickupPointRequest))
        .thenReturn(new FbbCreatePickupPointResponse());
    URI uri =
        new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.CREATE_DEFAULT_L5_FBB)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(fbbCreatePickupPointRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Service).createDefaultFbbPickupPoint(fbbCreatePickupPointRequest);
  }

  @Test
  public void createFbbPickupPointApplicationRuntimeExceptionTest() throws Exception {
    FbbCreatePickupPointRequest fbbCreatePickupPointRequest = new FbbCreatePickupPointRequest();
    Mockito.when(productLevel3V2Service.createDefaultFbbPickupPoint(fbbCreatePickupPointRequest))
        .thenThrow(new ApplicationRuntimeException());
    URI uri =
        new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.CREATE_DEFAULT_L5_FBB)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(fbbCreatePickupPointRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Service).createDefaultFbbPickupPoint(fbbCreatePickupPointRequest);
  }

  @Test
  public void createFbbPickupPointExceptionTest() throws Exception {
    FbbCreatePickupPointRequest fbbCreatePickupPointRequest = new FbbCreatePickupPointRequest();
    Mockito.when(productLevel3V2Service.createDefaultFbbPickupPoint(fbbCreatePickupPointRequest))
        .thenThrow(Exception.class);
    URI uri =
        new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.CREATE_DEFAULT_L5_FBB)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(fbbCreatePickupPointRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Service).createDefaultFbbPickupPoint(fbbCreatePickupPointRequest);
  }

  @Test
  public void getItemPickupPointL3ListingTest() throws Exception {
    ItemL5ListingRequest itemPickupPointListingL3Request = new ItemL5ListingRequest();
    Mockito.when(
      productLevel3V2Service.getItemPickupPointL3Listing(DEFAULT_STORE_ID, DEFAULT_USERNAME, DEFAULT_REQUEST_ID, 0, 1,
        itemPickupPointListingL3Request, true)).thenReturn(new PageImpl<>(new ArrayList<>()));
    URI uri =
      new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.FETCH_L5_DETAILS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("page", "0").addParameter("size", "1").build();
    mockMvc.perform(
      MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(itemPickupPointListingL3Request)))
      .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Service)
      .getItemPickupPointL3Listing(DEFAULT_STORE_ID, DEFAULT_USERNAME, DEFAULT_REQUEST_ID, 0, 1,
        itemPickupPointListingL3Request, true);
  }

  @Test
  public void getItemPickupPointL3ListingApplicationRuntimeExceptionTest() throws Exception {
    ItemL5ListingRequest itemPickupPointListingL3Request = new ItemL5ListingRequest();
    Mockito.when(
      productLevel3V2Service.getItemPickupPointL3Listing(DEFAULT_STORE_ID, DEFAULT_USERNAME, DEFAULT_REQUEST_ID, 0, 1,
        itemPickupPointListingL3Request, true)).thenThrow(ApplicationRuntimeException.class);
    URI uri =
      new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.FETCH_L5_DETAILS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("page", "0").addParameter("size", "1").build();
    mockMvc.perform(
      MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(itemPickupPointListingL3Request)))
      .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Service)
      .getItemPickupPointL3Listing(DEFAULT_STORE_ID, DEFAULT_USERNAME, DEFAULT_REQUEST_ID, 0, 1,
        itemPickupPointListingL3Request, true);
  }

  @Test
  public void getItemPickupPointL3ListingExceptionTest() throws Exception {
    ItemL5ListingRequest itemPickupPointListingL3Request = new ItemL5ListingRequest();
    Mockito.when(
        productLevel3V2Service.getItemPickupPointL3Listing(DEFAULT_STORE_ID, DEFAULT_USERNAME, DEFAULT_REQUEST_ID, 0, 1,
            itemPickupPointListingL3Request, true)).thenThrow(Exception.class);
    URI uri =
        new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.FETCH_L5_DETAILS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("page", "0").addParameter("size", "1").build();
    mockMvc.perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(itemPickupPointListingL3Request)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Service)
        .getItemPickupPointL3Listing(DEFAULT_STORE_ID, DEFAULT_USERNAME, DEFAULT_REQUEST_ID, 0, 1,
            itemPickupPointListingL3Request, true);
  }

  @Test
  public void getProductL5DetailsTest() throws Exception {
    List<ItemSkuPpCodeRequest> itemSkusRequest = new ArrayList<>();
    Mockito.when(
      productLevel3V2Service.getProductDetailsByItemSkuAndPickupPointCode(DEFAULT_STORE_ID,
        itemSkusRequest, true)).thenReturn(new ArrayList<>());
    URI uri = new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH
        + ProductLevel3V2ControllerPath.GET_PRODUCT_L5_DETAILS)
      .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
      .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
      .addParameter("username", DEFAULT_USERNAME).build();
    mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(itemSkusRequest)))
      .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Service)
      .getProductDetailsByItemSkuAndPickupPointCode(DEFAULT_STORE_ID, itemSkusRequest, true);
  }

  @Test
  public void getProductL5DetailsApplicationRuntimeExceptionTest() throws Exception {
    List<ItemSkuPpCodeRequest> itemSkusRequest = new ArrayList<>();
    Mockito.when(
      productLevel3V2Service.getProductDetailsByItemSkuAndPickupPointCode(DEFAULT_STORE_ID,
        itemSkusRequest, true)).thenThrow(ApplicationRuntimeException.class);
    URI uri = new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH
        + ProductLevel3V2ControllerPath.GET_PRODUCT_L5_DETAILS)
      .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
      .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
      .addParameter("username", DEFAULT_USERNAME).build();
    mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(itemSkusRequest)))
      .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Service)
      .getProductDetailsByItemSkuAndPickupPointCode(DEFAULT_STORE_ID, itemSkusRequest, true);
  }

  @Test
  public void getProductL5DetailsExceptionTest() throws Exception {
    List<ItemSkuPpCodeRequest> itemSkusRequest = new ArrayList<>();
    Mockito.when(
        productLevel3V2Service.getProductDetailsByItemSkuAndPickupPointCode(DEFAULT_STORE_ID,
            itemSkusRequest, true)).thenThrow(Exception.class);
    URI uri = new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH
            + ProductLevel3V2ControllerPath.GET_PRODUCT_L5_DETAILS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).build();
    mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(itemSkusRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Service)
        .getProductDetailsByItemSkuAndPickupPointCode(DEFAULT_STORE_ID, itemSkusRequest, true);
  }

  @Test
  public void updateBrandTest() throws Exception {
    BrandUpdateRequest brandUpdateRequest = new BrandUpdateRequest();
    URI uri = new URIBuilder().setPath(
            ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.UPDATE_BRAND_OF_PRODUCT.replaceAll(
                "\\{productCode\\}", PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isExternalOnly","false").build();
    mockMvc.perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(brandUpdateRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Service).updateBrandDataOfProduct(PRODUCT_SKU, brandUpdateRequest);
  }

  @Test
  public void updateBrandExceptionTest() throws Exception {
    BrandUpdateRequest brandUpdateRequest = new BrandUpdateRequest();
    Mockito.doThrow(ApplicationRuntimeException.class).when(productLevel3V2Service)
        .updateBrandDataOfProduct(PRODUCT_SKU, brandUpdateRequest);
    URI uri = new URIBuilder().setPath(
            ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.UPDATE_BRAND_OF_PRODUCT.replaceAll(
                "\\{productCode\\}", PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(brandUpdateRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Service).updateBrandDataOfProduct(PRODUCT_SKU, brandUpdateRequest);
  }

  @Test
  public void getProductCountTest() throws Exception {
    Mockito.when(productLevel3V2Service.getProductCount(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, false))
        .thenReturn(5L);
    URI uri = new URIBuilder().setPath(
        ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCTS_COUNT
            .replaceAll("\\{businessPartnerCode\\}", DEFAULT_BUSINESS_PARTNER_CODE)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Service).getProductCount(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, false);
  }

  @Test
  public void getProductCountExceptionTest() throws Exception {
    Mockito.doThrow(new Exception()).when(productLevel3V2Service)
        .getProductCount(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, false);
    URI uri = new URIBuilder().setPath(
        ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCTS_COUNT
            .replaceAll("\\{businessPartnerCode\\}", DEFAULT_BUSINESS_PARTNER_CODE))
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    try {
      mockMvc.perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk());
    } finally {
      Mockito.verify(productLevel3V2Service).getProductCount(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, false);
    }
  }

  @Test
  public void productBasicDetailsTest() throws Exception {
    URI uri = new URIBuilder().setPath(
            ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.BASIC_DETAILS.replaceAll(
                "\\{productCode\\}", PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("isExternalOnly","false").build();
    mockMvc.perform(
            MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Service).getProductL3BasicResponse(DEFAULT_STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void productBasicDetailsExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(productLevel3V2Service)
        .getProductL3BasicResponse(DEFAULT_STORE_ID, PRODUCT_SKU);
    URI uri = new URIBuilder().setPath(
            ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.BASIC_DETAILS.replaceAll(
                "\\{productCode\\}", PRODUCT_SKU)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
            MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productLevel3V2Service).getProductL3BasicResponse(DEFAULT_STORE_ID, PRODUCT_SKU);
  }


  @Test
  public void testEligibilityForNeedRevisionDeletion_Success() throws Exception {
    String storeId = "store123";
    String requestId = "req123";
    String channelId = "channel123";
    String clientId = "client123";
    String username = "user123";
    NeedRevisionEligibilityRequest request = new NeedRevisionEligibilityRequest();
    List<NeedRevisionEligibilityRequest> requestList = Collections.singletonList(request);
    NeedRevisionEligibilityResponse response = new NeedRevisionEligibilityResponse();
    List<NeedRevisionEligibilityResponse> responseList = Collections.singletonList(response);
    Mockito.when(productLevel3V2Service.eligibilityForNeedRevisionDeletion(storeId, requestList, true))
        .thenReturn(responseList);
    mockMvc.perform(MockMvcRequestBuilders.post(ProductLevel3V2ControllerPath.BASE_PATH
                + ProductLevel3V2ControllerPath.ELIGIBILITY_FOR_NEED_REVISION_DELETION)
            .param("storeId", storeId).param("requestId", requestId).param("channelId", channelId)
            .param("clientId", clientId).param("username", username)
            .contentType(MediaType.APPLICATION_JSON).content("[{}]")).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(true));
    Mockito.verify(productLevel3V2Service).eligibilityForNeedRevisionDeletion(storeId, requestList, true);
  }

  @Test
  public void testEligibilityForNeedRevisionDeletion_ExceptionTest() throws Exception {
    String storeId = "store123";
    String requestId = "req123";
    String channelId = "channel123";
    String clientId = "client123";
    String username = "user123";
    NeedRevisionEligibilityRequest request = new NeedRevisionEligibilityRequest();
    List<NeedRevisionEligibilityRequest> requestList = Collections.singletonList(request);
    Mockito.when(productLevel3V2Service.eligibilityForNeedRevisionDeletion(storeId, requestList, true))
        .thenThrow(RuntimeException.class);
    mockMvc.perform(MockMvcRequestBuilders.post(ProductLevel3V2ControllerPath.BASE_PATH
                + ProductLevel3V2ControllerPath.ELIGIBILITY_FOR_NEED_REVISION_DELETION)
            .param("storeId", storeId).param("requestId", requestId).param("channelId", channelId)
            .param("clientId", clientId).param("username", username)
            .contentType(MediaType.APPLICATION_JSON).content("[{}]")).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(false));
    Mockito.verify(productLevel3V2Service).eligibilityForNeedRevisionDeletion(storeId, requestList, true);
  }

  @Test
   void updatedProductMaterData_Success() throws Exception {
    ProductMasterDataEditRequest masterDataEditRequest = new ProductMasterDataEditRequest();
    Mockito.when(productLevel3V2Wrapper.editProductMasterData(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID,
      DEFAULT_USERNAME, masterDataEditRequest)).thenReturn(null);
    URI uri = new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_MASTER_DATA_EDIT_INFO
        .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
        .addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .build();
    mockMvc.perform(
        MockMvcRequestBuilders.post(uri)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(masterDataEditRequest)))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(true))
        .andExpect(jsonPath("$.errorCode").value(nullValue()));
    Mockito.verify(productLevel3V2Wrapper)
      .editProductMasterData(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
        masterDataEditRequest);
  }

  @Test
   void updatedProductMaterData_Exception() throws Exception {
    ProductMasterDataEditRequest masterDataEditRequest = new ProductMasterDataEditRequest();
    Mockito.when(productLevel3V2Wrapper.editProductMasterData(
        DEFAULT_STORE_ID, DEFAULT_REQUEST_ID,
        DEFAULT_USERNAME, masterDataEditRequest))
        .thenThrow(new RuntimeException("error occurred"));
    URI uri = new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_MASTER_DATA_EDIT_INFO
        .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
        .addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .build();
    mockMvc.perform(
        MockMvcRequestBuilders.post(uri)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(masterDataEditRequest)))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(false))
        .andExpect(jsonPath("$.errorMessage").value("error occurred"));
    Mockito.verify(productLevel3V2Wrapper)
      .editProductMasterData(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
        masterDataEditRequest);
  }

  @Test
   void updatedProductMaterData_ApiErrorCode() throws Exception {
    ProductMasterDataEditRequest masterDataEditRequest = new ProductMasterDataEditRequest();
    Mockito.when(productLevel3V2Wrapper.editProductMasterData(
        DEFAULT_STORE_ID, DEFAULT_REQUEST_ID,
        DEFAULT_USERNAME, masterDataEditRequest))
        .thenReturn(ApiErrorCode.INVALID_DATA_INPUT);
    URI uri = new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.PRODUCT_MASTER_DATA_EDIT_INFO
        .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
        .addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .build();
    mockMvc.perform(
        MockMvcRequestBuilders.post(uri)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(masterDataEditRequest)))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(true))
        .andExpect(jsonPath("$.errorCode").value(ApiErrorCode.INVALID_DATA_INPUT.getCode()))
        .andExpect(jsonPath("$.errorMessage").value(ApiErrorCode.INVALID_DATA_INPUT.getDesc()));
    Mockito.verify(productLevel3V2Wrapper)
      .editProductMasterData(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
        masterDataEditRequest);
  }

  @Test
  void updateCogsValue_Success() throws Exception {
    CogsUpdateRequests request = new CogsUpdateRequests();
    Mockito.doNothing().when(productLevel3V2Service).updateCogsValue(DEFAULT_PRODUCT_SKU, request);
    
    URI uri = new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.COGS
        .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
        .addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .build();
    
    mockMvc.perform(
        MockMvcRequestBuilders.post(uri)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request)))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(true))
        .andExpect(jsonPath("$.errorMessage").doesNotExist())
        .andExpect(jsonPath("$.errorCode").doesNotExist());
    
    Mockito.verify(productLevel3V2Service).updateCogsValue(DEFAULT_PRODUCT_SKU, request);
  }

  @Test
  void updateCogsValue_Exception() throws Exception {
    CogsUpdateRequests request = new CogsUpdateRequests();
    Mockito.doThrow(new RuntimeException("Error message")).when(productLevel3V2Service)
        .updateCogsValue(DEFAULT_PRODUCT_SKU, request);
    
    URI uri = new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.COGS
        .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
        .addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .build();
    
    mockMvc.perform(
        MockMvcRequestBuilders.post(uri)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(DEFAULT_OBJECT_MAPPER.writeValueAsString(request)))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(false))
        .andExpect(jsonPath("$.errorMessage").value("Error message"))
        .andExpect(jsonPath("$.errorCode").value(ErrorCategory.UNSPECIFIED.getCode()));
    
    Mockito.verify(productLevel3V2Service).updateCogsValue(DEFAULT_PRODUCT_SKU, request);
  }

  @Test
  void getCogsData_Success() throws Exception {
    CogsDataResponse cogsDataResponse = new CogsDataResponse();
    cogsDataResponse.setItemSku("ITEM-001");
    cogsDataResponse.setPickupPointCode("PP-001");
    cogsDataResponse.setInsuredAmount(100.0);
    
    List<CogsDataResponse> cogsDataList = Arrays.asList(cogsDataResponse);
    Mockito.when(productLevel3V2Service.getCogsData(DEFAULT_PRODUCT_SKU, 0, 20))
        .thenReturn(cogsDataList);
    
    URI uri = new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.COGS
        .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
        .addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("page", "0")
        .addParameter("size", "20")
        .build();
    
    mockMvc.perform(
        MockMvcRequestBuilders.get(uri)
            .accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(true))
        .andExpect(jsonPath("$.content").isArray())
        .andExpect(jsonPath("$.content[0].itemSku").value("ITEM-001"))
        .andExpect(jsonPath("$.content[0].pickupPointCode").value("PP-001"))
        .andExpect(jsonPath("$.content[0].insuredAmount").value(100.0));
    
    Mockito.verify(productLevel3V2Service).getCogsData(DEFAULT_PRODUCT_SKU, 0, 20);
  }

  @Test
  void getCogsData_Exception() throws Exception {
    Mockito.when(productLevel3V2Service.getCogsData(DEFAULT_PRODUCT_SKU, 0, 20))
        .thenThrow(new RuntimeException("Error message"));
    
    URI uri = new URIBuilder().setPath(ProductLevel3V2ControllerPath.BASE_PATH + ProductLevel3V2ControllerPath.COGS
        .replaceAll("\\{productSku\\}", DEFAULT_PRODUCT_SKU))
        .addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("page", "0")
        .addParameter("size", "20")
        .build();
    
    mockMvc.perform(
        MockMvcRequestBuilders.get(uri)
            .accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(false))
        .andExpect(jsonPath("$.errorMessage").value("Error message"))
        .andExpect(jsonPath("$.errorCode").value(ErrorCategory.UNSPECIFIED.getCode()));
    
    Mockito.verify(productLevel3V2Service).getCogsData(DEFAULT_PRODUCT_SKU, 0, 20);
  }

}
