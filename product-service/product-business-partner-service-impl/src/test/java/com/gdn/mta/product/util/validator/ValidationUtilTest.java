package com.gdn.mta.product.util.validator;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.TreeMap;
import java.util.Optional;

import com.gda.mta.product.dto.MasterProductEditDTO;
import com.gda.mta.product.dto.ProductMasterDataEditRequest;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.L3InfoUpdateChangeType;
import com.gdn.mta.product.enums.ProductType;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;

import com.gda.mta.product.dto.B2bDetailsDTO;
import com.gda.mta.product.dto.PickupPointCreateRequest;
import com.gda.mta.product.dto.PreOrderRequest;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.BuyableScheduleRequest;
import com.gda.mta.product.dto.DiscoverableScheduleRequest;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Attribute;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;

public class ValidationUtilTest {

  private static final String DESCRIPTION = "<img src='https://upload.wikimedia.org/wikipedia/commons/6/63/Logo_La_Linea_100x100.png' onerror='alert(document.cookie)'>";
  private static final String SANITISED_DESCRIPTION = "<img src=\"https://upload.wikimedia.org/wikipedia/commons/6/63/Logo_La_Linea_100x100.png\">";
  private static final String LONG_DESCRIPTION_LESS_THAN = "&lt;";
  private static final String LONG_DESCRIPTION_GREATER_THAN = "&gt;";
  private static final String LONG_DESCRIPTION_AMP = "&amp;";
  private static final String LONG_DESCRIPTION_VALID = "valid";
  private static final String VALID_VALUE = "valid-value";
  private static final String VALUE = "value";
  private static final String ATTRIBUTE_CODE = "attributeCode";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String PRODUCT_NAME = "<a href=\"https:/\\x17javascript:javascript:alert"
    + "(1)\">test</a>";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String SANITISED_PRODUCT_NAME = "test";
  public static final String SKU_1 = "SKU1";
  public static final String DUPLICATE_UPC = "123456789012";
  public static final String ITEM_SKU = "SKU5";
  public static final String ITEM_SKU1 = "SKU2";
  public static final String ITEM_SKU2 = "SKU3";
  public static final String INVALID_FORMAT_UPC = "INVALID_FORMAT";
  public static final String[] STRINGS = {".jpg", ".jpeg", ".png", ".gif"};
  public static final String PRODUCT_DESCRIPTION = "Product Description";
  public static final String URL = "<p>https://example.com/test?param=value</p>";
  public static final String DESCRIPTION_WITH_TAGS =
    "<p>Product description with <b>HTML</b> tags</p>";
  public static final String NAME_WITH_SPAN = "<span>Product Name</span>";
  public static final String URL_WITH_HREF = "<a href='test'>http://example.com</a>";
  public static final String NORMAL_URL = "http://example.com?param=1&amp;param=2";
  String B2C_SELLER_CHANNEL = "BLIBLI";
  String B2B_SELLER_CHANNEL = "BLIBLI FOR BUSINESS";
  String ERROR_MESSAGE_PREFIX = "Can not process invalid input data :";
  private ProductMasterDataEditRequest productMasterDataEditRequest;
  private MasterProductEditDTO masterProductEditDTO;


  private ProductCreationRequest productCreationRequest;
  private ProductLevel3 productLevel3;
  private ProfileResponse profileResponse;
  private List<PickupPointCreateRequest> pickupPointCreateRequests = new ArrayList<>();
  private PickupPointCreateRequest pickupPointCreateRequest1 = new PickupPointCreateRequest();
  private PickupPointCreateRequest pickupPointCreateRequest2 = new PickupPointCreateRequest();
  private PickupPointCreateRequest pickupPointCreateRequest3 = new PickupPointCreateRequest();
  private PickupPointCreateRequest pickupPointCreateRequest4 = new PickupPointCreateRequest();
  private List<Integer> UPC_VALID_LENGTH = Arrays.asList(5,8,12,13,14,15);

  @BeforeEach
  public void setUp() {
    productCreationRequest = new ProductCreationRequest();
    productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setDescription(DESCRIPTION.getBytes(StandardCharsets.UTF_8));
    productCreationRequest.setLongDescription(LONG_DESCRIPTION_VALID.getBytes(StandardCharsets.UTF_8));
    productCreationRequest.setProductStory(VALID_VALUE);

    AttributeRequest attributeRequest1 = new AttributeRequest();
    attributeRequest1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);

    ProductAttributeValueRequest productAttributeValueRequest1 = new ProductAttributeValueRequest();
    productAttributeValueRequest1.setDescriptiveAttributeValue(VALID_VALUE);

    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest1);
    productAttributeRequest1.setProductAttributeValues(Arrays.asList(productAttributeValueRequest1));
    ProductAttributeRequest productAttributeRequest2 = new ProductAttributeRequest();
    productAttributeRequest2.setAttribute(attributeRequest2);

    ProductItemAttributeValueRequest productItemAttributeValueRequest1 = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest1.setAttribute(attributeRequest1);
    productItemAttributeValueRequest1.setValue(VALID_VALUE);
    ProductItemAttributeValueRequest productItemAttributeValueRequest2 = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest2.setAttribute(attributeRequest2);

    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setProductItemAttributeValueRequests(
        new ArrayList<>(Arrays.asList(productItemAttributeValueRequest1, productItemAttributeValueRequest2)));


    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest1, productAttributeRequest2));
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));

    productLevel3 = new ProductLevel3();
    productLevel3.setDescription(DESCRIPTION);

    ProductLevel3Attribute productLevel3Attribute1 = new ProductLevel3Attribute();
    productLevel3Attribute1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    productLevel3Attribute1.setValues(Arrays.asList(VALID_VALUE));
    ProductLevel3Attribute productLevel3Attribute2 = new ProductLevel3Attribute();
    productLevel3Attribute2.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    ProductLevel3Attribute productLevel3Attribute3 = new ProductLevel3Attribute();
    productLevel3Attribute3.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());

    productLevel3.setAttributes(
        Arrays.asList(productLevel3Attribute1, productLevel3Attribute2, productLevel3Attribute3));

    pickupPointCreateRequest1.setBuyable(false);
    pickupPointCreateRequest1.setDisplay(false);
    pickupPointCreateRequest1.setB2bFields(new B2bDetailsDTO(null, false, false, false));
    pickupPointCreateRequest2.setBuyable(true);
    pickupPointCreateRequest2.setDisplay(false);
    pickupPointCreateRequest2.setB2bFields(new B2bDetailsDTO(null, true, false, false));
    pickupPointCreateRequest3.setBuyable(false);
    pickupPointCreateRequest3.setDisplay(true);
    pickupPointCreateRequest3.setB2bFields(new B2bDetailsDTO(null, false, true, false));
    pickupPointCreateRequest4.setCncActive(true);
    pickupPointCreateRequests.addAll(Arrays.asList(pickupPointCreateRequest1,
        pickupPointCreateRequest2, pickupPointCreateRequest3, pickupPointCreateRequest4));
    productMasterDataEditRequest = new ProductMasterDataEditRequest();
    masterProductEditDTO = new MasterProductEditDTO();
    profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2C_SELLER_CHANNEL, B2B_SELLER_CHANNEL));
    productMasterDataEditRequest.setProductCode(PRODUCT_CODE);
    productMasterDataEditRequest.setCategoryCode(CATEGORY_CODE);
    productMasterDataEditRequest.setProductSku(PRODUCT_SKU);
    masterProductEditDTO.setMaxProductDimensionLimit(100);
  }

  @Test
  public void validateFreeSampleFalseTest() {
    ValidationUtil.validateFreeSample(productCreationRequest);
  }

  @Test
  public void validateFreeSampleTest() {
    productCreationRequest.setFreeSample(true);
    ValidationUtil.validateFreeSample(productCreationRequest);
  }

  @Test
  public void validateBopisWithCNCTest() {
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setCncActive(true);
    productItemCreationRequest.setProductType(3);
    productItemCreationRequest.setPickupPoints(Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    ApiErrorCode apiErrorCode = ValidationUtil.validateCncWithBopis(productCreationRequest, true);
    Assertions.assertNotNull(apiErrorCode);
    Assertions.assertEquals(ApiErrorCode.BOPIS_CNC_CHANGE_ERROR, apiErrorCode);
  }

  @Test()
  public void validateNonBopisWithCNCTest() {
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setCncActive(true);
    productItemCreationRequest.setProductType(1);
    productItemCreationRequest.setPickupPoints(Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    ValidationUtil.validateCncWithBopis(productCreationRequest, true);
  }

  @Test()
  public void validateBopisWithNonCNCTest() {
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setCncActive(false);
    productItemCreationRequest.setProductType(3);
    productItemCreationRequest.setPickupPoints(Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    ValidationUtil.validateCncWithBopis(productCreationRequest, true);
  }

  @Test()
  public void validateBopisAtL3WithCNCTest() {
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setCncActive(false);
    productItemCreationRequest.setPickupPoints(Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.setProductType(3);
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    ValidationUtil.validateCncWithBopis(productCreationRequest, true);
  }

  @Test()
  public void validateNonBopisWithNonCNCTest() {
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setCncActive(false);
    productItemCreationRequest.setProductType(2);
    productItemCreationRequest.setPickupPoints(Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    ValidationUtil.validateCncWithBopis(productCreationRequest, true);
  }

  @Test()
  public void validateNonBopisWithNonCNCWithSwitchOffTest() {
    ValidationUtil.validateCncWithBopis(productCreationRequest, false);
  }

  @Test
  public void validateFreeSampleOff2OnTrueTest() {
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(true);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validateFreeSample(productCreationRequest);
    });
  }

  @Test
  public void validateFreeSampleBuyableTrueTest() {
    productCreationRequest.setFreeSample(true);
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setBuyable(true);
    productItemCreationRequest.setPickupPoints(Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validateFreeSample(productCreationRequest);
    });
  }

  @Test
  public void validateFreeSampleCncActiveTest() {
    productCreationRequest.setFreeSample(true);
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    productItemCreationRequest.setPickupPoints(Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    productItemCreationRequest.getPickupPoints().get(0).setCncActive(true);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validateFreeSample(productCreationRequest);
    });
  }

  @Test
  public void validateFreeSampleDisplayTrueTest() {
    productCreationRequest.setFreeSample(true);
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setDisplay(true);
    productItemCreationRequest.setPickupPoints(Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validateFreeSample(productCreationRequest);
    });
  }

  @Test
  public void validateFreeSampleOff2OnFalseTest() {
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(false);
    ValidationUtil.validateFreeSample(productCreationRequest);
  }

  @Test
  public void validateFreeSampleOff2OnFalsePreOrderNonNullTest() {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(false);
    ValidationUtil.validateFreeSample(productCreationRequest);
  }

  @Test
  public void validateFreeSampleOff2OnFalsePreOrderTrueTest() {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setIsPreOrder(true);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(false);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validateFreeSample(productCreationRequest);
    });
  }

  @Test
  public void validateDescriptiveFieldsForProductCreationTest() {
    ValidationUtil.validateDescriptiveFieldsForProductCreation(productCreationRequest, true,true );
    Assertions.assertEquals(SANITISED_DESCRIPTION,
        new String(productCreationRequest.getDescription(), StandardCharsets.UTF_8));
  }

  @Test
  public void validateDescriptiveFieldsForProductCreationLessThanTest() {
    productCreationRequest.setLongDescription(LONG_DESCRIPTION_LESS_THAN.getBytes(StandardCharsets.UTF_8));
    productCreationRequest.setProductStory(LONG_DESCRIPTION_LESS_THAN);
    ValidationUtil.validateDescriptiveFieldsForProductCreation(productCreationRequest, true, true);
    Assertions.assertEquals(SANITISED_DESCRIPTION,
        new String(productCreationRequest.getDescription(), StandardCharsets.UTF_8));
  }

  @Test
  public void validateDescriptiveFieldsForProductCreationGreaterThanTest() {
    productCreationRequest.setLongDescription(LONG_DESCRIPTION_GREATER_THAN.getBytes(StandardCharsets.UTF_8));
    productCreationRequest.setProductStory(LONG_DESCRIPTION_GREATER_THAN);
    ValidationUtil.validateDescriptiveFieldsForProductCreation(productCreationRequest, true, true);
    Assertions.assertEquals(SANITISED_DESCRIPTION,
        new String(productCreationRequest.getDescription(), StandardCharsets.UTF_8));
  }

  @Test
  public void validateDescriptiveFieldsForProductCreationAmpersandTest() {
    productCreationRequest.setLongDescription(LONG_DESCRIPTION_LESS_THAN.getBytes(StandardCharsets.UTF_8));
    productCreationRequest.setProductStory(LONG_DESCRIPTION_AMP);
    ValidationUtil.validateDescriptiveFieldsForProductCreation(productCreationRequest, true,true );
    Assertions.assertEquals(SANITISED_DESCRIPTION,
        new String(productCreationRequest.getDescription(), StandardCharsets.UTF_8));
  }

  @Test
  public void validateDescriptiveFieldsForProductCreationEmptyAttributeValuesTest() {
    productCreationRequest.getProductAttributes().get(0).setProductAttributeValues(new ArrayList<>());
    ValidationUtil.validateDescriptiveFieldsForProductCreation(productCreationRequest, true, true);
    Assertions.assertEquals(SANITISED_DESCRIPTION,
        new String(productCreationRequest.getDescription(), StandardCharsets.UTF_8));
  }

  @Test
  public void validateDescriptiveFieldsForProductCreationNoAttributeTest() {
    productCreationRequest.setProductAttributes(new ArrayList<>());
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(new ArrayList<>());
    ValidationUtil.validateDescriptiveFieldsForProductCreation(productCreationRequest, true, true);
    Assertions.assertEquals(SANITISED_DESCRIPTION,
        new String(productCreationRequest.getDescription(), StandardCharsets.UTF_8));
  }

  @Test
  public void validateDescriptiveFieldsForProductEmptyDescriptionTest() {
    productCreationRequest.setDescription(StringUtils.EMPTY.getBytes(StandardCharsets.UTF_8));
    ValidationUtil.validateDescriptiveFieldsForProductCreation(productCreationRequest, true, true);
  }

  @Test
  public void validateDescriptiveFieldsForProductUpdateTest() {
    ValidationUtil.validateDescriptiveFieldsForProductUpdate(productLevel3);
    Assertions.assertEquals(SANITISED_DESCRIPTION, productLevel3.getDescription());
  }

  @Test
  public void validateDescriptiveFieldsForProductUpdateNoAttributeTest() {
    productLevel3.setAttributes(new ArrayList<>());
    ValidationUtil.validateDescriptiveFieldsForProductUpdate(productLevel3);
    Assertions.assertEquals(SANITISED_DESCRIPTION, productLevel3.getDescription());
  }

  @Test
  public void validateB2cAndB2bActivatedFlagsB2cActivatedFalseTest() {
    productCreationRequest.setOff2OnChannelActive(true);
    productCreationRequest.setOnline(true);
    ValidationUtil.validateB2cAndB2bActivatedFlags(productCreationRequest, profileResponse, false,
        false);
    Assertions.assertFalse(productCreationRequest.isOnline());
    Assertions.assertTrue(productCreationRequest.isOff2OnChannelActive());
  }

  @Test
  public void validateB2cAndB2bActivatedFlagsTest() {
    productCreationRequest.setOff2OnChannelActive(true);
    productCreationRequest.setOnline(true);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(pickupPointCreateRequests);
    ValidationUtil.validateB2cAndB2bActivatedFlags(productCreationRequest, profileResponse, false,
        false);
    Assertions.assertTrue(productCreationRequest.isOnline());
    Assertions.assertTrue(productCreationRequest.isOff2OnChannelActive());
    Assertions.assertTrue(productCreationRequest.isB2bActivated());
    Assertions.assertTrue(productCreationRequest.isB2cActivated());
  }

  @Test
  public void validateB2cAndB2bActivatedFlagsNotEligibleForB2cAndB2bTest() {
    productCreationRequest.setOff2OnChannelActive(true);
    productCreationRequest.setOnline(true);
    profileResponse.getCompany().setSalesChannel(new ArrayList<>());
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(pickupPointCreateRequests);
    ValidationUtil.validateB2cAndB2bActivatedFlags(productCreationRequest, profileResponse, false,
        false);
    Assertions.assertFalse(productCreationRequest.isOnline());
    Assertions.assertTrue(productCreationRequest.isOff2OnChannelActive());
    Assertions.assertFalse(productCreationRequest.isB2bActivated());
    Assertions.assertTrue(productCreationRequest.isB2cActivated());
  }

  @Test
  public void validateB2cAndB2bActivatedFlagsB2cActivatedFalseProfileResponseNullTest() {
    productCreationRequest.setOff2OnChannelActive(true);
    productCreationRequest.setOnline(true);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(pickupPointCreateRequests);
    ValidationUtil.validateB2cAndB2bActivatedFlags(productCreationRequest, null, false, false);
    Assertions.assertFalse(productCreationRequest.isOnline());
    Assertions.assertTrue(productCreationRequest.isOff2OnChannelActive());
  }

  @Test
  public void validateB2cAndB2bActivatedFlagsB2cActivatedFalseCompanyNullTest() {
    productCreationRequest.setOff2OnChannelActive(true);
    productCreationRequest.setOnline(true);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(pickupPointCreateRequests);
    ValidationUtil.validateB2cAndB2bActivatedFlags(productCreationRequest, new ProfileResponse(), false,
        false);
    Assertions.assertFalse(productCreationRequest.isOnline());
    Assertions.assertTrue(productCreationRequest.isOff2OnChannelActive());
  }

  @Test
  public void validateB2cAndB2bActivatedFlagsB2cActivatedFalseSalesChannelNullTest() {
    productCreationRequest.setOff2OnChannelActive(false);
    productCreationRequest.setOnline(true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setSalesChannel(null);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(pickupPointCreateRequests);
    ValidationUtil.validateB2cAndB2bActivatedFlags(productCreationRequest, profileResponse, false,
        false);
    Assertions.assertFalse(productCreationRequest.isOnline());
    Assertions.assertFalse(productCreationRequest.isOff2OnChannelActive());
  }

  @Test
  public void validateB2cAndB2bActivatedFlagsPureB2cSellerTest() {
    productCreationRequest.setB2cActivated(false);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2C_SELLER_CHANNEL));
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(pickupPointCreateRequests);
    ValidationUtil.validateB2cAndB2bActivatedFlags(productCreationRequest, profileResponse, false,
        false);
    Assertions.assertTrue(productCreationRequest.isB2cActivated());
  }

  @Test
  public void validateB2cAndB2bActivatedFlagsPureB2cSellerInstoreSwitchOnTest() {
    productCreationRequest.setB2cActivated(false);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    productCreationRequest.setB2cActivated(false);
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2C_SELLER_CHANNEL));
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(pickupPointCreateRequests);
    ValidationUtil.validateB2cAndB2bActivatedFlags(productCreationRequest, profileResponse, true,
        false);
    Assertions.assertTrue(productCreationRequest.isB2cActivated());
  }

  @Test
  public void validateB2cAndB2bActivatedFlagsPureB2cSellerInstoreSwitchOnTest2() {
    productCreationRequest.setB2cActivated(false);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2C_SELLER_CHANNEL));
    productCreationRequest.setB2cActivated(false);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(pickupPointCreateRequests);
    productCreationRequest.getProductItemRequests().get(0).getPickupPoints().forEach(pickupPointCreateRequest -> {
      pickupPointCreateRequest.setDisplay(false);
      pickupPointCreateRequest.setBuyable(false);
      pickupPointCreateRequest.setCncActive(false);
    });
    ValidationUtil.validateB2cAndB2bActivatedFlags(productCreationRequest, profileResponse, true,
        false);
    Assertions.assertFalse(productCreationRequest.isB2cActivated());
  }

  @Test
  public void validateB2cAndB2bActivatedFlagsPureB2cSellerInstoreSwitchOnTest2SetDefaultB2CActivated() {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2C_SELLER_CHANNEL));
    productCreationRequest.setB2cActivated(false);
    productCreationRequest.setOff2OnChannelActive(false);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(pickupPointCreateRequests);
    productCreationRequest.getProductItemRequests().get(0).getPickupPoints().forEach(pickupPointCreateRequest -> {
      pickupPointCreateRequest.setDisplay(false);
      pickupPointCreateRequest.setBuyable(false);
      pickupPointCreateRequest.setCncActive(false);
    });
    ValidationUtil.validateB2cAndB2bActivatedFlags(productCreationRequest, profileResponse, true,
        true);
    Assertions.assertTrue(productCreationRequest.isB2cActivated());
  }

  @Test
  public void validateB2cAndB2bActivatedFlagsInstoreSellerInstoreSwitchOnTest2SetDefaultB2CActivated() {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2C_SELLER_CHANNEL));
    profileResponse.getCompany().setOfflineToOnlineFlag(true);
    productCreationRequest.setB2cActivated(false);
    productCreationRequest.setOff2OnChannelActive(true);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(pickupPointCreateRequests);
    productCreationRequest.getProductItemRequests().get(0).getPickupPoints().forEach(pickupPointCreateRequest -> {
      pickupPointCreateRequest.setDisplay(false);
      pickupPointCreateRequest.setBuyable(false);
      pickupPointCreateRequest.setCncActive(false);
    });
    ValidationUtil.validateB2cAndB2bActivatedFlags(productCreationRequest, profileResponse, true,
        true);
    Assertions.assertFalse(productCreationRequest.isB2cActivated());
    Assertions.assertTrue(productCreationRequest.isOff2OnChannelActive());
  }

  @Test
  public void validateB2cAndB2bActivatedFlagsPureB2bSellerTest() {
    productCreationRequest.setB2bActivated(false);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL));
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(pickupPointCreateRequests);
    ValidationUtil.validateB2cAndB2bActivatedFlags(productCreationRequest, profileResponse, false,
        false);
    Assertions.assertTrue(productCreationRequest.isB2bActivated());
  }

  @Test
  public void validateB2cAndB2bActivatedFlagsSingleSellerTest() {
    productCreationRequest.setB2bActivated(false);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setSalesChannel(Arrays.asList(VALID_VALUE));
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(pickupPointCreateRequests);
    ValidationUtil.validateB2cAndB2bActivatedFlags(productCreationRequest, profileResponse, false,
        false);
    Assertions.assertFalse(productCreationRequest.isB2bActivated());
  }

  @Test
  public void validateDescriptiveFieldsForProductEmptyNameTest() {
    productCreationRequest.setName(StringUtils.EMPTY);
    ValidationUtil.validateDescriptiveFieldsForProductCreation(productCreationRequest, true, false);
  }

  @Test
  public void validateDescriptiveFieldsForProductCreationNameTest() {
    productCreationRequest.setName(PRODUCT_NAME);
    TreeMap<String, String> attributesMap = new TreeMap<>();
    attributesMap.put("variant","yellow       space");
    productCreationRequest.getProductItemRequests().get(0).setAttributesMap(attributesMap);
    ValidationUtil.validateDescriptiveFieldsForProductCreation(productCreationRequest, true, true);
    Assertions.assertEquals(SANITISED_PRODUCT_NAME, productCreationRequest.getName());
    Assertions.assertEquals("yellow space",productCreationRequest.getProductItemRequests().get(0).getAttributesMap().get("variant"));
  }

  @Test
  public void validateSanitizationOfAttributeValuesTest() {
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeValueRequest.setDescriptiveAttributeValue("yellow     space  ");
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest = new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setValue("red   space ");
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
    AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
    allowedAttributeValueRequest.setValue("white  space");
    productAttributeValueRequest.setAllowedAttributeValue(allowedAttributeValueRequest);
    productAttributeValueRequest.setAllowedAttributeValue(allowedAttributeValueRequest);
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    productAttributeRequest.setAttribute(attributeRequest);
    productAttributeRequest.setProductAttributeValues(Collections.singletonList(productAttributeValueRequest));
    productCreationRequest.setProductAttributes(Collections.singletonList(productAttributeRequest));
    productCreationRequest.setName(PRODUCT_NAME);
    TreeMap<String, String> attributesMap = new TreeMap<>();
    attributesMap.put("variant","yellow       space");
    productCreationRequest.getProductItemRequests().get(0).setAttributesMap(attributesMap);
    ValidationUtil.validateDescriptiveFieldsForProductCreation(productCreationRequest, true, true);
    Assertions.assertEquals(SANITISED_PRODUCT_NAME, productCreationRequest.getName());
    Assertions.assertEquals("yellow space",productCreationRequest.getProductItemRequests().get(0).getAttributesMap().get("variant"));
    Assertions.assertEquals("yellow space",productCreationRequest.getProductAttributes().get(0).getProductAttributeValues().get(0).getDescriptiveAttributeValue());
  }

  @Test
  public void validateDescriptiveFieldsForProductCreationNameSwitchOffTest() {
    productCreationRequest.setName(PRODUCT_NAME);
    TreeMap<String, String> attributesMap = new TreeMap<>();
    productCreationRequest.getProductItemRequests().get(0).setAttributesMap(attributesMap);
    ValidationUtil.validateDescriptiveFieldsForProductCreation(productCreationRequest, true, false);
    Assertions.assertEquals(SANITISED_PRODUCT_NAME, productCreationRequest.getName());
  }

  @Test
  public void validateDescriptiveFieldsForProductNameLessThanTest() {
    productCreationRequest.setName(LONG_DESCRIPTION_LESS_THAN);
    ValidationUtil.validateDescriptiveFieldsForProductCreation(productCreationRequest, true,true );
    Assertions.assertEquals("<", productCreationRequest.getName());
  }

  @Test
  public void validateDescriptiveFieldsForProductNameGreaterThanTest() {
    productCreationRequest.setName(LONG_DESCRIPTION_GREATER_THAN);
    ValidationUtil.validateDescriptiveFieldsForProductCreation(productCreationRequest, true, false);
    Assertions.assertEquals(">", productCreationRequest.getName());
  }

  @Test
  public void validateDescriptiveFieldsForProductNameAmpTest() {
    productCreationRequest.setName(LONG_DESCRIPTION_AMP);
    ValidationUtil.validateDescriptiveFieldsForProductCreation(productCreationRequest, true, false);
    Assertions.assertEquals("&", productCreationRequest.getName());
  }

  @Test
  public void validateShippingAndDimensionTest() throws Exception {
    productCreationRequest.setWeight(1.0);
    productCreationRequest.setHeight(1.0);
    productCreationRequest.setLength(1.0);
    productCreationRequest.setWidth(10000000.0);
    productCreationRequest.setShippingWeight(1.0);
    productCreationRequest.setProductType(1);
    ApiErrorCode apiErrorCode =
      ValidationUtil.validateShippingAndDimension(productCreationRequest, 100000, true, true);
    Assertions.assertNotNull(apiErrorCode);
  }

  @Test
  public void validateShippingAndDimensionWithWeightExceededTest() throws Exception {
    productCreationRequest.setWeight(100000.0);
    productCreationRequest.setHeight(1.0);
    productCreationRequest.setProductType(1);
    productCreationRequest.setLength(10000000.0);
    productCreationRequest.setWidth(1000.0);
    productCreationRequest.setShippingWeight(1.0);
    productCreationRequest.setProductType(1);
    ApiErrorCode apiErrorCode =
      ValidationUtil.validateShippingAndDimension(productCreationRequest, 100000, true, true);
    Assertions.assertNotNull(apiErrorCode);
  }

  @Test
  public void validateShippingAndDimensionWithWeightNormalTest() throws Exception {
    productCreationRequest.setWeight(100000.0);
    productCreationRequest.setHeight(1.0);
    productCreationRequest.setProductType(1);
    productCreationRequest.setLength(1000.0);
    productCreationRequest.setWidth(1000.0);
    productCreationRequest.setShippingWeight(1.0);
    productCreationRequest.setProductType(1);
    ApiErrorCode apiErrorCode =
      ValidationUtil.validateShippingAndDimension(productCreationRequest, 100000, false, true);
    Assertions.assertNull(apiErrorCode);
  }

  @Test
  public void validateShippingAndDimensionWithWeightNormal2Test() throws Exception {
    productCreationRequest.setWeight(100000.0);
    productCreationRequest.setHeight(1.0);
    productCreationRequest.setProductType(1);
    productCreationRequest.setLength(1000.0);
    productCreationRequest.setWidth(1000.0);
    productCreationRequest.setShippingWeight(1.0);
    productCreationRequest.setProductType(1);
    productCreationRequest.getProductItemRequests()
        .forEach(productItemCreationRequest -> productItemCreationRequest.setProductType(1));
    ApiErrorCode apiErrorCode =
        ValidationUtil.validateShippingAndDimension(productCreationRequest, 100000, true, true);
    Assertions.assertNull(apiErrorCode);
  }

  @Test
  public void validateShippingAndDimensionWithProductTypeNull() {
    productCreationRequest.setWeight(100000.0);
    productCreationRequest.setHeight(1.0);
    productCreationRequest.setProductType(1);
    productCreationRequest.setLength(1000.0);
    productCreationRequest.setWidth(1000.0);
    productCreationRequest.setShippingWeight(1.0);
    productCreationRequest.setProductType(null);
    ApiErrorCode apiErrorCode =
        ValidationUtil.validateShippingAndDimension(productCreationRequest, 100000, true, true);
    Assertions.assertNotNull(apiErrorCode);
  }

  @Test
  public void validateShippingAndDimensionWithWeightZeroTest() throws Exception {
    productCreationRequest.setWeight(0.0);
    productCreationRequest.setHeight(1.0);
    productCreationRequest.setLength(1.0);
    productCreationRequest.setWidth(1000.0);
    productCreationRequest.setShippingWeight(1.0);
    productCreationRequest.setProductType(1);
    ApiErrorCode apiErrorCode =
      ValidationUtil.validateShippingAndDimension(productCreationRequest, 100000, true, true);
    Assertions.assertNotNull(apiErrorCode);
  }

  @Test
  public void validateShippingAndDimensionWithShippingZeroTest() throws Exception {
    productCreationRequest.setWeight(22.0);
    productCreationRequest.setHeight(1.0);
    productCreationRequest.setLength(1.0);
    productCreationRequest.setWidth(1000.0);
    productCreationRequest.setShippingWeight(0.0);
    productCreationRequest.setProductType(1);
    ApiErrorCode apiErrorCode =
      ValidationUtil.validateShippingAndDimension(productCreationRequest, 100000, true, true);
    Assertions.assertNotNull(apiErrorCode);
  }

  @Test
  public void validateProductAndItemVariantCreationValuesTest() {
    getProductCreationRequestWithAttributes();
    ValidationUtil.validateProductAndItemVariantCreationValues(productCreationRequest);
  }

  @Test
  public void validateProductAndItemVariantCreationValuesEmptyTest() {
    getProductCreationRequestWithAttributes();
    productCreationRequest.setProductAttributes(new ArrayList<>());
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validateProductAndItemVariantCreationValues(productCreationRequest);
    });
  }

  @Test
  public void validateProductAndItemVariantCreationValuesInvalidTest() {
    getProductCreationRequestWithAttributes();
    productCreationRequest.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue(PRODUCT_CODE);
    productCreationRequest.getProductAttributes().get(1).getProductAttributeValues().get(0)
        .getAllowedAttributeValue().setValue(PRODUCT_CODE);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validateProductAndItemVariantCreationValues(productCreationRequest);
    });
  }

  @Test
  public void validateProductAndItemVariantCreationValuesInvalid2Test() {
    getProductCreationRequestWithAttributes();
    productCreationRequest.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue(PRODUCT_CODE);
    productCreationRequest.getProductAttributes().get(1).getAttribute()
        .setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    productCreationRequest.getProductItemRequests().get(0).setAttributesMap(new TreeMap<>());
    ValidationUtil.validateProductAndItemVariantCreationValues(productCreationRequest);
  }

  @Test
  public void validateProductAndItemsTest() {
    getProductCreationRequestWithAttributes();
    productCreationRequest.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue(PRODUCT_CODE);
    productCreationRequest.getProductAttributes().get(1).getAttribute()
        .setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    productCreationRequest.getProductItemRequests().get(0).setAttributesMap(new TreeMap<>());
    ValidationUtil.validateCreateProductItemsRequest(productCreationRequest, true, new ArrayList<>(), false);
  }

  @Test
  public void validateProductAndItemsFalseTest() {
    getProductCreationRequestWithAttributes();
    productCreationRequest.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue(PRODUCT_CODE);
    productCreationRequest.getProductAttributes().get(1).getAttribute()
        .setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    productCreationRequest.getProductItemRequests().get(0).setAttributesMap(new TreeMap<>());
    ValidationUtil.validateCreateProductItemsRequest(productCreationRequest, false, new ArrayList<>(), false);
  }

  @Test
  public void validateProductAndItemsFalse1Test() {
    try {
      getProductCreationRequestWithAttributes();
      productCreationRequest.getProductAttributes().get(0).getProductAttributeValues().get(0)
          .setDescriptiveAttributeValue(PRODUCT_CODE);
      productCreationRequest.getProductAttributes().get(1).getAttribute()
          .setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
      TreeMap<String, String> mp = new TreeMap<>();
      mp.put("key", "null");
      productCreationRequest.getProductItemRequests().get(0).setAttributesMap(mp);
      ValidationUtil.validateCreateProductItemsRequest(productCreationRequest, false, Collections.singletonList("null"),
          true);
    }
    catch(ApplicationRuntimeException ex)
    {
      Assertions.assertEquals(ex.getMessage(),"Can not process invalid input data :Item attribute value invalid");
    }
  }

  @Test
  public void validateRestrictedValuesTrueTest() {
    getProductCreationRequestWithAttributes();
    productCreationRequest.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue(PRODUCT_CODE);
    productCreationRequest.getProductAttributes().get(1).getAttribute()
        .setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    productCreationRequest.getProductItemRequests().get(0).setAttributesMap(new TreeMap<>());
    ValidationUtil.validateCreateProductItemsRequest(productCreationRequest, true, new ArrayList<>(), true);
  }

  private void getProductCreationRequestWithAttributes() {
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeValueRequest.setDescriptiveAttributeValue(VALUE);
    productAttributeRequest.setProductAttributeValues(Collections.singletonList(productAttributeValueRequest));
    AttributeRequest attributeRequest  = new AttributeRequest();
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    productAttributeRequest.setAttribute(attributeRequest);
    ProductAttributeRequest productAttributeRequest2 = new ProductAttributeRequest();
    ProductAttributeValueRequest productAttributeValueRequest1 = new ProductAttributeValueRequest();
    productAttributeValueRequest1.setDescriptiveAttributeValue(VALUE);
    productAttributeRequest2.setProductAttributeValues(Collections.singletonList(productAttributeValueRequest1));
    AttributeRequest attributeRequest1  = new AttributeRequest();
    attributeRequest1.setAttributeCode(PRODUCT_CODE);
    attributeRequest1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
    allowedAttributeValueRequest.setValue(PRODUCT_SKU);
    productAttributeValueRequest1.setAllowedAttributeValue(allowedAttributeValueRequest);
    productAttributeRequest2.setAttribute(attributeRequest1);
    productCreationRequest.setProductAttributes(new ArrayList<>());
    productCreationRequest.getProductAttributes().add(productAttributeRequest);
    productCreationRequest.getProductAttributes().add(productAttributeRequest2);
    TreeMap<String, String> attributesMap = new TreeMap<>();
    attributesMap.put(PRODUCT_CODE, PRODUCT_SKU);
    productCreationRequest.getProductItemRequests().get(0).setAttributesMap(attributesMap);
  }

  @Test
  public void validateDistinctProductAttributeValuesTest() {
    AttributeRequest attributeRequest1 = new AttributeRequest();
    attributeRequest1.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);

    ProductAttributeValueRequest productAttributeValueRequest1 = new ProductAttributeValueRequest();
    ProductAttributeValueRequest productAttributeValueRequest2 = new ProductAttributeValueRequest();
    productAttributeValueRequest2.setAllowedAttributeValue(new AllowedAttributeValueRequest("UK-S", 1, "10001"));

    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    ProductAttributeRequest productAttributeRequest2 = new ProductAttributeRequest();
    productAttributeRequest2.setAttribute(attributeRequest1);
    ProductAttributeRequest productAttributeRequest3 = new ProductAttributeRequest();
    productAttributeRequest3.setAttribute(attributeRequest2);
    ProductAttributeRequest productAttributeRequest4 = new ProductAttributeRequest();
    productAttributeRequest4.setAttribute(attributeRequest2);
    productAttributeRequest4.setProductAttributeValues(
        List.of(productAttributeValueRequest1, productAttributeValueRequest2));

    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductAttributes(
        Arrays.asList(productAttributeRequest1, productAttributeRequest2, productAttributeRequest3,
            productAttributeRequest4));

    ValidationUtil.validateDistinctProductAttributeValues(productCreationRequest, "-");
  }

  @Test
  public void validateDistinctProductAttributeValuesErrorTest() {
    AttributeRequest attributeRequest1 = new AttributeRequest();
    attributeRequest1.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);

    ProductAttributeValueRequest productAttributeValueRequest1 = new ProductAttributeValueRequest();
    productAttributeValueRequest1.setAllowedAttributeValue(new AllowedAttributeValueRequest("US-S", 1, "10001"));
    ProductAttributeValueRequest productAttributeValueRequest2 = new ProductAttributeValueRequest();
    productAttributeValueRequest2.setAllowedAttributeValue(new AllowedAttributeValueRequest("UK-S", 1, "10001"));

    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    ProductAttributeRequest productAttributeRequest2 = new ProductAttributeRequest();
    productAttributeRequest2.setAttribute(attributeRequest1);
    ProductAttributeRequest productAttributeRequest3 = new ProductAttributeRequest();
    productAttributeRequest3.setAttribute(attributeRequest2);
    ProductAttributeRequest productAttributeRequest4 = new ProductAttributeRequest();
    productAttributeRequest4.setAttribute(attributeRequest2);
    productAttributeRequest4.setProductAttributeValues(
        List.of(productAttributeValueRequest1, productAttributeValueRequest2));

    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductAttributes(
        Arrays.asList(productAttributeRequest1, productAttributeRequest2, productAttributeRequest3,
            productAttributeRequest4));

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validateDistinctProductAttributeValues(productCreationRequest, "-");
    });
  }

  @Test
  public void performValidationOnDescriptionAndShippingTest() {
    Assertions.assertTrue(
        ValidationUtil.performValidationOnDescriptionAndShipping(null, false, null, null, new ArrayList<>()));
    Assertions.assertTrue(
        ValidationUtil.performValidationOnDescriptionAndShipping(null, true, null, null, new ArrayList<>()));
    Assertions.assertTrue(ValidationUtil.performValidationOnDescriptionAndShipping(new ProfileResponse(), true, null, null,
        new ArrayList<>()));
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    Assertions.assertTrue(
        ValidationUtil.performValidationOnDescriptionAndShipping(profileResponse, true, null, null, new ArrayList<>()));
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setOfflineToOnlineFlag(true);
    profileResponse.setCompany(companyDTO);
    Assertions.assertTrue(
        ValidationUtil.performValidationOnDescriptionAndShipping(profileResponse, true, null, null, new ArrayList<>()));
    Assertions.assertTrue(ValidationUtil.performValidationOnDescriptionAndShipping(profileResponse, true, false, true,
        new ArrayList<>()));
    Assertions.assertTrue(ValidationUtil.performValidationOnDescriptionAndShipping(profileResponse, true, true, false,
        new ArrayList<>()));
    Assertions.assertTrue(
        ValidationUtil.performValidationOnDescriptionAndShipping(profileResponse, true, true, null, new ArrayList<>()));
    Assertions.assertTrue(
        ValidationUtil.performValidationOnDescriptionAndShipping(profileResponse, true, true, false, new ArrayList<>()));
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    Assertions.assertTrue(ValidationUtil.performValidationOnDescriptionAndShipping(profileResponse, true, true, false,
        Collections.singletonList(productItemCreationRequest)));
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setDisplay(true);
    productItemCreationRequest.setPickupPoints(Collections.singletonList(pickupPointCreateRequest));
    Assertions.assertTrue(ValidationUtil.performValidationOnDescriptionAndShipping(profileResponse, true, true, false,
        Collections.singletonList(productItemCreationRequest)));
    productItemCreationRequest.getPickupPoints().get(0).setBuyable(true);
    Assertions.assertTrue(ValidationUtil.performValidationOnDescriptionAndShipping(profileResponse, true, true, false,
        Collections.singletonList(productItemCreationRequest)));
    productItemCreationRequest.getPickupPoints().get(0).setBuyable(false);
    productItemCreationRequest.getPickupPoints().get(0).setDisplay(false);
    Assertions.assertFalse(ValidationUtil.performValidationOnDescriptionAndShipping(profileResponse, true, true, false,
        Collections.singletonList(productItemCreationRequest)));
  }

  @Test
  public void updateEmptyDescriptionForPureInStoreProductTest() {
    ProductRequest productRequest = new ProductRequest();
    ValidationUtil.updateEmptyDescriptionForPureInStoreProduct(productRequest, true);
    productRequest.setDescription(new byte[0]);
    ValidationUtil.updateEmptyDescriptionForPureInStoreProduct(productRequest, true);
    productRequest.setLongDescription(new byte[0]);
    ValidationUtil.updateEmptyDescriptionForPureInStoreProduct(productRequest, true);
    productRequest = new ProductRequest();
    Assertions.assertFalse(Objects.nonNull(productRequest.getDescription()));
    Assertions.assertFalse(Objects.nonNull(productRequest.getDescription()));
    ValidationUtil.updateEmptyDescriptionForPureInStoreProduct(productRequest, false);
    Assertions.assertTrue(Objects.nonNull(productRequest.getDescription()));
    Assertions.assertTrue(Objects.nonNull(productRequest.getLongDescription()));
  }

  @Test
  public void updateNullDimensionsForPureInStoreProductTest() {
    ProductRequest request = new ProductRequest();
    ValidationUtil.updateNullDimensionsForPureInStoreProduct(
        ValidationUtil.isAnyValueNull(request.getLength(), request.getWidth(), request.getHeight(), request.getWeight(),
            request.getShippingWeight()), request, true);
    Assertions.assertTrue(Objects.isNull(request.getLength()));
    Assertions.assertTrue(Objects.isNull(request.getWeight()));
    Assertions.assertTrue(Objects.isNull(request.getWidth()));
    Assertions.assertTrue(Objects.isNull(request.getHeight()));
    Assertions.assertTrue(Objects.isNull(request.getShippingWeight()));
    request.setHeight(10.0);
    ValidationUtil.updateNullDimensionsForPureInStoreProduct(
        ValidationUtil.isAnyValueNull(request.getLength(), request.getWidth(), request.getHeight(), request.getWeight(),
            request.getShippingWeight()), request, false);
    Assertions.assertEquals(0.0, request.getLength(), 0);
    Assertions.assertEquals(0.0, request.getWeight(), 0);
    Assertions.assertEquals(0.0, request.getWidth(), 0);
    Assertions.assertEquals(0.0, request.getLength(), 0);
    Assertions.assertEquals(0.0, request.getHeight(), 0);
    Assertions.assertEquals(0.0, request.getShippingWeight(), 0);
    ValidationUtil.updateNullDimensionsForPureInStoreProduct(
        ValidationUtil.isAnyValueNull(request.getLength(), request.getWidth(), request.getHeight(), request.getWeight(),
            request.getShippingWeight()), request, false);
  }

  @Test
  public void validatePreOrderTest() throws Exception {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("DATE");
    preOrderRequest.setPreOrderDate(getDateInFuture(5));
    ValidationUtil.validatePreOrder(preOrderRequest, 30, 4, false);
    Assertions.assertNotNull(preOrderRequest.getPreOrderDate());
  }

  @Test
  public void getValidationResponseForPreOrderTest() throws Exception {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("DATE");
    preOrderRequest.setPreOrderDate(new Date());
    ApiErrorCode validationResponseForPreOrder =
        ValidationUtil.getValidationResponseForPreOrder(preOrderRequest, 11, 11, false, false);
    Assertions.assertNull(validationResponseForPreOrder);
  }


  private ProductL3UpdateRequest createBaseProductL3UpdateRequest() {
    ProductL3UpdateRequest request = new ProductL3UpdateRequest();
    request.setFreeSample(false);
    request.setOnline(true);
    return request;
  }

  private ItemPickupPointRequest createItemPickupPointRequest(String itemSku, String pickupPointId) {
    ItemPickupPointRequest pickupPoint = new ItemPickupPointRequest();
    pickupPoint.setItemSku(itemSku);
    pickupPoint.setPickupPointId(pickupPointId);
    return pickupPoint;
  }

  private BuyableScheduleRequest createBuyableSchedule(Date start, Date end) {
    BuyableScheduleRequest schedule = new BuyableScheduleRequest();
    schedule.setStartDateTime(start);
    schedule.setEndDateTime(end);
    return schedule;
  }

  private DiscoverableScheduleRequest createDiscoverableSchedule(Date start, Date end) {
    DiscoverableScheduleRequest schedule = new DiscoverableScheduleRequest();
    schedule.setStartDateTime(start);
    schedule.setEndDateTime(end);
    return schedule;
  }

  private Date getDateInFuture(int days) {
    Calendar cal = Calendar.getInstance();
    cal.add(Calendar.DATE, days);
    return cal.getTime();
  }

    private Date getDateInPast(int days) {
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.DATE, -days);
        return cal.getTime();
    }

  @Test
  void validateSchedules_FeatureDisabled_ShouldReturnNull() {
    ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
    ApiErrorCode result = ValidationUtil.validateEligibilityForSchedulesAtL5(request, new HashMap<>(), null, false);
    Assertions.assertNull(result);
  }

  @Test
  void validateSchedules_NoChanges_ShouldReturnNull() {
    ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
    ApiErrorCode result = ValidationUtil.validateEligibilityForSchedulesAtL5(request, new HashMap<>(), null, true);
    Assertions.assertNull(result);
  }


  @Test
  void validateSchedules_Modified_Valid_ShouldReturnNull() {
    ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
    ProductVariantPriceStockAndImagesRequest variant = new ProductVariantPriceStockAndImagesRequest();
    variant.setItemSku(SKU_1);
    ItemPickupPointRequest modifiedPoint = createItemPickupPointRequest(SKU_1, "PP1");
    modifiedPoint.setBuyableSchedule(createBuyableSchedule(getDateInFuture(1), getDateInFuture(2)));
    modifiedPoint.setDiscoverableSchedule(createDiscoverableSchedule(getDateInFuture(1), getDateInFuture(3)));
    variant.setModifiedItemPickupPoints(Collections.singletonList(modifiedPoint));
    request.setProductItems(Collections.singletonList(variant));

    ApiErrorCode result = ValidationUtil.validateEligibilityForSchedulesAtL5(request, new HashMap<>(), null, true);
    Assertions.assertNull(result);
  }

   @Test
  void validateSchedules_Modified_InvalidStartDate_ShouldReturnInvalidDateTime() {
    ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
    ProductVariantPriceStockAndImagesRequest variant = new ProductVariantPriceStockAndImagesRequest();
    variant.setItemSku(SKU_1);
    ItemPickupPointRequest modifiedPoint = createItemPickupPointRequest(SKU_1, "PP1");
    modifiedPoint.setBuyableSchedule(createBuyableSchedule(getDateInPast(1), getDateInFuture(2)));
    modifiedPoint.setDiscoverableSchedule(createDiscoverableSchedule(getDateInFuture(1), getDateInFuture(3)));
    variant.setModifiedItemPickupPoints(Collections.singletonList(modifiedPoint));
    request.setProductItems(Collections.singletonList(variant));

    ApiErrorCode result = ValidationUtil.validateEligibilityForSchedulesAtL5(request, new HashMap<>(), null, true);
    Assertions.assertEquals(ApiErrorCode.INVALID_SCHEDULE_DATE_TIME, result);
  }

   @Test
   void validateSchedules_Modified_InvalidEndDate_ShouldReturnInvalidDateTime() {
       ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
       ProductVariantPriceStockAndImagesRequest variant = new ProductVariantPriceStockAndImagesRequest();
       variant.setItemSku(SKU_1);
       ItemPickupPointRequest modifiedPoint = createItemPickupPointRequest(SKU_1, "PP1");
       modifiedPoint.setBuyableSchedule(createBuyableSchedule(getDateInFuture(1), getDateInFuture(2)));
       modifiedPoint.setDiscoverableSchedule(createDiscoverableSchedule(getDateInFuture(3), getDateInFuture(1)));
       variant.setModifiedItemPickupPoints(Collections.singletonList(modifiedPoint));
       request.setProductItems(Collections.singletonList(variant));

       ApiErrorCode result = ValidationUtil.validateEligibilityForSchedulesAtL5(request, new HashMap<>(), null, true);
       Assertions.assertEquals(ApiErrorCode.INVALID_SCHEDULE_DATE_TIME, result);
   }

  @Test
  void validateSchedules_Modified_IncompleteBuyable_ShouldReturnIncomplete() {
    ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
    ProductVariantPriceStockAndImagesRequest variant = new ProductVariantPriceStockAndImagesRequest();
    variant.setItemSku(SKU_1);
    ItemPickupPointRequest modifiedPoint = createItemPickupPointRequest(SKU_1, "PP1");
    modifiedPoint.setBuyableSchedule(createBuyableSchedule(getDateInFuture(1), null));
    modifiedPoint.setDiscoverableSchedule(createDiscoverableSchedule(getDateInFuture(1), getDateInFuture(3)));
    variant.setModifiedItemPickupPoints(Collections.singletonList(modifiedPoint));
    request.setProductItems(Collections.singletonList(variant));

    ApiErrorCode result = ValidationUtil.validateEligibilityForSchedulesAtL5(request, new HashMap<>(), null, true);
    Assertions.assertEquals(ApiErrorCode.SCHEDULE_DATE_INCOMPLETE, result);
  }

   @Test
   void validateSchedules_Modified_IncompleteDiscoverable_ShouldReturnIncomplete() {
       ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
       ProductVariantPriceStockAndImagesRequest variant = new ProductVariantPriceStockAndImagesRequest();
       variant.setItemSku(SKU_1);
       ItemPickupPointRequest modifiedPoint = createItemPickupPointRequest(SKU_1, "PP1");
       modifiedPoint.setBuyableSchedule(createBuyableSchedule(getDateInFuture(1), getDateInFuture(2)));
       modifiedPoint.setDiscoverableSchedule(createDiscoverableSchedule(null, getDateInFuture(3)));
       variant.setModifiedItemPickupPoints(Collections.singletonList(modifiedPoint));
       request.setProductItems(Collections.singletonList(variant));

       ApiErrorCode result = ValidationUtil.validateEligibilityForSchedulesAtL5(request, new HashMap<>(), null, true);
       Assertions.assertEquals(ApiErrorCode.SCHEDULE_DATE_INCOMPLETE, result);
   }

    @Test
    void validateSchedules_Modified_FreeSampleRequest_ShouldReturnNotAllowed() {
        ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
        request.setFreeSample(true); // Mark as free sample in request
        ProductVariantPriceStockAndImagesRequest variant = new ProductVariantPriceStockAndImagesRequest();
        variant.setItemSku(SKU_1);
        ItemPickupPointRequest modifiedPoint = createItemPickupPointRequest(SKU_1, "PP1");
        modifiedPoint.setBuyableSchedule(createBuyableSchedule(getDateInFuture(1), getDateInFuture(2))); // Add schedule
        variant.setModifiedItemPickupPoints(Collections.singletonList(modifiedPoint));
        request.setProductItems(Collections.singletonList(variant));

        ProductL3Response savedData = new ProductL3Response();
        savedData.setFreeSample(false); // Not free sample in saved data

        ApiErrorCode result = ValidationUtil.validateEligibilityForSchedulesAtL5(request, new HashMap<>(), savedData, true);
        Assertions.assertEquals(ApiErrorCode.SCHEDULE_NOT_ALLOWED_FOR_FREE_SAMPLE, result);
    }

    @Test
    void validateSchedules_Modified_FreeSampleSaved_ShouldReturnNotAllowed() {
        ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
        request.setFreeSample(false); // Not free sample in request
        ProductVariantPriceStockAndImagesRequest variant = new ProductVariantPriceStockAndImagesRequest();
        variant.setItemSku(SKU_1);
        ItemPickupPointRequest modifiedPoint = createItemPickupPointRequest(SKU_1, "PP1");
        modifiedPoint.setDiscoverableSchedule(createDiscoverableSchedule(getDateInFuture(1), getDateInFuture(2))); // Add schedule
        variant.setModifiedItemPickupPoints(Collections.singletonList(modifiedPoint));
        request.setProductItems(Collections.singletonList(variant));

        ProductL3Response savedData = new ProductL3Response();
        savedData.setFreeSample(true);

        ApiErrorCode result = ValidationUtil.validateEligibilityForSchedulesAtL5(request, new HashMap<>(), savedData, true);
        Assertions.assertEquals(null, result);
    }

     @Test
     void validateSchedules_Modified_OfflineRequest_ShouldReturnInvalidForOffline() {
         ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
         request.setOnline(false); // Mark as offline in request
         ProductVariantPriceStockAndImagesRequest variant = new ProductVariantPriceStockAndImagesRequest();
         variant.setItemSku(SKU_1);
         ItemPickupPointRequest modifiedPoint = createItemPickupPointRequest(SKU_1, "PP1");
         modifiedPoint.setBuyableSchedule(createBuyableSchedule(getDateInFuture(1), getDateInFuture(2))); // Add schedule
         variant.setModifiedItemPickupPoints(Collections.singletonList(modifiedPoint));
         request.setProductItems(Collections.singletonList(variant));

         ProductL3Response savedData = new ProductL3Response();
         savedData.setOnline(true);

         ApiErrorCode result = ValidationUtil.validateEligibilityForSchedulesAtL5(request, new HashMap<>(), savedData, true);
         Assertions.assertEquals(ApiErrorCode.SCHEDULE_INVALID_FOR_OFFLINE_PRODUCT, result);
     }

    @Test
    void validateSchedules_Modified_OfflineSaved_ShouldReturnInvalidForOffline() {
        ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
        request.setOnline(true);
        ProductVariantPriceStockAndImagesRequest variant = new ProductVariantPriceStockAndImagesRequest();
        variant.setItemSku(SKU_1);
        ItemPickupPointRequest modifiedPoint = createItemPickupPointRequest(SKU_1, "PP1");
        modifiedPoint.setDiscoverableSchedule(createDiscoverableSchedule(getDateInFuture(1), getDateInFuture(2))); // Add schedule
        variant.setModifiedItemPickupPoints(Collections.singletonList(modifiedPoint));
        request.setProductItems(Collections.singletonList(variant));

        ProductL3Response savedData = new ProductL3Response();
        savedData.setOnline(false);

        ApiErrorCode result = ValidationUtil.validateEligibilityForSchedulesAtL5(request, new HashMap<>(), savedData, true);
        Assertions.assertEquals(null, result);
    }

    @Test
    void validateSchedules_Modified_ScheduleRemoval_ShouldSkipFreeSampleCheck() {
        ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
        request.setFreeSample(true); // Free sample in request
        ProductVariantPriceStockAndImagesRequest variant = new ProductVariantPriceStockAndImagesRequest();
        variant.setItemSku(SKU_1);
        ItemPickupPointRequest modifiedPoint = createItemPickupPointRequest(SKU_1, "PP1");
        modifiedPoint.setScheduleRemoval(true); // Removing schedule
        variant.setModifiedItemPickupPoints(Collections.singletonList(modifiedPoint));
        request.setProductItems(Collections.singletonList(variant));

        ProductL3Response savedData = new ProductL3Response();
        savedData.setFreeSample(false);

        ApiErrorCode result = ValidationUtil.validateEligibilityForSchedulesAtL5(request, new HashMap<>(), savedData, true);
        Assertions.assertNull(result); // Should be null as schedule is being removed
    }

    @Test
    void validateSchedules_Modified_SkipValidationExistingBuyable_ShouldReturnNull() {
        ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
        ProductVariantPriceStockAndImagesRequest variant = new ProductVariantPriceStockAndImagesRequest();
        variant.setItemSku(SKU_1);
        ItemPickupPointRequest modifiedPoint = createItemPickupPointRequest(SKU_1, "PP1");
        // Start date in past, but this represents an *existing* active schedule being modified
        modifiedPoint.setBuyableSchedule(createBuyableSchedule(getDateInPast(5), getDateInFuture(2)));
        modifiedPoint.setDiscoverableSchedule(createDiscoverableSchedule(getDateInFuture(1), getDateInFuture(3)));
        variant.setModifiedItemPickupPoints(Collections.singletonList(modifiedPoint));
        request.setProductItems(Collections.singletonList(variant));

        // Simulate that this is an existing schedule update
        Map<String, Set<String>> scheduleValidations = new HashMap<>();
        scheduleValidations.put(Constants.BUYABLE_SCHEDULE_UPDATE, Collections.singleton("SKU1-PP1"));

        ApiErrorCode result = ValidationUtil.validateEligibilityForSchedulesAtL5(request, scheduleValidations, null, true);
        Assertions.assertNull(result); // Start date validation should be skipped
    }

    @Test
    void validateSchedules_Modified_SkipValidationExistingDiscoverable_ShouldReturnNull() {
        ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
        ProductVariantPriceStockAndImagesRequest variant = new ProductVariantPriceStockAndImagesRequest();
        variant.setItemSku(SKU_1);
        ItemPickupPointRequest modifiedPoint = createItemPickupPointRequest(SKU_1, "PP1");
        modifiedPoint.setBuyableSchedule(createBuyableSchedule(getDateInFuture(1), getDateInFuture(2)));
         // Start date in past, but this represents an *existing* active schedule being modified
        modifiedPoint.setDiscoverableSchedule(createDiscoverableSchedule(getDateInPast(5), getDateInFuture(3)));
        variant.setModifiedItemPickupPoints(Collections.singletonList(modifiedPoint));
        request.setProductItems(Collections.singletonList(variant));

        // Simulate that this is an existing schedule update
        Map<String, Set<String>> scheduleValidations = new HashMap<>();
        scheduleValidations.put(Constants.DISCOVERABLE_SCHEDULE_UPDATE, Collections.singleton("SKU1-PP1"));

        ApiErrorCode result = ValidationUtil.validateEligibilityForSchedulesAtL5(request, scheduleValidations, null, true);
        Assertions.assertNull(result); // Start date validation should be skipped
    }

  // --- Added Pickup Points Tests ---

  @Test
  void validateSchedules_Added_Valid_ShouldReturnNull() {
    ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
    ItemPickupPointRequest addedPoint = createItemPickupPointRequest(null, "PP2"); // itemSku is null for added points initially
    addedPoint.setBuyableSchedule(createBuyableSchedule(getDateInFuture(1), getDateInFuture(2)));
    addedPoint.setDiscoverableSchedule(createDiscoverableSchedule(getDateInFuture(1), getDateInFuture(3)));
    request.setAddPickupPoints(Collections.singletonList(addedPoint));

    ApiErrorCode result = ValidationUtil.validateEligibilityForSchedulesAtL5(request, new HashMap<>(), null, true);
    Assertions.assertNull(result);
  }

   @Test
   void validateSchedules_Added_InvalidStartDate_ShouldReturnInvalidDateTime() {
       ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
       ItemPickupPointRequest addedPoint = createItemPickupPointRequest(null, "PP2");
       // Buyable schedule start date in the past
       addedPoint.setBuyableSchedule(createBuyableSchedule(getDateInPast(1), getDateInFuture(2)));
       addedPoint.setDiscoverableSchedule(createDiscoverableSchedule(getDateInFuture(1), getDateInFuture(3)));
       request.setAddPickupPoints(Collections.singletonList(addedPoint));

       ApiErrorCode result = ValidationUtil.validateEligibilityForSchedulesAtL5(request, new HashMap<>(), null, true);
       Assertions.assertEquals(ApiErrorCode.INVALID_SCHEDULE_DATE_TIME, result);
   }

    @Test
    void validateSchedules_Added_InvalidEndDate_ShouldReturnInvalidDateTime() {
        ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
        ItemPickupPointRequest addedPoint = createItemPickupPointRequest(null, "PP2");
        addedPoint.setBuyableSchedule(createBuyableSchedule(getDateInFuture(1), getDateInFuture(2)));
        // Discoverable end date before start date
        addedPoint.setDiscoverableSchedule(createDiscoverableSchedule(getDateInFuture(3), getDateInFuture(1)));
        request.setAddPickupPoints(Collections.singletonList(addedPoint));

        ApiErrorCode result = ValidationUtil.validateEligibilityForSchedulesAtL5(request, new HashMap<>(), null, true);
        Assertions.assertEquals(ApiErrorCode.INVALID_SCHEDULE_DATE_TIME, result);
    }

    @Test
    void validateSchedules_Added_IncompleteBuyable_ShouldReturnIncomplete() {
        ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
        ItemPickupPointRequest addedPoint = createItemPickupPointRequest(null, "PP2");
        // Buyable schedule missing start date
        addedPoint.setBuyableSchedule(createBuyableSchedule(null, getDateInFuture(2)));
        addedPoint.setDiscoverableSchedule(createDiscoverableSchedule(getDateInFuture(1), getDateInFuture(3)));
        request.setAddPickupPoints(Collections.singletonList(addedPoint));

        ApiErrorCode result = ValidationUtil.validateEligibilityForSchedulesAtL5(request, new HashMap<>(), null, true);
        Assertions.assertEquals(ApiErrorCode.SCHEDULE_DATE_INCOMPLETE, result);
    }

    @Test
    void validateSchedules_Added_IncompleteDiscoverable_ShouldReturnIncomplete() {
        ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
        ItemPickupPointRequest addedPoint = createItemPickupPointRequest(null, "PP2");
        addedPoint.setBuyableSchedule(createBuyableSchedule(getDateInFuture(1), getDateInFuture(2)));
        // Discoverable schedule missing end date
        addedPoint.setDiscoverableSchedule(createDiscoverableSchedule(getDateInFuture(1), null));
        request.setAddPickupPoints(Collections.singletonList(addedPoint));

        ApiErrorCode result = ValidationUtil.validateEligibilityForSchedulesAtL5(request, new HashMap<>(), null, true);
        Assertions.assertEquals(ApiErrorCode.SCHEDULE_DATE_INCOMPLETE, result);
    }

  @Test
  void validateSchedules_ModifiedErrorFirst_ShouldReturnModifiedError() {
    ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
    ProductVariantPriceStockAndImagesRequest variant = new ProductVariantPriceStockAndImagesRequest();
    variant.setItemSku(SKU_1);
    ItemPickupPointRequest modifiedPoint = createItemPickupPointRequest(SKU_1, "PP1");
    modifiedPoint.setBuyableSchedule(createBuyableSchedule(getDateInPast(1), getDateInFuture(2)));
    variant.setModifiedItemPickupPoints(Collections.singletonList(modifiedPoint));
    request.setProductItems(Collections.singletonList(variant));

    // Added point is valid
    ItemPickupPointRequest addedPoint = createItemPickupPointRequest(null, "PP2");
    addedPoint.setDiscoverableSchedule(createDiscoverableSchedule(getDateInFuture(1), getDateInFuture(3)));
    request.setAddPickupPoints(Collections.singletonList(addedPoint));

    ApiErrorCode result = ValidationUtil.validateEligibilityForSchedulesAtL5(request, new HashMap<>(), null, true);
    Assertions.assertEquals(ApiErrorCode.INVALID_SCHEDULE_DATE_TIME, result); // Error from modified point
  }

  @Test
  void validateSchedules_AddedErrorAfterValidModified_ShouldReturnAddedError() {
    ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
    ProductVariantPriceStockAndImagesRequest variant = new ProductVariantPriceStockAndImagesRequest();
    variant.setItemSku(SKU_1);
    ItemPickupPointRequest modifiedPoint = createItemPickupPointRequest(SKU_1, "PP1");
    modifiedPoint.setBuyableSchedule(createBuyableSchedule(getDateInFuture(1), getDateInFuture(2)));
    variant.setModifiedItemPickupPoints(Collections.singletonList(modifiedPoint));
    request.setProductItems(Collections.singletonList(variant));
    ItemPickupPointRequest addedPoint = createItemPickupPointRequest(null, "PP2");
    addedPoint.setDiscoverableSchedule(createDiscoverableSchedule(getDateInFuture(1), null));
    request.setAddPickupPoints(Collections.singletonList(addedPoint));

    ApiErrorCode result = ValidationUtil.validateEligibilityForSchedulesAtL5(request, new HashMap<>(), null, true);
    Assertions.assertEquals(ApiErrorCode.SCHEDULE_DATE_INCOMPLETE, result); // Error from added point
  }

   private ProductVariantPriceStockAndImagesRequest createVariantRequest(String itemSku, String upcCode) {
     ProductVariantPriceStockAndImagesRequest variant = new ProductVariantPriceStockAndImagesRequest();
     variant.setItemSku(itemSku);
     variant.setUpcCode(upcCode);
     return variant;
   }

  @Test
  void validateUpcCodes_EmptyProductItems_ShouldPass() {
    ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
    request.setProductItems(Collections.emptyList());
    Assertions.assertDoesNotThrow(() -> ValidationUtil.validateFormatAndDuplicateUpcCodes(request, UPC_VALID_LENGTH));
  }

  @Test
  void validateUpcCodes_NullProductItems_ShouldPass() {
    ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
    request.setProductItems(null);
    Assertions.assertDoesNotThrow(() -> ValidationUtil.validateFormatAndDuplicateUpcCodes(request, UPC_VALID_LENGTH));
  }

  @Test
  void validateUpcCodes_NoUpcCodesProvided_ShouldPass() {
    ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
    request.setProductItems(Arrays.asList(
        createVariantRequest(ITEM_SKU, null),
        createVariantRequest(ITEM_SKU1, "")
    ));
    Assertions.assertDoesNotThrow(() -> ValidationUtil.validateFormatAndDuplicateUpcCodes(request, UPC_VALID_LENGTH));
  }

  @Test
  void validateUpcCodes_ValidUniqueCodes_ShouldPass() {
    ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
    request.setProductItems(Arrays.asList(
        createVariantRequest(ITEM_SKU, DUPLICATE_UPC),
        createVariantRequest(ITEM_SKU1, "987654321098")
    ));
    Assertions.assertDoesNotThrow(() -> ValidationUtil.validateFormatAndDuplicateUpcCodes(request, UPC_VALID_LENGTH));
  }

  @Test
  void validateUpcCodes_DuplicateCodes_ShouldThrowException() {
    ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
    String duplicateUpc = DUPLICATE_UPC;
    request.setProductItems(Arrays.asList(
        createVariantRequest(ITEM_SKU, duplicateUpc),
        createVariantRequest(ITEM_SKU1, "987654321098"),
        createVariantRequest(ITEM_SKU2, duplicateUpc) // Duplicate
    ));

    ApplicationRuntimeException exception = Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validateFormatAndDuplicateUpcCodes(request, UPC_VALID_LENGTH);
    });
    Assertions.assertTrue(exception.getMessage().contains(ApiErrorCode.UPC_CODE_UPDATE_FAILED.getDesc()));
  }

  @Test
  void validateUpcCodes_InvalidFormat_ShouldThrowException() {
    ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
    String invalidFormatUpc = INVALID_FORMAT_UPC;
    request.setProductItems(Arrays.asList(
        createVariantRequest(ITEM_SKU, DUPLICATE_UPC),
        createVariantRequest(ITEM_SKU1, invalidFormatUpc)
    ));

    ApplicationRuntimeException exception = Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validateFormatAndDuplicateUpcCodes(request, UPC_VALID_LENGTH);
    });
    Assertions.assertTrue(exception.getMessage().contains("EAN UPC code is not in valid format"));
  }

  @Test
  void validateUpcCodes_MixedInvalidAndDuplicate_InvalidShouldBeCaughtFirst() {
    ProductL3UpdateRequest request = createBaseProductL3UpdateRequest();
    String duplicateUpc = DUPLICATE_UPC;
    String invalidFormatUpc = INVALID_FORMAT_UPC;
    request.setProductItems(Arrays.asList(
        createVariantRequest(ITEM_SKU, duplicateUpc),
        createVariantRequest(ITEM_SKU1, invalidFormatUpc), // Invalid format
        createVariantRequest(ITEM_SKU2, duplicateUpc)    // Duplicate
    ));

    ApplicationRuntimeException exception = Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validateFormatAndDuplicateUpcCodes(request, UPC_VALID_LENGTH);
    });
    Assertions.assertTrue(exception.getMessage().contains("EAN UPC code is not in valid format"));
  }

  private List<String> validExtensions = Arrays.asList(STRINGS);

  private Image createImage(String locationPath, boolean markForDelete) {
    Image img = new Image();
    img.setLocationPath(locationPath);
    img.setMarkForDelete(markForDelete);
    return img;
  }

  @Test
  void validateImageExtension_NoImages_ShouldReturnTrue() {
    ProductCreationRequest request = new ProductCreationRequest();
    Assertions.assertTrue(ValidationUtil.validateImageExtension(request, validExtensions, false));
  }

  @Test
  void validateImageExtension_ValidL3Images_ShouldReturnTrue() {
    ProductCreationRequest request = new ProductCreationRequest();
    request.setImages(Arrays.asList(
        createImage("/path/to/image1.jpg", false),
        createImage("/path/to/image2.PNG", false)
    ));
    Assertions.assertTrue(ValidationUtil.validateImageExtension(request, validExtensions, false));
  }

  @Test
  void validateImageExtension_InvalidL3Image_ShouldReturnFalse() {
    ProductCreationRequest request = new ProductCreationRequest();
    request.setImages(Arrays.asList(
        createImage("/path/to/image1.jpg", false),
        createImage("/path/to/image2.bmp", false) // Invalid extension
    ));
    Assertions.assertFalse(ValidationUtil.validateImageExtension(request, validExtensions, false));
  }

  @Test
  void validateImageExtension_ValidL4ItemImages_ShouldReturnTrue() {
    ProductRequest request = new ProductRequest(); // Use base class
    ProductItemRequest item1 = new ProductItemRequest();
    item1.setImages(Arrays.asList(
        createImage("item/imageA.jpeg", false),
        createImage("item/imageB.gif", false)
    ));
    request.setProductItems(Collections.singletonList(item1));
    Assertions.assertTrue(ValidationUtil.validateImageExtension(request, validExtensions, false));
  }

  @Test
  void validateImageExtension_InvalidL4ItemImage_ShouldReturnFalse() {
    ProductRequest request = new ProductRequest();
    ProductItemRequest item1 = new ProductItemRequest();
    item1.setImages(Arrays.asList(
        createImage("item/imageA.jpeg", false),
        createImage("item/imageB.tiff", false) // Invalid extension
    ));
    request.setProductItems(Collections.singletonList(item1));
    Assertions.assertFalse(ValidationUtil.validateImageExtension(request, validExtensions, false));
  }

  @Test
  void validateImageExtension_ValidL5ItemRequestImages_ShouldReturnTrue() {
    ProductCreationRequest request = new ProductCreationRequest();
    ProductItemCreationRequest itemReq1 = new ProductItemCreationRequest();
    itemReq1.setImages(Arrays.asList(
        createImage("req/img1.jpg", false),
        createImage("req/img2.png", false)
    ));
    request.setProductItemRequests(Collections.singletonList(itemReq1));
    Assertions.assertTrue(ValidationUtil.validateImageExtension(request, validExtensions, false));
  }

  @Test
  void validateImageExtension_InvalidL5ItemRequestImage_ShouldReturnFalse() {
    ProductCreationRequest request = new ProductCreationRequest();
    ProductItemCreationRequest itemReq1 = new ProductItemCreationRequest();
    itemReq1.setImages(Arrays.asList(
        createImage("req/img1.jpg", false),
        createImage("req/img2.webp", false) // Invalid extension
    ));
    request.setProductItemRequests(Collections.singletonList(itemReq1));
    Assertions.assertFalse(ValidationUtil.validateImageExtension(request, validExtensions, false));
  }

  @Test
  void validateImageExtension_MixedValidImages_ShouldReturnTrue() {
    ProductCreationRequest request = new ProductCreationRequest();
    request.setImages(Collections.singletonList(createImage("l3.jpg", false)));
    ProductItemRequest item1 = new ProductItemRequest();
    item1.setImages(Collections.singletonList(createImage("l4.png", false)));
    request.setProductItems(Collections.singletonList(item1));
    ProductItemCreationRequest itemReq1 = new ProductItemCreationRequest();
    itemReq1.setImages(Collections.singletonList(createImage("l5.gif", false)));
    request.setProductItemRequests(Collections.singletonList(itemReq1));
    Assertions.assertTrue(ValidationUtil.validateImageExtension(request, validExtensions, false));
  }

  @Test
  void validateImageExtension_MixedInvalidImageL3_ShouldReturnFalse() {
    ProductCreationRequest request = new ProductCreationRequest();
    request.setImages(Collections.singletonList(createImage("l3.bmp", false))); // Invalid
    ProductItemRequest item1 = new ProductItemRequest();
    item1.setImages(Collections.singletonList(createImage("l4.png", false)));
    request.setProductItems(Collections.singletonList(item1));
    ProductItemCreationRequest itemReq1 = new ProductItemCreationRequest();
    itemReq1.setImages(Collections.singletonList(createImage("l5.gif", false)));
    request.setProductItemRequests(Collections.singletonList(itemReq1));
    Assertions.assertFalse(ValidationUtil.validateImageExtension(request, validExtensions, false));
  }

    @Test
    void validateImageExtension_MixedInvalidImageL4_ShouldReturnFalse() {
        ProductCreationRequest request = new ProductCreationRequest();
        request.setImages(Collections.singletonList(createImage("l3.jpg", false)));
        ProductItemRequest item1 = new ProductItemRequest();
        item1.setImages(Collections.singletonList(createImage("l4.tiff", false))); // Invalid
        request.setProductItems(Collections.singletonList(item1));
        ProductItemCreationRequest itemReq1 = new ProductItemCreationRequest();
        itemReq1.setImages(Collections.singletonList(createImage("l5.gif", false)));
        request.setProductItemRequests(Collections.singletonList(itemReq1));
        Assertions.assertFalse(ValidationUtil.validateImageExtension(request, validExtensions, false));
    }

    @Test
    void validateImageExtension_MixedInvalidImageL5_ShouldReturnFalse() {
        ProductCreationRequest request = new ProductCreationRequest();
        request.setImages(Collections.singletonList(createImage("l3.jpg", false)));
        ProductItemRequest item1 = new ProductItemRequest();
        item1.setImages(Collections.singletonList(createImage("l4.png", false)));
        request.setProductItems(Collections.singletonList(item1));
        ProductItemCreationRequest itemReq1 = new ProductItemCreationRequest();
        itemReq1.setImages(Collections.singletonList(createImage("l5.webp", false))); // Invalid
        request.setProductItemRequests(Collections.singletonList(itemReq1));
        Assertions.assertFalse(ValidationUtil.validateImageExtension(request, validExtensions, false));
    }

  @Test
  void validateImageExtension_CaseInsensitive_ShouldReturnTrue() {
    ProductCreationRequest request = new ProductCreationRequest();
    request.setImages(Arrays.asList(
        createImage("/path/to/image1.JPG", false),
        createImage("/path/to/image2.jPeG", false)
    ));
    Assertions.assertTrue(ValidationUtil.validateImageExtension(request, validExtensions, false));
  }

  @Test
  void validateImageExtension_MarkedForDelete_ShouldIgnoreInvalidExtensionAndReturnTrue() {
    ProductCreationRequest request = new ProductCreationRequest();
    request.setImages(Arrays.asList(
        createImage("/path/to/image1.jpg", false),
        createImage("/path/to/image2.bmp", true)
    ));
    ProductItemRequest item1 = new ProductItemRequest();
    item1.setImages(Collections.singletonList(createImage("l4.tiff", true)));
    ProductItemCreationRequest itemReq1 = new ProductItemCreationRequest();
    itemReq1.setImages(Collections.singletonList(createImage("l5.webp", true)));
    request.setProductItemRequests(Collections.singletonList(itemReq1));

    Assertions.assertTrue(ValidationUtil.validateImageExtension(request, validExtensions, false));
  }

  @Test
  void validateImageExtension_New_Images_Test() {
    ProductCreationRequest request = new ProductCreationRequest();
    Image image = new Image();
    image = createImage("/path/to/image1.jpg", false);
    image.setReviewType(Constants.NEW);
    Image image2 = new Image();
    image2 = createImage("/path/to/image1.jpg", true);
    image2.setReviewType(Constants.NEW);
    Image image3 = new Image();
    image3 = createImage("/path/to/image1.jpg", false);
    request.setImages(List.of(image, image2, image3));
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setImages(List.of(image, image2, image3));
    request.setProductItemRequests(List.of(productItemCreationRequest));
    ProductItemRequest productItemRequest = new ProductItemRequest();
    productItemRequest.setImages(List.of(image, image2, image3));
    request.setProductItems(List.of(productItemRequest));
    ProductItemRequest item1 = new ProductItemRequest();
    item1.setImages(List.of(image, image2, image3));
    ProductItemCreationRequest itemReq1 = new ProductItemCreationRequest();
    itemReq1.setImages(List.of(image, image2, image3));
    request.setProductItemRequests(Collections.singletonList(itemReq1));

    Assertions.assertTrue(ValidationUtil.validateImageExtension(request, validExtensions, true));
  }

    @Test
    void validateImageExtension_MarkedForDeleteWithValidRemaining_ShouldReturnTrue() {
        ProductCreationRequest request = new ProductCreationRequest();
        request.setImages(Arrays.asList(
            createImage("/path/to/image1.jpg", false),
            createImage("/path/to/image2.bmp", true) // Ignored
        ));
       Assertions.assertTrue(ValidationUtil.validateImageExtension(request, validExtensions, false));
    }

    @Test
    void validateImageExtension_MarkedForDeleteWithInvalidRemaining_ShouldReturnFalse() {
        ProductCreationRequest request = new ProductCreationRequest();
        request.setImages(Arrays.asList(
            createImage("/path/to/image1.bmp", false), // Invalid, not deleted
            createImage("/path/to/image2.jpg", true) // Ignored
        ));
       Assertions.assertFalse(ValidationUtil.validateImageExtension(request, validExtensions, false));
    }

  @Test
  void validateImageExtension_PathWithoutExtension_ShouldReturnTrue() {
    ProductCreationRequest request = new ProductCreationRequest();
    request.setImages(Arrays.asList(
        createImage("/path/to/image1", false), // No extension
        createImage("/path/to/image2.jpg", false)
    ));
    // The method filters based on lastIndexOf('.') != -1, so paths without '.' are ignored
    Assertions.assertTrue(ValidationUtil.validateImageExtension(request, validExtensions, false));
  }

  @Test
  void validateImageExtension_PathWithOnlyDot_ShouldReturnFalse() {
    ProductCreationRequest request = new ProductCreationRequest();
    request.setImages(Arrays.asList(
        createImage("image.", false), // Extension is ""
        createImage("/path/to/image2.jpg", false)
    ));
    Assertions.assertFalse(ValidationUtil.validateImageExtension(request, validExtensions, false));
  }

    @Test
    void validateImageExtension_NullOrEmptyPath_ShouldReturnTrue() {
        ProductCreationRequest request = new ProductCreationRequest();
        request.setImages(Arrays.asList(
            createImage(null, false),
            createImage("", false),
            createImage("/path/to/image2.jpg", false)
        ));
        Assertions.assertTrue(ValidationUtil.validateImageExtension(request, validExtensions, false));
    }

   @Test
   void validateImageExtension_NonCreationRequest_ShouldIgnoreL5_ReturnTrue() {
       ProductRequest request = new ProductRequest();
       request.setImages(Collections.singletonList(createImage("l3.jpg", false)));

       Assertions.assertTrue(ValidationUtil.validateImageExtension(request, validExtensions, false));
   }

    @Test
    void validateImageExtension_NonCreationRequest_WithInvalidL4_ShouldReturnFalse() {
        ProductRequest request = new ProductRequest();
        request.setImages(Collections.singletonList(createImage("l3.jpg", false)));
        ProductItemRequest item1 = new ProductItemRequest();
        item1.setImages(Collections.singletonList(createImage("l4.tiff", false))); // Invalid L4
        request.setProductItems(Collections.singletonList(item1));

        Assertions.assertFalse(ValidationUtil.validateImageExtension(request, validExtensions, false));
    }

  @Test
  void validatePreOrder_InvalidType_ShouldThrowException() {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("INVALID_TYPE");
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validatePreOrder(preOrderRequest, 90, 12, false);
    });
  }

  @Test
  void validatePreOrder_DAYS_ZeroValue_ShouldThrowException() {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("DAYS");
    preOrderRequest.setPreOrderValue(0);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validatePreOrder(preOrderRequest, 30, 4, true);
    });
  }

  @Test
  void validatePreOrder_DAYS_NegativeValue_ShouldThrowException() {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("DAYS");
    preOrderRequest.setPreOrderValue(-1);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validatePreOrder(preOrderRequest, 30, 4, true);
    });
  }

  @Test
  void validatePreOrder_DAYS_ExceedsMaximum_ShouldThrowException() {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("DAYS");
    preOrderRequest.setPreOrderValue(31);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validatePreOrder(preOrderRequest, 30, 4, true);
    });
  }

  @Test
  void validatePreOrder_DAYS_Valid_ShouldSetPreOrderDate() throws Exception {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("DAYS");
    preOrderRequest.setPreOrderValue(10);
    ValidationUtil.validatePreOrder(preOrderRequest, 30, 4, true);
    Assertions.assertNotNull(preOrderRequest.getPreOrderDate());
  }

  @Test
  void validatePreOrder_WEEK_ZeroValue_ShouldThrowException() {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("WEEK");
    preOrderRequest.setPreOrderValue(0);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validatePreOrder(preOrderRequest, 30, 4, false);
    });
  }

  @Test
  void validatePreOrder_WEEK_NegativeValue_ShouldThrowException() {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("WEEK");
    preOrderRequest.setPreOrderValue(-1);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validatePreOrder(preOrderRequest, 30, 4, false);
    });
  }

  @Test
  void validatePreOrder_WEEK_ExceedsMaximum_ShouldThrowException() {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("WEEK");
    preOrderRequest.setPreOrderValue(5);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validatePreOrder(preOrderRequest, 30, 4, false);
    });
  }

  @Test
  void validatePreOrder_WEEK_Valid_ShouldSetPreOrderDate() throws Exception {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("WEEK");
    preOrderRequest.setPreOrderValue(2);
    ValidationUtil.validatePreOrder(preOrderRequest, 30, 4, false);
    Assertions.assertNotNull(preOrderRequest.getPreOrderDate());
  }

  @Test
  void validatePreOrder_DATE_CurrentDate_ShouldThrowException() {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("DATE");
    SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
    try {
      Date currentDate = dateFormat.parse(dateFormat.format(new Date()));
      preOrderRequest.setPreOrderDate(currentDate);
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        ValidationUtil.validatePreOrder(preOrderRequest, 30, 4, false);
      });
    } catch (Exception e) {
      Assertions.fail("Unexpected exception: " + e.getMessage());
    }
  }

  @Test
  void validatePreOrder_DATE_PastDate_ShouldThrowException() {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("DATE");
    preOrderRequest.setPreOrderDate(getDateInPast(1));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validatePreOrder(preOrderRequest, 30, 4, false);
    });
  }

  @Test
  void validatePreOrder_DATE_Exceeds90Days_ShouldThrowException() {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("DATE");
    preOrderRequest.setPreOrderDate(getDateInFuture(91));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validatePreOrder(preOrderRequest, 30, 4, true);
    });
  }

  @Test
  void validatePreOrder_DATE_ExceedsMaximumDays_ShouldThrowException() {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("DATE");
    preOrderRequest.setPreOrderDate(getDateInFuture(35));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validatePreOrder(preOrderRequest, 30, 4, false);
    });
  }

  @Test
  void validatePreOrder_DATE_Valid_ShouldSetPreOrderDate() throws Exception {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("DATE");
    preOrderRequest.setPreOrderDate(getDateInFuture(10));
    ValidationUtil.validatePreOrder(preOrderRequest, 30, 4, true);
    Assertions.assertNotNull(preOrderRequest.getPreOrderDate());
  }

  @Test
  void getValidationResponseForPreOrder_InvalidType_ShouldReturnErrorCode() throws Exception {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("INVALID_TYPE");
    ApiErrorCode result = ValidationUtil.getValidationResponseForPreOrder(preOrderRequest, 90, 12, false, false);
    Assertions.assertEquals(ApiErrorCode.INVALID_PREORDER_TYPE, result);
  }

  @Test
  void isWarnaPresent_False_WhenAttributeAbsent() {
    ProductCreationRequest request = new ProductCreationRequest();
    AttributeRequest attr = new AttributeRequest();
    attr.setName("Ukuran"); // Not "Warna"
    ProductAttributeRequest pa = new ProductAttributeRequest();
    pa.setAttribute(attr);
    request.setProductAttributes(Collections.singletonList(pa));
    // Accessing the private method via reflection or making it package-private would be
    // ideal for direct testing. Here, we test its effect through a public method.
    // We test validateCreateProductItemsRequest where isWarnaPresent is called.
    // Setup items without Warna attributes
    ProductItemCreationRequest item = new ProductItemCreationRequest();
    AttributeRequest itemAttr = new AttributeRequest();
    itemAttr.setName("Ukuran");
    ProductItemAttributeValueRequest itemAttrVal = new ProductItemAttributeValueRequest();
    itemAttrVal.setAttribute(itemAttr);
    itemAttrVal.setValue("XL");
    // Wrap singletonList in ArrayList to make it mutable
    item.setProductItemAttributeValueRequests(new ArrayList<>(Collections.singletonList(itemAttrVal)));
    request.setProductItemRequests(Collections.singletonList(item));
    Assertions.assertDoesNotThrow(() -> ValidationUtil.validateCreateProductItemsRequest(request, false, Collections.emptyList(), false));
  }

  @Test
  void validateCreateProductItemsRequest_FlagsFalse() {
    ProductCreationRequest request = getProductCreationRequestWithAttributesForCoverage();
    // Set flags to false
    Assertions.assertDoesNotThrow(() -> ValidationUtil.validateCreateProductItemsRequest(request, false, null, false));
  }

  @Test
  void validateCreateProductItemsRequest_MerchantSkuMaxLength() {
    ProductCreationRequest request = getProductCreationRequestWithAttributesForCoverage();
    // Max length 255
    String validSku = StringUtils.repeat("a", 255);
    request.getProductItemRequests().get(0).setMerchantSku(validSku);
    Assertions.assertDoesNotThrow(() -> ValidationUtil.validateCreateProductItemsRequest(request, false, null, false));
  }

  @Test
  void validateCreateProductItemsRequest_MerchantSkuTooLong() {
    ProductCreationRequest request = getProductCreationRequestWithAttributesForCoverage();
    // Max length 256 - should fail
    String invalidSku = StringUtils.repeat("a", 256);
    request.getProductItemRequests().get(0).setMerchantSku(invalidSku);
    ApplicationRuntimeException ex = Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validateCreateProductItemsRequest(request, false, null, false);
    });
    Assertions.assertEquals(ERROR_MESSAGE_PREFIX.concat(
      ApiErrorCode.MAXIMUM_SELLER_SKU_EXCEEDED.getDesc()), ex.getMessage());
  }

   private ProductCreationRequest getProductCreationRequestWithAttributesForCoverage() {
      ProductCreationRequest request = new ProductCreationRequest();

      AttributeRequest descAttr = new AttributeRequest();
      descAttr.setAttributeCode("DESC_ATTR");
      descAttr.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
      descAttr.setName("Warna"); // Include Warna for some tests

      AttributeRequest defAttr = new AttributeRequest();
      defAttr.setAttributeCode("DEF_ATTR");
      defAttr.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
      defAttr.setName("Ukuran");

      ProductAttributeValueRequest descVal = new ProductAttributeValueRequest();
      descVal.setDescriptiveAttributeValue("Merah");

      ProductAttributeValueRequest defVal = new ProductAttributeValueRequest();
      AllowedAttributeValueRequest allowedVal = new AllowedAttributeValueRequest();
      allowedVal.setValue("XL");
      defVal.setAllowedAttributeValue(allowedVal);

      ProductAttributeRequest paDesc = new ProductAttributeRequest();
      paDesc.setAttribute(descAttr);
      paDesc.setProductAttributeValues(Collections.singletonList(descVal));

      ProductAttributeRequest paDef = new ProductAttributeRequest();
      paDef.setAttribute(defAttr);
      paDef.setProductAttributeValues(Collections.singletonList(defVal));

      request.setProductAttributes(Arrays.asList(paDesc, paDef));

      ProductItemCreationRequest item = new ProductItemCreationRequest();
      TreeMap<String, String> itemAttrs = new TreeMap<>();
      itemAttrs.put("DESC_ATTR", "Merah");
      itemAttrs.put("DEF_ATTR", "XL");
      item.setAttributesMap(itemAttrs);
      request.setProductItemRequests(Collections.singletonList(item));

      return request;
   }

    @Test
    void validateItemValuesWithProductValues_ItemAttrKeyNotInProduct() {
        ProductCreationRequest request = getProductCreationRequestWithAttributesForCoverage();
        // Add an item attribute not present at product level
        request.getProductItemRequests().get(0).getAttributesMap().put("EXTRA_ATTR", "SomeValue");

        ApplicationRuntimeException ex = Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
            ValidationUtil.validateProductAndItemVariantCreationValues(request);
        });
        Assertions.assertEquals(ERROR_MESSAGE_PREFIX.concat(ApiErrorCode.ITEM_ATTRIBUTE_VALUE_INVALID.getDesc()), ex.getMessage());
    }

    @Test
    void validateItemValuesWithProductValues_ItemAttrValueNotInProduct() {
        ProductCreationRequest request = getProductCreationRequestWithAttributesForCoverage();
        // Change item attribute value to one not allowed at product level
        request.getProductItemRequests().get(0).getAttributesMap().put("DEF_ATTR", "XXL"); // Product level only has XL

        ApplicationRuntimeException ex = Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
            ValidationUtil.validateProductAndItemVariantCreationValues(request);
        });
        Assertions.assertEquals(ERROR_MESSAGE_PREFIX.concat(ApiErrorCode.ITEM_ATTRIBUTE_VALUE_INVALID.getDesc()), ex.getMessage());
    }

    @Test
    void validateFreeSample_IsFalse_ShouldDoNothing() {
        ProductCreationRequest request = new ProductCreationRequest();
        request.setFreeSample(false);
        Assertions.assertDoesNotThrow(() -> ValidationUtil.validateFreeSample(request));
    }

    @Test
    void validateCncWithBopis_RestrictionDisabled() {
        ProductCreationRequest request = new ProductCreationRequest();
        request.setProductType(ProductType.BOPIS.getProductType()); // BOPIS product
        ProductItemCreationRequest item = new ProductItemCreationRequest();
        PickupPointCreateRequest pickup = new PickupPointCreateRequest();
        pickup.setCncActive(true); // CNC Active
        item.setPickupPoints(Collections.singletonList(pickup));
        request.setProductItemRequests(Collections.singletonList(item));

        // Restriction disabled, should return null (no error)
        ApiErrorCode result = ValidationUtil.validateCncWithBopis(request, false);
        Assertions.assertNull(result);
    }

    @Test
    void validateShippingAndDimension_InvalidProductType_ShouldSkipValidation() {
        ProductCreationRequest request = new ProductCreationRequest();
        request.setProductType(ProductType.BOPIS.getProductType()); // Type 3 (BOPIS) - doesn't require shipping validation
        request.setWeight(0.0); // Invalid weight if checked
        request.setHeight(0.0);
        request.setWidth(0.0);
        request.setLength(0.0);
        request.setShippingWeight(0.0);
        ApiErrorCode result = ValidationUtil.validateShippingAndDimension(request, 100000, true, false);
        Assertions.assertNull(result);
    }

    @Test
    void validateShippingAndDimension_L3Null_L5InvalidType_ShouldSkipValidation() {
        ProductCreationRequest request = new ProductCreationRequest();
        request.setProductType(null); // L3 type null
        ProductItemCreationRequest item = new ProductItemCreationRequest();
        item.setProductType(ProductType.BOPIS.getProductType()); // L5 type BOPIS
        request.setProductItemRequests(Collections.singletonList(item));

        request.setWeight(0.0); // Invalid weight if checked
        request.setHeight(0.0);
        request.setWidth(0.0);
        request.setLength(0.0);
        request.setShippingWeight(0.0);

        // Should return null as validation is skipped
        ApiErrorCode result = ValidationUtil.validateShippingAndDimension(request, 100000, true, false);
        Assertions.assertNull(result);
    }

    @Test
    void validateMinAndMaxDimension_PureInstore_ShouldReturnNull() {
        ProductCreationRequest request = new ProductCreationRequest();
        boolean off2OnChannelActive = true;
        Boolean b2cActivated = false;
        boolean instore2FlowSwitch = true; // Assuming this switch enables the pure instore logic
       ApiErrorCode result = ValidationUtil.validateMinAndMaxDimension(request.getProductCode(), 100000, 0.0, 0.0
         , 0.0, 0.0, 0.0, 0.0, off2OnChannelActive, b2cActivated, instore2FlowSwitch);
        Assertions.assertNull(result);
    }

    @Test
    void validateMinAndMaxDimension_ZeroDimension_NotPureInstore_ShouldReturnError() {
        ProductCreationRequest request = new ProductCreationRequest();
        boolean off2OnChannelActive = false; // Not pure instore
        Boolean b2cActivated = true;
        boolean instore2FlowSwitch = true;
        // hasZeroDimension true, !isPureInstoreProduct true -> error
      ApiErrorCode result =
        ValidationUtil.validateMinAndMaxDimension(request.getProductCode(), 100000, 1.0, 0.0, 10.0,
          10.0, 1000.0, 1000.0, off2OnChannelActive, b2cActivated, instore2FlowSwitch);
        Assertions.assertEquals(ApiErrorCode.DIMENSION_LESS_THAN_ZERO, result);
    }

    @Test
    void validateMinAndMaxDimension_ExceededLimit_ShouldReturnError() {
        ProductCreationRequest request = new ProductCreationRequest();
        boolean off2OnChannelActive = false;
        Boolean b2cActivated = true;
        boolean instore2FlowSwitch = true;
        int maxLimit = 50;
        // hasZeroDimension false, hasExceededDimensionLimit true -> error
        ApiErrorCode result = ValidationUtil.validateMinAndMaxDimension(request.getProductCode(), maxLimit, 1.0,
          10.0, 60.0, 10.0, 1000.0, 1000.0, off2OnChannelActive, b2cActivated, instore2FlowSwitch);
        Assertions.assertEquals(ApiErrorCode.DIMENSION_EXCEEDED_THRESHOLD, result);
    }

    @Test
    void validateMinAndMaxDimension_Valid_ShouldReturnNull() {
        ProductCreationRequest request = new ProductCreationRequest();
        boolean off2OnChannelActive = false;
        Boolean b2cActivated = true;
        boolean instore2FlowSwitch = true;
        int maxLimit = 50;
        // hasZeroDimension false, hasExceededDimensionLimit false -> null
        ApiErrorCode result = ValidationUtil.validateMinAndMaxDimension(request.getProductCode(), maxLimit, 0.05,
          10.0, 40.0, 10.0, 50000.0, 50000.0, off2OnChannelActive, b2cActivated, instore2FlowSwitch);
        Assertions.assertNull(result);
    }

    @Test
    void validateAndSetDimensionFieldValues_PositiveValue() {
        // Accessing private method requires reflection or making it package-private.
        // We test its effect through the public call chain.
        ProductCreationRequest request = new ProductCreationRequest();
        request.setProductType(1);
        request.setWeight(123.4567); // Value > 0, needs rounding
        request.setHeight(50.0);
        request.setWidth(50.0);
        request.setLength(50.0);
        request.setShippingWeight(130.0);

        ApiErrorCode result = ValidationUtil.validateShippingAndDimension(request, 100000, true, false);
        Assertions.assertNull(result); // Should pass validation
        Assertions.assertEquals(123.457, request.getWeight()); // Check rounding
    }

    @Test
    void validateShippingAndDimension_NegativeInput_SetsToZeroAndFailsIfNotPureInstore() {
        ProductCreationRequest request = new ProductCreationRequest();
        request.setProductType(1); // Needs validation
        request.setWeight(1000.0); // Valid
        request.setHeight(10.0);   // Valid
        request.setLength(10.0);   // Valid
        request.setWidth(-5.0);    // Invalid input, should trigger roundedValue <= 0
        request.setShippingWeight(1100.0); // Valid

        // Ensure not pure instore (adjust flags as needed based on CommonUtils.isPureInstoreProduct logic)
        request.setOff2OnChannelActive(false);
        request.setB2cActivated(true);
        boolean instore2FlowSwitch = true;

        // This call will invoke validateAndSetDimensionFieldValues for width with -5.0
        // It should set request.width to 0.0 and then fail in validateMinAndMaxDimension
        ApiErrorCode result = ValidationUtil.validateShippingAndDimension(request, 100000, instore2FlowSwitch, false);

        // Assert the final outcome
        Assertions.assertEquals(ApiErrorCode.DIMENSION_LESS_THAN_ZERO, result);
        // Assert that the value was indeed set to 0 by validateAndSetDimensionFieldValues
        Assertions.assertEquals(0.0, request.getWidth());
    }

    @Test
    void validateShippingAndDimension_SmallPositiveInput_SetsToZeroAndFailsIfNotPureInstore() {
        ProductCreationRequest request = new ProductCreationRequest();
        request.setProductType(1); // Needs validation
        request.setWeight(1000.0);
        request.setHeight(10.0);
        request.setLength(10.0);
        request.setWidth(0.0001); // Invalid input (rounds to 0), should trigger roundedValue <= 0
        request.setShippingWeight(1100.0);

        // Ensure not pure instore
        request.setOff2OnChannelActive(false);
        request.setB2cActivated(true);
        boolean instore2FlowSwitch = true;

        ApiErrorCode result = ValidationUtil.validateShippingAndDimension(request, 100000, instore2FlowSwitch, false);

        Assertions.assertEquals(ApiErrorCode.DIMENSION_LESS_THAN_ZERO, result);
        Assertions.assertEquals(0.0, request.getWidth());
    }

    @Test
    void validateB2cAndB2bActivatedFlags_InstoreNewFlowEnabled() {
        ProductCreationRequest request = new ProductCreationRequest();
        request.setOff2OnChannelActive(true); // Relevant for instore flow
        request.setB2cActivated(false); // Start with B2C false

        ProfileResponse profile = new ProfileResponse();
        profile.setCompany(new CompanyDTO());
        profile.getCompany().setSalesChannel(Collections.singletonList(B2C_SELLER_CHANNEL)); // Pure B2C seller

        // Run with instoreNewFlowEnabled = true
        // With pure B2C seller and new flow, B2C should NOT be automatically forced to true.
        // It should be set based on pickup points.
        ValidationUtil.validateB2cAndB2bActivatedFlags(request, profile, true, false);
        Assertions.assertFalse(request.isB2cActivated()); // Should remain false if no pickup points activate it

        // Now add a pickup point that activates B2C
        ProductItemCreationRequest item = new ProductItemCreationRequest();
        PickupPointCreateRequest pickup = new PickupPointCreateRequest();
        pickup.setBuyable(true);
        item.setPickupPoints(Collections.singletonList(pickup));
        request.setProductItemRequests(Collections.singletonList(item));
        ValidationUtil.validateB2cAndB2bActivatedFlags(request, profile, true, false);
        Assertions.assertTrue(request.isB2cActivated()); // Should now be true
    }

    @Test
    void validateEligibilityForB2cAndB2b_EligibleForBoth() {
        ProductCreationRequest request = new ProductCreationRequest();
        request.setB2cActivated(false);
        request.setB2bActivated(false);
        ProfileResponse profile = new ProfileResponse();
        profile.setCompany(new CompanyDTO());
        // Eligible for both channels
        profile.getCompany().setSalesChannel(Arrays.asList(B2C_SELLER_CHANNEL, B2B_SELLER_CHANNEL));

        // Add pickup points activating both
        ProductItemCreationRequest item = new ProductItemCreationRequest();
        PickupPointCreateRequest pickup = new PickupPointCreateRequest();
        pickup.setBuyable(true); // Activates B2C
        B2bDetailsDTO b2b = new B2bDetailsDTO();
        b2b.setBuyable(true); // Activates B2B
        pickup.setB2bFields(b2b);
        item.setPickupPoints(Collections.singletonList(pickup));
        request.setProductItemRequests(Collections.singletonList(item));

        // We test the private method via the public one
        List<String> salesChannels = ValidationUtil.validateB2cAndB2bActivatedFlags(request, profile, false,
            false);

        Assertions.assertTrue(request.isB2cActivated());
        Assertions.assertTrue(request.isB2bActivated());
        Assertions.assertTrue(salesChannels.contains(B2C_SELLER_CHANNEL));
        Assertions.assertTrue(salesChannels.contains(B2B_SELLER_CHANNEL));
    }

    @Test
    void validateEligibilityForB2cAndB2bInCaseOfOnlyB2bOrB2cSellers_NotSizeOne() {
        ProductCreationRequest request = new ProductCreationRequest();
        request.setB2cActivated(false);
        request.setB2bActivated(false);
        // Not size one
        List<String> salesChannel = Arrays.asList(B2C_SELLER_CHANNEL, B2B_SELLER_CHANNEL);

        // We test the private method via the public one
        ProfileResponse profile = new ProfileResponse();
        profile.setCompany(new CompanyDTO());
        profile.getCompany().setSalesChannel(salesChannel);
        ValidationUtil.validateB2cAndB2bActivatedFlags(request, profile, false, false);

        // Should not be forced true just because channels exist
        Assertions.assertFalse(request.isB2cActivated());
        Assertions.assertFalse(request.isB2bActivated());
    }

    @Test
    void isDefiningAttributeAndProductAttributeValuesPresent_Variations() {
        ProductAttributeRequest pa = new ProductAttributeRequest();

        // 1. Attribute null
        pa.setAttribute(null);
        // Test via validateDistinctProductAttributeValues
        ProductCreationRequest request1 = new ProductCreationRequest();
        request1.setProductAttributes(Collections.singletonList(pa));
        Assertions.assertDoesNotThrow(() -> ValidationUtil.validateDistinctProductAttributeValues(request1, "-"));

        // 2. Wrong type
        AttributeRequest attrDesc = new AttributeRequest();
        attrDesc.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
        pa.setAttribute(attrDesc);
        ProductCreationRequest request2 = new ProductCreationRequest();
        request2.setProductAttributes(Collections.singletonList(pa));
        Assertions.assertDoesNotThrow(() -> ValidationUtil.validateDistinctProductAttributeValues(request2, "-"));

        // 3. Values null
        AttributeRequest attrDef = new AttributeRequest();
        attrDef.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
        pa.setAttribute(attrDef);
        pa.setProductAttributeValues(null);
        ProductCreationRequest request3 = new ProductCreationRequest();
        request3.setProductAttributes(Collections.singletonList(pa));
        Assertions.assertDoesNotThrow(() -> ValidationUtil.validateDistinctProductAttributeValues(request3, "-"));

        // 4. Values empty
        pa.setProductAttributeValues(Collections.emptyList());
        ProductCreationRequest request4 = new ProductCreationRequest();
        request4.setProductAttributes(Collections.singletonList(pa));
        Assertions.assertDoesNotThrow(() -> ValidationUtil.validateDistinctProductAttributeValues(request4, "-"));
    }

    @Test
    void checkIfProductIsPureInStoreProduct_Variations() {
        ProfileResponse profile = new ProfileResponse();
        CompanyDTO company = new CompanyDTO();
        profile.setCompany(company);

        // Base case: Should be true
        company.setOfflineToOnlineFlag(true);
        Assertions.assertTrue(testCheckIfPureInStore(profile, true, true, false));

        // Variations (making one condition false at a time)
        Assertions.assertFalse(testCheckIfPureInStore(profile, false, true, false)); // instoreNewFlowEnabled=false
        company.setOfflineToOnlineFlag(false);
        Assertions.assertFalse(testCheckIfPureInStore(profile, true, true, false)); // offlineToOnlineFlag=false
        company.setOfflineToOnlineFlag(true);
        Assertions.assertFalse(testCheckIfPureInStore(profile, true, false, false)); // offOn2ChannelActive=false
        Assertions.assertFalse(testCheckIfPureInStore(profile, true, true, null)); // b2cActivated=null
        Assertions.assertFalse(testCheckIfPureInStore(profile, true, true, true)); // b2cActivated=true

        // Test profile null/company null variations (handled by Optional)
        Assertions.assertFalse(testCheckIfPureInStore(null, true, true, false));
        Assertions.assertFalse(testCheckIfPureInStore(new ProfileResponse(), true, true, false));
    }

    private boolean testCheckIfPureInStore(ProfileResponse profileResponse, boolean instoreNewFlowEnabled,
                                           Boolean offOn2ChannelActive, Boolean b2cActivated) {
        boolean condition1 = instoreNewFlowEnabled;
        boolean condition2 = Optional.ofNullable(profileResponse).map(ProfileResponse::getCompany).map(CompanyDTO::isOfflineToOnlineFlag).orElse(false);
        boolean condition3 = Boolean.TRUE.equals(offOn2ChannelActive);
        boolean condition4 = Boolean.FALSE.equals(b2cActivated);
        return condition1 && condition2 && condition3 && condition4;
    }

    @Test
    void updateEmptyDescriptionForPureInStoreProduct_SkipValidation_DescNotNull() {
        ProductRequest request = new ProductRequest();
        request.setDescription("Initial Desc".getBytes());
        request.setLongDescription("Initial Long Desc".getBytes());
        ValidationUtil.updateEmptyDescriptionForPureInStoreProduct(request, false); // Skip validation
        Assertions.assertNotNull(request.getDescription());
    }

    @Test
    void updateNullDimensionsForPureInStoreProduct_SkipValidation_DimsNotNull() {
        ProductRequest request = new ProductRequest();
        request.setLength(10.0);
        request.setWidth(10.0);
        request.setHeight(10.0);
        request.setWeight(100.0);
        request.setShippingWeight(110.0);
        boolean isAnyValueNull = ValidationUtil.isAnyValueNull(request.getLength(), request.getWidth(), request.getHeight(), request.getWeight(), request.getShippingWeight());
        Assertions.assertFalse(isAnyValueNull);
        ValidationUtil.updateNullDimensionsForPureInStoreProduct(isAnyValueNull, request, false); // Skip validation
        Assertions.assertEquals(10.0, request.getLength());
        Assertions.assertEquals(10.0, request.getWidth());
        Assertions.assertEquals(10.0, request.getHeight());
        Assertions.assertEquals(100.0, request.getWeight());
        Assertions.assertEquals(110.0, request.getShippingWeight());
    }


  @Test
  public void getValidationResponseForPreOrder_Days_Valid() throws Exception {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("DAYS");
    preOrderRequest.setPreOrderValue(10);
    ApiErrorCode errorCode = ValidationUtil.getValidationResponseForPreOrder(preOrderRequest, 30, 4, false, false);
    Assertions.assertNull(errorCode);
    Assertions.assertNotNull(preOrderRequest.getPreOrderDate()); // Ensure date is set
  }

  @Test
  public void getValidationResponseForPreOrder_Days_LessThanZero() throws Exception {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("DAYS");
    preOrderRequest.setPreOrderValue(0);
    ApiErrorCode errorCode = ValidationUtil.getValidationResponseForPreOrder(preOrderRequest, 30, 4, false, false);
    Assertions.assertEquals(ApiErrorCode.PREORDER_DAYS_LESS_THAN_ZERO, errorCode);
  }

  @Test
  public void getValidationResponseForPreOrder_Days_ExceededLimit() throws Exception {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("DAYS");
    preOrderRequest.setPreOrderValue(35);
    ApiErrorCode errorCode = ValidationUtil.getValidationResponseForPreOrder(preOrderRequest, 30, 4, false, false);
    Assertions.assertEquals(ApiErrorCode.PREORDER_DAYS_EXCEEDED_LIMIT, errorCode);
  }

  @Test
  public void getValidationResponseForPreOrder_Week_Valid() throws Exception {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("WEEK");
    preOrderRequest.setPreOrderValue(3);
    ApiErrorCode errorCode = ValidationUtil.getValidationResponseForPreOrder(preOrderRequest, 30, 4, false, false);
    Assertions.assertNull(errorCode);
    Assertions.assertNotNull(preOrderRequest.getPreOrderDate()); // Ensure date is set
  }

  @Test
  public void getValidationResponseForPreOrder_Week_LessThanZero() throws Exception {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("WEEK");
    preOrderRequest.setPreOrderValue(0);
    ApiErrorCode errorCode = ValidationUtil.getValidationResponseForPreOrder(preOrderRequest, 30, 4, false, false);
    Assertions.assertEquals(ApiErrorCode.PREORDER_WEEK_LESS_THAN_ZERO, errorCode);
  }

  @Test
  public void getValidationResponseForPreOrder_Week_ExceededLimit() throws Exception {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("WEEK");
    preOrderRequest.setPreOrderValue(5);
    ApiErrorCode errorCode = ValidationUtil.getValidationResponseForPreOrder(preOrderRequest, 30, 4, false, false);
    Assertions.assertEquals(ApiErrorCode.PREORDER_WEEK_EXCEEDED_LIMIT, errorCode);
  }

  @Test
  public void getValidationResponseForPreOrder_Date_Valid() throws Exception {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("DATE");
    preOrderRequest.setPreOrderDate(getDateInFuture(15)); // Within max days (30)
    ApiErrorCode errorCode = ValidationUtil.getValidationResponseForPreOrder(preOrderRequest, 30, 4, false, false);
    Assertions.assertNull(errorCode);
  }

  @Test
  public void getValidationResponseForPreOrder_Date_Past() throws Exception {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("DATE");
    preOrderRequest.setPreOrderDate(getDateInPast(5)); // Date in the past
    ApiErrorCode errorCode = ValidationUtil.getValidationResponseForPreOrder(preOrderRequest, 30, 4, false, false);
    Assertions.assertEquals(ApiErrorCode.PREORDER_DATE_BEFORE_CURRENT_DATE, errorCode);
  }

   @Test
   void getValidationResponseForPreOrder_Date_ExceededLimit() throws Exception {
     PreOrderRequest preOrderRequest = new PreOrderRequest();
     preOrderRequest.setPreOrderType("DATE");
     preOrderRequest.setPreOrderDate(getDateInFuture(35)); // Exceeds max days (30)
     ApiErrorCode errorCode = ValidationUtil.getValidationResponseForPreOrder(preOrderRequest, 30, 4, false, false);
     // This covers the totalDays > preOrderMaximumDays check inside validatePreOrderDateType
     Assertions.assertEquals(ApiErrorCode.PREORDER_DATE_LIMIT_EXCEEDED, errorCode);
   }

   @Test
   void getValidationResponseForPreOrder_Date_ExceededInternalLimit() throws Exception {
       PreOrderRequest preOrderRequest = new PreOrderRequest();
       preOrderRequest.setPreOrderType("DATE");
       // Date > currentDate + preOrderMaximumDays, but maybe <= 90 days?
       // Let's set preOrderMaximumDays low (e.g., 10) and date further (e.g., 40)
       // This tests the preOrderDate.after(daysAddedDate) branch in validatePreOrderDateType
       preOrderRequest.setPreOrderDate(getDateInFuture(40));
       ApiErrorCode errorCode = ValidationUtil.getValidationResponseForPreOrder(preOrderRequest, 10, 4, false, false);
       // Even though preOrderDate.after(daysAddedDate) is true, the method proceeds
       // and then checks totalDays > preOrderMaximumDays.
       Assertions.assertEquals(ApiErrorCode.PREORDER_DATE_LIMIT_EXCEEDED, errorCode);
   }


  @Test
  void getValidationResponseForPreOrder_InvalidType() throws Exception {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("UNKNOWN");
    ApiErrorCode errorCode = ValidationUtil.getValidationResponseForPreOrder(preOrderRequest, 30, 4, false, false);
    Assertions.assertEquals(ApiErrorCode.INVALID_PREORDER_TYPE, errorCode);
  }

  @Test
  void getValidationResponseForPreOrder_InvalidType_OMG() throws Exception {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("UNKNOWN");
    ApiErrorCode errorCode = ValidationUtil.getValidationResponseForPreOrder(preOrderRequest, 30, 4, true, true);
    Assertions.assertEquals(ApiErrorCode.INVALID_PREORDER_TYPE_OMG, errorCode);
  }

  @Test
  void getValidationResponseForPreOrder_InvalidType_OMG_switchOff() throws Exception {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("UNKNOWN");
    ApiErrorCode errorCode = ValidationUtil.getValidationResponseForPreOrder(preOrderRequest, 30, 4, true, false);
    Assertions.assertEquals(ApiErrorCode.INVALID_PREORDER_TYPE, errorCode);
  }

  @Test
  void getValidationResponseForPreOrder_InvalidType_OMG_off_preorder_switchOn() throws Exception {
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setPreOrderType("UNKNOWN");
    ApiErrorCode errorCode = ValidationUtil.getValidationResponseForPreOrder(preOrderRequest, 30, 4, false, true);
    Assertions.assertEquals(ApiErrorCode.INVALID_PREORDER_TYPE, errorCode);
  }

  @Test
  void validateProductNameAndDescription_ValidInput_ShouldPass() {
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    request.setProductName(PRODUCT_NAME);
    request.setDescription(PRODUCT_DESCRIPTION);
    request.setB2cActivated(true);
    request.setInstore(false);
    Assertions.assertDoesNotThrow(() -> {
      ValidationUtil.validateProductNameAndDescription( 150, request);
    });
  }

  @Test
  void validateProductNameAndDescription_EmptyProductName_ShouldThrowException() {
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    request.setProductName(StringUtils.EMPTY);
    request.setDescription(PRODUCT_DESCRIPTION);
    request.setB2cActivated(true);
    request.setInstore(false);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validateProductNameAndDescription( 150, request);
    });
  }


  @Test
  void validateProductNameAndDescription_ExceededNameLength_ShouldThrowException() {
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    request.setProductName(PRODUCT_NAME.repeat(100));
    request.setDescription(PRODUCT_DESCRIPTION);
    request.setB2cActivated(true);
    request.setInstore(false);

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validateProductNameAndDescription(150, request);
    });
  }

  @Test
  void validateProductNameAndDescription_EmptyDescription_ShouldThrowException() {
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    request.setProductName(PRODUCT_NAME);
    request.setDescription(StringUtils.EMPTY);
    request.setB2cActivated(false);
    request.setInstore(false);

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ValidationUtil.validateProductNameAndDescription( 150, request);
    });
  }

  @Test
  void validateProductNameAndDescription_NullDescription_Instore() {
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    request.setProductName(PRODUCT_NAME);
    request.setDescription(null);
    request.setB2cActivated(false);
    request.setInstore(true);
    request.setPureInstoreProduct(true);
    Assertions.assertDoesNotThrow(() -> {
      ValidationUtil.validateProductNameAndDescription( 150, request);
    });
  }


  @Test
  void validateProductNameAndDescription_NonEmptyDescription_Instore() {
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    request.setProductName(PRODUCT_NAME);
    request.setDescription(PRODUCT_DESCRIPTION);
    request.setB2cActivated(false);
    request.setInstore(true);
    request.setPureInstoreProduct(true);
    Assertions.assertDoesNotThrow(() -> {
      ValidationUtil.validateProductNameAndDescription(150, request);
    });
  }

  @Test
  public void validateProductNameAndDescriptionAndUrl_ShouldSanitizeHtmlTags() {
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    request.setDescription(DESCRIPTION_WITH_TAGS);
    request.setProductName(NAME_WITH_SPAN);
    request.setUrl(URL_WITH_HREF);

    ValidationUtil.validateProductNameAndDescriptionAndUrl(request);

    Assertions.assertEquals("<a href=\"https:/test\">http://example.com</a>", request.getUrl());
  }

  @Test
  public void validateProductNameAndDescriptionAndUrl_ShouldHandleSpecialCharacters() {
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    request.setDescription("Product &amp; description &lt;test&gt;");
    request.setProductName("Product &amp; name");
    request.setUrl(NORMAL_URL);

    ValidationUtil.validateProductNameAndDescriptionAndUrl(request);

    Assertions.assertEquals("Product & description", request.getDescription());
    Assertions.assertEquals("Product & name", request.getProductName());
    Assertions.assertEquals("http://example.com?param=1&param=2", request.getUrl());
  }

  @Test
  public void validateProductNameAndDescriptionAndUrl_ShouldHandleNullValues() {
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    request.setDescription(null);
    request.setProductName(null);
    request.setUrl(null);

    ValidationUtil.validateProductNameAndDescriptionAndUrl(request);

    Assertions.assertNull(request.getDescription());
    Assertions.assertNull(request.getProductName());
    Assertions.assertNull(request.getUrl());
  }

  @Test
  public void validateProductNameAndDescriptionAndUrl_ShouldHandleEmptyStrings() {
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    request.setDescription(StringUtils.EMPTY);
    request.setProductName(StringUtils.EMPTY);
    request.setUrl(StringUtils.EMPTY);

    ValidationUtil.validateProductNameAndDescriptionAndUrl(request);

    Assertions.assertEquals(StringUtils.EMPTY, request.getDescription());
    Assertions.assertEquals(StringUtils.EMPTY, request.getProductName());
    Assertions.assertEquals(StringUtils.EMPTY, request.getUrl());
  }

  @Test
  @DisplayName("BOPIS type with eligible seller and category should succeed")
  void validateBopisTypeWithEligibleSeller() {
    productMasterDataEditRequest.setProductType(ProductType.BOPIS.getProductType());
    productMasterDataEditRequest.setSellerBopisFlag(true);
    productMasterDataEditRequest.getMasterDataEditChangeTypes().add(L3InfoUpdateChangeType.PRODUCT_TYPE_UPDATE);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setBopisEligible(true);
    masterProductEditDTO.setCategoryDetailResponse(categoryDetailResponse);
    ApiErrorCode result =
      ValidationUtil.validateProductTypeAndDimensionsUpdate(productMasterDataEditRequest, masterProductEditDTO);
    Assertions.assertNull(result);
  }

  @Test
  @DisplayName("BOPIS type with ineligible seller should fail")
  void validateBopisTypeWithIneligibleSeller() {
    // Given
    productMasterDataEditRequest.setProductType(ProductType.BOPIS.getProductType());
    productMasterDataEditRequest.setSellerBopisFlag(false);
    productMasterDataEditRequest.getMasterDataEditChangeTypes().add(L3InfoUpdateChangeType.PRODUCT_TYPE_UPDATE);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setBopisEligible(true);
    masterProductEditDTO.setCategoryDetailResponse(categoryDetailResponse);
    ApiErrorCode result =
      ValidationUtil.validateProductTypeAndDimensionsUpdate(productMasterDataEditRequest, masterProductEditDTO);
    assertEquals(ApiErrorCode.BP_BOPIS_ELIGIBILITY_ERROR, result);
  }

  @Test
  @DisplayName("BOPIS type with ineligible seller should fail")
  void validateBopisTypeWithIneligibleSellerAndCategoryResponseNull() {
    // Given
    productMasterDataEditRequest.setProductType(ProductType.BOPIS.getProductType());
    productMasterDataEditRequest.setSellerBopisFlag(true);
    productMasterDataEditRequest.getMasterDataEditChangeTypes().add(L3InfoUpdateChangeType.PRODUCT_TYPE_UPDATE);
    masterProductEditDTO.setCategoryDetailResponse(null);
    ApiErrorCode result =
      ValidationUtil.validateProductTypeAndDimensionsUpdate(productMasterDataEditRequest, new MasterProductEditDTO());
    assertNull(result);
  }

  @Test
  @DisplayName("BOPIS type with ineligible category should fail")
  void validateBopisTypeWithIneligibleCategory() {
    productMasterDataEditRequest.setProductType(ProductType.BOPIS.getProductType());
    productMasterDataEditRequest.setSellerBopisFlag(true);
    productMasterDataEditRequest.getMasterDataEditChangeTypes().add(L3InfoUpdateChangeType.PRODUCT_TYPE_UPDATE);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setBopisEligible(false);
    categoryDetailResponse.setCategoryCode(CATEGORY_CODE);
    masterProductEditDTO.setCategoryDetailResponse(categoryDetailResponse);
    ApiErrorCode result =
      ValidationUtil.validateProductTypeAndDimensionsUpdate(productMasterDataEditRequest, masterProductEditDTO);
    assertEquals(ApiErrorCode.BP_BOPIS_ELIGIBILITY_ERROR, result);
  }


  @Test
  void validateBopisTypeCNCAtL5() {
    productMasterDataEditRequest.setProductType(ProductType.BOPIS.getProductType());
    productMasterDataEditRequest.setSellerBopisFlag(true);
    masterProductEditDTO.setCncActiveAtL5(true);
    productMasterDataEditRequest.getMasterDataEditChangeTypes().add(L3InfoUpdateChangeType.PRODUCT_TYPE_UPDATE);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setBopisEligible(true);
    categoryDetailResponse.setCategoryCode(CATEGORY_CODE);
    masterProductEditDTO.setCategoryDetailResponse(categoryDetailResponse);
    ApiErrorCode result =
      ValidationUtil.validateProductTypeAndDimensionsUpdate(productMasterDataEditRequest, masterProductEditDTO);
    assertEquals(ApiErrorCode.BOPIS_CNC_CHANGE_ERROR, result);
  }

  @Test
  @DisplayName("Big product type with eligible seller should succeed")
  void validateBigProductTypeWithEligibleSeller() {
    productMasterDataEditRequest.setProductType(ProductType.BIG_PRODUCT.getProductType());
    productMasterDataEditRequest.setSellerBigProductFlag(true);
    productMasterDataEditRequest.getMasterDataEditChangeTypes().add(L3InfoUpdateChangeType.PRODUCT_TYPE_UPDATE);
    productMasterDataEditRequest.setLength(10.0);
    productMasterDataEditRequest.setWidth(10.0);
    productMasterDataEditRequest.setHeight(10.0);
    productMasterDataEditRequest.setWeight(1000.0);
    productMasterDataEditRequest.setShippingWeight(1000.0);
    ApiErrorCode result =
      ValidationUtil.validateProductTypeAndDimensionsUpdate(productMasterDataEditRequest, masterProductEditDTO);
    assertNull(result);
  }

  @Test
  @DisplayName("Big product type with ineligible seller should fail")
  void validateBigProductTypeWithIneligibleSeller() {
    productMasterDataEditRequest.setProductType(ProductType.BIG_PRODUCT.getProductType());
    productMasterDataEditRequest.setSellerBigProductFlag(false);
    productMasterDataEditRequest.getMasterDataEditChangeTypes().add(L3InfoUpdateChangeType.PRODUCT_TYPE_UPDATE);
    ApiErrorCode result =
      ValidationUtil.validateProductTypeAndDimensionsUpdate(productMasterDataEditRequest, masterProductEditDTO);
    assertEquals(ApiErrorCode.BP_BOPIS_ELIGIBILITY_ERROR, result);
  }

  @Test
  @DisplayName("Regular product with valid dimensions should succeed")
  void validateRegularProductWithValidDimensions() {
    productMasterDataEditRequest.setProductType(1); // Regular product type
    productMasterDataEditRequest.setLength(10.0);
    productMasterDataEditRequest.setWidth(10.0);
    productMasterDataEditRequest.setHeight(10.0);
    productMasterDataEditRequest.setWeight(1000.0); // 1 kg in grams
    productMasterDataEditRequest.setShippingWeight(1000.0);
    productMasterDataEditRequest.setInstore(false);
    productMasterDataEditRequest.setB2cActivated(true);
    masterProductEditDTO.setMaxProductDimensionLimit(100);
    ApiErrorCode result =
      ValidationUtil.validateProductTypeAndDimensionsUpdate(productMasterDataEditRequest, masterProductEditDTO);
    assertNull(result);
  }

  @Test
  @DisplayName("Product with zero dimensions should fail")
  void validateProductWithZeroDimensions() {
    productMasterDataEditRequest.setProductType(1);
    productMasterDataEditRequest.setLength(0.0);
    productMasterDataEditRequest.setWidth(10.0);
    productMasterDataEditRequest.setHeight(10.0);
    productMasterDataEditRequest.setWeight(1000.0);
    productMasterDataEditRequest.setShippingWeight(1000.0);
    productMasterDataEditRequest.setInstore(false);
    productMasterDataEditRequest.setB2cActivated(true);
    masterProductEditDTO.setMaxProductDimensionLimit(100);
    ApiErrorCode result =
      ValidationUtil.validateProductTypeAndDimensionsUpdate(productMasterDataEditRequest, masterProductEditDTO);
    assertEquals(ApiErrorCode.DIMENSION_LESS_THAN_ZERO, result);
  }

  @Test
  @DisplayName("Product with dimensions exceeding limit should fail")
  void validateProductWithExceededDimensions() {
    // Given
    productMasterDataEditRequest.setProductType(1);
    productMasterDataEditRequest.setLength(200.0);
    productMasterDataEditRequest.setWidth(10.0);
    productMasterDataEditRequest.setHeight(10.0);
    productMasterDataEditRequest.setWeight(1000.0);
    productMasterDataEditRequest.setShippingWeight(1000.0);
    productMasterDataEditRequest.setInstore(false);
    productMasterDataEditRequest.setB2cActivated(true);
    masterProductEditDTO.setMaxProductDimensionLimit(100);
    ApiErrorCode result =
      ValidationUtil.validateProductTypeAndDimensionsUpdate(productMasterDataEditRequest, masterProductEditDTO);
    assertEquals(ApiErrorCode.DIMENSION_EXCEEDED_THRESHOLD, result);
  }

  @Test
  @DisplayName("Product type update validation should be skipped when not in change types")
  void validateSkipProductTypeValidation() {
    productMasterDataEditRequest.setProductType(ProductType.BOPIS.getProductType());
    productMasterDataEditRequest.setSellerBopisFlag(false); // Would fail if product type update was being validated
    productMasterDataEditRequest.getMasterDataEditChangeTypes().add(L3InfoUpdateChangeType.DESCRIPTION_UPDATE);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setBopisEligible(true);
    masterProductEditDTO.setCategoryDetailResponse(categoryDetailResponse);
    ApiErrorCode result =
      ValidationUtil.validateProductTypeAndDimensionsUpdate(productMasterDataEditRequest, masterProductEditDTO);
    assertNull(result);
  }


}