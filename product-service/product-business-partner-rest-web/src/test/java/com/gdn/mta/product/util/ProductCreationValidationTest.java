package com.gdn.mta.product.util;

import static com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE;
import static com.gdn.x.productcategorybase.dto.AttributeType.DESCRIPTIVE_ATTRIBUTE;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.UUID;

import com.gda.mta.product.dto.B2bDetailsDTO;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.gdn.mta.product.service.config.PreOrderConfig;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.BundleRecipeRequest;
import com.gda.mta.product.dto.PickupPointCreateRequest;
import com.gda.mta.product.dto.PreOrderRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerAttributeRequest;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.repository.AttributeRepository;
import com.gdn.mta.product.service.FileStorageService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.x.businesspartner.commons.enums.MerchantType;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.model.entity.PreOrder;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.google.common.collect.ImmutableSet;

/**
 * Created by parvej on 13/08/2020.
 */
public class ProductCreationValidationTest {

  private static final String DEFAULT_BP_CODE = "bp-code";
  private static final String SOURCE_DIRECTORY = "/tmp";
  private static final String FINAL_SOURCE_DIRECTORY = "/tmp";
  private static final String DEFAULT_USERNAME = "com.gdn.mta.developer";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_PRODUCT_CODE = "MTA-" + UUID.randomUUID().toString();
  private static final String DEFAULT_PRODUCT_NAME = "Produk";
  private static final String MERCHANT_SKU = "merchant_sku";
  private static final String UPC_CODE = "upc_code";
  private static final String ID = "id";
  private static final String IMAGE = "image1";
  private static final String VALUE = "value";
  private static final String BRAND_CODE = "brand_code";
  private static final String ATTRIBUTE_CODE = "attribute_code";
  private static final String PICKUP_POINT = "pickup_point";
  private static final String ATTRIBUTE_CODE1 = "attribute_code1";
  private static final String ATTRIBUTE_CODE2 = "attribute_code2";
  private static final String ATTRIBUTE_CODE3 = "attribute_code3";
  private static final String ATTRIBUTE_CODE4 = "attribute_code4";
  private static final String ATTRIBUTE_NAME1 = "Attribute 1";
  private static final String ATTRIBUTE_NAME2 = "Attribute 2";
  private static final String ATTRIBUTE_NAME3 = "Attribute 3";
  private static final String ATTRIBUTE_NAME4 = "Attribute 4";
  private static final String SPECIFICATION_DETAIL = "Spesifikasi Detail";
  private static final int MAX_STOCK_LIMIT = 100000000;
  private static final String PREORDER_TYPE = "DAYS";
  private static final Integer PREORDER_VALUE = 10;
  private static final String ITEM_SKU = "ITEM_SKU";
  private static final String ITEM_CODE = "ITEM_CODE";
  private static final String ITEM_SKU_2 = "DR6-00001-00001-00001";
  private static final String ALLOWED_ATTRIBUTE_VALUE_ID = "ALLOWED_ATTRIBUTE_VALUE_ID";
  private static final String SIZE_CHART_DELIMITER = "-";
  private static final String ATTRIBUTE_CODE_2 = "ATTRIBUTE_CODE_2";

  private ProductRequest productRequest;
  private ProductItemCreationRequest productItemCreationRequest;
  private ProductBusinessPartnerAttributeRequest productBusinessPartnerAttributeRequest;
  private ProductAttributeRequest productAttributeRequest;
  private PreOrderRequest preOrderRequest;

  @BeforeEach
  public void init() {
    this.productRequest =
        new ProductRequest.Builder().productCode(DEFAULT_PRODUCT_CODE).name(DEFAULT_PRODUCT_NAME).length(1.0).width(1.0)
            .height(1.0).weight(1.0).shippingWeight(1.0).description("Deskripsi 1".getBytes())
            .longDescription("Deskripsi 1".getBytes()).brand("Brand 1").brandCode(BRAND_CODE)
            .uniqueSellingPoint("Unique Selling Point").uom(null).storeId(null).promoSKU(false)
            .productCategories(new ArrayList<ProductCategoryRequest>())
            .productAttributes(new ArrayList<ProductAttributeRequest>())
            .productItems(new ArrayList<ProductItemRequest>()).build();

    productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setBuyable(true);
    productItemCreationRequest.setDisplay(true);
    productItemCreationRequest.setMerchantSku(MERCHANT_SKU);
    productItemCreationRequest.setUpcCode(UPC_CODE);
    Image image = new Image();
    image.setStoreId(DEFAULT_STORE_ID);
    image.setMarkForDelete(false);
    image.setMainImages(true);
    image.setActive(true);
    productItemCreationRequest.setImages(Arrays.asList(image));

    productBusinessPartnerAttributeRequest = new ProductBusinessPartnerAttributeRequest();
    productBusinessPartnerAttributeRequest.setAttributeId(ID);
    productBusinessPartnerAttributeRequest.setMandatory(true);
    productBusinessPartnerAttributeRequest.setValue(VALUE);

    productAttributeRequest = new ProductAttributeRequest();
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest.setMandatory(true);
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueRequest.setValue(VALUE);
    attributeRequest.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueRequest));
    productAttributeRequest.setAttribute(attributeRequest);

    preOrderRequest = PreOrderRequest.builder().isPreOrder(true).preOrderType(PREORDER_TYPE)
      .preOrderValue(PREORDER_VALUE).preOrderDate(new Date()).build();

    MockitoAnnotations.initMocks(this);

    Mockito.doNothing().when(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    ReflectionTestUtils.setField(productCreationValidation, "validateProductDescriptiveFieldExclusionList", StringUtils.EMPTY);
    ReflectionTestUtils.setField(productCreationValidation, "negativeStockValidation", true);
    ReflectionTestUtils.setField(productCreationValidation, "cncForWarehouseFeatureSwitch", true);
  }

  @InjectMocks
  private ProductCreationValidation productCreationValidation;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private XProductOutbound xProductOutbound;

  @Mock
  private AttributeRepository attributeRepository;

  @Mock
  private PreOrderConfig preOrderConfig;

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.fileStorageService, this.attributeRepository);
  }

  @Test
  public void validateProductTest() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, true, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void validateProductStockNullTest() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    productCreationRequest.getProductItemRequests().forEach(productItemCreationRequest1 -> productItemCreationRequest1.setStock(null));
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, true, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void validateProductFamilyColorAndWarnaTest() {
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setVariantCreation(true);
    attributeRequest.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    productAttributeRequest.setAttribute(attributeRequest);

    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    productCreationRequest.getProductAttributes().add(productAttributeRequest);
    productCreationRequest.getProductItemRequests().get(0)
        .setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest);
    productItemAttributeValue.setValue(VALUE);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, true, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }


  @Test
  public void validateProductFamilyColorAndNotWarnaTest() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    productCreationRequest.getProductItemRequests().get(0)
        .setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY,
            FINAL_SOURCE_DIRECTORY, MAX_STOCK_LIMIT, false, true, false, false);
      });
    } finally {
      Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
      Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
      deleteDummyFiles(productCreationRequest.getProductItemRequests());
    }

  }

  @Test
  public void validateProductNotBrandTest() {
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    ProductAttributeRequest productAttributeRequest  = new ProductAttributeRequest();
    productAttributeRequest.setProductAttributeName(Constants.INTERNAL);
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueRequest());
    productAttributeRequest.setProductAttributeValues(Collections.singletonList(productAttributeValueRequest));
    productCreationRequest.setBrandCode(BRAND_CODE);
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest.setName(Constants.INTERNAL);
    attributeRequest.setMandatory(false);
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueRequest.setValue(VALUE);
    attributeRequest.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueRequest));
    productAttributeRequest.setAttribute(attributeRequest);
    productCreationRequest.getProductAttributes().add(productAttributeRequest);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest.setAttribute(attributeRequest);
    productCreationRequest.getProductItemRequests().get(0)
        .setProductItemAttributeValueRequests(Collections.singletonList(productItemAttributeValueRequest));
    productCreationRequest.getProductItemRequests().get(1)
        .setProductItemAttributeValueRequests(Collections.singletonList(productItemAttributeValueRequest));
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, true, true, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void validateProductSwitchOffTest() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    ProductAttributeRequest productAttributeRequest  = new ProductAttributeRequest();
    productAttributeRequest.setProductAttributeName(Constants.INTERNAL);
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueRequest());
    productAttributeRequest.setProductAttributeValues(Collections.singletonList(productAttributeValueRequest));
    productCreationRequest.setBrandCode(BRAND_CODE);
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest.setName(Constants.BRAND);
    attributeRequest.setMandatory(false);
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueRequest.setValue(VALUE);
    attributeRequest.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueRequest));
    productAttributeRequest.setAttribute(attributeRequest);
    productCreationRequest.getProductAttributes().add(productAttributeRequest);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);

    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, true, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void validateProductSwitchOnRevisedProductTest() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    ProductAttributeRequest productAttributeRequest  = new ProductAttributeRequest();
    productAttributeRequest.setProductAttributeName(Constants.INTERNAL);
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueRequest());
    productAttributeRequest.setProductAttributeValues(Collections.singletonList(productAttributeValueRequest));
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest.setName(Constants.BRAND);
    attributeRequest.setMandatory(false);
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueRequest.setValue(VALUE);
    attributeRequest.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueRequest));
    productAttributeRequest.setAttribute(attributeRequest);
    productCreationRequest.getProductAttributes().add(productAttributeRequest);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    productCreationRequest.setBrandCode(BRAND_CODE);
    productCreationRequest.setOldProductCode(DEFAULT_PRODUCT_CODE);

    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, true, true, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void validateProductValueNotEmptyTest() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    ProductAttributeRequest productAttributeRequest  = new ProductAttributeRequest();
    productAttributeRequest.setProductAttributeName(Constants.INTERNAL);
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest1 =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest1.setValue(Constants.ACTIVE);
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest1);
    productAttributeRequest.setProductAttributeValues(Collections.singletonList(productAttributeValueRequest));
    productCreationRequest.setBrandCode(BRAND_CODE);
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest.setName(Constants.BRAND);
    attributeRequest.setMandatory(false);
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueRequest.setValue(VALUE);
    attributeRequest.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueRequest));
    productAttributeRequest.setAttribute(attributeRequest);
    productCreationRequest.getProductAttributes().add(productAttributeRequest);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, true, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void validateProductValueItemAttributeValuesTest() {
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    ProductAttributeRequest productAttributeRequest  = new ProductAttributeRequest();
    productAttributeRequest.setProductAttributeName(Constants.BRAND);
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeRequest.setProductAttributeValues(Collections.singletonList(productAttributeValueRequest));
    productCreationRequest.setBrandCode(BRAND_CODE);
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest.setName(Constants.BRAND);
    attributeRequest.setMandatory(false);
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(BRAND_CODE);
    predefinedAllowedAttributeValueRequest.setValue(VALUE);
    attributeRequest.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueRequest));
    productAttributeRequest.setAttribute(attributeRequest);
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
    productCreationRequest.getProductAttributes().add(productAttributeRequest);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest.setAttribute(attributeRequest);
    productCreationRequest.getProductItemRequests().get(0)
        .setProductItemAttributeValueRequests(Collections.singletonList(productItemAttributeValueRequest));
    productCreationRequest.getProductItemRequests().get(1)
        .setProductItemAttributeValueRequests(Collections.singletonList(productItemAttributeValueRequest));
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, true, true, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void validateProductValueItemAttributeInValidValuesTest() {
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    ProductAttributeRequest productAttributeRequest  = new ProductAttributeRequest();
    productAttributeRequest.setProductAttributeName(Constants.BRAND);
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueRequest());
    productAttributeRequest.setProductAttributeValues(Collections.singletonList(productAttributeValueRequest));
    productCreationRequest.setBrandCode(BRAND_CODE);
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest.setName(Constants.BRAND);
    attributeRequest.setMandatory(false);
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(BRAND_CODE);
    predefinedAllowedAttributeValueRequest.setValue(VALUE);
    attributeRequest.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueRequest));
    AttributeRequest attributeRequest1 = new AttributeRequest();
    attributeRequest1.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeRequest1.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest1.setName(Constants.ACTIVE);
    attributeRequest1.setMandatory(false);
    productAttributeRequest.setAttribute(attributeRequest);
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
    productCreationRequest.getProductAttributes().add(productAttributeRequest);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest.setAttribute(attributeRequest1);
    productCreationRequest.getProductItemRequests().get(0)
        .setProductItemAttributeValueRequests(Collections.singletonList(productItemAttributeValueRequest));
    productCreationRequest.getProductItemRequests().get(1)
        .setProductItemAttributeValueRequests(Collections.singletonList(productItemAttributeValueRequest));
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, true, true, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void validateProductValueItemAttributeInValidValues2Test() {
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    ProductAttributeRequest productAttributeRequest  = new ProductAttributeRequest();
    productAttributeRequest.setProductAttributeName(Constants.BRAND);
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeRequest.setProductAttributeValues(Collections.singletonList(productAttributeValueRequest));
    productCreationRequest.setBrandCode(BRAND_CODE);
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest.setName(Constants.BRAND);
    attributeRequest.setMandatory(false);
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(BRAND_CODE);
    predefinedAllowedAttributeValueRequest.setValue(VALUE);
    attributeRequest.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueRequest));
    AttributeRequest attributeRequest1 = new AttributeRequest();
    attributeRequest1.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeRequest1.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest1.setName(Constants.ACTIVE);
    attributeRequest1.setMandatory(false);
    productAttributeRequest.setAttribute(attributeRequest);
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
    productCreationRequest.getProductAttributes().add(productAttributeRequest);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest.setAttribute(attributeRequest1);
    productCreationRequest.getProductItemRequests().get(0)
        .setProductItemAttributeValueRequests(Collections.singletonList(productItemAttributeValueRequest));
    productCreationRequest.getProductItemRequests().get(1)
        .setProductItemAttributeValueRequests(Collections.singletonList(productItemAttributeValueRequest));
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, true, true, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void validateProductValueItemAttributeInValidValues3Test() {
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    ProductAttributeRequest productAttributeRequest  = new ProductAttributeRequest();
    productAttributeRequest.setProductAttributeName(Constants.BRAND);
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeRequest.setProductAttributeValues(Collections.singletonList(productAttributeValueRequest));
    productCreationRequest.setBrandCode(BRAND_CODE);
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest.setName(Constants.BRAND);
    attributeRequest.setMandatory(false);
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(BRAND_CODE);
    predefinedAllowedAttributeValueRequest.setValue(VALUE);
    attributeRequest.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueRequest));
    AttributeRequest attributeRequest1 = new AttributeRequest();
    attributeRequest1.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeRequest1.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest1.setName(Constants.ACTIVE);
    attributeRequest1.setMandatory(false);
    attributeRequest1.setPredefinedAllowedAttributeValues(
        Collections.singletonList(predefinedAllowedAttributeValueRequest));
    productAttributeRequest.setAttribute(attributeRequest);
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
    productCreationRequest.getProductAttributes().add(productAttributeRequest);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest.setAttribute(attributeRequest1);
    productCreationRequest.getProductItemRequests().get(0)
        .setProductItemAttributeValueRequests(Collections.singletonList(productItemAttributeValueRequest));
    productCreationRequest.getProductItemRequests().get(1)
        .setProductItemAttributeValueRequests(Collections.singletonList(productItemAttributeValueRequest));
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, true, true, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void validateProductValueItemAttributeInValidValuesNotValidTest() {
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    ProductAttributeRequest productAttributeRequest  = new ProductAttributeRequest();
    productAttributeRequest.setProductAttributeName(ATTRIBUTE_CODE);
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueRequest());
    productAttributeRequest.setProductAttributeValues(Collections.singletonList(productAttributeValueRequest));
    productCreationRequest.setBrandCode(IMAGE);
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest.setName(ATTRIBUTE_CODE);
    attributeRequest.setMandatory(false);
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(BRAND_CODE);
    predefinedAllowedAttributeValueRequest.setValue(IMAGE);
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest1 =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest1.setPredefinedAllowedAttributeCode(BRAND_CODE);
    predefinedAllowedAttributeValueRequest1.setValue(Constants.ACTIVE);
    attributeRequest.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueRequest));
    AttributeRequest attributeRequest1 = new AttributeRequest();
    attributeRequest1.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeRequest1.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest1.setName(Constants.BRAND);
    attributeRequest1.setMandatory(false);
    attributeRequest1.setPredefinedAllowedAttributeValues(
        Collections.singletonList(predefinedAllowedAttributeValueRequest1));
    productAttributeRequest.setAttribute(attributeRequest);
    productCreationRequest.getProductAttributes().add(productAttributeRequest);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest.setAttribute(attributeRequest1);
    productCreationRequest.getProductItemRequests().get(0)
        .setProductItemAttributeValueRequests(Collections.singletonList(productItemAttributeValueRequest));
    productCreationRequest.getProductItemRequests().get(1)
        .setProductItemAttributeValueRequests(Collections.singletonList(productItemAttributeValueRequest));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productCreationValidation
            .validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
                MAX_STOCK_LIMIT, true, true, false, false);
      });
    }
    finally {
      Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
      Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
      deleteDummyFiles(productCreationRequest.getProductItemRequests());
    }

  }

  @Test
  public void validateProductMandatoryTrueTest() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    ProductAttributeRequest productAttributeRequest  = new ProductAttributeRequest();
    productAttributeRequest.setProductAttributeName(Constants.INTERNAL);
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest1 =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest1.setValue(Constants.ACTIVE);
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest1);
    productAttributeRequest.setProductAttributeValues(Collections.singletonList(productAttributeValueRequest));
    productCreationRequest.setBrandCode(BRAND_CODE);
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest.setName(Constants.BRAND);
    attributeRequest.setMandatory(true);
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueRequest.setValue(VALUE);
    attributeRequest.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueRequest));
    productAttributeRequest.setAttribute(attributeRequest);
    productCreationRequest.getProductAttributes().add(productAttributeRequest);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);

    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, true, false, false);
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void validateProductBrandCodeTest() {
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    ProductAttributeRequest productAttributeRequest  = new ProductAttributeRequest();
    productAttributeRequest.setProductAttributeName(Constants.BRAND);
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueRequest());
    productAttributeRequest.setProductAttributeValues(Collections.singletonList(productAttributeValueRequest));
    productCreationRequest.setBrandCode(BRAND_CODE);
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest.setName(Constants.BRAND);
    attributeRequest.setMandatory(false);
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueRequest.setValue(VALUE);
    attributeRequest.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueRequest));
    productAttributeRequest.setAttribute(attributeRequest);
    productCreationRequest.getProductAttributes().add(productAttributeRequest);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productCreationValidation
            .validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
                MAX_STOCK_LIMIT, true, true, false, false);
      });
    }
    finally {
      Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
      Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
      deleteDummyFiles(productCreationRequest.getProductItemRequests());
    }
  }

  @Test
  public void validateProductBrandCodeValidTest() {
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    ProductAttributeRequest productAttributeRequest  = new ProductAttributeRequest();
    productAttributeRequest.setProductAttributeName(Constants.BRAND);
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeRequest.setProductAttributeValues(Collections.singletonList(productAttributeValueRequest));
    productCreationRequest.setBrandCode(BRAND_CODE);
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest.setName(Constants.INTERNAL);
    attributeRequest.setMandatory(false);
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(BRAND_CODE);
    predefinedAllowedAttributeValueRequest.setValue(VALUE);
    attributeRequest.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueRequest));
    productAttributeRequest.setAttribute(attributeRequest);
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
    productCreationRequest.getProductAttributes().add(productAttributeRequest);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest.setAttribute(attributeRequest);
    productCreationRequest.getProductItemRequests().get(0)
        .setProductItemAttributeValueRequests(Collections.singletonList(productItemAttributeValueRequest));
    productCreationRequest.getProductItemRequests().get(1)
        .setProductItemAttributeValueRequests(Collections.singletonList(productItemAttributeValueRequest));
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, true, true, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void validateProductBlankValueTest() {
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    productBusinessPartnerAttributeRequest.setValue(StringUtils.EMPTY);
    generateDummyImageFiles1(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    try {
      Assertions.assertThrows(Exception.class, () -> {
        productCreationValidation
            .validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
                MAX_STOCK_LIMIT, false, true, false, false);
      });
    }
    finally {
      Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
      Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
      deleteDummyFiles(productCreationRequest.getProductItemRequests());
    }
  }

  @Test
  public void validateProductEmptyPickupPointValueTest() {
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    productCreationRequest.getProductItemRequests().get(0).setPickupPointId(StringUtils.EMPTY);
    productCreationRequest.getProductItemRequests().get(1).setPickupPointId(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
          MAX_STOCK_LIMIT, false, true, false, false);
    });
  }

  @Test
  public void validateProductEmptyPickupPointValueTest_2() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    productCreationRequest.setBusinessPartnerCode(StringUtils.EMPTY);
    productCreationRequest.getProductItemRequests().get(0).setPickupPointId(StringUtils.EMPTY);
    productCreationRequest.getProductItemRequests().get(1).setPickupPointId(null);
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    Mockito.when(fileStorageService
      .checkImageAvailability(Mockito.any(), Mockito.anyString()))
      .thenReturn(SOURCE_DIRECTORY);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productCreationValidation
            .validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
                MAX_STOCK_LIMIT, false, true, false, false);
      });
    }
    finally {
      Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
      Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    }
  }

  @Test
  public void validateProductNonMandatoryBlankValueTest() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    productBusinessPartnerAttributeRequest.setMandatory(false);
    productBusinessPartnerAttributeRequest.setValue(StringUtils.EMPTY);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, true, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void validateProductAttributeNotMandatoryTest() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, true, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  private ProductCreationRequest generateProductCreationRequest() {
    ProductCreationRequest request = new ProductCreationRequest();
    request.setBusinessPartnerCode(DEFAULT_BP_CODE);
    this.productRequest.setId(UUID.randomUUID().toString());
    this.productRequest.setCreatedBy(DEFAULT_USERNAME);
    this.productRequest.setCreatedDate(Calendar.getInstance().getTime());
    this.productRequest.setUpdatedBy(DEFAULT_USERNAME);
    this.productRequest.setUpdatedDate(Calendar.getInstance().getTime());
    this.productRequest.setSpecificationDetail(SPECIFICATION_DETAIL);
    this.productRequest.getProductCategories().add(new ProductCategoryRequest(new CategoryRequest(), DEFAULT_STORE_ID));
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(DESCRIPTIVE_ATTRIBUTE);
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE1);
    attributeRequest.setMandatory(true);
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeValueRequest.setDescriptiveAttributeValue(VALUE);
    ProductAttributeRequest productAttributeRequest =
        new ProductAttributeRequest(attributeRequest, ATTRIBUTE_NAME1, false, 1, DEFAULT_STORE_ID);
    List<ProductAttributeValueRequest> productAttributeValueRequests = new ArrayList<>();
    productAttributeValueRequests.add(productAttributeValueRequest);
    productAttributeRequest.setProductAttributeValues(productAttributeValueRequests);

    AttributeRequest attributeRequest1 = new AttributeRequest();
    attributeRequest1.setAttributeType(DESCRIPTIVE_ATTRIBUTE);
    attributeRequest1.setAttributeCode(ATTRIBUTE_CODE2);
    attributeRequest1.setMandatory(false);
    attributeRequest1.setVariantCreation(true);
    ProductAttributeValueRequest productAttributeValueRequest1 = new ProductAttributeValueRequest();
    productAttributeValueRequest1.setDescriptiveAttributeValue(StringUtils.EMPTY);
    ProductAttributeRequest productAttributeRequest1 =
        new ProductAttributeRequest(attributeRequest1, ATTRIBUTE_NAME2, false, 1, DEFAULT_STORE_ID);
    List<ProductAttributeValueRequest> productAttributeValueRequests1 = new ArrayList<>();
    productAttributeValueRequests1.add(productAttributeValueRequest1);
    productAttributeRequest1.setProductAttributeValues(productAttributeValueRequests1);

    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setAttributeType(DEFINING_ATTRIBUTE);
    attributeRequest2.setAttributeCode(ATTRIBUTE_CODE3);
    attributeRequest2.setMandatory(false);
    attributeRequest2.setVariantCreation(true);
    ProductAttributeValueRequest productAttributeValueRequest2 = new ProductAttributeValueRequest();
    productAttributeValueRequest2
        .setAllowedAttributeValue(new AllowedAttributeValueRequest(StringUtils.EMPTY, 1, DEFAULT_STORE_ID));
    ProductAttributeRequest productAttributeRequest2 =
        new ProductAttributeRequest(attributeRequest2, ATTRIBUTE_NAME3, false, 1, DEFAULT_STORE_ID);
    List<ProductAttributeValueRequest> productAttributeValueRequests2 = new ArrayList<>();
    productAttributeValueRequests2.add(productAttributeValueRequest2);
    productAttributeRequest2.setProductAttributeValues(productAttributeValueRequests2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setAttributeType(DEFINING_ATTRIBUTE);
    attributeRequest3.setAttributeCode(ATTRIBUTE_CODE4);
    attributeRequest3.setMandatory(true);
    attributeRequest3.setVariantCreation(true);
    ProductAttributeValueRequest productAttributeValueRequest3 = new ProductAttributeValueRequest();
    productAttributeValueRequest3
        .setAllowedAttributeValue(new AllowedAttributeValueRequest(VALUE, 1, DEFAULT_STORE_ID));
    ProductAttributeRequest productAttributeRequest3 =
        new ProductAttributeRequest(attributeRequest3, ATTRIBUTE_NAME4, false, 1, DEFAULT_STORE_ID);
    List<ProductAttributeValueRequest> productAttributeValueRequests3 = new ArrayList<>();
    productAttributeValueRequests3.add(productAttributeValueRequest3);
    productAttributeRequest3.setProductAttributeValues(productAttributeValueRequests3);

    AttributeRequest attributeRequest4 = new AttributeRequest();
    attributeRequest4.setAttributeType(DESCRIPTIVE_ATTRIBUTE);
    attributeRequest4.setAttributeCode(ATTRIBUTE_CODE4);
    attributeRequest4.setMandatory(true);
    attributeRequest4.setVariantCreation(true);
    ProductAttributeValueRequest productAttributeValueRequest4 = new ProductAttributeValueRequest();
    productAttributeValueRequest4
        .setDescriptiveAttributeValue(VALUE);
    ProductAttributeRequest productAttributeRequest4 =
        new ProductAttributeRequest(attributeRequest4, ATTRIBUTE_NAME4, false, 1, DEFAULT_STORE_ID);
    List<ProductAttributeValueRequest> productAttributeValueRequests4 = new ArrayList<>();
    productAttributeValueRequests4.add(productAttributeValueRequest4);
    productAttributeRequest4.setProductAttributeValues(productAttributeValueRequests4);

    this.productRequest.getProductAttributes().add(productAttributeRequest);
    this.productRequest.getProductAttributes().add(productAttributeRequest1);
    this.productRequest.getProductAttributes().add(productAttributeRequest2);
    this.productRequest.getProductAttributes().add(productAttributeRequest3);
    this.productRequest.getProductAttributes().add(productAttributeRequest4);

    BeanUtils.copyProperties(productRequest, request);
    List<Image> images = new ArrayList<>();
    Image image = new Image();
    image.setStoreId(DEFAULT_STORE_ID);
    image.setMarkForDelete(false);
    image.setMainImages(true);
    image.setActive(true);
    image.setLocationPath(IMAGE);

    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setBuyable(true);
    productItemCreationRequest.setDisplay(true);
    productItemCreationRequest.setMerchantSku(MERCHANT_SKU);
    productItemCreationRequest.setUpcCode(UPC_CODE);
    productItemCreationRequest.setImages(Arrays.asList(image));
    productItemCreationRequest.setPickupPointId(PICKUP_POINT);
    TreeMap<String, String> attributesMap = new TreeMap<>();
    attributesMap.put(ATTRIBUTE_CODE1, StringUtils.EMPTY);
    attributesMap.put(ATTRIBUTE_CODE2, StringUtils.EMPTY);
    productItemCreationRequest.setAttributesMap(attributesMap);

    TreeMap<String, String> attributesMap1 = new TreeMap<>();
    attributesMap1.put(ATTRIBUTE_CODE3, StringUtils.EMPTY);
    attributesMap1.put(ATTRIBUTE_CODE4, StringUtils.EMPTY);

    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setBuyable(true);
    productItemCreationRequest1.setDisplay(true);
    productItemCreationRequest1.setMerchantSku(MERCHANT_SKU);
    productItemCreationRequest1.setUpcCode(UPC_CODE);
    productItemCreationRequest1.setImages(Arrays.asList(image));
    productItemCreationRequest1.setAttributesMap(attributesMap1);
    productItemCreationRequest1.setPickupPointId(PICKUP_POINT);
    List<ProductItemCreationRequest> productItemCreationRequests = new ArrayList<>();
    productItemCreationRequests.add(productItemCreationRequest);
    productItemCreationRequests.add(productItemCreationRequest1);
    request.setProductItemRequests(productItemCreationRequests);

    request.getProductItemRequests().forEach(productItemCreationRequest2 -> productItemCreationRequest2.setStock(10000));
    return request;
  }

  @Test
  public void testCheckNonMandatoryVariantCreatingEmptyAttributeValue() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, true, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void testCheckNonMandatoryEmptyAttributeValueRequest() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    productCreationRequest.getProductAttributes().get(2).setProductAttributeValues(null);
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, true, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void activateImageFlagTrueForFlow2Test() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    productCreationRequest.setProductCreationType(ProductCreationType.FLOW2);
    Image image = new Image();
    image.setActive(false);
    image.setOriginalImage(null);
    image.setLocationPath(IMAGE);
    image.setStoreId(DEFAULT_STORE_ID);
    image.setMarkForDelete(false);
    Image image1 = new Image();
    image1.setActive(false);
    image1.setOriginalImage(null);
    image1.setLocationPath(IMAGE);
    image1.setStoreId(DEFAULT_STORE_ID);
    image1.setMarkForDelete(false);
    productCreationRequest.setCommonImages(List.of(image));
    productCreationRequest.getProductItemRequests().get(0).setImages(List.of(image1));
    productCreationRequest.getProductItemRequests().get(1).setImages(List.of(image));
    productCreationRequest.getProductAttributes().get(2).setProductAttributeValues(null);
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, true, false, false);
    Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void activateImageFlagTrueForFlow2Test2() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    productCreationRequest.setProductCreationType(ProductCreationType.FLOW2);
    Image image = new Image();
    image.setActive(true);
    image.setOriginalImage(false);
    image.setLocationPath(IMAGE);
    image.setStoreId(DEFAULT_STORE_ID);
    image.setMarkForDelete(false);
    productCreationRequest.setCommonImages(List.of(image));
    productCreationRequest.getProductItemRequests().get(0).setImages(List.of(image));
    productCreationRequest.getProductAttributes().get(2).setProductAttributeValues(null);
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, true, false, false);
    Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void checkValidationForAttributeHavingMoreThan255CharacterTest2() {
    ApplicationRuntimeException applicationRuntimeException = new ApplicationRuntimeException();
    try {
      AttributeRequest attributeRequest2 = new AttributeRequest();
      attributeRequest2.setName(Constants.FAMILY_COLOUR);
      ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
      productItemAttributeValue.setAttribute(attributeRequest2);
      AttributeRequest attributeRequest3 = new AttributeRequest();
      attributeRequest3.setVariantCreation(true);
      attributeRequest3.setName(Constants.WARNA);
      ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
      productAttributeRequest1.setAttribute(attributeRequest3);
      ProductCreationRequest productCreationRequest = generateProductCreationRequest();
      productCreationRequest.setProductCreationType(ProductCreationType.FLOW2);
      ProductAttributeRequest productAttributeRequest3 = new ProductAttributeRequest();
      AttributeRequest attributeRequest4 = new AttributeRequest();
      attributeRequest4.setName(Constants.FAMILY_COLOUR);
      attributeRequest4.setAttributeType(DESCRIPTIVE_ATTRIBUTE);
      productAttributeRequest3.setAttribute(attributeRequest4);
      ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
      productAttributeValueRequest.setDescriptiveAttributeValue(
          "the maximum length for a text field is 255 characters: The maximum length of this field is up to 255 characters.,the maximum length for a text field is 255 characters: The maximum length of this field is up to 255 characters.the maximum length for a text field is 255 characters: The maximum length of this field is up to 255 characters");
      productAttributeRequest3.setProductAttributeValues(Arrays.asList(productAttributeValueRequest));
      productCreationRequest.getProductAttributes().add(productAttributeRequest3);
      Image image = new Image();
      image.setActive(true);
      image.setOriginalImage(false);
      image.setLocationPath(IMAGE);
      image.setStoreId(DEFAULT_STORE_ID);
      image.setMarkForDelete(false);
      productCreationRequest.setCommonImages(List.of(image));
      productCreationRequest.getProductItemRequests().get(0).setImages(List.of(image));
      productCreationRequest.getProductAttributes().get(2).setProductAttributeValues(null);
      productCreationRequest.getProductItemRequests().get(0)
          .setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
      productCreationRequest.getProductAttributes().add(productAttributeRequest1);
      productItemAttributeValue.setValue(VALUE);
      generateDummyImageFiles(productCreationRequest.getProductItemRequests());
      productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
          MAX_STOCK_LIMIT, false, true, false, false);
    } catch (ApplicationRuntimeException e) {
      applicationRuntimeException.setErrorMessage(e.getErrorMessage());
    } finally {
      Mockito.verify(fileStorageService).checkImageAvailability(Mockito.any(), Mockito.anyString());
      Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
      Assertions.assertEquals(ErrorCategory.VALIDATION.getMessage()
              + Constants.MAXIMUM_CHARACTERS_ACCEPTED_IS_255_CHARACTERS_INCLUDING_SPACE,
          applicationRuntimeException.getErrorMessage());
    }
  }

  @Test
  public void activateImageFlagTrueForFlow2Test3() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    productCreationRequest.setProductCreationType(ProductCreationType.FLOW2);
    Image image = new Image();
    image.setActive(false);
    image.setOriginalImage(true);
    image.setLocationPath(IMAGE);
    image.setStoreId(DEFAULT_STORE_ID);
    image.setMarkForDelete(false);
    productCreationRequest.setCommonImages(List.of(image));
    productCreationRequest.getProductItemRequests().get(0).setImages(List.of(image));
    productCreationRequest.getProductAttributes().get(2).setProductAttributeValues(null);
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, true, false, false);
    Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void testCheckNonMandatoryEmptyDescriptiveAttributeValueRequest() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    productCreationRequest.getProductAttributes().get(1).setProductAttributeValues(null);
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductItemRequests().get(0).setUpcCode(StringUtils.EMPTY);
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, true, true, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void testCheckNonMandatoryEmptyAttributeValueRequestDuplicateUpc() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    productCreationRequest.getProductAttributes().get(2).setProductAttributeValues(null);
    productCreationRequest.getProductItemRequests().get(0)
        .setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
            MAX_STOCK_LIMIT, false, true, true, false);
      });
    } finally {
      Mockito.verify(fileStorageService)
          .checkImageAvailability(Mockito.any(), Mockito.anyString());
      Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
      deleteDummyFiles(productCreationRequest.getProductItemRequests());
    }
  }

  @Test
  public void checkNonMandatoryVariantCreatingEmptyAttributeValueExceptionTest() {
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    productCreationRequest.getProductAttributes().get(2).getAttribute().setMandatory(true);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    try {
      Assertions.assertThrows(Exception.class, () -> {
        productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
            MAX_STOCK_LIMIT, false, true, false, false);
      });
    }
    finally {
      Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
      Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
      deleteDummyFiles(productCreationRequest.getProductItemRequests());
    }
  }

  private void generateDummyImageFiles(List<ProductItemCreationRequest> productItemCreationRequests) {
    productItemCreationRequests.forEach(productItemRequest -> {
      productItemRequest.getImages().forEach(image -> {
        try {
          new File(SOURCE_DIRECTORY, FilenameUtils.getName(image.getLocationPath())).createNewFile();
        } catch (IOException e) {
          e.printStackTrace();
        }
      });
    });
  }

  private void generateDummyImageFiles1(List<ProductItemCreationRequest> productItemCreationRequests) {
    productItemCreationRequests.forEach(productItemRequest -> {
      productItemRequest.getImages().forEach(image -> {
        try {
          image.setActive(false);
          new File(SOURCE_DIRECTORY, FilenameUtils.getName(image.getLocationPath())).createNewFile();
        } catch (IOException e) {
          e.printStackTrace();
        }
      });
    });
  }

  @Test
  public void validateProductMaxStockExceptionTest() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    productCreationRequest.getProductItemRequests()
        .forEach(productItemCreationRequest1 -> productItemCreationRequest1.setStock(MAX_STOCK_LIMIT + 1));
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
            MAX_STOCK_LIMIT, false, true, false, false);
      });
    } finally {
      Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
      Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
      deleteDummyFiles(productCreationRequest.getProductItemRequests());
    }
  }

  @Test
  public void validateWholeSalePriceSettingOnFreeSampleProductTest() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(true);
    productCreationRequest.setFreeSample(true);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, true, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void validateWholeSalePriceSettingOnFreeSampleProductL5Test() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(true);
    productCreationRequest.setFreeSample(false);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, false, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void validateWholeSalePriceSettingOnFreeSampleTrueL5Test() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(true);
    productCreationRequest.setFreeSample(true);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, false, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void validateWholeSalePriceSettingNonNullFreeSampleTrueL5Test() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setWholesalePriceActivated(false);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(true);
    productCreationRequest.setFreeSample(true);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, false, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void validateWholeSalePriceSettingTrueNullFreeSampleTrueL5Test() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setWholesalePriceActivated(true);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT);
    productCreationRequest.getProductItemRequests().get(0)
        .setPickupPoints(Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1)
        .setPickupPoints(Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(true);
    productCreationRequest.setFreeSample(true);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productCreationRequest.getProductItemRequests().get(0)
        .setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    productCreationValidation
        .validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY, MAX_STOCK_LIMIT, false,
            false, false, false);
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    Assertions.assertFalse(
        productCreationRequest.getProductItemRequests().get(0).getPickupPoints().get(0).getWholesalePriceActivated());
  }

  @Test
  public void validateWholeSalePriceFalseSettingOnFreeSampleProductTest() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(false);
    productCreationRequest.setFreeSample(true);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, true, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void validateNullWholeSalePriceSettingOnFreeSampleProductTest() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(null);
    productCreationRequest.setFreeSample(true);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, true, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void validateWholeSalePriceSettingOnNonFreeSampleProductTest() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(true);
    productCreationRequest.setFreeSample(false);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, true, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  @Test
  public void validateFreeSampleAndPreOrderProductExceptionTestTest() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    Exception exception = new Exception();
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setFreeSample(true);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest
      .setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productCreationValidation
            .validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
                MAX_STOCK_LIMIT, false, true, false, false);
      });
    } catch (Exception ex) {
      exception = ex;
      throw ex;
    } finally {
      Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
      Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
      deleteDummyFiles(productCreationRequest.getProductItemRequests());
    }
  }

  @Test
  public void validateFreeSampleProductTest() {
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setName(Constants.FAMILY_COLOUR);
    ProductItemAttributeValueRequest productItemAttributeValue = new ProductItemAttributeValueRequest();
    productItemAttributeValue.setAttribute(attributeRequest2);

    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setVariantCreation(true);
    attributeRequest3.setName(Constants.WARNA);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest3);

    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    preOrderRequest.setIsPreOrder(false);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setFreeSample(true);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    productCreationRequest
      .setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttributeRequest));
    productCreationRequest.getProductItemRequests().get(0).setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValue));
    productCreationRequest.getProductAttributes().add(productAttributeRequest1);
    productItemAttributeValue.setValue(VALUE);
    productCreationValidation
      .validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, true, false, false);
    Mockito.verify(fileStorageService)
      .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
  }

  private void deleteDummyFiles(List<ProductItemCreationRequest> productItemCreationRequests) {
    productItemCreationRequests.forEach(productItemRequest -> {
      productItemRequest.getImages().forEach(image -> {
        new File(SOURCE_DIRECTORY, FilenameUtils.getName(image.getLocationPath())).delete();
      });
    });
  }

  @Test
  public void bundleProductValidation_switchOffTest() throws Exception {
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setBundleRecipe(ImmutableSet.of(new BundleRecipeRequest()));
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBundleProduct(true);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(MerchantType.TD.name());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);

    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEnabled", false);

    productCreationValidation.bundleProductValidation(productCreationRequest, profileResponse);

    Assertions.assertFalse(productCreationRequest.isBundleProduct());
    Assertions.assertNull(productItemCreationRequest.getBundleRecipe());
  }

  @Test
  public void bundleProductValidation_nullProfileResponseTest() throws Exception {
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setBundleRecipe(ImmutableSet.of(new BundleRecipeRequest()));
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBundleProduct(true);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(MerchantType.TD.name());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);

    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEnabled", true);

    productCreationValidation.bundleProductValidation(productCreationRequest, null);

    Assertions.assertFalse(productCreationRequest.isBundleProduct());
    Assertions.assertNull(productItemCreationRequest.getBundleRecipe());
  }

  @Test
  public void bundleProductValidation_notEligibleMerchantTypeTest() throws Exception {
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setBundleRecipe(ImmutableSet.of(new BundleRecipeRequest()));
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBundleProduct(true);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(MerchantType.TC.name());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);

    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEligibleMerchantTypes", "TD");

    productCreationValidation.bundleProductValidation(productCreationRequest, profileResponse);

    Assertions.assertFalse(productCreationRequest.isBundleProduct());
    Assertions.assertNull(productItemCreationRequest.getBundleRecipe());
  }

  @Test
  public void bundleProductValidation_notBundleProductTest() throws Exception {
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setBundleRecipe(ImmutableSet.of(new BundleRecipeRequest()));
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBundleProduct(false);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(MerchantType.TD.name());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);

    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEligibleMerchantTypes", "TD");

    productCreationValidation.bundleProductValidation(productCreationRequest, profileResponse);

    Assertions.assertFalse(productCreationRequest.isBundleProduct());
    Assertions.assertNull(productItemCreationRequest.getBundleRecipe());
  }

  @Test
  public void bundleProductValidation_variantWithoutBundleRecipeTest() throws Exception {
    Set<BundleRecipeRequest> bundleRecipeRequestSet = new HashSet<>();
    bundleRecipeRequestSet.add(new BundleRecipeRequest());
    bundleRecipeRequestSet.add(null);
    bundleRecipeRequestSet.add(new BundleRecipeRequest(ITEM_SKU, 10));
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setBundleRecipe(bundleRecipeRequestSet);
    ProductItemCreationRequest productItemCreationRequest2 = new ProductItemCreationRequest();
    productItemCreationRequest2.setBundleRecipe(null);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBundleProduct(true);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest1, productItemCreationRequest2));

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(MerchantType.TD.name());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);

    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEligibleMerchantTypes", "TD");

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productCreationValidation.bundleProductValidation(productCreationRequest, profileResponse);
    });

  }

  @Test
  public void bundleProductValidation_validateAnyVariantWithMoreThanMaxSkusInBundleRecipeTest() throws Exception {
    Set<BundleRecipeRequest> bundleRecipeRequestSet = new HashSet<>();
    bundleRecipeRequestSet.add(new BundleRecipeRequest(ITEM_SKU, 1));
    bundleRecipeRequestSet.add(new BundleRecipeRequest(ITEM_SKU, 10));
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setBundleRecipe(bundleRecipeRequestSet);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBundleProduct(true);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest1));

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(MerchantType.TD.name());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);

    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEligibleMerchantTypes", "TD");
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingMaxNumberOfSkus", 1);

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productCreationValidation.bundleProductValidation(productCreationRequest, profileResponse);
    });

  }

  @Test
  public void bundleProductValidation_validateAnyVariantWithoutProperItemSkuInBundleRecipeTest() throws Exception {
    Set<BundleRecipeRequest> bundleRecipeRequestSet = new HashSet<>();
    bundleRecipeRequestSet.add(new BundleRecipeRequest(ITEM_SKU, 1));
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setBundleRecipe(bundleRecipeRequestSet);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBundleProduct(true);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest1));

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(MerchantType.TD.name());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);

    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEligibleMerchantTypes", "TD");
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingMaxNumberOfSkus", 1);

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productCreationValidation.bundleProductValidation(productCreationRequest, profileResponse);
    });

  }

  @Test
  public void bundleProductValidation_validateBundleChildSkuStatusProductNotInResponseTest() throws Exception {
    Set<BundleRecipeRequest> bundleRecipeRequestSet = new HashSet<>();
    bundleRecipeRequestSet.add(new BundleRecipeRequest(ITEM_SKU_2, 1));
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setBundleRecipe(bundleRecipeRequestSet);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBundleProduct(true);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest1));

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(MerchantType.TD.name());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);

    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEligibleMerchantTypes", "TD");
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingMaxNumberOfSkus", 1);

    Mockito.when(xProductOutbound.getProductBasicDetails(Arrays.asList("DR6-00001-00001"))).thenReturn(new ArrayList<>());

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productCreationValidation.bundleProductValidation(productCreationRequest, profileResponse);
    });
  }

  @Test
  public void bundleProductValidation_validateBundleChildSkuStatusProductDontExistsTest() throws Exception {
    Set<BundleRecipeRequest> bundleRecipeRequestSet = new HashSet<>();
    bundleRecipeRequestSet.add(new BundleRecipeRequest(ITEM_SKU_2, 1));
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setBundleRecipe(bundleRecipeRequestSet);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBundleProduct(true);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest1));

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(MerchantType.TD.name());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);

    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEligibleMerchantTypes", "TD");
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingMaxNumberOfSkus", 1);

    Mockito.when(xProductOutbound.getProductBasicDetails(Arrays.asList("DR6-00001-00001")))
        .thenReturn(Arrays.asList(ProductBasicResponse.builder().productSku("DR6-00001-00001").productExists(false).build()));

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productCreationValidation.bundleProductValidation(productCreationRequest, profileResponse);
    });
  }

  @Test
  public void bundleProductValidation_validateBundleChildSkuStatusProductMarkForDeleteTest() throws Exception {
    Set<BundleRecipeRequest> bundleRecipeRequestSet = new HashSet<>();
    bundleRecipeRequestSet.add(new BundleRecipeRequest(ITEM_SKU_2, 1));
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setBundleRecipe(bundleRecipeRequestSet);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBundleProduct(true);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest1));

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(MerchantType.TD.name());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);

    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEligibleMerchantTypes", "TD");
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingMaxNumberOfSkus", 1);

    Mockito.when(xProductOutbound.getProductBasicDetails(Arrays.asList("DR6-00001-00001")))
        .thenReturn(Arrays.asList(ProductBasicResponse.builder().productSku("DR6-00001-00001").productExists(true).markForDelete(true).build()));

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productCreationValidation.bundleProductValidation(productCreationRequest, profileResponse);
    });
  }

  @Test
  public void bundleProductValidation_validateBundleChildSkuStatusProductsuspendedTest() throws Exception {
    Set<BundleRecipeRequest> bundleRecipeRequestSet = new HashSet<>();
    bundleRecipeRequestSet.add(new BundleRecipeRequest(ITEM_SKU_2, 1));
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    ProductItemCreationRequest productItemCreationRequest2 = new ProductItemCreationRequest();
    productItemCreationRequest2.setBundleRecipe(new HashSet<>());
    productItemCreationRequest1.setBundleRecipe(bundleRecipeRequestSet);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBundleProduct(true);
    productCreationRequest.setProductItemRequests(
        Arrays.asList(productItemCreationRequest1, productItemCreationRequest2));

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(MerchantType.TD.name());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);

    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEligibleMerchantTypes", "TD");
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingMaxNumberOfSkus", 1);

    Mockito.when(xProductOutbound.getProductBasicDetails(Arrays.asList("DR6-00001-00001")))
        .thenReturn(Arrays.asList(ProductBasicResponse.builder().productSku("DR6-00001-00001").productExists(true).suspended(true).build()));

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productCreationValidation.bundleProductValidation(productCreationRequest, profileResponse);
    });
  }

  @Test
  public void bundleProductValidation_validateBundleChildSkuStatusProductArchivedTest() throws Exception {
    Set<BundleRecipeRequest> bundleRecipeRequestSet = new HashSet<>();
    bundleRecipeRequestSet.add(new BundleRecipeRequest(ITEM_SKU_2, 1));
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setBundleRecipe(bundleRecipeRequestSet);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBundleProduct(true);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest1));

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(MerchantType.TD.name());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);

    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEligibleMerchantTypes", "TD");
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingMaxNumberOfSkus", 1);

    Mockito.when(xProductOutbound.getProductBasicDetails(Arrays.asList("DR6-00001-00001")))
        .thenReturn(Arrays.asList(ProductBasicResponse.builder().productSku("DR6-00001-00001").productExists(true).archived(true).build()));

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productCreationValidation.bundleProductValidation(productCreationRequest, profileResponse);
    });
  }

  @Test
  public void bundleProductValidation_validateBundleChildSkuByMerchantTypeTdSellerErrorTest() throws Exception {
    Set<BundleRecipeRequest> bundleRecipeRequestSet = new HashSet<>();
    bundleRecipeRequestSet.add(new BundleRecipeRequest(ITEM_SKU_2, 1));
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setBundleRecipe(bundleRecipeRequestSet);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBundleProduct(true);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest1));

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(MerchantType.TD.name());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);

    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEligibleMerchantTypes", "TD");
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingMaxNumberOfSkus", 1);

    Mockito.when(xProductOutbound.getProductBasicDetails(Arrays.asList("DR6-00001-00001")))
        .thenReturn(Arrays.asList(ProductBasicResponse.builder().productSku("DR6-00001-00001").productExists(true).build()));

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productCreationValidation.bundleProductValidation(productCreationRequest, profileResponse);
    });
  }

  @Test
  public void bundleProductValidation_EmptyItems() throws Exception {
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBundleProduct(true);
    productCreationRequest.setProductItemRequests(null);

    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEligibleMerchantTypes", "TD");
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingMaxNumberOfSkus", 1);
    ReflectionTestUtils.setField(productCreationValidation, "warehouseBomActivated", true);

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(MerchantType.TD.name());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productCreationValidation.bundleProductValidation(productCreationRequest, profileResponse);
    });

    Assertions.assertTrue(productCreationRequest.isBundleProduct());
  }

  @Test
  public void bundleProductValidation_validateBundleChildSkuByMerchantTypeCCSellerErrorTest() throws Exception {
    Set<BundleRecipeRequest> bundleRecipeRequestSet = new HashSet<>();
    bundleRecipeRequestSet.add(new BundleRecipeRequest(ITEM_SKU_2, 1));
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setBundleRecipe(bundleRecipeRequestSet);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBundleProduct(true);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest1));

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(MerchantType.CC.name());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);
    profileResponse.setBusinessPartnerCode("DR6-00002");

    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEligibleMerchantTypes", "TD,CC");
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingMaxNumberOfSkus", 1);

    Mockito.when(xProductOutbound.getProductBasicDetails(Arrays.asList("DR6-00001-00001")))
        .thenReturn(Arrays.asList(ProductBasicResponse.builder().productSku("DR6-00001-00001").productExists(true).build()));

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productCreationValidation.bundleProductValidation(productCreationRequest, profileResponse);
    });

  }

  @Test
  public void bundleProductValidation_validateBundleChildSkuByMerchantTypeCCSellerChildProductVariantDeletedTest()
      throws Exception {
    Set<BundleRecipeRequest> bundleRecipeRequestSet = new HashSet<>();
    bundleRecipeRequestSet.add(new BundleRecipeRequest(ITEM_SKU_2, 1));
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setBundleRecipe(bundleRecipeRequestSet);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBundleProduct(true);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest1));

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(MerchantType.CC.name());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);
    profileResponse.setBusinessPartnerCode("DR6-00001");

    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEligibleMerchantTypes", "TD,CC");
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingMaxNumberOfSkus", 1);

    Mockito.when(xProductOutbound.getProductBasicDetails(Arrays.asList("DR6-00001-00001")))
        .thenReturn(Arrays.asList(ProductBasicResponse.builder().productSku("DR6-00001-00001").productExists(true).build()));
    Mockito.when(xProductOutbound.getItemBasicDetailV2Response(Mockito.anyList(), Mockito.anyBoolean()))
        .thenReturn(Arrays.asList(ItemBasicDetailV2Response.builder().markForDelete(true).build()));

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productCreationValidation.bundleProductValidation(productCreationRequest, profileResponse);
    });
  }

  @Test
  public void bundleProductValidation_validateBundleChildSkuByMerchantTypeCCSellerTest() throws Exception {
    Set<BundleRecipeRequest> bundleRecipeRequestSet = new HashSet<>();
    bundleRecipeRequestSet.add(new BundleRecipeRequest(ITEM_SKU_2, 1));
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setBundleRecipe(bundleRecipeRequestSet);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBundleProduct(true);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest1));

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(MerchantType.CC.name());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);
    profileResponse.setBusinessPartnerCode("DR6-00001");

    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEligibleMerchantTypes", "TD,CC");
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingMaxNumberOfSkus", 1);

    Mockito.when(xProductOutbound.getProductBasicDetails(Arrays.asList("DR6-00001-00001"))).thenReturn(
        Arrays.asList(ProductBasicResponse.builder().productSku("DR6-00001-00001").productExists(true).build()));
    Mockito.when(xProductOutbound.getItemBasicDetailV2Response(Arrays.asList(ITEM_SKU_2), false))
        .thenReturn(Arrays.asList(new ItemBasicDetailV2Response()));


    productCreationValidation.bundleProductValidation(productCreationRequest, profileResponse);

    Assertions.assertTrue(productCreationRequest.isBundleProduct());
    Assertions.assertNotNull(productItemCreationRequest1.getBundleRecipe());
  }

  @Test
  public void bundleProductValidation_validateBundleChildSkuByMerchantTypeCCSellerItenSkuDontExistsTest() throws Exception {
    Set<BundleRecipeRequest> bundleRecipeRequestSet = new HashSet<>();
    bundleRecipeRequestSet.add(new BundleRecipeRequest(ITEM_SKU_2, 1));
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setBundleRecipe(bundleRecipeRequestSet);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBundleProduct(true);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest1));

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(MerchantType.CC.name());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);
    profileResponse.setBusinessPartnerCode("DR6-00001");

    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEligibleMerchantTypes", "TD,CC");
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingMaxNumberOfSkus", 1);

    Mockito.when(xProductOutbound.getProductBasicDetails(Arrays.asList("DR6-00001-00001"))).thenReturn(
        Arrays.asList(ProductBasicResponse.builder().productSku("DR6-00001-00001").productExists(true).build()));
    Mockito.when(xProductOutbound.getItemBasicDetailV2Response(Arrays.asList(ITEM_SKU_2), false))
        .thenReturn(new ArrayList<>());


    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productCreationValidation.bundleProductValidation(productCreationRequest, profileResponse);
    });
  }

  @Test
  public void bundleProductValidation_validateBundleChildSkuByMerchantTypeCCSellerQuantityLessThanZeroTest() throws Exception {
    Set<BundleRecipeRequest> bundleRecipeRequestSet = new HashSet<>();
    bundleRecipeRequestSet.add(new BundleRecipeRequest(ITEM_SKU_2, 0));
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setBundleRecipe(bundleRecipeRequestSet);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBundleProduct(true);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest1));

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(MerchantType.CC.name());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);
    profileResponse.setBusinessPartnerCode("DR6-00001");

    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingEligibleMerchantTypes", "TD,CC");
    ReflectionTestUtils.setField(productCreationValidation, "productBundlingMaxNumberOfSkus", 1);

    Mockito.when(xProductOutbound.getProductBasicDetails(Arrays.asList("DR6-00001-00001")))
        .thenReturn(Arrays.asList(ProductBasicResponse.builder().productSku("DR6-00001-00001").productExists(true).build()));

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productCreationValidation.bundleProductValidation(productCreationRequest, profileResponse);
    });
  }

  @Test
  public void negativeStockValidationTest() throws Exception {
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(false);
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setWholesalePriceActivated(false);
    pickupPointCreateRequest.setStock(-20);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(true);
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setProductBusinessPartnerAttributes(
        Collections.singletonList(productBusinessPartnerAttributeRequest));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
            MAX_STOCK_LIMIT, false, false, false, false);
      });
    } finally {
      deleteDummyFiles(productCreationRequest.getProductItemRequests());
      Mockito.verify(fileStorageService)
          .checkImageAvailability(Mockito.any(), Mockito.anyString());
      Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    }
  }

  @Test
  public void negativeStockValidationSwitchOffTest() throws Exception {
    ReflectionTestUtils.setField(productCreationValidation, "negativeStockValidation", false);
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(false);
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setWholesalePriceActivated(false);
    pickupPointCreateRequest.setStock(-20);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(true);
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setProductBusinessPartnerAttributes(
        Collections.singletonList(productBusinessPartnerAttributeRequest));
      productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
          MAX_STOCK_LIMIT, false, false, false, false);
      deleteDummyFiles(productCreationRequest.getProductItemRequests());
      Mockito.verify(fileStorageService)
          .checkImageAvailability(Mockito.any(), Mockito.anyString());
      Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
    }

  @Test
  public void negativeStockValidationFalseTest() throws Exception {
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(false);
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setWholesalePriceActivated(false);
    pickupPointCreateRequest.setStock(20);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(true);
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setProductBusinessPartnerAttributes(
        Collections.singletonList(productBusinessPartnerAttributeRequest));
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, false, false, false);
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
    Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
  }
  @Test
  public void cnc1pProductStatusValidationTest() throws Exception {
    ReflectionTestUtils.setField(productCreationValidation, "cncForWarehouseFeatureSwitch", false);
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setWholesalePriceActivated(false);
    pickupPointCreateRequest.setStock(20);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT);
    pickupPointCreateRequest.setCncActive(true);
    pickupPointCreateRequest.setCncDisplay(false);
    pickupPointCreateRequest.setCncBuyable(false);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(true);
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setProductBusinessPartnerAttributes(
        Collections.singletonList(productBusinessPartnerAttributeRequest));
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, false, false, false);
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
    Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
  }
  @Test
  public void cnc1pProductStatusValidationCncActiveFalseTest() throws Exception {
    ReflectionTestUtils.setField(productCreationValidation, "cncForWarehouseFeatureSwitch", false);
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setWholesalePriceActivated(false);
    pickupPointCreateRequest.setStock(20);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT);
    pickupPointCreateRequest.setCncActive(false);
    pickupPointCreateRequest.setCncDisplay(false);
    pickupPointCreateRequest.setCncBuyable(false);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(true);
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setProductBusinessPartnerAttributes(
        Collections.singletonList(productBusinessPartnerAttributeRequest));
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, false, false, false);
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
    Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
  }
  @Test
  public void cnc1pProductStatusValidationCncStatusOfflineTest() throws Exception {
    ReflectionTestUtils.setField(productCreationValidation, "cncForWarehouseFeatureSwitch", true);
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setWholesalePriceActivated(false);
    pickupPointCreateRequest.setStock(20);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT);
    pickupPointCreateRequest.setCncActive(false);
    pickupPointCreateRequest.setBuyable(true);
    pickupPointCreateRequest.setDisplay(true);
    pickupPointCreateRequest.setCncDisplay(false);
    pickupPointCreateRequest.setCncBuyable(false);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(true);
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setProductBusinessPartnerAttributes(
        Collections.singletonList(productBusinessPartnerAttributeRequest));
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, false, false, false);
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
    Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
  }
  @Test
  public void cnc1pProductStatusValidationInvalidStatusTest() throws Exception {
    ApplicationRuntimeException applicationRuntimeException = new ApplicationRuntimeException();
    ReflectionTestUtils.setField(productCreationValidation, "cncForWarehouseFeatureSwitch", true);
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
    try {
      ProductCreationRequest productCreationRequest = generateProductCreationRequest();
      generateDummyImageFiles(productCreationRequest.getProductItemRequests());
      PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
      pickupPointCreateRequest.setWholesalePriceActivated(false);
      pickupPointCreateRequest.setStock(20);
      pickupPointCreateRequest.setPickupPointId(PICKUP_POINT);
      pickupPointCreateRequest.setCncActive(false);
      pickupPointCreateRequest.setBuyable(true);
      pickupPointCreateRequest.setDisplay(true);
      pickupPointCreateRequest.setCncDisplay(false);
      pickupPointCreateRequest.setCncBuyable(true);
      productCreationRequest.getProductItemRequests().get(0).setPickupPoints(
          Collections.singletonList(pickupPointCreateRequest));
      productCreationRequest.getProductItemRequests().get(1).setPickupPoints(
          Collections.singletonList(pickupPointCreateRequest));
      productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(true);
      productCreationRequest.setFreeSample(true);
      productCreationRequest.setProductBusinessPartnerAttributes(
          Collections.singletonList(productBusinessPartnerAttributeRequest));
      productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
          MAX_STOCK_LIMIT, false, false, false, false);
      deleteDummyFiles(productCreationRequest.getProductItemRequests());
    }
    catch (ApplicationRuntimeException e) {
      applicationRuntimeException.setErrorMessage(e.getErrorMessage());
    } finally {
      Mockito.verify(fileStorageService).checkImageAvailability(Mockito.any(), Mockito.anyString());
      Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
      Assertions.assertEquals(ErrorCategory.VALIDATION.getMessage()
              + ApiErrorCode.INVALID_PRODUCT_ITEM_STATUS.getDesc(),
          applicationRuntimeException.getErrorMessage());
    }
  }
  @Test
  public void cnc1pProductStatusValidationCncAndShippingStatusTeaserTest() throws Exception {
    ReflectionTestUtils.setField(productCreationValidation, "cncForWarehouseFeatureSwitch", true);
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(false);
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setWholesalePriceActivated(false);
    pickupPointCreateRequest.setStock(20);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT);
    pickupPointCreateRequest.setCncActive(false);
    pickupPointCreateRequest.setBuyable(false);
    pickupPointCreateRequest.setDisplay(true);
    pickupPointCreateRequest.setCncDisplay(true);
    pickupPointCreateRequest.setCncBuyable(false);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(true);
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setProductBusinessPartnerAttributes(
        Collections.singletonList(productBusinessPartnerAttributeRequest));
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, false, false, false);
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
    Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
  }

  @Test
  public void cnc1pProductStatusValidationCncAndShippingStatusTeaserPreorderTest() throws Exception {
    ReflectionTestUtils.setField(productCreationValidation, "cncForWarehouseFeatureSwitch", true);
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, 1);
    PreOrderRequest request = PreOrderRequest.builder()
        .isPreOrder(true)
        .preOrderType("DATE")
        .preOrderValue(null)
        .preOrderDate(calendar.getTime())
        .build();
    productCreationRequest.setPreOrder(request);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setWholesalePriceActivated(false);
    pickupPointCreateRequest.setStock(20);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT);
    pickupPointCreateRequest.setCncActive(false);
    pickupPointCreateRequest.setBuyable(false);
    pickupPointCreateRequest.setDisplay(true);
    pickupPointCreateRequest.setCncDisplay(true);
    pickupPointCreateRequest.setCncBuyable(false);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(true);
    productCreationRequest.setFreeSample(false);
    productCreationRequest.setProductBusinessPartnerAttributes(
        Collections.singletonList(productBusinessPartnerAttributeRequest));
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, false, false, true);
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
    Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
  }

  @Test
  public void cnc1pProductStatusValidationCncAndShippingStatusTeaserPreorderMaxStockTest() throws Exception {
    ReflectionTestUtils.setField(productCreationValidation, "cncForWarehouseFeatureSwitch", true);
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, 1);
    PreOrderRequest orderRequest = PreOrderRequest.builder()
        .isPreOrder(true)
        .preOrderType("DATE")
        .preOrderValue(null)
        .preOrderDate(calendar.getTime())
        .build();
    productCreationRequest.setPreOrder(orderRequest);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setWholesalePriceActivated(false);
    pickupPointCreateRequest.setStock(1000000001);
    pickupPointCreateRequest.setPreOrderQuota(1000000001);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT);
    pickupPointCreateRequest.setCncActive(false);
    pickupPointCreateRequest.setBuyable(false);
    pickupPointCreateRequest.setDisplay(true);
    pickupPointCreateRequest.setCncDisplay(true);
    pickupPointCreateRequest.setCncBuyable(false);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(true);
    productCreationRequest.setFreeSample(false);
    productCreationRequest.setProductBusinessPartnerAttributes(
        Collections.singletonList(productBusinessPartnerAttributeRequest));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
          MAX_STOCK_LIMIT, false, false, false, true);
    });
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
    Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
  }

  @Test
  public void cnc1pProductStatusValidationCncAndShippingStatusTeaserPreorderOffMaxStockTest() throws Exception {
    ReflectionTestUtils.setField(productCreationValidation, "cncForWarehouseFeatureSwitch", true);
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(false);
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, 1);
    PreOrderRequest request = PreOrderRequest.builder()
        .isPreOrder(false)
        .preOrderType("DATE")
        .preOrderValue(null)
        .preOrderDate(calendar.getTime())
        .build();
    productCreationRequest.setPreOrder(request);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setWholesalePriceActivated(false);
    pickupPointCreateRequest.setStock(1000000001);
    pickupPointCreateRequest.setPreOrderQuota(0);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT);
    pickupPointCreateRequest.setCncActive(false);
    pickupPointCreateRequest.setBuyable(false);
    pickupPointCreateRequest.setDisplay(true);
    pickupPointCreateRequest.setCncDisplay(true);
    pickupPointCreateRequest.setCncBuyable(false);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(true);
    productCreationRequest.setFreeSample(false);
    productCreationRequest.setProductBusinessPartnerAttributes(
        Collections.singletonList(productBusinessPartnerAttributeRequest));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
          MAX_STOCK_LIMIT, false, false, false, true);
    });
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
    Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
  }

  @Test
  public void cnc1pProductStatusValidationCncAndShippingStatusTeaserPreorderNegativeStockTest() throws Exception {
    ReflectionTestUtils.setField(productCreationValidation, "cncForWarehouseFeatureSwitch", true);
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, 1);
    PreOrderRequest request = PreOrderRequest.builder()
        .isPreOrder(true)
        .preOrderType("DATE")
        .preOrderValue(null)
        .preOrderDate(calendar.getTime())
        .build();
    productCreationRequest.setPreOrder(request);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setWholesalePriceActivated(false);
    pickupPointCreateRequest.setStock(10);
    pickupPointCreateRequest.setPreOrderQuota(-10);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT);
    pickupPointCreateRequest.setCncActive(false);
    pickupPointCreateRequest.setBuyable(false);
    pickupPointCreateRequest.setDisplay(true);
    pickupPointCreateRequest.setCncDisplay(true);
    pickupPointCreateRequest.setCncBuyable(false);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(true);
    productCreationRequest.setFreeSample(false);
    productCreationRequest.setProductBusinessPartnerAttributes(
        Collections.singletonList(productBusinessPartnerAttributeRequest));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
          MAX_STOCK_LIMIT, false, false, false, true);
    });
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
    Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
  }

  @Test
  public void cnc1pProductStatusValidationCncAndShippingStatusTeaserPreorderOffNegativeStockTest() throws Exception {
    ReflectionTestUtils.setField(productCreationValidation, "cncForWarehouseFeatureSwitch", true);
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(false);
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, 1);
    PreOrderRequest request = PreOrderRequest.builder()
        .isPreOrder(false)
        .preOrderType("DATE")
        .preOrderValue(null)
        .preOrderDate(calendar.getTime())
        .build();
    productCreationRequest.setPreOrder(request);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setWholesalePriceActivated(false);
    pickupPointCreateRequest.setStock(-10);
    pickupPointCreateRequest.setPreOrderQuota(0);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT);
    pickupPointCreateRequest.setCncActive(false);
    pickupPointCreateRequest.setBuyable(false);
    pickupPointCreateRequest.setDisplay(true);
    pickupPointCreateRequest.setCncDisplay(true);
    pickupPointCreateRequest.setCncBuyable(false);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(true);
    productCreationRequest.setFreeSample(false);
    productCreationRequest.setProductBusinessPartnerAttributes(
        Collections.singletonList(productBusinessPartnerAttributeRequest));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
          MAX_STOCK_LIMIT, false, false, false, true);
    });
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
    Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
  }

  @Test
  public void cnc1pProductStatusValidationCncAndShippingStatusTeaserPreorderNullTest() throws Exception {
    ReflectionTestUtils.setField(productCreationValidation, "cncForWarehouseFeatureSwitch", true);
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setWholesalePriceActivated(false);
    pickupPointCreateRequest.setStock(20);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT);
    pickupPointCreateRequest.setCncActive(false);
    pickupPointCreateRequest.setBuyable(false);
    pickupPointCreateRequest.setDisplay(true);
    pickupPointCreateRequest.setCncDisplay(true);
    pickupPointCreateRequest.setCncBuyable(false);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(true);
    productCreationRequest.setFreeSample(false);
    productCreationRequest.setProductBusinessPartnerAttributes(
        Collections.singletonList(productBusinessPartnerAttributeRequest));
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, false, false, true);
    deleteDummyFiles(productCreationRequest.getProductItemRequests());
    Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
  }

  @Test
  public void cnc1pProductStatusValidationCncAndShippingStatusTeaserPreorderFalseTest() throws Exception {
    ReflectionTestUtils.setField(productCreationValidation, "cncForWarehouseFeatureSwitch", true);
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
    ProductCreationRequest productCreationRequest = generateProductCreationRequest();
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, 1);
    PreOrderRequest request = PreOrderRequest.builder()
        .isPreOrder(false)
        .preOrderType("DATE")
        .preOrderValue(null)
        .preOrderDate(calendar.getTime())
        .build();
    productCreationRequest.setPreOrder(request);
    generateDummyImageFiles(productCreationRequest.getProductItemRequests());
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setWholesalePriceActivated(false);
    pickupPointCreateRequest.setStock(20);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT);
    pickupPointCreateRequest.setCncActive(false);
    pickupPointCreateRequest.setBuyable(false);
    pickupPointCreateRequest.setDisplay(true);
    pickupPointCreateRequest.setCncDisplay(true);
    pickupPointCreateRequest.setCncBuyable(false);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(
        Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(0).setWholesalePriceActivated(true);
    productCreationRequest.setFreeSample(false);
    productCreationRequest.setProductBusinessPartnerAttributes(
        Collections.singletonList(productBusinessPartnerAttributeRequest));
    productCreationValidation.validateProduct(productCreationRequest, true, SOURCE_DIRECTORY, FINAL_SOURCE_DIRECTORY,
        MAX_STOCK_LIMIT, false, false, false, true);

    deleteDummyFiles(productCreationRequest.getProductItemRequests());
    Mockito.verify(fileStorageService)
        .checkImageAvailability(Mockito.any(), Mockito.anyString());
    Mockito.verify(fileStorageService).editImageNameIfGcsEnabled(Mockito.any(ProductCreationRequest.class));
  }

  @Test
  public void validateProductSizeChartAttributeTest() throws Exception {
    ReflectionTestUtils.setField(productCreationValidation, "valueTypeAdditionForDefiningAttributes", true);
    ReflectionTestUtils.setField(productCreationValidation, "sizeChartValueTypeDelimiter", "-");
    ProductCreationRequest productCreationRequest = getProductRequestForSizeChartValidation();
    Mockito.when(attributeRepository.findDetailById(ID)).thenReturn(getAttributeResponse());
    productCreationValidation.validateProductSizeChartAttribute(productCreationRequest, Constants.MTA_API_CHANNEL_ID);
    Assertions.assertEquals("S", productCreationRequest.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .getAllowedAttributeValue().getValue());
    Assertions.assertEquals("S",
        productCreationRequest.getProductItemRequests().get(0).getProductItemAttributeValueRequests().get(0)
            .getValue());
    Assertions.assertEquals("S",
        productCreationRequest.getProductItemRequests().get(0).getAttributesMap().get(ATTRIBUTE_CODE));
    Mockito.verify(attributeRepository).findDetailById(ID);
  }

  @Test
   void validateProductSizeChartAttributeSwitchOffTest() throws Exception {
    ReflectionTestUtils.setField(productCreationValidation, "valueTypeAdditionForDefiningAttributes", false);
    ReflectionTestUtils.setField(productCreationValidation, "sizeChartValueTypeDelimiter", "-");
    ProductCreationRequest productCreationRequest = getProductRequestForSizeChartValidation();
    Mockito.when(attributeRepository.findDetailById(ID)).thenReturn(getAttributeResponse());
    productCreationValidation.validateProductSizeChartAttribute(productCreationRequest, Constants.DEFAULT_CHANNEL_ID);
    Assertions.assertEquals("S", productCreationRequest.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .getAllowedAttributeValue().getValue());
    Assertions.assertEquals("S",
        productCreationRequest.getProductItemRequests().get(0).getProductItemAttributeValueRequests().get(0)
            .getValue());
    Assertions.assertEquals("S",
        productCreationRequest.getProductItemRequests().get(0).getAttributesMap().get(ATTRIBUTE_CODE));
  }

  @Test
   void validateProductSizeChartAttributeNotSellerApiRequestTest() throws Exception {
    ReflectionTestUtils.setField(productCreationValidation, "valueTypeAdditionForDefiningAttributes", true);
    ReflectionTestUtils.setField(productCreationValidation, "sizeChartValueTypeDelimiter", "-");
    ProductCreationRequest productCreationRequest = getProductRequestForSizeChartValidation();
    Mockito.when(attributeRepository.findDetailById(ID)).thenReturn(getAttributeResponse());
    productCreationValidation.validateProductSizeChartAttribute(productCreationRequest, Constants.DEFAULT_CHANNEL_ID);
    Assertions.assertEquals("S", productCreationRequest.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .getAllowedAttributeValue().getValue());
    Assertions.assertEquals("S",
        productCreationRequest.getProductItemRequests().get(0).getProductItemAttributeValueRequests().get(0)
            .getValue());
    Assertions.assertEquals("S",
        productCreationRequest.getProductItemRequests().get(0).getAttributesMap().get(ATTRIBUTE_CODE));
  }

  @Test
   void validateBasePriceForB2BSeller_WhenB2BSellerAndFeatureEnabled_ShouldSetB2BPrice() {
    // Given
    ProductCreationRequest request = new ProductCreationRequest();
    List<ProductItemCreationRequest> productItemRequests = new ArrayList<>();
    ProductItemCreationRequest productItemRequest = new ProductItemCreationRequest();
    List<PickupPointCreateRequest> pickupPoints = new ArrayList<>();
    
    PickupPointCreateRequest pickupPoint = new PickupPointCreateRequest();
    pickupPoint.setPrice(1000.00);
    pickupPoints.add(pickupPoint);
    
    productItemRequest.setPickupPoints(pickupPoints);
    productItemRequests.add(productItemRequest);
    request.setProductItemRequests(productItemRequests);

    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setSalesChannel(Arrays.asList(Constants.B2B_SELLER_CHANNEL));
    profileResponse.setCompany(company);

    ReflectionTestUtils.setField(productCreationValidation, "b2bBasePriceBackFillEnabled", true);

    // When
    productCreationValidation.validateBasePriceForB2BSeller(request, profileResponse);

    // Then
    PickupPointCreateRequest updatedPickupPoint = request.getProductItemRequests().get(0).getPickupPoints().get(0);
    Assertions.assertNotNull(updatedPickupPoint.getB2bFields());
    Assertions.assertEquals(1000.00, updatedPickupPoint.getB2bFields().getPrice());
  }

  @Test
   void validateBasePriceForB2BSeller_WhenNotB2BSeller_ShouldNotModifyRequest() {
    // Given
    ProductCreationRequest request = new ProductCreationRequest();
    List<ProductItemCreationRequest> productItemRequests = new ArrayList<>();
    ProductItemCreationRequest productItemRequest = new ProductItemCreationRequest();
    List<PickupPointCreateRequest> pickupPoints = new ArrayList<>();
    
    PickupPointCreateRequest pickupPoint = new PickupPointCreateRequest();
    pickupPoint.setPrice(1000.00);
    pickupPoints.add(pickupPoint);
    
    productItemRequest.setPickupPoints(pickupPoints);
    productItemRequests.add(productItemRequest);
    request.setProductItemRequests(productItemRequests);

    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setSalesChannel(Arrays.asList("OTHER_CHANNEL"));
    profileResponse.setCompany(company);

    ReflectionTestUtils.setField(productCreationValidation, "b2bBasePriceBackFillEnabled", true);

    // When
    productCreationValidation.validateBasePriceForB2BSeller(request, profileResponse);

    // Then
    PickupPointCreateRequest updatedPickupPoint = request.getProductItemRequests().get(0).getPickupPoints().get(0);
    Assertions.assertNull(updatedPickupPoint.getB2bFields());
  }

  @Test
   void validateBasePriceForB2BSeller_WhenFeatureDisabled_ShouldNotModifyRequest() {
    ProductCreationRequest request = new ProductCreationRequest();
    List<ProductItemCreationRequest> productItemRequests = new ArrayList<>();
    ProductItemCreationRequest productItemRequest = new ProductItemCreationRequest();
    List<PickupPointCreateRequest> pickupPoints = new ArrayList<>();
    PickupPointCreateRequest pickupPoint = new PickupPointCreateRequest();
    pickupPoint.setPrice(1000.00);
    pickupPoints.add(pickupPoint);
    
    productItemRequest.setPickupPoints(pickupPoints);
    productItemRequests.add(productItemRequest);
    request.setProductItemRequests(productItemRequests);

    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setSalesChannel(Arrays.asList(Constants.B2B_SELLER_CHANNEL));
    profileResponse.setCompany(company);
    ReflectionTestUtils.setField(productCreationValidation, "b2bBasePriceBackFillEnabled", false);
    productCreationValidation.validateBasePriceForB2BSeller(request, profileResponse);
    PickupPointCreateRequest updatedPickupPoint = request.getProductItemRequests().get(0).getPickupPoints().get(0);
    Assertions.assertNull(updatedPickupPoint.getB2bFields());
  }

  @Test
    void validateBasePriceForB2BSeller_WhenB2BFieldsIsNull_ShouldNotEvaluatePrice() {
      // Given
      ProductCreationRequest request = new ProductCreationRequest();
      List<ProductItemCreationRequest> productItemRequests = new ArrayList<>();
      ProductItemCreationRequest productItemRequest = new ProductItemCreationRequest();
      List<PickupPointCreateRequest> pickupPoints = new ArrayList<>();
      
      PickupPointCreateRequest pickupPoint = new PickupPointCreateRequest();
      pickupPoint.setPrice(1000.00);
      pickupPoint.setB2bFields(null); // Explicitly set b2bFields to null to test short-circuit
      pickupPoints.add(pickupPoint);
      
      productItemRequest.setPickupPoints(pickupPoints);
      productItemRequests.add(productItemRequest);
      request.setProductItemRequests(productItemRequests);

      ProfileResponse profileResponse = new ProfileResponse();
      CompanyDTO company = new CompanyDTO();
      company.setSalesChannel(Arrays.asList(Constants.B2B_SELLER_CHANNEL));
      profileResponse.setCompany(company);

      ReflectionTestUtils.setField(productCreationValidation, "b2bBasePriceBackFillEnabled", true);

      // When
      productCreationValidation.validateBasePriceForB2BSeller(request, profileResponse);

      // Then
      PickupPointCreateRequest updatedPickupPoint = request.getProductItemRequests().get(0).getPickupPoints().get(0);
      Assertions.assertNotNull(updatedPickupPoint.getB2bFields());
      Assertions.assertEquals(1000.00, updatedPickupPoint.getB2bFields().getPrice());
  }

  @Test
    void validateBasePriceForB2BSeller_WhenB2BFieldsAndPriceExist_ShouldNotModify() {
      // Given
      ProductCreationRequest request = new ProductCreationRequest();
      List<ProductItemCreationRequest> productItemRequests = new ArrayList<>();
      ProductItemCreationRequest productItemRequest = new ProductItemCreationRequest();
      List<PickupPointCreateRequest> pickupPoints = new ArrayList<>();
      
      PickupPointCreateRequest pickupPoint = new PickupPointCreateRequest();
      pickupPoint.setPrice(1000.00);
      B2bDetailsDTO b2bDetails = new B2bDetailsDTO(2000.00, false, false, false); // Set non-null price
      pickupPoint.setB2bFields(b2bDetails);
      pickupPoints.add(pickupPoint);
      
      productItemRequest.setPickupPoints(pickupPoints);
      productItemRequests.add(productItemRequest);
      request.setProductItemRequests(productItemRequests);

      ProfileResponse profileResponse = new ProfileResponse();
      CompanyDTO company = new CompanyDTO();
      company.setSalesChannel(Arrays.asList(Constants.B2B_SELLER_CHANNEL));
      profileResponse.setCompany(company);

      ReflectionTestUtils.setField(productCreationValidation, "b2bBasePriceBackFillEnabled", true);

      // When
      productCreationValidation.validateBasePriceForB2BSeller(request, profileResponse);

      // Then
      PickupPointCreateRequest updatedPickupPoint = request.getProductItemRequests().get(0).getPickupPoints().get(0);
      Assertions.assertNotNull(updatedPickupPoint.getB2bFields());
      Assertions.assertEquals(2000.00, updatedPickupPoint.getB2bFields().getPrice()); // Price should remain unchanged
  }

  @Test
    void validateBasePriceForB2BSeller_WhenB2BFieldsExistWithNullPrice_ShouldSetPrice() {
    // Given
    ProductCreationRequest request = new ProductCreationRequest();
    List<ProductItemCreationRequest> productItemRequests = new ArrayList<>();
    ProductItemCreationRequest productItemRequest = new ProductItemCreationRequest();
    List<PickupPointCreateRequest> pickupPoints = new ArrayList<>();
    
    PickupPointCreateRequest pickupPoint = new PickupPointCreateRequest();
    pickupPoint.setPrice(1000.00);
    B2bDetailsDTO b2bDetails = new B2bDetailsDTO(null, false, false, false); // Explicitly set null price
    pickupPoint.setB2bFields(b2bDetails);
    pickupPoints.add(pickupPoint);
    
    productItemRequest.setPickupPoints(pickupPoints);
    productItemRequests.add(productItemRequest);
    request.setProductItemRequests(productItemRequests);

    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setSalesChannel(Arrays.asList(Constants.B2B_SELLER_CHANNEL));
    profileResponse.setCompany(company);

    ReflectionTestUtils.setField(productCreationValidation, "b2bBasePriceBackFillEnabled", true);

    // When
    productCreationValidation.validateBasePriceForB2BSeller(request, profileResponse);

    // Then
    PickupPointCreateRequest updatedPickupPoint = request.getProductItemRequests().get(0).getPickupPoints().get(0);
    Assertions.assertNotNull(updatedPickupPoint.getB2bFields());
    Assertions.assertEquals(1000.00, updatedPickupPoint.getB2bFields().getPrice());
  }

  @Test
   void validateBasePriceForB2BSeller_WhenB2BFieldsExistWithoutPrice_ShouldSetPrice() {
    ProductCreationRequest request = new ProductCreationRequest();
    List<ProductItemCreationRequest> productItemRequests = new ArrayList<>();
    ProductItemCreationRequest productItemRequest = new ProductItemCreationRequest();
    List<PickupPointCreateRequest> pickupPoints = new ArrayList<>();
    
    PickupPointCreateRequest pickupPoint = new PickupPointCreateRequest();
    pickupPoint.setPrice(1000.00);
    B2bDetailsDTO b2bDetails = new B2bDetailsDTO();
    pickupPoint.setB2bFields(b2bDetails);
    pickupPoints.add(pickupPoint);
    
    productItemRequest.setPickupPoints(pickupPoints);
    productItemRequests.add(productItemRequest);
    request.setProductItemRequests(productItemRequests);

    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setSalesChannel(Arrays.asList(Constants.B2B_SELLER_CHANNEL));
    profileResponse.setCompany(company);

    ReflectionTestUtils.setField(productCreationValidation, "b2bBasePriceBackFillEnabled", true);

    // When
    productCreationValidation.validateBasePriceForB2BSeller(request, profileResponse);

    // Then
    PickupPointCreateRequest updatedPickupPoint = request.getProductItemRequests().get(0).getPickupPoints().get(0);
    Assertions.assertNotNull(updatedPickupPoint.getB2bFields());
    Assertions.assertEquals(1000.00, updatedPickupPoint.getB2bFields().getPrice());
  }

  private ProductCreationRequest getProductRequestForSizeChartValidation() {
    AttributeRequest attributeRequest1 = new AttributeRequest();
    attributeRequest1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attributeRequest1.setId(ID);
    attributeRequest1.setAttributeCode(ATTRIBUTE_CODE);

    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setId(UUID.randomUUID().toString());
    attributeRequest2.setAttributeCode(ATTRIBUTE_CODE_2);
    attributeRequest2.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);

    AllowedAttributeValueRequest allowedAttributeValueRequest1 = new AllowedAttributeValueRequest("S", 1, "10001");
    allowedAttributeValueRequest1.setId(ALLOWED_ATTRIBUTE_VALUE_ID);
    ProductAttributeValueRequest productAttributeValueRequest1 = new ProductAttributeValueRequest();
    productAttributeValueRequest1.setAllowedAttributeValue(allowedAttributeValueRequest1);

    AllowedAttributeValueRequest allowedAttributeValueRequest2 = new AllowedAttributeValueRequest("UK-S", 1, "10001");
    allowedAttributeValueRequest2.setId(UUID.randomUUID().toString());
    ProductAttributeValueRequest productAttributeValueRequest2 = new ProductAttributeValueRequest();
    productAttributeValueRequest2.setAllowedAttributeValue(allowedAttributeValueRequest2);

    ProductAttributeValueRequest productAttributeValueRequest3 = new ProductAttributeValueRequest();

    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest1);
    productAttributeRequest1.setProductAttributeValues(
        Arrays.asList(productAttributeValueRequest1, productAttributeValueRequest2, productAttributeValueRequest3));

    ProductAttributeRequest productAttributeRequest2 = new ProductAttributeRequest();
    productAttributeRequest2.setAttribute(attributeRequest1);

    ProductAttributeRequest productAttributeRequest3 = new ProductAttributeRequest();
    productAttributeRequest3.setAttribute(attributeRequest2);

    ProductAttributeRequest productAttributeRequest4 = new ProductAttributeRequest();


    ProductItemAttributeValueRequest productItemAttributeValueRequest1 = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest1.setAttribute(attributeRequest1);
    productItemAttributeValueRequest1.setValue("S");

    ProductItemAttributeValueRequest productItemAttributeValueRequest2 = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest2.setAttribute(attributeRequest1);
    productItemAttributeValueRequest2.setValue("M");

    ProductItemAttributeValueRequest productItemAttributeValueRequest3 = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest3.setAttribute(attributeRequest2);

    ProductItemAttributeValueRequest productItemAttributeValueRequest4 = new ProductItemAttributeValueRequest();

    TreeMap<String, String> attributesMap = new TreeMap<>(Map.of(ATTRIBUTE_CODE, "S", ATTRIBUTE_CODE_2, "M"));

    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setProductItemAttributeValueRequests(
        Arrays.asList(productItemAttributeValueRequest1, productItemAttributeValueRequest2,
            productItemAttributeValueRequest3, productItemAttributeValueRequest4));
    productItemCreationRequest1.setAttributesMap(attributesMap);

    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductAttributes(
        Arrays.asList(productAttributeRequest1, productAttributeRequest2, productAttributeRequest3,
            productAttributeRequest4));
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest1));

    return productCreationRequest;
  }

  private AttributeResponse getAttributeResponse() {
    AllowedAttributeValueResponse allowedAttributeValueResponse1 = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse1.setId(ALLOWED_ATTRIBUTE_VALUE_ID);
    allowedAttributeValueResponse1.setValue("S");
    allowedAttributeValueResponse1.setValueType("UK");

    AllowedAttributeValueResponse allowedAttributeValueResponse2 = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse2.setValue("M");

    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAllowedAttributeValues(
        Arrays.asList(allowedAttributeValueResponse1, allowedAttributeValueResponse2));

    return attributeResponse;
  }
}
