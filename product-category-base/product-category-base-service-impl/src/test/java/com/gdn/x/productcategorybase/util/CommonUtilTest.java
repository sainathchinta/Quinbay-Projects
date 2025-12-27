package com.gdn.x.productcategorybase.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.ProductMigrationType;
import com.gdn.x.productcategorybase.domain.event.model.CommonImageBackfillingEventModel;
import com.gdn.x.productcategorybase.domain.event.model.InternalProductHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductSuitabilityAttributeModel;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SizeChartUpdateEventModel;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.MigrationPayload;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateWipRequest;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductMigrationRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.AiGeneratedFieldsResponse;
import com.gdn.x.productcategorybase.dto.response.BasicSizeChartDetailMapResponse;
import com.gdn.x.productcategorybase.dto.response.DimensionsAndUomResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataItemResponse;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.Origin;
import com.gdn.x.productcategorybase.entity.ProductItemUomInfo;
import com.gdn.x.productcategorybase.entity.ProductMigration;
import com.gdn.x.productcategorybase.entity.RestrictedKeyword;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

import com.gdn.inventory.dto.WarehouseMasterSKUEvent;
import com.gdn.x.productcategorybase.ProductPublishEventType;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemDomainEventModel;
import com.gdn.x.productcategorybase.dto.AttributeValueDTO;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.ProductAndItemLevelUpdatesDTO;
import com.gdn.x.productcategorybase.dto.ProductPublishUpdateDTO;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductItemImageRequest;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.entity.SizeChart;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationWip;
import com.gdn.x.productcategorybase.enums.UpdatedFields;

public class CommonUtilTest {

  private static final String ID = "id";
  private static final String PRODUCT_NAME = "productName";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String SKU_CODE_1 = "sku_code_1";
  private static final String SKU_CODE_2 = "sku_code_2";
  private static final String SKU_CODE_3 = "sku_code_3";
  private static final String SKU_CODE_4 = "sku_code_4";
  private static final String UPC_CODE_1  = "upc_code_1";
  private static final String ATTRIBUTE_CODE_1 = "attribute_code_1";
  private static final String ATTRIBUTE_CODE_2  = "attribute_code_2";
  private static final String DESCRIPTIVE_ATTRIBUTE_VALUE_1  = "descriptive_attribute_value_1";
  private static final String PREDEFINIED_ATTRIBUTE_VALUE_ID_1  = "predefinied_attribute_value_id_1";
  private static final String CATEGORY_CODE_1  = "category_code_1";
  private static final String BRAND  = "brand";
  private static final String PRODUCT_CODE = "MTA-00001";
  private static final String ITEM_CODE = "MTA-00001-00001";
  private static final String PREDEFINIED_ATTRIBUTE_VALUE_1  = "predefinied_attribute_value_1";
  private static final String PREDEFINIED_ATTRIBUTE_VALUE_2  = "predefinied_attribute_value_2";
  private static final String IMAGE_PATH_1  = "imagePath1";
  private static final String IMAGE_PATH_2  = "imagePath2";
  private static final double LENGTH = 10.343000045;
  private static final double WIDTH = 11.343000045;
  private static final double WEIGHT = 12.343000045;
  private static final double HEIGHT = 13.343000045;
  private static final double LENGTH_ROUNDED_OFF = 10.343;
  private static final double WIDTH_ROUNDED_OFF = 11.343;
  private static final double WEIGHT_ROUNDED_OFF = 12.343;
  private static final double HEIGHT_ROUNDED_OFF = 13.343;
  private static final String KEYWORD = "keyword";
  private static final String SIZE_CHART_NAME = "sizeChartName";
  private static final String SIZE_CHART_CODE = "sizeChartCode";
  private static final String STORE_ID = "10001";
  private static final String SIZE_CHART_ATTRIBUTE_CODE = "sizeChartAttributeCode";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String VALUE = "VALUE";
  private static final String VALUE_TYPE = "VALUE_TYPE";
  private static final String VALUE_TYPE_WITH_VALUE = "VALUE_TYPE-VALUE";
  public static final String ATTRIBUTE_NAME = "Color";

  private static final String VALUE_1 = "value1";
  private static final String VALUE_2 = "value2";
  private static final String PREDEFINED_ID_1 = "id1";
  private static final String PREDEFINED_ID_2 = "id2";
  private static final String SEQUENCE = "123";
  private static final String PREFIX_SIZE_CHART_CODE = "SC";
  private static final String HYPHEN = "-";
  private static final String PADDING_STRING = "0";
  private static final String SIZE_CHART_CODE_SIZE = "5";
  private static final String BRAND_CODE = "BRAND001";
  private static final String SELLER_CODE = "SELLER001";
  private static final String USERNAME = "testuser";
  private static final String REQUEST_ID = "REQ001";
  private static final String ERROR_MESSAGE = "Test error message";
  private static final String MIGRATION_TYPE = "TYPE001";
  private static final String MIGRATION_PAYLOAD = "PAYLOAD001";
  private static final String DOCUMENT_LINK_1 = "doc1";
  private static final String DOCUMENT_LINK_2 = "doc2";
  private static final String STATUS_COMPLETED = "COMPLETED";
  private static final String STATUS_FAILED = "FAILED";
  private static final String STATUS_IN_REVIEW = "IN_REVIEW";
  private static final String DATE_FORMAT = "yyyy-MM-dd";
  private static final String ATTRIBUTE_VALUE = "attributeValue";
  private static final String OLD_VALUE = "oldValue";
  private static final String VIDEO_NAME_1 = "videoName1";
  private static final String VIDEO_NAME_2 = "videoName2";
  public static final String AI_GENRATED_FIELDS_STRING =
      "{\"aiGeneratedCategory\":true,\"aiGeneratedBrand\":true}\n";

  private ProductDomainEventModel productDomainEventModel;
  private ProductItemDomainEventModel productItemDomainEventModel1;
  private ProductItemDomainEventModel productItemDomainEventModel2;
  private ProductImage productImage1;
  private ProductImage productImage2;
  private ProductItemImage productItemImage;
  private ProductItem productItem1;
  private ProductItem productItem2;
  private ProductItem productItem3;
  private ProductItem productItem4;
  private ProductAttribute productAttribute1;
  private ProductAttribute productAttribute2;
  private ProductAttributeValue productAttributeValue1;
  private ProductAttributeValue productAttributeValue2;
  private Product product1;
  private Product product2;
  private ProductCategory productCategory1;
  private Category category1;
  private PredefinedAllowedAttributeValue predefinedAllowedAttributeValue1;
  private PredefinedAllowedAttributeValue predefinedAllowedAttributeValue2;
  private String validPayload;
  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    productDomainEventModel = new ProductDomainEventModel();
    productItemDomainEventModel1 = new ProductItemDomainEventModel();
    productItemDomainEventModel2 = new ProductItemDomainEventModel();
    productItemDomainEventModel1.setSkuCode(SKU_CODE_1);
    productItemDomainEventModel2.setSkuCode(SKU_CODE_2);
    productDomainEventModel.setProductItems(Arrays.asList(productItemDomainEventModel1, productItemDomainEventModel2));
    productImage1 = new ProductImage();
    productImage2 = new ProductImage();
    productItemImage = new ProductItemImage();
    productItem1 = new ProductItem();
    productItem1.setSkuCode(SKU_CODE_1);
    productItem2 = new ProductItem();
    productItem2.setSkuCode(SKU_CODE_2);
    productItem3 = new ProductItem();
    productItem3.setSkuCode(SKU_CODE_3);
    productItem4 = new ProductItem();
    productItem4.setSkuCode(SKU_CODE_4);
    productAttribute1 = new ProductAttribute();
    productAttribute1.setAttribute(new Attribute());
    productAttribute1.getAttribute().setAttributeCode(ATTRIBUTE_CODE_1);
    productAttribute2 = new ProductAttribute();
    productAttribute2.setAttribute(new Attribute());
    productAttribute2.getAttribute().setAttributeCode(ATTRIBUTE_CODE_2);
    productAttributeValue1 = new ProductAttributeValue();
    productAttributeValue1.setId(ID);
    productAttributeValue2 = new ProductAttributeValue();
    productAttribute1.setProductAttributeValues(Arrays.asList(productAttributeValue1));
    productAttribute2.setProductAttributeValues(Arrays.asList(productAttributeValue2));
    product1 = new Product();
    product2 = new Product();
    product1.setProductAttributes(Arrays.asList(productAttribute1));
    category1 = new Category();
    category1.setCategoryCode(CATEGORY_CODE_1);
    productCategory1 = new ProductCategory();
    productCategory1.setCategory(category1);
    product1.setProductCategories(Arrays.asList(productCategory1));
    product2.setProductAttributes(Arrays.asList(productAttribute2));
    predefinedAllowedAttributeValue1 = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue1.setValue(PREDEFINIED_ATTRIBUTE_VALUE_1);
    predefinedAllowedAttributeValue2 = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue2.setValue(PREDEFINIED_ATTRIBUTE_VALUE_2);
    validPayload =
      "[{\"attributeId\":\"attr1\",\"mustShowOnCustomerSide\":true,\"attributeValue\":\"value1\",\"attributeType\":\"type1\"},"
        + "{\"attributeId\":\"attr2\",\"mustShowOnCustomerSide\":false,\"attributeValue\":\"value2\",\"attributeType\":\"type2\"}]";

  }

  @Test
  public void productDomainEventModelFlagUpdateBasedOnProductOrItemLevelChangeProductLevelDataUpdatedTest() {
    CommonUtil.productDomainEventModelFlagUpdateBasedOnProductOrItemLevelChange(productDomainEventModel,
        true, new HashSet<>(Arrays.asList(SKU_CODE_1)));
    Assertions.assertNull(productDomainEventModel.getEventTypes());
  }

  @Test
  public void productDomainEventModelFlagUpdateBasedOnProductOrItemLevelChangeItemLevelDataUpdatedTest() {
    CommonUtil.productDomainEventModelFlagUpdateBasedOnProductOrItemLevelChange(productDomainEventModel,
        false, new HashSet<>(Arrays.asList(SKU_CODE_1)));
    Assertions.assertTrue(productDomainEventModel.getEventTypes().contains(ProductPublishEventType
        .PUBLISH_SPECIFIC_ITEM_DATA_CHANGE_EVENT.name()));
    Assertions.assertTrue(productItemDomainEventModel1.isPublishL4());
    Assertions.assertFalse(productItemDomainEventModel2.isPublishL4());
  }

  @Test
  public void productDomainEventModelFlagUpdateBasedOnProductOrItemLevelChangeItemLevelDataUpdatedWithEventTypePresentTest() {
    productDomainEventModel.setEventTypes(new HashSet<>(Arrays.asList(ProductPublishEventType
        .PUBLISH_ITEM_PICKUP_POINT_DATA_CHANGE_EVENT.name())));
    CommonUtil.productDomainEventModelFlagUpdateBasedOnProductOrItemLevelChange(productDomainEventModel,
        false, null);
    Assertions.assertTrue(productDomainEventModel.getEventTypes().contains(ProductPublishEventType
        .PUBLISH_SPECIFIC_ITEM_DATA_CHANGE_EVENT.name()));
    Assertions.assertFalse(productItemDomainEventModel1.isPublishL4());
    Assertions.assertFalse(productItemDomainEventModel2.isPublishL4());
  }

  @Test
  public void commonImageUpdateWithProductImageTest() {
    boolean commonImage = CommonUtil.commonImageUpdateWithProductImage(null, true);
    Assertions.assertTrue(commonImage);
  }

  @Test
  public void commonImageUpdateWithProductImageFalseTest() {
    boolean commonImage = CommonUtil.commonImageUpdateWithProductImage(null, false);
    Assertions.assertFalse(commonImage);
  }

  @Test
  public void commonImageUpdateWithProductImageCommonImageTrueTest() {
    productImage1.setCommonImage(true);
    productImage2.setCommonImage(false);
    boolean commonImage = CommonUtil.commonImageUpdateWithProductImage(Arrays.asList(productImage1, productImage2), false);
    Assertions.assertTrue(commonImage);
  }

  @Test
  public void commonImageUpdateWithProductImageCommonImageFalseTest() {
    productImage2.setCommonImage(false);
    boolean commonImage = CommonUtil.commonImageUpdateWithProductImage(Arrays.asList(productImage2), false);
    Assertions.assertFalse(commonImage);
  }

  @Test
  public void itemLevelImagesUpdatedTest() {
    boolean commonImage = CommonUtil.itemLevelImagesUpdated(null, true);
    Assertions.assertTrue(commonImage);
  }

  @Test
  public void itemLevelImagesUpdatedFalseTest() {
    boolean commonImage = CommonUtil.itemLevelImagesUpdated(null, false);
    Assertions.assertFalse(commonImage);
  }

  @Test
  public void itemLevelImagesTrueCaseUpdated() {
    productItemImage.setCommonImage(false);
    boolean itemLevelImagesUpdated = CommonUtil.itemLevelImagesUpdated(Arrays.asList(productItemImage), false);
    Assertions.assertTrue(itemLevelImagesUpdated);
  }

  @Test
  public void itemLevelImagesFalseCaseUpdated() {
    productItemImage.setCommonImage(true);
    boolean itemLevelImagesUpdated = CommonUtil.itemLevelImagesUpdated(Arrays.asList(productItemImage), false);
    Assertions.assertFalse(itemLevelImagesUpdated);
  }

  @Test
  public void attributeValueUpdatedForNonVariantCreatingProductAtrributeTest() {
    boolean productAttributeValueUpdated = CommonUtil
        .attributeValueUpdatedForNonVariantCreatingProductAtrribute(productAttributeValue1, productAttributeValue2);
    Assertions.assertFalse(productAttributeValueUpdated);

  }

  @Test
  public void attributeValueUpdatedForNonVariantCreatingProductAtrributePreDefinedValueUpdatedTest() {
    productAttributeValue2.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue2);
    boolean productAttributeValueUpdated = CommonUtil
        .attributeValueUpdatedForNonVariantCreatingProductAtrribute(productAttributeValue1, productAttributeValue2);
    Assertions.assertTrue(productAttributeValueUpdated);
  }

  @Test
  public void attributeValueUpdatedForNonVariantCreatingProductAtrributeDefinedValueUpdatedTest() {
    productAttributeValue1.setDescriptiveAttributeValue(PREDEFINIED_ATTRIBUTE_VALUE_ID_1);
    productAttributeValue2.setDescriptiveAttributeValue(DESCRIPTIVE_ATTRIBUTE_VALUE_1);
    boolean productAttributeValueUpdated = CommonUtil
        .attributeValueUpdatedForNonVariantCreatingProductAtrribute(productAttributeValue1, productAttributeValue2);
    Assertions.assertTrue(productAttributeValueUpdated);
  }

  @Test
  public void productAttributeValueForNonVariantCreatingProductAttributeTest() {
    ProductAttributeValue productAttributeValue = CommonUtil
        .productAttributeValueForNonVariantCreatingProductAttribute(productAttribute1);
    Assertions.assertEquals(productAttributeValue1, productAttributeValue);
  }

  @Test
  public void productAttributeValueForNonVariantCreatingProductAttributeMfdTrueAttributeValueTest() {
    productAttribute1.getProductAttributeValues().get(0).setMarkForDelete(true);
    ProductAttributeValue productAttributeValue = CommonUtil
        .productAttributeValueForNonVariantCreatingProductAttribute(productAttribute1);
    Assertions.assertEquals(productAttributeValue, new ProductAttributeValue());
  }

  @Test
  public void productAttributeValueForNonVariantCreatingProductAttributeNullAttributeValueTest() {
    productAttribute1.setProductAttributeValues(null);
    ProductAttributeValue productAttributeValue = CommonUtil
        .productAttributeValueForNonVariantCreatingProductAttribute(productAttribute1);
    Assertions.assertEquals(productAttributeValue, new ProductAttributeValue());
  }

  @Test
  public void skuCodesOfUpdatedItemsEmptyItemsTest() {
    Set<String> skuCodesOfUpdatedItems = CommonUtil
        .skuCodesOfUpdatedItems(null, null, false).getRight();
    Assertions.assertEquals(skuCodesOfUpdatedItems, new HashSet<>());
  }

  @Test
  public void skuCodesOfUpdatedItemsTest() {
    productItem1.setSkuCode(SKU_CODE_1);
    productItem2.setSkuCode(SKU_CODE_2);
    productItem1.setUpcCode(UPC_CODE_1);
    productItem3.setSkuCode(SKU_CODE_1);
    productItem4.setSkuCode(SKU_CODE_2);
    Set<String> skuCodesOfUpdatedItems = CommonUtil
        .skuCodesOfUpdatedItems(Arrays.asList(productItem1, productItem2), Arrays.asList(productItem3, productItem4), false).getRight();
    Assertions.assertTrue(skuCodesOfUpdatedItems.contains(SKU_CODE_1));
    Assertions.assertFalse(skuCodesOfUpdatedItems.contains(SKU_CODE_2));
  }

  @Test
  public void skuCodesOfUpdatedItemsAddAndDeleteVariantTest() {
    productItem1.setSkuCode(SKU_CODE_1);
    productItem2.setSkuCode(SKU_CODE_2);
    productItem1.setUpcCode(UPC_CODE_1);
    productItem3.setSkuCode(SKU_CODE_1);
    productItem4.setSkuCode(SKU_CODE_2);
    Set<String> skuCodesOfUpdatedItems = CommonUtil
        .skuCodesOfUpdatedItems(Arrays.asList(productItem1), Arrays.asList( productItem4), false).getRight();
    Assertions.assertTrue(skuCodesOfUpdatedItems.contains(SKU_CODE_1));
    Assertions.assertTrue(skuCodesOfUpdatedItems.contains(SKU_CODE_2));
  }

  @Test
  public void nonVariantCreatingProductAtrributeWithNullAttributeTest() {
    productAttribute1.setMarkForDelete(false);
    productAttribute1.setAttribute(null);
    boolean nonVariantCreatingProductAtrribute = CommonUtil.nonVariantCreatingProductAtrribute(productAttribute1);
    Assertions.assertFalse(nonVariantCreatingProductAtrribute);
  }

  @Test
  public void nonVariantCreatingProductAtrributeWithMarkForDeleteTrueTest() {
    productAttribute1.setMarkForDelete(true);
    boolean nonVariantCreatingProductAtrribute = CommonUtil.nonVariantCreatingProductAtrribute(productAttribute1);
    Assertions.assertFalse(nonVariantCreatingProductAtrribute);
  }

  @Test
  public void nonVariantCreatingProductAtrributeWitVariantCreatingFalseTest() {
    productAttribute1.setMarkForDelete(false);
    productAttribute1.getAttribute().setVariantCreation(true);
    boolean nonVariantCreatingProductAtrribute = CommonUtil.nonVariantCreatingProductAtrribute(productAttribute1);
    Assertions.assertFalse(nonVariantCreatingProductAtrribute);
  }

  @Test
  public void nonVariantCreatingProductAtrributeTest() {
    productAttribute1.setMarkForDelete(false);
    productAttribute1.setAttribute(new Attribute());
    productAttribute1.getAttribute().setVariantCreation(false);
    boolean nonVariantCreatingProductAtrribute = CommonUtil.nonVariantCreatingProductAtrribute(productAttribute1);
    Assertions.assertTrue(nonVariantCreatingProductAtrribute);
  }

  @Test
  public void nonVariantCreatingAttributeUpdatedNoUpdateTest() {
    Map<String, ProductAttribute> productAttributeMap1 = new HashMap<>();
    Map<String, ProductAttribute> productAttributeMap2 = new HashMap<>();
    productAttributeMap1.put(ATTRIBUTE_CODE_1, productAttribute1);
    productAttributeMap2.put(ATTRIBUTE_CODE_1, productAttribute1);
    boolean nonVariantCreatingProductAtrribute = CommonUtil
        .nonVariantCreatingAttributeUpdated(productAttributeMap1, productAttributeMap2);
    Assertions.assertFalse(nonVariantCreatingProductAtrribute);
  }

  @Test
  public void nonVariantCreatingAttributeUpdatedTest() {
    productAttributeValue1.setDescriptiveAttributeValue(PREDEFINIED_ATTRIBUTE_VALUE_ID_1);
    Map<String, ProductAttribute> productAttributeMap1 = new HashMap<>();
    Map<String, ProductAttribute> productAttributeMap2 = new HashMap<>();
    productAttributeMap1.put(ATTRIBUTE_CODE_1, productAttribute1);
    productAttributeMap2.put(ATTRIBUTE_CODE_1, productAttribute2);
    boolean nonVariantCreatingProductAtrribute = CommonUtil
        .nonVariantCreatingAttributeUpdated(productAttributeMap1, productAttributeMap2);
    Assertions.assertTrue(nonVariantCreatingProductAtrribute);
  }

  @Test
  public void nonVariantCreatingAttributeUpdatedNewAttributeAddedTest() {
    productAttributeValue1.setDescriptiveAttributeValue(PREDEFINIED_ATTRIBUTE_VALUE_ID_1);
    Map<String, ProductAttribute> productAttributeMap1 = new HashMap<>();
    Map<String, ProductAttribute> productAttributeMap2 = new HashMap<>();
    productAttributeMap1.put(ATTRIBUTE_CODE_2, productAttribute1);
    productAttributeMap1.put(ATTRIBUTE_CODE_1, productAttribute2);
    productAttributeMap2.put(ATTRIBUTE_CODE_1, productAttribute2);
    boolean nonVariantCreatingProductAtrribute = CommonUtil
        .nonVariantCreatingAttributeUpdated(productAttributeMap1, productAttributeMap2);
    Assertions.assertTrue(nonVariantCreatingProductAtrribute);
  }

  @Test
  public void nonVariantCreatingAttributeUpdatedWithProductsTest() {
    product2.setProductAttributes(Arrays.asList(productAttribute1));
    boolean nonVariantCreatingAttributeUpdated = CommonUtil.nonVariantCreatingAttributeUpdated(product1, product2);
    Assertions.assertFalse(nonVariantCreatingAttributeUpdated);
  }

  @Test
  public void nonVariantCreatingAttributeUpdatedWithProductsFalseTest() {
    product1.setProductAttributes(Arrays.asList(productAttribute1, productAttribute2));
    boolean nonVariantCreatingAttributeUpdated = CommonUtil.nonVariantCreatingAttributeUpdated(product1, product2);
    Assertions.assertTrue(nonVariantCreatingAttributeUpdated);
  }

  @Test
  public void nonVariantCreatingAttributeUpdatedWithProductsUpdatedTrueTest() {
    boolean nonVariantCreatingAttributeUpdated = CommonUtil.nonVariantCreatingAttributeUpdated(product1, product2);
    Assertions.assertTrue(nonVariantCreatingAttributeUpdated);
  }

  @Test
  public void contentUpdateAtProductLevelTest() {
    boolean contentUpdateAtProductLevel = CommonUtil.contentUpdateAtProductLevel(product1, product1);
    Assertions.assertFalse(contentUpdateAtProductLevel);
  }

  @Test
  public void contentUpdateAtProductLevelNonVariantCreatingAttributeUpdatedTest() {
    product2.setProductCategories(Arrays.asList(productCategory1));
    boolean contentUpdateAtProductLevel = CommonUtil.contentUpdateAtProductLevel(product1, product2);
    Assertions.assertTrue(contentUpdateAtProductLevel);
  }

  private void updateDimentionsOfProduct(Product product, double value) {
    product.setWeight(value);
    product.setHeight(value);
    product.setShippingWeight(value);
    product.setWidth(value);
    product.setLength(value);
  }

  @Test
  public void contentUpdateAtProductLevelDimensionUpdatedTest() {
    updateDimentionsOfProduct(product1, WEIGHT);
    updateDimentionsOfProduct(product2, HEIGHT);
    product2.setProductCategories(Arrays.asList(productCategory1));
    boolean contentUpdateAtProductLevel = CommonUtil.contentUpdateAtProductLevel(product1, product2);
    Assertions.assertTrue(contentUpdateAtProductLevel);
  }

  @Test
  public void contentUpdateAtProductLevelBrandUpdatedTest() {
    product2.setBrand(BRAND);
    product2.setProductCategories(Arrays.asList(productCategory1));
    boolean contentUpdateAtProductLevel = CommonUtil.contentUpdateAtProductLevel(product1, product2);
    Assertions.assertTrue(contentUpdateAtProductLevel);
  }

  @Test
  public void productAndItemLevelUpdatesTest() {
    ProductAndItemLevelUpdatesDTO productAndItemLevelUpdatesDTO = CommonUtil.productAndItemLevelUpdates(product1, product1);
    Assertions.assertFalse(productAndItemLevelUpdatesDTO.isProductLevelDataUpdated());
    Assertions.assertEquals(productAndItemLevelUpdatesDTO.getUpdatedItemSkuCodes(), new HashSet<>());
  }

  @Test
  public void validateAndSetVideoToRequestTest() {
    ProductRequest productRequest = new ProductRequest();
    productRequest.setVideoUpdated(false);
    product1.setVideo(VIDEO_NAME_1);
    product2.setVideo(VIDEO_NAME_2);
    CommonUtil.validateAndSetVideoToRequest(product1, product2, productRequest);
    Assertions.assertEquals(product1.getVideo(), product2.getVideo());
  }

  @Test
  public void validateAndSetVideoToRequestTest_videoUpdated() {
    ProductRequest productRequest = new ProductRequest();
    productRequest.setVideoUpdated(true);
    product1.setVideo(VIDEO_NAME_1);
    product2.setVideo(VIDEO_NAME_2);
    CommonUtil.validateAndSetVideoToRequest(product1, product2, productRequest);
    Assertions.assertNotEquals(product1.getVideo(), product2.getVideo());
  }

  @Test
  public void updateProductPublishUpdateDtoCommonImageTrueProductImageImageTest() {
    ProductAndItemImageRequest productAndItemImageRequest = new ProductAndItemImageRequest();
    productAndItemImageRequest.setProductCode(PRODUCT_CODE);
    List<Image> imageList = new ArrayList<>();
    Image image = new Image();
    image.setCommonImage(true);
    imageList.add(image);
    productAndItemImageRequest.setProductImages(imageList);
    ProductPublishUpdateDTO productPublishUpdateDTO = CommonUtil.updateProductPublishUpdateDTO(productAndItemImageRequest);
    Assertions.assertTrue(productPublishUpdateDTO.isProductLevelDataUpdated());
  }

  @Test
  public void updateProductPublishUpdateDtoCommonImageFalseProductItemImageImageTest() {
    ProductAndItemImageRequest productAndItemImageRequest = new ProductAndItemImageRequest();
    productAndItemImageRequest.setProductCode(PRODUCT_CODE);
    List<Image> commonImageList = new ArrayList<>();
    Image commonImage = new Image();
    commonImage.setCommonImage(false);
    commonImageList.add(commonImage);
    productAndItemImageRequest.setProductImages(commonImageList);
    List<Image> imageList = new ArrayList<>();
    Image image = new Image();
    image.setCommonImage(false);
    imageList.add(image);
    List<ProductItemImageRequest> productItemImageRequests = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    productItemImageRequest.setItemImages(imageList);
    productItemImageRequest.setSkuCode(SKU_CODE_1);
    productItemImageRequests.add(productItemImageRequest);
    productAndItemImageRequest.setProductItemImages(productItemImageRequests);
    ProductPublishUpdateDTO productPublishUpdateDTO = CommonUtil.updateProductPublishUpdateDTO(productAndItemImageRequest);
    for (String skuCode : productPublishUpdateDTO.getUpdatedItemSkuCodes()) {
      Assertions.assertEquals(SKU_CODE_1, skuCode);
    }
  }

  @Test
  public void updateProductPublishUpdateDtoCommonImageTrueProductItemImageImageTest() {
    ProductAndItemImageRequest productAndItemImageRequest = new ProductAndItemImageRequest();
    productAndItemImageRequest.setProductCode(PRODUCT_CODE);
    List<Image> commonImageList = new ArrayList<>();
    Image commonImage = new Image();
    commonImage.setCommonImage(false);
    commonImageList.add(commonImage);
    productAndItemImageRequest.setProductImages(commonImageList);
    List<Image> imageList = new ArrayList<>();
    Image image = new Image();
    image.setCommonImage(true);
    imageList.add(image);
    List<ProductItemImageRequest> productItemImageRequests = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    productItemImageRequest.setItemImages(imageList);
    productItemImageRequest.setSkuCode(SKU_CODE_1);
    productItemImageRequests.add(productItemImageRequest);
    productAndItemImageRequest.setProductItemImages(productItemImageRequests);
    ProductPublishUpdateDTO productPublishUpdateDTO = CommonUtil.updateProductPublishUpdateDTO(productAndItemImageRequest);
    Assertions.assertTrue(productPublishUpdateDTO.getUpdatedItemSkuCodes().isEmpty());
  }

  @Test
  public void isPredefinedAllowedAttributeValueUpdatedWithBothNullTest() {
    boolean predefinedAllowedAttributeValueUpdated =
        CommonUtil.isPredefinedAllowedAttributeValueUpdated(null, null);
    Assertions.assertFalse(predefinedAllowedAttributeValueUpdated);
  }

  @Test
  public void isPredefinedAllowedAttributeValueUpdatedAddedTest() {
    boolean predefinedAllowedAttributeValueUpdated =
        CommonUtil.isPredefinedAllowedAttributeValueUpdated(null, predefinedAllowedAttributeValue1);
    Assertions.assertTrue(predefinedAllowedAttributeValueUpdated);
  }

  @Test
  public void isPredefinedAllowedAttributeValueUpdatedDeletedTest() {
    boolean predefinedAllowedAttributeValueUpdated =
        CommonUtil.isPredefinedAllowedAttributeValueUpdated(predefinedAllowedAttributeValue1, null);
    Assertions.assertTrue(predefinedAllowedAttributeValueUpdated);
  }

  @Test
  public void isPredefinedAllowedAttributeValueUpdatedTrueTest() {
    boolean predefinedAllowedAttributeValueUpdated =
        CommonUtil.isPredefinedAllowedAttributeValueUpdated(predefinedAllowedAttributeValue2,
            predefinedAllowedAttributeValue1);
    Assertions.assertTrue(predefinedAllowedAttributeValueUpdated);
  }

  @Test
  public void isPredefinedAllowedAttributeValueUpdatedFalseTest() {
    boolean predefinedAllowedAttributeValueUpdated =
        CommonUtil.isPredefinedAllowedAttributeValueUpdated(predefinedAllowedAttributeValue1,
            predefinedAllowedAttributeValue1);
    Assertions.assertFalse(predefinedAllowedAttributeValueUpdated);
  }

  @Test
  public void getSkusForWhichImageUpdatedTest() {
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(IMAGE_PATH_1);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setLocationPath(IMAGE_PATH_2);
    ProductItemImage productItemImage3 = new ProductItemImage();
    productItemImage3.setMarkForDelete(true);
    ProductItemImage productItemImage4 = new ProductItemImage();
    productItemImage4.setLocationPath(IMAGE_PATH_1);

    ProductItem productItem1 = new ProductItem();
    productItem1.setSkuCode(SKU_CODE_1);
    productItem1.setProductItemImages(Arrays.asList(productItemImage1));

    ProductItem productItem2 = new ProductItem();
    productItem2.setSkuCode(SKU_CODE_1);
    productItem2.setProductItemImages(Arrays.asList(productItemImage2, productItemImage3));

    Pair<Boolean, Set<String>> result =
        CommonUtil.getSkusForWhichImageUpdated(Arrays.asList(productItem2), Arrays.asList(productItem1));
    Assertions.assertFalse(result.getLeft());
    Assertions.assertTrue(result.getRight().contains(SKU_CODE_1));

    productItemImage1.setLocationPath(IMAGE_PATH_2);
    productItemImage1.setMarkForDelete(true);
    result =
        CommonUtil.getSkusForWhichImageUpdated(Arrays.asList(productItem2), Arrays.asList(productItem1));
    Assertions.assertFalse(result.getLeft());
    Assertions.assertTrue(result.getRight().contains(SKU_CODE_1));

    productItemImage1.setLocationPath(IMAGE_PATH_2);
    productItemImage1.setMarkForDelete(false);
    productItemImage1.setMainImages(true);
    result =
        CommonUtil.getSkusForWhichImageUpdated(Arrays.asList(productItem2), Arrays.asList(productItem1));
    Assertions.assertFalse(result.getLeft());
    Assertions.assertTrue(result.getRight().contains(SKU_CODE_1));

    productItemImage1.setLocationPath(IMAGE_PATH_2);
    productItemImage1.setMainImages(false);
    result =
        CommonUtil.getSkusForWhichImageUpdated(Arrays.asList(productItem2), Arrays.asList(productItem1));
    Assertions.assertFalse(result.getLeft());
    Assertions.assertTrue(result.getRight().isEmpty());

    productItemImage1.setLocationPath(IMAGE_PATH_2);
    productItemImage1.setMarkForDelete(true);
    productItemImage1.setCommonImage(true);
    productItemImage4.setLocationPath(IMAGE_PATH_2);
    productItemImage4.setMarkForDelete(true);
    productItem1.setProductItemImages(Arrays.asList(productItemImage4, productItemImage1));
    result =
        CommonUtil.getSkusForWhichImageUpdated(Arrays.asList(productItem2), Arrays.asList(productItem1));
    Assertions.assertTrue(result.getLeft());
    Assertions.assertFalse(result.getRight().isEmpty());

    productItemImage1.setLocationPath(IMAGE_PATH_2);
    productItemImage1.setMarkForDelete(true);
    productItemImage1.setCommonImage(true);
    productItemImage4.setLocationPath(IMAGE_PATH_2);
    productItemImage4.setMarkForDelete(true);
    productItem1.setProductItemImages(Arrays.asList(productItemImage1, productItemImage4));
    result =
        CommonUtil.getSkusForWhichImageUpdated(Arrays.asList(productItem2), Arrays.asList(productItem1));
    Assertions.assertTrue(result.getLeft());
    Assertions.assertTrue(result.getRight().isEmpty());

    productItemImage1.setLocationPath(IMAGE_PATH_1);
    productItemImage1.setMarkForDelete(false);
    productItemImage1.setCommonImage(true);
    productItemImage4.setLocationPath(IMAGE_PATH_1);
    productItem1.setProductItemImages(Arrays.asList(productItemImage4, productItemImage1));
    result =
        CommonUtil.getSkusForWhichImageUpdated(Arrays.asList(productItem2), Arrays.asList(productItem1));
    Assertions.assertTrue(result.getLeft());
    Assertions.assertFalse(result.getRight().isEmpty());

    productItemImage1.setLocationPath(IMAGE_PATH_1);
    productItemImage1.setMarkForDelete(false);
    productItemImage1.setCommonImage(true);
    productItemImage4.setLocationPath(IMAGE_PATH_1);
    productItem1.setProductItemImages(Arrays.asList(productItemImage1, productItemImage4));
    result =
        CommonUtil.getSkusForWhichImageUpdated(Arrays.asList(productItem2), Arrays.asList(productItem1));
    Assertions.assertTrue(result.getLeft());
    Assertions.assertTrue(result.getRight().isEmpty());
  }

  @Test
  public void updateDimensionsTest() {
    Product product = new Product();
    WarehouseMasterSKUEvent warehouseMasterSKUEvent =
        WarehouseMasterSKUEvent.builder().length(LENGTH).width(WIDTH).weight(WEIGHT).height(HEIGHT).build();
    CommonUtil.updateDimensions(product, warehouseMasterSKUEvent);
    Assertions.assertEquals(LENGTH, product.getLength().doubleValue(), 0);
    Assertions.assertEquals(WIDTH, product.getWidth().doubleValue(), 0);
    Assertions.assertEquals(WEIGHT, product.getWeight().doubleValue(), 0);
    Assertions.assertEquals(HEIGHT, product.getHeight().doubleValue(), 0);
  }

  @Test
  public void validateProductDimension_SwitchEnabled() {
    WarehouseMasterSKUEvent warehouseMasterSKUEvent =
        WarehouseMasterSKUEvent.builder().length(LENGTH).width(WIDTH).weight(WEIGHT).height(HEIGHT).build();
    CommonUtil.validateProductDimension(warehouseMasterSKUEvent, true);
    Assertions.assertEquals(LENGTH_ROUNDED_OFF, warehouseMasterSKUEvent.getLength().doubleValue(), 0);
    Assertions.assertEquals(WIDTH_ROUNDED_OFF, warehouseMasterSKUEvent.getWidth().doubleValue(), 0);
    Assertions.assertEquals(WEIGHT_ROUNDED_OFF, warehouseMasterSKUEvent.getWeight().doubleValue(), 0);
    Assertions.assertEquals(HEIGHT_ROUNDED_OFF, warehouseMasterSKUEvent.getHeight().doubleValue(), 0);
  }

  @Test
  public void validateProductDimension_SwitchEnabledDimension0() {
    WarehouseMasterSKUEvent warehouseMasterSKUEvent =
        WarehouseMasterSKUEvent.builder().length(0.0).width(0.0).weight(0.0).height(0.0).build();
    CommonUtil.validateProductDimension(warehouseMasterSKUEvent, true);
    Assertions.assertEquals(0.0, warehouseMasterSKUEvent.getLength().doubleValue(), 0);
    Assertions.assertEquals(0.0, warehouseMasterSKUEvent.getWidth().doubleValue(), 0);
    Assertions.assertEquals(0.0, warehouseMasterSKUEvent.getWeight().doubleValue(), 0);
    Assertions.assertEquals(0.0, warehouseMasterSKUEvent.getHeight().doubleValue(), 0);
  }

  @Test
  public void validateProductDimension_SwitchDisnabled() {
    WarehouseMasterSKUEvent warehouseMasterSKUEvent =
        WarehouseMasterSKUEvent.builder().length(LENGTH).width(WIDTH).weight(WEIGHT).height(WEIGHT).build();
    CommonUtil.validateProductDimension(warehouseMasterSKUEvent, false);
    Assertions.assertEquals(LENGTH, warehouseMasterSKUEvent.getLength().doubleValue(), 0);
    Assertions.assertEquals(WIDTH, warehouseMasterSKUEvent.getWidth().doubleValue(), 0);
    Assertions.assertEquals(WEIGHT, warehouseMasterSKUEvent.getWeight().doubleValue(), 0);
    Assertions.assertEquals(WEIGHT, warehouseMasterSKUEvent.getHeight().doubleValue(), 0);
  }

  @Test
  public void updateMainImageToUniqueInCaseOfMultipleMainImagesNoActiveMainImageTest() {
    CommonUtil.updateMainImageToUniqueInCaseOfMultipleMainImages(productItem1);
    Assertions.assertEquals(productItem1.getProductItemImages().size(), 0);
  }

  @Test
  public void getUpdatedFieldsTest3(){
    product2.setDescription(new byte[]{1,2});
    product2.setBrand(PRODUCT_CODE);
    product2.setName(PRODUCT_CODE);
    productCategory1 = new ProductCategory();
    Category category = new Category();
    category.setCategoryCode("New_Category");
    productCategory1.setCategory(category);
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCategory(new Category());
    productCategory.getCategory().setCategoryCode(CATEGORY_CODE_1);
    productCategory.setMarkForDelete(true);
    product2.setProductCategories(Arrays.asList(productCategory, productCategory1));
    Set<String> updatedFields = CommonUtil.getUpdatedFields(product1, product2);
    Assertions.assertTrue(updatedFields.containsAll(
        Set.of(UpdatedFields.BRAND_UPDATE.name(), UpdatedFields.NAME_UPDATE.name(),
            UpdatedFields.CATEGORY_UPDATE.name(), UpdatedFields.DESCRIPTION_UPDATE.name())));
  }

  @Test
  void getUpdatedFieldsSavedProductCategoryNotPresentTest(){
    product2.setDescription(new byte[]{1,2});
    product2.setBrand(PRODUCT_CODE);
    product2.setName(PRODUCT_CODE);
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCategory(new Category());
    productCategory.getCategory().setCategoryCode(CATEGORY_CODE_1);
    productCategory.setMarkForDelete(true);
    product1.setProductCategories(new ArrayList<>());
    Set<String> updatedFields = CommonUtil.getUpdatedFields(product1, product2);
    Assertions.assertFalse(updatedFields.containsAll(
      Set.of(UpdatedFields.BRAND_UPDATE.name(), UpdatedFields.NAME_UPDATE.name(),
        UpdatedFields.CATEGORY_UPDATE.name(), UpdatedFields.DESCRIPTION_UPDATE.name())));
  }

  @Test
  void getUpdatedFieldsNewProductCategoryNotPresentTest(){
    product2.setDescription(new byte[]{1,2});
    product2.setBrand(PRODUCT_CODE);
    product2.setName(PRODUCT_CODE);
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCategory(new Category());
    productCategory.getCategory().setCategoryCode(CATEGORY_CODE_1);
    productCategory.setMarkForDelete(true);
    Set<String> updatedFields = CommonUtil.getUpdatedFields(product1, product2);
    Assertions.assertFalse(updatedFields.containsAll(
      Set.of(UpdatedFields.BRAND_UPDATE.name(), UpdatedFields.NAME_UPDATE.name(),
        UpdatedFields.CATEGORY_UPDATE.name(), UpdatedFields.DESCRIPTION_UPDATE.name())));
  }

  @Test
  public void isAttributeEligibleForConcatenationTest() {
    AttributeValueDTO attributeValueDTO = new AttributeValueDTO();
    Assertions.assertFalse(CommonUtil.isAttributeEligibleForConcatenation(false, false, attributeValueDTO));
    Assertions.assertFalse(CommonUtil.isAttributeEligibleForConcatenation(true, false, attributeValueDTO));
    Assertions.assertFalse(CommonUtil.isAttributeEligibleForConcatenation(true, true, attributeValueDTO));
    attributeValueDTO.setValue(PREDEFINIED_ATTRIBUTE_VALUE_1);
    Assertions.assertFalse(CommonUtil.isAttributeEligibleForConcatenation(true, true, attributeValueDTO));
    attributeValueDTO.setValueType(DESCRIPTIVE_ATTRIBUTE_VALUE_1);
    Assertions.assertTrue(CommonUtil.isAttributeEligibleForConcatenation(true, true, attributeValueDTO));
  }

  @Test
  public void populateHistoryModelTest() {
    RestrictedKeyword restrictedKeyword = new RestrictedKeyword();
    restrictedKeyword.setId(ID);
    restrictedKeyword.setKeyword(KEYWORD);
    RestrictedKeywordHistoryEventModel model =
      CommonUtil.populateHistoryModel(restrictedKeyword, Boolean.TRUE);
    Assertions.assertEquals(ID, model.getKeywordId());
    Assertions.assertEquals(Boolean.TRUE.toString(),
      model.getRestrictedKeywordActivityHistoryList().get(0).getNewValue());
    Assertions.assertEquals(Boolean.FALSE.toString(),
      model.getRestrictedKeywordActivityHistoryList().get(0).getOldValue());
  }

  @Test
  public void populateHistoryModelExistingTrueTest() {
    RestrictedKeyword restrictedKeyword = new RestrictedKeyword();
    restrictedKeyword.setId(ID);
    restrictedKeyword.setKeyword(KEYWORD);
    restrictedKeyword.setValidateByDs(Boolean.TRUE);
    RestrictedKeywordHistoryEventModel model =
      CommonUtil.populateHistoryModel(restrictedKeyword, Boolean.FALSE);
    Assertions.assertEquals(ID, model.getKeywordId());
    Assertions.assertEquals(Boolean.FALSE.toString(),
      model.getRestrictedKeywordActivityHistoryList().get(0).getNewValue());
    Assertions.assertEquals(Boolean.TRUE.toString(),
      model.getRestrictedKeywordActivityHistoryList().get(0).getOldValue());
  }

  @Test
  public void populateHistoryModelNewValueNullTest() {
    RestrictedKeyword restrictedKeyword = new RestrictedKeyword();
    restrictedKeyword.setId(ID);
    restrictedKeyword.setKeyword(KEYWORD);
    restrictedKeyword.setValidateByDs(Boolean.TRUE);
    RestrictedKeywordHistoryEventModel model =
      CommonUtil.populateHistoryModel(restrictedKeyword, null);
    Assertions.assertEquals(ID, model.getKeywordId());
    Assertions.assertEquals(Boolean.FALSE.toString(),
      model.getRestrictedKeywordActivityHistoryList().get(0).getNewValue());
    Assertions.assertEquals(Boolean.TRUE.toString(),
      model.getRestrictedKeywordActivityHistoryList().get(0).getOldValue());
  }

  @Test
  public void generateBasicSizeChartDetailResponseTest() {
    SizeChart sizeChart = new SizeChart();
    sizeChart.setName(SIZE_CHART_NAME);
    sizeChart.setSizeChartCode(SIZE_CHART_CODE);
    sizeChart.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    BasicSizeChartDetailMapResponse basicSizeChartDetailResponse =
        CommonUtil.generateBasicSizeChartDetailResponse(Arrays.asList(sizeChart));
   Assertions.assertEquals(SIZE_CHART_NAME,
        basicSizeChartDetailResponse.getBasicSizeChartDetailResponseMap().get(SIZE_CHART_CODE)
            .getSizeChartName());
   Assertions.assertEquals(BUSINESS_PARTNER_CODE,
        basicSizeChartDetailResponse.getBasicSizeChartDetailResponseMap().get(SIZE_CHART_CODE)
            .getBusinessPartnerCode());
  }

  @Test
  public void getCategoryEligibleForSizeChartAdditionMapTest() {
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE_1);
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(SIZE_CHART_ATTRIBUTE_CODE);
    categoryAttribute.setAttribute(attribute);
    category.setCategoryAttributes(Arrays.asList(categoryAttribute));
    Map<String, Boolean> resultMap =
        CommonUtil.getCategoryEligibleForSizeChartAdditionMap(SIZE_CHART_ATTRIBUTE_CODE,
            Arrays.asList(category));
   Assertions.assertTrue(resultMap.get(CATEGORY_CODE_1));
  }

  @Test
  public void getSizeChartUpdateEventModelTest() {
    SizeChart sizeChart = new SizeChart();
    sizeChart.setStoreId(STORE_ID);
    sizeChart.setSizeChartCode(SIZE_CHART_CODE);
    SizeChartUpdateEventModel sizeChartUpdateEventModel = CommonUtil.getSizeChartUpdateEventModel(sizeChart);
    Assertions.assertEquals(STORE_ID, sizeChartUpdateEventModel.getStoreId());
    Assertions.assertEquals(SIZE_CHART_CODE, sizeChartUpdateEventModel.getSizeChartCode());
  }

  @Test
  public void testGetUpdatedFieldForDescription_WhenDescriptionsAreSame() {
    product1.setDescription("description".getBytes());
    Set<String> updatedFields = CommonUtil.getUpdatedFieldForDescription(product1, product2);
    Assertions.assertNotNull(updatedFields);
  }

  @Test
  public void testGetUpdatedFieldForDescription_WhenDescriptionsTest() {
    Set<String> updatedFields = CommonUtil.getUpdatedFieldForDescription(product1, product2);
    Assertions.assertNotNull(updatedFields);
  }

  @Test
  public void convertToIntArrayTest() {
    String input = "";
    Integer[] config = CommonUtil.convertToIntArray(input);
    Assertions.assertNotNull(config);
  }

  @Test
  public void convertToIntArrayNullTest() {
    Integer[] config = CommonUtil.convertToIntArray(null);
    Assertions.assertNotNull(config);
  }

  @Test
  public void convertToIntArrayEmptyTest() {
    String input = "14,,3";
    Integer[] config = CommonUtil.convertToIntArray(input);
    Assertions.assertNotNull(config);
  }

  @Test
  public void getAttributeValueBasedOnSizeChartTest() {
    String value = VALUE;
    String sizeChartValueTypeDelimiter = Constants.HYPHEN;
    String result = CommonUtil.getAttributeValueBasedOnSizeChart(false, false, value, null,
        AttributeType.PREDEFINED_ATTRIBUTE.name(), sizeChartValueTypeDelimiter);
    Assertions.assertEquals(value, result);
    result = CommonUtil.getAttributeValueBasedOnSizeChart(true, false, value, null,
        AttributeType.PREDEFINED_ATTRIBUTE.name(), sizeChartValueTypeDelimiter);
    Assertions.assertEquals(value, result);
    result =
        CommonUtil.getAttributeValueBasedOnSizeChart(true, true, value, null, AttributeType.PREDEFINED_ATTRIBUTE.name(),
            sizeChartValueTypeDelimiter);
    Assertions.assertEquals(value, result);
    result =
        CommonUtil.getAttributeValueBasedOnSizeChart(true, true, value, null, AttributeType.DEFINING_ATTRIBUTE.name(),
            sizeChartValueTypeDelimiter);
    Assertions.assertEquals(value, result);
    result = CommonUtil.getAttributeValueBasedOnSizeChart(true, true, value, VALUE_TYPE,
        AttributeType.DEFINING_ATTRIBUTE.name(), sizeChartValueTypeDelimiter);
    Assertions.assertEquals(VALUE_TYPE_WITH_VALUE, result);
  }

  @Test
  void testToCommonImageBackfillingEventModel_ValidPayload()
    throws JsonProcessingException {
    ProductMigration productMigration = new ProductMigration();
    productMigration.setMigrationType(ProductMigrationType.PRODUCT_ATTRIBUTE_UPDATE.name());
    productMigration.setProductCode(PRODUCT_CODE);
    productMigration.setMigrationPayload(validPayload);
    ObjectMapper objectMapper = new ObjectMapper();
    List<MigrationPayload> migrationPayloadList =
      List.of(new MigrationPayload("attr1", true, "value1", "type1"),
        new MigrationPayload("attr2", false, "value2", "type2"));
    CommonImageBackfillingEventModel result =
      CommonUtil.toCommonImageBackfillingEventModel(STORE_ID, productMigration);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(STORE_ID, result.getStoreId());
    Assertions.assertEquals(ProductMigrationType.PRODUCT_ATTRIBUTE_UPDATE.name(), result.getMigrationType());
    Assertions.assertEquals(PRODUCT_CODE, result.getProductCode());
    Assertions.assertNotNull(result.getMigrationPayloadList());
    Assertions.assertEquals(2, result.getMigrationPayloadList().size());
  }

  @Test
  void testToCommonImageBackfillingEventModel_InvalidPayload() throws JsonProcessingException {
    ProductMigration productMigration = new ProductMigration();
    productMigration.setMigrationType(ProductMigrationType.PRODUCT_ATTRIBUTE_UPDATE.name());
    productMigration.setProductCode(PRODUCT_CODE);
    String invalidPayload = "[{\"attributeId\":\"attr1\" \"mustShowOnCustomerSide\":true}]";
    productMigration.setMigrationPayload(invalidPayload);
    productMigration.setMigrationPayload(invalidPayload);
    CommonImageBackfillingEventModel result =
      CommonUtil.toCommonImageBackfillingEventModel(STORE_ID, productMigration);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(STORE_ID, result.getStoreId());
    Assertions.assertEquals(ProductMigrationType.PRODUCT_ATTRIBUTE_UPDATE.name(), result.getMigrationType());
    Assertions.assertEquals(PRODUCT_CODE, result.getProductCode());
    Assertions.assertNotNull(result.getMigrationPayloadList());
    Assertions.assertTrue(result.getMigrationPayloadList().isEmpty());
  }

  @Test
  void testToCommonImageBackfillingEventModel_NullPayload() {
    ProductMigration productMigration = new ProductMigration();
    productMigration.setMigrationType(ProductMigrationType.COMMON_IMAGE_MIGRATION.name());
    productMigration.setProductCode(PRODUCT_CODE);
    productMigration.setMigrationPayload(null);
    CommonImageBackfillingEventModel result =
      CommonUtil.toCommonImageBackfillingEventModel(STORE_ID, productMigration);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(STORE_ID, result.getStoreId());
    Assertions.assertEquals(ProductMigrationType.COMMON_IMAGE_MIGRATION.name(), result.getMigrationType());
    Assertions.assertEquals(PRODUCT_CODE, result.getProductCode());
  }


  @Test
  void testSetMissingProductAttributesAndValues_EmptyAttributes() {
    Product product = new Product();
    List<ProductAttribute> result = CommonUtil.setMissingProductAttributesAndValues(new ArrayList<>(), product, new HashMap<>());
    Assertions.assertNotNull(result);
    Assertions.assertTrue(result.isEmpty());
  }

  @Test
  void testSetMissingProductAttributesAndValues_ProductWithMissingAttributes() {
    Product product = new Product();
    Attribute attribute = new Attribute();
    attribute.setId(ATTRIBUTE_CODE_1);
    attribute.setName(ATTRIBUTE_NAME);
    attribute.setStoreId(STORE_ID);
    attribute.setAttributeType(com.gdn.x.productcategorybase.AttributeType.DESCRIPTIVE_ATTRIBUTE);

    List<Attribute> attributes = Collections.singletonList(attribute);
    Map<String, List<PredefinedAllowedAttributeValue>> predefinedMap = new HashMap<>();

    List<ProductAttribute> result = CommonUtil.setMissingProductAttributesAndValues(attributes, product, predefinedMap);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(1, result.size());
    Assertions.assertEquals(ATTRIBUTE_CODE_1, result.get(0).getAttributeId());
    Assertions.assertEquals(ATTRIBUTE_NAME, result.get(0).getProductAttributeName());
  }

  @Test
  void testSetMissingProductAttributesAndValues_PredefinedAttributeWithValues() {
    Product product = new Product();
    Attribute attribute = new Attribute();
    attribute.setId(ATTRIBUTE_CODE_2);
    attribute.setName(ATTRIBUTE_NAME);
    attribute.setStoreId(STORE_ID);
    attribute.setAttributeType(com.gdn.x.productcategorybase.AttributeType.PREDEFINED_ATTRIBUTE);
    attribute.setMandatory(false);

    PredefinedAllowedAttributeValue predefinedValue = new PredefinedAllowedAttributeValue();
    predefinedValue.setId(PREDEFINIED_ATTRIBUTE_VALUE_1);
    predefinedValue.setAttribute(attribute);

    Map<String, List<PredefinedAllowedAttributeValue>> predefinedMap = new HashMap<>();
    predefinedMap.put(ATTRIBUTE_CODE_2, Collections.singletonList(predefinedValue));

    List<Attribute> attributes = Collections.singletonList(attribute);

    List<ProductAttribute> result = CommonUtil.setMissingProductAttributesAndValues(attributes, product, predefinedMap);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(1, result.size());
    Assertions.assertEquals(ATTRIBUTE_CODE_2, result.get(0).getAttributeId());
    Assertions.assertEquals(ATTRIBUTE_NAME, result.get(0).getProductAttributeName());
    Assertions.assertNotNull(result.get(0).getProductAttributeValues());
  }

  @Test
  void testSetMissingProductAttributesAndValues_PredefinedAttributeWithNullValues() {
    Product product = new Product();
    Attribute attribute = new Attribute();
    attribute.setId(ATTRIBUTE_CODE_2);
    attribute.setName(ATTRIBUTE_NAME);
    attribute.setStoreId(STORE_ID);
    attribute.setAttributeType(com.gdn.x.productcategorybase.AttributeType.PREDEFINED_ATTRIBUTE);
    attribute.setMandatory(false);
    Map<String, List<PredefinedAllowedAttributeValue>> predefinedMap = new HashMap<>();
    predefinedMap.put(ATTRIBUTE_CODE_2, Collections.singletonList(null));

    List<Attribute> attributes = Collections.singletonList(attribute);

    List<ProductAttribute> result = CommonUtil.setMissingProductAttributesAndValues(attributes, product, predefinedMap);

    Assertions.assertNotNull(result);
    Assertions.assertTrue(result.isEmpty(), "Expected no ProductAttribute to be created when predefined value is null");
  }


  @Test
  void testSetMissingProductAttributesAndValues_PredefinedAttributeMandatory() {
    Product product = new Product();
    Attribute attribute = new Attribute();
    attribute.setId(ATTRIBUTE_CODE_2);
    attribute.setName(ATTRIBUTE_NAME);
    attribute.setStoreId(STORE_ID);
    attribute.setAttributeType(com.gdn.x.productcategorybase.AttributeType.PREDEFINED_ATTRIBUTE);
    attribute.setMandatory(true);

    PredefinedAllowedAttributeValue predefinedValue = new PredefinedAllowedAttributeValue();
    predefinedValue.setId(PREDEFINIED_ATTRIBUTE_VALUE_1);
    predefinedValue.setAttribute(attribute);

    Map<String, List<PredefinedAllowedAttributeValue>> predefinedMap = new HashMap<>();
    predefinedMap.put(ATTRIBUTE_CODE_2, Collections.singletonList(predefinedValue));

    List<Attribute> attributes = Collections.singletonList(attribute);

    List<ProductAttribute> result = CommonUtil.setMissingProductAttributesAndValues(attributes, product, predefinedMap);
    Assertions.assertNotNull(result);
  }


  @Test
  void testSetMissingProductAttributesAndValues_PredefinedAttributeWithoutValues() {
    Product product = new Product();
    Attribute attribute = new Attribute();
    attribute.setId(ATTRIBUTE_CODE_2);
    attribute.setName(ATTRIBUTE_NAME);
    attribute.setStoreId(STORE_ID);
    attribute.setAttributeType(com.gdn.x.productcategorybase.AttributeType.PREDEFINED_ATTRIBUTE);
    attribute.setMandatory(false);

    Map<String, List<PredefinedAllowedAttributeValue>> predefinedMap = new HashMap<>();

    List<Attribute> attributes = Collections.singletonList(attribute);

    List<ProductAttribute> result = CommonUtil.setMissingProductAttributesAndValues(attributes, product, predefinedMap);
    Assertions.assertNotNull(result);
    Assertions.assertTrue(result.isEmpty());
  }

  @Test
  void testSetMissingProductAttributesAndValues_DescriptiveAttribute() {
    Product product = new Product();
    Attribute attribute = new Attribute();
    attribute.setId(ATTRIBUTE_CODE_2);
    attribute.setName(ATTRIBUTE_NAME);
    attribute.setStoreId(STORE_ID);
    attribute.setAttributeType(com.gdn.x.productcategorybase.AttributeType.DEFINING_ATTRIBUTE);

    List<Attribute> attributes = Collections.singletonList(attribute);
    Map<String, List<PredefinedAllowedAttributeValue>> predefinedMap = new HashMap<>();

    List<ProductAttribute> result = CommonUtil.setMissingProductAttributesAndValues(attributes, product, predefinedMap);
    Assertions.assertNotNull(result);
  }

  @Test
  public void testProcessDescriptiveMultiValueAttributes() {
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProductAttributeValues(new ArrayList<>());
    List<ProductSuitabilityAttributeModel> attributeProductSuitabilityAttributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel model1 = new ProductSuitabilityAttributeModel();
    model1.setValue(VALUE_1);
    ProductSuitabilityAttributeModel model2 = new ProductSuitabilityAttributeModel();
    model2.setValue(VALUE_2);
    attributeProductSuitabilityAttributeModels.add(model1);
    attributeProductSuitabilityAttributeModels.add(model2);
    CommonUtil.processDescriptiveMultiValueAttributes(attributeProductSuitabilityAttributeModels, productAttribute);
    Assertions.assertEquals(2, productAttribute.getProductAttributeValues().size());
    Assertions.assertEquals(VALUE_1, productAttribute.getProductAttributeValues().get(0).getDescriptiveAttributeValue());
    Assertions.assertEquals(VALUE_2, productAttribute.getProductAttributeValues().get(1).getDescriptiveAttributeValue());
    Assertions.assertEquals(DescriptiveAttributeValueType.MULTIPLE, productAttribute.getProductAttributeValues().get(0).getDescriptiveAttributeValueType());
  }

  @Test
  public void testRemoveProductAttributeValuesAndProductAttribute() {
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeValue value1 = new ProductAttributeValue();
    ProductAttributeValue value2 = new ProductAttributeValue();
    productAttribute.setProductAttributeValues(Arrays.asList(value1, value2));
    CommonUtil.removeProductAttributeValuesAndProductAttribute(productAttribute);
    Assertions.assertTrue(productAttribute.isMarkForDelete());
    Assertions.assertTrue(value1.isMarkForDelete());
    Assertions.assertTrue(value2.isMarkForDelete());
  }

  @Test
  public void testRemoveExistingPredefinedValues() {
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeValue value1 = new ProductAttributeValue();
    value1.setPredefinedAllowedAttributeValueId(PREDEFINED_ID_1);
    ProductAttributeValue value2 = new ProductAttributeValue();
    value2.setPredefinedAllowedAttributeValueId(PREDEFINED_ID_2);
    productAttribute.setProductAttributeValues(Arrays.asList(value1, value2));
    Map<String, String> existingMap = new HashMap<>();
    existingMap.put(PREDEFINED_ID_1, VALUE_1);
    existingMap.put(PREDEFINED_ID_2, VALUE_2);
    List<ProductSuitabilityAttributeModel> newValues = new ArrayList<>();
    ProductSuitabilityAttributeModel model = new ProductSuitabilityAttributeModel();
    model.setValue(VALUE_1);
    newValues.add(model);
    CommonUtil.removeExistingPredefinedValues(newValues, productAttribute, existingMap, new HashSet<>(),
        false);
    Assertions.assertFalse(value1.isMarkForDelete());
    Assertions.assertTrue(value2.isMarkForDelete());
    Assertions.assertFalse(productAttribute.isMarkForDelete());
  }

  @Test
  public void testRemoveExistingPredefinedValuesSwitchTrue() {
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeValue value1 = new ProductAttributeValue();
    value1.setPredefinedAllowedAttributeValueId(PREDEFINED_ID_1);
    ProductAttributeValue value2 = new ProductAttributeValue();
    value2.setPredefinedAllowedAttributeValueId(PREDEFINED_ID_2);
    productAttribute.setProductAttributeValues(Arrays.asList(value1, value2));
    Map<String, String> existingMap = new HashMap<>();
    existingMap.put(PREDEFINED_ID_1, VALUE_1);
    existingMap.put(PREDEFINED_ID_2, VALUE_2);
    List<ProductSuitabilityAttributeModel> newValues = new ArrayList<>();
    ProductSuitabilityAttributeModel model = new ProductSuitabilityAttributeModel();
    model.setValue(VALUE_1);
    newValues.add(model);
    CommonUtil.removeExistingPredefinedValues(newValues, productAttribute, existingMap, new HashSet<>(), true);
    Assertions.assertFalse(value1.isMarkForDelete());
    Assertions.assertTrue(value2.isMarkForDelete());
    Assertions.assertFalse(productAttribute.isMarkForDelete());
  }

  @Test
  public void testRemoveExistingPredefinedValuesSwitchTrue2() {
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setId(PREDEFINED_ID_2);
    ProductAttributeValue value1 = new ProductAttributeValue();
    value1.setPredefinedAllowedAttributeValueId(PREDEFINED_ID_1);
    ProductAttributeValue value2 = new ProductAttributeValue();
    value2.setId(PREDEFINED_ID_2);
    productAttribute.setProductAttributeValues(Arrays.asList(value1, value2));
    Map<String, String> existingMap = new HashMap<>();
    existingMap.put(PREDEFINED_ID_1, VALUE_1);
    List<ProductSuitabilityAttributeModel> newValues = new ArrayList<>();
    ProductSuitabilityAttributeModel model = new ProductSuitabilityAttributeModel();
    model.setValue(VALUE_1);
    newValues.add(model);
    CommonUtil.removeExistingPredefinedValues(newValues, productAttribute, existingMap,
        Collections.singleton(PREDEFINED_ID_2), true);
    Assertions.assertFalse(value1.isMarkForDelete());
    Assertions.assertTrue(value2.isMarkForDelete());
    Assertions.assertTrue(productAttribute.getProductAttributeValues().stream()
        .filter(productAttributeValue -> PREDEFINED_ID_2.equals(productAttributeValue.getId())).toList().get(0)
        .isMarkForDelete());
  }

  @Test
  public void testGetExistingPredefinedIdAndValueMap() {
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeValue value1 = new ProductAttributeValue();
    PredefinedAllowedAttributeValue predefined1 = new PredefinedAllowedAttributeValue();
    predefined1.setId(PREDEFINED_ID_1);
    predefined1.setValue(VALUE_1);
    value1.setPredefinedAllowedAttributeValue(predefined1);
    ProductAttributeValue value2 = new ProductAttributeValue();
    PredefinedAllowedAttributeValue predefined2 = new PredefinedAllowedAttributeValue();
    predefined2.setId(PREDEFINED_ID_2);
    predefined2.setValue(VALUE_2);
    value2.setPredefinedAllowedAttributeValue(predefined2);
    productAttribute.setProductAttributeValues(Arrays.asList(value1, value2));
    Map<String, String> result = CommonUtil.getExistingPredefinedIdAndValueMap(productAttribute, new HashSet<>());
    Assertions.assertEquals(2, result.size());
    Assertions.assertEquals(VALUE_1, result.get(PREDEFINED_ID_1));
    Assertions.assertEquals(VALUE_2, result.get(PREDEFINED_ID_2));
  }

  @Test
  public void testGetExistingPredefinedIdAndEmptyValueMap() {
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeValue value1 = new ProductAttributeValue();
    value1.setId(PREDEFINED_ID_1);
    ProductAttributeValue value2 = new ProductAttributeValue();
    PredefinedAllowedAttributeValue predefined2 = new PredefinedAllowedAttributeValue();
    predefined2.setId(PREDEFINED_ID_2);
    predefined2.setValue(VALUE_2);
    value2.setPredefinedAllowedAttributeValue(predefined2);
    productAttribute.setProductAttributeValues(Arrays.asList(value1, value2));
    HashSet<String> nullValueIds = new HashSet<>();
    Map<String, String> result = CommonUtil.getExistingPredefinedIdAndValueMap(productAttribute, nullValueIds);
    Assertions.assertEquals(1, result.size());
    Assertions.assertTrue(nullValueIds.contains(PREDEFINED_ID_1));
    Assertions.assertEquals(VALUE_2, result.get(PREDEFINED_ID_2));
  }

  @Test
  public void testRemoveProductAttributeIfAllValuesDeleted_AllDeleted() {
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeValue value1 = new ProductAttributeValue();
    value1.setMarkForDelete(true);
    ProductAttributeValue value2 = new ProductAttributeValue();
    value2.setMarkForDelete(true);
    productAttribute.setProductAttributeValues(Arrays.asList(value1, value2));
    CommonUtil.removeProductAttributeIfAllValuesDeleted(productAttribute);
    Assertions.assertTrue(productAttribute.isMarkForDelete());
  }

  @Test
  public void testRemoveProductAttributeIfAllValuesDeleted_NotAllDeleted() {
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeValue value1 = new ProductAttributeValue();
    value1.setMarkForDelete(true);
    ProductAttributeValue value2 = new ProductAttributeValue();
    value2.setMarkForDelete(false);
    productAttribute.setProductAttributeValues(Arrays.asList(value1, value2));
    CommonUtil.removeProductAttributeIfAllValuesDeleted(productAttribute);
    Assertions.assertFalse(productAttribute.isMarkForDelete());
  }

  @Test
  public void testProcessExistingDescriptiveMultiValueAttributes_RemoveAll() {
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeValue value1 = new ProductAttributeValue();
    value1.setDescriptiveAttributeValue(VALUE_1);
    productAttribute.setProductAttributeValues(Arrays.asList(value1));
    List<ProductSuitabilityAttributeModel> ProductSuitabilityAttributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel model = new ProductSuitabilityAttributeModel();
    model.setValue(null);
    ProductSuitabilityAttributeModels.add(model);
    CommonUtil.processExistingDescriptiveMultiValueAttributes(ProductSuitabilityAttributeModels, productAttribute);
    Assertions.assertTrue(productAttribute.isMarkForDelete());
    Assertions.assertTrue(value1.isMarkForDelete());
  }

  @Test
  public void testProcessExistingDescriptiveMultiValueAttributes_UpdateValues() {
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeValue value1 = new ProductAttributeValue();
    value1.setDescriptiveAttributeValue(VALUE_1);
    productAttribute.setProductAttributeValues(new ArrayList<>(Arrays.asList(value1)));
    List<ProductSuitabilityAttributeModel> ProductSuitabilityAttributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel model1 = new ProductSuitabilityAttributeModel();
    model1.setValue(VALUE_1);
    ProductSuitabilityAttributeModel model2 = new ProductSuitabilityAttributeModel();
    model2.setValue(VALUE_2);
    ProductSuitabilityAttributeModels.add(model1);
    ProductSuitabilityAttributeModels.add(model2);
    CommonUtil.processExistingDescriptiveMultiValueAttributes(ProductSuitabilityAttributeModels, productAttribute);
    Assertions.assertEquals(2, productAttribute.getProductAttributeValues().size());
    Assertions.assertFalse(value1.isMarkForDelete());
    Assertions.assertEquals(VALUE_2, productAttribute.getProductAttributeValues().get(1).getDescriptiveAttributeValue());
  }

  @Test
  public void testProcessExistingDescriptiveMultiValueAttributes_RemoveSomeValues() {
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeValue value1 = new ProductAttributeValue();
    value1.setDescriptiveAttributeValue(VALUE_1);
    ProductAttributeValue value2 = new ProductAttributeValue();
    value2.setDescriptiveAttributeValue(VALUE_2);
    productAttribute.setProductAttributeValues(new ArrayList<>(Arrays.asList(value1, value2)));
    List<ProductSuitabilityAttributeModel> ProductSuitabilityAttributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel model1 = new ProductSuitabilityAttributeModel();
    model1.setValue(VALUE_1);
    ProductSuitabilityAttributeModels.add(model1);
    CommonUtil.processExistingDescriptiveMultiValueAttributes(ProductSuitabilityAttributeModels, productAttribute);
    Assertions.assertEquals(2, productAttribute.getProductAttributeValues().size());
    Assertions.assertFalse(value1.isMarkForDelete());
    Assertions.assertTrue(value2.isMarkForDelete());
  }

  @Test
  public void testSetBasicProductAttributeValueDetails() {
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    PredefinedAllowedAttributeValue predefinedValue = new PredefinedAllowedAttributeValue();
    predefinedValue.setId(PREDEFINED_ID_1);
    CommonUtil.setBasicProductAttributeValueDetails(productAttributeValue, predefinedValue);
    Assertions.assertEquals(DescriptiveAttributeValueType.PREDEFINED, productAttributeValue.getDescriptiveAttributeValueType());
    Assertions.assertEquals(predefinedValue, productAttributeValue.getPredefinedAllowedAttributeValue());
    Assertions.assertEquals(PREDEFINED_ID_1, productAttributeValue.getPredefinedAllowedAttributeValueId());
  }

  @Test
  public void testFormUpdateRequestFromCreation() {
    BrandAuthCreateRequest createRequest = new BrandAuthCreateRequest();
    createRequest.setBrandCode(BRAND_CODE);
    createRequest.setSellerCode(SELLER_CODE);
    BrandAuthUpdateRequest updateRequest = CommonUtil.formUpdateRequestFromCreation(createRequest);
    Assertions.assertEquals(BRAND_CODE, updateRequest.getBrandCode());
    Assertions.assertEquals(SELLER_CODE, updateRequest.getSellerCode());
  }

  @Test
  public void testGenerateCreateWipHistory() {
    BrandAuthorisationWip createResponse = new BrandAuthorisationWip();
    createResponse.setBrandCode(BRAND_CODE);
    createResponse.setSellerCode(SELLER_CODE);
    createResponse.setAuthStartDate(new Date());
    createResponse.setAuthExpireDate(new Date());
    DateFormat historyDateFormat = new SimpleDateFormat(DATE_FORMAT);
    BrandAuthorisationHistory history = CommonUtil.generateCreateWipHistory(
        createResponse, USERNAME, STORE_ID, historyDateFormat);
    Assertions.assertEquals(BRAND_CODE, history.getBrandCode());
    Assertions.assertEquals(SELLER_CODE, history.getSellerCode());
    Assertions.assertEquals(STORE_ID, history.getStoreId());
    Assertions.assertEquals(USERNAME, history.getCreatedBy());
    Assertions.assertEquals(USERNAME, history.getUpdatedBy());
  }

  @Test
  public void testGenerateCreateBrandAuthWipRequest() {
    BrandAuthCreateWipRequest request = new BrandAuthCreateWipRequest();
    request.setBrandCode(BRAND_CODE);
    request.setSellerCode(SELLER_CODE);
    request.setDocumentLinks(Arrays.asList(DOCUMENT_LINK_1, DOCUMENT_LINK_2));
    BrandAuthorisationWip wip = CommonUtil.generateCreateBrandAuthWipRequest(request, STORE_ID);
    Assertions.assertEquals(BRAND_CODE, wip.getBrandCode());
    Assertions.assertEquals(SELLER_CODE, wip.getSellerCode());
    Assertions.assertEquals(STORE_ID, wip.getStoreId());
    Assertions.assertEquals(STATUS_IN_REVIEW, wip.getAuthorisationStatus().name());
    Assertions.assertEquals(DOCUMENT_LINK_1 + "," + DOCUMENT_LINK_2, wip.getDocumentLink());
  }

  @Test
  public void testValidateNewFlowCreation_Valid() {
    Date futureDate = new Date(System.currentTimeMillis() + (31 * 24 * 60 * 60 * 1000L)); // 31 days in future
    boolean result = CommonUtil.validateNewFlowCreation(futureDate, 30);
    Assertions.assertTrue(result);
  }

  @Test
  public void testValidateNewFlowCreation_Invalid() {
    Date pastDate = new Date(System.currentTimeMillis() - (24 * 60 * 60 * 1000L)); // 1 day in past
    boolean result = CommonUtil.validateNewFlowCreation(pastDate, 30);
    Assertions.assertFalse(result);
  }

  @Test
  public void testUpdateProductMigrationEntity() {
    ProductMigration savedProductMigration = new ProductMigration();
    ProductMigrationRequest request = new ProductMigrationRequest();
    request.setUpdatedStatus(STATUS_COMPLETED);
    request.setErrorMessage(ERROR_MESSAGE);
    CommonUtil.updateProductMigrationEntity(REQUEST_ID, request, savedProductMigration);
    Assertions.assertEquals(STATUS_COMPLETED, savedProductMigration.getStatus());
    Assertions.assertEquals(ERROR_MESSAGE, savedProductMigration.getErrorMessage());
    Assertions.assertEquals(REQUEST_ID, savedProductMigration.getUpdatedBy());
    Assertions.assertNotNull(savedProductMigration.getUpdatedDate());
  }

  @Test
  public void testGetProductMigrationRequest() {
    CommonImageBackfillingEventModel eventModel = new CommonImageBackfillingEventModel();
    eventModel.setProductCode(PRODUCT_CODE);
    eventModel.setMigrationType(MIGRATION_TYPE);
    eventModel.setMigrationPayloadList(Arrays.asList(new MigrationPayload()));
    ProductMigrationRequest request = CommonUtil.getProductMigrationRequest(eventModel, ERROR_MESSAGE);
    Assertions.assertEquals(PRODUCT_CODE, request.getProductCode());
    Assertions.assertEquals(MIGRATION_TYPE, request.getMigrationType());
    Assertions.assertEquals(STATUS_FAILED, request.getUpdatedStatus());
    Assertions.assertEquals(ERROR_MESSAGE, request.getErrorMessage());
    Assertions.assertNotNull(request.getMigrationPayload());
  }

  @Test
  public void processExistingDescriptiveAttributeValueTest() {
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttribute.setProductAttributeValues(new ArrayList<>(Collections.singletonList(productAttributeValue)));
    List<ProductSuitabilityAttributeModel> attributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel attributeModel = new ProductSuitabilityAttributeModel();
    attributeModel.setValue(ATTRIBUTE_VALUE);
    attributeModels.add(attributeModel);
    CommonUtil.processExistingDescriptiveAttributeValue(attributeModels, productAttribute);
  }


  @Test
  public void processExistingDescriptiveAttributeValueTest_UpdateValue() {
    List<ProductSuitabilityAttributeModel> attributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel attributeModel = new ProductSuitabilityAttributeModel();
    attributeModel.setValue(null);
    attributeModels.add(attributeModel);
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setDescriptiveAttributeValue(OLD_VALUE);
    productAttribute.setProductAttributeValues(Collections.singletonList(productAttributeValue));
    CommonUtil.processExistingDescriptiveAttributeValue(attributeModels, productAttribute);
    assertTrue(productAttribute.getProductAttributeValues().get(0).isMarkForDelete());
  }

  @Test
  public void processExistingDescriptiveAttributeValueSameTest_UpdateValue() {
    List<ProductSuitabilityAttributeModel> attributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel attributeModel = new ProductSuitabilityAttributeModel();
    attributeModel.setValue(OLD_VALUE);
    attributeModels.add(attributeModel);
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setDescriptiveAttributeValue(OLD_VALUE);
    productAttribute.setProductAttributeValues(Collections.singletonList(productAttributeValue));
    CommonUtil.processExistingDescriptiveAttributeValue(attributeModels, productAttribute);
  }

  @Test
  public void processExistingDescriptiveAttributeValueEmptyTest_UpdateValue() {
    List<ProductSuitabilityAttributeModel> attributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel attributeModel = new ProductSuitabilityAttributeModel();
    attributeModel.setValue(null);
    attributeModels.add(attributeModel);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProductAttributeValues(new ArrayList<>());
    CommonUtil.processExistingDescriptiveAttributeValue(attributeModels, productAttribute);
  }

  @Test
  public void processExistingDescriptiveAttributeValueTest_UpdateValueWithExistingValue() {
    List<ProductSuitabilityAttributeModel> attributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel attributeModel = new ProductSuitabilityAttributeModel();
    attributeModel.setValue(VALUE_2);
    attributeModels.add(attributeModel);
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setDescriptiveAttributeValue(VALUE_1);
    productAttribute.setProductAttributeValues(new ArrayList<>(Arrays.asList(productAttributeValue)));
    ProductAttributeValue existingValue2 = new ProductAttributeValue();
    existingValue2.setDescriptiveAttributeValue(VALUE_2);
    existingValue2.setMarkForDelete(true);
    productAttribute.getProductAttributeValues().add(existingValue2);
    CommonUtil.processExistingDescriptiveAttributeValue(attributeModels, productAttribute);
    assertTrue(productAttributeValue.isMarkForDelete());
    assertFalse(existingValue2.isMarkForDelete());
  }

  @Test
  public void processExistingDescriptiveAttributeValueTest_UpdateValueWithNewValue() {
    List<ProductSuitabilityAttributeModel> attributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel attributeModel = new ProductSuitabilityAttributeModel();
    attributeModel.setValue(VALUE_2);
    attributeModels.add(attributeModel);
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setDescriptiveAttributeValue(VALUE_1);
    productAttribute.setProductAttributeValues(new ArrayList<>(Arrays.asList(productAttributeValue)));
    CommonUtil.processExistingDescriptiveAttributeValue(attributeModels, productAttribute);
    assertTrue(productAttributeValue.isMarkForDelete());
    assertEquals(VALUE_1, productAttribute.getProductAttributeValues().get(0).getDescriptiveAttributeValue());
  }

  @Test
  public void processExistingDescriptiveAttributeValueTest_EmptyExistingValues() {
    List<ProductSuitabilityAttributeModel> attributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel attributeModel = new ProductSuitabilityAttributeModel();
    attributeModel.setValue(VALUE_1);
    attributeModels.add(attributeModel);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProductAttributeValues(new ArrayList<>());
    CommonUtil.processExistingDescriptiveAttributeValue(attributeModels, productAttribute);
    assertEquals(1, productAttribute.getProductAttributeValues().size());
    assertFalse(productAttribute.getProductAttributeValues().get(0).isMarkForDelete());
    assertEquals(VALUE_1, productAttribute.getProductAttributeValues().get(0).getDescriptiveAttributeValue());
  }

  @Test
  public void testGetPredefinedValueToProductAttributeValueMap() {
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeValue value1 = new ProductAttributeValue();
    PredefinedAllowedAttributeValue predefined1 = new PredefinedAllowedAttributeValue();
    predefined1.setValue(VALUE_1);
    value1.setPredefinedAllowedAttributeValue(predefined1);
    ProductAttributeValue value2 = new ProductAttributeValue();
    PredefinedAllowedAttributeValue predefined2 = new PredefinedAllowedAttributeValue();
    predefined2.setValue(VALUE_2);
    value2.setPredefinedAllowedAttributeValue(predefined2);
    ProductAttributeValue value3 = new ProductAttributeValue();
    value3.setPredefinedAllowedAttributeValue(null);
    ProductAttributeValue value4 = new ProductAttributeValue();
    PredefinedAllowedAttributeValue predefined4 = new PredefinedAllowedAttributeValue();
    predefined4.setValue("");
    value4.setPredefinedAllowedAttributeValue(predefined4);
    productAttribute.setProductAttributeValues(Arrays.asList(value1, value2, value3, value4));
    Map<String, ProductAttributeValue> result =
        CommonUtil.getPredefinedValueToProductAttributeValueMap(productAttribute);
    assertEquals(2, result.size());
    assertEquals(value1, result.get(VALUE_1));
    assertEquals(value2, result.get(VALUE_2));
    assertNull(result.get(""));
  }

  @Test
  public void testGetPredefinedValueToProductAttributeValueMap_WithDuplicateValues() {
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeValue value1 = new ProductAttributeValue();
    PredefinedAllowedAttributeValue predefined1 = new PredefinedAllowedAttributeValue();
    predefined1.setValue(VALUE_1);
    value1.setPredefinedAllowedAttributeValue(predefined1);
    ProductAttributeValue value2 = new ProductAttributeValue();
    PredefinedAllowedAttributeValue predefined2 = new PredefinedAllowedAttributeValue();
    predefined2.setValue(VALUE_1);
    value2.setPredefinedAllowedAttributeValue(predefined2);
    ProductAttributeValue value3 = new ProductAttributeValue();
    PredefinedAllowedAttributeValue predefined3 = new PredefinedAllowedAttributeValue();
    predefined3.setValue(VALUE_2);
    value3.setPredefinedAllowedAttributeValue(predefined3);
    productAttribute.setProductAttributeValues(Arrays.asList(value1, value2, value3));
    Map<String, ProductAttributeValue> result =
        CommonUtil.getPredefinedValueToProductAttributeValueMap(productAttribute);
    assertEquals(2, result.size());
    assertEquals(value1, result.get(VALUE_1));
    assertEquals(value3, result.get(VALUE_2));
  }

  @Test
  public void getInternalProductHistoryEventModelTest() {
    product1.setProductCode(PRODUCT_CODE);
    InternalProductHistoryEventModel internalProductHistoryEventModel =
        CommonUtil.getInternalProductHistoryEventModel(product1, Constants.DS_EXTRACTED_ATTRIBUTE,
            Constants.DS_ATTRIBUTE_CHANGE);
    Assertions.assertEquals(Constants.DS_EXTRACTED_ATTRIBUTE, internalProductHistoryEventModel.getUsername());
    Assertions.assertEquals(Constants.DS_ATTRIBUTE_CHANGE, internalProductHistoryEventModel.getActivity());
  }

  @Test
  public void getInternalHistoryNotesTest() {
    String notes =
        CommonUtil.getInternalHistoryNotes(List.of(ATTRIBUTE_CODE_1) , List.of(Constants.HYPHEN), ATTRIBUTE_NAME);
    Assertions.assertTrue(notes.contains(ATTRIBUTE_CODE_1));
  }


  @Test
  public void getNewAttributeValuesTest() {
    ProductSuitabilityAttributeModel productSuitabilityAttributeModel = new ProductSuitabilityAttributeModel();
    productSuitabilityAttributeModel.setAttributeCode(ATTRIBUTE_CODE_1);
    productSuitabilityAttributeModel.setValue(ATTRIBUTE_VALUE);
    List<String> newValues = CommonUtil.getNewAttributeValues(List.of(productSuitabilityAttributeModel),
        com.gdn.x.productcategorybase.AttributeType.DESCRIPTIVE_ATTRIBUTE);
    Assertions.assertEquals(ATTRIBUTE_VALUE, newValues.get(0));
    newValues = CommonUtil.getNewAttributeValues(List.of(productSuitabilityAttributeModel),
        com.gdn.x.productcategorybase.AttributeType.PREDEFINED_ATTRIBUTE);
    Assertions.assertEquals(ATTRIBUTE_VALUE, newValues.get(0));
    newValues = CommonUtil.getNewAttributeValues(List.of(productSuitabilityAttributeModel),
        com.gdn.x.productcategorybase.AttributeType.DEFINING_ATTRIBUTE);
    Assertions.assertEquals(ATTRIBUTE_VALUE, newValues.get(0));
    productSuitabilityAttributeModel.setValue(null);
    newValues = CommonUtil.getNewAttributeValues(List.of(productSuitabilityAttributeModel),
        com.gdn.x.productcategorybase.AttributeType.DESCRIPTIVE_ATTRIBUTE);
    Assertions.assertEquals(Constants.HYPHEN, newValues.get(0));
  }

  @Test
  public void getExistingAttributeValues_descriptiveAttribute_returnsDescriptiveValues() {
    ProductAttributeValue value1 = new ProductAttributeValue();
    value1.setDescriptiveAttributeValue(ATTRIBUTE_VALUE);
    value1.setMarkForDelete(false);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProductAttributeValues(List.of(value1));
    Attribute attribute = new Attribute();
    attribute.setAttributeType(com.gdn.x.productcategorybase.AttributeType.DESCRIPTIVE_ATTRIBUTE);
    List<String> result = CommonUtil.getExistingAttributeValues(attribute, List.of(), productAttribute);
    Assertions.assertEquals(1, result.size());
    Assertions.assertEquals(ATTRIBUTE_VALUE, result.get(0));
  }

  @Test
  public void getExistingAttributeValues_predefinedAttribute_returnsPredefinedValues() {
    PredefinedAllowedAttributeValue predefinedValue = new PredefinedAllowedAttributeValue();
    predefinedValue.setValue(ATTRIBUTE_VALUE);
    ProductAttributeValue value1 = new ProductAttributeValue();
    value1.setPredefinedAllowedAttributeValue(predefinedValue);
    value1.setMarkForDelete(false);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProductAttributeValues(List.of(value1));
    Attribute attribute = new Attribute();
    attribute.setAttributeType(com.gdn.x.productcategorybase.AttributeType.PREDEFINED_ATTRIBUTE);
    List<String> result = CommonUtil.getExistingAttributeValues(attribute, List.of(), productAttribute);
    Assertions.assertEquals(1, result.size());
    Assertions.assertEquals(ATTRIBUTE_VALUE, result.get(0));
  }

  @Test
  public void getExistingAttributeValues_unknownType_returnsOldValues() {
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProductAttributeValues(List.of());
    Attribute attribute = new Attribute();
    attribute.setAttributeType(com.gdn.x.productcategorybase.AttributeType.ALL);
    List<String> oldValues = List.of(ATTRIBUTE_VALUE);
    List<String> result = CommonUtil.getExistingAttributeValues(attribute, oldValues, productAttribute);
    Assertions.assertEquals(oldValues, result);
  }
  @Test
  public void getExistingAttributeValues_descriptiveMultiValue_returnsValues() {
    ProductAttributeValue value = new ProductAttributeValue();
    value.setDescriptiveAttributeValue(ATTRIBUTE_VALUE);
    value.setMarkForDelete(false);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProductAttributeValues(List.of(value));
    Attribute attribute = new Attribute();
    attribute.setAttributeType(com.gdn.x.productcategorybase.AttributeType.DESCRIPTIVE_MULTIVALUE);
    List<String> result = CommonUtil.getExistingAttributeValues(attribute, List.of(), productAttribute);
    Assertions.assertEquals(List.of(ATTRIBUTE_VALUE), result);
  }

  @Test
  public void getExistingAttributeValues_predefinedMultiValue_returnsValues() {
    PredefinedAllowedAttributeValue predefined = new PredefinedAllowedAttributeValue();
    predefined.setValue(ATTRIBUTE_VALUE);
    ProductAttributeValue value = new ProductAttributeValue();
    value.setPredefinedAllowedAttributeValue(predefined);
    value.setMarkForDelete(false);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProductAttributeValues(List.of(value));
    Attribute attribute = new Attribute();
    attribute.setAttributeType(com.gdn.x.productcategorybase.AttributeType.PREDEFINED_MULTIVALUE);
    List<String> result = CommonUtil.getExistingAttributeValues(attribute, List.of(), productAttribute);
    Assertions.assertEquals(List.of(ATTRIBUTE_VALUE), result);
  }

  @Test
  public void testGetDistributionInfoResponse_WithValidJson() throws Exception {
    Product product = new Product();
    Map<String, String> map = new HashMap<>();
    map.put(PRODUCT_NAME, "P1");
    map.put(CATEGORY_NAME, "C1");
    product.setDistributionInfo(new ObjectMapper().writeValueAsString(map));
    DistributionInfoResponse resp = CommonUtil.getDistributionInfoResponse(product);
    assertEquals("P1", resp.getProductName());
    assertEquals("C1", resp.getCategoryName());
  }

  @Test
  public void testGetDistributionInfoResponse_NullProduct() {
    assertNull(CommonUtil.getDistributionInfoResponse(null));
  }

  @Test
  public void testGetDistributionInfoResponse_BlankDistributionInfo() {
    Product product = new Product();
    product.setDistributionInfo("");
    assertNull(CommonUtil.getDistributionInfoResponse(product));
  }

  @Test
  public void testGetDistributionInfoResponse_InvalidJson() {
    Product product = new Product();
    product.setDistributionInfo("invalid-json");
    assertNull(CommonUtil.getDistributionInfoResponse(product));
  }

  @Test
  public void testGetDistributionInfoResponse_EmptyMap() throws Exception {
    Product product = new Product();
    product.setDistributionInfo(new ObjectMapper().writeValueAsString(new HashMap<>()));
    assertNull(CommonUtil.getDistributionInfoResponse(product));
  }

  @Test
  public void testMapToDistributionInfoPerSkuResponse() throws Exception {
    Product product = new Product();
    Map<String, String> map = new HashMap<>();
    map.put(PRODUCT_NAME, "P1");
    map.put(CATEGORY_NAME, "C1");
    product.setDistributionInfo(new ObjectMapper().writeValueAsString(map));
    ProductItemUomInfo entity = new ProductItemUomInfo();
    entity.setSkuCode("S1");
    entity.setOrigin(Origin.LOCAL);
    productItem1.setSkuCode("S1");
    productItem1.setOmniChannelSku("OCS1");
    entity.setExpiry(true);
    List<DimensionsAndUomResponse> dims = Collections.singletonList(new DimensionsAndUomResponse());
    entity.setUom(new ObjectMapper().writeValueAsString(dims));
    Map<String, ProductItem> productItemHashMap = new HashMap<>();
    productItemHashMap.put("S1", productItem1);
    DistributionInfoPerSkuResponse resp =
        CommonUtil.mapToDistributionInfoPerSkuResponse(entity, product, productItemHashMap);
    assertEquals("S1", resp.getSkuCode());
    assertEquals("P1", resp.getDistributionInfoResponse().getProductName());
    assertEquals("C1", resp.getDistributionInfoResponse().getCategoryName());
    assertEquals("LOCAL", resp.getDistributionItemInfoResponse().getOrigin());
    assertEquals("OCS1", resp.getDistributionItemInfoResponse().getOmniChannelSku());
    assertTrue(resp.getDistributionItemInfoResponse().isExpiry());
    assertEquals(1, resp.getDimensionsAndUomResponse().size());
  }

  @Test
  public void testBuildDimensionsList_ValidJson() throws Exception {
    ProductItemUomInfo entity = new ProductItemUomInfo();
    List<DimensionsAndUomResponse> list =
        Arrays.asList(new DimensionsAndUomResponse("U1", "T1", "T1", 1, 2, 3, 4.5, 5.5, Arrays.asList("123")));
    entity.setUom(new ObjectMapper().writeValueAsString(list));
    List<DimensionsAndUomResponse> result = CommonUtil.buildDimensionsList(entity);
    assertEquals(1, result.size());
    assertEquals("U1", result.get(0).getUomCode());
  }

  @Test
  public void testBuildDimensionsList_BlankUom() {
    ProductItemUomInfo entity = new ProductItemUomInfo();
    entity.setUom("");
    assertNull(CommonUtil.buildDimensionsList(entity));
  }

  @Test
  public void testBuildDimensionsList_InvalidJson() {
    ProductItemUomInfo entity = new ProductItemUomInfo();
    entity.setUom("not-json");
    List<DimensionsAndUomResponse> result = CommonUtil.buildDimensionsList(entity);
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void setProductItemUomInfoEventModelTest() throws JsonProcessingException {
    CommonUtil.setProductItemUomInfoEventModel(productItem1, productItemDomainEventModel1);
    ProductItemUomInfo productItemUomInfo = new ProductItemUomInfo();
    productItemUomInfo.setProductItem(productItem1);
    List<DimensionsAndUomResponse> dimensionsAndUomResponses = new ArrayList<>();
    DimensionsAndUomResponse dimensionsAndUomResponse = new DimensionsAndUomResponse();
    dimensionsAndUomResponse.setUomCode(CATEGORY_CODE_1);
    ObjectMapper mapper = new ObjectMapper();
    dimensionsAndUomResponses.add(dimensionsAndUomResponse);
    productItemUomInfo.setUom(mapper.writeValueAsString(dimensionsAndUomResponses));
    productItem1.setProductItemUomInfo(productItemUomInfo);
    CommonUtil.setProductItemUomInfoEventModel(productItem1, productItemDomainEventModel1);
    Assertions.assertNotNull(productItem1);
  }

  @Test
  public void setProductMasterData_Test() {
    List <Product> productCodeDetails = new ArrayList<>();
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setLength(LENGTH);
    product.setHeight(HEIGHT);
    product.setWidth(WIDTH);
    product.setWeight(WEIGHT);
    product.setShippingWeight(WEIGHT);
    productCodeDetails.add(product);

    List <ProductItem> productItems = new ArrayList<>();
    ProductItem productItem = new ProductItem();
    productItem.setMarkForDelete(false);
    productItem.setSkuCode("productCode-1-0001");
    productItem.setDangerousGoodsLevel(0);
    productItems.add(productItem);
    productItem.setVatApplicable(false);
    product.setProductItems(productItems);
    List<ProductMasterDataItemResponse> productMasterDataResponseList = new ArrayList<>();
    productMasterDataResponseList = CommonUtil.setProductMasterData(productCodeDetails);
    Assertions.assertEquals(0, productMasterDataResponseList.getFirst().getDangerousGoodsLevel());
    Assertions.assertEquals(WEIGHT, productMasterDataResponseList.getFirst().getWeight());
  }

  @Test
  public void setProductMasterData_Test_With_Description() {
    List<Product> productCodeDetails = new ArrayList<>();
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setLength(LENGTH);
    product.setHeight(HEIGHT);
    product.setWidth(WIDTH);
    product.setShippingWeight(WEIGHT);
    product.setDescription(new byte[] {1, 2});
    productCodeDetails.add(product);

    List<ProductItem> productItems = new ArrayList<>();
    ProductItem productItem = new ProductItem();
    productItem.setMarkForDelete(false);
    productItem.setSkuCode("productCode-1-0001");
    productItem.setDangerousGoodsLevel(0);
    productItems.add(productItem);
    productItem.setVatApplicable(false);
    product.setProductItems(productItems);

    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE_1);
    productCategory.setCategory(category);
    product.setProductCategories(Arrays.asList(productCategory));
    List<ProductMasterDataItemResponse> productMasterDataResponseList = CommonUtil.setProductMasterData(productCodeDetails);
    Assertions.assertEquals(productMasterDataResponseList.get(0).getDangerousGoodsLevel(), 0);
    Assertions.assertEquals(0.0, productMasterDataResponseList.getFirst().getWeight()); // Weight should be 0.0 when null
  }

  @Test
  public void extractProductCodeFromItemCodeTest() {
    CommonUtil.extractProductCodeFromItemCode(PRODUCT_CODE);
    CommonUtil.extractProductCodeFromItemCode(StringUtils.EMPTY);
    Assertions.assertEquals(PRODUCT_CODE, CommonUtil.extractProductCodeFromItemCode(ITEM_CODE));
  }

  @Test
  void getAIGeneratedFieldsResponseTest() {
    AiGeneratedFieldsResponse aiGeneratedFieldsResponse =
        CommonUtil.getAIGeneratedFieldsResponse(AI_GENRATED_FIELDS_STRING);
    Assertions.assertTrue(aiGeneratedFieldsResponse.isAiGeneratedBrand());
    Assertions.assertTrue(aiGeneratedFieldsResponse.isAiGeneratedCategory());
  }

  @Test
  void getAIGeneratedFieldsResponse_ExceptionTest() {
    AiGeneratedFieldsResponse aiGeneratedFieldsResponse =
        CommonUtil.getAIGeneratedFieldsResponse("");
    Assertions.assertFalse(aiGeneratedFieldsResponse.isAiGeneratedBrand());
    Assertions.assertFalse(aiGeneratedFieldsResponse.isAiGeneratedCategory());
  }
}
