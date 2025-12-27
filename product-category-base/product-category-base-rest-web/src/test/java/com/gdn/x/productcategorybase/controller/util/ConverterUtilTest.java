package com.gdn.x.productcategorybase.controller.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.VideoDTO;
import com.gdn.x.productcategorybase.dto.request.DimensionAndUomDTO;
import com.gdn.x.productcategorybase.dto.request.DistributionItemInfoRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUomInfoDTO;
import com.gdn.x.productcategorybase.dto.request.VideoAddEditRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.exception.ValidationException;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.AttributeValueUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryKeywordsUpdateListDTO;
import com.gdn.x.productcategorybase.dto.CategoryMappingsUpdateDTO;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.ProductBrandUpdateDTO;
import com.gdn.x.productcategorybase.dto.ProductBrandUpdateResponseDTO;
import com.gdn.x.productcategorybase.dto.WholesaleMappingDTO;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryAttributeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordUpdateRequestList;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordsUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryMappingsUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeAddRequest;
import com.gdn.x.productcategorybase.dto.request.MinWholesaleDiscountRequest;
import com.gdn.x.productcategorybase.dto.request.ProductBrandUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.WholesaleConfigRequest;
import com.gdn.x.productcategorybase.dto.request.WholesaleMappingRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.LookupResponse;
import com.gdn.x.productcategorybase.dto.request.RestrictedKeywordsUpdateRequest;
import com.gdn.x.productcategorybase.dto.RestrictedKeywordsUpdateDTO;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductBrandUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.Lookup;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.service.FileStorageService;
import com.google.common.collect.ImmutableSet;

public class ConverterUtilTest {

  private static final String CREATED_BY = "CREATED_BY";
  private static final String VALUE = "VALUE";
  private static final String VALUE_TYPE = "UK";
  private static final int SEQUENCE = 1;
  private static final String VALUE1 = "value1";
  private static final String IMAGE_URL = "imageUrl";
  private static final String ITEM_ATTRIBUTES = "itemAttributes";
  private static final String ATTRIBUTE_CODE = "attributeCode";
  private static final String ATTRIBUTE_CODE1 = "attributeCode1";
  private static final String STORE_ID = "storeId";
  private static final String BRAND_NAME = "brand-name";
  private static final String BRAND_CODE = "brand-code";
  private static final String NEW_BRAND_CODE = "new-brand-code";
  private static final String CATEGORY_NAME = "category-name";
  private static final String CATEGORY_NAME1 = "categoryName";
  private static final String PRODUCT_NAME = "productName";
  private static final String PRODUCT_CODE = "MTA-1100587";
  private static final String ID = "id";
  private static final Double LENGTH = 0.0;
  private static final String NAME = "product_name";
  private static final Double SHIPPING_WEIGHT = 0.0;
  private static final String UNIQUE_SELLING_POINT = "usp";
  private static final String UOM = "uom";
  private static final Double WEIGHT = 0.0;
  private static final Double WIDTH = 0.0;
  private static final String LOCATION_PATH = "location_path";
  private static final String ATTRIBUTE_ID = "attributeId";
  private static final String KEYWORD_ID = "KeywordId";
  private static final String KEYWORD = "keyword";
  private static final String IMAGE_SOURCE_DIRECTORY = "./src/test/resources/testFolders";
  private static final String FILENAME = "image1.jpg";
  private static final Integer QUANTITY = 10;
  private static final Double PERCENTAGE = 10D;
  private static final Double PRICE = 100000D;
  private static final String CONFIGURATION_TYPE = "PERCENTAGE";
  private static final String DESCRIPTION = "description";
  private static final String LOOKUP_GROUP = "lookupGroup";
  private static final String CODE = "code";
  private static final String WARNA = "Warna";
  private static final String BLACK = "Black";
  private static final String HITAM = "Hitam";
  private static final String ITEM_NAME = "Item Black name";
  private static final String SOURCE_ITEM_CODE = "MTA-0000001-00001";
  public static final String TYPE = "type";
  public static final String MESSAGE = "message";
  public static final int ACTION = 1;
  public static final String DESTINATION_CATEGORY = "destination_category";
  public static final Boolean VALIDATE_ON_UI = true;
  public static final Boolean VALIDATE_BY_DS = false;
  public static final String ITEM_DISTRIBUTION_INFO = "itemDistributionInfo";
  public static final String AI_GENRATED_FIELDS_STRING =
      "{\"aiGeneratedCategory\":true,\"aiGeneratedBrand\":true}\n";

  @InjectMocks
  private ConverterUtil converterUtil;

  @Mock
  public FileStorageService fileStorageService;

  private ProductRequest productRequest;

  private Product product;

  private ProductItem productItem;

  private Attribute attribute1;

  private ProductItemRequest productItemRequest = new ProductItemRequest();
  private ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
  private List<ProductItemAttributeValueRequest> productItemAttributeValueRequests = new ArrayList<>();
  private Image image = new Image();
  private List<Image> imageList = new ArrayList<>();
  private static final String DEFAULT_ITEM_NAME = "ITEM NAME";
  private static final String DEFAULT_LOCATION_PATH = "LOCATION PATH";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_ATTRIBUTE_VALUE = "Attribute Value";

  private ProductCategory productCategory = new ProductCategory();
  private ProductCategory productCategory1 = new ProductCategory();
  private ProductAttribute productAttribute = new ProductAttribute();
  private ProductImage productImage = new ProductImage();
  private CategoryKeywordUpdateRequestList categoryKeywordUpdateRequestList = new CategoryKeywordUpdateRequestList();
  private CategoryKeywordsUpdateRequest categoryKeywordsUpdateRequest = new CategoryKeywordsUpdateRequest();
  private CategoryMappingsUpdateRequest categoryMappingsUpdateRequest = new CategoryMappingsUpdateRequest();
  private List<CategoryAttributeUpdateRequest> categoryAttributeUpdateRequestList = new ArrayList<>();
  private CategoryAttributeUpdateRequest categoryAttributeUpdateRequest = new CategoryAttributeUpdateRequest();
  private List<CategoryKeywordsUpdateRequest> categoryKeywordsUpdateRequestList = new ArrayList<>();
  private WholesaleMappingRequest wholesaleMappingRequest = new WholesaleMappingRequest();
  private WholesaleConfigRequest wholesaleConfigRequest = new WholesaleConfigRequest();
  private MinWholesaleDiscountRequest minWholesaleDiscountRequest = new MinWholesaleDiscountRequest();
  private List<Lookup> lookupList = new ArrayList<>();
  private final RestrictedKeywordsUpdateRequest restrictedKeywordsUpdateRequest = new RestrictedKeywordsUpdateRequest();

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    image.setActive(Boolean.TRUE);
    image.setLocationPath(DEFAULT_LOCATION_PATH);
    image.setMainImages(Boolean.TRUE);
    image.setStoreId(DEFAULT_STORE_ID);
    productItemAttributeValueRequest.setValue(DEFAULT_ATTRIBUTE_VALUE);
    productItemAttributeValueRequest.setAttribute(new AttributeRequest());
    productItemAttributeValueRequests.add(productItemAttributeValueRequest);
    productItemRequest.setProductItemAttributeValues(productItemAttributeValueRequests);
    imageList.add(image);
    productItemRequest.setImages(imageList);
    productItemRequest.setGeneratedItemName(DEFAULT_ITEM_NAME);

    productRequest = new ProductRequest();
    ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest.setValue(VALUE);
    Map<String, String> attributesMap = new HashMap<>();
    attributesMap.put(ATTRIBUTE_CODE, VALUE1);
    ProductItemRequest productItemRequest = new ProductItemRequest();
    productItemRequest.setAttributesMap(attributesMap);
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE1);
    attributeRequest.setScreeningMandatory(Boolean.TRUE);
    attributeRequest.setVariantCreatingUI(Boolean.TRUE);
    productItemAttributeValueRequest.setAttribute(attributeRequest);
    productItemAttributeValueRequests = new ArrayList<>();
    productItemAttributeValueRequests.add(productItemAttributeValueRequest);

    productItemRequest.setProductItemAttributeValues(productItemAttributeValueRequests);
    productRequest.setProductItems(Arrays.asList(productItemRequest));
    product = new Product();
    productItem = new ProductItem();
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    productItemAttributeValue.setAttribute(attribute);
    productItemAttributeValue.setValue(VALUE1);
    productItem.setProductItemAttributeValues(new ArrayList<>(Arrays.asList(productItemAttributeValue)));

    productCategory.setCategory(new Category(STORE_ID, CATEGORY_NAME, 1));
    Category category = new Category(STORE_ID, CATEGORY_NAME, 1);
    category.setMarkForDelete(true);
    productCategory1.setCategory(category);
    productAttribute.setAttribute(new Attribute("Attribute-name",
        com.gdn.x.productcategorybase.AttributeType.DEFINING_ATTRIBUTE, true, STORE_ID));
    productImage.setLocationPath(LOCATION_PATH);
    productImage.setMainImages(Boolean.TRUE);
    List<ProductImage> productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductImages(productImages);
    this.product.setId(ID);
    this.product.setBrand(BRAND_NAME);
    this.product.setLength(LENGTH);
    this.product.setName(NAME);
    this.product.setProductCode(PRODUCT_CODE);
    this.product.setShippingWeight(SHIPPING_WEIGHT);
    this.product.setUniqueSellingPoint(UNIQUE_SELLING_POINT);
    this.product.setViewable(true);
    this.product.setActivated(true);
    this.product.setUom(UOM);
    this.product.setWeight(WEIGHT);
    this.product.setWidth(WIDTH);
    this.product.setCreatedBy(CREATED_BY);
    productCategory1.setMarkForDelete(true);
    this.product.getProductCategories().add(productCategory1);
    this.product.getProductCategories().add(productCategory);
    this.product.getProductAttributes().add(productAttribute);
    product.setProductItems(Arrays.asList(productItem));

    attribute1 = new Attribute();
    attribute1.setAttributeCode(ATTRIBUTE_CODE);
    attribute1.setPredefinedAllowedAttributeValues(Arrays.asList(new PredefinedAllowedAttributeValue()));
    attribute1.setAllowedAttributeValues(Arrays.asList(new AllowedAttributeValue()));
    categoryAttributeUpdateRequest.setAttributeId(ATTRIBUTE_ID);
    categoryAttributeUpdateRequestList.add(categoryAttributeUpdateRequest);
    categoryMappingsUpdateRequest.setAddedAttributes(categoryAttributeUpdateRequestList);
    categoryMappingsUpdateRequest.setDeletedAttributes(categoryAttributeUpdateRequestList);
    categoryKeywordsUpdateRequest.setKeywordId(KEYWORD_ID);
    categoryKeywordsUpdateRequestList.add(categoryKeywordsUpdateRequest);
    categoryMappingsUpdateRequest.setAddedKeywords(categoryKeywordsUpdateRequestList);
    categoryMappingsUpdateRequest.setDeletedKeywords(categoryKeywordsUpdateRequestList);

    categoryKeywordsUpdateRequest =
        CategoryKeywordsUpdateRequest.builder().keyword(KEYWORD).keywordId(KEYWORD_ID).message(MESSAGE).action(ACTION)
            .type(TYPE).destinationCategory(DESTINATION_CATEGORY).exclusionList(ImmutableSet.of(DESTINATION_CATEGORY)).build();
    categoryKeywordUpdateRequestList.setAddedKeywords(Arrays.asList(categoryKeywordsUpdateRequest));

    minWholesaleDiscountRequest = MinWholesaleDiscountRequest.builder().price(PRICE).percentage(PERCENTAGE).build();
    wholesaleConfigRequest = WholesaleConfigRequest.builder().quantity(QUANTITY)
        .minWholesaleDiscount(Collections.singletonList(minWholesaleDiscountRequest)).build();
    wholesaleMappingRequest = WholesaleMappingRequest.builder().configurationType(CONFIGURATION_TYPE)
        .wholesaleConfig(Collections.singletonList(wholesaleConfigRequest)).build();

    restrictedKeywordsUpdateRequest.setKeywordId(KEYWORD_ID);
    restrictedKeywordsUpdateRequest.setKeyword(KEYWORD);
    restrictedKeywordsUpdateRequest.setValidateOnUi(VALIDATE_ON_UI);
    restrictedKeywordsUpdateRequest.setValidateByDs(VALIDATE_BY_DS);

    Lookup lookup = new Lookup();
    lookup.setCode(CODE);
    lookup.setDescription(DESCRIPTION);
    lookup.setLookupGroup(LOOKUP_GROUP);
    lookup.setName(NAME);
    lookup.setOrderNumber(1);
    lookup.setId(ID);
    lookupList.add(lookup);

    ConverterUtil.setFileStorageService(fileStorageService);
  }


  @Test
  public void convertRequestToProductItemsTest() throws IOException {
    File mainDirectory = new File(IMAGE_SOURCE_DIRECTORY);
    mainDirectory.mkdir();
    File directory1 = new File(IMAGE_SOURCE_DIRECTORY, LOCATION_PATH);
    directory1.mkdir();
    File file1 = new File(directory1, FILENAME);
    file1.createNewFile();
    image.setLocationPath(LOCATION_PATH + File.separator + FILENAME);
    image.setActive(false);
    productItemRequest.getProductItemAttributeValues().get(0).getAttribute().setVariantCreatingUI(Boolean.TRUE);
    productItemRequest.getProductItemAttributeValues().get(0).getAttribute().setScreeningMandatory(Boolean.TRUE);
    productItemRequest.getImages().add(image);
    productItemRequest.setSourceItemCode(SOURCE_ITEM_CODE);
    productItemRequest.setContentChanged(true);
    ProductItem response = ConverterUtil.convertRequestToProductItems(
        STORE_ID, productItemRequest, false, false);
    assertEquals(DEFAULT_ATTRIBUTE_VALUE, response.getProductItemAttributeValues().get(0).getValue());
    assertEquals(image.getLocationPath(),
        response.getProductItemImages().get(0).getLocationPath());
    assertFalse(response.getProductItemImages().get(0).isActive());
    assertTrue(response.getProductItemImages().get(0).isMainImages());
    assertEquals(DEFAULT_STORE_ID, response.getProductItemImages().get(0).getStoreId());
    assertEquals(SOURCE_ITEM_CODE, response.getSourceItemCode());
    assertTrue(response.isContentChanged());
    FileUtils.deleteDirectory(mainDirectory);
  }

  @Test
  public void convertRequestToProductItemsWithDuplicateImagesTest() throws IOException {
    File mainDirectory = new File(IMAGE_SOURCE_DIRECTORY);
    mainDirectory.mkdir();
    File directory1 = new File(IMAGE_SOURCE_DIRECTORY, LOCATION_PATH);
    directory1.mkdir();
    File file1 = new File(directory1, FILENAME);
    file1.createNewFile();
    image.setLocationPath(LOCATION_PATH + File.separator + FILENAME);
    image.setActive(false);
    image.setMainImages(false);
    Image imageDuplicate = new Image();
    imageDuplicate.setLocationPath(LOCATION_PATH + File.separator + FILENAME);
    imageDuplicate.setMainImages(true);
    productItemRequest.getProductItemAttributeValues().get(0).getAttribute().setVariantCreatingUI(Boolean.TRUE);
    productItemRequest.getProductItemAttributeValues().get(0).getAttribute().setScreeningMandatory(Boolean.TRUE);
    productItemRequest.setImages(Arrays.asList(image,imageDuplicate));
    productItemRequest.setSourceItemCode(SOURCE_ITEM_CODE);
    productItemRequest.setContentChanged(true);
    ProductItem response = ConverterUtil.convertRequestToProductItems(
        STORE_ID, productItemRequest, false, false);
    assertEquals(DEFAULT_ATTRIBUTE_VALUE, response.getProductItemAttributeValues().get(0).getValue());
    assertEquals(image.getLocationPath(),
        response.getProductItemImages().get(0).getLocationPath());
    assertEquals(1, response.getProductItemImages().size());
    assertFalse(response.getProductItemImages().get(0).isActive());
    assertTrue(response.getProductItemImages().get(0).isMainImages());
    assertEquals(SOURCE_ITEM_CODE, response.getSourceItemCode());
    assertTrue(response.isContentChanged());
    FileUtils.deleteDirectory(mainDirectory);
  }

  @Test
  public void convertRequestToProductItemsWithDuplicateImagesInCreationTest() throws IOException {
    File mainDirectory = new File(IMAGE_SOURCE_DIRECTORY);
    mainDirectory.mkdir();
    File directory1 = new File(IMAGE_SOURCE_DIRECTORY, LOCATION_PATH);
    directory1.mkdir();
    File file1 = new File(directory1, FILENAME);
    file1.createNewFile();
    image.setLocationPath(LOCATION_PATH + File.separator + FILENAME);
    image.setActive(false);
    image.setMainImages(false);
    Image imageDuplicate = new Image();
    imageDuplicate.setLocationPath(LOCATION_PATH + File.separator + FILENAME);
    imageDuplicate.setMainImages(true);
    productItemRequest.getProductItemAttributeValues().get(0).getAttribute()
        .setVariantCreatingUI(Boolean.TRUE);
    productItemRequest.getProductItemAttributeValues().get(0).getAttribute()
        .setScreeningMandatory(Boolean.TRUE);
    productItemRequest.setImages(Arrays.asList(image, imageDuplicate));
    productItemRequest.setSourceItemCode(SOURCE_ITEM_CODE);
    productItemRequest.setContentChanged(true);
    ProductItem response =
        ConverterUtil.convertRequestToProductItems(STORE_ID, productItemRequest, false, true);
    assertEquals(DEFAULT_ATTRIBUTE_VALUE,
        response.getProductItemAttributeValues().get(0).getValue());
    assertEquals(image.getLocationPath(), response.getProductItemImages().get(0).getLocationPath());
    assertEquals(1, response.getProductItemImages().size());
    assertFalse(response.getProductItemImages().get(0).isActive());
    assertTrue(response.getProductItemImages().get(0).isMainImages());
    assertEquals(SOURCE_ITEM_CODE, response.getSourceItemCode());
    assertTrue(response.isContentChanged());
    FileUtils.deleteDirectory(mainDirectory);
  }

  @Test
  public void convertRequestToProductItems_emptyProductItemAttributes() {
    productItemRequest.setProductItemAttributeValues(new ArrayList<>());
    productItemRequest.setImages(new ArrayList<>());
    ProductItem response = ConverterUtil.convertRequestToProductItems(
        STORE_ID, productItemRequest, false, false);
    assertEquals(0, response.getProductItemAttributeValues().size());
    assertEquals(0, response.getProductItemImages().size());
    assertNull(response.getSourceItemCode());
    assertFalse(response.isContentChanged());
  }

  @Test
  public void convertMasterAttributeAddRequestToAttributeValueUpdateDTOTest() throws Exception {
    MasterAttributeAddRequest masterAttributeAddRequest = new MasterAttributeAddRequest();
    masterAttributeAddRequest.setValue(VALUE);
    masterAttributeAddRequest.setSequence(SEQUENCE);
    AttributeValueUpdateDTO attributeValueUpdateDTO =
        this.converterUtil.convertMasterAttributeAddRequestToAttributeValueUpdateDTO(masterAttributeAddRequest);
    assertNotNull(attributeValueUpdateDTO);
    assertEquals(VALUE, attributeValueUpdateDTO.getValue());
    assertEquals(SEQUENCE,(int) attributeValueUpdateDTO.getSequence());
  }

  @Test
  public void generateItemDataHashMapTest() {
    Map<Map<String, String>, Map<String, Object>> itemDataMap =
        ConverterUtil.generateItemDataHashMap(productRequest, "-");
    List<ProductItemAttributeValueRequest> itemAttributeValueRequests =
        (List<ProductItemAttributeValueRequest>) itemDataMap.get(productRequest.getProductItems()
            .get(0).getAttributesMap()).get(ITEM_ATTRIBUTES);
    assertEquals(VALUE, itemAttributeValueRequests.get(0).getValue());
  }

  @Test
  public void setRequiredDataTest() throws Exception {
    Map<Map<String, String>, Map<String, Object>> itemDataMap =
        ConverterUtil.generateItemDataHashMap(productRequest, "-");
    ConverterUtil
        .setRequiredData(STORE_ID, product, itemDataMap, new HashMap<>(),
            StringUtils.EMPTY, StringUtils.EMPTY, new ProductRequest(), false, true, new HashSet<>());
    assertEquals(VALUE, product.getProductItems().get(0).getProductItemAttributeValues()
        .get(1).getValue());
    assertTrue(product.getProductItems().get(0).getVatApplicable());
  }

  @Test
  public void setRequiredDataDistributionInfoTest() throws Exception {
    productRequest.getProductItems().get(0).setProductItemUomInfoDTO(new ProductItemUomInfoDTO());
    Map<Map<String, String>, Map<String, Object>> itemDataMap =
        ConverterUtil.generateItemDataHashMap(productRequest, Constants.HYPHEN);
    ConverterUtil
        .setRequiredData(STORE_ID, product, itemDataMap, new HashMap<>(),
            StringUtils.EMPTY, StringUtils.EMPTY, new ProductRequest(), false, true, new HashSet<>());
    assertEquals(VALUE, product.getProductItems().get(0).getProductItemAttributeValues()
        .get(1).getValue());
    assertTrue(product.getProductItems().get(0).getVatApplicable());
  }

  @Test
  public void setRequiredDataDistributionInfo2Test() throws Exception {
    ProductItemUomInfoDTO productItemUomInfoDTO = new ProductItemUomInfoDTO();
    productItemUomInfoDTO.setDimensionAndUomDTOList(Collections.singletonList(new DimensionAndUomDTO()));
    productRequest.getProductItems().get(0).setProductItemUomInfoDTO(productItemUomInfoDTO);
    Map<Map<String, String>, Map<String, Object>> itemDataMap =
        ConverterUtil.generateItemDataHashMap(productRequest, Constants.HYPHEN);
    product.setCreatedMerchant(PRODUCT_CODE);
    ConverterUtil.setRequiredData(STORE_ID, product, itemDataMap, new HashMap<>(), StringUtils.EMPTY, StringUtils.EMPTY,
        new ProductRequest(), false, true, Collections.singleton(PRODUCT_CODE));
    assertEquals(VALUE, product.getProductItems().get(0).getProductItemAttributeValues()
        .get(1).getValue());
    assertTrue(product.getProductItems().get(0).getVatApplicable());
  }

  @Test
  public void setRequiredDataDistributionInfo3Test() throws Exception {
    ProductItemUomInfoDTO productItemUomInfoDTO = new ProductItemUomInfoDTO();
    productItemUomInfoDTO.setDimensionAndUomDTOList(Collections.singletonList(new DimensionAndUomDTO()));
    productItemUomInfoDTO.setDistributionItemInfoRequest(new DistributionItemInfoRequest());
    productRequest.getProductItems().get(0).setProductItemUomInfoDTO(productItemUomInfoDTO);
    Map<Map<String, String>, Map<String, Object>> itemDataMap =
        ConverterUtil.generateItemDataHashMap(productRequest, Constants.HYPHEN);
    ConverterUtil
        .setRequiredData(STORE_ID, product, itemDataMap, new HashMap<>(),
            StringUtils.EMPTY, StringUtils.EMPTY, new ProductRequest(), false, true, new HashSet<>());
    assertEquals(VALUE, product.getProductItems().get(0).getProductItemAttributeValues()
        .get(1).getValue());
    assertTrue(product.getProductItems().get(0).getVatApplicable());
  }

  @Test
  public void setRequiredDataCombinedValueAndValueTypeTest() throws Exception {
    productRequest.getProductItems().get(0).getAttributesMap().entrySet()
        .forEach(entry -> entry.setValue("UK-" + entry.getValue()));
    Map<Map<String, String>, Map<String, Object>> itemDataMap =
        ConverterUtil.generateItemDataHashMap(productRequest, "-");
    ConverterUtil
        .setRequiredData(STORE_ID, product, itemDataMap, new HashMap<>(),
            StringUtils.EMPTY, StringUtils.EMPTY, new ProductRequest(), false, false, new HashSet<>());
    assertEquals(VALUE, product.getProductItems().get(0).getProductItemAttributeValues()
        .get(1).getValue());
    assertTrue(product.getProductItems().get(0).getVatApplicable());
  }

  @Test
  public void setRequiredDataSkipMissingVariantsExceptionTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> ConverterUtil
        .setRequiredData(STORE_ID, product, new HashMap<>(), new HashMap<>(),
            StringUtils.EMPTY, StringUtils.EMPTY, new ProductRequest(), true, false, new HashSet<>()));
    assertEquals(1, product.getProductItems().size());
  }

  @Test
  public void setRequiredDataSkipMissingVariantsTest() throws Exception {
    Map<Map<String, String>, Map<String, Object>> itemDataMap =
        ConverterUtil.generateItemDataHashMap(productRequest, "-");
    ConverterUtil
        .setRequiredData(STORE_ID, product, itemDataMap, new HashMap<>(),
            StringUtils.EMPTY, StringUtils.EMPTY, new ProductRequest(), true, false, new HashSet<>());
    assertEquals(product.getProductItems().size(), 1);
    assertEquals(VALUE, product.getProductItems().get(0).getProductItemAttributeValues()
        .get(1).getValue());
  }

  @Test
  public void setRequiredDataTest1() {
    productRequest.setProductItems(null);
    Map<Map<String, String>, Map<String, Object>> itemDataMap = ConverterUtil.generateItemDataHashMap(productRequest, "-");
    try {
      ConverterUtil
          .setRequiredData(STORE_ID, product, itemDataMap, new HashMap<>(),
              StringUtils.EMPTY, StringUtils.EMPTY, new ProductRequest(), false, false, new HashSet<>());
    } catch (Exception e) {
    }
  }

  @Test
  public void setRequiredDataTest2() throws Exception {
    productRequest.getProductItems().get(0).setProductItemAttributeValues(null);
    Map<Map<String, String>, Map<String, Object>> itemDataMap =
        ConverterUtil.generateItemDataHashMap(productRequest, "-");
    ConverterUtil
        .setRequiredData(STORE_ID, product, itemDataMap, new HashMap<>(),
            StringUtils.EMPTY, StringUtils.EMPTY, new ProductRequest(), false, false, new HashSet<>());
    assertTrue(product.getProductItems().get(0).getVatApplicable());
  }

  @Test
  public void setRequiredDataDuplicateImagesTest2() throws Exception {
    productRequest.getProductItems().get(0).setProductItemAttributeValues(null);
    Image image = new Image();
    image.setOriginalImage(true);
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(true);
    Image image1 = new Image();
    image1.setOriginalImage(true);
    image1.setLocationPath(PRODUCT_CODE);
    image1.setMainImages(false);
    Image image2 = new Image();
    image2.setOriginalImage(true);
    image2.setLocationPath(NAME);
    image2.setMainImages(false);
    productRequest.getProductItems().get(0).setImages(Arrays.asList(image1, image, image2));
    Map<Map<String, String>, Map<String, Object>> itemDataMap =
        ConverterUtil.generateItemDataHashMap(productRequest, "-");
    ConverterUtil
        .setRequiredData(STORE_ID, product, itemDataMap, new HashMap<>(),
            StringUtils.EMPTY, StringUtils.EMPTY, new ProductRequest(), false, false, new HashSet<>());
    assertTrue(product.getProductItems().get(0).getVatApplicable());
  }

  @Test
  public void setRequiredDataDuplicateImagesWithDifferentImageTest() throws Exception {
    productRequest.getProductItems().get(0).setProductItemAttributeValues(null);
    Image image = new Image();
    image.setOriginalImage(true);
    image.setLocationPath(LOCATION_PATH.concat("-1"));
    image.setMainImages(true);
    Image image1 = new Image();
    image1.setOriginalImage(true);
    image1.setLocationPath(LOCATION_PATH);
    image1.setMainImages(false);
    Image image2 = new Image();
    image2.setOriginalImage(true);
    image2.setLocationPath(LOCATION_PATH);
    image2.setMainImages(false);
    productRequest.getProductItems().get(0).setImages(Arrays.asList(image1, image, image2));
    Map<Map<String, String>, Map<String, Object>> itemDataMap =
      ConverterUtil.generateItemDataHashMap(productRequest, "-");
    ConverterUtil
      .setRequiredData(STORE_ID, product, itemDataMap, new HashMap<>(),
          StringUtils.EMPTY, StringUtils.EMPTY, new ProductRequest(), false, false, new HashSet<>());
    assertTrue(product.getProductItems().get(0).getVatApplicable());
  }

  @Test
  public void setRequiredDataTest3() throws Exception {
    productRequest.getProductItems().get(0).getProductItemAttributeValues().get(0).getAttribute()
        .setName(ATTRIBUTE_CODE);
    Map<Map<String, String>, Map<String, Object>> itemDataMap =
        ConverterUtil.generateItemDataHashMap(productRequest, "-");
    ConverterUtil
        .setRequiredData(STORE_ID, product, itemDataMap, new HashMap<>(),
            StringUtils.EMPTY, StringUtils.EMPTY, new ProductRequest(), false, false, new HashSet<>());
    assertTrue(product.getProductItems().get(0).getVatApplicable());
  }

  @Test
  public void setRequiredDataTestWithMissingItems() throws Exception {
    ProductRequest productRequest = generateProductRequest();
    Image image = new Image();
    image.setOriginalImage(true);
    image.setLocationPath(PRODUCT_CODE);
    image.setMainImages(true);
    productRequest.setImages(Collections.singletonList(image));

    Map<Map<String, String>, Map<String, Object>> itemDataMap =
        ConverterUtil.generateItemDataHashMap(productRequest, "-");
    ConverterUtil
        .setRequiredData(STORE_ID, product, itemDataMap, new HashMap<>(),
            StringUtils.EMPTY, StringUtils.EMPTY, productRequest, false, false, new HashSet<>());
    assertEquals(HITAM, product.getProductItems().get(0).getProductItemAttributeValues()
        .get(1).getValue());
    assertTrue(product.getProductItems().get(0).getVatApplicable());
  }

  @Test
  public void setRequiredDataTest4() throws Exception {
    ProductRequest productRequest = generateProductRequest();
    Image image = new Image();
    image.setOriginalImage(true);
    image.setLocationPath(PRODUCT_CODE);
    image.setMainImages(true);
    productRequest.setImages(Collections.singletonList(image));
    product.setProductImages(new ArrayList<>());

    Map<Map<String, String>, Map<String, Object>> itemDataMap =
        ConverterUtil.generateItemDataHashMap(productRequest, "-");
    ConverterUtil
        .setRequiredData(STORE_ID, product, itemDataMap, new HashMap<>(),
            StringUtils.EMPTY, StringUtils.EMPTY, productRequest, false, false, new HashSet<>());
    assertEquals(HITAM, product.getProductItems().get(0).getProductItemAttributeValues()
        .get(1).getValue());
    assertTrue(product.getProductItems().get(0).getVatApplicable());
  }

  @Test
  public void setRequiredDataDuplicateImagesTest() throws Exception {
    ProductRequest productRequest = generateProductRequest();
    Image image = new Image();
    image.setOriginalImage(true);
    image.setLocationPath(PRODUCT_CODE);
    image.setMainImages(true);
    Image image1 = new Image();
    image1.setOriginalImage(true);
    image1.setLocationPath(PRODUCT_CODE);
    image1.setMainImages(false);
    Image image2 = new Image();
    image2.setOriginalImage(true);
    image2.setLocationPath(NAME);
    image2.setMainImages(false);
    productRequest.setImages(Arrays.asList(image1, image, image2));
    product.setProductImages(new ArrayList<>());

    Map<Map<String, String>, Map<String, Object>> itemDataMap =
        ConverterUtil.generateItemDataHashMap(productRequest, "-");
    ConverterUtil
        .setRequiredData(STORE_ID, product, itemDataMap, new HashMap<>(),
            StringUtils.EMPTY, StringUtils.EMPTY, productRequest, false, false, new HashSet<>());
    assertEquals(HITAM, product.getProductItems().get(0).getProductItemAttributeValues()
        .get(1).getValue());
    assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
  }

  @Test
  public void setRequiredDataTestWithDefiningAttribute() throws Exception {
    ProductAttribute productAttribute = new ProductAttribute();
    Attribute attribute = new Attribute();
    attribute.setName(WARNA);
    attribute.setVariantCreation(true);
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    productAttribute.setAttribute(attribute);
    productAttribute.setProduct(product);
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setDescriptiveAttributeValue(BLACK);
    productAttributeValue.setProductAttribute(productAttribute);
    AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
    allowedAttributeValue.setValue(VALUE);
    allowedAttributeValue.setSequence(SEQUENCE);
    productAttributeValue.setAllowedAttributeValue(allowedAttributeValue);
    productAttribute.setProductAttributeValues(Collections.singletonList(productAttributeValue));
    product.getProductAttributes().add(productAttribute);
    product.getProductItems().get(0).setGeneratedItemName(ITEM_NAME);
    Image image = new Image();
    image.setOriginalImage(true);
    image.setLocationPath(PRODUCT_CODE);
    ProductRequest productRequest = new ProductRequest();
    productRequest.setImages(Collections.singletonList(image));
    productRequest.setIgnoreMissingItems(true);
    ProductItemRequest productItemRequest = new ProductItemRequest();
    ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setName(Constants.FAMILY_COLOUR);
    productItemAttributeValueRequest.setAttribute(attributeRequest);
    productItemAttributeValueRequest.setValue(HITAM);
    productItemRequest.setProductItemAttributeValues(Collections.singletonList(productItemAttributeValueRequest));
    productItemRequest.setGeneratedItemName(ITEM_NAME);
    productRequest.setProductItems(Collections.singletonList(productItemRequest));

    Map<Map<String, String>, Map<String, Object>> itemDataMap = ConverterUtil.generateItemDataHashMap(productRequest, "-");
    ConverterUtil.setRequiredData(STORE_ID, product, itemDataMap, new HashMap<>(),
        StringUtils.EMPTY, StringUtils.EMPTY, productRequest, false, false, new HashSet<>());
    assertTrue(product.getProductItems().get(0).getVatApplicable());
  }

  @Test
  public void setRequiredDataTestWithoutFamilyColour() throws Exception {
    ProductRequest productRequest = generateProductRequest();
    AttributeRequest attributeRequest = new AttributeRequest();
    productItemAttributeValueRequest.setAttribute(attributeRequest);
    productItemAttributeValueRequest.setValue(HITAM);
    productItemRequest.setProductItemAttributeValues(Collections.singletonList(productItemAttributeValueRequest));
    productItemRequest.setGeneratedItemName(ITEM_NAME);
    productRequest.setProductItems(Collections.singletonList(productItemRequest));

    Map<Map<String, String>, Map<String, Object>> itemDataMap = ConverterUtil.generateItemDataHashMap(productRequest, "-");
    ConverterUtil.setRequiredData(STORE_ID, product, itemDataMap, new HashMap<>(),
        StringUtils.EMPTY, StringUtils.SPACE, productRequest, false, false, new HashSet<>());
    assertEquals(VALUE1, product.getProductItems().get(0).getProductItemAttributeValues().get(0).getValue());
    assertTrue(product.getProductItems().get(0).getVatApplicable());
  }

  @Test
  public void setRequiredDataTest_WithException() {
    Map<Map<String, String>, Map<String, Object>> itemDataMap = new HashMap<>();
    try {
      ConverterUtil
          .setRequiredData(STORE_ID, product, itemDataMap, new HashMap<>(),
              StringUtils.EMPTY, StringUtils.EMPTY, new ProductRequest(), false, false, new HashSet<>());
    } catch (Exception ex) {
      assertEquals(ex.getClass(), ApplicationRuntimeException.class);
    }
  }

  @Test
  public void convertProductItemAttributeRequestsTest(){
    List<ProductItemAttributeValue> productItemAttributeValues =
        ConverterUtil.convertProductItemAttributeRequests(productItemAttributeValueRequests,
            productItem, new HashMap<>());
    assertNotNull(productItemAttributeValues);
    assertEquals(VALUE, productItemAttributeValues.get(0).getValue());
  }

  @Test
  public void convertProductItemAttributeRequestsTest_WithNull(){
    List<ProductItemAttributeValue> productItemAttributeValues =
        ConverterUtil.convertProductItemAttributeRequests(null,
            productItem, new HashMap<>());
    assertTrue(productItemAttributeValues.isEmpty());
  }

  @Test
  public void toAttributeResponseTest_predefinedAttributeType() {
    attribute1.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    AttributeResponse attributeResponse = ConverterUtil.toAttributeResponse(attribute1);
    assertNotNull(attributeResponse);
    assertEquals(ATTRIBUTE_CODE, attributeResponse.getAttributeCode());
    assertEquals(AttributeType.PREDEFINED_ATTRIBUTE.name(), attributeResponse.getAttributeType().toString());
    assertEquals(1, attributeResponse.getPredefinedAllowedAttributeValues().size());
    assertEquals(0, attributeResponse.getAllowedAttributeValues().size());
  }

  @Test
  public void toAttributeResponseTest_definingAttributeType() {
    attribute1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    AttributeResponse attributeResponse = ConverterUtil.toAttributeResponse(attribute1);
    assertNotNull(attributeResponse);
    assertEquals(ATTRIBUTE_CODE, attributeResponse.getAttributeCode());
    assertEquals(AttributeType.DEFINING_ATTRIBUTE.name(), attributeResponse.getAttributeType().toString());
    assertEquals(0, attributeResponse.getPredefinedAllowedAttributeValues().size());
    assertEquals(1, attributeResponse.getAllowedAttributeValues().size());
  }

  @Test
  void toAttributeResponseTestPredefinedMultiAttributeTypeMfdTrueTest() {
    attribute1.setAttributeType(AttributeType.PREDEFINED_MULTIVALUE);
    attribute1.getPredefinedAllowedAttributeValues().get(0).setMarkForDelete(true);
    AttributeResponse attributeResponse = ConverterUtil.toAttributeResponse(attribute1);
    assertNotNull(attributeResponse);
    assertEquals(ATTRIBUTE_CODE, attributeResponse.getAttributeCode());
    assertEquals(AttributeType.PREDEFINED_MULTIVALUE.name(), attributeResponse.getAttributeType().toString());
    assertEquals(0, attributeResponse.getPredefinedAllowedAttributeValues().size());
  }

  @Test
  void toAttributeResponseTestPredefinedMultiAttributeTypeTest() {
    attribute1.setAttributeType(AttributeType.PREDEFINED_MULTIVALUE);
    AttributeResponse attributeResponse = ConverterUtil.toAttributeResponse(attribute1);
    assertNotNull(attributeResponse);
    assertEquals(ATTRIBUTE_CODE, attributeResponse.getAttributeCode());
    assertEquals(AttributeType.PREDEFINED_MULTIVALUE.name(), attributeResponse.getAttributeType().toString());
    assertEquals(1, attributeResponse.getPredefinedAllowedAttributeValues().size());
  }

  @Test
  void toAttributeResponseTest_InvalidType() {
    attribute1.setAttributeType(AttributeType.DESCRIPTIVE_MULTIVALUE);
    AttributeResponse attributeResponse = ConverterUtil.toAttributeResponse(attribute1);
    assertNotNull(attributeResponse);
    assertEquals(ATTRIBUTE_CODE, attributeResponse.getAttributeCode());
    assertEquals(AttributeType.DESCRIPTIVE_MULTIVALUE.name(),
   attributeResponse.getAttributeType().toString());
  }

  @Test
  public void convertProductToProductDetailResponseTest_emptyItemResponses() {
    product.getProductImages().forEach(image -> image.setCommonImage(true));
    product.getProductItems()
        .forEach(item -> item.getProductItemImages().forEach(itemImage -> itemImage.setCommonImage(true)));
    ProductDetailResponse productDetailResponse =
        ConverterUtil.convertProductToProductDetailResponse(product, false, false, false);
    assertTrue(CollectionUtils.isEmpty(productDetailResponse.getProductItemResponses()));
    assertEquals(NAME, productDetailResponse.getName());
    assertEquals(PRODUCT_CODE, productDetailResponse.getProductCode());
    assertTrue(CollectionUtils.isNotEmpty(productDetailResponse.getProductAttributeResponses()));
    assertTrue(CollectionUtils.isNotEmpty(productDetailResponse.getProductCategoryResponses()));
    assertFalse(productDetailResponse.getImages().stream().anyMatch(image -> !image.isCommonImage()));
  }

  @Test
  public void convertProductToProductDetailResponseTest() {
    product.setRevised(true);
    ProductDetailResponse productDetailResponse =
        ConverterUtil.convertProductToProductDetailResponse(product, false, false, true);
    assertTrue(CollectionUtils.isNotEmpty(productDetailResponse.getProductItemResponses()));
    assertEquals(NAME, productDetailResponse.getName());
    assertEquals(PRODUCT_CODE, productDetailResponse.getProductCode());
    assertTrue(CollectionUtils.isNotEmpty(productDetailResponse.getProductAttributeResponses()));
    assertTrue(CollectionUtils.isNotEmpty(productDetailResponse.getProductCategoryResponses()));
    assertTrue(productDetailResponse.isRevised());
  }

  @Test
  public void convertProductToProductDetailResponseTestForSortedcategoryResponse() {
    ProductDetailResponse productDetailResponse =
        ConverterUtil.convertProductToProductDetailResponse(product, true, false, true);
    assertTrue(CollectionUtils.isNotEmpty(productDetailResponse.getProductItemResponses()));
    assertEquals(NAME, productDetailResponse.getName());
    assertEquals(PRODUCT_CODE, productDetailResponse.getProductCode());
    assertTrue(CollectionUtils.isNotEmpty(productDetailResponse.getProductAttributeResponses()));
    assertTrue(CollectionUtils.isNotEmpty(productDetailResponse.getProductCategoryResponses()));
    assertFalse(productDetailResponse.getProductCategoryResponses().get(0).isMarkForDelete());
    assertTrue(productDetailResponse.getProductCategoryResponses().get(1).isMarkForDelete());
  }

  @Test
  public void convertProductToProductDetailResponseDistributionInfoTest() throws JsonProcessingException {
    product.setRevised(true);
    HashMap<String, String> map = new HashMap<>();
    map.put(CATEGORY_NAME1, "categoryName");
    map.put(PRODUCT_NAME, "productName");
    product.setDistributionInfo(new ObjectMapper().writeValueAsString(map));
    product.setAiGeneratedFields(AI_GENRATED_FIELDS_STRING);
    ProductDetailResponse productDetailResponse =
        ConverterUtil.convertProductToProductDetailResponse(product, false, false, true);
    assertTrue(CollectionUtils.isNotEmpty(productDetailResponse.getProductItemResponses()));
    assertEquals(NAME, productDetailResponse.getName());
    assertEquals(PRODUCT_CODE, productDetailResponse.getProductCode());
    assertTrue(CollectionUtils.isNotEmpty(productDetailResponse.getProductAttributeResponses()));
    assertTrue(CollectionUtils.isNotEmpty(productDetailResponse.getProductCategoryResponses()));
    assertTrue(productDetailResponse.isRevised());
    assertTrue(StringUtils.isNotEmpty(productDetailResponse.getDistributionInfoResponse().getProductName()));
    assertTrue(productDetailResponse.getAiGeneratedFieldsResponse().isAiGeneratedCategory());
  }

  @Test
  public void toCategoryMappingsUpdateDTOTest() {
    CategoryMappingsUpdateDTO categoryMappingsUpdateDTO = ConverterUtil.toCategoryMappingsUpdateDTO(categoryMappingsUpdateRequest);
    assertNotNull(categoryMappingsUpdateDTO);
    assertNotNull(categoryMappingsUpdateDTO.getAddedAttributes());
    assertEquals(categoryMappingsUpdateDTO.getAddedAttributes().get(0).getAttributeId(), ATTRIBUTE_ID);
    assertNotNull(categoryMappingsUpdateDTO.getDeletedAttributes());
    assertEquals(categoryMappingsUpdateDTO.getDeletedAttributes().get(0).getAttributeId(), ATTRIBUTE_ID);
    assertNotNull(categoryMappingsUpdateDTO.getAddedKeywords());
    assertEquals(categoryMappingsUpdateDTO.getAddedKeywords().get(0).getKeywordId(), KEYWORD_ID);
    assertNotNull(categoryMappingsUpdateDTO.getDeletedKeywords());
    assertEquals(categoryMappingsUpdateDTO.getDeletedKeywords().get(0).getKeywordId(), KEYWORD_ID);
  }

  @Test
  public void toCategoryKeywordsUpdateListDTOTest() {
    CategoryKeywordsUpdateListDTO result =
        ConverterUtil.toCategoryKeywordsUpdateListDTO(categoryKeywordUpdateRequestList,STORE_ID);
    assertNotNull(result);
    assertEquals(0, result.getDeletedRestrictedKeywords().size());
    assertEquals(1, result.getAddedRestrictedKeywords().size());
    assertEquals(KEYWORD, result.getAddedRestrictedKeywords().get(0).getKeyword());
    assertEquals(KEYWORD_ID, result.getAddedRestrictedKeywords().get(0).getKeywordId());
    assertEquals(KEYWORD_ID, result.getAddedRestrictedKeywords().get(0).getKeywordId());
    assertEquals(MESSAGE, result.getAddedRestrictedKeywords().get(0).getMessage());
    assertEquals(TYPE, result.getAddedRestrictedKeywords().get(0).getType());
    assertEquals(ACTION, result.getAddedRestrictedKeywords().get(0).getAction());
    assertEquals(DESTINATION_CATEGORY, result.getAddedRestrictedKeywords().get(0).getDestinationCategory());
    assertEquals(DESTINATION_CATEGORY, result.getAddedRestrictedKeywords().get(0).getExclusionList().iterator().next());
  }

  @Test
  public void toRestrictedKeywordsUpdateDTOTest() {
    RestrictedKeywordsUpdateDTO result =
            ConverterUtil.toRestrictedKeywordsUpdateDTO(restrictedKeywordsUpdateRequest);
    assertNotNull(result);
    assertEquals(KEYWORD, result.getKeyword());
    assertEquals(KEYWORD_ID, result.getKeywordId());
    assertEquals(VALIDATE_ON_UI, result.getValidateOnUi());
    assertEquals(VALIDATE_BY_DS, result.getValidateByDs());
  }

  @Test
  public void toCategoryWholesaleDTOTest() {
    WholesaleMappingDTO wholesaleMappingDTO = ConverterUtil.toCategoryWholesaleDTO(wholesaleMappingRequest);
    assertNotNull(wholesaleMappingDTO);
    assertEquals(CONFIGURATION_TYPE, wholesaleMappingDTO.getConfigurationType());
    assertEquals(1, wholesaleMappingDTO.getWholesaleConfig().size());
  }

  @Test
  public void toLookupResponseList() {
    List<LookupResponse> result = ConverterUtil.toLookupResponseList(lookupList);
    assertNotNull(result);
    assertEquals(1, result.size());
    assertEquals(DESCRIPTION, result.get(0).getDescription());
    assertEquals(CODE, result.get(0).getCode());
    assertEquals(ID, result.get(0).getId());
    assertEquals(LOOKUP_GROUP, result.get(0).getLookupGroup());
    assertEquals(NAME, result.get(0).getName());
    assertEquals(1, (int) result.get(0).getOrderNumber());
  }

  @Test
  public void convertProductAttributeToResponseTest() {
    ProductAttribute productAttribute = new ProductAttribute();
    Attribute attribute = new Attribute();
    attribute.setName(WARNA);
    attribute.setSizeAttribute(true);
    attribute.setVariantCreation(true);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    productAttribute.setAttribute(attribute);
    productAttribute.setProduct(product);
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setDescriptiveAttributeValue(BLACK);
    productAttributeValue.setProductAttribute(productAttribute);
    productAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    productAttribute.setProductAttributeValues(Collections.singletonList(productAttributeValue));
    product.getProductAttributes().add(productAttribute);
    product.getProductItems().get(0).setGeneratedItemName(ITEM_NAME);
    productAttribute.getProductAttributeValues().get(0).setPredefinedAllowedAttributeValue(null);
    ProductAttributeResponse productAttributeResponse =
        ConverterUtil.convertProductAttributeToResponse(productAttribute);
    assertNull(productAttributeResponse.getProductAttributeName());
    assertTrue(productAttributeResponse.getAttribute().isSizeAttribute());
  }

  private ProductRequest generateProductRequest() {
    ProductAttribute productAttribute = new ProductAttribute();
    Attribute attribute = new Attribute();
    attribute.setName(WARNA);
    attribute.setVariantCreation(true);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    productAttribute.setAttribute(attribute);
    productAttribute.setProduct(product);
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setDescriptiveAttributeValue(BLACK);
    productAttributeValue.setProductAttribute(productAttribute);
    productAttribute.setProductAttributeValues(Collections.singletonList(productAttributeValue));
    product.getProductAttributes().add(productAttribute);
    product.getProductItems().get(0).setGeneratedItemName(ITEM_NAME);
    Image image = new Image();
    image.setOriginalImage(true);
    image.setLocationPath(PRODUCT_CODE);
    ProductRequest productRequest = new ProductRequest();
    productRequest.setImages(Collections.singletonList(image));
    productRequest.setIgnoreMissingItems(true);
    ProductItemRequest productItemRequest = new ProductItemRequest();
    ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setName(Constants.FAMILY_COLOUR);
    productItemAttributeValueRequest.setAttribute(attributeRequest);
    productItemAttributeValueRequest.setValue(HITAM);
    productItemRequest.setProductItemAttributeValues(Collections.singletonList(productItemAttributeValueRequest));
    productItemRequest.setGeneratedItemName(ITEM_NAME);
    productRequest.setProductItems(Collections.singletonList(productItemRequest));
    return productRequest;
  }

  @Test
  public void convertProductItemRequestToProductItemRequestEmptyTest() {
    List<ProductItemRequest> request = new ArrayList<>();
    List<ProductItem> productItems = new ArrayList<>();
    converterUtil.convertProductItemRequestToProductItem(request, productItems);
    assertTrue(CollectionUtils.isEmpty(productItems));
  }

  @Test
  public void convertProductItemRequestToProductItemTest() {
    List<ProductItemRequest> request = new ArrayList<>();
    productItemRequest.setProductItemAttributeValues(new ArrayList<>());
    request.add(productItemRequest);
    List<ProductItem> productItems = new ArrayList<>();
    converterUtil.convertProductItemRequestToProductItem(request, productItems);
    assertEquals(DEFAULT_ITEM_NAME, productItems.get(0).getGeneratedItemName());
    assertEquals(1, productItems.get(0).getProductItemImages().size());
  }

  @Test
  public void convertProductItemRequestToProductItemWithAllowedAttributeValuesTest() {
    List<ProductItemRequest> request = new ArrayList<>();
    AllowedAttributeValueRequest allowedAttributeValue = new AllowedAttributeValueRequest();
    allowedAttributeValue.setAllowedAttributeCode(ATTRIBUTE_CODE);
    allowedAttributeValue.setValue(ATTRIBUTE_ID);
    allowedAttributeValue.setSequence(SEQUENCE);
    productItemRequest.getProductItemAttributeValues().get(0).getAttribute()
        .setAllowedAttributeValues(Arrays.asList(allowedAttributeValue));
    request.add(productItemRequest);
    List<ProductItem> productItems = new ArrayList<>();
    converterUtil.convertProductItemRequestToProductItem(request, productItems);
    assertEquals(DEFAULT_ITEM_NAME, productItems.get(0).getGeneratedItemName());
    assertEquals(ATTRIBUTE_CODE,
        productItems.get(0).getProductItemAttributeValues().get(0).getAttribute().getAllowedAttributeValues().get(0)
            .getAllowedAttributeCode());
    assertEquals(ATTRIBUTE_ID,
        productItems.get(0).getProductItemAttributeValues().get(0).getAttribute().getAllowedAttributeValues().get(0)
            .getValue());
  }

  @Test
  public void convertProductToProductDetailResponseForImagesTest() {
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(LOOKUP_GROUP);
    productItemImage.setOriginalImage(false);
    productItemImage.setMarkForDelete(false);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage);
    ProductDetailResponse productDetailResponse =
        ConverterUtil.convertProductToProductDetailResponseForImages(product);
    assertEquals(NAME, productDetailResponse.getName());
    assertEquals(PRODUCT_CODE, productDetailResponse.getProductCode());
    assertTrue(CollectionUtils.isNotEmpty(productDetailResponse.getImages()));
    assertEquals(1, productDetailResponse.getImages().size());
  }

  @Test
  public void convertProductToProductDetailResponseForImages1Test() {
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(LOOKUP_GROUP);
    productItemImage.setOriginalImage(true);
    productItemImage.setMarkForDelete(false);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(LOOKUP_GROUP);
    productItemImage1.setOriginalImage(true);
    productItemImage1.setMarkForDelete(true);
    product.getProductItems().get(0).getProductItemImages().addAll(Arrays.asList(productItemImage, productItemImage1));
    ProductDetailResponse productDetailResponse =
        ConverterUtil.convertProductToProductDetailResponseForImages(product);
    assertEquals(NAME, productDetailResponse.getName());
    assertEquals(PRODUCT_CODE, productDetailResponse.getProductCode());
    assertTrue(CollectionUtils.isEmpty(productDetailResponse.getImages()));
    assertEquals(0, productDetailResponse.getImages().size());
  }

  @Test
  public void convertProductToProductDetailResponseForImages2Test() {
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(LOOKUP_GROUP);
    productItemImage.setOriginalImage(null);
    productItemImage.setMarkForDelete(false);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage);
    ProductDetailResponse productDetailResponse =
        ConverterUtil.convertProductToProductDetailResponseForImages(product);
    assertEquals(NAME, productDetailResponse.getName());
    assertEquals(PRODUCT_CODE, productDetailResponse.getProductCode());
    assertTrue(CollectionUtils.isEmpty(productDetailResponse.getImages()));
  }

  @Test
  public void convertProductToProductDetailResponseForImages3Test() {
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(LOOKUP_GROUP);
    productItemImage.setEdited(true);
    productItemImage.setMarkForDelete(false);
    productItemImage.setActive(true);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage);
    ProductDetailResponse productDetailResponse =
        ConverterUtil.convertProductToProductDetailResponseForImages(product);
    assertEquals(NAME, productDetailResponse.getName());
    assertEquals(PRODUCT_CODE, productDetailResponse.getProductCode());
    assertTrue(CollectionUtils.isEmpty(productDetailResponse.getImages()));
  }

  @Test
  public void convertProductToProductDetailResponseForImages4Test() {
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(LOOKUP_GROUP);
    productItemImage.setEdited(false);
    productItemImage.setMarkForDelete(false);
    productItemImage.setActive(true);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage);
    ProductDetailResponse productDetailResponse =
        ConverterUtil.convertProductToProductDetailResponseForImages(product);
    assertEquals(NAME, productDetailResponse.getName());
    assertEquals(PRODUCT_CODE, productDetailResponse.getProductCode());
    assertTrue(CollectionUtils.isEmpty(productDetailResponse.getImages()));
  }



  @Test
  public void setProductDetailsFromProductRequestTest() {
    image.setMainImages(true);
    productRequest.setImages(Arrays.asList(image));
    ConverterUtil.setProductDetailsFromProductRequest(STORE_ID, product, productRequest,
        new HashMap<>(), false, false);
    assertTrue(product.getProductImages().get(0).isMainImages());
  }

  @Test
  public void setProductDetailsFromProductRequestCreationTest() {
    image.setMainImages(true);
    productRequest.setImages(Arrays.asList(image));
    ConverterUtil.setProductDetailsFromProductRequest(STORE_ID, product, productRequest,
        new HashMap<>(), false, true);
    assertTrue(product.getProductImages().get(0).isMainImages());
  }

  @Test
  public void setProductDetailsFromProductRequestSuitabilityTest() {
    image.setMainImages(true);
    productRequest.setImages(Arrays.asList(image));
    ProductAttributeRequest productAttribute1 = new ProductAttributeRequest();
    productAttribute1.setAttribute(AttributeRequest.builder().hideForSeller(true).build());
    productRequest.setProductAttributes(List.of(productAttribute1));
    productRequest.getProductAttributes().get(0).getAttribute().setHideForSeller(true);
    ConverterUtil.setProductDetailsFromProductRequest(STORE_ID, product, productRequest,
        new HashMap<>(), true, false);
    assertTrue(product.getProductImages().get(0).isMainImages());
  }

  @Test
  public void setProductDetailsFromProductRequestCreationSuitabilityTest() {
    image.setMainImages(true);
    productRequest.setImages(Arrays.asList(image));
    ProductAttributeRequest productAttribute1 = new ProductAttributeRequest();
    productAttribute1.setAttribute(AttributeRequest.builder().hideForSeller(true).build());
    productRequest.setProductAttributes(List.of(productAttribute1));
    productRequest.getProductAttributes().get(0).getAttribute().setHideForSeller(true);
    ConverterUtil.setProductDetailsFromProductRequest(STORE_ID, product, productRequest,
        new HashMap<>(), true, true);
    assertTrue(product.getProductImages().get(0).isMainImages());
  }

  @Test
  public void setProductDetailsFromProductRequestSuitabilityNotHidenTest() {
    image.setMainImages(true);
    productRequest.setImages(Arrays.asList(image));
    ProductAttributeRequest productAttribute1 = new ProductAttributeRequest();
    productAttribute1.setAttribute(AttributeRequest.builder().hideForSeller(false).build());
    productRequest.setProductAttributes(List.of(productAttribute1));
    ConverterUtil.setProductDetailsFromProductRequest(STORE_ID, product, productRequest,
        new HashMap<>(), true, false);
    assertTrue(product.getProductImages().get(0).isMainImages());
  }

  @Test
  public void setProductDetailsFromProductRequest_nullSequenceTest() {
    image.setMainImages(false);
    image.setSequence(null);
    productRequest.setImages(Arrays.asList(image));
    ConverterUtil.setProductDetailsFromProductRequest(STORE_ID, product, productRequest,
        new HashMap<>(), false, false);
    assertTrue(product.getProductImages().get(0).isMainImages());
  }

  @Test
  public void setProductDetailsFromProductRequestCreation_nullSequenceTest() {
    image.setMainImages(false);
    image.setSequence(null);
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setValue("value");
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
    productAttributeValueRequest.setDescriptiveAttributeValueType(
        com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.NONE);
    productAttributeValueRequest.setAllowedAttributeValue(allowedAttributeValueRequest);
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(
        com.gdn.x.productcategorybase.dto.AttributeType.PREDEFINED_ATTRIBUTE);
    productAttributeRequest.setAttribute(attributeRequest);
    productAttributeRequest
        .setProductAttributeValues(Arrays.asList(productAttributeValueRequest));
    productRequest.setProductAttributes(List.of(productAttributeRequest));
    productRequest.setImages(Arrays.asList(image));
    ConverterUtil.setProductDetailsFromProductRequest(STORE_ID, product, productRequest,
        new HashMap<>(), false, true);
    assertTrue(product.getProductImages().get(0).isMainImages());
  }

  @Test
  public void setProductDetailsFromProductRequestEdit_nullSequenceTest() {
    image.setMainImages(false);
    image.setSequence(null);
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setValue("value");
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
    productAttributeValueRequest.setDescriptiveAttributeValueType(
        com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.NONE);
    productAttributeValueRequest.setAllowedAttributeValue(allowedAttributeValueRequest);
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(
        com.gdn.x.productcategorybase.dto.AttributeType.PREDEFINED_ATTRIBUTE);
    productAttributeRequest.setAttribute(attributeRequest);
    productAttributeRequest
        .setProductAttributeValues(Arrays.asList(productAttributeValueRequest));
    productRequest.setProductAttributes(List.of(productAttributeRequest));
    productRequest.setImages(Arrays.asList(image));
    ConverterUtil.setProductDetailsFromProductRequest(STORE_ID, product, productRequest,
        new HashMap<>(), false, false);
    assertTrue(product.getProductImages().get(0).isMainImages());
  }

  @Test
  public void setProductDetailsFromProductRequest_nonNullSequenceTest() {
    image.setMainImages(false);
    image.setSequence(0);
    productRequest.setImages(Arrays.asList(image));
    ConverterUtil.setProductDetailsFromProductRequest(STORE_ID, product, productRequest,
        new HashMap<>(), false, false);
    assertTrue(product.getProductImages().get(0).isMainImages());
  }

  @Test
  public void setProductDetailsFromProductRequest_nonZeroSequenceTest() {
    image.setMainImages(false);
    image.setSequence(1);
    productRequest.setImages(Arrays.asList(image));
    ConverterUtil.setProductDetailsFromProductRequest(STORE_ID, product, productRequest,
        new HashMap<>(), false, false);
    assertTrue(product.getProductImages().get(0).isMainImages());
  }

  @Test
  public void convertProductToProductAndAttributeDetailResponseTest() {
    ProductAndAttributeDetailResponse response = ConverterUtil.convertProductToProductAndAttributeDetailResponse(product);
    assertEquals(NAME, response.getName());
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertTrue(CollectionUtils.isNotEmpty(response.getProductAttributeResponses()));
  }

  @Test
  public void setProductItemImageForMissingVariantsTest() {
    ProductImage productImage1 = new ProductImage();
    productImage1.setMarkForDelete(true);
    ProductImage productImage2 = new ProductImage();
    productImage2.setMainImages(true);

    ProductItemImage productItemImage1 = new ProductItemImage();
    ProductItem productItem1 = new ProductItem();
    productItem1.setProductItemImages(Arrays.asList(productItemImage1));
    ProductItem productItem2 = new ProductItem();

    Product product = new Product();
    product.setProductImages(Arrays.asList(productImage1, productImage2));
    product.setProductItems(Arrays.asList(productItem1, productItem2));

    ConverterUtil.setProductItemImageForMissingVariants(product);
  }

  @Test
  public void setProductItemImageForMissingVariantsNoMainImageTest() {
    ProductImage productImage1 = new ProductImage();
    productImage1.setMarkForDelete(true);
    ProductImage productImage2 = new ProductImage();

    ProductItemImage productItemImage1 = new ProductItemImage();
    ProductItem productItem1 = new ProductItem();
    productItem1.setProductItemImages(Arrays.asList(productItemImage1));
    ProductItem productItem2 = new ProductItem();

    Product product = new Product();
    product.setProductImages(Arrays.asList(productImage1, productImage2));
    product.setProductItems(Arrays.asList(productItem1, productItem2));

    ConverterUtil.setProductItemImageForMissingVariants(product);
  }

  @Test
  public void setProductItemImageForMissingVariantsNoProductImageTest() {
    ProductImage productImage1 = new ProductImage();
    productImage1.setMarkForDelete(true);

    Product product = new Product();
    product.setProductImages(Arrays.asList(productImage1));

    ConverterUtil.setProductItemImageForMissingVariants(product);
  }

  @Test
  public void convertToAttributeResponsesTest() {
    attribute1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    List<AttributeResponse> attributeResponses =
        ConverterUtil.convertToAttributeResponses(Arrays.asList(attribute1), true);
    assertEquals(1, attributeResponses.size());
    assertNull(attributeResponses.get(0).getAllowedAttributeValues());
    assertNull(attributeResponses.get(0).getPredefinedAllowedAttributeValues());
  }

  @Test
  public void convertToAttributeResponsesFetchAttributeBasicDetailsFalseTest() {
    attribute1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    List<AttributeResponse> attributeResponses =
        ConverterUtil.convertToAttributeResponses(Arrays.asList(attribute1), false);
    assertEquals(1, attributeResponses.size());
    assertNotNull(attributeResponses.get(0).getAllowedAttributeValues());
    assertNotNull(attributeResponses.get(0).getPredefinedAllowedAttributeValues());
  }

  @Test
  public void convertToAttributeResponsesAttributesEmptyTest() {
    attribute1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    List<AttributeResponse> attributeResponses =
        ConverterUtil.convertToAttributeResponses(new ArrayList<>(), false);
    assertEquals(0, attributeResponses.size());
  }

  @Test
  public void getProductBrandUpdateDTOTest() {
    ProductBrandUpdateRequest productBrandUpdateRequest = new ProductBrandUpdateRequest();
    productBrandUpdateRequest.setProductCode(PRODUCT_CODE);
    productBrandUpdateRequest.setOldBrandCode(BRAND_CODE);
    productBrandUpdateRequest.setNewBrandCode(NEW_BRAND_CODE);
    ProductBrandUpdateDTO productBrandUpdateDTO = ConverterUtil.getProductBrandUpdateDTO(productBrandUpdateRequest);
    assertEquals(PRODUCT_CODE, productBrandUpdateDTO.getProductCode());
    assertEquals(BRAND_CODE, productBrandUpdateDTO.getOldBrandCode());
    assertEquals(NEW_BRAND_CODE, productBrandUpdateDTO.getNewBrandCode());
  }

  @Test
  public void getProductBrandUpdateResponseTest() {
    ProductBrandUpdateResponseDTO productBrandUpdateResponseDTO = new ProductBrandUpdateResponseDTO();
    productBrandUpdateResponseDTO.setProductCode(PRODUCT_CODE);
    productBrandUpdateResponseDTO.setBrandCode(NEW_BRAND_CODE);
    productBrandUpdateResponseDTO.setBrandName(BRAND_NAME);
    ProductBrandUpdateResponse productBrandUpdateResponse =
        ConverterUtil.getProductBrandUpdateResponse(productBrandUpdateResponseDTO);
    assertEquals(PRODUCT_CODE, productBrandUpdateResponse.getProductCode());
    assertEquals(NEW_BRAND_CODE, productBrandUpdateResponse.getBrandCode());
    assertEquals(BRAND_NAME, productBrandUpdateResponse.getBrandName());
  }

  @Test
  public void convertMasterAttributeRequestToAttributeTest() {
    AllowedAttributeValueRequest allowedAttributeValue = new AllowedAttributeValueRequest();
    allowedAttributeValue.setValue(VALUE);
    allowedAttributeValue.setSequence(SEQUENCE);
    allowedAttributeValue.setValueType(VALUE1);
    MasterAttributeRequest masterAttributeRequest = new MasterAttributeRequest();
    masterAttributeRequest.setSizeAttribute(true);
    masterAttributeRequest.setAttributeImageUrl(IMAGE_URL);
    masterAttributeRequest.setAttributeType(
        com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE);
    masterAttributeRequest.setAllowedAttributeValues(
        Collections.singletonList(allowedAttributeValue));
    masterAttributeRequest.setValueTypes(Collections.singletonList(VALUE1));
    Attribute attribute =
        ConverterUtil.convertMasterAttributeRequestToAttribute(masterAttributeRequest, STORE_ID);
    assertEquals(IMAGE_URL, attribute.getAttributeImageUrl());
    assertEquals(VALUE1,
        attribute.getAllowedAttributeValues().stream().findFirst().get().getValueType());
  }

  @Test
  public void convertMasterAttributeRequestToAttribute_predefinedAttributeTest() {
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setValue(VALUE);
    MasterAttributeRequest masterAttributeRequest = new MasterAttributeRequest();
    masterAttributeRequest.setAttributeType(
        com.gdn.x.productcategorybase.dto.AttributeType.PREDEFINED_ATTRIBUTE);
    masterAttributeRequest.setPredefinedAllowedAttributeValues(
        Collections.singletonList(predefinedAllowedAttributeValueRequest));
    Attribute attribute =
        ConverterUtil.convertMasterAttributeRequestToAttribute(masterAttributeRequest, STORE_ID);
    assertEquals(VALUE,
        attribute.getPredefinedAllowedAttributeValues().stream().findFirst().get().getValue());
  }

  @Test
  public void convertMasterAttributeRequestToAttribute_predefinedMultiValueTest() {
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setValue(VALUE);
    MasterAttributeRequest masterAttributeRequest = new MasterAttributeRequest();
    masterAttributeRequest.setAttributeType(
        com.gdn.x.productcategorybase.dto.AttributeType.PREDEFINED_MULTIVALUE);
    masterAttributeRequest.setPredefinedAllowedAttributeValues(
        Collections.singletonList(predefinedAllowedAttributeValueRequest));
    Attribute attribute =
        ConverterUtil.convertMasterAttributeRequestToAttribute(masterAttributeRequest, STORE_ID);
    assertEquals(VALUE,
        attribute.getPredefinedAllowedAttributeValues().stream().findFirst().get().getValue());
  }

  @Test
  public void convertMasterAttributeRequestValueTypeAttributeTest() {
    AllowedAttributeValueRequest allowedAttributeValue = new AllowedAttributeValueRequest();
    allowedAttributeValue.setValue(VALUE);
    allowedAttributeValue.setSequence(SEQUENCE);
    allowedAttributeValue.setValueType(VALUE1);
    MasterAttributeRequest masterAttributeRequest = new MasterAttributeRequest();
    masterAttributeRequest.setValueTypeAttribute(true);
    masterAttributeRequest.setAttributeType(
        com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE);
    masterAttributeRequest.setAllowedAttributeValues(
        Collections.singletonList(allowedAttributeValue));
    masterAttributeRequest.setValueTypes(Collections.singletonList(VALUE1));
    Attribute attribute =
        ConverterUtil.convertMasterAttributeRequestToAttribute(masterAttributeRequest, STORE_ID);
    assertEquals(VALUE1,
        attribute.getAllowedAttributeValues().stream().findFirst().get().getValueType());
  }

  @Test
  public void convertMasterAttributeRequestEmptyValueTypeAttributeTest() {
    AllowedAttributeValueRequest allowedAttributeValue = new AllowedAttributeValueRequest();
    allowedAttributeValue.setValue(VALUE);
    allowedAttributeValue.setSequence(SEQUENCE);
    allowedAttributeValue.setValueType(VALUE1);
    MasterAttributeRequest masterAttributeRequest = new MasterAttributeRequest();
    masterAttributeRequest.setValueTypeAttribute(true);
    masterAttributeRequest.setAttributeType(
        com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE);
    masterAttributeRequest.setAllowedAttributeValues(
        Collections.singletonList(allowedAttributeValue));
    Attribute attribute =
        ConverterUtil.convertMasterAttributeRequestToAttribute(masterAttributeRequest, STORE_ID);
    assertEquals(null,
        attribute.getAllowedAttributeValues().stream().findFirst().get().getValueType());
  }

  @Test
  public void convertMasterAttributeRequestFalseValueTypeAttributeTest() {
    AllowedAttributeValueRequest allowedAttributeValue = new AllowedAttributeValueRequest();
    allowedAttributeValue.setValue(VALUE);
    allowedAttributeValue.setSequence(SEQUENCE);
    allowedAttributeValue.setValueType(VALUE1);
    MasterAttributeRequest masterAttributeRequest = new MasterAttributeRequest();
    masterAttributeRequest.setValueTypeAttribute(false);
    masterAttributeRequest.setAttributeType(
        com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE);
    masterAttributeRequest.setAllowedAttributeValues(
        Collections.singletonList(allowedAttributeValue));
    Attribute attribute =
        ConverterUtil.convertMasterAttributeRequestToAttribute(masterAttributeRequest, STORE_ID);
    assertEquals(null,
        attribute.getAllowedAttributeValues().stream().findFirst().get().getValueType());
  }

  @Test
  public void convertMasterAttributeRequestNullValueTypeTest() {
    AllowedAttributeValueRequest allowedAttributeValue = new AllowedAttributeValueRequest();
    allowedAttributeValue.setValue(VALUE);
    allowedAttributeValue.setSequence(SEQUENCE);
    allowedAttributeValue.setValueType(null);
    MasterAttributeRequest masterAttributeRequest = new MasterAttributeRequest();
    masterAttributeRequest.setValueTypeAttribute(true);
    masterAttributeRequest.setValueTypes(Collections.singletonList(VALUE1));
    masterAttributeRequest.setAttributeType(
        com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE);
    masterAttributeRequest.setAllowedAttributeValues(
        Collections.singletonList(allowedAttributeValue));
    Attribute attribute =
        ConverterUtil.convertMasterAttributeRequestToAttribute(masterAttributeRequest, STORE_ID);
    assertEquals(null,
        attribute.getAllowedAttributeValues().stream().findFirst().get().getValueType());
  }

  @Test
  public void createAttributeAndItemIdMapTest() {
    AllowedAttributeValue allowedAttributeValue1 = new AllowedAttributeValue();
    allowedAttributeValue1.setValue(VALUE);
    allowedAttributeValue1.setValueType(VALUE_TYPE);
    ProductAttributeValue productAttributeValue1 = new ProductAttributeValue();
    productAttributeValue1.setAllowedAttributeValue(allowedAttributeValue1);
    ProductAttribute productAttribute1 = new ProductAttribute();
    productAttribute1.setProductAttributeValues(Arrays.asList(productAttributeValue1));

    AllowedAttributeValue allowedAttributeValue2 = new AllowedAttributeValue();
    allowedAttributeValue2.setValue(VALUE);
    ProductAttributeValue productAttributeValue2 = new ProductAttributeValue();
    productAttributeValue2.setAllowedAttributeValue(allowedAttributeValue1);
    ProductAttribute productAttribute2 = new ProductAttribute();
    productAttribute2.setProductAttributeValues(Arrays.asList(productAttributeValue2));

    ProductAttributeValue productAttributeValue3 = new ProductAttributeValue();
    ProductAttribute productAttribute3 = new ProductAttribute();
    productAttribute3.setProductAttributeValues(Arrays.asList(productAttributeValue3));

    Attribute attribute1 = new Attribute();
    attribute1.setVariantCreation(true);
    attribute1.setAttributeCode(ATTRIBUTE_CODE);
    ProductItemAttributeValue productItemAttributeValue1 = new ProductItemAttributeValue();
    productItemAttributeValue1.setAttribute(attribute1);
    productItemAttributeValue1.setValue(VALUE);

    ProductItemAttributeValue productItemAttributeValue2 = new ProductItemAttributeValue();
    productItemAttributeValue2.setAttribute(attribute1);
    productItemAttributeValue2.setValue(VALUE1);

    Attribute attribute2 = new Attribute();
    ProductItemAttributeValue productItemAttributeValue3 = new ProductItemAttributeValue();
    productItemAttributeValue3.setAttribute(attribute2);
    productItemAttributeValue3.setValue(VALUE);

    ProductItem productItem = new ProductItem();
    productItem.setId(ID);
    productItem.setProductItemAttributeValues(
        Arrays.asList(productItemAttributeValue1, productItemAttributeValue2, productItemAttributeValue3));

    Product product = new Product();
    product.setProductAttributes(Arrays.asList(productAttribute1, productAttribute2, productAttribute3));
    product.setProductItems(Arrays.asList(productItem));

    Map<String, String> map = ConverterUtil.createAttributeAndItemIdMap(product, true, "-");

    assertEquals(ID, map.entrySet().iterator().next().getValue());
  }

  @Test
  public void createAttributeAndItemIdMapValueTypeAdditionFalseTest() {
    AllowedAttributeValue allowedAttributeValue1 = new AllowedAttributeValue();
    allowedAttributeValue1.setValue(VALUE);
    allowedAttributeValue1.setValueType(VALUE_TYPE);
    ProductAttributeValue productAttributeValue1 = new ProductAttributeValue();
    productAttributeValue1.setAllowedAttributeValue(allowedAttributeValue1);
    ProductAttribute productAttribute1 = new ProductAttribute();
    productAttribute1.setProductAttributeValues(Arrays.asList(productAttributeValue1));

    AllowedAttributeValue allowedAttributeValue2 = new AllowedAttributeValue();
    allowedAttributeValue2.setValue(VALUE);
    ProductAttributeValue productAttributeValue2 = new ProductAttributeValue();
    productAttributeValue2.setAllowedAttributeValue(allowedAttributeValue1);
    ProductAttribute productAttribute2 = new ProductAttribute();
    productAttribute2.setProductAttributeValues(Arrays.asList(productAttributeValue2));

    ProductAttributeValue productAttributeValue3 = new ProductAttributeValue();
    ProductAttribute productAttribute3 = new ProductAttribute();
    productAttribute3.setProductAttributeValues(Arrays.asList(productAttributeValue3));

    Attribute attribute1 = new Attribute();
    attribute1.setVariantCreation(true);
    attribute1.setAttributeCode(ATTRIBUTE_CODE);
    ProductItemAttributeValue productItemAttributeValue1 = new ProductItemAttributeValue();
    productItemAttributeValue1.setAttribute(attribute1);
    productItemAttributeValue1.setValue(VALUE);

    ProductItemAttributeValue productItemAttributeValue2 = new ProductItemAttributeValue();
    productItemAttributeValue2.setAttribute(attribute1);
    productItemAttributeValue2.setValue(VALUE1);

    Attribute attribute2 = new Attribute();
    ProductItemAttributeValue productItemAttributeValue3 = new ProductItemAttributeValue();
    productItemAttributeValue3.setAttribute(attribute2);
    productItemAttributeValue3.setValue(VALUE);

    ProductItem productItem = new ProductItem();
    productItem.setId(ID);
    productItem.setProductItemAttributeValues(
        Arrays.asList(productItemAttributeValue1, productItemAttributeValue2, productItemAttributeValue3));

    Product product = new Product();
    product.setProductAttributes(Arrays.asList(productAttribute1, productAttribute2, productAttribute3));
    product.setProductItems(Arrays.asList(productItem));

    Map<String, String> map = ConverterUtil.createAttributeAndItemIdMap(product, false, "-");

    assertEquals(ID, map.entrySet().iterator().next().getValue());
  }

  @Test
  public void convertMasterAttributeRequestToAttributeExcludingAttributeValuesTest() {
    MasterAttributeRequest request = new MasterAttributeRequest();
    request.setSizeAttribute(true);
    request.setAttributeType(com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE);
    request.setAttributeImageUrl(IMAGE_URL);
    ConverterUtil.convertMasterAttributeRequestToAttributeExcludingAttributeValues(request,
        STORE_ID);
    Assertions.assertNotNull(request);
  }

  @Test
  public void convertMasterAttributeRequestToAttributeExcludingAttributeValuesFalseTest() {
    MasterAttributeRequest request = new MasterAttributeRequest();
    request.setSizeAttribute(true);
    request.setAttributeType(com.gdn.x.productcategorybase.dto.AttributeType.PREDEFINED_ATTRIBUTE);
    request.setAttributeImageUrl(IMAGE_URL);
    try {
      ConverterUtil.convertMasterAttributeRequestToAttributeExcludingAttributeValues(request,
          STORE_ID);
    } catch (ValidationException ex) {
      assertEquals(
          ErrorMessage.ATTRIBUTE_TYPE_CANNOT_BE_SIZE_ATTRIBUTE_ERROR_CODE.getMessage(),
          ex.getErrorCode());
      assertEquals(ErrorMessage.ATTRIBUTE_TYPE_CANNOT_BE_SIZE_ATTRIBUTE.getMessage(),
          ex.getErrorMessage());
    }
  }

  @Test
  public void convertMasterAttributeRequestToAttributeEmptyUrlTest() {
    MasterAttributeRequest request = new MasterAttributeRequest();
    request.setSizeAttribute(true);
    request.setAttributeType(com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE);
    try {
      ConverterUtil.convertMasterAttributeRequestToAttributeExcludingAttributeValues(request,
          STORE_ID);
    } catch (ValidationException ex) {
      assertEquals(ErrorMessage.ATTRIBUTE_IMAGE_URL_MUST_NOT_BE_EMPTY.getMessage(),
          ex.getErrorMessage());
    }
  }

  @Test
  void setVideoDTOExceptionTest() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    ConverterUtil.setVideoDTO(product, productDetailResponse);
    assertNull(productDetailResponse.getVideoDTO());
  }

  @Test
  void setVideoDTOTest() throws JsonProcessingException {
    String url = "url";
    VideoDTO videoDTO = new VideoDTO();
    videoDTO.setSourceUrl(url);
    product.setVideo(new ObjectMapper().writeValueAsString(videoDTO));
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    ConverterUtil.setVideoDTO(product, productDetailResponse);
    assertNotNull(productDetailResponse.getVideoDTO());
  }

  @Test
  void setVideoDTOWithAllFieldsTest() throws JsonProcessingException {
    String videoId = "video123";
    String finalUrl = "https://example.com/videos/final";
    String videoName = "Product Demo Video";
    String coverImagePath = "/images/cover.jpg";
    String sourceUrl = "https://example.com/videos/source";
    VideoDTO originalVideoDTO =
      VideoDTO.builder().videoId(videoId).finalUrl(finalUrl).videoName(videoName)
        .coverImagePath(coverImagePath).sourceUrl(sourceUrl).build();
    product.setVideo(new ObjectMapper().writeValueAsString(originalVideoDTO));
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    ConverterUtil.setVideoDTO(product, productDetailResponse);
    VideoDTO resultDTO = productDetailResponse.getVideoDTO();
    assertNotNull(resultDTO);
    assertEquals(videoId, resultDTO.getVideoId());
    assertEquals(finalUrl, resultDTO.getFinalUrl());
    assertEquals(videoName, resultDTO.getVideoName());
    assertEquals(coverImagePath, resultDTO.getCoverImagePath());
    assertEquals(sourceUrl, resultDTO.getSourceUrl());
  }

  @Test
  void setVideoDTOWithInvalidJsonTest() {
    product.setVideo("{ invalid json : this will cause parsing error }");
    product.setProductCode("TEST-123");
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    ConverterUtil.setVideoDTO(product, productDetailResponse);
    assertNull(productDetailResponse.getVideoDTO());
  }

  @Test
  void convertVideoAddEditRequestToDTOTest(){
    VideoAddEditRequest videoAddEditRequest = new VideoAddEditRequest();
    videoAddEditRequest.setVideoUrl("url");
    String result = ConverterUtil.convertVideoAddEditRequestToDTO(videoAddEditRequest);
    assertTrue(result.contains("url"));
  }

  @Test
  void convertVideoAddEditRequestToDTOExceptionTest(){
    String result = ConverterUtil.convertVideoAddEditRequestToDTO(null);
    assertTrue(result.isEmpty());
  }

}
