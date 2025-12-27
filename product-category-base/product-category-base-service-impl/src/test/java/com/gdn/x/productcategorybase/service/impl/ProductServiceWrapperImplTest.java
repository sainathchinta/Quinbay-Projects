package com.gdn.x.productcategorybase.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.domain.event.model.CommonImageBackfillingEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CompressedVideoUpdateEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductSuitabilityAttributeModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductSuitabilityEventModel;
import com.gdn.x.productcategorybase.domain.event.model.VendorPublishEventModel;
import com.gdn.x.productcategorybase.dto.MasterProductDataUpdateDTO;
import com.gdn.x.productcategorybase.dto.ProductBrandUpdateDTO;
import com.gdn.x.productcategorybase.dto.ProductBrandUpdateResponseDTO;
import com.gdn.x.productcategorybase.dto.ProductDTO;
import com.gdn.x.productcategorybase.dto.ProductDetailEditDTO;
import com.gdn.x.productcategorybase.dto.VideoDTO;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.EditProductDetailRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductMasterDataUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductMigrationRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.AuditTrailDto;
import com.gdn.x.productcategorybase.dto.response.AuditTrailListResponse;
import com.gdn.x.productcategorybase.dto.response.EditProductItemAndImageResponse;
import com.gdn.x.productcategorybase.dto.response.NewlySavedItemResponse;
import com.gdn.x.productcategorybase.entity.ExtractionStatus;
import com.gdn.x.productcategorybase.entity.ProductAttributeExtracted;
import com.gdn.x.productcategorybase.enums.UpdatedFields;
import com.gdn.x.productcategorybase.repository.ProductRepository;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.brand.BrandService;
import com.gdn.x.productcategorybase.service.CategoryReferenceService;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.CategoryShippingService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.FileStorageService;
import com.gdn.x.productcategorybase.service.ImageService;
import com.gdn.x.productcategorybase.service.PredefinedAllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.ProductAttributeExtractionService;
import com.gdn.x.productcategorybase.service.ProductItemService;
import com.gdn.x.productcategorybase.service.ProductItemServiceWrapper;
import com.gdn.x.productcategorybase.service.SizeChartService;
import com.gdn.x.productcategorybase.service.ProductMigrationService;
import com.gdn.x.productcategorybase.service.ProductScoreService;
import com.gdn.x.productcategorybase.service.ProductService;
import com.gdn.x.productcategorybase.service.SchedulerService;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import com.gdn.x.productcategorybase.config.KafkaPublisher;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import com.gdn.common.util.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.inventory.dto.WarehouseMasterSKUEvent;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.ExtractionType;
import com.gdn.x.productcategorybase.ProductMasterEvent;
import com.gdn.x.productcategorybase.ProductMigrationStatus;
import com.gdn.x.productcategorybase.ProductMigrationType;
import com.gdn.x.productcategorybase.domain.event.model.AggregateImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AggregateProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AggregateProductCategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AggregateProductItemDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductSalesCategoryMapping;
import com.gdn.x.productcategorybase.dto.CategoryChangeDTO;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.LocationPathAndCommonImage;
import com.gdn.x.productcategorybase.dto.ProductAndItemLevelUpdatesDTO;
import com.gdn.x.productcategorybase.dto.ProductPublishUpdateDTO;
import com.gdn.x.productcategorybase.dto.SimpleMasterProductUpdateDTO;
import com.gdn.x.productcategorybase.dto.SimpleMasterProductUpdateResponseDTO;
import com.gdn.x.productcategorybase.dto.UpdateNeedRevisionDTO;
import com.gdn.x.productcategorybase.dto.request.BatchVatUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.NeedRevisionConfigRequest;
import com.gdn.x.productcategorybase.dto.request.ProductBrandDataUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest;
import com.gdn.x.productcategorybase.domain.event.model.BrandHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.BrandDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrUpdateBrandDomainEventModel;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.entity.solr.SolrUpdateBrandModel;
import org.apache.commons.lang3.tuple.Triple;
import com.gdn.x.productcategorybase.dto.request.ProductItemImageUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CopyImageEditRequest;
import com.gdn.x.productcategorybase.dto.response.BatchVatUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.ImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductItemCompleteResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.SimpleItemDetailResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.entity.ProductMigration;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.outbound.matrix.MatrixOutbound;
import com.gdn.x.productcategorybase.outbound.model.MatrixAttributeExtractionRequest;
import com.gdn.x.productcategorybase.outbound.model.MatrixAttributeExtractionResponse;
import com.gdn.x.productcategorybase.outbound.model.SingleBaseResponse;
import com.gdn.x.productcategorybase.util.AggregateUtil;
import com.google.common.collect.ImmutableSet;
import com.gdn.x.productcategorybase.util.CommonUtil;

public class ProductServiceWrapperImplTest {

  public static final String VIDEO_ID = "VID123";
  public static final String FINAL_URL = "http://example.com/video.mp4";
  public static final String COVER_IMAGE_PATH = "http://example.com/cover.jpg";
  public static final String TEST_VIDEO = "Test Video";
  public static final String SOURCE_URL = "http://example.com/source.mp4";
  public static final String EXISTING_VIDEO_IN_PRODUCT =
    "{\"videoId\":\"VID123\",\"finalUrl\":\"http://old.example.com/video.mp4\",\"coverImagePath\":\"http://old.example.com/cover.jpg\",\"videoName\":\"Old Video\",\"sourceUrl\":\"http://old.example.com/source.mp4\"}";
  public static final String VIDEO_JSON =
    "{\"videoId\":\"%s\",\"finalUrl\":\"http://old.example.com/video.mp4\",\"coverImagePath\":\"http://old.example.com/cover.jpg\",\"videoName\":\"Old Video\",\"sourceUrl\":\"http://old.example.com/source.mp4\"}";
  public static final String PRODUCT_CODE_NOT_FOUND_ERROR =
    "Can not process invalid input data :Product not found : productCode";
  public static final String CLIENT_ID = "clientId";
  @Mock
  private ProductService productService;

  @Mock
  private BrandService brandService;

  @Mock
  private PredefinedAllowedAttributeValueService predefinedAllowedAttributeValueService;

  @Mock
  private DomainEventPublisherService domainEventPublisherService;

  @Mock
  private ProductItemService productItemService;

  @Mock
  private CategoryService categoryService;

  @Mock
  private CategoryReferenceService categoryReferenceService;

  @Mock
  private CategoryShippingService categoryShippingService;

  @Mock
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Mock
  private ProductScoreService productScoreService;

  @Mock
  private ProductMigrationService productMigrationService;

  @Mock
  private ProductAttributeExtractionService productAttributeExtractionService;

  @Mock
  private ImageService imageService;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private MatrixOutbound matrixOutbound;

  @Mock
  private SchedulerService schedulerService;

  @Mock
  private ProductItemServiceWrapper productItemServiceWrapper;

  @Mock
  private AttributeService attributeService;

  @Mock
  private SizeChartService sizeChartService;

  @Mock
  private KafkaPublisher kafkaPublisher;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Captor
  private ArgumentCaptor<Product> productArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductSalesCategoryMapping> productSalesCategoryMappingArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductMigration> commonImageMigrationArgumentCaptor;

  @InjectMocks
  private ProductServiceWrapperImpl productServiceWrapper;

  @Captor
  private ArgumentCaptor<VideoDTO> videoDTOCaptor;

  @Mock
  private ProductRepository productRepository;

  private static final String STORE_ID = "storeId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String ID = "id";
  private static final String PRODUCT_ID = "productId";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_CODE1 = "MTA-1234567";
  private static final List<String> PRODUCT_CODES = toSingleList(PRODUCT_CODE);
  private static final String SKU_CODE = "skuCode";
  private static final String SKU_CODE_1 = "skuCode1";
  private static final String LOCATION_PATH = "locationPath";
  private static final String PRODUCT_NAME = "productName";
  private static final String PRODUCT_NAME1 = "productName1";
  private static final String ORIGINAL_PRODUCT_NAME = "originalProductName";
  private static final String BRAND = "brand";
  private static final String BRAND_CODE = "brand-code";
  private static final String BRAND_CODE_2 = "brand-code2";
  private static final String ORIGINAL_BRAND = "originalBrand";
  private static final Double LENGTH = 1.0;
  private static final Double ORIGINAL_LENGTH = 2.0;
  private static final Double WIDTH = 1.0;
  private static final Double ORIGINAL_WIDTH = 2.0;
  private static final Double HEIGHT = 1.0;
  private static final Double ORIGINAL_HEIGHT = 2.0;
  private static final Double WEIGHT = 1.0;
  private static final Double ORIGINAL_WEIGHT = 2.0;
  private static final Double SHIPPING_WEIGHT = 1.0;
  private static final Double ORIGINAL_SHIPPING_WEIGHT = 2.0;
  private static final Integer DANGEROUS_GOODS_LEVEL = 1;
  private static final String BRAND_ATTRIBUTE_NAME = "Brand";
  private static final String BRAND_ATTRIBUTE_CODE = "brandAttributeCode";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final int TOTAL_ELEMENTS = 1;
  private static final String ITEM_NAME = "itemName";
  private static final String CATEGORY_CODE = "category_code";
  private static final String CATEGORY_CODE_1 = "categoryCode1";
  private static final String CATEGORY_NAME_1 = "categoryName1";
  private static final String CATEGORY_CODE_2 = "categoryCode2";
  private static final String CATEGORY_NAME_2 = "categoryName2";
  private static final String CATEGORY_CODE_NEW = "categoryCodeNew";
  private static final String CATEGORY_NAME_NEW = "categoryNameNew";
  private static final String CATEGORY_ID_1 = "categoryId1";
  private static final String CATEGORY_ID_2 = "categoryId2";
  private static final String CATEGORY_ID_NEW = "categoryIdNew";
  private static final String UPC_CODE = "upcCode";
  private static final String CATALOG_CODE = "catalog_code";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final Pageable DEFAULT_PAGEABLE = PageRequest.of(0, 2);
  private static final String ERROR_MESSAGE_FOR_UPDATE_SCORE = "Error while updating the product's score.";
  private static final String BUSINESS_PARTNER_CODE = "bpCode";
  private static final String MERCHANT_TYPE_TD = "TD";
  private static final String MERCHANT_TYPE_CM = "CM";
  private static final String IMAGE_PATH_1 = "imagePath1";
  private static final String IMAGE_PATH_2 = "imagePath2";
  private static final String ATTRIBUTE_NAME = "attributeName";
  private static final String ATTRIBUTE_NAME_2 = "attributeName2";
  private static final String ATTRIBUTE_VALUE = "attributeValue";
  private static final String OLD_PRODUCT = "oldProduct";
  private static final String NEW_PRODUCT = "newProduct";
  private static final String ATTRIBUTE_CODE = "attributeCode";
  private static final String ATTRIBUTE_VALUE_EN = "attributeValueEn";
  private static final String ATTRIBUTE_ID = "attributeId";
  private static final String PRODUCT_ATTRIBUTE_VALUE_ID = "productAttributeValueId";
  private static final String CATEGORY_ID = "categoryId";

  private SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO;
  private Product productData;
  private ProductItem productItem = new ProductItem();
  private ProductItem productItem1 = new ProductItem();
  private ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
  private Attribute attributeData = new Attribute();
  private PredefinedAllowedAttributeValue originalBrandAttribute = new PredefinedAllowedAttributeValue();
  private ProductAttribute productAttribute = new ProductAttribute();
  private ProductAttributeValue productAttributeValue = new ProductAttributeValue();
  private ProductAttributeValue productAttributeValue1 = new ProductAttributeValue();
  private ProductImage productImage = new ProductImage();
  private ProductItemImage productItemImage = new ProductItemImage();
  private ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
  private Image image = new Image();
  private PredefinedAllowedAttributeValue updatedBrandAttribute;
  private Brand brand;
  private ProductAndItemImageRequest productAndItemImageRequest;
  private Pageable pageable;
  private Category category2;
  private Category categoryNew;
  private WarehouseMasterSKUEvent warehouseMasterSKUEvent;
  private ProductCategory productCategoryData = new ProductCategory();
  private Category category = new Category();
  private Catalog catalog = new Catalog();
  private List<String> productScoreUpdateList;
  private ProductMigration commonImageMigration;
  private ProductBrandUpdateDTO productBrandUpdateDTO;

  private ProductRequest productRequest;

  private static final Product PRODUCT = generateProduct();
  private static final ProductDomainEventModel PRODUCT_DOMAIN_EVENT_MODEL =
      AggregateUtil.toProductDomainEventModel(PRODUCT);
  private static final AggregateImageDomainEventModel AGGREGATE_IMAGE_DOMAIN_EVENT_MODEL =
      AggregateUtil.toAggregateImageDomainEventModel(PRODUCT.getProductImages());
  private static final AggregateProductItemDomainEventModel AGGREGATE_PRODUCT_ITEM_DOMAIN_EVENT_MODEL =
      AggregateUtil.toAggregateProductItemDomainEventModel(PRODUCT.getProductItems());
  private static final AggregateProductCategoryDomainEventModel AGGREGATE_PRODUCT_CATEGORY_DOMAIN_EVENT_MODEL =
      AggregateUtil.toAggregateProductCategoryDomainEventModel(PRODUCT.getProductCategories());
  private static final AggregateProductAttributeDomainEventModel AGGREGATE_PRODUCT_ATTRIBUTE_DOMAIN_EVENT_MODEL =
      AggregateUtil.toAggregateProductAttributeDomainEventModel(PRODUCT.getProductAttributes());
  private BatchVatUpdateRequest batchVatUpdateRequest;
  private CompressedVideoUpdateEventModel compressedVideoUpdateEventModel;

  private CommonImageBackfillingEventModel commonImageBackfillingEventModel;

  @BeforeEach
  public void init(){
    initMocks(this);
    simpleMasterProductUpdateDTO = new SimpleMasterProductUpdateDTO();
    simpleMasterProductUpdateDTO.setProductCode(PRODUCT_CODE);
    simpleMasterProductUpdateDTO.setName(PRODUCT_NAME1);
    simpleMasterProductUpdateDTO.setBrand(BRAND);
    simpleMasterProductUpdateDTO.setLength(LENGTH);
    simpleMasterProductUpdateDTO.setWeight(WEIGHT);
    simpleMasterProductUpdateDTO.setWidth(WIDTH);
    simpleMasterProductUpdateDTO.setHeight(HEIGHT);
    simpleMasterProductUpdateDTO.setShippingWeight(SHIPPING_WEIGHT);
    simpleMasterProductUpdateDTO.setDangerousGoodsLevel(DANGEROUS_GOODS_LEVEL);

    productRequest = new ProductRequest();
    productRequest.setProductCode(PRODUCT_CODE);
    productRequest.setScoreUpdated(true);
    ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
    CategoryRequest categoryRequest = new CategoryRequest();
    categoryRequest.setCategoryCode(CATEGORY_CODE);
    categoryRequest.setProductCategories(Arrays.asList(productCategoryRequest));
    categoryRequest.setSalesCategoryReferences(Collections.emptyList());
    productCategoryRequest.setCategory(categoryRequest);
    productRequest.setProductCategories(Arrays.asList(productCategoryRequest));
    productData = new Product.Builder().productCode(PRODUCT_CODE).name(ORIGINAL_PRODUCT_NAME).length(ORIGINAL_LENGTH)
        .width(ORIGINAL_WIDTH).height(ORIGINAL_HEIGHT).weight(ORIGINAL_WEIGHT).shippingWeight(ORIGINAL_SHIPPING_WEIGHT)
        .brand(ORIGINAL_BRAND).storeId(STORE_ID).build();
    productData.setId(PRODUCT_ID);
    productData.setProductItems(new ArrayList<>());
    productItem.setMarkForDelete(true);
    productItem.setInternalUpdate(false);
    productItem.setDangerousGoodsLevel(0);
    productItem.setProductId(PRODUCT_ID);
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setProductItem(productItem);
    attributeData.setName(BRAND_ATTRIBUTE_NAME);
    productItemAttributeValue.setAttribute(attributeData);
    productItemAttributeValue.setValue(ORIGINAL_BRAND);
    productItem.setProductItemAttributeValues(new ArrayList<>());
    productItem.getProductItemAttributeValues().add(productItemAttributeValue);
    productData.getProductItems().add(productItem);
    originalBrandAttribute.setPredefinedAllowedAttributeCode(BRAND_ATTRIBUTE_NAME);
    originalBrandAttribute.setValue(ORIGINAL_BRAND);
    updatedBrandAttribute = new PredefinedAllowedAttributeValue();
    updatedBrandAttribute.setPredefinedAllowedAttributeCode(BRAND_ATTRIBUTE_NAME);
    updatedBrandAttribute.setValue(BRAND);
    productAttribute.setProductAttributeName(BRAND_ATTRIBUTE_NAME);
    attributeData.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    productAttribute.setAttribute(attributeData);
    productAttributeValue.setPredefinedAllowedAttributeValue(originalBrandAttribute);
    productAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    productAttribute.setProductAttributeValues(Arrays.asList(productAttributeValue));
    productData.setProductAttributes(Arrays.asList(productAttribute));
    category.setCategoryCode(CATEGORY_CODE);
    catalog.setCatalogCode(CATALOG_CODE);
    category.setCatalog(catalog);
    category.setDangerousGoodsLevel(1);
    productCategoryData.setCategory(category);
    productData.setProductCategories(Arrays.asList(productCategoryData));
    brand = new Brand();
    brand.setBrandCode(BRAND_ATTRIBUTE_CODE);
    brand.setBrandName(BRAND_ATTRIBUTE_NAME);
    image.setLocationPath(LOCATION_PATH);
    BeanUtils.copyProperties(image, productItemImage);
    BeanUtils.copyProperties(image, productImage);
    productData.setProductImages(Arrays.asList(productImage));
    productData.getProductItems().get(0).setSkuCode(SKU_CODE);
    productData.getProductItems().get(0).setProductItemImages(Arrays.asList(productItemImage));
    productAndItemImageRequest = new ProductAndItemImageRequest();
    productAndItemImageRequest.setProductCode(PRODUCT_CODE);
    productAndItemImageRequest.setProductImages(Arrays.asList(image));
    productItemImageRequest.setSkuCode(SKU_CODE);
    productItemImageRequest.setItemImages(Arrays.asList(image));
    productAndItemImageRequest.setProductItemImages(Arrays.asList(productItemImageRequest));

    productItem.setProduct(productData);
    productItem.setSkuCode(SKU_CODE);
    productItem.setGeneratedItemName(PRODUCT_NAME + ITEM_NAME);
    pageable = PageRequest.of(PAGE, SIZE);

    category = new Category();
    category.setCategoryCode(CATEGORY_CODE_1);
    category.setName(CATEGORY_NAME_1);
    category.setId(CATEGORY_ID_1);
    category.setDangerousGoodsLevel(1);

    category2 = new Category();
    category2.setName(CATEGORY_NAME_2);
    category2.setCategoryCode(CATEGORY_CODE_2);
    category2.setId(CATEGORY_ID_2);
    category2.setActivated(true);
    category2.setDisplay(true);

    categoryNew = new Category();
    categoryNew.setCategoryCode(CATEGORY_CODE_NEW);
    categoryNew.setName(CATEGORY_NAME_NEW);
    categoryNew.setId(CATEGORY_ID_NEW);
    categoryNew.setDisplay(true);
    categoryNew.setActivated(true);


    ProductCategory productCategory2 = new ProductCategory(productData, category2, STORE_ID);
    productCategory2.setMarkForDelete(true);
    List<ProductCategory> productCategoryList = new ArrayList<>();
    productCategoryList.add(new ProductCategory(productData, category, STORE_ID));
    productCategoryList.add(productCategory2);
    productData.setProductCategories(productCategoryList);

    warehouseMasterSKUEvent = WarehouseMasterSKUEvent.builder().build().builder().itemCode(PRODUCT_CODE).height(HEIGHT)
        .length(LENGTH).weight(WEIGHT).width(WIDTH).upcCodes(Arrays.asList(UPC_CODE)).build();

    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(PRODUCT);
    when(productService.getProductByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID)).thenReturn(PRODUCT);

    productScoreUpdateList = new ArrayList<>();
    productScoreUpdateList.add(PRODUCT_CODE);
    productScoreUpdateList.add(PRODUCT_CODE1);

    Map<String, Boolean> itemCodeToVatMap = Collections.singletonMap(SKU_CODE, Boolean.TRUE);
    batchVatUpdateRequest =
        BatchVatUpdateRequest.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).itemCodeToVatMap(itemCodeToVatMap)
            .build();
    productItem1.setSkuCode(SKU_CODE_1);

    commonImageMigration = ProductMigration.builder().productCode(PRODUCT_CODE)
      .migrationType(ProductMigrationType.COMMON_IMAGE_MIGRATION.name())
      .status(ProductMigrationStatus.PUBLISHED.name()).build();
    ReflectionTestUtils.setField(productServiceWrapper, "validateDimensionSwitch", Boolean.FALSE);
    ReflectionTestUtils.setField(productServiceWrapper, "publishVendorEventForDsAttributeMappingEnabled", Boolean.TRUE);

    productBrandUpdateDTO = new ProductBrandUpdateDTO();
    productBrandUpdateDTO.setProductCode(PRODUCT_CODE);
    productBrandUpdateDTO.setOldBrandCode(BRAND_CODE);
    productBrandUpdateDTO.setNewBrandCode(BRAND_ATTRIBUTE_CODE);
    compressedVideoUpdateEventModel = new CompressedVideoUpdateEventModel();
    compressedVideoUpdateEventModel.setAdditionalFields(Map.of(Constants.PRODUCT_CODE, PRODUCT_CODE));
    compressedVideoUpdateEventModel.setVideoId(VIDEO_ID);
    compressedVideoUpdateEventModel.setFinalUrl(FINAL_URL);
    compressedVideoUpdateEventModel.setCoverImagePath(COVER_IMAGE_PATH);
    compressedVideoUpdateEventModel.setVideoName(TEST_VIDEO);
    compressedVideoUpdateEventModel.setSourceUrl(SOURCE_URL);
    compressedVideoUpdateEventModel.setClientId(CLIENT_ID);
    BeanUtils.copyProperties(productAttributeValue, productAttributeValue1);

    commonImageBackfillingEventModel = new CommonImageBackfillingEventModel();
    commonImageBackfillingEventModel.setStoreId(STORE_ID);
    commonImageBackfillingEventModel.setProductCode(PRODUCT_CODE);
    commonImageBackfillingEventModel.setMigrationType("DELETE_MFD_TRUE_IMAGES_AND_ATTRIBUTES");
    ReflectionTestUtils.setField(productServiceWrapper, "clientIdForVideoCompression", CLIENT_ID);
  }

  @Test
  public void updateMasterProductDataTest() throws Exception {
    when(productService.updateProductAndGetDimensionChanged(STORE_ID,
      simpleMasterProductUpdateDTO)).thenReturn(new MasterProductDataUpdateDTO(productData, true, false));
    SimpleMasterProductUpdateResponseDTO responseDTO =
        productServiceWrapper.updateMasterProductData(STORE_ID, simpleMasterProductUpdateDTO);
    verify(productService).evictCacheForSimpleMasterProductUpdate(STORE_ID, PRODUCT_ID,
      PRODUCT_CODE);
    verify(productService).updateProductAndGetDimensionChanged(STORE_ID, simpleMasterProductUpdateDTO);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, PRODUCT, false);
    verify(domainEventPublisherService).publishProductChangeCategory(PRODUCT, null, false, true, true, false, new HashSet<>());
    assertTrue(responseDTO.getUpdateSuccess());
    assertEquals(PRODUCT_CODE, responseDTO.getProductCode());
  }

  @Test
  public void updateMasterProductDataTestException() throws Exception {
    doThrow(ApplicationRuntimeException.class).when(productService)
        .updateProductAndGetDimensionChanged(STORE_ID, simpleMasterProductUpdateDTO);
    SimpleMasterProductUpdateResponseDTO responseDTO =
        productServiceWrapper.updateMasterProductData(STORE_ID, simpleMasterProductUpdateDTO);
    verify(productService).updateProductAndGetDimensionChanged(STORE_ID, simpleMasterProductUpdateDTO);
    Assertions.assertFalse(responseDTO.getUpdateSuccess());
    assertEquals(PRODUCT_CODE, responseDTO.getProductCode());
  }

  @Test
  public void updateProductAndItemImagesByProductCodeSetDgFalse() throws Exception {
    when(productService.updateProductAndItemImagesByProductCode(productAndItemImageRequest,
      STORE_ID, false)).thenReturn(productData);
    Mockito.when(this.productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(eq(STORE_ID),
        anyString())).thenReturn(productData);
    boolean status = productServiceWrapper.updateProductAndItemImagesByProductCode(productAndItemImageRequest, STORE_ID, false);
    verify(productService).updateProductAndItemImagesByProductCode(productAndItemImageRequest, STORE_ID, false);
    verify(domainEventPublisherService).publishProductChangeCategory(any(), any(), anyBoolean(),
        anyBoolean(), anyBoolean(), anyBoolean(), anyBoolean(),
        Mockito.anySet(), anyBoolean(), anyBoolean(), Mockito.anySet());
    verify(productService).evictCompleteProductAndItemsCache(anyString(), any());
    assertTrue(status);
  }

  @Test
  public void updateProductAndItemImagesByProductCode() throws Exception {
    when(productService.updateProductAndItemImagesByProductCode(productAndItemImageRequest,
      STORE_ID, true)).thenReturn(productData);
    Mockito.when(this.productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(eq(STORE_ID),
        anyString())).thenReturn(productData);
    boolean status = productServiceWrapper.updateProductAndItemImagesByProductCode(productAndItemImageRequest, STORE_ID, true);
    verify(productService).updateProductAndItemImagesByProductCode(productAndItemImageRequest, STORE_ID, true);
    verify(domainEventPublisherService).publishProductChangeCategory(any(), any(), anyBoolean(),
        anyBoolean(), anyBoolean(), anyBoolean(), anyBoolean(),
        Mockito.anySet(), anyBoolean(), anyBoolean(), Mockito.anySet());
    verify(productService).evictCompleteProductAndItemsCache(anyString(), any());
    assertTrue(status);
  }

  private static  <T> List<T> toSingleList(T object) {
    List<T> result = new ArrayList<>();
    result.add(object);
    return result;
  }

  private static Product generateProduct() {
    Product product = new Product();
    product.setId(PRODUCT_ID);
    product.setStoreId(STORE_ID);
    product.setProductCode(PRODUCT_CODE);

    ProductImage productImage = new ProductImage();
    productImage.setId(ID);
    productImage.setStoreId(STORE_ID);
    product.setProductImages(toSingleList(productImage));

    ProductItem productItem = new ProductItem();
    productItem.setId(ID);
    productItem.setStoreId(STORE_ID);
    product.setProductItems(toSingleList(productItem));

    ProductCategory productCategory = new ProductCategory();
    productCategory.setId(ID);
    productCategory.setStoreId(STORE_ID);
    product.setProductCategories(toSingleList(productCategory));

    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setId(ID);
    productAttribute.setStoreId(STORE_ID);
    product.setProductAttributes(toSingleList(productAttribute));

    return product;
  }

  @Test
  public void republishProductByProductCodesToAgpTest_splitTrue() throws Exception {
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE))
        .thenReturn(PRODUCT);
    when(domainEventPublisherService.toProductDomainEventModel(PRODUCT,false, StringUtils.EMPTY))
        .thenReturn(PRODUCT_DOMAIN_EVENT_MODEL);
    when(domainEventPublisherService.republishImageToAgp(AGGREGATE_IMAGE_DOMAIN_EVENT_MODEL))
        .thenReturn(AGGREGATE_IMAGE_DOMAIN_EVENT_MODEL);
    when(domainEventPublisherService.republishProductItemToAgp(AGGREGATE_PRODUCT_ITEM_DOMAIN_EVENT_MODEL))
        .thenReturn(AGGREGATE_PRODUCT_ITEM_DOMAIN_EVENT_MODEL);
    when(domainEventPublisherService.republishProductCategoryToAgp(AGGREGATE_PRODUCT_CATEGORY_DOMAIN_EVENT_MODEL))
        .thenReturn(AGGREGATE_PRODUCT_CATEGORY_DOMAIN_EVENT_MODEL);
    when(domainEventPublisherService.republishProductAttributeToAgp(AGGREGATE_PRODUCT_ATTRIBUTE_DOMAIN_EVENT_MODEL))
        .thenReturn(AGGREGATE_PRODUCT_ATTRIBUTE_DOMAIN_EVENT_MODEL);
    when(domainEventPublisherService.republishProductToAgp(PRODUCT_DOMAIN_EVENT_MODEL))
        .thenReturn(PRODUCT_DOMAIN_EVENT_MODEL);

    productServiceWrapper.republishProductByProductCodesToAgp(STORE_ID, PRODUCT_CODES, true);

    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID,PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, PRODUCT, true);
    verify(domainEventPublisherService).toProductDomainEventModel(PRODUCT,false, StringUtils.EMPTY);
    verify(domainEventPublisherService).republishImageToAgp(any(AggregateImageDomainEventModel.class));
    verify(domainEventPublisherService).republishProductItemToAgp(any(AggregateProductItemDomainEventModel.class));
    verify(domainEventPublisherService).republishProductCategoryToAgp(any(AggregateProductCategoryDomainEventModel.class));
    verify(domainEventPublisherService).republishProductAttributeToAgp(any(AggregateProductAttributeDomainEventModel.class));
    verify(domainEventPublisherService).republishProductToAgp(PRODUCT_DOMAIN_EVENT_MODEL);
  }

  @Test
  public void republishProductByProductCodesToAgpTest_splitTrue_errorWhenFindByStoreIdAndProductCode() throws Exception {
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE))
        .thenThrow(new ApplicationRuntimeException());

    productServiceWrapper.republishProductByProductCodesToAgp(STORE_ID, PRODUCT_CODES, true);

    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID,PRODUCT_CODE);
  }

  @Test
  public void republishProductByProductCodesToAgpTest_splitTrue_errorWhenPublishProductToAgp() throws Exception {
    when(domainEventPublisherService.toProductDomainEventModel(PRODUCT,false, StringUtils.EMPTY))
        .thenReturn(PRODUCT_DOMAIN_EVENT_MODEL);
    doThrow(RuntimeException.class).when(this.domainEventPublisherService).republishProductToAgp(PRODUCT_DOMAIN_EVENT_MODEL);

    productServiceWrapper.republishProductByProductCodesToAgp(STORE_ID, PRODUCT_CODES, true);

    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID,PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, PRODUCT, true);
    verify(domainEventPublisherService).toProductDomainEventModel(PRODUCT,false, StringUtils.EMPTY);
    verify(domainEventPublisherService).republishProductToAgp(PRODUCT_DOMAIN_EVENT_MODEL);
  }

  @Test
  public void republishProductByProductCodesToAgpTest_splitTrue_emptyProductCodes() throws Exception {
    List<String> productCodes = Collections.singletonList(StringUtils.EMPTY);
    productServiceWrapper.republishProductByProductCodesToAgp(STORE_ID, productCodes, true);
  }

  @Test
  public void republishProductByProductCodesToAgpTest_splitFalse() throws Exception {
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE))
        .thenReturn(PRODUCT);
    when(domainEventPublisherService.toProductDomainEventModel(PRODUCT,false, StringUtils.EMPTY))
        .thenReturn(PRODUCT_DOMAIN_EVENT_MODEL);
    when(domainEventPublisherService.republishProductToAgp(PRODUCT_DOMAIN_EVENT_MODEL))
        .thenReturn(PRODUCT_DOMAIN_EVENT_MODEL);

    productServiceWrapper.republishProductByProductCodesToAgp(STORE_ID, PRODUCT_CODES, false);

    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID,PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, PRODUCT, true);
    verify(domainEventPublisherService).toProductDomainEventModel(PRODUCT,false, StringUtils.EMPTY);
    verify(domainEventPublisherService).republishProductToAgp(PRODUCT_DOMAIN_EVENT_MODEL);
  }

  @Test
  public void republishProductByProductCodesToAgpTest_splitFalse_errorWhenFindByStoreIdAndProductCode() throws Exception {
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE))
        .thenThrow(new ApplicationRuntimeException());

    productServiceWrapper.republishProductByProductCodesToAgp(STORE_ID, PRODUCT_CODES, false);

    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID,PRODUCT_CODE);
  }

  @Test
  public void republishProductByProductCodesToAgpTest_splitFalse_errorWhenPublishProductToAgp() throws Exception {
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE))
        .thenReturn(PRODUCT);
    when(domainEventPublisherService.toProductDomainEventModel(PRODUCT,false, StringUtils.EMPTY))
        .thenReturn(PRODUCT_DOMAIN_EVENT_MODEL);
    doThrow(RuntimeException.class).when(this.domainEventPublisherService).republishProductToAgp(PRODUCT_DOMAIN_EVENT_MODEL);

    productServiceWrapper.republishProductByProductCodesToAgp(STORE_ID, PRODUCT_CODES, false);

    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID,PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, PRODUCT, true);
    verify(domainEventPublisherService).toProductDomainEventModel(PRODUCT,false, StringUtils.EMPTY);
    verify(domainEventPublisherService).republishProductToAgp(PRODUCT_DOMAIN_EVENT_MODEL);
  }

  @Test
  public void republishProductByProductCodesToAgpTest_splitFalse_emptyProductCodes() throws Exception {
    List<String> productCodes = Collections.singletonList(StringUtils.EMPTY);
    productServiceWrapper.republishProductByProductCodesToAgp(STORE_ID, productCodes, false);
  }

  @Test
  public void getItemListByProductCodeTest() throws Exception {
    when(this.productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE))
        .thenReturn(productData);
    when(this.productItemService.findListOfItemsByProduct(STORE_ID, productData, pageable))
        .thenReturn(new PageImpl<>(Arrays.asList(productItem), pageable, TOTAL_ELEMENTS));
    Page<SimpleItemDetailResponse> simpleItemDetailResponsePage =
        this.productServiceWrapper.getItemListByProductCode(STORE_ID, PRODUCT_CODE, PAGE, SIZE);
    Mockito.verify(this.productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productItemService).findListOfItemsByProduct(STORE_ID, productData, pageable);
    assertEquals(PRODUCT_CODE, simpleItemDetailResponsePage.getContent().get(0).getProductCode());
    assertEquals(SKU_CODE, simpleItemDetailResponsePage.getContent().get(0).getItemCode());
    assertEquals(PRODUCT_NAME+ ITEM_NAME, simpleItemDetailResponsePage.getContent().get(0).getItemName());
  }

  @Test
  public void updateProductCategoryByStoreIdAndProductCodeTest() throws Exception {
    CategorySummaryResponse categorySummaryResponse =
        new CategorySummaryResponse(CATEGORY_CODE_NEW, CATEGORY_NAME_NEW, CATEGORY_ID_1, CATEGORY_ID_NEW,false);
    ImmutablePair<Product, CategorySummaryResponse> pair = new ImmutablePair<>(productData, categorySummaryResponse);
    when(productService.updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE_NEW,
        false)).thenReturn(pair);
    when(categoryReferenceService.getSalesCategoryReferenceByMasterCategory(Mockito.any(),Mockito.any(),Mockito.anyBoolean())).thenReturn(new ProductSalesCategoryMapping());
    productServiceWrapper.updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE_NEW, true,
        false);
    verify(productService).updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE_NEW,
        false);
    Mockito.verify(categoryReferenceService).getSalesCategoryReferenceByMasterCategory(CATEGORY_ID_1, CATEGORY_ID_NEW, false);
    Mockito.verify(productService).evictProductCache(STORE_ID, productData);
    Mockito.verify(applicationCacheServiceBean).evictProductCategoriesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(domainEventPublisherService)
        .publishProductChangeCategory(Mockito.any(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(),
            Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any());
  }

  @Test
  public void updateProductCategoryByStoreIdAndProductCode1Test() throws Exception {
    CategorySummaryResponse categorySummaryResponse =
        new CategorySummaryResponse(CATEGORY_CODE_NEW, CATEGORY_NAME_NEW, CATEGORY_ID_1, CATEGORY_ID_NEW, false);
    ImmutablePair<Product, CategorySummaryResponse> pair = new ImmutablePair<>(productData, categorySummaryResponse);
    when(productService.updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE,
        CATEGORY_CODE_NEW,true)).thenReturn(pair);
    when(categoryReferenceService.getSalesCategoryReferenceByMasterCategory(Mockito.any(),Mockito.any(),Mockito.anyBoolean())).thenReturn(new ProductSalesCategoryMapping());
    productServiceWrapper.updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE_NEW, false,true);
    verify(productService).updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE_NEW,true);
    Mockito.verify(categoryReferenceService).getSalesCategoryReferenceByMasterCategory(CATEGORY_ID_1, CATEGORY_ID_NEW, false);
    Mockito.verify(productService).evictProductCache(STORE_ID, productData);
    Mockito.verify(applicationCacheServiceBean).evictProductCategoriesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(domainEventPublisherService).publishProductChangeCategory(productData, null, false, false, true, true, new HashSet<>());
  }

  @Test
  public void updateProductDimensionsTest() throws Exception {
    productItem.setUpcCode(UPC_CODE);
    productItem.setProductId(PRODUCT_ID);
    when(productItemService.findByStoreIdAndSkuCode(DEFAULT_STORE_ID, warehouseMasterSKUEvent.getItemCode()))
        .thenReturn(productItem);
    when(productService.getProductByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID)).thenReturn(productData);
    when(productService.updateProductDimensions(warehouseMasterSKUEvent, productItem.getProductId())).thenReturn(
        productData);
    doNothing().when(productService).evictProductCache(DEFAULT_STORE_ID, productData);
    doNothing().when(applicationCacheServiceBean)
        .evictProductItemsCacheByStoreIdAndProductId(DEFAULT_STORE_ID, productData.getId());
    boolean status = productServiceWrapper.updateProductDimensions(warehouseMasterSKUEvent);
    verify(productItemService).findByStoreIdAndSkuCode(DEFAULT_STORE_ID, warehouseMasterSKUEvent.getItemCode());
    verify(productService).updateProductDimensions(warehouseMasterSKUEvent, PRODUCT_ID);
    Mockito.verify(productService).evictProductCache(DEFAULT_STORE_ID, productData);
    Mockito.verify(applicationCacheServiceBean).evictProductItemsCacheByStoreIdAndProductId(DEFAULT_STORE_ID, productData.getId());
    Mockito.verify(productService).publishProduct(productData, false);
    assertTrue(status);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(productService, attributeService, predefinedAllowedAttributeValueService);
  }

  @Test
  public void getItemResponseByItemCodeTest() throws Exception {
    productData.setProductCategories(Arrays.asList(productCategoryData));
    when(this.productItemService.getProductItemByItemCode(STORE_ID, SKU_CODE)).thenReturn(productItem);
    when(this.productService.getProductByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID)).thenReturn(productData);
    ProductItemCompleteResponse productItemCompleteResponse =
        productServiceWrapper.getItemResponseByItemCode(STORE_ID, SKU_CODE);
    Mockito.verify(this.productItemService).getProductItemByItemCode(STORE_ID, SKU_CODE);
    Mockito.verify(this.productService).getProductByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productService).setCompleteProductDetailsCached(STORE_ID, productData, false);
    assertEquals(SKU_CODE, productItemCompleteResponse.getSkuCode());
    assertEquals(ORIGINAL_PRODUCT_NAME, productItemCompleteResponse.getProductResponse().getName());
    assertEquals(PRODUCT_CODE, productItemCompleteResponse.getProductResponse().getProductCode());
    assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString(),
        productItemCompleteResponse.getProductResponse().getProductAttributeResponses().get(0).getAttribute()
            .getAttributeType());
  }

  @Test
  public void activateProductTest() throws Exception {
    when(productService.updateProductActivated(STORE_ID, PRODUCT_CODE, true)).thenReturn(productData);
    productServiceWrapper.updateProductActivated(STORE_ID, PRODUCT_CODE, true);
    verify(productService).updateProductActivated(STORE_ID, PRODUCT_CODE, true);
    verify(productService).evictProductCache(STORE_ID, productData);
    verify(applicationCacheServiceBean).evictProductItemsCacheByStoreIdAndProductId(STORE_ID, productData.getId());
    verify(productService).publishProduct(productData, false);
  }

  @Test
  public void activateProduct() throws Exception {
    productServiceWrapper.activateProduct(STORE_ID, PRODUCT_ID);
    verify(productService).activateProduct(STORE_ID, PRODUCT_ID);
    verify(productService).getProductByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, PRODUCT, false);
    verify(productService).publishProduct(PRODUCT, false);
  }

  @Test
  public void deactivateProduct() throws Exception {
    productServiceWrapper.deactivateProduct(STORE_ID, PRODUCT_ID);
    verify(productService).deactivateProduct(STORE_ID, PRODUCT_ID);
    verify(productService).getProductByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, PRODUCT, false);
    verify(productService).publishProduct(PRODUCT, false);
  }

  @Test
  public void updateProductViewable() throws Exception {
    PRODUCT.setActivated(true);
    PRODUCT.setViewable(false);
    productServiceWrapper.updateProductViewable(STORE_ID, PRODUCT_CODE, true);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).evictProductCache(STORE_ID, productData);
    verify(applicationCacheServiceBean).evictProductItemsCacheByStoreIdAndProductId(STORE_ID,
      PRODUCT_ID);
    verify(productService).updateProductViewable(STORE_ID, PRODUCT_CODE, true);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, PRODUCT, false);
    verify(domainEventPublisherService).publishProductChangeCategory(productArgumentCaptor.capture(), any(),
        eq(false), eq(true), eq(true), eq(false), eq(new HashSet<>()));
    verify(domainEventPublisherService).publishEvent(PRODUCT, ProductMasterEvent.ACTIVATED);
    assertTrue(productArgumentCaptor.getValue().isViewable());
  }


  @Test
  public void updateProductImage() throws Exception {
    productData.setVersion(1L);
    when(productService.updateProductImage(PRODUCT)).thenReturn(new ProductPublishUpdateDTO(productData, true, new HashSet<>()));
    productServiceWrapper.updateProductImage(PRODUCT);
    verify(productService).updateProductImage(PRODUCT);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, PRODUCT, false);
    verify(domainEventPublisherService).publishProductChangeCategory(
        new ProductPublishUpdateDTO(PRODUCT, true, new HashSet<>()), null, false, false, false, false, false,
        new HashSet<>(), false, false, new HashSet<>());
    verify(productService).evictProductCache(productData.getStoreId(), productData);
    verify(applicationCacheServiceBean)
        .evictProductImagesCacheByStoreIdAndProductId(productData.getStoreId(), productData.getId());
    verify(productService).evictProductItemsAndProductItemImagesCache(productData.getStoreId(), productData);
  }

  @Test
  public void getProductDetailsWithoutItemsByProductCodeAndMarkForDeleteFalseTest() throws Exception {
    productServiceWrapper.getProductDetailsWithoutItemsByProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setProductCategoriesWithCategoriesCached(STORE_ID, productData, false);
    verify(productService).setProductAttributesWithValuesAndAttributeCached(STORE_ID, productData, false);
    verify(productService).setProductImagesCached(STORE_ID, productData, false);
  }

  @Test
  public void getCompleteProductDetailByProductIdAndMarkForDeleteFalseTest() throws Exception {
    productServiceWrapper.getCompleteProductDetailByProductIdAndMarkForDeleteFalse(STORE_ID, PRODUCT_ID);
    verify(productService).getProductByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, productData, false);
  }

  @Test
  public void getCompleteProductDetailByProductIdTest() throws Exception {
    productServiceWrapper.getCompleteProductDetailByProductId(STORE_ID, PRODUCT_ID);
    verify(productService).getProductByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, productData, true);
  }

  @Test
  public void updateProductScoreTest() throws Exception {
    when(productScoreService.findByMarkForDeleteFalseAndUpdatedFalse(eq(DEFAULT_PAGEABLE)))
        .thenReturn(new PageImpl<>(productScoreUpdateList, DEFAULT_PAGEABLE, 2));
    productServiceWrapper.updateProductScore(STORE_ID, DEFAULT_PAGEABLE, true, 2);
    Mockito.verify(productScoreService).findByMarkForDeleteFalseAndUpdatedFalse(eq(DEFAULT_PAGEABLE));
    Mockito.verify(this.domainEventPublisherService).publishProductScoreUpdate(productScoreUpdateList);
    Mockito.verify(this.productScoreService).updateProducts(productScoreUpdateList);
  }

  @Test
  public void updateProductScoreWithLessBatchSizeTest() throws Exception {
    when(productScoreService.findByMarkForDeleteFalseAndUpdatedFalse(eq(DEFAULT_PAGEABLE)))
        .thenReturn(new PageImpl<>(productScoreUpdateList, DEFAULT_PAGEABLE, 4));
    productServiceWrapper.updateProductScore(STORE_ID, DEFAULT_PAGEABLE, true, 4);
    Mockito.verify(productScoreService, times(2)).findByMarkForDeleteFalseAndUpdatedFalse(DEFAULT_PAGEABLE);
    Mockito.verify(this.domainEventPublisherService, times(2)).publishProductScoreUpdate(Mockito.anyList());
    Mockito.verify(this.productScoreService, times(2)).updateProducts(productScoreUpdateList);
  }

  @Test
  public void updateProductScoreExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(productScoreService)
        .findByMarkForDeleteFalseAndUpdatedFalse(eq(DEFAULT_PAGEABLE));
    try {
      productServiceWrapper.updateProductScore(STORE_ID, DEFAULT_PAGEABLE, true, 2);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      assertTrue(
          applicationRuntimeException.getErrorMessage().contains(ERROR_MESSAGE_FOR_UPDATE_SCORE));
    }
  }

  @Test
  public void updateProductScoreEmptyProductTest() throws Exception {
    when(productScoreService.findByMarkForDeleteFalseAndUpdatedFalse(DEFAULT_PAGEABLE))
        .thenReturn(new PageImpl<>(new ArrayList<>(), DEFAULT_PAGEABLE, 0));
    productServiceWrapper.updateProductScore(STORE_ID, DEFAULT_PAGEABLE, true, 2);
    Mockito.verify(productScoreService).findByMarkForDeleteFalseAndUpdatedFalse(DEFAULT_PAGEABLE);
  }

  @Test
  public void updateProductScoreSwitchFalseTest() throws Exception {
    productServiceWrapper.updateProductScore(STORE_ID, DEFAULT_PAGEABLE, false, 2);
  }

  @Test
  public void updateProductScoreWithCountEqualsBatchSizeTest() throws Exception {
    productScoreUpdateList.remove(1);
    when(productScoreService
        .findByMarkForDeleteFalseAndUpdatedFalse(eq(DEFAULT_PAGEABLE)))
        .thenReturn(new PageImpl<>(productScoreUpdateList,DEFAULT_PAGEABLE, 1));
    productServiceWrapper.updateProductScore(STORE_ID, DEFAULT_PAGEABLE, true, 1);
    Mockito.verify(productScoreService)
        .findByMarkForDeleteFalseAndUpdatedFalse(eq(DEFAULT_PAGEABLE));
    Mockito.verify(this.domainEventPublisherService).publishProductScoreUpdate(productScoreUpdateList);
    Mockito.verify(this.productScoreService).updateProducts(productScoreUpdateList);
  }

  @Test
  public void updateProductItemImagesByProductCode() throws Exception {
    ProductItemImageUpdateRequest productItemImageUpdateRequest = new ProductItemImageUpdateRequest();
    productItemImageUpdateRequest.setProductCode(PRODUCT_CODE);
    productItemImageUpdateRequest.setCopyToAllVariantImages(Arrays.asList(image));
    productItemImageUpdateRequest.setUpdateProductItemImages(Arrays.asList(productItemImageRequest));
    doNothing().when(productService).evictCompleteProductAndItemsCache(STORE_ID, productData);
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO();
    productPublishUpdateDTO.setProduct(productData);
    when(productService.updateProductItemImagesByProductCode(productItemImageUpdateRequest,
      STORE_ID)).thenReturn(
      Pair.of(productPublishUpdateDTO, Collections.singletonList(new LocationPathAndCommonImage())));
    GdnRestListResponse<LocationPathAndCommonImage> gdnRestListResponse =
        productServiceWrapper.updateProductItemImagesByProductCode(productItemImageUpdateRequest, STORE_ID, REQUEST_ID);
    verify(productService).updateProductItemImagesByProductCode(productItemImageUpdateRequest, STORE_ID);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, productData, false);
    verify(productService).evictCompleteProductAndItemsCache(STORE_ID, productData);
    verify(domainEventPublisherService).publishProductChangeCategory(productPublishUpdateDTO, null, false, false,
        true, false, false, new HashSet<>(), true, true, new HashSet<>());
    assertTrue(gdnRestListResponse.isSuccess());
  }

  @Test
  public void deleteOriginalImagesTest() throws Exception {
    productData.getProductImages().get(0).setOriginalImage(true);
    productData.getProductImages().get(0).setMarkForDelete(false);
    productData.setReviewPending(true);
    productData.getProductItems().get(0).getProductItemImages().get(0).setOriginalImage(true);
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(productData);
    boolean status = productServiceWrapper.deleteOriginalImagesByProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).update(productArgumentCaptor.capture());
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(eq(STORE_ID), any(Product.class), eq(false));
    verify(domainEventPublisherService)
        .publishProductChangeCategory(any(Product.class), eq(null), eq(false), eq(false), eq(false), eq(false), eq(new HashSet<>()));
    verify(productService).evictAllProductDetailCache(anyString(), productArgumentCaptor.capture());
    assertEquals(true, status);
    assertEquals(true, productArgumentCaptor.getValue().getProductImages().get(0).isMarkForDelete());
    assertEquals(false, productArgumentCaptor.getValue().isReviewPending());
  }

  @Test
  public void deleteOriginalImagesFalseTest() throws Exception {
    productData.getProductImages().get(0).setOriginalImage(false);
    productData.getProductImages().get(0).setMarkForDelete(false);
    productData.setReviewPending(true);
    productData.getProductItems().get(0).getProductItemImages().get(0).setOriginalImage(true);
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(productData);
    boolean status = productServiceWrapper.deleteOriginalImagesByProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).update(productArgumentCaptor.capture());
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(eq(STORE_ID), any(Product.class), eq(false));
    verify(productService).evictAllProductDetailCache(anyString(), productArgumentCaptor.capture());
    verify(domainEventPublisherService)
        .publishProductChangeCategory(any(Product.class), eq(null), eq(false), eq(false), eq(false), eq(false), eq(new HashSet<>()));
    assertEquals(true, status);
    assertEquals(false, productArgumentCaptor.getValue().getProductImages().get(0).isMarkForDelete());
    assertEquals(false, productArgumentCaptor.getValue().isReviewPending());
  }

  @Test
  public void deleteOriginalImagesExceptionTest() throws Exception {
    productData.getProductImages().get(0).setOriginalImage(false);
    productData.getProductImages().get(0).setMarkForDelete(false);
    productData.setReviewPending(true);
    productData.getProductItems().get(0).getProductItemImages().get(0).setOriginalImage(true);
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(productData);
    Mockito.doThrow(IOException.class).when(productService).update(any(Product.class));
    boolean status = productServiceWrapper.deleteOriginalImagesByProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).update(productArgumentCaptor.capture());
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(eq(STORE_ID), any(Product.class), eq(false));
    assertEquals(false, status);
  }

  @Test
  public void deleteOriginalImagesNullTest() throws Exception {
    productData.getProductImages().get(0).setOriginalImage(null);
    productData.getProductImages().get(0).setMarkForDelete(false);
    productData.setReviewPending(true);
    productData.getProductItems().get(0).getProductItemImages().get(0).setOriginalImage(null);
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(productData);
    boolean status = productServiceWrapper.deleteOriginalImagesByProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).update(productArgumentCaptor.capture());
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(eq(STORE_ID), any(Product.class), eq(false));
    verify(productService).evictAllProductDetailCache(anyString(), productArgumentCaptor.capture());
    verify(domainEventPublisherService)
        .publishProductChangeCategory(any(Product.class), eq(null), eq(false), eq(false), eq(false), eq(false), eq(new HashSet<>()));
    assertEquals(true, status);
    assertEquals(false, productArgumentCaptor.getValue().getProductImages().get(0).isMarkForDelete());
    assertEquals(false, productArgumentCaptor.getValue().isReviewPending());
  }

  @Test
  public void deleteOriginalImages_imageNullTest() throws Exception {
    productData.setProductImages(Collections.singletonList(null));
    productData.setReviewPending(true);
    productData.getProductItems().get(0).setProductItemImages(Collections.singletonList(null));
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(productData);
    boolean status = productServiceWrapper.deleteOriginalImagesByProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).update(productArgumentCaptor.capture());
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(eq(STORE_ID), any(Product.class), eq(false));
    verify(domainEventPublisherService)
        .publishProductChangeCategory(any(Product.class), eq(null), eq(false), eq(false), eq(false), eq(false), eq(new HashSet<>()));
    verify(this.productService).evictAllProductDetailCache(STORE_ID, productData);
    assertEquals(true, status);
  }

  @Test
  public void updateVatFlagBySkuCodesTest() {
    productItem.setVatApplicable(Boolean.FALSE);
    Mockito.when(
            this.productItemService.findItemsBySkuCodesAndMarkForDeleteFalse(STORE_ID,
                Collections.singletonList(SKU_CODE)))
        .thenReturn(Collections.singletonList(productItem));
    List<BatchVatUpdateResponse> response =
        productServiceWrapper.updateVatFlagBySkuCodes(STORE_ID, REQUEST_ID, USERNAME, batchVatUpdateRequest);
    Mockito.verify(domainEventPublisherService).publishVatApplicableUpdateEvent(SKU_CODE, Boolean.TRUE);
    Mockito.verify(this.productItemService)
        .findItemsBySkuCodesAndMarkForDeleteFalse(STORE_ID, Collections.singletonList(SKU_CODE));
    Mockito.verify(this.productItemService).saveProductItem(any(ProductItem.class));
    Mockito.verify(this.domainEventPublisherService)
        .publishVatApplicableExternalHistoryEvent(REQUEST_ID, STORE_ID, productItem.getId(), productItem.getSkuCode(),
            productItem.getGeneratedItemName(), USERNAME, String.valueOf(Boolean.FALSE), String.valueOf(Boolean.TRUE));
    Mockito.verify(this.applicationCacheServiceBean)
        .evictProductItemsCacheByStoreIdAndProductId(productItem.getStoreId(), productItem.getProductId());
    assertTrue(CollectionUtils.isEmpty(response));
  }

  @Test
  public void updateVatFlagBySkuCodes_storeIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> productServiceWrapper.updateVatFlagBySkuCodes(StringUtils.EMPTY, REQUEST_ID, USERNAME, batchVatUpdateRequest));
  }

  @Test
  public void updateVatFlagBySkuCodes_requestIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> productServiceWrapper.updateVatFlagBySkuCodes(STORE_ID, StringUtils.EMPTY, USERNAME, batchVatUpdateRequest));
  }

  @Test
  public void updateVatFlagBySkuCodes_merchantCodeTest() {
    batchVatUpdateRequest.setBusinessPartnerCode(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> productServiceWrapper.updateVatFlagBySkuCodes(STORE_ID, REQUEST_ID, USERNAME, batchVatUpdateRequest));
  }

  @Test
  public void updateVatFlagBySkuCodes_nullValueTest() {
    productItem.setVatApplicable(Boolean.FALSE);
    batchVatUpdateRequest.setItemCodeToVatMap(Collections.singletonMap(SKU_CODE, null));
    Mockito.when(this.productItemService.findItemsBySkuCodesAndMarkForDeleteFalse(STORE_ID, Collections.emptyList()))
        .thenReturn(Collections.emptyList());
    List<BatchVatUpdateResponse> response =
        productServiceWrapper.updateVatFlagBySkuCodes(STORE_ID, REQUEST_ID, USERNAME, batchVatUpdateRequest);
    Mockito.verify(this.productItemService).findItemsBySkuCodesAndMarkForDeleteFalse(eq(STORE_ID), Mockito.anyList());
    assertEquals(ErrorMessage.VAT_FLAG_TO_UPDATE_NULL.getMessage(), response.get(0).getErrorMessage());
  }


  @Test
  public void updateVatFlagBySkuCodes_noFlagChangeTest() {
    productItem.setVatApplicable(Boolean.TRUE);
    Mockito.when(
            this.productItemService.findItemsBySkuCodesAndMarkForDeleteFalse(STORE_ID,
                Collections.singletonList(SKU_CODE)))
        .thenReturn(Collections.singletonList(productItem));
    List<BatchVatUpdateResponse> response =
        productServiceWrapper.updateVatFlagBySkuCodes(STORE_ID, REQUEST_ID, USERNAME, batchVatUpdateRequest);
    Mockito.verify(this.productItemService)
        .findItemsBySkuCodesAndMarkForDeleteFalse(STORE_ID, Collections.singletonList(SKU_CODE));
  }

  @Test
  public void updateVatFlagBySkuCodes_updateToFalseTest() {
    productItem.setVatApplicable(Boolean.TRUE);
    batchVatUpdateRequest.setItemCodeToVatMap(Collections.singletonMap(SKU_CODE, Boolean.FALSE));
    Mockito.when(
            this.productItemService.findItemsBySkuCodesAndMarkForDeleteFalse(STORE_ID,
                Collections.singletonList(SKU_CODE)))
        .thenReturn(Arrays.asList(productItem, productItem1));
    List<BatchVatUpdateResponse> response =
        productServiceWrapper.updateVatFlagBySkuCodes(STORE_ID, REQUEST_ID, USERNAME, batchVatUpdateRequest);
    Mockito.verify(domainEventPublisherService).publishVatApplicableUpdateEvent(SKU_CODE, Boolean.FALSE);
    Mockito.verify(this.productItemService)
        .findItemsBySkuCodesAndMarkForDeleteFalse(STORE_ID, Collections.singletonList(SKU_CODE));
    Mockito.verify(this.productItemService).saveProductItem(any(ProductItem.class));
    Mockito.verify(this.domainEventPublisherService)
        .publishVatApplicableExternalHistoryEvent(REQUEST_ID, STORE_ID, productItem.getId(), productItem.getSkuCode(),
            productItem.getGeneratedItemName(), USERNAME, String.valueOf(Boolean.TRUE), String.valueOf(Boolean.FALSE));
    Mockito.verify(this.applicationCacheServiceBean)
        .evictProductItemsCacheByStoreIdAndProductId(productItem.getStoreId(), productItem.getProductId());
    assertTrue(CollectionUtils.isEmpty(response));
  }

  @Test
  public void getMasterProductDetailsByItemCodeTest() throws Exception {
    productItem.setProductId(PRODUCT_ID);
    when(productService.getProductByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID)).thenReturn(productData);
    doNothing().when(productService).setProductImagesCached(STORE_ID, productData, true);
    doNothing().when(productService).setProductCategoriesWithCategoriesCached(STORE_ID, productData,true);
    when(productItemService.getProductItemBySkuCode(STORE_ID, SKU_CODE)).thenReturn(productItem);

    productServiceWrapper.getMasterProductDetailsByItemCode(STORE_ID, SKU_CODE);

    verify(productService).getProductByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productService).setProductImagesCached(STORE_ID, productData, true);
    verify(productService).setProductCategoriesWithCategoriesCached(STORE_ID, productData,true);
    verify(productItemService).getProductItemBySkuCode(STORE_ID, SKU_CODE);
  }

  @Test
  public void getMasterProductDetailsByItemCodeProductNotFoundTest() throws Exception {
    productItem.setProductId(PRODUCT_ID);
    when(productItemService.getProductItemBySkuCode(STORE_ID, SKU_CODE)).thenReturn(productItem);
    when(productService.getProductByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productServiceWrapper.getMasterProductDetailsByItemCode(STORE_ID, SKU_CODE));
    }
    finally {
      verify(productItemService).getProductItemBySkuCode(STORE_ID, SKU_CODE);
      verify(productService).getProductByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    }
  }

  @Test
  public void getMasterProductDetailsByItemCodeProductItemNotFoundTest() throws Exception {
    when(productItemService.getProductItemBySkuCode(STORE_ID, SKU_CODE)).thenReturn(null);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productServiceWrapper.getMasterProductDetailsByItemCode(STORE_ID, SKU_CODE));
    }
    finally {
      verify(productItemService).getProductItemBySkuCode(STORE_ID, SKU_CODE);
    }
  }

  @Test
  public void updateImagesTest() throws Exception {
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    productImageEditRequest.setProductCode(PRODUCT_CODE);
    Map<String, Map<String, String>> errorMap = new HashMap<>();
    errorMap.put(productImageEditRequest.getImagePath(), new HashMap<>());
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(productData, false, new HashSet<>());
    ImmutablePair<ProductPublishUpdateDTO, Map<String, Map<String, String>>> pair = new ImmutablePair<>(productPublishUpdateDTO, errorMap);
    when(productService.updateImages(STORE_ID, true, Arrays.asList(productImageEditRequest), null,
      false)).thenReturn(pair);
    doNothing().when(productService).evictProductCache(STORE_ID, productData);
    doNothing().when(productService).evictProductItemsAndProductItemImagesCache(STORE_ID, productData);
    doNothing().when(applicationCacheServiceBean)
        .evictProductImagesCacheByStoreIdAndProductId(STORE_ID, productData.getId());
    Map<String, String> status = productServiceWrapper.updateImages(STORE_ID, productImageEditRequest);
    verify(productService).updateImages(STORE_ID, true, Arrays.asList(productImageEditRequest),
      null, false);
    verify(productService).evictProductCache(STORE_ID, productData);
    verify(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(STORE_ID, productData.getId());
    verify(productService).evictProductItemsAndProductItemImagesCache(STORE_ID, productData);
    verify(domainEventPublisherService).publishProductChangeCategory(productPublishUpdateDTO, null, false,
        false, true, false, false, new HashSet<>(), true, true, new HashSet<>());
  }

  @Test
  public void backfillCommonImageFlagInProductAndItemImagesTest() throws Exception {
    when(productMigrationService.findProductMigrationByProductCodeAndStatusAndMigrationType(PRODUCT_CODE,
        ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.COMMON_IMAGE_MIGRATION.name())).thenReturn(commonImageMigration);
    when(productMigrationService.saveProductMigration(commonImageMigrationArgumentCaptor.capture())).thenReturn(
        commonImageMigration);
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(productData);
    doNothing().when(productService).setCompleteProductDetailsCached(STORE_ID, productData, false);
    doNothing().when(productService).update(productArgumentCaptor.capture());
    doNothing().when(productService).evictAllProductDetailCacheByProductCode(STORE_ID, PRODUCT_CODE);
    when(domainEventPublisherService.publishProductForMasterDataMigration(productArgumentCaptor.capture(),
      eq(StringUtils.EMPTY))).thenReturn(new ProductDomainEventModel());

    productServiceWrapper.backfillCommonImageFlagInProductAndItemImages(STORE_ID, PRODUCT_CODE,
      StringUtils.EMPTY);

    verify(productMigrationService).findProductMigrationByProductCodeAndStatusAndMigrationType(PRODUCT_CODE,
        ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.COMMON_IMAGE_MIGRATION.name());
    verify(productMigrationService, times(2)).saveProductMigration(any(ProductMigration.class));
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, productData, false);
    verify(productService).update(productArgumentCaptor.getValue());
    verify(productService).evictAllProductDetailCacheByProductCode(STORE_ID, PRODUCT_CODE);
    verify(domainEventPublisherService).publishProductForMasterDataMigration(productArgumentCaptor.getValue(),
      StringUtils.EMPTY);

    assertEquals(ProductMigrationStatus.SUCCESS.name(),
        commonImageMigrationArgumentCaptor.getAllValues().get(1).getStatus());
  }

  @Test
  public void backfillCommonImageFlagInProductAndItemImagesExceptionTest() throws Exception {
    when(productMigrationService.findProductMigrationByProductCodeAndStatusAndMigrationType(PRODUCT_CODE,
        ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.COMMON_IMAGE_MIGRATION.name())).thenReturn(commonImageMigration);
    when(productMigrationService.saveProductMigration(commonImageMigrationArgumentCaptor.capture())).thenReturn(
        commonImageMigration);
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenThrow(
        ApplicationRuntimeException.class);

    productServiceWrapper.backfillCommonImageFlagInProductAndItemImages(STORE_ID, PRODUCT_CODE,
      StringUtils.EMPTY);

    verify(productMigrationService).findProductMigrationByProductCodeAndStatusAndMigrationType(PRODUCT_CODE,
        ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.COMMON_IMAGE_MIGRATION.name());
    verify(productMigrationService, times(2)).saveProductMigration(any(ProductMigration.class));
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);

    assertEquals(ProductMigrationStatus.FAILED.name(),
        commonImageMigrationArgumentCaptor.getAllValues().get(1).getStatus());
  }

  @Test
  public void updateCommonImagesTest() throws Exception {
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    productImageEditRequest.setProductCode(PRODUCT_CODE);
    Map<String, Map<String, String>> errorMap = new HashMap<>();
    errorMap.put(productImageEditRequest.getImagePath(), new HashMap<>());
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(productData, false, new HashSet<>());
    ImmutablePair<ProductPublishUpdateDTO, Map<String, Map<String, String>>> pair = new ImmutablePair<>(productPublishUpdateDTO, errorMap);
    when(productService.updateImages(STORE_ID, false, Arrays.asList(productImageEditRequest), null,
      false)).thenReturn(pair);
    when(productService.getProductByStoreIdAndProductCodeCached(anyString(), anyString())).thenReturn(productData);
    doNothing().when(productService).evictProductCache(STORE_ID, productData);
    doNothing().when(productService).evictProductItemsAndProductItemImagesCache(STORE_ID, productData);
    doNothing().when(applicationCacheServiceBean)
        .evictProductImagesCacheByStoreIdAndProductId(STORE_ID, productData.getId());
    Map<String, Map<String, String>> status =
        productServiceWrapper.updateCommonImages(STORE_ID, Arrays.asList(productImageEditRequest));
    verify(productService).updateImages(STORE_ID, false, Arrays.asList(productImageEditRequest),
      null, false);
    verify(productService).evictProductCache(STORE_ID, productData);
    verify(productService).getProductByStoreIdAndProductCodeCached(anyString(), anyString());
    verify(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(STORE_ID, productData.getId());
    verify(productService).setCompleteProductDetailsCached(anyString(), any(), anyBoolean());
    verify(productService).evictProductItemsAndProductItemImagesCache(STORE_ID, productData);
    verify(domainEventPublisherService).publishProductChangeCategory(productPublishUpdateDTO, null, false, false, true,
        false, false, new HashSet<>(), true, true, new HashSet<>());
  }

  @Test
  public void migrateFinalImageFromGfsToGcsTest() throws Exception {
    List<String> productCodeList = new ArrayList<>();
    productCodeList.add(PRODUCT_CODE);
    Set<String> locationPathSet = new HashSet<>();
    locationPathSet.add(LOCATION_PATH);
    Mockito.when(imageService.findProductAndItemLocationPathsByProductCodeListAndStoreId(productCodeList, STORE_ID)).thenReturn(locationPathSet);
    productServiceWrapper.migrateFinalImageFromGfsToGcs(productCodeList, STORE_ID);
    Mockito.verify(imageService).findProductAndItemLocationPathsByProductCodeListAndStoreId(productCodeList, STORE_ID);
    Mockito.verify(fileStorageService).migrateFinalImageFromGfsToGcs(locationPathSet);
  }

  @Test
  public void migrateFinalImageFromGfsToGcsLocationPathSetEmptyTest() throws Exception {
    List<String> productCodeList = new ArrayList<>();
    productCodeList.add(PRODUCT_CODE);
    Set<String> locationPathSet = new HashSet<>();
    Mockito.when(imageService.findProductAndItemLocationPathsByProductCodeListAndStoreId(productCodeList, STORE_ID))
        .thenReturn(locationPathSet);
    productServiceWrapper.migrateFinalImageFromGfsToGcs(productCodeList, STORE_ID);
    Mockito.verify(imageService).findProductAndItemLocationPathsByProductCodeListAndStoreId(productCodeList, STORE_ID);
  }

  @Test
  public void findProductImagesByProductCodeTest(){
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(productData);
    doNothing().when(productService).setProductImagesCached(STORE_ID, productData, true);
    List<ImageResponse> productImagesByProductCode =
      productServiceWrapper.findProductImagesByProductCode(STORE_ID, PRODUCT_CODE, false);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setProductImagesCached(STORE_ID, productData, false);
  }

  @Test
  public void updateProductContentExceptionTest() throws Exception {
    Product savedProduct = new Product();
    savedProduct.setVersion(2L);
    productData.setVersion(1L);
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(productData.getStoreId(), productData.getProductCode()))
        .thenReturn(savedProduct);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productServiceWrapper.updateProductContent(
          productData, false, false, true));
    } finally {
      Mockito.verify(productService)
          .getProductByStoreIdAndProductCodeCached(productData.getStoreId(), productData.getProductCode());
    }
  }

  @Test
  public void updateProductContentTest() throws Exception {
    productData.setVersion(1L);
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(productData.getStoreId(), productData.getProductCode()))
        .thenReturn(productData);
    Mockito.when(productService.saveAndFlush(productData)).thenReturn(productData);
    productServiceWrapper.updateProductContent(productData, true, false, false);
    Mockito.verify(productService)
        .getProductByStoreIdAndProductCodeCached(productData.getStoreId(), productData.getProductCode());
    Mockito.verify(productService).setCompleteProductDetailsCached(productData.getStoreId(), productData, true);
    Mockito.verify(productService).regenerateProductCategories(productData, productData);
    Mockito.verify(productService).regenerateProductAttributeContent(productData, productData, false, false);
    Mockito.verify(productService).regenerateProductItemContent(productData, productData);
    Mockito.verify(productService).regenerateProductImageContent(productData, productData);
    Mockito.verify(productService).saveAndFlush(productData);
    Mockito.verify(productService).evictAllProductDetailCache(productData.getStoreId(), productData);
    Mockito.verify(domainEventPublisherService)
        .publishProductChangeCategory(new ProductPublishUpdateDTO(productData, false, new HashSet<>()), null, false, false,
            false, false, false, new HashSet<>(), false, false, new HashSet<>());
  }

  @Test
  public void updateProductContentTestWithDescriptionUpdate() throws Exception {
    Product updatedProduct = new Product();
    BeanUtils.copyProperties(productData, updatedProduct);
    updatedProduct.setDescription(new byte[] {1, 2});
    updatedProduct.setVersion(1L);
    productData.setVersion(1L);
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(productData.getStoreId(), productData.getProductCode()))
        .thenReturn(updatedProduct);
    Mockito.when(productService.saveAndFlush(productData)).thenReturn(productData);
    productServiceWrapper.updateProductContent(productData, true, false, false);
    Mockito.verify(productService)
        .getProductByStoreIdAndProductCodeCached(productData.getStoreId(), productData.getProductCode());
    Mockito.verify(productService).setCompleteProductDetailsCached(productData.getStoreId(), updatedProduct, true);
    Mockito.verify(productService).regenerateProductCategories(productData, productData);
    Mockito.verify(productService).regenerateProductAttributeContent(productData, productData, false, false);
    Mockito.verify(productService).regenerateProductItemContent(productData, productData);
    Mockito.verify(productService).regenerateProductImageContent(productData, productData);
    Mockito.verify(productService).saveAndFlush(productData);
    Mockito.verify(productService).evictAllProductDetailCache(productData.getStoreId(), productData);
    Mockito.verify(domainEventPublisherService)
        .publishProductChangeCategory(new ProductPublishUpdateDTO(productData, true, new HashSet<>()), null, false, false,
            false, false, false, new HashSet<>(), false, false, Set.of(UpdatedFields.DESCRIPTION_UPDATE.name()));
    Mockito.verify(productAttributeExtractionService).addProductsToProductAttributeExtraction(productData.getProductCode(),
        productData.getProductCategories().get(0).getCategory());
  }

  @Test
  public void updateProductContentDataChangedTest() throws Exception {
    productData.setVersion(1L);
    Product savedProduct = new Product();
    savedProduct.setVersion(1L);
    savedProduct.setStoreId(STORE_ID);
    savedProduct.setProductCode(PRODUCT_CODE);
    savedProduct.setProductCategories(Collections.singletonList(productCategoryData));
    savedProduct.setName(PRODUCT_NAME);
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(productData.getStoreId(), productData.getProductCode()))
        .thenReturn(savedProduct);
    Mockito.when(productService.saveAndFlush(productData)).thenReturn(productData);
    productServiceWrapper.updateProductContent(productData, true, false, false);
    Mockito.verify(productService)
        .getProductByStoreIdAndProductCodeCached(productData.getStoreId(), productData.getProductCode());
    Mockito.verify(productService).setCompleteProductDetailsCached(productData.getStoreId(), productData, true);
    Mockito.verify(productService).regenerateProductCategories(productData, productData);
    Mockito.verify(productService).regenerateProductAttributeContent(productData, productData, false, false);
    Mockito.verify(productService).regenerateProductItemContent(productData, productData);
    Mockito.verify(productService).regenerateProductImageContent(productData, productData);
    Mockito.verify(productService).saveAndFlush(productData);
    Mockito.verify(productService).evictAllProductDetailCache(productData.getStoreId(), productData);
    Mockito.verify(domainEventPublisherService)
        .publishProductChangeCategory(new ProductPublishUpdateDTO(productData, true, new HashSet<>()), null, true, false,
            false, false, false, new HashSet<>(), false, false,
            Set.of(UpdatedFields.CATEGORY_UPDATE.name(), UpdatedFields.NAME_UPDATE.name(), UpdatedFields.BRAND_UPDATE.name()));
    Mockito.verify(productAttributeExtractionService).addProductsToProductAttributeExtraction(productData.getProductCode(),
        productData.getProductCategories().get(0).getCategory());
  }

  @Test
  public void updateProductContentCategoryChangeDTONotNullTest() throws Exception {
    productData.setVersion(1L);
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(productData.getStoreId(), productData.getProductCode()))
        .thenReturn(productData);
    Mockito.when(productService.saveAndFlush(productData)).thenReturn(productData);
    Mockito.when(productService.regenerateProductCategories(productData, productData)).thenReturn(new CategoryChangeDTO());
    productData.setDistributionInfo(PRODUCT_ID);
    productServiceWrapper.updateProductContent(productData, true, false, true);
    Mockito.verify(productService)
        .getProductByStoreIdAndProductCodeCached(productData.getStoreId(), productData.getProductCode());
    Mockito.verify(productService).setCompleteProductDetailsCached(productData.getStoreId(), productData, true);
    Mockito.verify(productService).regenerateProductCategories(productData, productData);
    Mockito.verify(productService).regenerateProductAttributeContent(productData, productData, true, false);
    Mockito.verify(productService).regenerateProductItemContent(productData, productData);
    Mockito.verify(productService).regenerateProductImageContent(productData, productData);
    Mockito.verify(productService).saveAndFlush(productData);
    Mockito.verify(productService).evictAllProductDetailCache(productData.getStoreId(), productData);
    Mockito.verify(domainEventPublisherService)
        .publishProductChangeCategory(new ProductPublishUpdateDTO(productData, false, new HashSet<>()), null, false, false,
            false, false, false, new HashSet<>(), false, false, new HashSet<>());
  }

  @Test
  public void updateProductContentNewCategoryIdNotNullTest() throws Exception {
    productData.setVersion(1L);
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(productData.getStoreId(), productData.getProductCode()))
        .thenReturn(productData);
    Mockito.when(productService.saveAndFlush(productData)).thenReturn(productData);
    CategoryChangeDTO categoryChangeDTO = new CategoryChangeDTO();
    categoryChangeDTO.setNewCategoryId(CATEGORY_ID_2);
    Mockito.when(productService.regenerateProductCategories(productData, productData)).thenReturn(categoryChangeDTO);
    productServiceWrapper.updateProductContent(productData, true, false, true);
    Mockito.verify(productService)
        .getProductByStoreIdAndProductCodeCached(productData.getStoreId(), productData.getProductCode());
    Mockito.verify(productService).setCompleteProductDetailsCached(productData.getStoreId(), productData, true);
    Mockito.verify(productService).regenerateProductCategories(productData, productData);
    Mockito.verify(productService).regenerateProductAttributeContent(productData, productData, true, false);
    Mockito.verify(productService).regenerateProductItemContent(productData, productData);
    Mockito.verify(productService).regenerateProductImageContent(productData, productData);
    Mockito.verify(productService).saveAndFlush(productData);
    Mockito.verify(productService).evictAllProductDetailCache(productData.getStoreId(), productData);
    Mockito.verify(domainEventPublisherService)
        .publishProductChangeCategory(new ProductPublishUpdateDTO(productData, false, new HashSet<>()), null, false, false,
            false, false, false, new HashSet<>(), false, false, new HashSet<>());
  }

  @Test
  public void updateProductContentOldCategoryIdNotNullTest() throws Exception {
    productData.setVersion(1L);
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(productData.getStoreId(), productData.getProductCode()))
        .thenReturn(productData);
    Mockito.when(productService.saveAndFlush(productData)).thenReturn(productData);
    CategoryChangeDTO categoryChangeDTO = new CategoryChangeDTO();
    categoryChangeDTO.setNewCategoryId(CATEGORY_ID_2);
    categoryChangeDTO.setOldCategoryId(CATEGORY_ID_2);
    Mockito.when(productService.regenerateProductCategories(productData, productData)).thenReturn(categoryChangeDTO);
    productServiceWrapper.updateProductContent(productData, false, false, true);
    Mockito.verify(productService)
        .getProductByStoreIdAndProductCodeCached(productData.getStoreId(), productData.getProductCode());
    Mockito.verify(productService).setCompleteProductDetailsCached(productData.getStoreId(), productData, true);
    Mockito.verify(productService).regenerateProductCategories(productData, productData);
    Mockito.verify(productService).regenerateProductAttributeContent(productData, productData, true, true);
    Mockito.verify(productService).regenerateProductItemContent(productData, productData);
    Mockito.verify(productService).regenerateProductImageContent(productData, productData);
    Mockito.verify(productService).saveAndFlush(productData);
    Mockito.verify(productService).evictAllProductDetailCache(productData.getStoreId(), productData);
    Mockito.verify(categoryReferenceService)
        .getSalesCategoryReferenceByMasterCategory(categoryChangeDTO.getOldCategoryId(),
            categoryChangeDTO.getNewCategoryId(), false);
  }

  @Test
  public void updateAndMarkForNeedRevisionTest() throws Exception {
    Mockito.when(productService.updateAndMarkForNeedRevision(productData)).thenReturn(
      UpdateNeedRevisionDTO.builder().product(productData)
        .productSalesCategoryMapping(new ProductSalesCategoryMapping())
        .isBrandChanged(Boolean.TRUE).build());
    Mockito.doNothing().when(productService).evictCacheOnMarkForNeedRevision(productData, Boolean.TRUE,
      new ProductSalesCategoryMapping());
    productData.setDistributionInfo(PRODUCT_CODE);
    this.productServiceWrapper.updateAndMarkForNeedRevision(productData);
    verify(productService).updateAndMarkForNeedRevision(productData);
    verify(productService).evictCacheOnMarkForNeedRevision(productData, Boolean.TRUE,
      new ProductSalesCategoryMapping());
  }

  @Test
  public void getProductAndAttributeDetailsByProductCodeTest() throws Exception {
    Mockito.when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE)).thenReturn(
        productData);
    this.productServiceWrapper.getProductAndAttributeDetailsByProductCode(STORE_ID, PRODUCT_CODE, false);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setProductAttributesWithValuesAndAttributeCached(STORE_ID, productData, false);
  }

  @Test
  public void getProductAndAttributeDetailsInAllProuductTrueTest() throws Exception {
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(productData);
    this.productServiceWrapper.getProductAndAttributeDetailsByProductCode(STORE_ID, PRODUCT_CODE, true);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setProductAttributesWithValuesAndAttributeCached(STORE_ID, productData, false);
  }

  @Test
  public void migrateImagesFromGfsToGcsForProducts() throws Exception {
    ProductMigration productMigration = new ProductMigration();
    productMigration.setProductCode(PRODUCT_CODE);
    productMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
    productMigration.setMigrationType(ProductMigrationType.GFS_TO_GCS_SOURCE_IMAGE_MIGRATION.name());
    Product product = getProductForImageMigration();

    Mockito.when(productMigrationService.findProductMigrationByProductCodeAndStatusAndMigrationType(PRODUCT_CODE,
            ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.GFS_TO_GCS_SOURCE_IMAGE_MIGRATION.name()))
        .thenReturn(productMigration);
    Mockito.when(productMigrationService.saveProductMigration(commonImageMigrationArgumentCaptor.capture()))
        .thenReturn(productMigration);
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    Mockito.doNothing().when(productService).setProductAndItemImagesCached(STORE_ID, product, false);
    Mockito.when(fileStorageService.isAlreadyGcsImage(IMAGE_PATH_1)).thenReturn(true);
    Mockito.when(fileStorageService.isAlreadyGcsImage(IMAGE_PATH_2)).thenReturn(false);
    Mockito.when(fileStorageService.getGcsPathWithPrefix(IMAGE_PATH_2)).thenReturn(IMAGE_PATH_2);
    Mockito.doNothing().when(fileStorageService)
        .migrateImagesFromGfsToGcs(PRODUCT_CODE, ImmutableSet.of(Pair.of(IMAGE_PATH_2, IMAGE_PATH_2)));
    Mockito.when(productService.saveAndFlush(product)).thenReturn(product);
    Mockito.doNothing().when(productService).evictProductAndItemImageCache(product);
    Mockito.when(domainEventPublisherService.publishProductChangeCategory(product, null, false, true, true, false, new HashSet<>()))
        .thenReturn(new ProductDomainEventModel());
    Mockito.doNothing().when(domainEventPublisherService)
        .publishImagePathUpdateEvent(STORE_ID, PRODUCT_CODE, ImmutableSet.of(Pair.of(IMAGE_PATH_2, IMAGE_PATH_2)));

    productServiceWrapper.migrateImagesFromGfsToGcsForProducts(STORE_ID, PRODUCT_CODE);

    Mockito.verify(productMigrationService).findProductMigrationByProductCodeAndStatusAndMigrationType(PRODUCT_CODE,
        ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.GFS_TO_GCS_SOURCE_IMAGE_MIGRATION.name());
    Mockito.verify(productMigrationService, times(2)).saveProductMigration(commonImageMigrationArgumentCaptor.getValue());
    Mockito.verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).setProductAndItemImagesCached(STORE_ID, product, false);
    Mockito.verify(fileStorageService, times(2)).isAlreadyGcsImage(IMAGE_PATH_1);
    Mockito.verify(fileStorageService, times(2)).isAlreadyGcsImage(IMAGE_PATH_2);
    Mockito.verify(fileStorageService, times(2)).getGcsPathWithPrefix(IMAGE_PATH_2);
    Mockito.verify(fileStorageService)
        .migrateImagesFromGfsToGcs(PRODUCT_CODE, ImmutableSet.of(Pair.of(IMAGE_PATH_2, IMAGE_PATH_2)));
    Mockito.verify(productService).saveAndFlush(product);
    Mockito.verify(productService).evictProductAndItemImageCache(product);
    Mockito.verify(domainEventPublisherService).publishProductChangeCategory(product, null, false, true, true, false, new HashSet<>());
    Mockito.verify(domainEventPublisherService)
        .publishImagePathUpdateEvent(STORE_ID, PRODUCT_CODE, ImmutableSet.of(Pair.of(IMAGE_PATH_2, IMAGE_PATH_2)));
  }

  @Test
  public void migrateImagesFromGfsToGcsForProductsNoImagesToBeMigrated() throws Exception {
    ProductMigration productMigration = new ProductMigration();
    productMigration.setProductCode(PRODUCT_CODE);
    productMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
    productMigration.setMigrationType(ProductMigrationType.GFS_TO_GCS_SOURCE_IMAGE_MIGRATION.name());
    Product product = getProductForImageMigration();
    product.getProductImages().remove(3);
    product.getProductItems().get(0).getProductItemImages().remove(3);

    Mockito.when(productMigrationService.findProductMigrationByProductCodeAndStatusAndMigrationType(PRODUCT_CODE,
            ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.GFS_TO_GCS_SOURCE_IMAGE_MIGRATION.name()))
        .thenReturn(productMigration);
    Mockito.when(productMigrationService.saveProductMigration(commonImageMigrationArgumentCaptor.capture()))
        .thenReturn(productMigration);
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    Mockito.doNothing().when(productService).setProductAndItemImagesCached(STORE_ID, product, false);
    Mockito.when(fileStorageService.isAlreadyGcsImage(IMAGE_PATH_1)).thenReturn(true);

    productServiceWrapper.migrateImagesFromGfsToGcsForProducts(STORE_ID, PRODUCT_CODE);

    Mockito.verify(productMigrationService).findProductMigrationByProductCodeAndStatusAndMigrationType(PRODUCT_CODE,
        ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.GFS_TO_GCS_SOURCE_IMAGE_MIGRATION.name());
    Mockito.verify(productMigrationService, times(2))
        .saveProductMigration(commonImageMigrationArgumentCaptor.getValue());
    Mockito.verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).setProductAndItemImagesCached(STORE_ID, product, false);
    Mockito.verify(fileStorageService, times(2)).isAlreadyGcsImage(IMAGE_PATH_1);
  }

  @Test
  public void migrateImagesFromGfsToGcsForProductsException() throws Exception {
    ProductMigration productMigration = new ProductMigration();
    productMigration.setProductCode(PRODUCT_CODE);
    productMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
    productMigration.setMigrationType(ProductMigrationType.GFS_TO_GCS_SOURCE_IMAGE_MIGRATION.name());
    Product product = getProductForImageMigration();
    product.getProductImages().remove(3);
    product.getProductItems().get(0).getProductItemImages().remove(3);

    Mockito.when(productMigrationService.findProductMigrationByProductCodeAndStatusAndMigrationType(PRODUCT_CODE,
            ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.GFS_TO_GCS_SOURCE_IMAGE_MIGRATION.name()))
        .thenReturn(productMigration);
    Mockito.when(productMigrationService.saveProductMigration(commonImageMigrationArgumentCaptor.capture()))
        .thenReturn(productMigration);
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(null);

    productServiceWrapper.migrateImagesFromGfsToGcsForProducts(STORE_ID, PRODUCT_CODE);

    Mockito.verify(productMigrationService).findProductMigrationByProductCodeAndStatusAndMigrationType(PRODUCT_CODE,
        ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.GFS_TO_GCS_SOURCE_IMAGE_MIGRATION.name());
    Mockito.verify(productMigrationService, times(2))
        .saveProductMigration(commonImageMigrationArgumentCaptor.getValue());
    Mockito.verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
  }

  private Product getProductForImageMigration() {
    ProductImage productImage1 = new ProductImage();
    productImage1.setMarkForDelete(true);

    ProductImage productImage2 = new ProductImage();
    productImage2.setActive(true);

    ProductImage productImage3 = new ProductImage();
    productImage3.setLocationPath(IMAGE_PATH_1);

    ProductImage productImage4 = new ProductImage();
    productImage4.setLocationPath(IMAGE_PATH_2);

    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setMarkForDelete(true);

    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setActive(true);

    ProductItemImage productItemImage3 = new ProductItemImage();
    productItemImage3.setLocationPath(IMAGE_PATH_1);

    ProductItemImage productItemImage4 = new ProductItemImage();
    productItemImage4.setLocationPath(IMAGE_PATH_2);

    ProductItem productItem = new ProductItem();
    productItem.setProductItemImages(new ArrayList<>(Arrays.asList(productItemImage1, productItemImage2, productItemImage3, productItemImage4)));
    Product product = new Product();
    product.setStoreId(STORE_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setProductImages(new ArrayList<>(Arrays.asList(productImage1, productImage2, productImage3, productImage4)));
    product.setProductItems(Arrays.asList(productItem));
    return product;
  }

  @Test
  public void autoFillAttributesTest() throws Exception {
    Product product = new Product();
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    Attribute attribute = new Attribute();
    category.setExtractionType(ExtractionType.INLINE_TEXT);
    productCategory.setCategory(category);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setName(ATTRIBUTE_NAME);
    productAttribute.setAttribute(attribute);
    productAttribute.setProductAttributeValues(Arrays.asList(productAttributeValue));
    product.setProductAttributes(Arrays.asList(productAttribute));
    product.setProductCategories(Arrays.asList(productCategory));
    product.setDescription("".getBytes(StandardCharsets.UTF_8));

    HashMap<String, Object> attributeValueAndNameMap = new HashMap<>();
    attributeValueAndNameMap.put(ATTRIBUTE_NAME, ATTRIBUTE_VALUE);
    attributeValueAndNameMap.put(BRAND_ATTRIBUTE_CODE, new HashMap<>());
    MatrixAttributeExtractionResponse value = new MatrixAttributeExtractionResponse();
    value.setExtractedAttributes(attributeValueAndNameMap);
    SingleBaseResponse<MatrixAttributeExtractionResponse> matrixAttributeExtractionResponse =
        new SingleBaseResponse<>(true, null, null, value);


    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    doNothing().when(productService).setCompleteProductDetailsCached(product.getStoreId(), product, false);
    when(matrixOutbound.extractProductAttributesByTextDetails(
        any(MatrixAttributeExtractionRequest.class))).thenReturn(matrixAttributeExtractionResponse);
    when(productService.saveAndFlush(product)).thenReturn(product);
    when(domainEventPublisherService.publishProduct(product)).thenReturn(new ProductDomainEventModel());
    when(categoryService.getCategoryAttributes(anyString(), anyString())).thenReturn(new ArrayList<>());
    doNothing().when(productService).evictAllProductDetailCache(product.getStoreId(), product);

    productServiceWrapper.autoFillAttributes(STORE_ID, PRODUCT_CODE);

    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(product.getStoreId(), product, false);
    verify(matrixOutbound).extractProductAttributesByTextDetails(
        any(MatrixAttributeExtractionRequest.class));
    verify(productService).saveAndFlush(product);
    verify(domainEventPublisherService).publishProduct(product);
    verify(categoryService).getCategoryAttributes(any(), any());
    verify(productService).evictAllProductDetailCache(product.getStoreId(), product);
  }

  @Test
  public void autoFillAttributesAttributeMappedToCategoryTest() throws Exception {
    Product product = new Product();
    product.setStoreId(STORE_ID);
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    Attribute attribute = new Attribute();
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(attribute);
    category.setExtractionType(ExtractionType.INLINE_TEXT);
    category.setCategoryAttributes(Arrays.asList(categoryAttribute));
    productCategory.setCategory(category);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setName(ATTRIBUTE_NAME);
    product.setProductCategories(Arrays.asList(productCategory));
    product.setDescription("".getBytes(StandardCharsets.UTF_8));


    HashMap<String, Object> attributeValueAndNameMap = new HashMap<>();
    attributeValueAndNameMap.put(ATTRIBUTE_NAME, ATTRIBUTE_VALUE);
    attributeValueAndNameMap.put(BRAND_ATTRIBUTE_CODE, new HashMap<>());
    MatrixAttributeExtractionResponse value = new MatrixAttributeExtractionResponse();
    value.setExtractedAttributes(attributeValueAndNameMap);
    SingleBaseResponse<MatrixAttributeExtractionResponse> matrixAttributeExtractionResponse =
        new SingleBaseResponse<>(true, null, null, value);


    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    doNothing().when(productService).setCompleteProductDetailsCached(product.getStoreId(), product, false);
    when(matrixOutbound.extractProductAttributesByTextDetails(
        any(MatrixAttributeExtractionRequest.class))).thenReturn(matrixAttributeExtractionResponse);
    when(productService.saveAndFlush(product)).thenReturn(product);
    when(domainEventPublisherService.publishProduct(product)).thenReturn(new ProductDomainEventModel());
    doNothing().when(productService).evictAllProductDetailCache(product.getStoreId(), product);
    when(categoryService.getCategoryAttributes(any(), any())).thenReturn(Arrays.asList(categoryAttribute));


    productServiceWrapper.autoFillAttributes(STORE_ID, PRODUCT_CODE);

    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(product.getStoreId(), product, false);
    verify(matrixOutbound).extractProductAttributesByTextDetails(
        any(MatrixAttributeExtractionRequest.class));
    verify(productService).saveAndFlush(product);
    verify(domainEventPublisherService).publishProduct(product);
    verify(productService).evictAllProductDetailCache(product.getStoreId(), product);
    verify(categoryService).getCategoryAttributes(any(), any());
  }

  @Test
  public void autoFillAttributesAttributeNoMappingTest() throws Exception {
    Product product = new Product();
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    Attribute attribute = new Attribute();
    category.setExtractionType(ExtractionType.INLINE_TEXT);
    productCategory.setCategory(category);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setName(ATTRIBUTE_NAME);
    product.setProductCategories(Arrays.asList(productCategory));
    product.setDescription("".getBytes(StandardCharsets.UTF_8));


    HashMap<String, Object> attributeValueAndNameMap = new HashMap<>();
    attributeValueAndNameMap.put(ATTRIBUTE_NAME, ATTRIBUTE_VALUE);
    attributeValueAndNameMap.put(BRAND_ATTRIBUTE_CODE, new HashMap<>());
    MatrixAttributeExtractionResponse value = new MatrixAttributeExtractionResponse();
    value.setExtractedAttributes(attributeValueAndNameMap);
    SingleBaseResponse<MatrixAttributeExtractionResponse> matrixAttributeExtractionResponse =
        new SingleBaseResponse<>(true, null, null, value);


    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    doNothing().when(productService).setCompleteProductDetailsCached(product.getStoreId(), product, false);
    when(categoryService.getCategoryAttributes(anyString(), anyString())).thenReturn(new ArrayList<>());


    productServiceWrapper.autoFillAttributes(STORE_ID, PRODUCT_CODE);

    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(product.getStoreId(), product, false);
    verify(categoryService).getCategoryAttributes(any(), any());
  }

  @Test
  public void autoFillAttributesNameNotMatchingTest() throws Exception {
    Product product = new Product();
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    Attribute attribute = new Attribute();
    category.setExtractionType(ExtractionType.INLINE_TEXT);
    productCategory.setCategory(category);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setName(ATTRIBUTE_NAME);
    productAttribute.setAttribute(attribute);
    productAttribute.setProductAttributeValues(Arrays.asList(productAttributeValue));
    product.setProductAttributes(Arrays.asList(productAttribute));
    product.setProductCategories(Arrays.asList(productCategory));
    product.setDescription("".getBytes(StandardCharsets.UTF_8));

    HashMap<String, Object> attributeValueAndNameMap = new HashMap<>();
    attributeValueAndNameMap.put(ATTRIBUTE_NAME_2, ATTRIBUTE_VALUE);
    attributeValueAndNameMap.put(BRAND_ATTRIBUTE_CODE, new HashMap<>());
    MatrixAttributeExtractionResponse value = new MatrixAttributeExtractionResponse();
    value.setExtractedAttributes(attributeValueAndNameMap);
    SingleBaseResponse<MatrixAttributeExtractionResponse> matrixAttributeExtractionResponse =
        new SingleBaseResponse<>(true, null, null, value);


    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    doNothing().when(productService).setCompleteProductDetailsCached(product.getStoreId(), product, false);
    when(matrixOutbound.extractProductAttributesByTextDetails(
        any(MatrixAttributeExtractionRequest.class))).thenReturn(matrixAttributeExtractionResponse);
    when(categoryService.getCategoryAttributes(anyString(), anyString())).thenReturn(new ArrayList<>());


    productServiceWrapper.autoFillAttributes(STORE_ID, PRODUCT_CODE);

    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(product.getStoreId(), product, false);
    verify(matrixOutbound).extractProductAttributesByTextDetails(
        any(MatrixAttributeExtractionRequest.class));
    verify(categoryService).getCategoryAttributes(any(), any());
  }

  @Test
  public void autoFillAttributesNameWrongRepsonseFromAttributeExtractionTest() throws Exception {
    Product product = new Product();
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    Attribute attribute = new Attribute();
    category.setExtractionType(ExtractionType.INLINE_TEXT);
    productCategory.setCategory(category);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setName(ATTRIBUTE_NAME);
    productAttribute.setAttribute(attribute);
    productAttribute.setProductAttributeValues(Arrays.asList(productAttributeValue));
    product.setProductAttributes(Arrays.asList(productAttribute));
    product.setProductCategories(Arrays.asList(productCategory));
    product.setDescription("".getBytes(StandardCharsets.UTF_8));

    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    doNothing().when(productService).setCompleteProductDetailsCached(product.getStoreId(), product, false);
    when(matrixOutbound.extractProductAttributesByTextDetails(
        any(MatrixAttributeExtractionRequest.class))).thenReturn(null);
    when(categoryService.getCategoryAttributes(anyString(), anyString())).thenReturn(new ArrayList<>());


    productServiceWrapper.autoFillAttributes(STORE_ID, PRODUCT_CODE);

    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(product.getStoreId(), product, false);
    verify(matrixOutbound).extractProductAttributesByTextDetails(
        any(MatrixAttributeExtractionRequest.class));
    verify(categoryService).getCategoryAttributes(any(), any());
  }

  @Test

  public void updateFlagsOnNeedCorrectionAndEvictCache() throws Exception {
    NeedRevisionConfigRequest needRevisionConfigRequest =
      NeedRevisionConfigRequest.builder().reviewPending(true).revised(true).viewable(true).build();
    when(productService.updateFlagsOnNeedCorrection(STORE_ID, PRODUCT_CODE,
      needRevisionConfigRequest)).thenReturn(productData);
    productServiceWrapper.updateFlagsOnNeedCorrectionAndEvictCache(STORE_ID, PRODUCT_CODE, needRevisionConfigRequest);
    verify(productService).updateFlagsOnNeedCorrection(STORE_ID, PRODUCT_CODE, needRevisionConfigRequest);
    verify(productService).evictProductCache(STORE_ID, productData);

  }

  @Test
  public void updateProductReviewPendingAndEvictCache() throws Exception {
    when(productService.updateProductReviewPending(STORE_ID, PRODUCT_CODE, true)).thenReturn(productData);
    productServiceWrapper.updateProductReviewPendingAndEvictCache(STORE_ID, PRODUCT_CODE, true);
    verify(productService).updateProductReviewPending(STORE_ID, PRODUCT_CODE, true);
    verify(productService).evictProductCache(STORE_ID, productData);
  }

//  @Test
//  public void regenerateProductAndEvictCache() throws Exception {
//    Product newProduct = product;
//    ProductDTO productDTO = ConverterUtil.convertProductToDTO(newProduct);
//    ProductDTO newProductDTO = productDTO;
//    ProductItemDTO productItemDTO = ConverterUtil.convertProductItemToDTO(productItem);
//    newProduct.setReviewPending(false);
//    Map<String, ProductDTO> productMap = new HashMap<>();
//    Map<ProductItem, String> productItemMap = new HashMap<>();
//    productMap.put(OLD_PRODUCT, productDTO);
//    productMap.put(NEW_PRODUCT, newProductDTO);
//    productItemMap.put(productItem, String.valueOf(Boolean.TRUE));
//    Pair<Map<String, ProductDTO>, Map<ProductItem, String>> productAndProductItemMap =
//      Pair.of(productMap, productItemMap);
//    doNothing().when(productService)
//        .evictCacheAndPublishForProductItemUpdate(STORE_ID, product, newProduct, false, true,
//          true, convertItemWithOldVatValueMap(productItemMap),
//            false, new ProductAndItemLevelUpdatesDTO(), new HashSet<>(), new HashSet<>());
//    when(productService.regenerateProductItem(anyString(), any(Product.class), any(Product.class), anyBoolean(),
//        anyBoolean(), anyBoolean(), anyBoolean(), anyBoolean(), anyBoolean() , anyBoolean())).thenReturn(productAndProductItemMap);
//    this.productServiceWrapper.regenerateProductAndEvictCache(STORE_ID, product, newProduct, false, true, true, false,
//        false, false, false, new ProductAndItemLevelUpdatesDTO(), new HashSet<>());
//    verify(productService).regenerateProductItem(STORE_ID, product, newProduct, false, true, true
//      , false, false, false , false);
//    verify(productService).evictCacheAndPublishForProductItemUpdate(STORE_ID, product, newProduct, false, true, true,
//      convertItemWithOldVatValueMap(productItemMap), false, new ProductAndItemLevelUpdatesDTO(), new HashSet<>(),
//        new HashSet<>());
//  }

  @Test
  public void initTest() {
    productServiceWrapper.init();
  }

  private Map<ProductItem, String> convertItemWithOldVatValueMap(Map<ProductItem, String> itemWithOldVatValueMapDTO) {
    return itemWithOldVatValueMapDTO.entrySet()
      .stream()
      .collect(Collectors.toMap(Map.Entry::getKey,
        Map.Entry::getValue
      ));
  }

  @Test
  public void migrateFinalImagesFromGfsToGcsForProductsTest() throws Exception {
    when(productMigrationService.findProductMigrationByProductCodeAndStatusAndMigrationType(PRODUCT_CODE,
        ProductMigrationStatus.PUBLISHED.name(),
        ProductMigrationType.GFS_TO_GCS_FINAL_IMAGE_MIGRATION.name())).thenReturn(new ProductMigration());
    when(imageService.findProductAndItemLocationPathsByProductCodeListAndStoreId(Arrays.asList(PRODUCT_CODE),
        STORE_ID)).thenReturn(ImmutableSet.of(LOCATION_PATH));
    doNothing().when(fileStorageService).migrateFinalImageFromGfsToGcs(ImmutableSet.of(LOCATION_PATH));
    when(productMigrationService.saveProductMigration(commonImageMigrationArgumentCaptor.capture())).thenReturn(
        new ProductMigration());

    productServiceWrapper.migrateFinalImagesFromGfsToGcsForProducts(STORE_ID, PRODUCT_CODE);

    verify(productMigrationService).findProductMigrationByProductCodeAndStatusAndMigrationType(PRODUCT_CODE,
        ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.GFS_TO_GCS_FINAL_IMAGE_MIGRATION.name());
    verify(imageService).findProductAndItemLocationPathsByProductCodeListAndStoreId(Arrays.asList(PRODUCT_CODE),
        STORE_ID);
    verify(fileStorageService).migrateFinalImageFromGfsToGcs(ImmutableSet.of(LOCATION_PATH));
    verify(productMigrationService).saveProductMigration(commonImageMigrationArgumentCaptor.getAllValues().get(0));
    verify(productMigrationService).saveProductMigration(commonImageMigrationArgumentCaptor.getAllValues().get(1));

    assertEquals(ProductMigrationStatus.IN_PROGRESS.name(), commonImageMigrationArgumentCaptor.getAllValues().get(0).getStatus());
    assertEquals(ProductMigrationStatus.SUCCESS.name(), commonImageMigrationArgumentCaptor.getAllValues().get(1).getStatus());
  }

  @Test
  public void migrateFinalImagesFromGfsToGcsForProductsExceptionTest() throws Exception {
    when(productMigrationService.findProductMigrationByProductCodeAndStatusAndMigrationType(PRODUCT_CODE,
        ProductMigrationStatus.PUBLISHED.name(),
        ProductMigrationType.GFS_TO_GCS_FINAL_IMAGE_MIGRATION.name())).thenReturn(new ProductMigration());
    when(imageService.findProductAndItemLocationPathsByProductCodeListAndStoreId(Arrays.asList(PRODUCT_CODE),
        STORE_ID)).thenThrow(ApplicationRuntimeException.class);
    when(productMigrationService.saveProductMigration(commonImageMigrationArgumentCaptor.capture())).thenReturn(
        new ProductMigration());

    productServiceWrapper.migrateFinalImagesFromGfsToGcsForProducts(STORE_ID, PRODUCT_CODE);

    verify(productMigrationService).findProductMigrationByProductCodeAndStatusAndMigrationType(PRODUCT_CODE,
        ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.GFS_TO_GCS_FINAL_IMAGE_MIGRATION.name());
    verify(imageService).findProductAndItemLocationPathsByProductCodeListAndStoreId(Arrays.asList(PRODUCT_CODE),
        STORE_ID);
    verify(productMigrationService).saveProductMigration(commonImageMigrationArgumentCaptor.getAllValues().get(0));
    verify(productMigrationService).saveProductMigration(commonImageMigrationArgumentCaptor.getAllValues().get(1));

    assertEquals(ProductMigrationStatus.IN_PROGRESS.name(), commonImageMigrationArgumentCaptor.getAllValues().get(0).getStatus());
    assertEquals(ProductMigrationStatus.FAILED.name(), commonImageMigrationArgumentCaptor.getAllValues().get(1).getStatus());
  }

  @Test
  public void migrateFinalImagesFromGfsToGcsForProductsNotFoundTest()  {
    when(productMigrationService.findProductMigrationByProductCodeAndStatusAndMigrationType(PRODUCT_CODE,
        ProductMigrationStatus.PUBLISHED.name(),
        ProductMigrationType.GFS_TO_GCS_FINAL_IMAGE_MIGRATION.name())).thenReturn(null);

    productServiceWrapper.migrateFinalImagesFromGfsToGcsForProducts(STORE_ID, PRODUCT_CODE);

    verify(productMigrationService).findProductMigrationByProductCodeAndStatusAndMigrationType(PRODUCT_CODE,
        ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.GFS_TO_GCS_FINAL_IMAGE_MIGRATION.name());
  }

  @Test
  public void saveProductItemDetailsTest() throws Exception {
    Mockito.when(productItemService.saveProductItemDetails(STORE_ID, Arrays.asList(productItem), PRODUCT_CODE))
        .thenReturn(productData);
    productServiceWrapper.saveProductItemDetails(STORE_ID, Arrays.asList(productItem), PRODUCT_CODE);

    Mockito.when(productItemService.saveProductItemDetails(STORE_ID, Arrays.asList(productItem), PRODUCT_CODE))
        .thenReturn(null);
    productServiceWrapper.saveProductItemDetails(STORE_ID, Arrays.asList(productItem), PRODUCT_CODE);

    verify(productItemService, times(2)).saveProductItemDetails(STORE_ID, Arrays.asList(productItem), PRODUCT_CODE);
    verify(productService).evictProductCache(STORE_ID, productData);
    verify(applicationCacheServiceBean).evictProductItemsCacheByStoreIdAndProductId(STORE_ID, productData.getId());
    verify(applicationCacheServiceBean).evictProductItemImagesCacheByStoreIdAndProductId(STORE_ID, productData.getId());
    verify(applicationCacheServiceBean).evictProductItemAttributeValuesCacheByStoreIdAndProductId(STORE_ID, productData.getId());
  }
  @Test
  public void backfillUpcInProductTest() throws Exception {
    when(productMigrationService.findProductMigrationByProductCodeAndStatusAndMigrationType(PRODUCT_CODE,
      ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.UPC_MIGRATION.name())).thenReturn(commonImageMigration);
    when(productMigrationService.saveProductMigration(commonImageMigrationArgumentCaptor.capture())).thenReturn(
      commonImageMigration);
    when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE)).thenReturn(
        productData);
    doNothing().when(productItemServiceWrapper).setProductItemsCached(STORE_ID, productData, false);
    when(domainEventPublisherService.publishProductForMasterDataMigration(productArgumentCaptor.capture(),
      eq(StringUtils.EMPTY))).thenReturn(new ProductDomainEventModel());

    productServiceWrapper.backfillCommonImageFlagInProductAndItemImages(STORE_ID, PRODUCT_CODE,
      ProductMigrationType.UPC_MIGRATION.name());

    verify(productMigrationService).findProductMigrationByProductCodeAndStatusAndMigrationType(PRODUCT_CODE,
      ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.UPC_MIGRATION.name());
    verify(productMigrationService, times(2)).saveProductMigration(any(ProductMigration.class));
    verify(domainEventPublisherService).publishProductForMasterDataMigration(any(Product.class),
      anyString());
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productItemServiceWrapper).setProductItemsCached(STORE_ID, productData, false);
    assertEquals(ProductMigrationStatus.SUCCESS.name(),
      commonImageMigrationArgumentCaptor.getAllValues().get(1).getStatus());
  }

  @Test
  public void markForDeleteProductAndEvictCacheTest() throws Exception {
    Set<String> imageLocations = new HashSet<>();
    Pair<Product, Set<String>> productSetPair = Pair.of(productData, imageLocations);
    when(productService.markForDeleteProduct(STORE_ID, productData.getId())).thenReturn(productSetPair);
    this.productServiceWrapper.markForDeleteProductAndEvictCache(STORE_ID, productData.getId());
    verify(productService).evictAllProductDetailCache(STORE_ID, productData);
    verify(domainEventPublisherService).publishProduct(productData);
    verify(productService).deleteImageByImageLocations(imageLocations);
    verify(productService).markForDeleteProduct(STORE_ID, productData.getId());
  }

  @Test
  public void markForDeleteProductAndEvictCacheFalseTest() throws Exception {
    productImage.setOriginalImage(true);
    Set<String> imageLocations = new HashSet<>();
    Pair<Product, Set<String>> productSetPair = Pair.of(productData, imageLocations);
    productSetPair.getRight().add(productImage.getLocationPath());
    productSetPair.getLeft().setEdited(true);
    when(productService.markForDeleteProduct(STORE_ID, productData.getId())).thenReturn(productSetPair);
    this.productServiceWrapper.markForDeleteProductAndEvictCache(STORE_ID, productData.getId());
    verify(productService).evictAllProductDetailCache(STORE_ID, productData);
    verify(domainEventPublisherService).publishProduct(productData);
    verify(productService).markForDeleteProduct(STORE_ID, productData.getId());
  }

  @Test
  public void updateProductDimensionsInvalidDimensionTest() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "validateDimensionSwitch", Boolean.TRUE);
    warehouseMasterSKUEvent.setLength(null);
    warehouseMasterSKUEvent.setWeight(null);
    warehouseMasterSKUEvent.setWidth(null);
    warehouseMasterSKUEvent.setHeight(null);
    boolean response = productServiceWrapper.updateProductDimensions(warehouseMasterSKUEvent);
    Assertions.assertFalse(response);
  }

  @Test
  public void updateProductDimensionsInvalidSwitchOffTest() throws Exception {
    warehouseMasterSKUEvent.setHeight(0.0);
    warehouseMasterSKUEvent.setWidth(0.0);
    warehouseMasterSKUEvent.setWeight(0.0);
    warehouseMasterSKUEvent.setLength(0.0);
    productItem.setUpcCode(UPC_CODE);
    productItem.setProductId(PRODUCT_ID);
    when(productItemService.findByStoreIdAndSkuCode(DEFAULT_STORE_ID, warehouseMasterSKUEvent.getItemCode()))
        .thenReturn(productItem);
    when(productService.getProductByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID)).thenReturn(productData);
    when(productService.updateProductDimensions(warehouseMasterSKUEvent, productItem.getProductId())).thenReturn(
        productData);
    doNothing().when(productService).evictProductCache(DEFAULT_STORE_ID, productData);
    doNothing().when(applicationCacheServiceBean)
        .evictProductItemsCacheByStoreIdAndProductId(DEFAULT_STORE_ID, productData.getId());
    boolean status = productServiceWrapper.updateProductDimensions(warehouseMasterSKUEvent);
    verify(productItemService).findByStoreIdAndSkuCode(DEFAULT_STORE_ID, warehouseMasterSKUEvent.getItemCode());
    verify(productService).updateProductDimensions(warehouseMasterSKUEvent, PRODUCT_ID);
    Mockito.verify(productService).evictProductCache(DEFAULT_STORE_ID, productData);
    Mockito.verify(applicationCacheServiceBean).evictProductItemsCacheByStoreIdAndProductId(DEFAULT_STORE_ID, productData.getId());
    Mockito.verify(productService).publishProduct(productData, false);
    assertTrue(status);
  }

  @Test
  public void testUpdateProductMasterDataAndImagesAndUpcCode() throws Exception {
    boolean ignoreSalesCategoryPublish = false;
    EditProductDetailRequest editProductDetailRequest = new EditProductDetailRequest();
    productRequest.setPristineCategory(true);
    productRequest.setScoreUpdated(true);
    editProductDetailRequest.setProductRequest(productRequest);
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    productImageEditRequest.setProductCode(productRequest.getProductCode());
    editProductDetailRequest.setProductImageEditRequests(Collections.singletonList(productImageEditRequest));
    Product newProduct = productData;
    productData.setDescription("".getBytes(StandardCharsets.UTF_8));
    productData.setBrand(BRAND);
    newProduct.setDescription("description".getBytes(StandardCharsets.UTF_8));
    newProduct.setBrand(BRAND.concat(BRAND));
    ProductDTO productDTO = ConverterUtil.convertProductToDTO(newProduct);
    ProductDTO newProductDTO = productDTO;
    newProduct.setReviewPending(false);
    Map<String, ProductDTO> productMap = new HashMap<>();
    Map<ProductItem, String> productItemMap = new HashMap<>();
    productMap.put(OLD_PRODUCT, productDTO);
    productMap.put(NEW_PRODUCT, newProductDTO);
    productItemMap.put(productItem, String.valueOf(Boolean.TRUE));
    Pair<Map<String, ProductDTO>, Map<ProductItem, String>> productAndProductItemMap =
      Pair.of(productMap, productItemMap);
    ProductAttributeExtracted productAttributeExtracted;
    productAttributeExtracted = new ProductAttributeExtracted();
    productAttributeExtracted.setProductCode(PRODUCT_CODE);
    productAttributeExtracted.setCnCategoryCode(CATEGORY_CODE);
    productAttributeExtracted.setStatus(ExtractionStatus.PENDING);
    newProduct.setProductImages(Arrays.asList(productImage));
    newProduct.setProductCode(PRODUCT_CODE);
    List<NewlySavedItemResponse> newlySavedItemResponseList = new ArrayList<>();
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(productData);
    when(productService.checkBrandChanges(newProduct)).thenReturn(null);
    when(productService.adjustProductItem(STORE_ID, productData, newProduct, false, false, true))
      .thenReturn(productAndProductItemMap);
    when(productAttributeExtractionService.fetchAndSetProductAttributeExtraction(PRODUCT_CODE,
      newProduct.getProductCategories().stream().findFirst().get().getCategory(),
      STORE_ID))
      .thenReturn(productAttributeExtracted);
    when(productService.updateImages(STORE_ID, false,
      editProductDetailRequest.getProductImageEditRequests(), productDTO, true)).thenReturn(Pair.of(
      ProductPublishUpdateDTO.builder().productDTO(productDTO).productLevelDataUpdated(true)
        .build(), Collections.emptyMap()));
    when(productService.updateProductContentAndImages(STORE_ID, newProduct,
      editProductDetailRequest, newlySavedItemResponseList)).thenReturn(ProductDetailEditDTO.builder().finalProduct(newProduct)
      .productAndProductItemMap(productAndProductItemMap)
      .productImageMapPair(Pair.of(newProduct, new HashMap<>()))
      .productAndItemLevelUpdatesDTO(ProductAndItemLevelUpdatesDTO.builder().updatedItemSkuCodes(new HashSet<>()).build()).build());
    when(productService.saveAndFlush(newProduct)).thenReturn(newProduct);
    doNothing().when(productAttributeExtractionService).saveProductToAttributeExtraction(productAttributeExtracted);
    doNothing().when(productService)
      .evictCacheAndPublishForProductItemUpdate(STORE_ID, productData, newProduct, true, false,
        true, convertItemWithOldVatValueMap(productItemMap),
        false, new ProductAndItemLevelUpdatesDTO(false, new HashSet<>()), new HashSet<>(), new HashSet<>());
    EditProductItemAndImageResponse editProductItemAndImageResponse =
      productServiceWrapper.updateProductMasterDataAndImagesAndUpcCode(newProduct,
        editProductDetailRequest, PRODUCT_CODE, STORE_ID, newlySavedItemResponseList);
    verify(productService).evictCacheAndPublishForProductItemUpdate(STORE_ID, productData, newProduct
      , true, false,
      true, convertItemWithOldVatValueMap(productItemMap),
      false, new ProductAndItemLevelUpdatesDTO(false,new HashSet<>() ), new HashSet<>(), new HashSet<>());
    Mockito.verify(productService).updateProductContentAndImages(STORE_ID, newProduct,
      editProductDetailRequest, newlySavedItemResponseList);
    assertEquals(newlySavedItemResponseList, editProductItemAndImageResponse.getNewlySavedItemResponses());
  }

  @Test
  public void testUpdateProductMasterDataAndImagesAndUpcCodeWithNonEmptyNewlySaved() throws Exception {
    boolean ignoreSalesCategoryPublish = false;
    EditProductDetailRequest editProductDetailRequest = new EditProductDetailRequest();
    productRequest.setPristineCategory(true);
    productRequest.setScoreUpdated(true);
    editProductDetailRequest.setProductRequest(productRequest);
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    productImageEditRequest.setProductCode(productRequest.getProductCode());
    editProductDetailRequest.setProductImageEditRequests(Collections.singletonList(productImageEditRequest));
    Product newProduct = productData;
    productData.setDescription("".getBytes(StandardCharsets.UTF_8));
    productData.setBrand(BRAND);
    newProduct.setDescription("description".getBytes(StandardCharsets.UTF_8));
    newProduct.setBrand(BRAND.concat(BRAND));
    ProductDTO productDTO = ConverterUtil.convertProductToDTO(newProduct);
    ProductDTO newProductDTO = productDTO;
    newProduct.setReviewPending(false);
    Map<String, ProductDTO> productMap = new HashMap<>();
    Map<ProductItem, String> productItemMap = new HashMap<>();
    productMap.put(OLD_PRODUCT, productDTO);
    productMap.put(NEW_PRODUCT, newProductDTO);
    productItemMap.put(productItem, String.valueOf(Boolean.TRUE));
    Pair<Map<String, ProductDTO>, Map<ProductItem, String>> productAndProductItemMap =
      Pair.of(productMap, productItemMap);
    ProductAttributeExtracted productAttributeExtracted;
    productAttributeExtracted = new ProductAttributeExtracted();
    productAttributeExtracted.setProductCode(PRODUCT_CODE);
    productAttributeExtracted.setCnCategoryCode(CATEGORY_CODE);
    productAttributeExtracted.setStatus(ExtractionStatus.PENDING);
    newProduct.setProductImages(Arrays.asList(productImage));
    newProduct.setProductCode(PRODUCT_CODE);
    NewlySavedItemResponse newlySavedItemResponse = new NewlySavedItemResponse();
    List<NewlySavedItemResponse> newlySavedItemResponseList =
      Collections.singletonList(newlySavedItemResponse);
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(productData);
    when(productService.checkBrandChanges(newProduct)).thenReturn(null);
    when(productService.adjustProductItem(STORE_ID, productData, newProduct, false, false, true))
      .thenReturn(productAndProductItemMap);
    when(productAttributeExtractionService.fetchAndSetProductAttributeExtraction(PRODUCT_CODE,
      newProduct.getProductCategories().stream().findFirst().get().getCategory(),
      STORE_ID))
      .thenReturn(productAttributeExtracted);
    when(productService.updateImages(STORE_ID, false,
      editProductDetailRequest.getProductImageEditRequests(), productDTO, true)).thenReturn(Pair.of(
      ProductPublishUpdateDTO.builder().productDTO(productDTO).productLevelDataUpdated(true)
        .build(), Collections.emptyMap()));
    when(productService.updateProductContentAndImages(STORE_ID, newProduct,
      editProductDetailRequest, newlySavedItemResponseList)).thenReturn(ProductDetailEditDTO.builder().finalProduct(newProduct)
      .productAndProductItemMap(productAndProductItemMap)
      .productImageMapPair(Pair.of(newProduct, new HashMap<>()))
      .productAndItemLevelUpdatesDTO(ProductAndItemLevelUpdatesDTO.builder().updatedItemSkuCodes(new HashSet<>()).build()).build());
    when(productService.saveAndFlush(newProduct)).thenReturn(newProduct);
    doNothing().when(productAttributeExtractionService).saveProductToAttributeExtraction(productAttributeExtracted);
    doNothing().when(productService)
      .evictCacheAndPublishForProductItemUpdate(STORE_ID, productData, newProduct, true, false,
        true, convertItemWithOldVatValueMap(productItemMap),
        false, new ProductAndItemLevelUpdatesDTO(false, new HashSet<>()), new HashSet<>(), new HashSet<>());
    EditProductItemAndImageResponse editProductItemAndImageResponse =
      productServiceWrapper.updateProductMasterDataAndImagesAndUpcCode(newProduct,
        editProductDetailRequest, PRODUCT_CODE, STORE_ID, newlySavedItemResponseList);
    verify(productService).evictCacheAndPublishForProductItemUpdate(STORE_ID, productData, newProduct
      , true, false,
      true, convertItemWithOldVatValueMap(productItemMap),
      false, new ProductAndItemLevelUpdatesDTO(false,new HashSet<>() ), new HashSet<>(), new HashSet<>());
    Mockito.verify(productService).updateProductContentAndImages(STORE_ID, newProduct,
      editProductDetailRequest, newlySavedItemResponseList);
    assertEquals(newlySavedItemResponseList, editProductItemAndImageResponse.getNewlySavedItemResponses());
  }

  @Test
  public void testSetExtractedProductAttributes_ProductDetailsChanged() {
    ProductDetailEditDTO productDetailEditDTO = new ProductDetailEditDTO();
    EditProductDetailRequest editProductDetailRequest = new EditProductDetailRequest();
    productRequest.setPristineCategory(true);
    productRequest.setScoreUpdated(true);
    editProductDetailRequest.setProductRequest(productRequest);
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    productImageEditRequest.setProductCode(productRequest.getProductCode());
    editProductDetailRequest.setProductImageEditRequests(
      Collections.singletonList(productImageEditRequest));
    productDetailEditDTO.setProductDetailsChanged(true);
    Product newProduct = productData;
    productData.setDescription("".getBytes(StandardCharsets.UTF_8));
    productData.setBrand(BRAND);
    newProduct.setDescription("description".getBytes(StandardCharsets.UTF_8));
    newProduct.setBrand(BRAND.concat(BRAND));
    newProduct.setReviewPending(false);
    Mockito.when(productAttributeExtractionService.fetchAndSetProductAttributeExtraction(
      eq(newProduct.getProductCode()), eq(
        newProduct.getProductCategories().stream().findFirst().orElse(new ProductCategory())
          .getCategory()), eq(STORE_ID))).thenReturn(new ProductAttributeExtracted());
    productServiceWrapper.setExtractedProductAttributes(newProduct, editProductDetailRequest,
      newProduct.getProductCode(), STORE_ID, productDetailEditDTO);
    Mockito.verify(productAttributeExtractionService, Mockito.times(1))
      .fetchAndSetProductAttributeExtraction(eq(newProduct.getProductCode()), eq(
        newProduct.getProductCategories().stream().findFirst().orElse(new ProductCategory())
          .getCategory()), eq(STORE_ID));
    Mockito.verify(productAttributeExtractionService, Mockito.times(1))
      .saveProductToAttributeExtraction(any());
  }

  @Test
  public void testSetExtractedProductAttributes_NullProductAttributeExtracted() {
    ProductDetailEditDTO productDetailEditDTO = new ProductDetailEditDTO();
    EditProductDetailRequest editProductDetailRequest = new EditProductDetailRequest();
    productRequest.setPristineCategory(true);
    productRequest.setScoreUpdated(true);
    editProductDetailRequest.setProductRequest(productRequest);
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    productImageEditRequest.setProductCode(productRequest.getProductCode());
    editProductDetailRequest.setProductImageEditRequests(
      Collections.singletonList(productImageEditRequest));
    productDetailEditDTO.setProductDetailsChanged(true);
    Product newProduct = productData;
    productData.setDescription("".getBytes(StandardCharsets.UTF_8));
    productData.setBrand(BRAND);
    newProduct.setDescription("description".getBytes(StandardCharsets.UTF_8));
    newProduct.setBrand(BRAND.concat(BRAND));
    Mockito.when(productAttributeExtractionService.fetchAndSetProductAttributeExtraction(
      eq(newProduct.getProductCode()), eq(
        newProduct.getProductCategories().stream().findFirst().orElse(new ProductCategory())
          .getCategory()), eq(STORE_ID))).thenReturn(null);
    productServiceWrapper.setExtractedProductAttributes(newProduct, editProductDetailRequest,
      newProduct.getProductCode(), STORE_ID, productDetailEditDTO);
    Mockito.verify(productAttributeExtractionService, Mockito.times(1))
      .fetchAndSetProductAttributeExtraction(eq(newProduct.getProductCode()), eq(
        newProduct.getProductCategories().stream().findFirst().orElse(new ProductCategory())
          .getCategory()), eq(STORE_ID));
    Mockito.verify(productAttributeExtractionService, never()).saveProductToAttributeExtraction(any());
  }

  //TODO:check this test case
  @Test
  public void updateProductBrandDataTest() throws Exception {
    productData.setBrand(BRAND_ATTRIBUTE_NAME);
    when(productService.updateProductBrand(STORE_ID, productBrandUpdateDTO)).thenReturn(
        Pair.of(productData, BRAND_ATTRIBUTE_CODE));
    ProductBrandUpdateResponseDTO responseDTO =
        productServiceWrapper.updateProductBrandData(STORE_ID, productBrandUpdateDTO);
    verify(productService).evictCacheForSimpleMasterProductUpdate(STORE_ID, PRODUCT_ID, PRODUCT_CODE);
    verify(productService).updateProductBrand(STORE_ID, productBrandUpdateDTO);
    verify(domainEventPublisherService).publishProductChangeCategory(PRODUCT, null, true, true, true, false,
        new HashSet<>());
    assertEquals(PRODUCT_CODE, responseDTO.getProductCode());
    assertEquals(BRAND_ATTRIBUTE_CODE, responseDTO.getBrandCode());
    assertEquals(BRAND_ATTRIBUTE_NAME, responseDTO.getBrandName());
  }

  @Test
  public void updateProductBrandDataTestException() throws Exception {
    doThrow(ApplicationRuntimeException.class).when(productService).updateProductBrand(STORE_ID, productBrandUpdateDTO);
    try{
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productServiceWrapper.updateProductBrandData(STORE_ID, productBrandUpdateDTO));
    }finally {
      verify(productService).updateProductBrand(STORE_ID, productBrandUpdateDTO);
    }
  }

  @Test
  public void getProductAttributeDetailsByProductIdTest() {
    productServiceWrapper.getProductAttributeDetailsByProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(productService).getProductAttributes(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void processProductAttributeDataBackFillingTest_2() {
    CommonImageBackfillingEventModel productAttributeDataBackFillingEventModel =
        new CommonImageBackfillingEventModel();
    productAttributeDataBackFillingEventModel.setProductCode(PRODUCT_CODE);
    Mockito.when(productService.fetchProductAndInsertMissingProductAttributes(
            productAttributeDataBackFillingEventModel))
        .thenReturn(Pair.of(productData, List.of(productAttributeValue)));
    Mockito.doNothing().when(productService)
        .evictProductCache(productAttributeDataBackFillingEventModel.getStoreId(), productData);

    productServiceWrapper.processProductAttributeDataBackFilling(
        productAttributeDataBackFillingEventModel);

    Mockito.verify(productService)
        .fetchProductAndInsertMissingProductAttributes(productAttributeDataBackFillingEventModel);
    Mockito.verify(productService).clearProductCacheAndProductAttributesCache(
        productAttributeDataBackFillingEventModel.getStoreId(), productData.getProductCode(),
        productData.getId());
    Mockito.verify(domainEventPublisherService)
        .publishPBPAttributeMigrationEvent(productAttributeValue, productData.getProductCode());
  }

  @Test
  public void processProductAttributeDataBackFillingTest_mfdFilteringEnabledTrue() {
    ReflectionTestUtils.setField(productServiceWrapper,
        "productAttributeMigrationPredefinedAttributeValueFilteringEnabled", Boolean.TRUE);
    CommonImageBackfillingEventModel productAttributeDataBackFillingEventModel =
        new CommonImageBackfillingEventModel();
    productAttributeDataBackFillingEventModel.setProductCode(PRODUCT_CODE);
    productAttributeValue1.setMarkForDelete(true);
    Mockito.when(productService.fetchProductAndInsertMissingProductAttributes(
            productAttributeDataBackFillingEventModel))
        .thenReturn(Pair.of(productData, List.of(productAttributeValue, productAttributeValue1)));
    Mockito.doNothing().when(productService)
        .evictProductCache(productAttributeDataBackFillingEventModel.getStoreId(), productData);

    productServiceWrapper.processProductAttributeDataBackFilling(
        productAttributeDataBackFillingEventModel);

    Mockito.verify(productService)
        .fetchProductAndInsertMissingProductAttributes(productAttributeDataBackFillingEventModel);
    Mockito.verify(productService).clearProductCacheAndProductAttributesCache(
        productAttributeDataBackFillingEventModel.getStoreId(), productData.getProductCode(),
        productData.getId());
    Mockito.verify(domainEventPublisherService)
        .publishPBPAttributeMigrationEvent(productAttributeValue, productData.getProductCode());
  }

  @Test
  public void processProductAttributeDataBackFillingTestWithNullProduct() {
    CommonImageBackfillingEventModel productAttributeDataBackFillingEventModel =
        new CommonImageBackfillingEventModel();
    productAttributeDataBackFillingEventModel.setProductCode(PRODUCT_CODE);
    productAttributeDataBackFillingEventModel.setStoreId(STORE_ID);
    ProductMigrationRequest migrationRequest = ProductMigrationRequest.builder()
      .productCode(productAttributeDataBackFillingEventModel.getProductCode())
      .migrationType(productAttributeDataBackFillingEventModel.getMigrationType())
      .updatedStatus(ProductMigrationStatus.FAILED.name()).migrationPayload(
        String.valueOf(productAttributeDataBackFillingEventModel.getMigrationPayloadList()))
      .errorMessage(ErrorMessage.PRODUCT_NOT_FOUND.getMessage()).build();
    Mockito.when(productService.fetchProductAndInsertMissingProductAttributes(
            productAttributeDataBackFillingEventModel))
        .thenReturn(Pair.of(null, List.of(productAttributeValue)));
    productServiceWrapper.processProductAttributeDataBackFilling(
        productAttributeDataBackFillingEventModel);

    Mockito.verify(productService)
        .fetchProductAndInsertMissingProductAttributes(productAttributeDataBackFillingEventModel);
    Mockito.verify(schedulerService).updateProductMigrationStatus(STORE_ID, Constants.CLIENT_ID, migrationRequest);
  }

  @Test
  public void processProductAttributeDataBackFillingTestWithEmptyProductAttributes() {
    CommonImageBackfillingEventModel productAttributeDataBackFillingEventModel =
        new CommonImageBackfillingEventModel();
    productAttributeDataBackFillingEventModel.setProductCode(PRODUCT_CODE);
    ProductMigrationRequest migrationRequest = ProductMigrationRequest.builder()
      .productCode(productAttributeDataBackFillingEventModel.getProductCode())
      .migrationType(productAttributeDataBackFillingEventModel.getMigrationType())
      .updatedStatus(ProductMigrationStatus.FAILED.name()).migrationPayload(
        String.valueOf(productAttributeDataBackFillingEventModel.getMigrationPayloadList()))
      .errorMessage(ErrorMessage.NO_PENDING_ATTRIBUTES_TO_BE_MAPPED.getMessage()).build();
    productAttributeDataBackFillingEventModel.setProductCode(PRODUCT_CODE);
    Mockito.when(productService.fetchProductAndInsertMissingProductAttributes(
        productAttributeDataBackFillingEventModel)).thenReturn(Pair.of(productData, new ArrayList<>()));
    productServiceWrapper.processProductAttributeDataBackFilling(
        productAttributeDataBackFillingEventModel);
    Mockito.verify(schedulerService).updateProductMigrationStatus(Constants.DEFAULT_STORE_ID, Constants.CLIENT_ID, migrationRequest);
    Mockito.verify(productService)
        .fetchProductAndInsertMissingProductAttributes(productAttributeDataBackFillingEventModel);
  }

  @Test
  public void processProductDsAttributeMappingTest() throws Exception {
    ProductSuitabilityEventModel eventModel = new ProductSuitabilityEventModel();
    eventModel.setStoreId(STORE_ID);
    eventModel.setProductId(PRODUCT_CODE);
    List<ProductSuitabilityAttributeModel> attributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel attributeModel = new ProductSuitabilityAttributeModel();
    attributeModel.setAttributeCode(ATTRIBUTE_CODE);
    attributeModel.setValue(ATTRIBUTE_VALUE);
    attributeModels.add(attributeModel);
    eventModel.setAttributeValueExtractions(attributeModels);
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setStoreId(STORE_ID);
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCategoryId(CATEGORY_ID);
    product.setProductCategories(Collections.singletonList(productCategory));
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setId(ATTRIBUTE_ID);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setDsExtraction(true);
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(attribute);
    when(productService.getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE)).thenReturn(
        product);
    when(attributeService.findAttributesByStoreIdAndAttributeCodeList(STORE_ID,
        Collections.singletonList(ATTRIBUTE_CODE))).thenReturn(Collections.singletonList(attribute));
    when(categoryService.getCategoryAttributes(STORE_ID, CATEGORY_ID)).thenReturn(List.of(categoryAttribute));
    productServiceWrapper.validateAndProcessProductDsAttributeMapping(eventModel);
    verify(productService).getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    verify(attributeService).findAttributesByStoreIdAndAttributeCodeList(STORE_ID,
        Collections.singletonList(ATTRIBUTE_CODE));
    verify(categoryService).getCategoryAttributes(STORE_ID, CATEGORY_ID);
    verify(productService).saveAndFlush(product);
    verify(productService).evictAllProductDetailCache(STORE_ID, product);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, product, true);
    verify(domainEventPublisherService).publishProductChangeCategory(product, null, false, false, false, false,
        new HashSet<>());
  }

  @Test
  public void processProductDsAttributeMappingEmptyMapTest() throws Exception {
    ProductSuitabilityEventModel eventModel = new ProductSuitabilityEventModel();
    eventModel.setStoreId(STORE_ID);
    eventModel.setProductId(PRODUCT_CODE);
    List<ProductSuitabilityAttributeModel> attributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel attributeModel = new ProductSuitabilityAttributeModel();
    attributeModel.setAttributeCode(ATTRIBUTE_CODE);
    attributeModel.setValue(ATTRIBUTE_VALUE);
    attributeModels.add(attributeModel);
    eventModel.setAttributeValueExtractions(attributeModels);
    productData.setProductCode(PRODUCT_CODE);
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCategoryId(CATEGORY_ID);
    productData.setProductCategories(Collections.singletonList(productCategory));
    Attribute attribute = new Attribute();
    attribute.setAttributeCode("");
    attribute.setId("");
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(attribute);
    when(productService.getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE)).thenReturn(
        productData);
    when(attributeService.findAttributesByStoreIdAndAttributeCodeList(STORE_ID,
        Collections.singletonList(ATTRIBUTE_CODE))).thenReturn(new ArrayList<>());
    when(categoryService.getCategoryAttributes(STORE_ID, CATEGORY_ID)).thenReturn(List.of(categoryAttribute));
    productServiceWrapper.validateAndProcessProductDsAttributeMapping(eventModel);
    verify(productService).getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    verify(attributeService).findAttributesByStoreIdAndAttributeCodeList(STORE_ID,
        Collections.singletonList(ATTRIBUTE_CODE));
    verify(categoryService).getCategoryAttributes(STORE_ID, CATEGORY_ID);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, productData, true);
  }

  @Test
  void processProductDsAttributeMappingNullTest() throws Exception {
    ProductSuitabilityEventModel eventModel = new ProductSuitabilityEventModel();
    eventModel.setStoreId(STORE_ID);
    eventModel.setProductId(PRODUCT_CODE);
    List<ProductSuitabilityAttributeModel> attributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel attributeModel = new ProductSuitabilityAttributeModel();
    attributeModel.setAttributeCode(ATTRIBUTE_CODE);
    attributeModel.setValue(null);
    attributeModels.add(attributeModel);
    eventModel.setAttributeValueExtractions(attributeModels);
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setStoreId(STORE_ID);
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCategoryId(CATEGORY_ID);
    product.setProductCategories(Collections.singletonList(productCategory));
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setId(ATTRIBUTE_ID);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setDsExtraction(true);
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(attribute);
    when(productService.getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE)).thenReturn(
      product);
    when(attributeService.findAttributesByStoreIdAndAttributeCodeList(STORE_ID,
      Collections.singletonList(ATTRIBUTE_CODE))).thenReturn(Collections.singletonList(attribute));
    when(categoryService.getCategoryAttributes(STORE_ID, CATEGORY_ID)).thenReturn(List.of(categoryAttribute));
    productServiceWrapper.validateAndProcessProductDsAttributeMapping(eventModel);
    verify(productService).getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    verify(attributeService).findAttributesByStoreIdAndAttributeCodeList(STORE_ID,
      Collections.singletonList(ATTRIBUTE_CODE));
    verify(categoryService).getCategoryAttributes(STORE_ID, CATEGORY_ID);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, product, true);
  }

  @Test
  void processProductDsAttributeMappingBlankTest() throws Exception {
    ProductSuitabilityEventModel eventModel = new ProductSuitabilityEventModel();
    eventModel.setStoreId(STORE_ID);
    eventModel.setProductId(PRODUCT_CODE);
    List<ProductSuitabilityAttributeModel> attributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel attributeModel = new ProductSuitabilityAttributeModel();
    attributeModel.setAttributeCode(ATTRIBUTE_CODE);
    attributeModel.setValue(StringUtils.EMPTY);
    attributeModels.add(attributeModel);
    eventModel.setAttributeValueExtractions(attributeModels);
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setStoreId(STORE_ID);
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCategoryId(CATEGORY_ID);
    product.setProductCategories(Collections.singletonList(productCategory));
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setId(ATTRIBUTE_ID);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setDsExtraction(true);
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(attribute);
    when(productService.getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE)).thenReturn(
      product);
    when(attributeService.findAttributesByStoreIdAndAttributeCodeList(STORE_ID,
      Collections.singletonList(ATTRIBUTE_CODE))).thenReturn(Collections.singletonList(attribute));
    when(categoryService.getCategoryAttributes(STORE_ID, CATEGORY_ID)).thenReturn(List.of(categoryAttribute));
    productServiceWrapper.validateAndProcessProductDsAttributeMapping(eventModel);
    verify(productService).getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    verify(attributeService).findAttributesByStoreIdAndAttributeCodeList(STORE_ID,
      Collections.singletonList(ATTRIBUTE_CODE));
    verify(categoryService).getCategoryAttributes(STORE_ID, CATEGORY_ID);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, product, true);
  }

  @Test
  public void publishEventToVendorTest() {
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setReviewPending(true);
    productServiceWrapper.publishEventToVendor(product);
    verify(domainEventPublisherService).publishVendorEvent(any(VendorPublishEventModel.class));
  }

  @Test
  public void publishEventToVendorSwitchOnTest() {
    ReflectionTestUtils.setField(productServiceWrapper, "publishVendorEventForDsAttributeMappingEnabled", Boolean.TRUE);
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setReviewPending(true);
    productServiceWrapper.publishEventToVendor(product);
    verify(domainEventPublisherService).publishVendorEvent(any(VendorPublishEventModel.class));
  }

  @Test
  public void publishEventToVendorSwitchOffTest() {
    ReflectionTestUtils.setField(productServiceWrapper, "publishVendorEventForDsAttributeMappingEnabled",
        Boolean.FALSE);
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setReviewPending(true);
    productServiceWrapper.publishEventToVendor(product);
  }

  @Test
  public void createAndMapProductAttributesAndAttributeValuesTest() throws Exception {
    List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList = new ArrayList<>();
    ProductSuitabilityAttributeModel productSuitabilityAttributeModel = new ProductSuitabilityAttributeModel();
    productSuitabilityAttributeModel.setAttributeCode(ATTRIBUTE_CODE);
    productSuitabilityAttributeModel.setValue(ATTRIBUTE_VALUE);
    productSuitabilityAttributeModel.setValueEn(ATTRIBUTE_VALUE_EN);
    productSuitabilityAttributeModelList.add(productSuitabilityAttributeModel);
    productData.setProductAttributes(new ArrayList<>(Collections.singletonList(productAttribute)));
    productServiceWrapper.createAndMapProductAttributesAndAttributeValues(STORE_ID, productData, attributeData,
        productSuitabilityAttributeModelList);
  }

  @Test
  public void createAndMapProductAttributesAndAttributeValuesPredefinedTest() throws Exception {
    List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList = new ArrayList<>();
    ProductSuitabilityAttributeModel productSuitabilityAttributeModel = new ProductSuitabilityAttributeModel();
    productSuitabilityAttributeModel.setAttributeCode(ATTRIBUTE_CODE);
    productSuitabilityAttributeModel.setValue(ATTRIBUTE_VALUE);
    productSuitabilityAttributeModel.setValueEn(ATTRIBUTE_VALUE_EN);
    productSuitabilityAttributeModelList.add(productSuitabilityAttributeModel);
    productData.setProductAttributes(new ArrayList<>(Collections.singletonList(productAttribute)));
    attributeData.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    when(predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(STORE_ID),
        eq(attributeData), eq(ATTRIBUTE_VALUE))).thenReturn(Collections.singletonList(originalBrandAttribute));
    productServiceWrapper.createAndMapProductAttributesAndAttributeValues(STORE_ID, productData, attributeData,
        productSuitabilityAttributeModelList);
    verify(predefinedAllowedAttributeValueService).findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(STORE_ID),
        eq(attributeData), eq(ATTRIBUTE_VALUE));
  }

  @Test
  public void createAndMapProductAttributesAndAttributeValuesMultiPredefinedTest() throws Exception {
    List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList = new ArrayList<>();
    ProductSuitabilityAttributeModel productSuitabilityAttributeModel = new ProductSuitabilityAttributeModel();
    productSuitabilityAttributeModel.setAttributeCode(ATTRIBUTE_CODE);
    productSuitabilityAttributeModel.setValue(ATTRIBUTE_VALUE);
    productSuitabilityAttributeModel.setValueEn(ATTRIBUTE_VALUE_EN);
    productSuitabilityAttributeModelList.add(productSuitabilityAttributeModel);
    productData.setProductAttributes(new ArrayList<>(Collections.singletonList(productAttribute)));
    attributeData.setAttributeType(AttributeType.PREDEFINED_MULTIVALUE);
    when(predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(STORE_ID),
        eq(attributeData), eq(ATTRIBUTE_VALUE))).thenReturn(Collections.singletonList(originalBrandAttribute));
    productServiceWrapper.createAndMapProductAttributesAndAttributeValues(STORE_ID, productData, attributeData,
        productSuitabilityAttributeModelList);
    verify(predefinedAllowedAttributeValueService).findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(STORE_ID),
        eq(attributeData), eq(ATTRIBUTE_VALUE));
  }

  @Test
  public void createAndMapProductAttributesAndAttributeValuesMultiDescriptiveTest() throws Exception {
    List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList = new ArrayList<>();
    ProductSuitabilityAttributeModel productSuitabilityAttributeModel = new ProductSuitabilityAttributeModel();
    productSuitabilityAttributeModel.setAttributeCode(ATTRIBUTE_CODE);
    productSuitabilityAttributeModel.setValue(ATTRIBUTE_VALUE);
    productSuitabilityAttributeModel.setValueEn(ATTRIBUTE_VALUE_EN);
    productSuitabilityAttributeModelList.add(productSuitabilityAttributeModel);
    productData.setProductAttributes(new ArrayList<>(Collections.singletonList(productAttribute)));
    attributeData.setAttributeType(AttributeType.DESCRIPTIVE_MULTIVALUE);
    productServiceWrapper.createAndMapProductAttributesAndAttributeValues(STORE_ID, productData, attributeData,
        productSuitabilityAttributeModelList);
  }


  @Test
  public void createAndMapProductAttributesAndAttributeValuesAllTest() throws Exception {
    List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList = new ArrayList<>();
    ProductSuitabilityAttributeModel productSuitabilityAttributeModel = new ProductSuitabilityAttributeModel();
    productSuitabilityAttributeModel.setAttributeCode(ATTRIBUTE_CODE);
    productSuitabilityAttributeModel.setValue(ATTRIBUTE_VALUE);
    productSuitabilityAttributeModel.setValueEn(ATTRIBUTE_VALUE_EN);
    productSuitabilityAttributeModelList.add(productSuitabilityAttributeModel);
    productData.setProductAttributes(new ArrayList<>(Collections.singletonList(productAttribute)));
    attributeData.setAttributeType(AttributeType.ALL);
    productServiceWrapper.createAndMapProductAttributesAndAttributeValues(STORE_ID, productData, attributeData,
        productSuitabilityAttributeModelList);
  }

  @Test
  public void processExistingPredefinedAttributeValuesTest() throws Exception {
    List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList = new ArrayList<>();
    ProductSuitabilityAttributeModel productSuitabilityAttributeModel = new ProductSuitabilityAttributeModel();
    productSuitabilityAttributeModel.setAttributeCode(ATTRIBUTE_CODE);
    productSuitabilityAttributeModel.setValue(ATTRIBUTE_VALUE);
    productSuitabilityAttributeModel.setValueEn(ATTRIBUTE_VALUE_EN);
    productSuitabilityAttributeModelList.add(productSuitabilityAttributeModel);

    when(predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(STORE_ID),
        eq(attributeData), eq(ATTRIBUTE_VALUE))).thenReturn(Collections.singletonList(originalBrandAttribute));
    productAttribute.setProductAttributeValues(new ArrayList<>(Collections.singletonList(productAttributeValue)));
    productServiceWrapper.processExistingPredefinedAttributeValues(STORE_ID, attributeData,
        productSuitabilityAttributeModelList, productAttribute);

    verify(predefinedAllowedAttributeValueService).findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(STORE_ID),
        eq(attributeData), eq(ATTRIBUTE_VALUE));
  }

  @Test
  public void processExistingPredefinedAttributeValuesSameAttributeValueTest() throws Exception {
    List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList = new ArrayList<>();
    ProductSuitabilityAttributeModel productSuitabilityAttributeModel = new ProductSuitabilityAttributeModel();
    productSuitabilityAttributeModel.setAttributeCode(ATTRIBUTE_CODE);
    productSuitabilityAttributeModel.setValue(ATTRIBUTE_VALUE);
    productSuitabilityAttributeModel.setValueEn(ATTRIBUTE_VALUE_EN);
    productSuitabilityAttributeModelList.add(productSuitabilityAttributeModel);
    originalBrandAttribute.setValue(ATTRIBUTE_VALUE);
    productServiceWrapper.processExistingPredefinedAttributeValues(STORE_ID, attributeData,
        productSuitabilityAttributeModelList, productAttribute);
  }

  @Test
  public void processExistingPredefinedAttributeValuesDifferentAttributeValueTest() throws Exception {
    List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList = new ArrayList<>();
    ProductSuitabilityAttributeModel productSuitabilityAttributeModel = new ProductSuitabilityAttributeModel();
    productSuitabilityAttributeModel.setAttributeCode(ATTRIBUTE_CODE);
    productSuitabilityAttributeModel.setValue(ATTRIBUTE_VALUE);
    productSuitabilityAttributeModel.setValueEn(ATTRIBUTE_VALUE_EN);
    productSuitabilityAttributeModelList.add(productSuitabilityAttributeModel);
    ProductAttributeValue productAttributeValue1 = new ProductAttributeValue();
    productAttributeValue1.setMarkForDelete(true);
    PredefinedAllowedAttributeValue originalBrandAttribute = new PredefinedAllowedAttributeValue();
    originalBrandAttribute.setValue(ATTRIBUTE_VALUE);
    productAttributeValue1.setPredefinedAllowedAttributeValue(originalBrandAttribute);
    productAttributeValue1.getPredefinedAllowedAttributeValue().setValue(ATTRIBUTE_VALUE);
    productAttribute.setProductAttributeValues(Arrays.asList(productAttributeValue, productAttributeValue1));
    productServiceWrapper.processExistingPredefinedAttributeValues(STORE_ID, attributeData,
        productSuitabilityAttributeModelList, productAttribute);
  }

  @Test
  public void processExistingPredefinedAttributeNullValuesTest() throws Exception {
    List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList = new ArrayList<>();
    ProductSuitabilityAttributeModel productSuitabilityAttributeModel = new ProductSuitabilityAttributeModel();
    productSuitabilityAttributeModel.setAttributeCode(ATTRIBUTE_CODE);
    productSuitabilityAttributeModelList.add(productSuitabilityAttributeModel);
    productServiceWrapper.processExistingPredefinedAttributeValues(STORE_ID, attributeData,
        productSuitabilityAttributeModelList, productAttribute);
    assertTrue(productData.getProductAttributes().get(0).isMarkForDelete());

  }

  @Test
  public void processExistingPredefinedMultivalueAttributesTest() throws Exception {
    List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList = new ArrayList<>();
    ProductSuitabilityAttributeModel productSuitabilityAttributeModel = new ProductSuitabilityAttributeModel();
    productSuitabilityAttributeModel.setAttributeCode(ATTRIBUTE_CODE);
    productSuitabilityAttributeModel.setValue(ATTRIBUTE_VALUE);
    productSuitabilityAttributeModel.setValueEn(ATTRIBUTE_VALUE_EN);
    productSuitabilityAttributeModelList.add(productSuitabilityAttributeModel);

    when(predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(STORE_ID),
        eq(attributeData), eq(ATTRIBUTE_VALUE))).thenReturn(Collections.singletonList(originalBrandAttribute));

    productAttribute.setProductAttributeValues(new ArrayList<>(Collections.singletonList(productAttributeValue)));
    productServiceWrapper.processExistingPredefinedMultiValueAttributes(STORE_ID, attributeData,
        productSuitabilityAttributeModelList, productAttribute);

    verify(predefinedAllowedAttributeValueService).findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(STORE_ID),
        eq(attributeData), eq(ATTRIBUTE_VALUE));
  }

  @Test
  public void processExistingPredefinedMultivalueAttributesNullTest() throws Exception {
    List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList = new ArrayList<>();
    ProductSuitabilityAttributeModel productSuitabilityAttributeModel = new ProductSuitabilityAttributeModel();
    productSuitabilityAttributeModel.setAttributeCode(ATTRIBUTE_CODE);
    productSuitabilityAttributeModelList.add(productSuitabilityAttributeModel);
    productAttribute.setProductAttributeValues(new ArrayList<>(Collections.singletonList(productAttributeValue)));
    productServiceWrapper.processExistingPredefinedMultiValueAttributes(STORE_ID, attributeData,
        productSuitabilityAttributeModelList, productAttribute);
    Assertions.assertTrue(productData.getProductAttributes().get(0).isMarkForDelete());
  }

  @Test
  public void processPredefinedMultiValueAttributesTest() throws Exception {
    ProductAttribute productAttribute = new ProductAttribute();
    Attribute attribute = new Attribute();
    attribute.setAttributeType(AttributeType.PREDEFINED_MULTIVALUE);
    List<ProductSuitabilityAttributeModel> attributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel attributeModel = new ProductSuitabilityAttributeModel();
    attributeModel.setValue(ATTRIBUTE_VALUE);
    attributeModel.setValueEn(ATTRIBUTE_VALUE_EN);
    attributeModels.add(attributeModel);
    when(predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(any(), any(),
        any())).thenReturn(Collections.emptyList());
    when(predefinedAllowedAttributeValueService.addPredefinedAllowedAttributeValue(any(), any(), any(), any(),
        anyInt(), any())).thenReturn(new PredefinedAllowedAttributeValue());
    productServiceWrapper.processPredefinedMultiValueAttributes(STORE_ID, attribute, attributeModels, productAttribute,
        0);
    verify(predefinedAllowedAttributeValueService).findByStoreIdAndAttributeAndValueOrderByMarkForDelete(any(), any(),
        any());
    verify(predefinedAllowedAttributeValueService).addPredefinedAllowedAttributeValue(any(), any(), any(), any(),
        anyInt(), any());
  }



  @Test
  public void processPredefinedAttributesTest() throws Exception {
    ProductAttribute productAttribute = new ProductAttribute();
    Attribute attribute = new Attribute();
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    List<ProductSuitabilityAttributeModel> attributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel attributeModel = new ProductSuitabilityAttributeModel();
    attributeModel.setValue(ATTRIBUTE_VALUE);
    attributeModel.setValueEn(ATTRIBUTE_VALUE_EN);
    attributeModels.add(attributeModel);
    when(predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(any(), any(),
        any())).thenReturn(Collections.emptyList());
    when(predefinedAllowedAttributeValueService.addPredefinedAllowedAttributeValue(any(), any(), any(), any(),
        anyInt(), any())).thenReturn(new PredefinedAllowedAttributeValue());
    productServiceWrapper.processPredefinedAttributes(STORE_ID, attribute, attributeModels, productAttribute);
    verify(predefinedAllowedAttributeValueService).findByStoreIdAndAttributeAndValueOrderByMarkForDelete(any(), any(),
        any());
    verify(predefinedAllowedAttributeValueService).addPredefinedAllowedAttributeValue(any(), any(), any(), any(),
        anyInt(), any());
  }

  @Test
  public void generateProductAttributeValueTest() throws Exception {
    ProductAttribute productAttribute = new ProductAttribute();
    Attribute attribute = new Attribute();
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    when(predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(any(), any(),
        any())).thenReturn(Collections.emptyList());
    when(predefinedAllowedAttributeValueService.addPredefinedAllowedAttributeValue(any(), any(), any(), any(),
        anyInt(), any())).thenReturn(new PredefinedAllowedAttributeValue());
    ProductAttributeValue result =
        productServiceWrapper.generateProductAttributeValue(STORE_ID, productAttribute, attribute, ATTRIBUTE_VALUE,
            ATTRIBUTE_VALUE_EN, 0);
    assertNotNull(result);
    verify(predefinedAllowedAttributeValueService).findByStoreIdAndAttributeAndValueOrderByMarkForDelete(any(), any(),
        any());
    verify(predefinedAllowedAttributeValueService).addPredefinedAllowedAttributeValue(any(), any(), any(), any(),
        anyInt(), any());
  }

  @Test
  public void getCategoryAttributeCodesTest() {
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCategoryId(CATEGORY_ID);
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    categoryAttribute.setAttribute(attribute);
    when(categoryService.getCategoryAttributes(STORE_ID, CATEGORY_ID)).thenReturn(
        Collections.singletonList(categoryAttribute));
    List<String> result = productServiceWrapper.getCategoryAttributeCodes(STORE_ID, productCategory);
    assertNotNull(result);
    assertEquals(1, result.size());
    assertEquals(ATTRIBUTE_CODE, result.get(0));
    verify(categoryService).getCategoryAttributes(STORE_ID, CATEGORY_ID);
  }

  @Test
  public void processProductDsAttributeMappingTest_NullProduct() throws Exception {
    ProductSuitabilityEventModel productSuitabilityEventModel = new ProductSuitabilityEventModel();
    productSuitabilityEventModel.setStoreId(STORE_ID);
    productSuitabilityEventModel.setProductId(PRODUCT_CODE);
    productSuitabilityEventModel.setAttributeValueExtractions(new ArrayList<>());
    when(productService.getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE)).thenReturn(
        null);
    productServiceWrapper.validateAndProcessProductDsAttributeMapping(productSuitabilityEventModel);
    verify(productService).getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, null, true);
    verifyNoMoreInteractions(productService);
  }

  @Test
  public void processProductDsAttributeMappingTest_NullProductCategory() throws Exception {
    ProductSuitabilityEventModel productSuitabilityEventModel = new ProductSuitabilityEventModel();
    productSuitabilityEventModel.setStoreId(STORE_ID);
    productSuitabilityEventModel.setProductId(PRODUCT_CODE);
    productSuitabilityEventModel.setAttributeValueExtractions(new ArrayList<>());
    Product product = new Product();
    product.setProductCategories(new ArrayList<>());
    when(productService.getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE)).thenReturn(
        product);
    productServiceWrapper.validateAndProcessProductDsAttributeMapping(productSuitabilityEventModel);
    verify(productService).getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, product, true);
    verifyNoMoreInteractions(productService);
  }

  @Test
  public void processProductDsAttributeMappingTest_NewAttribute_Descriptive() throws Exception {
    ProductSuitabilityEventModel productSuitabilityEventModel = new ProductSuitabilityEventModel();
    productSuitabilityEventModel.setStoreId(STORE_ID);
    productSuitabilityEventModel.setProductId(PRODUCT_CODE);
    ProductSuitabilityAttributeModel attributeModel = new ProductSuitabilityAttributeModel();
    attributeModel.setAttributeCode(ATTRIBUTE_CODE);
    attributeModel.setValue(ATTRIBUTE_VALUE);
    attributeModel.setValueEn(ATTRIBUTE_VALUE_EN);
    productSuitabilityEventModel.setAttributeValueExtractions(Collections.singletonList(attributeModel));
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCategoryId(CATEGORY_ID);
    productData.setProductCategories(Collections.singletonList(productCategory));
    productData.setProductAttributes(new ArrayList<>());
    attributeData.setId(ATTRIBUTE_ID);
    attributeData.setAttributeCode(ATTRIBUTE_CODE);
    attributeData.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attributeData.setDsExtraction(true);
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(attributeData);
    when(productService.getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE)).thenReturn(
        productData);
    when(attributeService.findAttributesByStoreIdAndAttributeCodeList(STORE_ID,
        Collections.singletonList(ATTRIBUTE_CODE))).thenReturn(Collections.singletonList(attributeData));
    when(categoryService.getCategoryAttributes(STORE_ID, CATEGORY_ID)).thenReturn(List.of(categoryAttribute));
    productServiceWrapper.validateAndProcessProductDsAttributeMapping(productSuitabilityEventModel);
    verify(productService).getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    verify(attributeService).findAttributesByStoreIdAndAttributeCodeList(STORE_ID,
        Collections.singletonList(ATTRIBUTE_CODE));
    verify(categoryService).getCategoryAttributes(STORE_ID, CATEGORY_ID);
    verify(productService).saveAndFlush(productData);
    verify(productService).evictAllProductDetailCache(STORE_ID, productData);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, productData, true);
    verify(domainEventPublisherService).publishProductChangeCategory(productData, null, false, false, false, false,
        new HashSet<>());
  }

  @Test
  public void processProductDsAttributeMappingTest_NewAttribute_DescriptiveSameValue() throws Exception {
    ProductSuitabilityEventModel productSuitabilityEventModel = new ProductSuitabilityEventModel();
    productSuitabilityEventModel.setStoreId(STORE_ID);
    productSuitabilityEventModel.setProductId(PRODUCT_CODE);
    ProductSuitabilityAttributeModel attributeModel = new ProductSuitabilityAttributeModel();
    attributeModel.setAttributeCode(ATTRIBUTE_CODE);
    attributeModel.setValue(ATTRIBUTE_VALUE);
    attributeModel.setValueEn(ATTRIBUTE_VALUE_EN);
    productSuitabilityEventModel.setAttributeValueExtractions(Collections.singletonList(attributeModel));
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCategoryId(CATEGORY_ID);
    productData.setProductCategories(Collections.singletonList(productCategory));
    productData.setProductAttributes(new ArrayList<>());
    attributeData.setId(ATTRIBUTE_ID);
    attributeData.setAttributeCode(ATTRIBUTE_CODE);
    attributeData.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attributeData.setDsExtraction(true);
    productAttributeValue.setDescriptiveAttributeValue(ATTRIBUTE_VALUE);
    productAttribute.setAttributeId(ATTRIBUTE_ID);
    productData.setProductAttributes(List.of(productAttribute));
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(attributeData);
    when(productService.getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE)).thenReturn(
        productData);
    when(attributeService.findAttributesByStoreIdAndAttributeCodeList(STORE_ID,
        Collections.singletonList(ATTRIBUTE_CODE))).thenReturn(Collections.singletonList(attributeData));
    when(categoryService.getCategoryAttributes(STORE_ID, CATEGORY_ID)).thenReturn(List.of(categoryAttribute));
    productServiceWrapper.validateAndProcessProductDsAttributeMapping(productSuitabilityEventModel);
    verify(productService).getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    verify(attributeService).findAttributesByStoreIdAndAttributeCodeList(STORE_ID,
        Collections.singletonList(ATTRIBUTE_CODE));
    verify(categoryService).getCategoryAttributes(STORE_ID, CATEGORY_ID);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, productData, true);
  }

  @Test
  public void processProductDsAttributeMappingTest_ExistingAttribute_Descriptive() throws Exception {
    ProductSuitabilityEventModel productSuitabilityEventModel = new ProductSuitabilityEventModel();
    productSuitabilityEventModel.setStoreId(STORE_ID);
    productSuitabilityEventModel.setProductId(PRODUCT_CODE);
    ProductSuitabilityAttributeModel attributeModel = new ProductSuitabilityAttributeModel();
    attributeModel.setAttributeCode(ATTRIBUTE_CODE);
    attributeModel.setValue(ATTRIBUTE_VALUE);
    attributeModel.setValueEn(ATTRIBUTE_VALUE_EN);
    productSuitabilityEventModel.setAttributeValueExtractions(Collections.singletonList(attributeModel));
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCategoryId(CATEGORY_ID);
    productData.setProductCategories(Collections.singletonList(productCategory));
    attributeData.setId(ATTRIBUTE_ID);
    attributeData.setAttributeCode(ATTRIBUTE_CODE);
    attributeData.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attributeData.setDsExtraction(true);
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(attributeData);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttributeId(ATTRIBUTE_ID);
    productAttribute.setProductAttributeValues(new ArrayList<>());
    productData.setProductAttributes(Collections.singletonList(productAttribute));
    when(productService.getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE)).thenReturn(
        productData);
    when(attributeService.findAttributesByStoreIdAndAttributeCodeList(STORE_ID,
        Collections.singletonList(ATTRIBUTE_CODE))).thenReturn(Collections.singletonList(attributeData));
    when(categoryService.getCategoryAttributes(STORE_ID, CATEGORY_ID)).thenReturn(List.of(categoryAttribute));
    productServiceWrapper.validateAndProcessProductDsAttributeMapping(productSuitabilityEventModel);
    verify(productService).getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    verify(attributeService).findAttributesByStoreIdAndAttributeCodeList(STORE_ID,
        Collections.singletonList(ATTRIBUTE_CODE));
    verify(categoryService).getCategoryAttributes(STORE_ID, CATEGORY_ID);
    verify(productService).saveAndFlush(productData);
    verify(productService).evictAllProductDetailCache(STORE_ID, productData);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, productData, true);
    verify(domainEventPublisherService).publishProductChangeCategory(productData, null, false, false, false, false,
        new HashSet<>());
  }

  @Test
  public void createAndMapProductAttributeValues_DescriptiveAttributeTest() throws Exception {
    List<ProductSuitabilityAttributeModel> attributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel attributeModel = new ProductSuitabilityAttributeModel();
    attributeModel.setValue(ATTRIBUTE_VALUE);
    attributeModels.add(attributeModel);
    List<ProductAttribute> productAttributes = new ArrayList<>();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttributeId(attributeData.getId());
    productAttribute.setProductAttributeValues(new ArrayList<>());
    productAttributes.add(productAttribute);
    attributeData.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    productServiceWrapper.createAndMapProductAttributeValues(STORE_ID, productAttribute, attributeData, attributeModels);
    assertFalse(productAttribute.isMarkForDelete());
  }

  @Test
  public void createAndMapProductAttributeValues_PredefinedAttributeTest() throws Exception {
    List<ProductSuitabilityAttributeModel> attributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel attributeModel = new ProductSuitabilityAttributeModel();
    attributeModel.setValue(ATTRIBUTE_VALUE);
    attributeModels.add(attributeModel);
    List<ProductAttribute> productAttributes = new ArrayList<>();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttributeId(attributeData.getId());
    productAttributes.add(productAttribute);
    attributeData.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    when(predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(STORE_ID),
        eq(attributeData), eq(ATTRIBUTE_VALUE))).thenReturn(Collections.singletonList(originalBrandAttribute));
    productServiceWrapper.createAndMapProductAttributeValues(STORE_ID, productAttribute, attributeData, attributeModels);
    verify(predefinedAllowedAttributeValueService).findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(STORE_ID),
        eq(attributeData), eq(ATTRIBUTE_VALUE));
  }

  @Test
  public void createAndMapProductAttributeValues_DescriptiveMultiValueTest() throws Exception {
    List<ProductSuitabilityAttributeModel> attributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel attributeModel = new ProductSuitabilityAttributeModel();
    attributeModel.setValue(ATTRIBUTE_VALUE);
    attributeModels.add(attributeModel);
    List<ProductAttribute> productAttributes = new ArrayList<>();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttributeId(attributeData.getId());
    productAttributes.add(productAttribute);
    attributeData.setAttributeType(AttributeType.DESCRIPTIVE_MULTIVALUE);
    productServiceWrapper.createAndMapProductAttributeValues(STORE_ID, productAttribute, attributeData, attributeModels);
  }

  @Test
  public void createAndMapProductAttributeValues_PredefinedMultiValueTest() throws Exception {
    List<ProductSuitabilityAttributeModel> attributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel attributeModel = new ProductSuitabilityAttributeModel();
    attributeModel.setValue(ATTRIBUTE_VALUE);
    attributeModels.add(attributeModel);
    List<ProductAttribute> productAttributes = new ArrayList<>();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttributeId(attributeData.getId());
    productAttributes.add(productAttribute);
    attributeData.setAttributeType(AttributeType.PREDEFINED_MULTIVALUE);
    when(predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(STORE_ID),
        eq(attributeData), eq(ATTRIBUTE_VALUE))).thenReturn(Collections.singletonList(originalBrandAttribute));
    productServiceWrapper.createAndMapProductAttributeValues(STORE_ID, productAttribute, attributeData, attributeModels);
    verify(predefinedAllowedAttributeValueService).findByStoreIdAndAttributeAndValueOrderByMarkForDelete(eq(STORE_ID),
        eq(attributeData), eq(ATTRIBUTE_VALUE));
  }

  @Test
  public void createAndMapProductAttributeValues_DefaultCaseTest() throws Exception {
    List<ProductSuitabilityAttributeModel> attributeModels = new ArrayList<>();
    ProductSuitabilityAttributeModel attributeModel = new ProductSuitabilityAttributeModel();
    attributeModel.setValue(ATTRIBUTE_VALUE);
    attributeModels.add(attributeModel);
    List<ProductAttribute> productAttributes = new ArrayList<>();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttributeId(attributeData.getId());
    productAttributes.add(productAttribute);
    attributeData.setAttributeType(AttributeType.ALL);
    productServiceWrapper.createAndMapProductAttributeValues(STORE_ID, productAttribute, attributeData, attributeModels);
  }

  @Test
  public void deleteMfdTrueImagesAndAttributes_Success() {
    productServiceWrapper.deleteMfdTrueImagesAndAttributes(commonImageBackfillingEventModel);
    verify(schedulerService).updateProductMigrationStatus(
        eq(STORE_ID),
        eq(null),
        eq(CommonUtil.getProductMigrationRequest(commonImageBackfillingEventModel, null, ProductMigrationStatus.IN_PROGRESS))
    );
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(eq(STORE_ID), eq(productData), eq(true));
    verify(productService).deleteMfdTrueRowsFromProduct(productData);
    verify(productService).evictAllProductDetailCache(STORE_ID, productData);
  }

  @Test
  public void deleteMfdTrueImagesAndAttributes_ProductNotFound() {
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE))
        .thenReturn(null);
    assertThrows(ApplicationRuntimeException.class, () ->
        productServiceWrapper.deleteMfdTrueImagesAndAttributes(commonImageBackfillingEventModel)
    );
    verify(schedulerService).updateProductMigrationStatus(
        eq(STORE_ID),
        eq(null),
        eq(CommonUtil.getProductMigrationRequest(commonImageBackfillingEventModel, null, ProductMigrationStatus.IN_PROGRESS))
    );
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verifyNoMoreInteractions(productService);
  }


  @Test
  public void testProcessCompressedUpdatedVideo_WithValidData_NoExistingVideo() throws Exception {
    productData.setVideo(null);
    when(productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(
      eq(DEFAULT_STORE_ID), eq(PRODUCT_CODE)))
      .thenReturn(productData);

    doNothing().when(productService).processCompressedUpdatedVideo(
      anyString(), any(VideoDTO.class));

    doNothing().when(productService).evictAllProductDetailCache(
      anyString(), any(Product.class));
    productServiceWrapper.processCompressedUpdatedVideo(compressedVideoUpdateEventModel);
    verify(productRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
      DEFAULT_STORE_ID, PRODUCT_CODE);

    verify(productService).processCompressedUpdatedVideo(
      eq(PRODUCT_CODE), videoDTOCaptor.capture());
    VideoDTO capturedVideoDTO = videoDTOCaptor.getValue();
    assertNotNull(capturedVideoDTO);
    assertEquals(VIDEO_ID, capturedVideoDTO.getVideoId());
    assertEquals(FINAL_URL, capturedVideoDTO.getFinalUrl());
    assertEquals(COVER_IMAGE_PATH, capturedVideoDTO.getCoverImagePath());
    assertEquals(TEST_VIDEO, capturedVideoDTO.getVideoName());
    assertEquals(SOURCE_URL, capturedVideoDTO.getSourceUrl());

    verify(productService).evictAllProductDetailCache(
      eq(DEFAULT_STORE_ID), eq(productData));
  }

  @Test
  public void testProcessCompressedUpdatedVideo_WithValidData_WithExistingVideo() throws Exception {
    String existingVideoJson = String.format(VIDEO_JSON, VIDEO_ID);
    productData.setVideo(existingVideoJson);
    when(productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(
      eq(DEFAULT_STORE_ID), eq(PRODUCT_CODE)))
      .thenReturn(productData);

    doNothing().when(productService).processCompressedUpdatedVideo(
      anyString(), any(VideoDTO.class));

    doNothing().when(productService).evictAllProductDetailCache(
      anyString(), any(Product.class));
    productServiceWrapper.processCompressedUpdatedVideo(compressedVideoUpdateEventModel);
    verify(productRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
      DEFAULT_STORE_ID, PRODUCT_CODE);

    verify(productService).processCompressedUpdatedVideo(
      eq(PRODUCT_CODE), videoDTOCaptor.capture());
    VideoDTO capturedVideoDTO = videoDTOCaptor.getValue();
    assertNotNull(capturedVideoDTO);
    assertEquals(FINAL_URL, capturedVideoDTO.getFinalUrl());

    verify(productService).evictAllProductDetailCache(
      eq(DEFAULT_STORE_ID), eq(productData));
  }

  @Test
  public void testProcessCompressedUpdatedVideo_WithMissingProductCode() {
    compressedVideoUpdateEventModel.setAdditionalFields(Map.of(Constants.PRODUCT_CODE, StringUtils.EMPTY));

    ApplicationRuntimeException exception = Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productServiceWrapper.processCompressedUpdatedVideo(compressedVideoUpdateEventModel);
    });
    assertTrue(Objects.nonNull(exception));
  }

  @Test
  public void testProcessCompressedUpdatedVideo_WithMissingVideoId() {
    compressedVideoUpdateEventModel.setVideoId(null);
    ApplicationRuntimeException exception = Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productServiceWrapper.processCompressedUpdatedVideo(compressedVideoUpdateEventModel);
    });
    assertTrue(Objects.nonNull(exception));
  }

  @Test
  public void testProcessCompressedUpdatedVideo_WithMissingFinalUrl() {
    compressedVideoUpdateEventModel.setFinalUrl(null);
    ApplicationRuntimeException exception = Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productServiceWrapper.processCompressedUpdatedVideo(compressedVideoUpdateEventModel);
    });
    assertTrue(Objects.nonNull(exception));
  }

  @Test
  public void testProcessCompressedUpdatedVideo_ProductNotFound() {
    when(productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(
      eq(DEFAULT_STORE_ID), eq(PRODUCT_CODE)))
      .thenReturn(null);
    ApplicationRuntimeException exception = Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productServiceWrapper.processCompressedUpdatedVideo(compressedVideoUpdateEventModel);
    });
    assertTrue(Objects.nonNull(exception));
    assertEquals(PRODUCT_CODE_NOT_FOUND_ERROR, exception.getMessage());
  }


  @Test
  public void testProcessCompressedUpdatedVideo_HandlesOptionalFields() throws Exception {
    compressedVideoUpdateEventModel.setCoverImagePath(null);
    compressedVideoUpdateEventModel.setVideoName(null);
    compressedVideoUpdateEventModel.setSourceUrl(null);
    productData.setVideo(null);
    when(productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(
      eq(DEFAULT_STORE_ID), eq(PRODUCT_CODE)))
      .thenReturn(productData);

    doNothing().when(productService).processCompressedUpdatedVideo(
      anyString(), any(VideoDTO.class));

    doNothing().when(productService).evictAllProductDetailCache(
      anyString(), any(Product.class));

    productServiceWrapper.processCompressedUpdatedVideo(compressedVideoUpdateEventModel);
    verify(productRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
      DEFAULT_STORE_ID, PRODUCT_CODE);

    verify(productService).processCompressedUpdatedVideo(
      eq(PRODUCT_CODE), videoDTOCaptor.capture());
    VideoDTO capturedVideoDTO = videoDTOCaptor.getValue();
    assertNotNull(capturedVideoDTO);
    assertEquals(VIDEO_ID, capturedVideoDTO.getVideoId());
    assertEquals(FINAL_URL, capturedVideoDTO.getFinalUrl());
    assertEquals("", capturedVideoDTO.getCoverImagePath());
    assertEquals("", capturedVideoDTO.getVideoName());
    assertEquals("", capturedVideoDTO.getSourceUrl());

    verify(productService).evictAllProductDetailCache(
      eq(DEFAULT_STORE_ID), eq(productData));
  }

  @Test
  void processCompressedUpdatedVideoClientIdIgnoreTest() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "clientIdForVideoCompression", "pcb");
    productServiceWrapper.processCompressedUpdatedVideo(compressedVideoUpdateEventModel);
    verify(productRepository, never()).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
      DEFAULT_STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void deleteMfdTrueImagesAndAttributes_WhenBothFlagsTrue_ShouldSkipDeletion() {
    // Setup
    ReflectionTestUtils.setField(productServiceWrapper, "skipMfdTrueProductsForDeletion", true);
    CommonImageBackfillingEventModel eventModel = new CommonImageBackfillingEventModel();
    eventModel.setStoreId(STORE_ID);
    eventModel.setProductCode(PRODUCT_CODE);

    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setMarkForDelete(true);

    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE))
        .thenReturn(product);

    // Execute
    productServiceWrapper.deleteMfdTrueImagesAndAttributes(eventModel);

    // Verify
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(schedulerService, times(2)).updateProductMigrationStatus(eq(STORE_ID), any(), any());
    verifyNoMoreInteractions(productService);
  }

  @Test
  public void deleteMfdTrueImagesAndAttributes_WhenBothFlagsFalse_ShouldProcessDeletion() {
    // Setup
    ReflectionTestUtils.setField(productServiceWrapper, "skipMfdTrueProductsForDeletion", false);
    CommonImageBackfillingEventModel eventModel = new CommonImageBackfillingEventModel();
    eventModel.setStoreId(STORE_ID);
    eventModel.setProductCode(PRODUCT_CODE);

    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setMarkForDelete(true);

    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE))
        .thenReturn(product);
    doNothing().when(productService).setCompleteProductDetailsCached(STORE_ID, product, true);
    doNothing().when(productService).deleteMfdTrueRowsFromProduct(product);
    doNothing().when(productService).evictAllProductDetailCache(STORE_ID, product);

    // Execute
    productServiceWrapper.deleteMfdTrueImagesAndAttributes(eventModel);

    // Verify
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).setCompleteProductDetailsCached(STORE_ID, product, true);
    verify(productService).deleteMfdTrueRowsFromProduct(product);
    verify(productService).evictAllProductDetailCache(STORE_ID, product);
    verify(schedulerService, times(2)).updateProductMigrationStatus(eq(STORE_ID), any(), any());
  }

  @Test
  public void updateMasterDataAndEvictCache_Success() throws Exception {
    // Given
    String sizeChartCode = "SIZE_CHART_001";
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setSizeChartCode(sizeChartCode);
    request.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    request.setCategoryCode(CATEGORY_CODE);
    request.setName("Updated Product Name");

    AuditTrailListResponse auditTrailResponse = new AuditTrailListResponse();

    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE))
        .thenReturn(productData);
    doNothing().when(sizeChartService).validateSizeChartForBusinessPartnerCodeAndCategoryCode(
        STORE_ID, sizeChartCode, BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    when(productService.updateMasterDataAndGenerateHistory(STORE_ID, USERNAME, request, productData))
        .thenReturn(auditTrailResponse);
    doNothing().when(productService).evictProductCache(STORE_ID, productData);
    when(kafkaTopicProperties.getProductSkuUpdateExternalHistoryEvent()).thenReturn("product-sku-update-event");
    doNothing().when(kafkaPublisher).send("product-sku-update-event", auditTrailResponse);

    // When
    productServiceWrapper.updateMasterDataAndEvictCache(STORE_ID, USERNAME, request);

    // Then
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(sizeChartService).validateSizeChartForBusinessPartnerCodeAndCategoryCode(
        STORE_ID, sizeChartCode, BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    verify(productService).updateMasterDataAndGenerateHistory(STORE_ID, USERNAME, request, productData);
    verify(productService).saveAndFlush(productData);
    verify(productService).evictProductCache(STORE_ID, productData);
    verify(kafkaTopicProperties).getProductSkuUpdateExternalHistoryEvent();
    verify(kafkaPublisher).send("product-sku-update-event", null, auditTrailResponse);
  }

  @Test
  public void updateMasterDataAndEvictCacheProductNameUpdate_Success() throws Exception {
    // Given
    String sizeChartCode = "SIZE_CHART_001";
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setSizeChartCode(sizeChartCode);
    request.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    request.setCategoryCode(CATEGORY_CODE);
    request.setName("Updated Product Name");

    AuditTrailListResponse auditTrailResponse = new AuditTrailListResponse();
    AuditTrailDto auditTrailDto = new AuditTrailDto();
    auditTrailDto.setActionKey(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_NAME);
    auditTrailResponse.setAuditTrailResponseList(List.of(auditTrailDto));

    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE))
        .thenReturn(productData);
    doNothing().when(sizeChartService).validateSizeChartForBusinessPartnerCodeAndCategoryCode(
        STORE_ID, sizeChartCode, BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    when(productService.updateMasterDataAndGenerateHistory(STORE_ID, USERNAME, request, productData))
        .thenReturn(auditTrailResponse);
    doNothing().when(productService).evictProductCache(STORE_ID, productData);
    when(kafkaTopicProperties.getProductSkuUpdateExternalHistoryEvent()).thenReturn("product-sku-update-event");
    doNothing().when(kafkaPublisher).send("product-sku-update-event", auditTrailResponse);

    // When
    productServiceWrapper.updateMasterDataAndEvictCache(STORE_ID, USERNAME, request);

    // Then
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(sizeChartService).validateSizeChartForBusinessPartnerCodeAndCategoryCode(
        STORE_ID, sizeChartCode, BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    verify(productService).updateMasterDataAndGenerateHistory(STORE_ID, USERNAME, request, productData);
    verify(productService).saveAndFlush(productData);
    verify(productService).evictProductCache(STORE_ID, productData);
    verify(productService).evictProductItemCache(productData);
    verify(kafkaTopicProperties).getProductSkuUpdateExternalHistoryEvent();
    verify(kafkaPublisher).send("product-sku-update-event", null, auditTrailResponse);
  }

  @Test
  public void updateMasterDataAndEvictCache_SuccessWithCommonImages() throws Exception {
    // Given
    String sizeChartCode = "SIZE_CHART_001";
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setSizeChartCode(sizeChartCode);
    request.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    request.setCategoryCode(CATEGORY_CODE);
    request.setName("Updated Product Name");
    request.setCommonImages(List.of(ProductImageEditRequest.builder().build()));

    AuditTrailListResponse auditTrailResponse = new AuditTrailListResponse();

    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE))
        .thenReturn(productData);
    doNothing().when(sizeChartService).validateSizeChartForBusinessPartnerCodeAndCategoryCode(
        STORE_ID, sizeChartCode, BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    when(productService.updateMasterDataAndGenerateHistory(STORE_ID, USERNAME, request, productData))
        .thenReturn(auditTrailResponse);
    doNothing().when(productService).evictProductCache(STORE_ID, productData);
    when(kafkaTopicProperties.getProductSkuUpdateExternalHistoryEvent()).thenReturn("product-sku-update-event");
    doNothing().when(kafkaPublisher).send("product-sku-update-event", auditTrailResponse);

    // When
    productServiceWrapper.updateMasterDataAndEvictCache(STORE_ID, USERNAME, request);

    // Then
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(sizeChartService).validateSizeChartForBusinessPartnerCodeAndCategoryCode(
        STORE_ID, sizeChartCode, BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    verify(productService).updateMasterDataAndGenerateHistory(STORE_ID, USERNAME, request, productData);
    verify(productService).saveAndFlush(productData);
    verify(productService).evictProductCache(STORE_ID, productData);
    verify(domainEventPublisherService).publishProduct(productData);
    verify(kafkaTopicProperties).getProductSkuUpdateExternalHistoryEvent();
    verify(kafkaPublisher).send("product-sku-update-event", null, auditTrailResponse);
  }

  @Test
  public void updateMasterDataAndEvictCache_WithoutSizeChart_Success() throws Exception {
    // Given
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setName("Updated Product Name");

    AuditTrailListResponse auditTrailResponse = new AuditTrailListResponse();

    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE))
        .thenReturn(productData);
    when(productService.updateMasterDataAndGenerateHistory(STORE_ID, USERNAME, request, productData))
        .thenReturn(auditTrailResponse);
    doNothing().when(productService).evictProductCache(STORE_ID, productData);
    when(kafkaTopicProperties.getProductSkuUpdateExternalHistoryEvent()).thenReturn("product-sku-update-event");
    doNothing().when(kafkaPublisher).send("product-sku-update-event", auditTrailResponse);

    // When
    productServiceWrapper.updateMasterDataAndEvictCache(STORE_ID, USERNAME, request);

    // Then
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(sizeChartService, never()).validateSizeChartForBusinessPartnerCodeAndCategoryCode(
        anyString(), anyString(), anyString(), anyString());
    verify(productService).updateMasterDataAndGenerateHistory(STORE_ID, USERNAME, request, productData);
    verify(productService).saveAndFlush(productData);
    verify(productService).evictProductCache(STORE_ID, productData);
    verify(kafkaTopicProperties).getProductSkuUpdateExternalHistoryEvent();
    verify(kafkaPublisher).send("product-sku-update-event", null, auditTrailResponse);
  }

  @Test
  public void updateMasterDataAndEvictCache_WithNullAuditTrailResponse_NoKafkaPublish() throws Exception {
    // Given
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setName("Updated Product Name");

    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE))
        .thenReturn(productData);
    when(productService.updateMasterDataAndGenerateHistory(STORE_ID, USERNAME, request, productData))
        .thenReturn(null);
    doNothing().when(productService).evictProductCache(STORE_ID, productData);

    // When
    productServiceWrapper.updateMasterDataAndEvictCache(STORE_ID, USERNAME, request);

    // Then
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productService).updateMasterDataAndGenerateHistory(STORE_ID, USERNAME, request, productData);
    verify(productService).saveAndFlush(productData);
    verify(productService).evictProductCache(STORE_ID, productData);
    verify(kafkaTopicProperties, never()).getProductSkuUpdateExternalHistoryEvent();
    verify(kafkaPublisher, never()).send(anyString(), any());
  }

  @Test
  public void updateMasterDataAndEvictCache_WithEmptySizeChartCode_NoValidation() throws Exception {
    // Given
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setSizeChartCode(""); // Empty string
    request.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    request.setCategoryCode(CATEGORY_CODE);
    request.setName("Updated Product Name");

    AuditTrailListResponse auditTrailResponse = new AuditTrailListResponse();

    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE))
        .thenReturn(productData);
    when(productService.updateMasterDataAndGenerateHistory(STORE_ID, USERNAME, request, productData))
        .thenReturn(auditTrailResponse);
    doNothing().when(productService).evictProductCache(STORE_ID, productData);
    when(kafkaTopicProperties.getProductSkuUpdateExternalHistoryEvent()).thenReturn("product-sku-update-event");
    doNothing().when(kafkaPublisher).send("product-sku-update-event", auditTrailResponse);

    // When
    productServiceWrapper.updateMasterDataAndEvictCache(STORE_ID, USERNAME, request);

    // Then
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(sizeChartService, never()).validateSizeChartForBusinessPartnerCodeAndCategoryCode(
        anyString(), anyString(), anyString(), anyString());
    verify(productService).updateMasterDataAndGenerateHistory(STORE_ID, USERNAME, request, productData);
    verify(productService).saveAndFlush(productData);
    verify(productService).evictProductCache(STORE_ID, productData);
    verify(kafkaTopicProperties).getProductSkuUpdateExternalHistoryEvent();
    verify(kafkaPublisher).send("product-sku-update-event", null, auditTrailResponse);
  }

  @Test
  public void updateMasterDataAndEvictCache_ProductNotFound_ThrowsException() throws Exception {
    // Given
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setName("Updated Product Name");

    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE))
        .thenReturn(null);

    // When & Then
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class, () -> {
      productServiceWrapper.updateMasterDataAndEvictCache(STORE_ID, USERNAME, request);
    });

    // Verify only getProduct was called
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(sizeChartService, never()).validateSizeChartForBusinessPartnerCodeAndCategoryCode(
        anyString(), anyString(), anyString(), anyString());
    verify(productService, never()).updateMasterDataAndGenerateHistory(anyString(), anyString(), any(), any());
    verify(productService, never()).evictProductCache(anyString(), any());
    verify(kafkaPublisher, never()).send(anyString(), any());
  }

  @Test
  public void updateMasterDataAndEvictCache_SizeChartValidationThrowsException() throws Exception {
    // Given
    String sizeChartCode = "SIZE_CHART_001";
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setSizeChartCode(sizeChartCode);
    request.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    request.setCategoryCode(CATEGORY_CODE);
    request.setName("Updated Product Name");

    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE))
        .thenReturn(productData);
    doThrow(new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, "Invalid size chart"))
        .when(sizeChartService).validateSizeChartForBusinessPartnerCodeAndCategoryCode(
            STORE_ID, sizeChartCode, BUSINESS_PARTNER_CODE, CATEGORY_CODE);

    // When & Then
    assertThrows(ApplicationRuntimeException.class, () -> {
      productServiceWrapper.updateMasterDataAndEvictCache(STORE_ID, USERNAME, request);
    });

    // Then
    verify(productService, never()).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(sizeChartService).validateSizeChartForBusinessPartnerCodeAndCategoryCode(
        STORE_ID, sizeChartCode, BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    verify(productService, never()).updateMasterDataAndGenerateHistory(anyString(), anyString(), any(), any());
    verify(productService, never()).evictProductCache(anyString(), any());
    verify(kafkaPublisher, never()).send(anyString(), any());
  }

  @Test
  public void processExistingPredefinedAttributeValuesNullPredefinedValueTest() throws Exception {
    String attributeValue = "newValue";
    String attributeValueEn = "newValueEn";
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeValue existingProductAttributeValue = new ProductAttributeValue();
    existingProductAttributeValue.setMarkForDelete(false);
    existingProductAttributeValue.setPredefinedAllowedAttributeValue(null); // Setting null predefined value
    productAttribute.setProductAttributeValues(Collections.singletonList(existingProductAttributeValue));

    PredefinedAllowedAttributeValue newPredefinedValue = new PredefinedAllowedAttributeValue();
    newPredefinedValue.setValue(attributeValue);

    when(predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(
        STORE_ID, attributeData, attributeValue))
        .thenReturn(Collections.singletonList(newPredefinedValue));
    productServiceWrapper.processExistingPredefinedAttributeValues(STORE_ID, attributeData,
        Collections.singletonList(ProductSuitabilityAttributeModel.builder()
            .value(attributeValue)
            .valueEn(attributeValueEn)
            .build()),
        productAttribute);
    verify(predefinedAllowedAttributeValueService).findByStoreIdAndAttributeAndValueOrderByMarkForDelete(
        STORE_ID, attributeData, attributeValue);
    assertEquals(newPredefinedValue, existingProductAttributeValue.getPredefinedAllowedAttributeValue());
    assertFalse(existingProductAttributeValue.isMarkForDelete());
  }

  @Test
  public void updateCommonImagesAndPublishHistory_MixedSuccessAndFailure() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = USERNAME;
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setProductSku(SKU_CODE);
    request.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);

    List<ProductImageEditRequest> commonImages = new ArrayList<>();

    // First image - Add operation (should succeed)
    ProductImageEditRequest imageRequest1 = new ProductImageEditRequest();
    imageRequest1.setImagePath(IMAGE_PATH_1);
    imageRequest1.setProductCode(PRODUCT_CODE);
    CopyImageEditRequest copyRequest1 = new CopyImageEditRequest();
    copyRequest1.setAdd(true);
    imageRequest1.setCopyToAllVariantImages(copyRequest1);
    commonImages.add(imageRequest1);

    // Second image - Delete operation (should fail)
    ProductImageEditRequest imageRequest2 = new ProductImageEditRequest();
    imageRequest2.setImagePath(IMAGE_PATH_2);
    imageRequest2.setProductCode(PRODUCT_CODE);
    CopyImageEditRequest copyRequest2 = new CopyImageEditRequest();
    copyRequest2.setMarkForDelete(true);
    imageRequest2.setCopyToAllVariantImages(copyRequest2);
    commonImages.add(imageRequest2);

    // Third image - Main image operation (should succeed)
    ProductImageEditRequest imageRequest3 = new ProductImageEditRequest();
    imageRequest3.setImagePath("imagePath3");
    imageRequest3.setProductCode(PRODUCT_CODE);
    CopyImageEditRequest copyRequest3 = new CopyImageEditRequest();
    copyRequest3.setMainImage(true);
    imageRequest3.setCopyToAllVariantImages(copyRequest3);
    commonImages.add(imageRequest3);

    request.setCommonImages(commonImages);

    // Mock updateCommonImages response - mixed success/failure
    Map<String, Map<String, String>> imageErrorMap = new HashMap<>();

    // First image - SUCCESS
    Map<String, String> image1Response = new HashMap<>();
    image1Response.put(Constants.COPY_ALL_STATUS, Constants.SUCCESS);
    imageErrorMap.put(IMAGE_PATH_1, image1Response);

    // Second image - FAILED
    Map<String, String> image2Response = new HashMap<>();
    image2Response.put(Constants.COPY_ALL_STATUS, "FAILED");
    imageErrorMap.put(IMAGE_PATH_2, image2Response);

    // Third image - SUCCESS
    Map<String, String> image3Response = new HashMap<>();
    image3Response.put(Constants.COPY_ALL_STATUS, Constants.SUCCESS);
    imageErrorMap.put("imagePath3", image3Response);

    when(productService.updateImages(eq(storeId), eq(false), any(List.class), eq(null), eq(false)))
        .thenReturn(Pair.of(new ProductPublishUpdateDTO(productData, false, new HashSet<>()), imageErrorMap));
    when(productService.getProductByStoreIdAndProductCodeCached(storeId, PRODUCT_CODE))
        .thenReturn(productData);
    doNothing().when(productService).evictProductCache(storeId, productData);
    doNothing().when(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(storeId, PRODUCT_ID);
    doNothing().when(productService).evictProductItemsAndProductItemImagesCache(storeId, productData);
    when(domainEventPublisherService.publishProductChangeCategory(any(), eq(null), eq(false),
        eq(false), eq(true), eq(false), eq(false), any(HashSet.class), eq(true), eq(true), any(HashSet.class))).thenReturn(null);
    when(kafkaTopicProperties.getProductSkuUpdateExternalHistoryEvent()).thenReturn("product-sku-update-event");

    // When
    Map<String, Map<String, String>> result = productServiceWrapper.updateCommonImagesAndPublishHistory(
        storeId, username, request);

    // Then
    assertNotNull(result);
    assertEquals(Constants.SUCCESS, result.get(IMAGE_PATH_1).get(Constants.COPY_ALL_STATUS));
    assertEquals("FAILED", result.get(IMAGE_PATH_2).get(Constants.COPY_ALL_STATUS));
    assertEquals(Constants.SUCCESS, result.get("imagePath3").get(Constants.COPY_ALL_STATUS));

    // Verify that only successful operations trigger history publishing
    ArgumentCaptor<AuditTrailListResponse> auditTrailCaptor = ArgumentCaptor.forClass(AuditTrailListResponse.class);
    verify(kafkaPublisher).send(eq("product-sku-update-event"), eq(SKU_CODE), auditTrailCaptor.capture());

    // Verify audit trail contains only successful operations (2 entries: add + main image)
    AuditTrailListResponse capturedAuditTrail = auditTrailCaptor.getValue();
    assertNotNull(capturedAuditTrail);
    assertEquals(username, capturedAuditTrail.getChangedBy());
    assertEquals(Constants.CLIENT_ID, capturedAuditTrail.getClientId());
    assertTrue(capturedAuditTrail.isUpdateDirectly());
    assertTrue(capturedAuditTrail.isUpdateDirectlyToDB());
    assertEquals(2, capturedAuditTrail.getAuditTrailResponseList().size());

    // Verify the audit trail entries
    List<String> actionKeys = capturedAuditTrail.getAuditTrailResponseList().stream()
        .map(AuditTrailDto::getActionKey)
        .collect(Collectors.toList());

    assertTrue(actionKeys.contains(Constants.UPDATE_PRODUCT_ACTIVITY_ADD_IMAGE));
    assertTrue(actionKeys.contains(Constants.UPDATE_PRODUCT_ACTIVITY_MAIN_IMAGE_UPDATED));
    assertFalse(actionKeys.contains(Constants.UPDATE_PRODUCT_ACTIVITY_DELETE_IMAGE)); // Should not be present as it failed

    // Verify the specific audit trail entries
    AuditTrailDto addImageAudit = capturedAuditTrail.getAuditTrailResponseList().stream()
        .filter(audit -> Constants.UPDATE_PRODUCT_ACTIVITY_ADD_IMAGE.equals(audit.getActionKey()))
        .findFirst()
        .orElse(null);
    assertNotNull(addImageAudit);
    assertEquals(IMAGE_PATH_1, addImageAudit.getNewValue());
    assertEquals(StringUtils.EMPTY, addImageAudit.getOldValue());

    AuditTrailDto mainImageAudit = capturedAuditTrail.getAuditTrailResponseList().stream()
        .filter(audit -> Constants.UPDATE_PRODUCT_ACTIVITY_MAIN_IMAGE_UPDATED.equals(audit.getActionKey()))
        .findFirst()
        .orElse(null);
    assertNotNull(mainImageAudit);
    assertTrue(mainImageAudit.getNewValue().contains("MainImage:true:imagePath3"));
    assertTrue(mainImageAudit.getOldValue().contains("MainImage:false:imagePath3"));

    // Verify all service method calls
    verify(productService).updateImages(eq(storeId), eq(false), any(List.class), eq(null), eq(false));
    verify(productService).getProductByStoreIdAndProductCodeCached(storeId, PRODUCT_CODE);
    verify(productService).evictProductCache(storeId, productData);
    verify(productService).setCompleteProductDetailsCached(storeId, productData, false);
    verify(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(storeId, PRODUCT_ID);
    verify(productService).evictProductItemsAndProductItemImagesCache(storeId, productData);
    verify(domainEventPublisherService).publishProductChangeCategory(any(), eq(null), eq(false),
        eq(false), eq(true), eq(false), eq(false), any(HashSet.class), eq(true), eq(true), any(HashSet.class));
  }

  @Test
  public void updateCommonImagesAndPublishHistory_deleteImageSuccesss() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = USERNAME;
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setProductSku(SKU_CODE);
    request.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);

    List<ProductImageEditRequest> commonImages = new ArrayList<>();

    // Delete operation
    ProductImageEditRequest imageRequest = new ProductImageEditRequest();
    imageRequest.setImagePath(IMAGE_PATH_1);
    imageRequest.setProductCode(PRODUCT_CODE);
    CopyImageEditRequest copyRequest = new CopyImageEditRequest();
    copyRequest.setMarkForDelete(true);
    imageRequest.setCopyToAllVariantImages(copyRequest);
    commonImages.add(imageRequest);

    request.setCommonImages(commonImages);

    // Mock updateCommonImages response
    Map<String, Map<String, String>> imageErrorMap = new HashMap<>();

    // Second image - FAILED
    Map<String, String> imageResponse = new HashMap<>();
    imageResponse.put(Constants.COPY_ALL_STATUS, Constants.SUCCESS);
    imageErrorMap.put(IMAGE_PATH_1, imageResponse);



    when(productService.updateImages(eq(storeId), eq(false), any(List.class), eq(null), eq(false)))
        .thenReturn(Pair.of(new ProductPublishUpdateDTO(productData, false, new HashSet<>()), imageErrorMap));
    when(productService.getProductByStoreIdAndProductCodeCached(storeId, PRODUCT_CODE))
        .thenReturn(productData);
    doNothing().when(productService).evictProductCache(storeId, productData);
    doNothing().when(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(storeId, PRODUCT_ID);
    doNothing().when(productService).evictProductItemsAndProductItemImagesCache(storeId, productData);
    when(domainEventPublisherService.publishProductChangeCategory(any(), eq(null), eq(false),
        eq(false), eq(true), eq(false), eq(false), any(HashSet.class), eq(true), eq(true), any(HashSet.class))).thenReturn(null);
    when(kafkaTopicProperties.getProductSkuUpdateExternalHistoryEvent()).thenReturn("product-sku-update-event");

    // When
    Map<String, Map<String, String>> result = productServiceWrapper.updateCommonImagesAndPublishHistory(
        storeId, username, request);

    // Then
    assertNotNull(result);
    assertEquals(Constants.SUCCESS, result.get(IMAGE_PATH_1).get(Constants.COPY_ALL_STATUS));


    // Verify that only successful operations trigger history publishing
    ArgumentCaptor<AuditTrailListResponse> auditTrailCaptor = ArgumentCaptor.forClass(AuditTrailListResponse.class);
    verify(kafkaPublisher).send(eq("product-sku-update-event"), eq(SKU_CODE), auditTrailCaptor.capture());

    // Verify audit trail contains only successful operations (2 entries: add + main image)
    AuditTrailListResponse capturedAuditTrail = auditTrailCaptor.getValue();
    assertNotNull(capturedAuditTrail);
    assertEquals(username, capturedAuditTrail.getChangedBy());
    assertEquals(Constants.CLIENT_ID, capturedAuditTrail.getClientId());
    assertTrue(capturedAuditTrail.isUpdateDirectly());
    assertTrue(capturedAuditTrail.isUpdateDirectlyToDB());
    assertEquals(1, capturedAuditTrail.getAuditTrailResponseList().size());

    // Verify the audit trail entries
    List<String> actionKeys = capturedAuditTrail.getAuditTrailResponseList().stream()
        .map(AuditTrailDto::getActionKey)
        .collect(Collectors.toList());

    assertFalse(actionKeys.contains(Constants.UPDATE_PRODUCT_ACTIVITY_ADD_IMAGE));
    assertFalse(actionKeys.contains(Constants.UPDATE_PRODUCT_ACTIVITY_MAIN_IMAGE_UPDATED));
    assertTrue(actionKeys.contains(Constants.UPDATE_PRODUCT_ACTIVITY_DELETE_IMAGE));

    // Verify the specific audit trail entries
    AuditTrailDto deleteImageAudit = capturedAuditTrail.getAuditTrailResponseList().stream()
        .filter(audit -> Constants.UPDATE_PRODUCT_ACTIVITY_DELETE_IMAGE.equals(audit.getActionKey()))
        .findFirst()
        .orElse(null);
    assertNotNull(deleteImageAudit);
    assertEquals(IMAGE_PATH_1, deleteImageAudit.getNewValue());
    assertEquals(StringUtils.EMPTY, deleteImageAudit.getOldValue());

    // Verify all service method calls
    verify(productService).updateImages(eq(storeId), eq(false), any(List.class), eq(null), eq(false));
    verify(productService).getProductByStoreIdAndProductCodeCached(storeId, PRODUCT_CODE);
    verify(productService).evictProductCache(storeId, productData);
    verify(productService).setCompleteProductDetailsCached(storeId, productData, false);
    verify(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(storeId, PRODUCT_ID);
    verify(productService).evictProductItemsAndProductItemImagesCache(storeId, productData);
    verify(domainEventPublisherService).publishProductChangeCategory(any(), eq(null), eq(false),
        eq(false), eq(true), eq(false), eq(false), any(HashSet.class), eq(true), eq(true), any(HashSet.class));
  }

  @Test
  public void updateCommonImagesAndPublishHistory_noHistory() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = USERNAME;
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setProductSku(SKU_CODE);
    request.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);

    List<ProductImageEditRequest> commonImages = new ArrayList<>();

    // Delete operation
    ProductImageEditRequest imageRequest = new ProductImageEditRequest();
    imageRequest.setImagePath(IMAGE_PATH_1);
    imageRequest.setProductCode(PRODUCT_CODE);
    CopyImageEditRequest copyRequest = new CopyImageEditRequest();
    imageRequest.setCopyToAllVariantImages(copyRequest);
    commonImages.add(imageRequest);

    request.setCommonImages(commonImages);

    // Mock updateCommonImages response
    Map<String, Map<String, String>> imageErrorMap = new HashMap<>();

    // Second image - FAILED
    Map<String, String> imageResponse = new HashMap<>();
    imageResponse.put(Constants.COPY_ALL_STATUS, Constants.SUCCESS);
    imageErrorMap.put(IMAGE_PATH_1, imageResponse);



    when(productService.updateImages(eq(storeId), eq(false), any(List.class), eq(null), eq(false)))
        .thenReturn(Pair.of(new ProductPublishUpdateDTO(productData, false, new HashSet<>()), imageErrorMap));
    when(productService.getProductByStoreIdAndProductCodeCached(storeId, PRODUCT_CODE))
        .thenReturn(productData);
    doNothing().when(productService).evictProductCache(storeId, productData);
    doNothing().when(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(storeId, PRODUCT_ID);
    doNothing().when(productService).evictProductItemsAndProductItemImagesCache(storeId, productData);
    when(domainEventPublisherService.publishProductChangeCategory(any(), eq(null), eq(false),
        eq(false), eq(true), eq(false), eq(false), any(HashSet.class), eq(true), eq(true), any(HashSet.class))).thenReturn(null);
    when(kafkaTopicProperties.getProductSkuUpdateExternalHistoryEvent()).thenReturn("product-sku-update-event");

    // When
    Map<String, Map<String, String>> result = productServiceWrapper.updateCommonImagesAndPublishHistory(
        storeId, username, request);

    // Then
    assertNotNull(result);
    assertEquals(Constants.SUCCESS, result.get(IMAGE_PATH_1).get(Constants.COPY_ALL_STATUS));


    // Verify that only successful operations trigger history publishing
    ArgumentCaptor<AuditTrailListResponse> auditTrailCaptor = ArgumentCaptor.forClass(AuditTrailListResponse.class);

       verify(productService).updateImages(eq(storeId), eq(false), any(List.class), eq(null), eq(false));
    verify(productService).getProductByStoreIdAndProductCodeCached(storeId, PRODUCT_CODE);
    verify(productService).evictProductCache(storeId, productData);
    verify(productService).setCompleteProductDetailsCached(storeId, productData, false);
    verify(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(storeId, PRODUCT_ID);
    verify(productService).evictProductItemsAndProductItemImagesCache(storeId, productData);
    verify(domainEventPublisherService).publishProductChangeCategory(any(), eq(null), eq(false),
        eq(false), eq(true), eq(false), eq(false), any(HashSet.class), eq(true), eq(true), any(HashSet.class));
  }

  @Test
  public void updateCommonImagesAndPublishHistory_all_Failure() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = USERNAME;
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setProductSku(SKU_CODE);
    request.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);

    List<ProductImageEditRequest> commonImages = new ArrayList<>();

    // First image - Add operation (should succeed)
    ProductImageEditRequest imageRequest1 = new ProductImageEditRequest();
    imageRequest1.setImagePath(IMAGE_PATH_1);
    imageRequest1.setProductCode(PRODUCT_CODE);
    CopyImageEditRequest copyRequest1 = new CopyImageEditRequest();
    copyRequest1.setAdd(true);
    imageRequest1.setCopyToAllVariantImages(copyRequest1);
    commonImages.add(imageRequest1);
    request.setCommonImages(commonImages);

    // Mock updateCommonImages response - mixed success/failure
    Map<String, Map<String, String>> imageErrorMap = new HashMap<>();

    // First image - SUCCESS
    Map<String, String> image1Response = new HashMap<>();
    image1Response.put(Constants.COPY_ALL_STATUS, "FAILED");
    imageErrorMap.put(IMAGE_PATH_1, image1Response);

    when(productService.updateImages(eq(storeId), eq(false), any(List.class), eq(null), eq(false)))
        .thenReturn(Pair.of(new ProductPublishUpdateDTO(productData, false, new HashSet<>()), imageErrorMap));
    when(productService.getProductByStoreIdAndProductCodeCached(storeId, PRODUCT_CODE))
        .thenReturn(productData);
    doNothing().when(productService).evictProductCache(storeId, productData);
    doNothing().when(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(storeId, PRODUCT_ID);
    doNothing().when(productService).evictProductItemsAndProductItemImagesCache(storeId, productData);
    when(domainEventPublisherService.publishProductChangeCategory(any(), eq(null), eq(false),
        eq(false), eq(true), eq(false), eq(false), any(HashSet.class), eq(true), eq(true), any(HashSet.class))).thenReturn(null);
    when(kafkaTopicProperties.getProductSkuUpdateExternalHistoryEvent()).thenReturn("product-sku-update-event");

    // When
    Map<String, Map<String, String>> result = productServiceWrapper.updateCommonImagesAndPublishHistory(
        storeId, username, request);

    // Then
    assertNotNull(result);
    assertEquals("FAILED", result.get(IMAGE_PATH_1).get(Constants.COPY_ALL_STATUS));


    // Verify all service method calls
    verify(productService).updateImages(eq(storeId), eq(false), any(List.class), eq(null), eq(false));
    verify(productService).getProductByStoreIdAndProductCodeCached(storeId, PRODUCT_CODE);
    verify(productService).evictProductCache(storeId, productData);
    verify(productService).setCompleteProductDetailsCached(storeId, productData, false);
    verify(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(storeId, PRODUCT_ID);
    verify(productService).evictProductItemsAndProductItemImagesCache(storeId, productData);
    verify(domainEventPublisherService).publishProductChangeCategory(any(), eq(null), eq(false),
        eq(false), eq(true), eq(false), eq(false), any(HashSet.class), eq(true), eq(true), any(HashSet.class));
  }

  @Test
  public void getProductMasterDataByProductCode() {
    ProductCodesRequest request = new ProductCodesRequest();
    request.setProductCodes(PRODUCT_CODES);

    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setLength(LENGTH);
    product.setHeight(HEIGHT);
    product.setWidth(WIDTH);
    product.setWeight(WEIGHT);

    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    productServiceWrapper.getProductMasterDataByProductCode(STORE_ID, request);

    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productItemServiceWrapper).setProductItemsCached(STORE_ID, product, true);
    verify(productService).setProductCategoriesWithCategoriesCached(STORE_ID, product, false);
  }

  @Test
  public void getProductMasterDataByProductCode_Catch() {
    ProductCodesRequest request = new ProductCodesRequest();
    request.setProductCodes(PRODUCT_CODES);

    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setLength(LENGTH);
    product.setHeight(HEIGHT);
    product.setWidth(WIDTH);
    product.setWeight(WEIGHT);
    doThrow(RuntimeException.class).when(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    productServiceWrapper.getProductMasterDataByProductCode(STORE_ID, request);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
  }

  // Test cases for updateProductBrandDataWithBrandInfo method

  @Test
  public void testUpdateProductBrandDataWithBrandInfo_OnlyBrandNameUpdateWithBrandLevelUpdateRequired() throws Exception {
    // Given
    String storeId = STORE_ID;
    ProductBrandDataUpdateRequest request = new ProductBrandDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setOldBrandCode(BRAND_CODE);
    request.setNewBrandName(BRAND);
    request.setOnlyBrandNameUpdate(true);
    request.setBrandLevelUpdateRequired(true);

    // Mock the return value for brandService.updateOnlyBrandName
    BrandHistoryEventModel brandHistoryEventModel = new BrandHistoryEventModel();
    BrandWip brandWip = new BrandWip();
    brandWip.setId("brandWipId");
    Brand brand = new Brand();
    Triple<BrandHistoryEventModel, BrandWip, Brand> mockTriple = Triple.of(brandHistoryEventModel, brandWip, brand);
    
    when(brandService.updateOnlyBrandName(storeId, BRAND, BRAND_CODE)).thenReturn(mockTriple);

    Product mockProduct = new Product();
    when(productService.updateOnlyBrandNameOfProduct(storeId, request)).thenReturn(mockProduct);
    when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId,
        PRODUCT_CODE)).thenReturn(mockProduct);
    

    SolrUpdateBrandDomainEventModel mockSolrUpdateEvent = new SolrUpdateBrandDomainEventModel();
    BrandDomainEventModel mockBrandDomainEvent = new BrandDomainEventModel();
    
    when(domainEventPublisherService.publishSolrUpdateBrandEvent(any())).thenReturn(mockSolrUpdateEvent);
    when(domainEventPublisherService.publishBrandUpdated(any())).thenReturn(mockBrandDomainEvent);

    // When
    productServiceWrapper.updateProductBrandDataWithBrandInfo(storeId, request);

    // Then
    verify(brandService).updateOnlyBrandName(storeId, BRAND, BRAND_CODE);
    verify(productService).updateOnlyBrandNameOfProduct(storeId, request);
    verify(productService).evictCompleteProductAndItemsCache(any(), any());
    verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID,
        PRODUCT_CODE);
    verify(domainEventPublisherService).publishBrandHistory(brandHistoryEventModel);
    verify(domainEventPublisherService).publishSolrUpdateBrandEvent(any());
    verify(domainEventPublisherService).publishBrandUpdated(brand);
    verify(domainEventPublisherService).publishProductChangeCategory(any(), any(), eq(true),
        eq(true), eq(true), eq(false), any());
  }

  @Test
  public void testUpdateProductBrandDataWithBrandInfo_OnlyBrandNameUpdateWithoutBrandLevelUpdate() throws Exception {
    // Given
    String storeId = STORE_ID;
    ProductBrandDataUpdateRequest request = new ProductBrandDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setOldBrandCode(BRAND_CODE);
    request.setNewBrandName(BRAND);
    request.setOnlyBrandNameUpdate(true);

    Product mockProduct = new Product();
    when(productService.updateOnlyBrandNameOfProduct(storeId, request)).thenReturn(mockProduct);
    when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId,
        PRODUCT_CODE)).thenReturn(mockProduct);

    productServiceWrapper.updateProductBrandDataWithBrandInfo(storeId, request);

    // Then
    verify(productService).updateOnlyBrandNameOfProduct(storeId, request);
    verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID,
        PRODUCT_CODE);
    verify(productService).evictCompleteProductAndItemsCache(any(), any());
    verify(domainEventPublisherService).publishProductChangeCategory(any(), any(), eq(true),
        eq(true), eq(true), eq(false), any());
  }

  @Test
  public void testUpdateProductBrandDataWithBrandInfo_NotOnlyBrandNameUpdate() throws Exception {
    // Given
    String storeId = STORE_ID;
    ProductBrandDataUpdateRequest request = new ProductBrandDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setOldBrandCode(BRAND_CODE);
    request.setNewBrandCode(BRAND_CODE_2);
    request.setOnlyBrandNameUpdate(false);

    Pair<Product, String> mockResult = Pair.of(productData, BRAND_CODE);

    // When
    when(productService.updateProductBrand(eq(storeId),
        any(ProductBrandUpdateDTO.class))).thenReturn(mockResult);
    when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId,
        PRODUCT_CODE)).thenReturn(productData);

    productServiceWrapper.updateProductBrandDataWithBrandInfo(storeId, request);

    // Then
    verify(productService).updateProductBrand(eq(storeId), any(ProductBrandUpdateDTO.class));
    verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID,
        PRODUCT_CODE);
    verify(productService).evictCacheForSimpleMasterProductUpdate(eq(storeId), anyString(),
        anyString());
    verify(domainEventPublisherService).publishProductChangeCategory(any(), any(), eq(true),
        eq(true), eq(true), eq(false), any());
  }

  @Test
  public void testUpdateProductBrandDataWithBrandInfo_ValidationFailure_BlankBrandCode()
      throws Exception {
    // Given
    String storeId = STORE_ID;
    ProductBrandDataUpdateRequest request = new ProductBrandDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setNewBrandCode(""); // Blank brand code
    request.setNewBrandName(BRAND);
    request.setOnlyBrandNameUpdate(true);
    request.setBrandLevelUpdateRequired(true);

    // When & Then
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class, () -> {
      productServiceWrapper.updateProductBrandDataWithBrandInfo(storeId, request);
    });

    assertEquals("Can not process invalid input data :"
        + ErrorMessage.BRAND_CODE_MUST_NOT_BE_BLANK.getMessage(), exception.getMessage());
    verify(brandService, never()).updateOnlyBrandName(anyString(), anyString(), anyString());
    verify(productService, never()).updateOnlyBrandNameOfProduct(any(), any());
    verify(productService, never()).updateProductBrand(any(), any());
  }

  @Test
  public void testUpdateProductBrandDataWithBrandInfo_ValidationFailure_BlankBrandName()
      throws Exception {
    // Given
    String storeId = STORE_ID;
    ProductBrandDataUpdateRequest request = new ProductBrandDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setOldBrandCode(BRAND_CODE);
    request.setNewBrandName(""); // Blank brand name
    request.setOnlyBrandNameUpdate(true);
    request.setBrandLevelUpdateRequired(true);

    // When & Then
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class, () -> {
      productServiceWrapper.updateProductBrandDataWithBrandInfo(storeId, request);
    });

    assertEquals("Can not process invalid input data :"
        + ErrorMessage.BRAND_NAME_MUST_NOT_BE_BLANK.getMessage(), exception.getMessage());
    verify(brandService, never()).updateOnlyBrandName(anyString(), anyString(), anyString());
    verify(productService, never()).updateOnlyBrandNameOfProduct(any(), any());
    verify(productService, never()).updateProductBrand(any(), any());
  }

  @Test
  public void testUpdateProductBrandDataWithBrandInfo_ValidationFailure_NullBrandCode()
      throws Exception {
    // Given
    String storeId = STORE_ID;
    ProductBrandDataUpdateRequest request = new ProductBrandDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setNewBrandCode(null); // Null brand code
    request.setNewBrandName(BRAND);
    request.setOnlyBrandNameUpdate(true);
    request.setBrandLevelUpdateRequired(true);

    // When & Then
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class, () -> {
      productServiceWrapper.updateProductBrandDataWithBrandInfo(storeId, request);
    });

    assertEquals("Can not process invalid input data :"
        + ErrorMessage.BRAND_CODE_MUST_NOT_BE_BLANK.getMessage(), exception.getMessage());
    verify(brandService, never()).updateOnlyBrandName(anyString(), anyString(), anyString());
    verify(productService, never()).updateOnlyBrandNameOfProduct(any(), any());
    verify(productService, never()).updateProductBrand(any(), any());
  }

  @Test
  public void testUpdateProductBrandDataWithBrandInfo_ValidationFailure_NullBrandName()
      throws Exception {
    // Given
    String storeId = STORE_ID;
    ProductBrandDataUpdateRequest request = new ProductBrandDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setOldBrandCode(BRAND_CODE);
    request.setNewBrandName(null); // Null brand name
    request.setOnlyBrandNameUpdate(true);
    request.setBrandLevelUpdateRequired(true);

    // When & Then
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class, () -> {
      productServiceWrapper.updateProductBrandDataWithBrandInfo(storeId, request);
    });

    assertEquals("Can not process invalid input data :"
        + ErrorMessage.BRAND_NAME_MUST_NOT_BE_BLANK.getMessage(), exception.getMessage());
    verify(brandService, never()).updateOnlyBrandName(anyString(), anyString(), anyString());
    verify(productService, never()).updateOnlyBrandNameOfProduct(any(), any());
    verify(productService, never()).updateProductBrand(any(), any());
  }

  @Test
  public void testUpdateProductBrandDataWithBrandInfo_ExceptionHandling() throws Exception {
    // Given
    String storeId = STORE_ID;
    ProductBrandDataUpdateRequest request = new ProductBrandDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setOldBrandCode(BRAND_CODE);
    request.setNewBrandName(BRAND);
    request.setOnlyBrandNameUpdate(true);
    request.setBrandLevelUpdateRequired(true);

    // When
    doThrow(new RuntimeException("Service error")).when(brandService).updateOnlyBrandName(storeId, BRAND, BRAND_CODE);

    // Then
    assertThrows(RuntimeException.class, () -> {
      productServiceWrapper.updateProductBrandDataWithBrandInfo(storeId, request);
    });

    verify(brandService).updateOnlyBrandName(storeId, BRAND, BRAND_CODE);
    verify(productService, never()).updateOnlyBrandNameOfProduct(any(), any());
  }
}