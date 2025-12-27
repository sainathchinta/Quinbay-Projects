package com.gdn.x.productcategorybase.service.impl;

import static com.google.common.base.Predicates.not;
import static java.util.stream.Collectors.toList;

import java.util.LinkedHashSet;
import java.util.stream.Collectors;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.domain.event.model.CommonImageBackfillingEventModel;
import com.gdn.x.productcategorybase.dto.MasterProductDataUpdateDTO;
import com.gdn.x.productcategorybase.dto.MigrationPayload;
import com.gdn.x.productcategorybase.dto.ProductBrandUpdateDTO;
import com.gdn.x.productcategorybase.dto.ProductDTO;
import com.gdn.x.productcategorybase.dto.ProductDetailEditDTO;
import com.gdn.x.productcategorybase.dto.VideoDTO;
import com.gdn.x.productcategorybase.dto.UpdateNeedRevisionDTO;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.EditProductDetailRequest;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.ProductMasterDataUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductBrandDataUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.VideoAddEditRequest;
import com.gdn.x.productcategorybase.dto.response.AuditTrailDto;
import com.gdn.x.productcategorybase.dto.response.AuditTrailListResponse;
import com.gdn.x.productcategorybase.dto.response.BasicInfoProductResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.ProductItemUomInfo;
import com.gdn.x.productcategorybase.exception.ValidationException;
import com.gdn.x.productcategorybase.service.ProductItemAttributeValueService;
import com.gdn.x.productcategorybase.service.ProductItemUomInfoService;
import com.gdn.x.productcategorybase.service.brand.BrandAuthorisationServiceBean;
import com.gdn.x.productcategorybase.util.ConverterUtil;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
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
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.common.util.BeanUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.util.CollectionUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnDigestUtil;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.inventory.dto.WarehouseMasterSKUEvent;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.ProductPublishEventType;
import com.gdn.x.productcategorybase.domain.event.model.ProductSalesCategoryMapping;
import com.gdn.x.productcategorybase.domain.event.model.VatUpdateDomainEventModel;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.ProductAndItemLevelUpdatesDTO;
import com.gdn.x.productcategorybase.dto.ReplaceProductImagesDTO;
import com.gdn.x.productcategorybase.dto.ReplaceProductItemImagesDTO;
import com.gdn.x.productcategorybase.dto.SimpleMasterProductUpdateDTO;
import com.gdn.x.productcategorybase.dto.request.CopyImageEditRequest;
import com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest;
import com.gdn.x.productcategorybase.dto.request.NeedRevisionConfigRequest;
import com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemImageUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.NewlySavedItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductSalesCategoryMappingResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductImageCleanup;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.entity.SystemParameter;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.repository.ProductImageCleanupRepository;
import com.gdn.x.productcategorybase.repository.ProductImageRepository;
import com.gdn.x.productcategorybase.repository.ProductItemImageRepository;
import com.gdn.x.productcategorybase.repository.ProductRepository;
import com.gdn.x.productcategorybase.repository.ProductRepositoryCustom;
import com.gdn.x.productcategorybase.repository.ProductScoreUpdateRepository;
import com.gdn.x.productcategorybase.service.AllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.ApplicationConfigPropertiesService;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.CategoryReferenceService;
import com.gdn.x.productcategorybase.service.CategoryShippingService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.FileStorageService;
import com.gdn.x.productcategorybase.service.ImageService;
import com.gdn.x.productcategorybase.service.PredefinedAllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.ProductAttributeExtractionService;
import com.gdn.x.productcategorybase.service.ProductAttributeValueService;
import com.gdn.x.productcategorybase.service.ProductCategoryService;
import com.gdn.x.productcategorybase.service.ProductItemServiceWrapper;
import com.gdn.x.productcategorybase.service.ProductService;
import com.gdn.x.productcategorybase.service.SystemParameterService;
import com.gdn.x.productcategorybase.service.brand.BrandService;
import com.gdn.x.productcategorybase.util.ProductImageUtil;
import com.google.common.collect.ImmutableSet;

public class ProductServiceTest {

  @Mock
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Mock
  private AttributeService attributeService;

  @Mock
  private CategoryServiceBean categoryService;

  @Mock
  private ProductAttributeServiceBean productAttributeService;

  @Mock
  private ProductAttributeValueService productAttributeValueService;

  @Mock
  private ProductItemAttributeValueService productItemAttributeValueService;

  @Mock
  private ProductItemServiceBean productItemService;

  @Mock
  private ProductRepository repository;

  @Mock
  private ProductScoreUpdateRepository productScoreUpdateRepository;

  @Mock
  private ApplicationConfigPropertiesService applicationConfigPropertiesService;

  @Mock
  private ProductImageCleanupRepository productImageCleanupRepository;

  @Mock
  private ProductImageRepository productImageRepository;

  @Mock
  private ProductItemImageRepository productItemImageRepository;

  @Mock
  private DomainEventPublisherService domainEventPublisherService;

  @Mock
  private AllowedAttributeValueService allowedAttributeValueService;

  @Mock
  private PredefinedAllowedAttributeValueService predefinedAllowedAttributeValueService;

  @Mock
  private CategoryReferenceService categoryReferenceService;

  @Mock
  private ProductCategoryService productCategoryService;

  @Mock
  private ImageService imageService;

  @Mock
  private ProductItemServiceWrapper productItemServiceWrapper;

  @Mock
  private ApplicationContext applicationContext;

  @Mock
  private BrandService brandService;

  @Mock
  private CategoryShippingService categoryShippingService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ProductService productService;

  @Mock
  private ProductRepositoryCustom productRepositoryCustom;

  @Mock
  private ProductAttributeExtractionService productAttributeExtractionService;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private BrandAuthorisationServiceBean brandAuthorisationServiceBean;

  @Captor
  private ArgumentCaptor<PageRequest> pageRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<Product> productArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductSalesCategoryMapping> productSalesCategoryMappingArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<ProductImageCleanup>> listArgumentCaptor;

  @Captor
  private ArgumentCaptor<Product> productCaptor;

  @Mock
  private ProductItemUomInfoService productItemUomInfoService;

  @InjectMocks
  private ProductServiceBean service;

  private final String[] strBahan = {"Metal", "Plastik"};
  private final String[] strBahanId = {"MetalId", "PlastikId"};


  private final String[] strOS = {"Android", "IOS", "Ubuntu"};
  private final String[] strUkuran = {"16GB", "32GB"};
  private final String[] strUkuranId = {"16GBId", "32GBId"};
  private final String[] strWarna = {"Biru", "Merah", "Ungu"};

  private final String[] strWarnaId = {"BiruId", "MerahId", "UnguId"};
  private static final String OLD_SALES_CATEGORY = "sales_1";
  private static final String NEW_SALES_CATEGORY = "sales_2";
  private static final String NEW_UMKM_SALES_CATEGORY = "sales_3";

  private static final String BRAND_1_NAME = "BRAND_1_NAME";
  private static final String CATEGORY_1_NAME = "CATEGORY_1_NAME";
  private static final String CATEGORY_ID = "CATEGORY_ID";
  private static final String CREATED_BY = "SYSTEM";
  private static final Pageable DEFAULT_PAGEABLE = PageRequest.of(0, 10);
  private static final String DESKRIPSI_BARANG = "ini adalah deskripsi barang";
  private static final String ERROR_MESSAGE_FOR_SAVE = "use update for existing entity";
  private static final String ERROR_MESSAGE_FOR_UPDATE = "can not update un existence data";
  private static final Logger LOG = LoggerFactory.getLogger(ProductServiceTest.class);
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String PRODUCT_1_NAME = "PRODUCT_1_NAME";
  private static final String PRODUCT_ID = "id";
  private static final String PRODUCT_ITEM_ID_1 = "productItemId1";
  private static final String PRODUCT_ITEM_ID_2 = "productItemId2";
  private static final String ITEM_SKU_CODE_1 = "itemSkuCode1-00001";
  private static final String ITEM_SKU_CODE_2 = "itemSkuCode2-00002";
  private static final String STORE_ID = "10001";
  private static final String HASH = "hash";
  private static final String WARNA_ID = "warnaId";
  private static final String UPDATED_UPC = "upcCodeUpdated";
  private static final String UPC_CODE = "upcCode";
  private static final String ATTRIBUTE_CODE = "ATT-1234";
  private static final String PREDEFINED_ATTRIBUTE = "PREDEFINED_ATTRIBUTE";
  private static final String UUID_1 = "uuid";;
  private static final String UNIQUE_SELLING_POINT = "UNIQUE_SELLING_POINT";
  private static final String UOM = "UOM";
  private static final String ID = "ID";
  private static final String LOCATION_PATH = "MTA-0000001/locationPath.jpeg";
  private static final String ID_1 = "ID1";
  private static final String UPDATED_BY = "SYSTEM";
  private static final Double WEIGHT_10 = 10.0D;
  private static final String WRONG_PRODUCT_CODE = "WRONG_PRODUCT_CODE";
  private static final VerificationMode NEVER_CALLED = times(0);
  private static final String DEFAULT_PRODUCT_CODE = "MTA-0000001";
  private static final String DEFAULT_PRODUCT_NAME = "PRODUCT NAME";
  private static final String DEFAULT_PRODUCT_ID = UUID.randomUUID().toString();
  private static final String PRODUCT_ATTRIBUTE_ID_1 = "productAttributeId1";
  private static final String PRODUCT_ATTRIBUTE_ID_2 = "productAttributeId2";
  private static final String PRODUCT_ATTRIBUTE_ID_3 = "productAttributeId3";
  private static final String PRODUCT_ATTRIBUTE_ID_4 = "productAttributeId4";
  private static final String PRODUCT_ATTRIBUTE_ID_5 = "productAttributeId5";
  private static final String PRODUCT_ATTRIBUTE_VALUE_ID_1 = "productAttributeValueId1";
  private static final String PRODUCT_ATTRIBUTE_VALUE_ID_1_1 = "productAttributeValueId1_1";
  private static final String PRODUCT_ATTRIBUTE_VALUE_ID_2 = "productAttributeValueId2";
  private static final String PRODUCT_ATTRIBUTE_VALUE_ID_3 = "productAttributeValueId3";
  private static final String PRODUCT_ATTRIBUTE_VALUE_ID_4 = "productAttributeValueId4";
  private static final String PRODUCT_ATTRIBUTE_VALUE_ID_5 = "productAttributeValueId5";
  private static final String PRODUCT_ATTRIBUTE_VALUE_1 = "productAttributeValue1";
  private static final String PRODUCT_ATTRIBUTE_VALUE_2 = "productAttributeValue2";
  private static final String PRODUCT_ATTRIBUTE_VALUE_3 = "productAttributeValue3";
  private static final String PRODUCT_ATTRIBUTE_VALUE_5 = "productAttributeValue5";
  private static final String PRODUCT_ATTRIBUTE_VALUE_6 = "productAttributeValue6";
  private static final String PRODUCT_ITEM_ATTRIBUTE_VALUE_1 = "productItemAttributeValue1";
  private static final String PRODUCT_ITEM_ATTRIBUTE_VALUE_2 = "productItemAttributeValue2";
  private static final String PRODUCT_ITEM_ATTRIBUTE_VALUE_3 = "productItemAttributeValue3";
  private static final String PRODUCT_ITEM_ATTRIBUTE_VALUE_4 = "productItemAttributeValue4";
  private static final String PRODUCT_ITEM_ATTRIBUTE_VALUE_5 = "productItemAttributeValue5";
  private static final String DEFAULT_IMAGE_ID = UUID.randomUUID().toString();
  private static final String LOCATION_PATH_1 = "locationPath1";
  private static final String LOCATION_PATH_2 = "locationPath2";
  private static final String LOCATION_PATH_3 = "locationPath3";
  private static final String LOCATION_PATH_4 = "locationPath4";
  private static final String PRODUCT_IMAGE_ID_1 = "productImageId1";
  private static final String PRODUCT_IMAGE_ID_2 = "productImageId2";
  private static final String PRODUCT_IMAGE_ID_3 = "productImageId3";
  private static final String PRODUCT_IMAGE_ID_4 = "productImageId4";
  private static final String DEFAULT_SKU_CODE = "MTA-0000001-00001";
  private static final String DEFAULT_ATTRIBUTE_CODE = "ATTR-0000001";
  private static final String DEFAULT_ATTRIBUTE_CODE_2 = "ATTR-0000002";
  private static final String DEFAULT_ATTRIBUTE_CODE_3 = "ATTR-0000003";
  private static final String DEFAULT_ATTRIBUTE_CODE_4 = "ATTR-0000004";
  private static final String ATTRIBUTE_ID_1 = "attributeId1";
  private static final String ATTRIBUTE_ID_2 = "attributeId2";
  private static final String ATTRIBUTE_ID_3 = "attributeId3";
  private static final String ATTRIBUTE_ID_4 = "attributeId4";
  private static final String ATTRIBUTE_NAME_1 = "attributeName1";
  private static final String ATTRIBUTE_NAME_2 = "attributeName2";
  private static final String ATTRIBUTE_NAME_3 = "attributeName3";
  private static final String ATTRIBUTE_NAME_4 = "attributeName4";
  private static final String ATTRIBUTE_CODE_2 = "attributeCode2";
  private static final String ATTRIBUTE_CODE_3 = "attributeCode3";
  private static final String ATTRIBUTE_CODE_4 = "attributeCode4";
  private static final String ATTRIBUTE_CODE_5 = "attributeCode5";
  private static final String ATTRIBUTE_ID = "attributeId";
  private static final String ATTRIBUTE_ID_5 = "attributeId5";
  private static final String ATTRIBUTE_VALUE = "attributeValue";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_GENERATED_ITEM_NAME = "GENERATED ITEM NAME";
  private static final boolean IS_PROMO_SKU = true;
  private static final String SPACE = " ";
  private static final String COLOUR_FAMILY = "Family Colour";
  private static final String REMOVE_PRODUCT_ITEM_1 = "PRODUCT_1_NAME Plastik 16GB Biru";
  private static final String REMOVE_PRODUCT_ITEM_2 = "PRODUCT_1_NAME Metal 16GB Merah";
  private static final String SKU_CODE = "skuCode";
  private static final String GENERATED_ITEM_NAME = "generatedItemName";
  private static final String CATEGORY_CODE_1 = "category_1";
  private static final String CATEGORY_CODE_2 = "category_2";
  private static final String CATEGORY_ID_1 = "category_id_1";
  private static final String CATEGORY_ID_2 = "category_id_2";
  private static final String CATEGORY_ID_3 = "category_id_3";
  private static final String SALES_CATEGORY_1 = "Sales_cat_1";
  private static final String SALES_CATEGORY_2 = "Sales_cat_2";
  private static final String SALES_CATEGORY_3 = "Sales_cat_3";
  private static final String BASE_FOLDER = "./src/test/resources/testFolders";
  private static final String FOLDER1 = "./src/test/resources/testFolders/folder1";
  private static final String FOLDER2 = "./src/test/resources/testFolders/folder2";
  private static final String FOLDER3 = "./src/test/resources/testFolders/folder3";
  private static final String PRODUCT_CODE1 = "MTA-1234567";
  private static final String PRODUCT_CODE2 = "MTA-1234568";
  private static final String FILE_NAME_PATH1 = "/1/MTA-1234568/Testing1.txt";
  private static final String FILE_NAME_PATH2 = "/1/MTA-1234568/Testing2.txt";
  private static final String FILE_NAME1 = "Testing1.txt";
  private static final String FILE_NAME2 = "Testing2.txt";
  private static final String DEFAULT_MERCHANT_CODE = "merchant_code";
  private static final String PRODUCT_NAME = "productName";
  private static final String PRODUCT_NAME1 = "productName1";
  private static final String BRAND = "brand";
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
  private static final Integer DEFAULT_DANGEROUS_GOODS_LEVEL = 0;
  private static final String BRAND_ATTRIBUTE_NAME = "Brand";
  private static final String BRAND_ATTRIBUTE_CODE = "brandAttributeCode";
  private static final String NEW_BRAND_NAME = "newBrand";
  private static final String NEW_BRAND_CODE = "newBrandCode";
  private static final String ITEM_NAME = "itemName";
  private static final String CATEGORY_CODE_NEW = "categoryCodeNew";
  private static final String CATEGORY_ID_NEW = "categoryIdNew";
  private static final String CATEGORY_NAME_NEW = "categoryNameNew";
  private static final String OLD_PRODUCT_CODE = "MTA-0000001";
  private static final String NEW_PRODUCT_CODE = "MTA-0000002";
  private static final String NEW_PRODUCT_SKU_CODE = "MTA-0000002-00001";
  private static final String CREATED_MERCHANT = "MERCHANT_CODE";
  private static final String RESIZE_LOCATION_PATH = "MTA-0000001/resize/locationPath.jpg";
  private static final String RESIZE_LOCATION_PATH1 = "MTA-0000001/resize/location1Path.jpg";
  private static final String LOCATION_PATH1 = "MTA-0000001/location1Path.jpeg";
  private static final String DEFINING_ATTRIBUTE_NAME = "definingAttributeName";
  private static final String DEFINING_ATTRIBUTE_NAME_TRUE = "definingAttributeNameTrue";
  private  ProductRequest productRequest;
  private static final String VIDEO_ID = "VID123";
  private static final String FINAL_URL = "http://example.com/video.mp4";
  private static final String COVER_IMAGE_PATH = "http://example.com/cover.jpg";
  private static final String SOURCE_URL = "http://example.com/source.mp4";
  private static final String EXISTING_VIDEO_JSON =
    "{\"videoId\":\"VID123\",\"finalUrl\":\"http://old.example.com/video.mp4\",\"coverImagePath\":\"http://old.example.com/cover.jpg\",\"videoName\":\"Old Video\",\"sourceUrl\":\"http://old.example.com/source.mp4\"}";
  public static final String TEST_VIDEO = "Test Video";
  public static final String UPDATED_VIDEO_JSON = "updatedVideoJSON";
  public static final String SELLER_CODE = "sellerCode";
  private Attribute ukuran;
  private Attribute warna;
  private ReplaceProductImagesDTO replaceProductImages;
  private ProductSalesCategoryMapping productSalesCategoryMapping;
  private Attribute deskripsi;
  private final List<String> Ids = new ArrayList<String>();
  private Attribute os;
  private Product product;
  private ProductItem productItem;
  private Attribute attribute;
  private ProductCategory productCategory = new ProductCategory();
  private Map<String, Long> categoryCountMap;
  private Category category;
  private Category category1;
  private ProductCategory productCategory1 = new ProductCategory();
  private List<String> productCodes = new ArrayList<>();
  private List<String> imageLocation = new ArrayList<>();
  private ProductImage productImage = new ProductImage();
  private Page<Product> productPage;
  private List<Product> products;
  private Attribute bahan;
  private List<ProductItem> productItems;
  private List<ProductImage> productImages;
  private List<ProductAttribute> productAttributes;
  private ProductAttribute productAttribute1;
  private ProductAttribute productAttribute2;
  private ProductAttribute productAttribute3;
  private ProductAttribute productAttribute4;
  private List<ProductAttributeValue> productAttributeValues1;
  private List<ProductAttributeValue> productAttributeValues2;
  private List<ProductAttributeValue> productAttributeValues3;
  private List<ProductCategory> productCategories;
  private Attribute attribute1;
  private Attribute attribute2;
  private Attribute attribute3;
  private Attribute attribute4;
  private Brand brand;
  private BrandResponse newBrand;
  private BrandResponse oldBrand;
  private PredefinedAllowedAttributeValue updatedBrandAttribute;
  private SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO;
  private ProductBrandUpdateDTO productBrandUpdateDTO;
  private Attribute brandAttribute;
  private ProductAttribute brandProductAttribute;
  private Category category2;
  private WarehouseMasterSKUEvent warehouseMasterSKUEvent;
  private List<Product> productList;
  private ProductAttribute definingProductAttribute;
  private ProductAttribute definingProductAttribute1;

  private Product createAdvancedProduct() {
    Product product = this.createDefaultProduct();
    String uuid = GdnUUIDHelper.generateUUID();
    product.setId(uuid);

    this.warna = new Attribute("Warna", AttributeType.DEFINING_ATTRIBUTE, true, ProductServiceTest.STORE_ID);
    this.warna.setId("warnaId");
    this.ukuran = new Attribute("Ukuran", AttributeType.DEFINING_ATTRIBUTE, true, ProductServiceTest.STORE_ID);
    this.ukuran.setId("ukuranId");
    this.bahan = new Attribute("Bahan", AttributeType.DEFINING_ATTRIBUTE, true, ProductServiceTest.STORE_ID);
    this.bahan.setId("bahanId");
    this.deskripsi = new Attribute("Deskripsi", AttributeType.DESCRIPTIVE_ATTRIBUTE, true, ProductServiceTest.STORE_ID);
    this.deskripsi.setId("deskripsiId");
    this.os = new Attribute("OS", AttributeType.PREDEFINED_ATTRIBUTE, true, ProductServiceTest.STORE_ID);
    this.os.setId("osId");

    List<ProductAttribute> productAttributes = new ArrayList<ProductAttribute>();
    ProductAttribute productAttributeWarna =
        new ProductAttribute(this.warna, product, "warna", false, Integer.valueOf(1), ProductServiceTest.STORE_ID);
    productAttributeWarna.setProductId(product.getId());
    productAttributeWarna.setId(this.Ids.get(0));
    ProductAttribute productAttributeUkuran =
        new ProductAttribute(this.ukuran, product, "ukuran", false, Integer.valueOf(2), ProductServiceTest.STORE_ID);
    productAttributeUkuran.setId(this.Ids.get(1));
    ProductAttribute productAttributeBahan =
        new ProductAttribute(this.bahan, product, "bahan", false, Integer.valueOf(3), ProductServiceTest.STORE_ID);
    productAttributeBahan.setId(this.Ids.get(2));
    ProductAttribute productAttributeDeskripsi =
        new ProductAttribute(this.deskripsi, product, "deskripsi", false, Integer.valueOf(4), ProductServiceTest.STORE_ID);
    productAttributeDeskripsi.setId(this.Ids.get(3));
    ProductAttributeValue pavDeskripsi =
        new ProductAttributeValue(productAttributeDeskripsi, null, ProductServiceTest.DESKRIPSI_BARANG,
            DescriptiveAttributeValueType.SINGLE);
    pavDeskripsi.setId("id-1");

    productAttributeDeskripsi.getProductAttributeValues().add(pavDeskripsi);

    ProductAttribute productAttributeOS =
        new ProductAttribute(this.os, product, "OS", false, 1, ProductServiceTest.STORE_ID);
    productAttributeOS.setId(this.Ids.get(4));
    List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues =
        new ArrayList<PredefinedAllowedAttributeValue>();
    for (int i = 0; i < this.strOS.length; i++) {
      PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
          new PredefinedAllowedAttributeValue(this.os, this.strOS[i], ProductServiceTest.STORE_ID, i + 1);
      predefinedAllowedAttributeValue.setId(this.strOS[i]);
      ProductAttributeValue productAttributeValue =
          new ProductAttributeValue(productAttributeOS, null, null, DescriptiveAttributeValueType.PREDEFINED);
      productAttributeValue.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
      productAttributeValue.setId("id" + i);
      predefinedAllowedAttributeValues.add(predefinedAllowedAttributeValue);
      productAttributeOS.getProductAttributeValues().add(productAttributeValue);
    }
    productAttributes.add(productAttributeWarna);
    productAttributes.add(productAttributeUkuran);
    productAttributes.add(productAttributeBahan);
    productAttributes.add(productAttributeDeskripsi);
    productAttributes.add(productAttributeOS);
    product.setProductAttributes(productAttributes);

    List<AllowedAttributeValue> allowedAttributeWarna = new ArrayList<AllowedAttributeValue>();
    for (int i = 0; i < this.strWarna.length; i++) {
      AllowedAttributeValue allowedAttributeValue =
          new AllowedAttributeValue(this.warna, this.strWarna[i], ProductServiceTest.STORE_ID, Integer.valueOf(i + 1));
      allowedAttributeValue.setId(this.strWarnaId[i]);
      allowedAttributeWarna.add(allowedAttributeValue);
      ProductAttributeValue pavWarna =
          new ProductAttributeValue(productAttributeWarna, allowedAttributeValue, null,
              DescriptiveAttributeValueType.NONE);
      pavWarna.setId("id-2");
      productAttributeWarna.getProductAttributeValues().add(pavWarna);
    }
    List<AllowedAttributeValue> allowedAttributeUkuran = new ArrayList<AllowedAttributeValue>();
    for (int i = 0; i < this.strUkuran.length; i++) {
      AllowedAttributeValue allowedAttributeValue =
          new AllowedAttributeValue(this.ukuran, this.strUkuran[i], ProductServiceTest.STORE_ID, Integer.valueOf(i + 1));
      allowedAttributeValue.setId(this.strUkuranId[i]);
      allowedAttributeUkuran.add(allowedAttributeValue);
      ProductAttributeValue pavUkuran =
          new ProductAttributeValue(productAttributeUkuran, allowedAttributeValue, null,
              DescriptiveAttributeValueType.NONE);
      pavUkuran.setId("id-3");
      productAttributeUkuran.getProductAttributeValues().add(pavUkuran);
    }
    List<AllowedAttributeValue> allowedAttributeBahan = new ArrayList<AllowedAttributeValue>();
    for (int i = 0; i < this.strUkuran.length; i++) {
      AllowedAttributeValue allowedAttributeValue =
          new AllowedAttributeValue(this.bahan, this.strBahan[i], ProductServiceTest.STORE_ID, Integer.valueOf(i + 1));
      allowedAttributeValue.setId(this.strBahanId[i]);
      allowedAttributeBahan.add(allowedAttributeValue);
      ProductAttributeValue pavBahan =
          new ProductAttributeValue(productAttributeBahan, allowedAttributeValue, null,
              DescriptiveAttributeValueType.NONE);
      pavBahan.setId("id-4");
      productAttributeBahan.getProductAttributeValues().add(pavBahan);
    }

    ProductAttributeValue markdeleted =
        new ProductAttributeValue(productAttributeWarna, new AllowedAttributeValue(this.warna, "Abu - Abu",
            ProductServiceTest.STORE_ID, Integer.valueOf(10)), null, DescriptiveAttributeValueType.NONE);
    markdeleted.getAllowedAttributeValue().setId("allowedAttributeValueWarnaId3");
    markdeleted.setMarkForDelete(true);
    markdeleted.setId("id-5");
    productAttributeWarna.getProductAttributeValues().add(markdeleted);

    List<AllowedAttributeValue> allowedAttributeDeskripsi = new ArrayList<AllowedAttributeValue>();
    allowedAttributeDeskripsi.add(new AllowedAttributeValue(this.deskripsi, ProductServiceTest.DESKRIPSI_BARANG,
        ProductServiceTest.STORE_ID, Integer.valueOf(1)));

    this.ukuran.setAllowedAttributeValues(allowedAttributeUkuran);
    this.warna.setAllowedAttributeValues(allowedAttributeWarna);
    this.bahan.setAllowedAttributeValues(allowedAttributeBahan);
    this.deskripsi.setAllowedAttributeValues(allowedAttributeDeskripsi);
    when(attributeService.getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, warna.getId()))
        .thenReturn(warna);
    when(attributeService.getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, ukuran.getId()))
        .thenReturn(ukuran);
    when(attributeService.getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, bahan.getId()))
        .thenReturn(bahan);
    when(attributeService.getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, deskripsi.getId()))
        .thenReturn(deskripsi);
    return product;
  }

  private Category createDefaultCategory() {
    return new Category(ProductServiceTest.STORE_ID, ProductServiceTest.CATEGORY_1_NAME, 1);
  }

  private Product createDefaultProduct() {
    return new Product.Builder().productCode(PRODUCT_CODE).name(PRODUCT_1_NAME).length(ORIGINAL_LENGTH)
        .width(ORIGINAL_WIDTH).height(ORIGINAL_HEIGHT).weight(ORIGINAL_WEIGHT).shippingWeight(ORIGINAL_SHIPPING_WEIGHT)
        .brand(ORIGINAL_BRAND).uniqueSellingPoint(UNIQUE_SELLING_POINT).uom(null).storeId(STORE_ID)
        .promoSKU(false).uom(UOM).build();
  }

  private Product generateProduct() throws Exception {
    Product product = new Product();
    product.setReviewPending(true);
    product.setId(ProductServiceTest.DEFAULT_PRODUCT_ID);
    product.setProductCode(ProductServiceTest.DEFAULT_PRODUCT_CODE);
    product.setName(ProductServiceTest.DEFAULT_PRODUCT_NAME);
    product.setProductAttributes(new ArrayList<>());
    product.setProductCategories(new ArrayList<>());
    product.setProductImages(new ArrayList<>());
    product.setProductItems(new ArrayList<>());
    product.setVersion(0L);
    ProductAttribute productAttribute1 = new ProductAttribute();
    productAttribute1.setId(PRODUCT_ATTRIBUTE_ID_1);
    productAttribute1.setProductAttributeValues(new ArrayList<>());
    Attribute attribute1 = new Attribute();
    attribute1.setId(ATTRIBUTE_ID_1);
    attribute1.setName(ATTRIBUTE_NAME_1);
    attribute1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute1.setAttributeCode(ProductServiceTest.DEFAULT_ATTRIBUTE_CODE);
    productAttribute1.setAttribute(attribute1);
    ProductAttributeValue productAttributeValue1 = new ProductAttributeValue();
    productAttributeValue1.setId(PRODUCT_ATTRIBUTE_VALUE_ID_1);
    productAttributeValue1.setDescriptiveAttributeValue("VALUE");
    productAttributeValue1.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    productAttributeValue1.setProductAttribute(productAttribute1);
    ProductAttributeValue productAttributeValue1_1 = new ProductAttributeValue();
    productAttributeValue1_1.setId(PRODUCT_ATTRIBUTE_VALUE_ID_1_1);
    productAttributeValue1_1.setMarkForDelete(true);
    productAttributeValue1_1.setProductAttribute(productAttribute1);
    productAttributeValue1_1.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    productAttribute1.getProductAttributeValues().add(productAttributeValue1);
    productAttribute1.getProductAttributeValues().add(productAttributeValue1_1);
    ProductAttribute productAttribute2 = new ProductAttribute();
    productAttribute2.setId(PRODUCT_ATTRIBUTE_ID_2);
    productAttribute2.setProductAttributeValues(new ArrayList<>());
    Attribute attribute2 = new Attribute();
    attribute2.setId(ATTRIBUTE_ID_2);
    attribute2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attribute2.setAttributeCode(ProductServiceTest.DEFAULT_ATTRIBUTE_CODE_2);
    attribute2.setName(ATTRIBUTE_NAME_2);
    productAttribute2.setAttribute(attribute2);
    ProductAttributeValue productAttributeValue2 = new ProductAttributeValue();
    productAttributeValue2.setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValue());
    productAttributeValue2.getPredefinedAllowedAttributeValue().setValue("VALUE");
    productAttributeValue2.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
    productAttributeValue2.setId(PRODUCT_ATTRIBUTE_VALUE_ID_2);
    productAttributeValue2.setProductAttribute(productAttribute2);
    productAttribute2.getProductAttributeValues().add(productAttributeValue2);
    ProductAttribute productAttribute3 = new ProductAttribute();
    productAttribute3.setId(PRODUCT_ATTRIBUTE_ID_3);
    productAttribute3.setProductAttributeValues(new ArrayList<>());
    Attribute attribute3 = new Attribute();
    attribute3.setId(ATTRIBUTE_ID_3);
    attribute3.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute3.setAttributeCode(ProductServiceTest.DEFAULT_ATTRIBUTE_CODE_3);
    attribute3.setName(ATTRIBUTE_NAME_3);
    productAttribute3.setAttribute(attribute3);
    ProductAttributeValue productAttributeValue3 = new ProductAttributeValue();
    productAttributeValue3.setAllowedAttributeValue(new AllowedAttributeValue());
    productAttributeValue3.setId(PRODUCT_ATTRIBUTE_VALUE_ID_3);
    productAttributeValue3.setProductAttribute(productAttribute3);
    productAttributeValue3.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    productAttribute3.getProductAttributeValues().add(productAttributeValue3);
    ProductAttribute productAttribute4 = new ProductAttribute();
    productAttribute4.setMarkForDelete(true);
    product.getProductAttributes().add(productAttribute1);
    product.getProductAttributes().add(productAttribute2);
    product.getProductAttributes().add(productAttribute3);
    product.getProductAttributes().add(productAttribute4);
    ProductImage productImage1 = new ProductImage();
    productImage1.setId(ProductServiceTest.DEFAULT_IMAGE_ID + "1");
    productImage1.setOriginalImage(Boolean.TRUE);
    productImage1.setLocationPath(LOCATION_PATH_1);
    ProductImage productImage2 = new ProductImage();
    productImage2.setMarkForDelete(true);
    productImage2.setOriginalImage(Boolean.FALSE);
    productImage2.setId(ProductServiceTest.DEFAULT_IMAGE_ID + "2");
    productImage2.setLocationPath(LOCATION_PATH);
    product.getProductImages().add(productImage1);
    product.getProductImages().add(productImage2);
    ProductItem productItem1 = new ProductItem();
    productItem1.setGeneratedItemName(ProductServiceTest.DEFAULT_PRODUCT_NAME + " 1");
    productItem1.setSkuCode(ProductServiceTest.DEFAULT_SKU_CODE);
    productItem1.setProductItemAttributeValues(new ArrayList<>());
    productItem1.setProductItemImages(new ArrayList<>());
    ProductItemAttributeValue productItemAttributeValue1 = new ProductItemAttributeValue();
    productItemAttributeValue1.setAttribute(attribute2);
    ProductItemAttributeValue productItemAttributeValue2 = new ProductItemAttributeValue();
    productItemAttributeValue2.setMarkForDelete(true);
    ProductItemAttributeValue productItemAttributeValue3 = new ProductItemAttributeValue();
    productItemAttributeValue3.setAttribute(attribute3);
    productItemAttributeValue3.setValue("VALUE");
    ProductItemAttributeValue productItemAttributeValue4 = new ProductItemAttributeValue();
    productItemAttributeValue4.setAttribute(attribute3);
    productItemAttributeValue4.setValue("VALUE 1");
    productItem1.getProductItemAttributeValues().add(productItemAttributeValue1);
    productItem1.getProductItemAttributeValues().add(productItemAttributeValue2);
    productItem1.getProductItemAttributeValues().add(productItemAttributeValue3);
    productItem1.getProductItemAttributeValues().add(productItemAttributeValue4);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setId(ProductServiceTest.DEFAULT_IMAGE_ID + "1");
    productItemImage1.setOriginalImage(Boolean.TRUE);
    productItemImage1.setLocationPath(LOCATION_PATH_1);
    productItemImage1.setLocationPath(LOCATION_PATH);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setId(ProductServiceTest.DEFAULT_IMAGE_ID + "2");
    productItemImage2.setMarkForDelete(true);
    productItemImage2.setLocationPath(LOCATION_PATH1);
    productItemImage2.setOriginalImage(Boolean.FALSE);
    productItemImage2.setLocationPath(LOCATION_PATH_2);
    productItem1.getProductItemImages().add(productItemImage1);
    productItem1.getProductItemImages().add(productItemImage2);
    ProductItem productItem2 = new ProductItem();
    productItem2.setMarkForDelete(true);
    productItem2.setSkuCode(ITEM_SKU_CODE_2);
    product.getProductItems().add(productItem1);
    product.getProductItems().add(productItem2);
    product.setCreatedMerchant(DEFAULT_MERCHANT_CODE );
    return product;
  }

  @BeforeEach
  public void initialize() throws Exception {
    MockitoAnnotations.initMocks(this);
    for (int i = 0; i < 10; i++) {
      this.Ids.add("id" + i);
    }

    productItem = new ProductItem();
    productItem.setMarkForDelete(false);

    this.product = this.createDefaultProduct();
    product.setId(PRODUCT_ID);
    this.product.setViewable(true);
    this.product.setActivated(true);
    this.product.setProductItems(new ArrayList<>());
    product.getProductItems().add(productItem);
    this.products = new ArrayList<>();
    this.products.add(product);
    this.productPage = new PageImpl<>(this.products, ProductServiceTest.DEFAULT_PAGEABLE, this.products.size());

    categoryCountMap = new HashMap<>();
    categoryCountMap.put(CATEGORY_ID, 1L);
    category = new Category();
    category.setId(CATEGORY_ID);
    category.setCategoryCode(CATEGORY_ID);
    Catalog catalog = new Catalog();
    catalog.setCatalogType(CatalogType.MASTER_CATALOG);
    category.setCatalog(catalog);

    category1 = new Category();
    category1.setCategoryCode(CATEGORY_CODE_1);
    category1.setId(CATEGORY_ID_1);
    category1.setActivated(true);

    category2 = new Category();
    category2.setCategoryCode(CATEGORY_CODE_2);
    category2.setId(CATEGORY_ID_2);
    category2.setActivated(true);

    attribute1 = new Attribute();
    attribute1.setId(ATTRIBUTE_ID_1);
    attribute1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute1.setAttributeCode(ProductServiceTest.DEFAULT_ATTRIBUTE_CODE);
    attribute1.setName(ATTRIBUTE_NAME_1);

    attribute2 = new Attribute();
    attribute2.setId(ATTRIBUTE_ID_2);
    attribute2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attribute2.setAttributeCode(ProductServiceTest.DEFAULT_ATTRIBUTE_CODE_2);
    attribute2.setName(ATTRIBUTE_NAME_2);

    attribute3 = new Attribute();
    attribute3.setId(ATTRIBUTE_ID_3);
    attribute3.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute3.setAttributeCode(ProductServiceTest.DEFAULT_ATTRIBUTE_CODE_3);
    attribute3.setName(ATTRIBUTE_NAME_3);

    attribute4 = new Attribute();
    attribute4.setId(ATTRIBUTE_ID_4);
    attribute4.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attribute4.setAttributeCode(ProductServiceTest.DEFAULT_ATTRIBUTE_CODE_4);
    attribute4.setName(ATTRIBUTE_NAME_4);

    brandAttribute = new Attribute();
    brandAttribute.setName(BRAND_ATTRIBUTE_NAME);
    brandAttribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);

    brand = new Brand();
    brand.setBrandCode(BRAND_ATTRIBUTE_CODE);
    brand.setBrandName(BRAND_ATTRIBUTE_NAME);

    oldBrand = new BrandResponse();
    oldBrand.setBrandCode(BRAND_ATTRIBUTE_CODE);
    oldBrand.setBrandName(BRAND_ATTRIBUTE_NAME);

    newBrand = new BrandResponse();
    newBrand.setBrandCode(NEW_BRAND_CODE);
    newBrand.setBrandName(NEW_BRAND_NAME);

    productBrandUpdateDTO = new ProductBrandUpdateDTO();
    productBrandUpdateDTO.setProductCode(PRODUCT_CODE);
    productBrandUpdateDTO.setOldBrandCode(BRAND_ATTRIBUTE_CODE);
    productBrandUpdateDTO.setNewBrandCode(NEW_BRAND_CODE);

    updatedBrandAttribute = new PredefinedAllowedAttributeValue();
    updatedBrandAttribute.setPredefinedAllowedAttributeCode(BRAND_ATTRIBUTE_NAME);
    updatedBrandAttribute.setValue(BRAND);

    PredefinedAllowedAttributeValue originalBrandAttribute = new PredefinedAllowedAttributeValue();
    originalBrandAttribute.setPredefinedAllowedAttributeCode(BRAND_ATTRIBUTE_NAME);
    originalBrandAttribute.setValue(ORIGINAL_BRAND);

    brandProductAttribute = new ProductAttribute();
    brandProductAttribute.setProductAttributeName(BRAND_ATTRIBUTE_NAME);
    brandProductAttribute.setAttribute(brandAttribute);

    ProductAttributeValue brandProductAttributeValue = new ProductAttributeValue();
    brandProductAttributeValue.setPredefinedAllowedAttributeValue(originalBrandAttribute);
    brandProductAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    brandProductAttribute.setProductAttributeValues(new ArrayList<>());
    brandProductAttribute.getProductAttributeValues().add(brandProductAttributeValue);

    ProductItem productItem1 = new ProductItem();
    productItem1.setId(PRODUCT_ITEM_ID_1);
    productItem1.setSkuCode(ITEM_SKU_CODE_1);
    productItem1.setGeneratedItemName(PRODUCT_NAME + ITEM_NAME);
    productItem1.setDangerousGoodsLevel(0);
    productItem1.setProductItemAttributeValues(new ArrayList<>());
    productItem1.setProductItemImages(new ArrayList<>());
    ProductItemAttributeValue productItemAttributeValue1 = new ProductItemAttributeValue();
    productItemAttributeValue1.setAttribute(attribute1);
    productItemAttributeValue1.setValue(PRODUCT_ITEM_ATTRIBUTE_VALUE_1);
    ProductItemAttributeValue productItemAttributeValue2 = new ProductItemAttributeValue();
    productItemAttributeValue2.setMarkForDelete(true);
    productItemAttributeValue2.setValue(PRODUCT_ITEM_ATTRIBUTE_VALUE_2);
    productItemAttributeValue2.setAttribute(attribute2);
    ProductItemAttributeValue productItemAttributeValue3 = new ProductItemAttributeValue();
    productItemAttributeValue3.setAttribute(attribute2);
    productItemAttributeValue3.setValue(PRODUCT_ITEM_ATTRIBUTE_VALUE_3);
    ProductItemAttributeValue productItemAttributeValue4 = new ProductItemAttributeValue();
    productItemAttributeValue4.setAttribute(attribute3);
    productItemAttributeValue4.setValue(PRODUCT_ITEM_ATTRIBUTE_VALUE_4);
    ProductItemAttributeValue brandProductItemAttributeValue = new ProductItemAttributeValue();
    brandProductItemAttributeValue.setAttribute(brandAttribute);
    brandProductItemAttributeValue.setValue(ORIGINAL_BRAND);
    productItem1.getProductItemAttributeValues().add(productItemAttributeValue1);
    productItem1.getProductItemAttributeValues().add(productItemAttributeValue2);
    productItem1.getProductItemAttributeValues().add(productItemAttributeValue3);
    productItem1.getProductItemAttributeValues().add(productItemAttributeValue4);
    productItem1.getProductItemAttributeValues().add(brandProductItemAttributeValue);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setId(ProductServiceTest.DEFAULT_IMAGE_ID + "1");
    productItemImage1.setOriginalImage(Boolean.TRUE);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setId(ProductServiceTest.DEFAULT_IMAGE_ID + "2");
    productItemImage2.setMarkForDelete(true);
    productItemImage2.setOriginalImage(Boolean.FALSE);
    productItem1.getProductItemImages().add(productItemImage1);
    productItem1.getProductItemImages().add(productItemImage2);

    ProductItem productItem2 = new ProductItem();
    productItem2.setId(PRODUCT_ITEM_ID_2);
    productItem2.setSkuCode(ITEM_SKU_CODE_2);
    productItem2.setMarkForDelete(true);
    productItem1.setDangerousGoodsLevel(0);

    productItems = new ArrayList<>(Arrays.asList(productItem1, productItem2));

    ProductImage productImage1 = new ProductImage();
    productImage1.setId(PRODUCT_IMAGE_ID_1);
    productImage1.setOriginalImage(true);
    productImage1.setMainImages(true);
    productImage1.setLocationPath(LOCATION_PATH);

    ProductImage productImage2 = new ProductImage();
    productImage2.setOriginalImage(false);
    productImage2.setMainImages(false);
    productImage2.setId(PRODUCT_IMAGE_ID_2);
    productImage2.setSequence(0);
    productImage2.setLocationPath(LOCATION_PATH_2);

    ProductImage productImage3 = new ProductImage();
    productImage3.setId(PRODUCT_IMAGE_ID_3);
    productImage3.setMainImages(true);
    productImage3.setOriginalImage(false);
    productImage3.setActive(true);
    productImage3.setLocationPath(LOCATION_PATH_3);

    ProductImage productImage4 = new ProductImage();
    productImage4.setId(PRODUCT_IMAGE_ID_4);
    productImage4.setOriginalImage(false);
    productImage4.setMarkForDelete(true);
    productImage4.setLocationPath(LOCATION_PATH_4);

    productImages = new ArrayList<>(Arrays.asList(productImage1, productImage2, productImage3, productImage4));

    productAttribute1 = new ProductAttribute();
    productAttribute1.setId(PRODUCT_ATTRIBUTE_ID_1);
    productAttribute1.setAttributeId(ATTRIBUTE_ID_1);
    productAttribute1.setAttribute(attribute1);

    ProductAttributeValue productAttributeValue1 = new ProductAttributeValue();
    productAttributeValue1.setId(PRODUCT_ATTRIBUTE_VALUE_ID_1);
    productAttributeValue1.setDescriptiveAttributeValue(PRODUCT_ATTRIBUTE_VALUE_1);
    productAttributeValue1.setProductAttributeId(PRODUCT_ATTRIBUTE_ID_1);

    ProductAttributeValue productAttributeValue1_1 = new ProductAttributeValue();
    productAttributeValue1_1.setId(PRODUCT_ATTRIBUTE_VALUE_ID_1_1);
    productAttributeValue1_1.setMarkForDelete(true);
    productAttributeValue1_1.setProductAttributeId(PRODUCT_ATTRIBUTE_ID_1);

    productAttributeValues1 = new ArrayList<>(Arrays.asList(productAttributeValue1, productAttributeValue1_1));

    productAttribute2 = new ProductAttribute();
    productAttribute2.setId(PRODUCT_ATTRIBUTE_ID_2);
    productAttribute2.setAttributeId(ATTRIBUTE_ID_2);
    productAttribute2.setAttribute(attribute2);

    ProductAttributeValue productAttributeValue2 = new ProductAttributeValue();
    productAttributeValue2.setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValue());
    productAttributeValue2.getPredefinedAllowedAttributeValue().setValue(PRODUCT_ATTRIBUTE_VALUE_2);
    productAttributeValue2.setId(PRODUCT_ATTRIBUTE_VALUE_ID_2);
    productAttributeValue2.setProductAttributeId(PRODUCT_ATTRIBUTE_ID_2);

    productAttributeValues2 = new ArrayList<>();
    productAttributeValues2.add(productAttributeValue2);

    productAttribute3 = new ProductAttribute();
    productAttribute3.setId(PRODUCT_ATTRIBUTE_ID_3);
    productAttribute3.setAttributeId(ATTRIBUTE_ID_3);
    productAttribute3.setAttribute(attribute3);

    ProductAttributeValue productAttributeValue3 = new ProductAttributeValue();
    productAttributeValue3.setAllowedAttributeValue(new AllowedAttributeValue());
    productAttributeValue3.setId(PRODUCT_ATTRIBUTE_VALUE_ID_3);
    productAttributeValue3.setProductAttributeId(PRODUCT_ATTRIBUTE_ID_3);

    productAttributeValues3 = new ArrayList<>();
    productAttributeValues3.add(productAttributeValue3);


    productAttribute4 = new ProductAttribute();
    productAttribute4.setId(PRODUCT_ATTRIBUTE_ID_4);
    productAttribute4.setMarkForDelete(true);
    productAttribute4.setAttributeId(ATTRIBUTE_ID_3);
    productAttribute4.setAttribute(new Attribute());
    productAttribute4.setProductAttributeValues(new ArrayList<>());


    productAttributes = new ArrayList<>();
    productAttributes.add(productAttribute1);
    productAttributes.add(productAttribute2);
    productAttributes.add(productAttribute3);
    productAttributes.add(productAttribute4);
    productAttribute1.setProductAttributeValues(productAttributeValues1);
    productAttribute2.setProductAttributeValues(productAttributeValues2);
    productAttribute3.setProductAttributeValues(productAttributeValues3);

    ProductCategory productCategory1 = new ProductCategory();
    productCategory1.setCategoryId(CATEGORY_ID_1);

    ProductCategory productCategory2 = new ProductCategory();
    productCategory2.setCategoryId(CATEGORY_ID_2);
    productCategory2.setMarkForDelete(true);

    productCategories = new ArrayList<>(Arrays.asList(productCategory1, productCategory2));

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
    categoryRequest.setCategoryCode("CA-404040");
    categoryRequest.setProductCategories(Arrays.asList(productCategoryRequest));
    categoryRequest.setSalesCategoryReferences(Collections.emptyList());
    productCategoryRequest.setCategory(categoryRequest);
    productRequest.setProductCategories(Arrays.asList(productCategoryRequest));

    warehouseMasterSKUEvent = WarehouseMasterSKUEvent.builder().itemCode(PRODUCT_CODE).height(HEIGHT)
        .length(LENGTH).weight(WEIGHT).width(WIDTH).upcCodes(Collections.singletonList(UPC_CODE)).build();

    when(applicationContext.getBean(ProductService.class)).thenReturn(productService);
    when(productService.getProductByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID)).thenReturn(product);
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    when(repository.findByStoreIdAndId(STORE_ID, PRODUCT_ID)).thenReturn(product);
    when(repository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    when(repository.findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, PRODUCT_ID)).thenReturn(product);
    when(productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCategories);
    when(categoryService.getCategoryByStoreIdAndIdCached(DEFAULT_STORE_ID, CATEGORY_ID_1))
        .thenReturn(category1);
    when(categoryService.getCategoryByStoreIdAndIdCached(DEFAULT_STORE_ID, CATEGORY_ID_2))
        .thenReturn(category2);
    when(categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, CATEGORY_CODE_1))
        .thenReturn(category1);
    when(categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, CATEGORY_CODE_2))
        .thenReturn(category2);
    when(productAttributeService.getProductAttributesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productAttributes);
    when(attributeService.getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, ATTRIBUTE_ID_1))
        .thenReturn(attribute1);
    when(attributeService.getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, ATTRIBUTE_ID_2))
        .thenReturn(attribute2);
    when(attributeService.getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, ATTRIBUTE_ID_3))
        .thenReturn(attribute3);
    when(attributeService.getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, ATTRIBUTE_ID_4))
        .thenReturn(attribute4);
    when(imageService.getProductImagesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID)).thenReturn(productImages);
    when(imageService.getProductImagesByStoreIdAndProductId(DEFAULT_STORE_ID, PRODUCT_ID)).thenReturn(productImages);
    when(brandService.findByBrandName(BRAND, false)).thenReturn(brand);
    when(predefinedAllowedAttributeValueService
        .findByStoreIdAndPredefinedAllowedAttributeCode(STORE_ID, BRAND_ATTRIBUTE_CODE)).thenReturn(updatedBrandAttribute);
    when(this.repository.findById(PRODUCT_ID)).thenReturn(Optional.of(product));

    category1 = new Category();
    category1.setId(CATEGORY_ID_1);
    category1.setCategoryCode(CATEGORY_CODE_1);
    catalog.setCatalogType(CatalogType.MASTER_CATALOG);
    category1.setCatalog(catalog);
    when(this.repository.countByStoreIdAndProductCode(ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE))
        .thenReturn(1L);
    when(
        this.repository
            .findByStoreIdAndNameContainingIgnoreCaseAndCreatedByContainingIgnoreCaseAndMarkForDeleteFalseOrderByUpdatedDateDesc(
                ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_1_NAME, ProductServiceTest.CREATED_BY))
        .thenReturn(this.products);
    when(
        this.repository
            .findByStoreIdAndNameContainingIgnoreCaseAndCreatedByContainingIgnoreCaseAndMarkForDeleteFalseOrderByUpdatedDateDesc(
                ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_1_NAME, ProductServiceTest.CREATED_BY,
                ProductServiceTest.DEFAULT_PAGEABLE)).thenReturn(this.productPage);
    when(
        this.repository
            .findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseAndNameContainingIgnoreCaseOrderByUpdatedDateDesc(
                ProductServiceTest.STORE_ID, true, true, ProductServiceTest.PRODUCT_1_NAME)).thenReturn(this.products);
    when(
        this.repository
            .findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseAndNameContainingIgnoreCaseOrderByUpdatedDateDesc(
                ProductServiceTest.STORE_ID, true, true, ProductServiceTest.PRODUCT_1_NAME,
                ProductServiceTest.DEFAULT_PAGEABLE)).thenReturn(this.productPage);
    when(
        this.repository.findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseAndNameContainingIgnoreCaseAndUpdatedByContainingIgnoreCaseOrderByUpdatedDateDesc(
                ProductServiceTest.STORE_ID, true, true,ProductServiceTest.PRODUCT_1_NAME,
                ProductServiceTest.UPDATED_BY)).thenReturn(this.products);
    when(
        this.repository
            .findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseAndNameContainingIgnoreCaseAndUpdatedByContainingIgnoreCaseOrderByUpdatedDateDesc(
                ProductServiceTest.STORE_ID, true, true, ProductServiceTest.PRODUCT_1_NAME,
                ProductServiceTest.UPDATED_BY, ProductServiceTest.DEFAULT_PAGEABLE)).thenReturn(this.productPage);
    when(
        this.repository.findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseOrderByUpdatedDateDesc(
            ProductServiceTest.STORE_ID, true, true)).thenReturn(this.products);
    when(
        this.repository.findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseOrderByUpdatedDateDesc(
            ProductServiceTest.STORE_ID, true, true, ProductServiceTest.DEFAULT_PAGEABLE)).thenReturn(this.productPage);
    when(this.domainEventPublisherService.publishProduct((Product) any())).thenReturn(null);
    when(
        this.repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(anyString(),
            eq(ProductServiceTest.DEFAULT_PRODUCT_CODE))).thenReturn(generateProduct());
    when(this.repository.saveAndFlush((Product) any())).thenReturn(null);

    doNothing().when(productAttributeExtractionService)
        .addProductsToProductAttributeExtraction(anyString(), any(Category.class));

    this.replaceProductImages = new ReplaceProductImagesDTO();
    this.replaceProductImages.setProductCode(PRODUCT_CODE);
    this.replaceProductImages.setGeneratedImageCount(2);
    this.replaceProductImages.setProductItem(new ArrayList<ReplaceProductItemImagesDTO>());
    this.replaceProductImages.getProductItem().add(new ReplaceProductItemImagesDTO(PRODUCT_CODE, 3));

    productSalesCategoryMapping = new ProductSalesCategoryMapping(Collections.singletonList(OLD_SALES_CATEGORY),
        Collections.singletonList(NEW_SALES_CATEGORY), Collections.singletonList(NEW_UMKM_SALES_CATEGORY),
        new ArrayList<>(), new ArrayList<>(),true);

    ReflectionTestUtils.setField(service, "imageSourceDirectory", BASE_FOLDER);
    ReflectionTestUtils.setField(service, "fullImageSourceDirectory", FOLDER1 + "/");
    ReflectionTestUtils.setField(service, "mediumImageSourceDirectory", FOLDER2);
    ReflectionTestUtils.setField(service, "thumbnailImageSourceDirectory", FOLDER3);
    ReflectionTestUtils.setField(service, "setCommonImageAsMainImage", false);
    ReflectionTestUtils.setField(service, "productBasicInfoFetchBatchSize", 10);

    productCodes.add(PRODUCT_CODE1);
    productCodes.add(PRODUCT_CODE2);

    productList = new ArrayList<>();
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE1);
    ProductImage productImage = new ProductImage();
    productImage.setLocationPath(PRODUCT_CODE1 + File.separator + FILE_NAME1);
    product.setProductImages(Arrays.asList(productImage));
    Product product1 = new Product();
    product1.setProductCode(PRODUCT_CODE2);
    ProductImage productImage_1 = new ProductImage();
    productImage_1.setLocationPath(PRODUCT_CODE2 + File.separator + FILE_NAME2);
    product1.setProductImages(Arrays.asList(productImage_1));
    productList.add(product);
    productList.add(product1);

    definingProductAttribute = new ProductAttribute();
    Attribute definingAttribute = new Attribute();
    definingAttribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    definingProductAttribute.setAttribute(definingAttribute);
    definingProductAttribute.setProductAttributeName(DEFINING_ATTRIBUTE_NAME);
    definingProductAttribute.setMarkForDelete(false);

    definingProductAttribute1 = new ProductAttribute();
    definingProductAttribute1.setAttribute(definingAttribute);
    definingProductAttribute1.setProductAttributeName(DEFINING_ATTRIBUTE_NAME_TRUE);
    definingProductAttribute1.setMarkForDelete(true);
    ProductImageUtil.setFileStorageService(fileStorageService);
  }

  private Product generateProductRequest() {
    Product product = this.createDefaultProduct();
    product.setId(PRODUCT_ID);
    product.setViewable(true);
    product.setActivated(true);

    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE_2);
    category.setId(CATEGORY_ID_3);

    Attribute attribute1 = new Attribute();
    attribute1.setId(ATTRIBUTE_ID_1);
    attribute1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute1.setAttributeCode(ProductServiceTest.DEFAULT_ATTRIBUTE_CODE);

    Attribute attribute2 = new Attribute();
    attribute2.setId(ATTRIBUTE_ID_2);
    attribute2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attribute2.setAttributeCode(ProductServiceTest.DEFAULT_ATTRIBUTE_CODE_2);

    Attribute attribute3 = new Attribute();
    attribute3.setId(ATTRIBUTE_ID_3);
    attribute3.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute3.setAttributeCode(ProductServiceTest.DEFAULT_ATTRIBUTE_CODE_3);

    Attribute attribute4 = new Attribute();
    attribute4.setId(ATTRIBUTE_ID_4);
    attribute4.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attribute4.setAttributeCode(ProductServiceTest.DEFAULT_ATTRIBUTE_CODE_4);

    ProductItem productItem1 = new ProductItem();
    productItem1.setId(PRODUCT_ITEM_ID_1);
    productItem1.setSkuCode(ITEM_SKU_CODE_1);
    productItem1.setGeneratedItemName(ProductServiceTest.DEFAULT_PRODUCT_NAME + " 1");
    productItem1.setProductItemAttributeValues(new ArrayList<>());
    productItem1.setProductItemImages(new ArrayList<>());
    ProductItemAttributeValue productItemAttributeValue1 = new ProductItemAttributeValue();
    productItemAttributeValue1.setAttribute(attribute1);
    productItemAttributeValue1.setValue(PRODUCT_ITEM_ATTRIBUTE_VALUE_1);
    ProductItemAttributeValue productItemAttributeValue2 = new ProductItemAttributeValue();
    productItemAttributeValue2.setMarkForDelete(true);
    productItemAttributeValue2.setValue(PRODUCT_ITEM_ATTRIBUTE_VALUE_2);
    ProductItemAttributeValue productItemAttributeValue3 = new ProductItemAttributeValue();
    productItemAttributeValue3.setAttribute(attribute2);
    productItemAttributeValue3.setValue(PRODUCT_ITEM_ATTRIBUTE_VALUE_3);
    productItemAttributeValue3.setMarkForDelete(true);
    ProductItemAttributeValue productItemAttributeValue4 = new ProductItemAttributeValue();
    productItemAttributeValue4.setAttribute(attribute3);
    productItemAttributeValue4.setValue(PRODUCT_ITEM_ATTRIBUTE_VALUE_5);
    ProductItemAttributeValue productItemAttributeValue5 = new ProductItemAttributeValue();
    productItemAttributeValue5.setAttribute(attribute4);
    productItemAttributeValue5.setValue(PRODUCT_ITEM_ATTRIBUTE_VALUE_5);
    productItem1.getProductItemAttributeValues().add(productItemAttributeValue1);
    productItem1.getProductItemAttributeValues().add(productItemAttributeValue2);
    productItem1.getProductItemAttributeValues().add(productItemAttributeValue3);
    productItem1.getProductItemAttributeValues().add(productItemAttributeValue4);
    productItem1.getProductItemAttributeValues().add(productItemAttributeValue5);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setId(ProductServiceTest.DEFAULT_IMAGE_ID + "1");
    productItemImage1.setOriginalImage(Boolean.TRUE);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setId(ProductServiceTest.DEFAULT_IMAGE_ID + "2");
    productItemImage2.setMarkForDelete(true);
    productItemImage2.setOriginalImage(Boolean.FALSE);
    productItem1.getProductItemImages().add(productItemImage1);
    productItem1.getProductItemImages().add(productItemImage2);

    ProductItem productItem2 = new ProductItem();
    productItem2.setId(PRODUCT_ITEM_ID_2);
    productItem2.setSkuCode(ITEM_SKU_CODE_2);
    productItem2.setMarkForDelete(true);

    List<ProductItem> productItems = new ArrayList<>();
    productItems.add(productItem1);
    productItems.add(productItem2);
    product.setProductItems(productItems);

    ProductImage productImage1 = new ProductImage();
    productImage1.setId(PRODUCT_IMAGE_ID_1);
    productImage1.setLocationPath(LOCATION_PATH_1);
    productImage1.setOriginalImage(true);
    productImage1.setMainImages(true);

    ProductImage productImage2 = new ProductImage();
    productImage2.setOriginalImage(false);
    productImage2.setMainImages(false);
    productImage2.setId(PRODUCT_IMAGE_ID_2);
    productImage2.setLocationPath(LOCATION_PATH_2);
    productImage2.setSequence(0);

    ProductImage productImage3 = new ProductImage();
    productImage3.setOriginalImage(false);
    productImage2.setMainImages(false);
    productImage3.setLocationPath(LOCATION_PATH_3);
    productImage3.setId(PRODUCT_IMAGE_ID_3);

    ProductImage productImage4 = new ProductImage();
    productImage4.setOriginalImage(false);
    productImage4.setId(PRODUCT_IMAGE_ID_4);
    productImage4.setLocationPath(LOCATION_PATH_4);
    productImage4.setMarkForDelete(true);

    List<ProductImage> productImages = new ArrayList<>();
    productImages.add(productImage1);
    productImages.add(productImage2);
    productImages.add(productImage3);
    productImages.add(productImage4);
    product.setProductImages(productImages);

    ProductAttribute productAttribute1 = new ProductAttribute();
    productAttribute1.setId(PRODUCT_ATTRIBUTE_ID_1);
    productAttribute1.setAttributeId(ATTRIBUTE_ID_1);
    attribute1.setAllowedAttributeValues(Arrays.asList(new AllowedAttributeValue()));
    productAttribute1.setAttribute(attribute1);

    ProductAttributeValue productAttributeValue1 = new ProductAttributeValue();
    productAttributeValue1.setId(PRODUCT_ATTRIBUTE_VALUE_ID_1);
    productAttributeValue1.setDescriptiveAttributeValue(PRODUCT_ATTRIBUTE_VALUE_6);
    productAttributeValue1.setProductAttribute(productAttribute1);

    ProductAttributeValue productAttributeValue1_1 = new ProductAttributeValue();
    productAttributeValue1_1.setId(PRODUCT_ATTRIBUTE_VALUE_ID_1_1);
    productAttributeValue1_1.setMarkForDelete(true);
    productAttributeValue1_1.setProductAttributeId(PRODUCT_ATTRIBUTE_ID_1);
    productAttributeValue1_1.setProductAttribute(productAttribute1);

    List<ProductAttributeValue> productAttributeValues1 =
        new ArrayList<>(Arrays.asList(productAttributeValue1, productAttributeValue1_1));
    productAttribute1.setProductAttributeValues(productAttributeValues1);

    ProductAttribute productAttribute2 = new ProductAttribute();
    productAttribute2.setId(PRODUCT_ATTRIBUTE_ID_2);
    productAttribute2.setAttribute(attribute2);

    ProductAttributeValue productAttributeValue2 = new ProductAttributeValue();
    productAttributeValue2.setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValue());
    productAttributeValue2.getPredefinedAllowedAttributeValue().setValue(PRODUCT_ATTRIBUTE_VALUE_3);
    productAttributeValue2.setId(PRODUCT_ATTRIBUTE_VALUE_ID_2);
    productAttributeValue2.setProductAttributeId(PRODUCT_ATTRIBUTE_ID_2);
    productAttributeValue2.setProductAttribute(productAttribute2);

    List<ProductAttributeValue> productAttributeValues2 = new ArrayList<>();
    productAttributeValues2.add(productAttributeValue2);
    productAttribute2.setProductAttributeValues(productAttributeValues2);

    ProductAttribute productAttribute3 = new ProductAttribute();
    productAttribute3.setId(PRODUCT_ATTRIBUTE_ID_3);
    productAttribute3.setAttribute(attribute3);

    ProductAttributeValue productAttributeValue3 = new ProductAttributeValue();
    productAttributeValue3.setAllowedAttributeValue(new AllowedAttributeValue());
    productAttributeValue3.setId(PRODUCT_ATTRIBUTE_VALUE_ID_3);
    productAttributeValue3.setProductAttributeId(PRODUCT_ATTRIBUTE_ID_3);
    productAttributeValue3.setProductAttribute(productAttribute3);

    List<ProductAttributeValue> productAttributeValues3 = new ArrayList<>();
    productAttributeValues3.add(productAttributeValue3);
    productAttribute3.setProductAttributeValues(productAttributeValues3);

    ProductAttribute productAttribute4 = new ProductAttribute();
    productAttribute4.setId(PRODUCT_ATTRIBUTE_ID_4);
    productAttribute4.setAttribute(attribute4);

    ProductAttributeValue productAttributeValue4 = new ProductAttributeValue();
    productAttributeValue4.setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValue());
    productAttributeValue4.setId(PRODUCT_ATTRIBUTE_VALUE_ID_4);
    productAttributeValue4.setProductAttributeId(PRODUCT_ATTRIBUTE_ID_4);
    productAttributeValue4.setProductAttribute(productAttribute4);

    List<ProductAttributeValue> productAttributeValues4 = new ArrayList<>();
    productAttributeValues4.add(productAttributeValue4);
    productAttribute4.setProductAttributeValues(productAttributeValues4);

    List<ProductAttribute> productAttributes = new ArrayList<>(
            Arrays.asList(productAttribute1, productAttribute2, productAttribute3, productAttribute4));
    product.setProductAttributes(productAttributes);

    ProductCategory productCategory1 = new ProductCategory();
    productCategory1.setCategory(category);

    List<ProductCategory> productCategories = new ArrayList<>();
    productCategories.add(productCategory1);
    product.setProductCategories(productCategories);
    product.setCreatedMerchant(DEFAULT_MERCHANT_CODE);
    return product;
  }

  @AfterEach
  public void postTest() {
    verifyNoMoreInteractions(this.repository);
    verifyNoMoreInteractions(this.productItemService);
    verifyNoMoreInteractions(this.productAttributeValueService);
    verifyNoMoreInteractions(this.productAttributeService);
    verifyNoMoreInteractions(this.categoryService);
    verifyNoMoreInteractions(this.attributeService);
    verifyNoMoreInteractions(this.applicationCacheServiceBean);
    verifyNoMoreInteractions(this.allowedAttributeValueService);
    verifyNoMoreInteractions(this.predefinedAllowedAttributeValueService);
    verifyNoMoreInteractions(this.categoryReferenceService);
    verifyNoMoreInteractions(productCategoryService);
    verifyNoMoreInteractions(imageService);
    verifyNoMoreInteractions(productItemServiceWrapper);
    verifyNoMoreInteractions(applicationContext);
    verifyNoMoreInteractions(domainEventPublisherService);
    verifyNoMoreInteractions(brandService);
    verifyNoMoreInteractions(categoryShippingService);
    verifyNoMoreInteractions(productImageCleanupRepository);
    verifyNoMoreInteractions(productAttributeExtractionService);
  }

  @Test
  public void testActivateProduct() throws Exception {
    Product savedProduct = new Product();
    BeanUtils.copyProperties(product, savedProduct);
    when(this.repository.findById(PRODUCT_ID)).thenReturn(Optional.of(savedProduct));

    this.service.activateProduct(ProductServiceTest.STORE_ID, PRODUCT_ID);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productItemServiceWrapper).setProductItemsCached(STORE_ID, product, false);
    verify(repository).findById(PRODUCT_ID);
    verify(repository).saveAndFlush(product);
    verify(applicationCacheServiceBean).evictProductCacheByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(applicationCacheServiceBean).evictProductCacheByStoreIdAndProductCode(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductItemsCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    assertTrue(product.isActivated());
    assertTrue(product.getProductItems().get(0).isActivated());
  }


  @Test
  public void testCountByProductCode() throws Exception {
    long count = this.service.countByProductCode(ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE);

    assertEquals(count, 1);

    count = this.service.countByProductCode(ProductServiceTest.STORE_ID, ProductServiceTest.WRONG_PRODUCT_CODE);

    assertEquals(count, 0);

    verify(this.repository, times(1)).countByStoreIdAndProductCode(ProductServiceTest.STORE_ID,
        ProductServiceTest.PRODUCT_CODE);
    verify(this.repository, times(1)).countByStoreIdAndProductCode(ProductServiceTest.STORE_ID,
        ProductServiceTest.WRONG_PRODUCT_CODE);
  }


  @Test
  public void testDeactivateProduct() throws Exception {
    Product savedProduct = new Product();
    BeanUtils.copyProperties(product, savedProduct);
    when(this.repository.findById(product.getId())).thenReturn(Optional.of(savedProduct));

    this.service.deactivateProduct(ProductServiceTest.STORE_ID, PRODUCT_ID);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productItemServiceWrapper).setProductItemsCached(STORE_ID, product, false);
    verify(repository).findById(PRODUCT_ID);
    verify(repository).saveAndFlush(product);
    verify(applicationCacheServiceBean).evictProductCacheByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(applicationCacheServiceBean).evictProductCacheByStoreIdAndProductCode(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductItemsCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Assertions.assertFalse(product.isActivated());
    Assertions.assertFalse(product.getProductItems().get(0).isActivated());
  }

  @Test
  public void testDeleteProductItemsByProductAttributes() throws Exception {
    Product product = this.createAdvancedProduct();
    this.service.sortAndGenerateProductItem(product);
    List<ProductAttributeValue> productAttributeValues = new ArrayList<>();
    productAttributeValues.add(product.getProductAttributes().get(0).getProductAttributeValues().get(0));
    when(productService.getProductByStoreIdAndProductIdCached(ProductServiceTest.STORE_ID, product.getId()))
        .thenReturn(product);
    when(this.repository.findById(product.getId())).thenReturn(Optional.of(product));
    when(this.repository.saveAndFlush(product)).thenReturn(null);
    this.service.deleteProductItemsByProductAttributeValues(product.getStoreId(), productAttributeValues);
    verify(applicationContext).getBean(ProductService.class);
    verify(this.repository).findById(product.getId());
    verify(this.repository).saveAndFlush(product);
    verify(this.productItemService, times(13)).getSequence(ProductServiceTest.PRODUCT_CODE);
    verify(productService).getProductByStoreIdAndProductIdCached(ProductServiceTest.STORE_ID, product.getId());
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, product.getId());
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, product.getId());
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(DEFAULT_STORE_ID, product, false);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, product.getId());
    verifyEvictAllProductDetailCacheByProduct(product);
    verify(this.domainEventPublisherService).publishProduct((Product) any());
  }

  @Test
  public void testFindById() throws Exception {
    String id = GdnUUIDHelper.generateUUID();
    this.product.setId(id);
    when(this.repository.findById(id)).thenReturn(Optional.of(this.product));
    Product savedProduct = this.service.findById(id);
    Assertions.assertEquals(savedProduct, (this.product));
    verify(this.repository, times(1)).findById(id);
  }

  @Test
  public void testFindByStoreId() throws Exception {
    when(
        this.repository.findByStoreIdAndMarkForDeleteFalseOrderByUpdatedDateDesc(ProductServiceTest.STORE_ID,
            ProductServiceTest.DEFAULT_PAGEABLE)).thenReturn(this.productPage);
    assertTrue(1 == this.service.findByStoreId(ProductServiceTest.STORE_ID, ProductServiceTest.DEFAULT_PAGEABLE)
        .getTotalElements());
    verify(this.repository, times(1)).findByStoreIdAndMarkForDeleteFalseOrderByUpdatedDateDesc(
        ProductServiceTest.STORE_ID, ProductServiceTest.DEFAULT_PAGEABLE);
  }

  @Test
  public void testFindByStoreIdAndIdInitProductDetailWithoutImages() throws Exception {
    Product savedProduct =
        service.getProductDetailsWithoutImagesByProductCodeAndMarkForDeleteFalse(ProductServiceTest.STORE_ID, PRODUCT_ID);
    Assertions.assertEquals(savedProduct, (this.product));
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productItemService).getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(DEFAULT_STORE_ID, CATEGORY_ID_1);
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
  }

  @Test
  public void testFindByStoreIdAndIdInitProductDetailWithoutImagesWithEmptyId() throws Exception {
    String id = GdnUUIDHelper.generateUUID();
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.service.getProductDetailsWithoutImagesByProductCodeAndMarkForDeleteFalse(ProductServiceTest.STORE_ID, id));
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductIdCached(STORE_ID, id);
    }
  }

  @Test
  public void testFindByWeightBiggerOrEqualThan() {
    when(
        this.repository.findByStoreIdAndWeightGreaterThanEqualAndMarkForDeleteFalse(ProductServiceTest.STORE_ID,
            ProductServiceTest.WEIGHT_10)).thenReturn(this.products);
    when(
        this.repository.findByStoreIdAndWeightGreaterThanEqualAndMarkForDeleteFalse(ProductServiceTest.STORE_ID,
            ProductServiceTest.WEIGHT_10, ProductServiceTest.DEFAULT_PAGEABLE)).thenReturn(this.productPage);
    assertTrue(this.service.findByWeightBiggerOrEqualThan(ProductServiceTest.STORE_ID, ProductServiceTest.WEIGHT_10)
        .size() == this.service.findByWeightBiggerOrEqualThan(ProductServiceTest.STORE_ID,
        ProductServiceTest.WEIGHT_10, ProductServiceTest.DEFAULT_PAGEABLE).getTotalElements());
    verify(this.repository, times(1)).findByStoreIdAndWeightGreaterThanEqualAndMarkForDeleteFalse(
        ProductServiceTest.STORE_ID, ProductServiceTest.WEIGHT_10);
    verify(this.repository, times(1)).findByStoreIdAndWeightGreaterThanEqualAndMarkForDeleteFalse(
        ProductServiceTest.STORE_ID, ProductServiceTest.WEIGHT_10, ProductServiceTest.DEFAULT_PAGEABLE);
  }

  @Test
  public void testFindByWeightLesserOrEqualThan() {
    when(
        this.repository.findByStoreIdAndWeightLessThanEqualAndMarkForDeleteFalse(ProductServiceTest.STORE_ID,
            ProductServiceTest.WEIGHT_10)).thenReturn(this.products);
    when(
        this.repository.findByStoreIdAndWeightLessThanEqualAndMarkForDeleteFalse(ProductServiceTest.STORE_ID,
            ProductServiceTest.WEIGHT_10, ProductServiceTest.DEFAULT_PAGEABLE)).thenReturn(this.productPage);
    assertTrue(this.service.findByWeightLesserOrEqualThan(ProductServiceTest.STORE_ID, ProductServiceTest.WEIGHT_10)
        .size() == this.service.findByWeightLesserOrEqualThan(ProductServiceTest.STORE_ID,
        ProductServiceTest.WEIGHT_10, ProductServiceTest.DEFAULT_PAGEABLE).getTotalElements());
    verify(this.repository, times(1)).findByStoreIdAndWeightLessThanEqualAndMarkForDeleteFalse(
        ProductServiceTest.STORE_ID, ProductServiceTest.WEIGHT_10);
    verify(this.repository, times(1)).findByStoreIdAndWeightLessThanEqualAndMarkForDeleteFalse(
        ProductServiceTest.STORE_ID, ProductServiceTest.WEIGHT_10, ProductServiceTest.DEFAULT_PAGEABLE);
  }

  @Test
  public void testFindDetailByStoreIdAndProductCodes() throws Exception {
    List<String> productCodes = new ArrayList<>();
    productCodes.add(PRODUCT_CODE);
    List<Product> result =
        service.getProductWithProductCategoriesAndItemsByStoreIdAndProductCodes(ProductServiceTest.STORE_ID, productCodes);
    assertEquals(product, result.get(0));
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(DEFAULT_STORE_ID, CATEGORY_ID_1);
    verify(productItemServiceWrapper).setProductItemsCached(DEFAULT_STORE_ID, product, false);
  }

  @Test
  public void testFindDetailByStoreIdAndProductCodesWithException() throws Exception {
    List<String> productCodes = new ArrayList<>();
    productCodes.add(PRODUCT_CODE);
    when(productService.getProductByStoreIdAndProductCodeCached(ProductServiceTest.STORE_ID, PRODUCT_CODE))
        .thenThrow(new ApplicationRuntimeException());
    try {
      this.service.getProductWithProductCategoriesAndItemsByStoreIdAndProductCodes(ProductServiceTest.STORE_ID, productCodes);
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void testGenerateSpecificationDetail() throws Exception {
    Product product = this.createAdvancedProduct();
    when(this.attributeService.findById("warnaId")).thenReturn(this.warna);
    when(this.attributeService.findById("ukuranId")).thenReturn(this.ukuran);
    when(this.attributeService.findById("bahanId")).thenReturn(this.bahan);
    when(this.attributeService.findById("deskripsiId")).thenReturn(this.deskripsi);
    when(this.attributeService.findById("osId")).thenReturn(this.os);
    String specificationDetail = this.service.generateSpecificationDetail(product);
    Assertions.assertEquals(specificationDetail,
        ("<ul><li>Warna<ul><li>Biru</li><li>Merah</li><li>Ungu</li><li>Abu - "
            + "Abu</li></ul></li><li>Ukuran<ul><li>16GB</li><li>32GB</li></ul></li><li>Bahan<ul"
            + "><li>Metal</li><li>Plastik</li></ul></li><li>Deskripsi<ul><li>ini adalah "
            + "deskripsi barang</li></ul></li><li>OS<ul><li>Android</li><li>IOS</li><li>Ubuntu"
            + "</li></ul></li></ul>"));
    verify(this.attributeService, times(1)).findById("warnaId");
    verify(this.attributeService, times(1)).findById("ukuranId");
    verify(this.attributeService, times(1)).findById("bahanId");
    verify(this.attributeService, times(1)).findById("deskripsiId");
    verify(this.attributeService, times(1)).findById("osId");
  }


  @Test
  public void testGenerateSpecificationDetail_skuValueTrue() throws Exception {
    Attribute attribute = new Attribute();
    attribute.setSkuValue(true);
    Product entity = new Product();
    entity.setProductAttributes(new ArrayList<ProductAttribute>());
    entity.getProductAttributes().add(new ProductAttribute());
    entity.getProductAttributes().get(0).setAttribute(new Attribute());
    Mockito.when(this.attributeService.findById(anyString())).thenReturn(attribute);
    this.service.generateSpecificationDetail(entity);
    Mockito.verify(this.attributeService).findById(any());
  }

  @Test
  public void testGetDeletedProductItem() throws Exception {
    int testDeletedIndex = 0;
    Product product = this.createAdvancedProduct();
    this.service.sortAndGenerateProductItem(product);
    ProductAttribute productAttribute = product.getProductAttributes().get(0);
    ProductAttributeValue productAttributeValue = productAttribute.getProductAttributeValues().get(testDeletedIndex);
    ProductServiceTest.LOG.debug("Mark for delete " + productAttribute.getAttribute().getName() + " "
        + productAttributeValue.getAllowedAttributeValue().getValue());
    productAttributeValue.setMarkForDelete(true);
    List<ProductItem> productItems = this.service.getDeletedProductItem(product);
    ProductServiceTest.LOG.debug("List deleted item : ");
    for (ProductItem productItem : productItems) {
      ProductServiceTest.LOG.debug(productItem.getGeneratedItemName());
    }
    int n = 0;
    for (int i = 0; i < (this.strBahan.length); i++) {
      for (int j = 0; j < (this.strUkuran.length); j++) {
        Assertions.assertEquals(
            productItems.get(n).getGeneratedItemName(),
            (product.getName() + " " + this.strBahan[i] + " " + this.strUkuran[j] + " "
                + this.strWarna[testDeletedIndex]));
        String hash =
            product.getProductCode() + " bahanId " + this.strBahanId[i] + " ukuranId " + this.strUkuranId[j]
                + " warnaId " + this.strWarnaId[testDeletedIndex];
        n++;
      }
    }
    verify(this.productItemService, times(20)).getSequence(ProductServiceTest.PRODUCT_CODE);
  }

  @Test
  public void testMarkForDelete() throws Exception {
    Product product = generateProductRequest();
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(ProductServiceTest.STORE_ID, product.getId()))
        .thenReturn(product);
    when(this.repository.findById(product.getId())).thenReturn(Optional.of(product));
    this.service.markForDeleteProduct(ProductServiceTest.STORE_ID, product.getId());

    Assertions.assertEquals(product.isMarkForDelete(), (true));
    verify(this.repository, times(1)).findByStoreIdAndIdAndMarkForDeleteFalse(ProductServiceTest.STORE_ID,
        product.getId());
    verify(this.repository, times(1)).findById(product.getId());
    verify(this.repository, times(1)).saveAndFlush(product);
  }

  @Test
  public void testMarkForDeleteEditedProduct() throws Exception {
    Product product = generateProductRequest();
    product.setEdited(Boolean.TRUE);
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(ProductServiceTest.STORE_ID, product.getId()))
        .thenReturn(product);
    when(this.repository.findById(product.getId())).thenReturn(Optional.of(product));
    this.service.markForDeleteProduct(ProductServiceTest.STORE_ID, product.getId());

    Assertions.assertEquals(product.isMarkForDelete(), (true));
    verify(this.repository, times(1)).findByStoreIdAndIdAndMarkForDeleteFalse(ProductServiceTest.STORE_ID,
        product.getId());
    verify(this.repository, times(1)).findById(product.getId());
    verify(this.repository, times(1)).saveAndFlush(product);
  }

  @Test
  public void testMarkForDeleteWithEmptyProduct() throws Exception {
    String id = GdnUUIDHelper.generateUUID();
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(ProductServiceTest.STORE_ID, id)).thenReturn(null);
    try {
      this.service.markForDeleteProduct(ProductServiceTest.STORE_ID, id);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationException);
      assertTrue(e.getMessage().contains("Can not perform delete on un exist data : "));
      verify(this.repository, times(1)).findByStoreIdAndIdAndMarkForDeleteFalse(ProductServiceTest.STORE_ID, id);
    }
  }

  @Test
  public void testRegenerateProductItem() throws Exception {
    ReflectionTestUtils.setField(service, "ranchIntegrationEnabled", true);
    Product product = this.createAdvancedProduct();
    List<String> attributeIds = new ArrayList<>();
    Product savedProduct = getProduct(product, attributeIds);
    product.getProductItems().get(0).setOmniChannelSku(SKU_CODE);
    setProductImage(savedProduct);
    savedProduct.getProductItems().get(0).setViewable(true);
    savedProduct.getProductItems().get(0).setActivated(true);
    when(productImageCleanupRepository.findLocationPathByProductCode(product.getProductCode())).thenReturn(new ArrayList<>());
    when(productImageCleanupRepository.saveAll(anyList())).thenReturn(new ArrayList());
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    this.service.regenerateProductItem(ProductServiceTest.STORE_ID, savedProduct, product, false,
      false, true, false, false, false, false);
    verify(this.repository).saveAndFlush(savedProduct);
    verify(attributeService).getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, "warnaId");
    verify(productImageCleanupRepository).findLocationPathByProductCode(product.getProductCode());
    verify(productImageCleanupRepository).saveAll(listArgumentCaptor.capture());
    assertEquals(1, listArgumentCaptor.getValue().size());
  }

  private void setProductImage(Product product) {
    productImage.setLocationPath(LOCATION_PATH);
    productImage.setOriginalImage(Boolean.TRUE);
    product.setProductImages(Arrays.asList(productImage));
  }

  @Test
  public void testRegenerateProductItemWithSameItemImages() throws Exception {
    Product product = this.createAdvancedProduct();
    product.setProductItems(new ArrayList<>());
    ProductItem productItem = new ProductItem();
    productItem.setHash(HASH.getBytes());
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setId(ID);
    productItemImage.setLocationPath(LOCATION_PATH);
    productItemImage.setMainImages(true);
    productItemImage.setProductItem(productItem);
    product.getProductItems().add(productItem);
    product.getProductItems().get(0).setProductItemImages(new ArrayList<>());
    product.getProductItems().get(0).getProductItemImages().add(productItemImage);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setId(ID_1);
    productItemImage1.setLocationPath(LOCATION_PATH);
    productItemImage1.setMainImages(true);
    List<String> attributeIds = new ArrayList<>();
    Product savedProduct = getProduct(product, attributeIds);
    ProductItem productItem1 = new ProductItem();
    productItem1.setHash(HASH.getBytes());
    productItemImage1.setProductItem(productItem1);
    savedProduct.getProductItems().add(productItem1);
    savedProduct.getProductItems().get(0).setProductItemImages(new ArrayList<>());
    savedProduct.getProductItems().get(0).getProductItemImages().add(productItemImage1);
    savedProduct.getProductItems().get(1).setProductItemImages(new ArrayList<>());
    savedProduct.getProductItems().get(1).getProductItemImages().add(productItemImage);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage1);
    product.getProductItems().get(1).getProductItemImages().add(productItemImage1);
    savedProduct.getProductItems().get(0).setContentChanged(true);
    savedProduct.getProductItems().get(0).setActivated(true);
    savedProduct.getProductItems().get(1).setActivated(false);
    savedProduct.getProductItems().get(0).setViewable(false);
    savedProduct.getProductItems().get(1).setActivated(true);
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    when(productImageCleanupRepository.findLocationPathByProductCode(product.getProductCode())).thenReturn(new ArrayList<>());
    this.service.regenerateProductItem(ProductServiceTest.STORE_ID, savedProduct, product, true,
      false, true, false, false, false, false);
    verify(this.repository).saveAndFlush(savedProduct);
    verify(attributeService).getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, warna.getId());
    verify(productImageCleanupRepository).findLocationPathByProductCode(product.getProductCode());
  }

  @Test
  public void testRegenerateProductItemWithDifferentItemImages() throws Exception {
    Product product = this.createAdvancedProduct();
    product.setProductItems(new ArrayList<>());
    ProductItem productItem = new ProductItem();
    productItem.setHash(HASH.getBytes());
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setId(ID);
    productItemImage.setLocationPath(LOCATION_PATH);
    productItemImage.setMainImages(true);
    productItemImage.setProductItem(productItem);
    product.getProductItems().add(productItem);
    product.getProductItems().get(0).setProductItemImages(new ArrayList<>());
    product.getProductItems().get(0).getProductItemImages().add(productItemImage);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setId(ID_1);
    productItemImage1.setLocationPath(LOCATION_PATH);
    productItemImage1.setMainImages(true);
    productItemImage1.setOriginalImage(Boolean.TRUE);
    List<String> attributeIds = new ArrayList<>();
    Product savedProduct = getProduct(product, attributeIds);
    ProductItem productItem1 = new ProductItem();
    productItem1.setHash(HASH.getBytes());
    productItemImage1.setProductItem(productItem1);
    savedProduct.getProductItems().add(productItem1);
    savedProduct.getProductItems().get(0).setProductItemImages(new ArrayList<>());
    savedProduct.getProductItems().get(0).getProductItemImages().add(productItemImage1);
    savedProduct.getProductItems().get(1).setProductItemImages(new ArrayList<>());
    savedProduct.getProductItems().get(1).getProductItemImages().add(productItemImage1);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage);
    product.getProductItems().get(1).getProductItemImages().add(productItemImage);
    product.getProductItems().get(0).setContentChanged(true);
    product.getProductItems().get(0).setViewable(true);
    product.getProductItems().get(0).setActivated(true);
    product.getProductItems().get(1).setViewable(true);
    product.getProductItems().get(1).setActivated(true);
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    when(productImageCleanupRepository.findLocationPathByProductCode(product.getProductCode())).thenReturn(new ArrayList<>());
    this.service.regenerateProductItem(ProductServiceTest.STORE_ID, savedProduct, product, false,
      false, true, false, false, false, false);
    verify(this.repository).saveAndFlush(savedProduct);
    verify(attributeService).getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, warna.getId());
    verify(productImageCleanupRepository).findLocationPathByProductCode(product.getProductCode());
  }

  @Test
  public void testRegenerateProductItemWithNoCategoryChange() throws Exception {
    Product product = this.createAdvancedProduct();
    List<String> attributeIds = new ArrayList<>();
    Product savedProduct = getProductWithCategory(product, attributeIds);
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    when(productImageCleanupRepository.findLocationPathByProductCode(product.getProductCode())).thenReturn(new ArrayList<>());
    this.service.regenerateProductItem(ProductServiceTest.STORE_ID, savedProduct, product, true,
      false, true, false, false, false, false);
    verify(this.repository).saveAndFlush(savedProduct);
    verify(attributeService).getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, warna.getId());
    verify(productImageCleanupRepository).findLocationPathByProductCode(product.getProductCode());
  }

  @Test
  public void testRegenerateProductItemWithCategoryChange() throws Exception {
    ReflectionTestUtils.setField(service, "avoidBrandDelete", true);
    ReflectionTestUtils.setField(service, "brandAttributeId", "brand");
    Product product = this.createAdvancedProduct();
    List<String> attributeIds = new ArrayList<>();
    Product savedProduct = getProductWithDifferentCategory(product, attributeIds);
    savedProduct.getProductAttributes().get(0).setAttributeId("brand");
    when(this.repository.findById(savedProduct.getId())).thenReturn(Optional.of(savedProduct));
    Category category2 = createDefaultCategory();
    category2.setId(CATEGORY_ID_2);
    category2.setCategoryCode(CATEGORY_CODE_2);
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    when(categoryService.findByStoreIdAndId(STORE_ID, CATEGORY_ID_2)).thenReturn(category2);
    when(categoryReferenceService.getSalesCategoryReferenceByMasterCategory(CATEGORY_ID_1, CATEGORY_ID_2, false))
        .thenReturn(new ProductSalesCategoryMapping(Collections.singletonList(SALES_CATEGORY_1),
            Collections.singletonList(SALES_CATEGORY_2), Collections.singletonList(SALES_CATEGORY_3),
            new ArrayList<>(), new ArrayList<>(),true));
    when(productImageCleanupRepository.findLocationPathByProductCode(product.getProductCode())).thenReturn(new ArrayList<>());
    this.service.regenerateProductItem(ProductServiceTest.STORE_ID, savedProduct, product, false,
      false, true, false, false, false, false);
    verify(this.repository).saveAndFlush(savedProduct);
    verify(attributeService).getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, warna.getId());
    Mockito.verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_2);
    Mockito.verify(productImageCleanupRepository).findLocationPathByProductCode(product.getProductCode());
  }

  private Product getProductWithCategory(Product product, List<String> attributeIds) {
    Category category = createDefaultCategory();
    category.setId(CATEGORY_1_NAME);
    productCategory = new ProductCategory(product, category, STORE_ID);
    List<ProductCategory> productCategoryList = new ArrayList<ProductCategory>();
    productCategoryList.add(productCategory);
    productCategoryList.add(productCategory);
    product.setProductCategories(productCategoryList);
    Product savedProduct = getProduct(product, attributeIds);
    savedProduct.setProductCategories(Collections.singletonList(productCategory));
    return savedProduct;
  }

  private Product getProductWithDifferentCategory(Product product, List<String> attributeIds) {
    Category category1 = createDefaultCategory();
    category1.setId(CATEGORY_ID_1);
    category1.setCategoryCode(CATEGORY_CODE_1);
    Category category2 = createDefaultCategory();
    category2.setId(CATEGORY_ID_2);
    category2.setCategoryCode(CATEGORY_CODE_2);
    ProductCategory productCategory1 = new ProductCategory(product, category1, STORE_ID);
    productCategory1.setMarkForDelete(Boolean.TRUE);
    ProductCategory productCategory2 = new ProductCategory(product, category2, STORE_ID);
    productCategory2.setMarkForDelete(Boolean.FALSE);
    List<ProductCategory> productCategoryList = new ArrayList<>();
    productCategoryList.add(productCategory1);
    productCategoryList.add(productCategory2);
    product.setProductCategories(productCategoryList);
    Product savedProduct = getProduct(product, attributeIds);
    savedProduct.setProductCategories(new ArrayList<>(Collections.singletonList(productCategory1)));
    return savedProduct;
  }


  private Product getProduct(Product product, List<String> attributeIds) {
    attributeIds.add(WARNA_ID);
    product.getProductAttributes().get(0).setId(null);
    ProductItem pi =
        new ProductItem(product, UPC_CODE, SKU_CODE, GENERATED_ITEM_NAME, HASH.getBytes(), ProductServiceTest.STORE_ID);
    product.getProductItems().add(pi);
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setAttribute(attribute);
    attribute = new Attribute(COLOUR_FAMILY, AttributeType.valueOf(PREDEFINED_ATTRIBUTE), true, STORE_ID);
    attribute.setVariantCreatingUI(true);
    attribute.setScreeningMandatory(true);
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    List<ProductItemAttributeValue> productItemAttributeValues = new ArrayList<>();
    productItemAttributeValue.setAttribute(attribute);
    productItemAttributeValues.add(productItemAttributeValue);
    pi.setProductItemAttributeValues(productItemAttributeValues);
    Product savedProduct = this.createAdvancedProduct();
    ProductItem savedProductItem =
        new ProductItem(product, UPC_CODE, SKU_CODE, GENERATED_ITEM_NAME, HASH.getBytes(), ProductServiceTest.STORE_ID);
    product.getProductItems().get(0).setId(UUID_1);
    savedProductItem.setId(UUID_1);
    savedProductItem.setProductItemAttributeValues(productItemAttributeValues);
    savedProductItem.setActivated(true);
    savedProductItem.setViewable(true);
    savedProduct.getProductItems().add(savedProductItem);
    return savedProduct;
  }

  @Test
  public void testSaveProductSuccessfully() throws Exception {
    Product product = generateProduct();
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    productCategory.setCategory(category);
    product.getProductCategories().add(productCategory);
    product.getProductAttributes().remove(3);
    product.setId(null);
    Product savedProduct = new Product();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(product, savedProduct);
    savedProduct.setId(uuid);
    when(this.repository.saveAndFlush(product)).thenReturn(savedProduct);
    Assertions.assertEquals(this.service.save(product), (uuid));
    verify(this.repository, times(1)).saveAndFlush(product);
    verify(this.productItemService).getSequence(DEFAULT_PRODUCT_CODE);
    verify(this.domainEventPublisherService)
        .publishProduct(any(Product.class), eq(true));
  }

  @Test
  public void testSaveProductSuccessfullyWithRequestContainingProductItems() throws Exception {
    Product product = this.createAdvancedProduct();
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    productCategory.setCategory(category);
    product.getProductCategories().add(productCategory);
    product.getProductAttributes().remove(3);
    ProductImage productImage = new ProductImage();
    product.getProductImages().add(productImage);
    generateProductItems(product);
    Product savedProduct = new Product();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(product, savedProduct);
    savedProduct.setId(uuid);
    when(repository.findById(product.getId())).thenReturn(Optional.ofNullable(null));
    when(this.repository.saveAndFlush(product)).thenReturn(savedProduct);
    Assertions.assertEquals(this.service.save(product), (uuid));
    verify(this.repository, times(1)).findById(product.getId());
    verify(this.repository, times(1)).saveAndFlush(product);
    verify(this.productItemService, times(12)).getSequence(product.getProductCode());
    verify(this.domainEventPublisherService).publishProduct(any(Product.class), eq(true));
  }

  @Test
  public void testSaveProductSuccessfullyWithRequestNotContainingAllProductItems() throws Exception {
    Product product = this.createAdvancedProduct();
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    productCategory.setCategory(category);
    product.getProductCategories().add(productCategory);
    product.getProductAttributes().remove(3);
    ProductImage productImage = new ProductImage();
    product.getProductImages().add(productImage);
    generateProductItems(product);
    for (int i = 0; i < product.getProductItems().size();) {
      ProductItem productItem = product.getProductItems().get(i);
      if (REMOVE_PRODUCT_ITEM_1.equals(productItem.getGeneratedItemName()) || REMOVE_PRODUCT_ITEM_2
          .equals(productItem.getGeneratedItemName())) {
        product.getProductItems().remove(i);
      } else {
        i++;
      }
    }
    Product savedProduct = new Product();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(product, savedProduct);
    savedProduct.setId(uuid);
    when(repository.findById(product.getId())).thenReturn(Optional.ofNullable(null));
    when(this.repository.saveAndFlush(product)).thenReturn(savedProduct);
    Assertions.assertEquals(this.service.save(product), (uuid));
    verify(this.repository, times(1)).findById(product.getId());
    verify(this.repository, times(1)).saveAndFlush(productArgumentCaptor.capture());
    Product productWithColourFamily = productArgumentCaptor.getValue();
    List<ProductItem> productItemsNotPresentInRequest = productWithColourFamily.getProductItems().stream().filter(
        productItem -> (productItem.getGeneratedItemName().equals(REMOVE_PRODUCT_ITEM_1) || productItem
            .getGeneratedItemName().equals(REMOVE_PRODUCT_ITEM_2))).collect(toList());
    for (ProductItem productItem : productItemsNotPresentInRequest) {
      for (ProductItemAttributeValue productItemAttributeValue : productItem.getProductItemAttributeValues()) {
        if (COLOUR_FAMILY.equals(productItemAttributeValue.getAttribute().getName())) {
          Assertions.assertNotNull(productItemAttributeValue.getValue());
          break;
        }
      }
    }
    verify(this.productItemService, times(12)).getSequence(product.getProductCode());
    verify(this.domainEventPublisherService).publishProduct(any(Product.class), eq(true));
  }

  private void generateProductItems(Product product) throws Exception {
    List<ProductAttribute> productAttributes = new ArrayList<ProductAttribute>();
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      if (productAttribute.getAttribute().getAttributeType().equals(AttributeType.DEFINING_ATTRIBUTE)) {
        productAttributes.add(productAttribute);
      }
    }
    Collections.sort(productAttributes, new Comparator<ProductAttribute>() {
      @Override
      public int compare(ProductAttribute arg0, ProductAttribute arg1) {
        return arg0.getAttribute().getId().compareTo(arg1.getAttribute().getId());
      }
    });
    for (ProductAttribute productAttribute : productAttributes) {
      List<ProductAttributeValue> productAttributeValues = productAttribute.getProductAttributeValues();
      if (!CollectionUtils.isEmpty(productAttributeValues)) {
        Collections.sort(productAttributeValues, new Comparator<ProductAttributeValue>() {
          @Override
          public int compare(ProductAttributeValue productAttributeValue1,
              ProductAttributeValue productAttributeValue2) {
            return productAttributeValue1.getAllowedAttributeValue().getId()
                .compareTo(productAttributeValue2.getAllowedAttributeValue().getId());
          }
        });
      }
    }
    this.generateProductItem(product, productAttributes, 0, productAttributes.size(), product.getName(),
        product.getProductCode());
  }

  private void generateProductItem(Product product, List<ProductAttribute> productAttributes, int index, int size,
      String itemName, String result) throws Exception {
    if (index >= size) {
      ProductItem productItem =
          new ProductItem(product, null, null, itemName, GdnDigestUtil.getDigestFromString("SHA-256", "UTF-8", result),
              product.getStoreId());

      ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
      productItemAttributeValue
          .setAttribute(new Attribute(COLOUR_FAMILY, AttributeType.valueOf("PREDEFINED_ATTRIBUTE"), true, STORE_ID));
      for (String warna : strWarna) {
        if (itemName.contains(warna)) {
          productItemAttributeValue.setValue(warna);
          break;
        }
      }
      productItem.getProductItemAttributeValues().add(productItemAttributeValue);
      product.getProductItems().add(productItem);
    } else {
      List<ProductAttributeValue> attrVals = productAttributes.get(index).getProductAttributeValues();
      String attributeId = productAttributes.get(index).getAttribute().getId();
      if (!CollectionUtils.isEmpty(attrVals)) {
        for (ProductAttributeValue attrVal : attrVals) {
          if (!attrVal.isMarkForDelete()) {
            this.generateProductItem(product, productAttributes, index + 1, size,
                itemName + SPACE + attrVal.getAllowedAttributeValue().getValue(),
                result + SPACE + attributeId + SPACE + attrVal.getAllowedAttributeValue().getId());
          }
        }
      }
    }
  }


  @Test
  public void testSaveProductWithoutItemGenrationSuccessfully() throws Exception {
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(LOCATION_PATH_1);
    Product product = generateProduct();
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    productCategory.setCategory(category);
    product.getProductCategories().add(productCategory);
    product.getProductAttributes().remove(3);
    product.setId(null);
    testRegenerateProductItem();
    ProductItem pi =
        new ProductItem(product, "upcCode", "skuCode", "generatedItemName", "hash".getBytes(),
            ProductServiceTest.STORE_ID);
    pi.setProductItemImages(Arrays.asList(productItemImage1));
    product.getProductItems().add(pi);
    Product savedProduct = new Product();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(product, savedProduct);
    savedProduct.setId(uuid);
    when(this.repository.save(product)).thenReturn(savedProduct);
    Assertions.assertEquals(this.service.saveProduct(product, new ArrayList<>(), true), (savedProduct));
    verify(this.repository, times(1)).save(product);
    verify(this.domainEventPublisherService)
        .publishProduct(any(Product.class), eq(true));
    verify(productAttributeExtractionService)
        .addProductsToProductAttributeExtraction(anyString(), any(Category.class));
  }

  @Test
  public void testSaveProductWithMissingAttributesNotPresent() throws Exception {
    ReflectionTestUtils.setField(service, "setMissingAttributesInCreationEnabled", true);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(LOCATION_PATH_1);
    Product product = generateProduct();
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE_1);
    category.setId(CATEGORY_ID);
    productCategory.setCategory(category);
    product.getProductCategories().add(productCategory);
    product.getProductAttributes().remove(3);
    product.setStoreId(STORE_ID);
    product.setId(null);
    testRegenerateProductItem();
    ProductItem pi =
      new ProductItem(product, UPC_CODE, SKU_CODE, GENERATED_ITEM_NAME, HASH.getBytes(),
        ProductServiceTest.STORE_ID);
    pi.setProductItemImages(List.of(productItemImage1));
    product.getProductItems().add(pi);
    Product savedProduct = new Product();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(product, savedProduct);
    savedProduct.setId(uuid);
    List<CategoryAttribute> categoryAttributes = List.of(
      createCategoryAttribute(ATTRIBUTE_ID_1, true, false),
      createCategoryAttribute(ATTRIBUTE_ID_2, true, false)
    );
    List<String> missingAttributes =  List.of(ATTRIBUTE_ID_1);
    List<Attribute> missingAttributeList = List.of(createAttribute(ATTRIBUTE_ID_2,
      AttributeType.PREDEFINED_ATTRIBUTE));
    when(categoryService.getCategoryAttributesMarkForDeleteFalse(any(), any())).thenReturn(categoryAttributes);
    when(attributeService.findByAttributeIds(any(), eq(missingAttributes))).thenReturn(missingAttributeList);
    when(predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(any(), any(), any()))
      .thenReturn(new ArrayList<>());
    when(this.repository.save(product)).thenReturn(savedProduct);
    Assertions.assertEquals(this.service.saveProduct(product, new ArrayList<>(), true), (savedProduct));
    verify(this.repository, times(1)).save(product);
    verify(this.domainEventPublisherService)
      .publishProduct(any(Product.class), eq(true));
    verify(productAttributeExtractionService)
      .addProductsToProductAttributeExtraction(anyString(), any(Category.class));
    verify(categoryService).getCategoryAttributesMarkForDeleteFalse(STORE_ID, CATEGORY_ID);
  }

  @Test
  public void testSaveProductWithMissingAttributesException() throws Exception {
    ReflectionTestUtils.setField(service, "setMissingAttributesInCreationEnabled", true);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(LOCATION_PATH_1);
    Product product = generateProduct();
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE_1);
    category.setId(CATEGORY_ID);
    productCategory.setCategory(category);
    product.getProductCategories().add(productCategory);
    product.getProductAttributes().remove(3);
    product.setStoreId(STORE_ID);
    product.setId(null);
    testRegenerateProductItem();
    ProductItem pi =
      new ProductItem(product, UPC_CODE, SKU_CODE, GENERATED_ITEM_NAME, HASH.getBytes(),
        ProductServiceTest.STORE_ID);
    pi.setProductItemImages(List.of(productItemImage1));
    product.getProductItems().add(pi);
    Product savedProduct = new Product();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(product, savedProduct);
    savedProduct.setId(uuid);
    List<CategoryAttribute> categoryAttributes = List.of(
      createCategoryAttribute(ATTRIBUTE_ID_1, true, false),
      createCategoryAttribute(ATTRIBUTE_ID_2, true, false)
    );
    List<String> missingAttributes =  List.of(ATTRIBUTE_ID_1);
    List<Attribute> missingAttributeList = List.of(createAttribute(ATTRIBUTE_ID_2,
      AttributeType.PREDEFINED_ATTRIBUTE));
    Mockito.doThrow(new RuntimeException()).when(categoryService).getCategoryAttributesMarkForDeleteFalse(anyString(), anyString());
    when(attributeService.findByAttributeIds(any(), eq(missingAttributes))).thenReturn(missingAttributeList);
    when(predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(any(), any(), any()))
      .thenReturn(new ArrayList<>());
    when(this.repository.save(product)).thenReturn(savedProduct);
    Assertions.assertEquals(this.service.saveProduct(product, new ArrayList<>(), true), (savedProduct));
    verify(this.repository, times(1)).save(product);
    verify(this.domainEventPublisherService)
      .publishProduct(any(Product.class), eq(true));
    verify(productAttributeExtractionService)
      .addProductsToProductAttributeExtraction(anyString(), any(Category.class));
    verify(categoryService).getCategoryAttributesMarkForDeleteFalse(STORE_ID, CATEGORY_ID);
  }

  @Test
  public void testSaveProductWithMissingAttributesPresent() throws Exception {
    ReflectionTestUtils.setField(service, "setMissingAttributesInCreationEnabled", true);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(LOCATION_PATH_1);
    Product product = generateProduct();
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE_1);
    category.setId(CATEGORY_ID);
    productCategory.setCategory(category);
    product.getProductCategories().add(productCategory);
    product.getProductAttributes().remove(3);
    product.setStoreId(STORE_ID);
    product.setId(null);
    testRegenerateProductItem();
    ProductItem pi =
      new ProductItem(product, UPC_CODE, SKU_CODE, GENERATED_ITEM_NAME, HASH.getBytes(),
        ProductServiceTest.STORE_ID);
    pi.setProductItemImages(List.of(productItemImage1));
    product.getProductItems().add(pi);
    Product savedProduct = new Product();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(product, savedProduct);
    savedProduct.setId(uuid);
    List<CategoryAttribute> categoryAttributes = List.of(
      createCategoryAttribute(ATTRIBUTE_ID_4, true, false),
      createCategoryAttribute(ATTRIBUTE_ID_5, true, false)
    );
    List<String> missingAttributes =  List.of(ATTRIBUTE_ID_4);
    Attribute attribute5 = createAttribute(ATTRIBUTE_ID_5, AttributeType.PREDEFINED_ATTRIBUTE);
    Attribute attribute4 = createAttribute(ATTRIBUTE_ID_4, AttributeType.DESCRIPTIVE_ATTRIBUTE);
    List<Attribute> missingAttributeList = List.of(attribute5, attribute4);
    when(categoryService.getCategoryAttributesMarkForDeleteFalse(any(), any())).thenReturn(categoryAttributes);
    when(attributeService.findByAttributeIds(STORE_ID, List.of(ATTRIBUTE_ID_4, ATTRIBUTE_ID_5))).thenReturn(missingAttributeList);
    when(predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(any(), any(), any()))
      .thenReturn(new ArrayList<>());
    when(this.repository.save(product)).thenReturn(savedProduct);
    Assertions.assertEquals(this.service.saveProduct(product, new ArrayList<>(), true), (savedProduct));
    verify(this.repository, times(1)).save(product);
    verify(this.domainEventPublisherService)
      .publishProduct(any(Product.class), eq(true));
    verify(productAttributeExtractionService)
      .addProductsToProductAttributeExtraction(anyString(), any(Category.class));
    verify(categoryService).getCategoryAttributesMarkForDeleteFalse(STORE_ID, CATEGORY_ID);
    verify(attributeService).findByAttributeIds(STORE_ID, List.of(ATTRIBUTE_ID_4, ATTRIBUTE_ID_5));
    verify(predefinedAllowedAttributeValueService).findByStoreIdAndAttributeAndValueOrderByMarkForDelete(STORE_ID, attribute5, Constants.HYPHEN);
  }

  @Test
  public void testSaveProductWithMissingAttributesPresentAndNoAttributeResponse() throws Exception {
    ReflectionTestUtils.setField(service, "setMissingAttributesInCreationEnabled", true);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(LOCATION_PATH_1);
    Product product = generateProduct();
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE_1);
    category.setId(CATEGORY_ID);
    productCategory.setCategory(category);
    product.getProductCategories().add(productCategory);
    product.getProductAttributes().remove(3);
    product.setStoreId(STORE_ID);
    product.setId(null);
    testRegenerateProductItem();
    ProductItem pi =
      new ProductItem(product, UPC_CODE, SKU_CODE, GENERATED_ITEM_NAME, HASH.getBytes(),
        ProductServiceTest.STORE_ID);
    pi.setProductItemImages(List.of(productItemImage1));
    product.getProductItems().add(pi);
    Product savedProduct = new Product();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(product, savedProduct);
    savedProduct.setId(uuid);
    List<CategoryAttribute> categoryAttributes = List.of(
      createCategoryAttribute(ATTRIBUTE_ID_4, true, false),
      createCategoryAttribute(ATTRIBUTE_ID_5, true, false)
    );
    List<String> missingAttributes =  List.of(ATTRIBUTE_ID_4);
    Attribute attribute5 = createAttribute(ATTRIBUTE_ID_5, AttributeType.PREDEFINED_ATTRIBUTE);
    Attribute attribute4 = createAttribute(ATTRIBUTE_ID_4, AttributeType.DESCRIPTIVE_ATTRIBUTE);
    List<Attribute> missingAttributeList = List.of(attribute5, attribute4);
    when(categoryService.getCategoryAttributesMarkForDeleteFalse(any(), any())).thenReturn(categoryAttributes);
    when(attributeService.findByAttributeIds(STORE_ID, List.of(ATTRIBUTE_ID_4, ATTRIBUTE_ID_5))).thenReturn(new ArrayList<>());
    when(predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(any(), any(), any()))
      .thenReturn(new ArrayList<>());
    when(this.repository.save(product)).thenReturn(savedProduct);
    Assertions.assertEquals(this.service.saveProduct(product, new ArrayList<>(), true), (savedProduct));
    verify(this.repository, times(1)).save(product);
    verify(this.domainEventPublisherService)
      .publishProduct(any(Product.class), eq(true));
    verify(productAttributeExtractionService)
      .addProductsToProductAttributeExtraction(anyString(), any(Category.class));
    verify(categoryService).getCategoryAttributesMarkForDeleteFalse(STORE_ID, CATEGORY_ID);
    verify(attributeService).findByAttributeIds(STORE_ID, List.of(ATTRIBUTE_ID_4, ATTRIBUTE_ID_5));
  }

  @Test
  public void testSaveProductWithMissingAttributesPresentAndNoCategoryResponse() throws Exception {
    ReflectionTestUtils.setField(service, "setMissingAttributesInCreationEnabled", true);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(LOCATION_PATH_1);
    Product product = generateProduct();
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE_1);
    category.setId(CATEGORY_ID);
    productCategory.setCategory(category);
    product.getProductAttributes().remove(3);
    product.setStoreId(STORE_ID);
    product.setId(null);
    testRegenerateProductItem();
    ProductItem pi =
      new ProductItem(product, UPC_CODE, SKU_CODE, GENERATED_ITEM_NAME, HASH.getBytes(),
        ProductServiceTest.STORE_ID);
    pi.setProductItemImages(List.of(productItemImage1));
    product.getProductItems().add(pi);
    Product savedProduct = new Product();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(product, savedProduct);
    savedProduct.setId(uuid);
    List<CategoryAttribute> categoryAttributes = List.of(
      createCategoryAttribute(ATTRIBUTE_ID_4, true, false),
      createCategoryAttribute(ATTRIBUTE_ID_5, true, false)
    );
    List<String> missingAttributes =  List.of(ATTRIBUTE_ID_4);
    Attribute attribute5 = createAttribute(ATTRIBUTE_ID_5, AttributeType.PREDEFINED_ATTRIBUTE);
    Attribute attribute4 = createAttribute(ATTRIBUTE_ID_4, AttributeType.DESCRIPTIVE_ATTRIBUTE);
    List<Attribute> missingAttributeList = List.of(attribute5, attribute4);
    when(categoryService.getCategoryAttributesMarkForDeleteFalse(any(), any())).thenReturn(categoryAttributes);
    when(attributeService.findByAttributeIds(STORE_ID, List.of(ATTRIBUTE_ID_4, ATTRIBUTE_ID_5))).thenReturn(new ArrayList<>());
    when(predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(any(), any(), any()))
      .thenReturn(new ArrayList<>());
    when(this.repository.save(product)).thenReturn(savedProduct);
    Assertions.assertEquals(this.service.saveProduct(product, new ArrayList<>(), true), (savedProduct));
    verify(this.repository, times(1)).save(product);
    verify(this.domainEventPublisherService)
      .publishProduct(any(Product.class), eq(true));
    verify(productAttributeExtractionService)
      .addProductsToProductAttributeExtraction(anyString(), any(Category.class));
  }


  private CategoryAttribute createCategoryAttribute(String id, boolean mustShow, boolean variantCreation) {
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    Attribute attribute = new Attribute();
    attribute.setId(id);
    attribute.setMustShowOnCustomerSide(mustShow);
    attribute.setVariantCreation(variantCreation);
    categoryAttribute.setAttribute(attribute);
    return categoryAttribute;
  }

  private Attribute createAttribute(String id, AttributeType type) {
    Attribute attribute = new Attribute();
    attribute.setId(id);
    attribute.setAttributeType(type);
    return attribute;
  }



  @Test
  public void testSaveProductCommonImageTest() throws Exception {
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(LOCATION_PATH_1);
    Product product = generateProduct();
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    productCategory.setCategory(category);
    product.getProductCategories().add(productCategory);
    product.getProductAttributes().remove(3);
    product.setId(null);
    testRegenerateProductItem();
    ProductItem pi =
        new ProductItem(product, "upcCode", "skuCode", "generatedItemName", "hash".getBytes(),
            ProductServiceTest.STORE_ID);
    pi.setProductItemImages(Arrays.asList(productItemImage1));
    product.getProductItems().add(pi);
    Product savedProduct = new Product();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(product, savedProduct);
    savedProduct.setId(uuid);
    when(this.repository.save(product)).thenReturn(savedProduct);
    Image image = new Image();
    image.setMainImages(false);
    image.setLocationPath(PRODUCT_1_NAME);

    Assertions.assertEquals(this.service.saveProduct(product, Collections.singletonList(image), true), (savedProduct));
    verify(this.repository, times(1)).save(product);
    verify(this.domainEventPublisherService)
        .publishProduct(any(Product.class), eq(true));
    verify(productAttributeExtractionService)
        .addProductsToProductAttributeExtraction(anyString(), any(Category.class));
  }

  @Test
  public void testSaveProductCommonImage1Test() throws Exception {
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(LOCATION_PATH_1);
    Product product = generateProduct();
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    productCategory.setCategory(category);
    product.getProductCategories().add(productCategory);
    product.getProductAttributes().remove(3);
    product.setId(null);
    testRegenerateProductItem();
    ProductItem pi =
        new ProductItem(product, "upcCode", "skuCode", "generatedItemName", "hash".getBytes(),
            ProductServiceTest.STORE_ID);
    pi.setProductItemImages(Arrays.asList(productItemImage1));
    product.getProductItems().add(pi);
    Product savedProduct = new Product();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(product, savedProduct);
    savedProduct.setId(uuid);
    when(this.repository.save(product)).thenReturn(savedProduct);
    Image image = new Image();
    image.setMainImages(true);
    image.setLocationPath(LOCATION_PATH);
    Assertions.assertEquals(this.service.saveProduct(product, Collections.singletonList(image), false), (savedProduct));
    verify(this.repository, times(1)).save(product);
    verify(this.domainEventPublisherService)
        .publishProduct(any(Product.class), eq(true));
    verify(productAttributeExtractionService)
        .addProductsToProductAttributeExtraction(anyString(), any(Category.class));
  }

  @Test
  public void testSaveProductCommonImage2Test() throws Exception {
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(LOCATION_PATH_1);
    Product product = generateProduct();
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    productCategory.setCategory(category);
    product.getProductCategories().add(productCategory);
    product.getProductAttributes().remove(3);
    product.setId(null);
    testRegenerateProductItem();
    ProductItem pi =
        new ProductItem(product, "upcCode", "skuCode", "generatedItemName", "hash".getBytes(),
            ProductServiceTest.STORE_ID);
    pi.setProductItemImages(Arrays.asList(productItemImage1));
    product.getProductItems().add(pi);
    Product savedProduct = new Product();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(product, savedProduct);
    savedProduct.setId(uuid);
    when(this.repository.save(product)).thenReturn(savedProduct);
    Image image = new Image();
    image.setMainImages(true);
    image.setLocationPath(LOCATION_PATH);
    Assertions.assertEquals(this.service.saveProduct(product, Collections.singletonList(image), false), (savedProduct));
    verify(this.repository, times(1)).save(product);
    verify(this.domainEventPublisherService)
        .publishProduct(any(Product.class), eq(true));
    verify(productAttributeExtractionService)
        .addProductsToProductAttributeExtraction(anyString(), any(Category.class));
  }

  @Test
  public void testSaveProductSuccessfully_emptyProductCode() throws Exception {
    Product product = generateProduct();
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    productCategory.setCategory(category);
    product.getProductCategories().add(productCategory);
    product.getProductAttributes().remove(3);
    product.setId(null);
    product.setProductCode(new String());
    Product savedProduct = new Product();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(product, savedProduct);
    savedProduct.setId(uuid);
    when(this.categoryService.getSequence("UK")).thenReturn("UK");
    when(this.repository.saveAndFlush(product)).thenReturn(savedProduct);
    Assertions.assertEquals(this.service.save(product), (uuid));
    verify(this.categoryService).getSequence("UK");
    verify(this.repository, times(1)).saveAndFlush(product);
    verify(this.productItemService).getSequence(anyString());
    verify(this.domainEventPublisherService)
        .publishProduct(any(Product.class), eq(true));
  }

  @Test
  public void testSaveProductWithEmptyProductCode() throws Exception {
    Product product = generateProduct();
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    productCategory.setCategory(category);
    product.getProductCategories().add(productCategory);
    product.getProductAttributes().remove(3);
    product.setId(null);
    Product savedProduct = new Product();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(product, savedProduct);
    savedProduct.setId(uuid);
    when(this.categoryService.getSequence("UK")).thenReturn("000001");
    when(this.productItemService.getSequence("UK-0000001")).thenReturn("00001");
    when(this.repository.saveAndFlush(product)).thenReturn(savedProduct);
    Assertions.assertEquals(this.service.save(product), (uuid));
    verify(this.repository, times(1)).saveAndFlush(product);
    verify(this.productItemService).getSequence(DEFAULT_PRODUCT_CODE);
    verify(this.domainEventPublisherService)
        .publishProduct(any(Product.class), eq(true));
  }

  @Test
  public void testSaveProductWithFilledId() {

    Product product = this.createDefaultProduct();
    Product savedProduct = new Product();
    String uuid = GdnUUIDHelper.generateUUID();
    product.setId(uuid);
    BeanUtils.copyProperties(product, savedProduct);
    when(this.repository.findById(product.getId())).thenReturn(Optional.of(savedProduct));
    try {
      this.service.save(product);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(ProductServiceTest.ERROR_MESSAGE_FOR_SAVE)) {
        assertTrue(true);
      } else {
        assertTrue(false);
      }
      verify(this.repository, times(1)).findById(product.getId());
      verify(this.productItemService).getSequence(ProductServiceTest.PRODUCT_CODE);
    }

  }

  @Test
  public void testSaveProductWithGenerateSpecificationDetailBySystem() throws Exception {
    Product product = this.createDefaultProduct();
    Product savedProduct = new Product();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(product, savedProduct);
    savedProduct.setId(uuid);
    when(this.repository.saveAndFlush(product)).thenReturn(savedProduct);
    savedProduct.setSpecificationDetail(this.service.generateSpecificationDetail(savedProduct));
    Assertions.assertEquals(this.service.saveProductWithSpecificationDetailGenaratedBySystem(product), (uuid));
    verify(this.repository, times(1)).saveAndFlush(product);
    verify(this.productItemService).getSequence(ProductServiceTest.PRODUCT_CODE);
    verify(domainEventPublisherService).publishProduct(product, true);
  }

  @Test
  public void generateSpecificationDetailWithNullProductAttribute() throws Exception {
    Product product = this.createDefaultProduct();
    Product savedProduct = new Product();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(product, savedProduct);
    savedProduct.setId(uuid);
    when(this.repository.saveAndFlush(product)).thenReturn(savedProduct);

    Attribute attr = new Attribute();
    attr.setId(uuid);
    attr.setSkuValue(false);
    when(this.attributeService.findById(anyString())).thenReturn(attr);

    ProductAttribute prdAttr = new ProductAttribute();
    prdAttr.setAttribute(attr);
    List<ProductAttribute> prdAttrList = new ArrayList<>();
    prdAttrList.add(prdAttr);
    savedProduct.setProductAttributes(prdAttrList);
    savedProduct.setSpecificationDetail(this.service.generateSpecificationDetail(savedProduct));

    Assertions.assertEquals(this.service.saveProductWithSpecificationDetailGenaratedBySystem(product), (uuid));

    verify(this.attributeService).findById(anyString());
    verify(this.repository, times(1)).saveAndFlush(product);
    verify(this.productItemService).getSequence(ProductServiceTest.PRODUCT_CODE);
    verify(domainEventPublisherService).publishProduct(product, true);
  }

  @Test
  public void generateSpecificationDetailWithNullPrdAttrValue() throws Exception {
    Product product = this.createDefaultProduct();
    String uuid = GdnUUIDHelper.generateUUID();
    Attribute attr = new Attribute();
    attr.setId(uuid);
    attr.setSkuValue(false);
    attr.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);

    ProductAttribute prdAttr = new ProductAttribute();
    prdAttr.setAttribute(attr);
    product.setProductAttributes(Arrays.asList(prdAttr));

    Product savedProduct = new Product();
    BeanUtils.copyProperties(product, savedProduct);
    savedProduct.setId(uuid);
    when(this.repository.saveAndFlush(product)).thenReturn(savedProduct);
    when(this.attributeService.findById(anyString())).thenReturn(attr);

    savedProduct.setSpecificationDetail(this.service.generateSpecificationDetail(savedProduct));

    this.service.sortAndGenerateProductItem(product);

    verify(this.attributeService).findById(anyString());
    verify(this.productItemService).getSequence(ProductServiceTest.PRODUCT_CODE);
  }

  @Test
  public void testSortAndGenerateProductItem() throws Exception {
    Product product = this.createAdvancedProduct();
    Product productResponse = this.service.sortAndGenerateProductItem(product);

    List<ProductItem> productItems = productResponse.getProductItems();

    int n = 0;
    for (int i = 0; i < (this.strBahan.length); i++) {
      for (int j = 0; j < (this.strUkuran.length); j++) {
        for (int k = 0; k < (this.strWarna.length); k++) {
          Assertions.assertEquals(
              productItems.get(n).getGeneratedItemName(),
              (product.getName() + " " + this.strBahan[i] + " " + this.strUkuran[j] + " "
                  + this.strWarna[k]));
          String hash =
              product.getProductCode() + " bahanId " + this.strBahanId[i] + " ukuranId " + this.strUkuranId[j]
                  + " warnaId " + this.strWarnaId[k];

          List<ProductItemAttributeValue> productItemAttributeValues =
              productItems.get(n).getProductItemAttributeValues();
          String atribut[] =
              {this.strBahan[i], this.strUkuran[j], this.strWarna[k], this.strOS[0], this.strOS[1], this.strOS[2]};
          int l = 0;
          for (ProductItemAttributeValue productItemAttributeValue : productItemAttributeValues) {
            l++;
          }

          n++;
        }
      }
    }
    verify(this.productItemService, times(12)).getSequence(ProductServiceTest.PRODUCT_CODE);
  }

  @Test
  public void sortAndGenerateProductItemAttrValueIsNull() throws Exception {
    Product product = this.createAdvancedProduct();
    product.getProductAttributes().get(0).setProductAttributeValues(null);;
    this.service.sortAndGenerateProductItem(product);

    Mockito.verify(this.productItemService, never()).getSequence(anyString());
  }

  @Test
  public void testSortAndGenerateProductItem_DefiningProductAttribute_WithNullAllowedAttributeValue()
      throws Exception {
    Product product = this.createAdvancedProduct();
    Attribute emptyAttribute =
        new Attribute("Test", AttributeType.DEFINING_ATTRIBUTE, false, DEFAULT_STORE_ID);
    emptyAttribute.setId(UUID.randomUUID().toString());

    ProductAttribute emptyProductAttribute =
        new ProductAttribute(emptyAttribute, product, "Test", false, 0, DEFAULT_STORE_ID);

    List<ProductAttributeValue> emptyProductAttributeValues = new ArrayList<>();
    ProductAttributeValue emptyProductAttributeValue =
        new ProductAttributeValue(emptyProductAttribute, null, "1234",
            DescriptiveAttributeValueType.SINGLE);
    emptyProductAttributeValues.add(emptyProductAttributeValue);

    emptyProductAttribute.setProductAttributeValues(emptyProductAttributeValues);
    product.getProductAttributes().add(emptyProductAttribute);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.service.sortAndGenerateProductItem(product));
  }

  @Test
  public void testUpdateProductNonExistenceEntity() {
    Product product = this.createDefaultProduct();
    Product savedProduct = new Product();
    String uuid = GdnUUIDHelper.generateUUID();
    product.setId(uuid);
    BeanUtils.copyProperties(product, savedProduct);
    when(this.repository.findById(uuid)).thenReturn(Optional.ofNullable(null));
    try {
      this.service.update(product);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(ProductServiceTest.ERROR_MESSAGE_FOR_UPDATE)) {
        assertTrue(true);
      } else {
        assertTrue(false);
      }
      verify(this.repository, times(1)).findById(product.getId());
    }
  }

  @Test
  public void testUpdateProductSuccessfully() throws Exception {
    when(this.repository.findById(product.getId())).thenReturn(Optional.of(product));
    this.service.update(product);
    verify(this.repository, times(1)).findById(PRODUCT_ID);
    verify(this.repository, times(1)).saveAndFlush(product);
  }

  @Test
  public void testUpdateProductWithEmptyId() {
    Product product = this.createDefaultProduct();
    Product savedProduct = new Product();
    BeanUtils.copyProperties(product, savedProduct);
    try {
      this.service.update(product);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(ProductServiceTest.ERROR_MESSAGE_FOR_UPDATE)) {
        assertTrue(true);
      } else {
        assertTrue(false);
      }
    }
  }

  @Test
  public void testUpdateCategoryChangeProductWithEmptyId() {
    Product product = this.createDefaultProduct();
    Product savedProduct = new Product();
    BeanUtils.copyProperties(product, savedProduct);
    try {
      this.service.update(product);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      assertTrue(applicationRuntimeException.getErrorMessage().contains(ProductServiceTest.ERROR_MESSAGE_FOR_UPDATE));
    }
  }

  @Test
  public void testUpdateProductWithGenerateSpecificationDetailBySystem() throws Exception {
    Product product = this.createDefaultProduct();
    Product savedProduct = new Product();
    String uuid = GdnUUIDHelper.generateUUID();
    product.setId(uuid);
    BeanUtils.copyProperties(product, savedProduct);
    when(productImageCleanupRepository.findLocationPathByProductCode(product.getProductCode())).thenReturn(new ArrayList<>());
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    this.service.updateProductWithSpecificationDetailGeneratedBySystem(ProductServiceTest.STORE_ID, savedProduct,
        product);
    assertTrue(true);
    verify(this.repository, times(1)).saveAndFlush(product);
    verify(productImageCleanupRepository).findLocationPathByProductCode(product.getProductCode());
  }

  @Test
  public void testUpdateProduct_nullProductItem() throws Exception {
    Product entity = this.createDefaultProduct();
    entity.setProductItems(new ArrayList<ProductItem>());
    entity.getProductItems().add(null);
    entity.setId("1");
    Mockito.when(this.repository.findById(anyString())).thenReturn(Optional.of(new Product()));
    this.service.update(entity);
    Mockito.verify(this.repository).findById(anyString());
    Mockito.verify(this.repository).saveAndFlush((Product) any());
  }

  @Test
  public void testChangeProductCategoryToExistingProductCategory() throws Exception {
    ReflectionTestUtils.setField(service, "avoidBrandDelete", true);
    ReflectionTestUtils.setField(service, "brandAttributeId", "brand");
    Product product = this.createAdvancedProduct();
    for (int i = 0; i < 1; i++) {
      product.getProductAttributes().get(i).setId(null);
    }
    ProductItem pi =
        new ProductItem(product, "upcCode", "skuCode", "generatedItemName", "hash".getBytes(),
            ProductServiceTest.STORE_ID);
    product.getProductItems().add(pi);
    Product savedProduct = this.createAdvancedProduct();
    ProductItem savedProductItem =
        new ProductItem(product, "upcCode", "skuCode", "generatedItemName", "hash".getBytes(),
            ProductServiceTest.STORE_ID);
    product.getProductItems().get(0).setId("uuid");
    savedProductItem.setId("uuid");
    savedProduct.getProductItems().add(savedProductItem);

    // saved product has CAT-01 and CAT-02, currently selected category is CAT-02
    // new product changed its selected category from CAT-02 back to CAT-01
    List<ProductCategory> existingProductCategories = new ArrayList<ProductCategory>();
    List<ProductCategory> changedProductCategories = new ArrayList<ProductCategory>();
    List<String> attributeIds=new ArrayList<>();
    attributeIds.add("warnaId");
    for (int i = 1; i <= 2; i++) {
      Category existingCategory = this.createDefaultCategory();
      existingCategory.setId("CAT-0" + i);
      ProductCategory existingProductCategory =
          new ProductCategory(savedProduct, existingCategory, ProductServiceTest.STORE_ID);
      ProductCategory changedProductCategory =
          new ProductCategory(product, existingCategory, ProductServiceTest.STORE_ID);
      if (i == 2) {
        existingProductCategory.setMarkForDelete(false); // CAT-02 (selected)
      } else {
        existingProductCategory.setMarkForDelete(true); // CAT-01 MFD
        changedProductCategory.setMarkForDelete(false); // CAT-01 (selected)
        changedProductCategories.add(changedProductCategory);
      }
      existingProductCategories.add(existingProductCategory);
    }
    savedProduct.setProductCategories(existingProductCategories);
    product.setProductCategories(changedProductCategories);
    when(productImageCleanupRepository.findLocationPathByProductCode(product.getProductCode())).thenReturn(new ArrayList<>());
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    savedProduct.getProductAttributes().get(0).setAttributeId("non-brand");
    this.service.regenerateProductItem(ProductServiceTest.STORE_ID, savedProduct, product, true,
      false, true, false, false, false, false);
    Assertions.assertFalse(existingProductCategories.get(0).isMarkForDelete());
    assertTrue(existingProductCategories.get(1).isMarkForDelete());
    verify(this.repository).saveAndFlush(savedProduct);
    verify(attributeService).getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, warna.getId());
    verify(productImageCleanupRepository).findLocationPathByProductCode(product.getProductCode());
  }

  @Test
  public void regenerateProductItemTest_WhenUpcCodeUpdated() throws Exception {
    Product product = this.createAdvancedProduct();
    List<String> attributeIds = new ArrayList<>();
    attributeIds.add(WARNA_ID);
    product.getProductAttributes().get(0).setMarkForDelete(true);
    ProductItem updatedItem = new ProductItem(product, UPDATED_UPC, DEFAULT_SKU_CODE,
        DEFAULT_GENERATED_ITEM_NAME, HASH.getBytes(), STORE_ID);
    product.getProductItems().add(updatedItem);
    Product savedProduct = this.createAdvancedProduct();
    ProductItem savedProductItem = new ProductItem(product, UPC_CODE, DEFAULT_SKU_CODE, DEFAULT_GENERATED_ITEM_NAME,
        HASH.getBytes(), STORE_ID);
    product.getProductItems().get(0).setId(DEFAULT_PRODUCT_ID);
    savedProductItem.setId(DEFAULT_PRODUCT_ID);
    savedProduct.getProductItems().add(savedProductItem);

    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    when(productImageCleanupRepository.findLocationPathByProductCode(product.getProductCode())).thenReturn(new ArrayList<>());
    this.service.regenerateProductItem(STORE_ID, savedProduct, product, false, false, true, false
      , false, false, false);
    verify(this.repository).saveAndFlush(savedProduct);
    verify(productImageCleanupRepository).findLocationPathByProductCode(any());
  }

  @Test
  public void updateProductViewableTest_Active_NotViewable_Activated() throws Exception {
    product.setViewable(false);
    product.setActivated(true);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    when(this.repository.findById(anyString())).thenReturn(Optional.of(product));
    this.service.updateProductViewable(ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE, true);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(
        ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE);
    verify(this.repository).findById(anyString());
    verify(this.repository).saveAndFlush(product);
    verify(productItemServiceWrapper).setProductItemsCached(STORE_ID, product, false);
  }

  @Test
  public void updateProductViewableTest_Active_NotViewable_NotActivated() throws Exception {
    product.setViewable(false);
    product.setActivated(true);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    when(this.repository.findById(anyString())).thenReturn(Optional.of(product));
    this.service.updateProductViewable(ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE, false);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(
        ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE);
    verify(this.repository).findById(anyString());
    verify(this.repository).saveAndFlush(product);
    verify(productItemServiceWrapper).setProductItemsCached(STORE_ID, product, false);
  }

  @Test
  public void updateProductViewable_NotActive_NotViewable_NotActivated() throws Exception {
    product.setActivated(false);
    product.setViewable(false);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    when(this.repository.findById(anyString())).thenReturn(Optional.of(product));
    this.service.updateProductViewable(ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE, true);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(
        ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE);
    verify(this.repository).findById(anyString());
    verify(this.repository).saveAndFlush(product);
    verify(productItemServiceWrapper).setProductItemsCached(DEFAULT_STORE_ID, product, false);
  }

  @Test
  public void updateProductViewable_NotActive_Viewable_NotActivated() throws Exception {
    product.setActivated(false);
    product.setViewable(true);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    when(this.repository.findById(anyString())).thenReturn(Optional.of(product));
    this.service.updateProductViewable(ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE, true);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(
        ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE);
    verify(this.repository).findById(anyString());
    verify(this.repository).saveAndFlush(product);
    verify(productItemServiceWrapper).setProductItemsCached(DEFAULT_STORE_ID, product, false);
  }

  @Test
  public void updateProductReviewPendingTest() throws Exception {
    service.updateProductReviewPending(ProductServiceTest.STORE_ID, PRODUCT_CODE, false);
    verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(ProductServiceTest.STORE_ID,
        ProductServiceTest.PRODUCT_CODE);
    verify(this.repository).saveAndFlush(product);
  }

  @Test
  public void updateFlagsOnNeedCorrectionTest() throws Exception {
    service.updateFlagsOnNeedCorrection(ProductServiceTest.STORE_ID, PRODUCT_CODE,
        new NeedRevisionConfigRequest(true, true, true));
    verify(repository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE);
    verify(this.repository).saveAndFlush(product);
  }

  @Test
  public void updateFlagsOnNeedCorrectionProductCodeNullTest() throws Exception {
    doThrow(new ApplicationRuntimeException()).when(
      repository).saveAndFlush(any(Product.class));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> service.updateFlagsOnNeedCorrection(ProductServiceTest.STORE_ID, PRODUCT_CODE,
        new NeedRevisionConfigRequest(true, true, true)));
    } finally {
      verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE);
      verify(repository).saveAndFlush(product);
    }
  }


  @Test
  public void updateProductActivatedTest() throws Exception {
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    when(this.repository.findById(anyString())).thenReturn(Optional.of(product));
    this.service.updateProductActivated(ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE, true);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(
        ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE);
    verify(productItemServiceWrapper).setProductItemsCached(STORE_ID, product, false);
    verify(this.repository).findById(anyString());
    verify(this.repository).saveAndFlush(product);
    assertTrue(product.isActivated());
    product.getProductItems().forEach(productItem -> assertTrue(productItem.isActivated()));
  }

  @Test
  public void updateProductActivatedWithActivatedIsFalseTest() throws Exception {
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    when(this.repository.findById(anyString())).thenReturn(Optional.of(product));
    this.service.updateProductActivated(ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE, false);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(
        ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE);
    verify(productItemServiceWrapper).setProductItemsCached(STORE_ID, product, false);
    verify(this.repository).findById(anyString());
    verify(this.repository).saveAndFlush(product);
    Assertions.assertFalse(product.isActivated());
    product.getProductItems().forEach(productItem -> Assertions.assertFalse(productItem.isActivated()));
  }

  @Test
  public void updateProductContentTest_WithNullPredefinedAllowedAttributeValue() throws Exception {
    Product request = generateProductRequest();
    request.setVersion(1L);
    product.setVersion(1L);
    product.setProductItems(productItems);
    for (ProductAttribute productAttribute : request.getProductAttributes()) {
      Attribute attribute = productAttribute.getAttribute();
      for (ProductAttributeValue productAttributeValue : productAttribute
          .getProductAttributeValues()) {
        if (AttributeType.PREDEFINED_ATTRIBUTE.equals(attribute.getAttributeType())) {
          productAttributeValue.setPredefinedAllowedAttributeValue(null);
        }
      }
    }
    this.service.updateProductContent(request, true);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE);
    verify(this.repository).saveAndFlush(product);
    verifyProductDetailInteractionsForMarkForDeleteTrueCached();
    verifyEvictAllProductDetailCache();
    verify(categoryReferenceService).getSalesCategoryReferenceByMasterCategory(CATEGORY_ID_1, CATEGORY_ID_2, false);
    verify(productAttributeExtractionService)
        .addProductsToProductAttributeExtraction(anyString(), any(Category.class));
    verify(this.domainEventPublisherService).publishProductChangeCategory(product, null, false, false, false, false, new HashSet<>());
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      Attribute attribute = productAttribute.getAttribute();
      for (ProductAttributeValue productAttributeValue : productAttribute
          .getProductAttributeValues()) {
        if (AttributeType.PREDEFINED_ATTRIBUTE.equals(attribute.getAttributeType())) {
          Assertions.assertNull(productAttributeValue.getPredefinedAllowedAttributeValue());
        }
      }
    }
  }

  @Test
  public void saveAndFlushTest() {
    service.saveAndFlush(product);
    verify(repository).saveAndFlush(product);
  }

  @Test
  public void updateProductContentTest() throws Exception {
    Product request = generateProductRequest();
    request.setVersion(1L);
    product.setVersion(1L);
    product.setProductItems(productItems);
    productAttributes.add(definingProductAttribute);
    productAttributes.add(definingProductAttribute1);
    when(categoryReferenceService.getSalesCategoryReferenceByMasterCategory(CATEGORY_ID_1, CATEGORY_ID_2, false))
        .thenReturn(new ProductSalesCategoryMapping(Collections.singletonList(SALES_CATEGORY_1),
            Collections.singletonList(SALES_CATEGORY_2), Collections.singletonList(SALES_CATEGORY_3),
            new ArrayList<>(), new ArrayList<>(),true));
    this.service.updateProductContent(request, true);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE);
    verify(this.repository).saveAndFlush(product);
    verifyProductDetailInteractionsForMarkForDeleteTrueCached();
    verify(categoryReferenceService).getSalesCategoryReferenceByMasterCategory(CATEGORY_ID_1, CATEGORY_ID_2, false);
    verifyEvictAllProductDetailCache();
    verify(this.domainEventPublisherService)
        .publishProductChangeCategory(eq(product), productSalesCategoryMappingArgumentCaptor.capture(), eq(false),
            eq(false), eq(Boolean.FALSE), eq(false), eq(new HashSet<>()));
    verify(productAttributeExtractionService)
        .addProductsToProductAttributeExtraction(anyString(), any(Category.class));
    ProductCategory productCategory = product.getProductCategories().stream()
        .filter(productCategory1 -> !productCategory1.isMarkForDelete())
        .findFirst().orElse(null);
    assertEquals(CATEGORY_ID_2, productCategory.getCategory().getId());
    List<ProductItemAttributeValue> productItemAttributeValues =
        product.getProductItems().get(0).getProductItemAttributeValues().stream()
        .filter(productItemAttributeValue -> !productItemAttributeValue.isMarkForDelete()).collect(toList());
    assertEquals(4, productItemAttributeValues.size());
    assertEquals(PRODUCT_ITEM_ATTRIBUTE_VALUE_5, productItemAttributeValues.get(1).getValue());
    assertEquals(PRODUCT_1_NAME + SPACE + PRODUCT_ITEM_ATTRIBUTE_VALUE_5,
        product.getProductItems().get(0).getGeneratedItemName());
    assertEquals(7, product.getProductAttributes().size());
    assertEquals(PRODUCT_ATTRIBUTE_VALUE_6,
        product.getProductAttributes().get(0).getProductAttributeValues().get(0).getDescriptiveAttributeValue());
    assertTrue(product.getProductAttributes().stream().filter(not(ProductAttribute::isMarkForDelete))
        .map(ProductAttribute::getProductAttributeName).collect(toList()).contains(DEFINING_ATTRIBUTE_NAME));
    Assertions.assertFalse(product.getProductAttributes().stream().filter(not(ProductAttribute::isMarkForDelete))
        .map(ProductAttribute::getProductAttributeName).collect(toList()).contains(DEFINING_ATTRIBUTE_NAME_TRUE));
    assertEquals(Collections.singletonList(SALES_CATEGORY_1),
        productSalesCategoryMappingArgumentCaptor.getValue().getOldSalesCategoryCodes());
    assertEquals(Collections.singletonList(SALES_CATEGORY_2),
        productSalesCategoryMappingArgumentCaptor.getValue().getNewSalesCategoryCodes());
    assertEquals(Collections.singletonList(SALES_CATEGORY_3),
        productSalesCategoryMappingArgumentCaptor.getValue().getNewUmkmSalesCategoryCodes());
    assertTrue(product.getProductItems().get(0).isActivated());
    assertTrue(product.getProductItems().get(0).isViewable());
  }

  @Test
  public void updateProductContentRemoveDuplicateTest() throws Exception {
    ReflectionTestUtils.setField(service, "removeDuplicateAttributes", true);
    Product request = generateProductRequest();
    request.setVersion(1L);
    product.setVersion(1L);
    product.setProductItems(productItems);
    productAttributes.add(definingProductAttribute);
    productAttributes.add(definingProductAttribute);
    productAttributes.add(definingProductAttribute1);
    when(categoryReferenceService.getSalesCategoryReferenceByMasterCategory(CATEGORY_ID_1, CATEGORY_ID_2, false))
        .thenReturn(new ProductSalesCategoryMapping(Collections.singletonList(SALES_CATEGORY_1),
            Collections.singletonList(SALES_CATEGORY_2), Collections.singletonList(SALES_CATEGORY_3),
            new ArrayList<>(), new ArrayList<>(),true));
    this.service.updateProductContent(request, true);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE);
    verify(this.repository).saveAndFlush(product);
    verifyProductDetailInteractionsForMarkForDeleteTrueCached();
    verify(categoryReferenceService).getSalesCategoryReferenceByMasterCategory(CATEGORY_ID_1, CATEGORY_ID_2, false);
    verifyEvictAllProductDetailCache();
    verify(this.domainEventPublisherService)
        .publishProductChangeCategory(eq(product), productSalesCategoryMappingArgumentCaptor.capture(), eq(false),
            eq(false), eq(Boolean.FALSE), eq(false), eq(new HashSet<>()));
    verify(productAttributeExtractionService)
        .addProductsToProductAttributeExtraction(anyString(), any(Category.class));
    ProductCategory productCategory = product.getProductCategories().stream()
        .filter(productCategory1 -> !productCategory1.isMarkForDelete())
        .findFirst().orElse(null);
    assertEquals(CATEGORY_ID_2, productCategory.getCategory().getId());
    List<ProductItemAttributeValue> productItemAttributeValues =
        product.getProductItems().get(0).getProductItemAttributeValues().stream()
            .filter(productItemAttributeValue -> !productItemAttributeValue.isMarkForDelete()).collect(toList());
    assertEquals(4, productItemAttributeValues.size());
    assertEquals(PRODUCT_ITEM_ATTRIBUTE_VALUE_5, productItemAttributeValues.get(1).getValue());
    assertEquals(PRODUCT_1_NAME + SPACE + PRODUCT_ITEM_ATTRIBUTE_VALUE_5,
        product.getProductItems().get(0).getGeneratedItemName());
    assertEquals(8, product.getProductAttributes().size());
    assertEquals(PRODUCT_ATTRIBUTE_VALUE_6,
        product.getProductAttributes().get(0).getProductAttributeValues().get(0).getDescriptiveAttributeValue());
    Assertions.assertFalse(product.getProductAttributes().stream().filter(not(ProductAttribute::isMarkForDelete))
        .map(ProductAttribute::getProductAttributeName).collect(toList()).contains(DEFINING_ATTRIBUTE_NAME_TRUE));
    assertEquals(Collections.singletonList(SALES_CATEGORY_1),
        productSalesCategoryMappingArgumentCaptor.getValue().getOldSalesCategoryCodes());
    assertEquals(Collections.singletonList(SALES_CATEGORY_2),
        productSalesCategoryMappingArgumentCaptor.getValue().getNewSalesCategoryCodes());
    assertEquals(Collections.singletonList(SALES_CATEGORY_3),
        productSalesCategoryMappingArgumentCaptor.getValue().getNewUmkmSalesCategoryCodes());
    assertTrue(product.getProductItems().get(0).isActivated());
    assertTrue(product.getProductItems().get(0).isViewable());
  }

  @Test
  public void updateProductContentNoChangeTest() throws Exception {
    Product request = generateProductRequest();
    request.setVersion(1L);
    request.getProductCategories().get(0).getCategory().setCategoryCode(CATEGORY_CODE_1);
    request.setName(product.getName());
    request.setBrand(product.getBrand());
    request.setUniqueSellingPoint(product.getUniqueSellingPoint());
    request.setDescription(DESKRIPSI_BARANG.getBytes());
    product.setDescription(DESKRIPSI_BARANG.getBytes());
    product.setVersion(1L);
    product.setProductItems(productItems);
    productAttributes.get(3).getAttribute().setVariantCreation(true);
    when(categoryReferenceService.getSalesCategoryReferenceByMasterCategory(CATEGORY_ID_1, CATEGORY_ID_2, false))
        .thenReturn(new ProductSalesCategoryMapping(Collections.singletonList(SALES_CATEGORY_1),
            Collections.singletonList(SALES_CATEGORY_2), Collections.singletonList(SALES_CATEGORY_3),
            new ArrayList<>(), new ArrayList<>(),true));
    this.service.updateProductContent(request, true);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE);
    verify(this.repository).saveAndFlush(product);
    verifyProductDetailInteractionsForMarkForDeleteTrueCached();
    verifyEvictAllProductDetailCache();
    verify(this.domainEventPublisherService)
        .publishProductChangeCategory(eq(product), productSalesCategoryMappingArgumentCaptor.capture(), eq(false),
            eq(false), eq(Boolean.FALSE), eq(false), eq(new HashSet<>()));
  }


  @Test
  public void updateProductContentWithProductNotFoundTest() throws Exception {
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(null);
    Product request = generateProductRequest();
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.service.updateProductContent(request, true));
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void updateProductContentWithRegenerateDefiningAttributesTest() throws Exception {
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttribute(attribute);
    productAttribute.setId(PRODUCT_ATTRIBUTE_ID_5);
    ProductAttributeValue productAttributeValue2 = new ProductAttributeValue();
    productAttributeValue2.setAllowedAttributeValue(new AllowedAttributeValue());
    productAttributeValue2.getAllowedAttributeValue().setValue(PRODUCT_ATTRIBUTE_VALUE_5);
    productAttributeValue2.setId(PRODUCT_ATTRIBUTE_VALUE_ID_5);
    productAttribute.setProductAttributeValues(Collections.singletonList(productAttributeValue2));
    Product request = generateProductRequest();
    request.setProductAttributes(new ArrayList<>(request.getProductAttributes()));
    request.getProductAttributes().add(productAttribute);
    request.setVersion(1L);
    product.setVersion(1L);
    product.setProductItems(productItems);
    this.service.updateProductContent(request, true);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(
        ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE);
    verify(this.repository).saveAndFlush(product);
    verifyProductDetailInteractionsForMarkForDeleteTrueCached();
    verify(categoryReferenceService).getSalesCategoryReferenceByMasterCategory(CATEGORY_ID_1, CATEGORY_ID_2, false);
    verifyEvictAllProductDetailCache();
    verify(this.domainEventPublisherService).publishProductChangeCategory(product, null , false, false, false, false, new HashSet<>());
    verify(productAttributeExtractionService)
        .addProductsToProductAttributeExtraction(anyString(), any(Category.class));
    ProductCategory productCategory = product.getProductCategories().stream()
        .filter(productCategory1 -> !productCategory1.isMarkForDelete())
        .findFirst().orElse(null);
    assertEquals(CATEGORY_ID_2, productCategory.getCategory().getId());
    List<ProductItemAttributeValue> productItemAttributeValues =
        product.getProductItems().get(0).getProductItemAttributeValues().stream()
            .filter(productItemAttributeValue -> !productItemAttributeValue.isMarkForDelete()).collect(toList());
    assertEquals(4, productItemAttributeValues.size());
    assertEquals(PRODUCT_ITEM_ATTRIBUTE_VALUE_5, productItemAttributeValues.get(1).getValue());
    assertEquals(PRODUCT_1_NAME + SPACE + PRODUCT_ITEM_ATTRIBUTE_VALUE_5,
        product.getProductItems().get(0).getGeneratedItemName());
    assertEquals(6, product.getProductAttributes().size());
  }

  @Test
  public void updateProductContentDuplicateAttributesTest() throws Exception {
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttribute(attribute);
    productAttribute.setId(PRODUCT_ATTRIBUTE_ID_5);
    ProductAttributeValue productAttributeValue2 = new ProductAttributeValue();
    productAttributeValue2.setAllowedAttributeValue(new AllowedAttributeValue());
    productAttributeValue2.getAllowedAttributeValue().setValue(PRODUCT_ATTRIBUTE_VALUE_5);
    productAttributeValue2.setId(PRODUCT_ATTRIBUTE_VALUE_ID_5);
    productAttribute.setProductAttributeValues(Collections.singletonList(productAttributeValue2));
    Product request = generateProductRequest();
    request.setProductAttributes(new ArrayList<>(request.getProductAttributes()));
    request.getProductAttributes().add(productAttribute);
    request.setVersion(1L);
    product.setVersion(1L);
    product.setProductItems(productItems);
    Attribute attribute5 = new Attribute();
    attribute5.setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    product.getProductItems().get(0).getProductItemAttributeValues().get(1).setAttribute(attribute5);
    product.getProductItems().get(0).getProductItemAttributeValues().get(1).setMarkForDelete(false);
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(
        STORE_ID, DEFAULT_PRODUCT_CODE)).thenReturn(product);
    this.service.updateProductContent(request, true);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(
        ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE);
    verify(this.repository).saveAndFlush(product);
    verifyProductDetailInteractionsForMarkForDeleteTrueCached();
    verify(categoryReferenceService).getSalesCategoryReferenceByMasterCategory(CATEGORY_ID_1, CATEGORY_ID_2, false);
    verifyEvictAllProductDetailCache();
    verify(this.domainEventPublisherService).publishProductChangeCategory(product, null , false, false, false, false, new HashSet<>());
    verify(productAttributeExtractionService)
        .addProductsToProductAttributeExtraction(anyString(), any(Category.class));
    ProductCategory productCategory = product.getProductCategories().stream()
        .filter(productCategory1 -> !productCategory1.isMarkForDelete())
        .findFirst().orElse(null);
    assertEquals(CATEGORY_ID_2, productCategory.getCategory().getId());
    List<ProductItemAttributeValue> productItemAttributeValues =
        product.getProductItems().get(0).getProductItemAttributeValues().stream()
            .filter(productItemAttributeValue -> !productItemAttributeValue.isMarkForDelete()).collect(toList());
    assertEquals(4, productItemAttributeValues.size());
    assertEquals(PRODUCT_ITEM_ATTRIBUTE_VALUE_5, productItemAttributeValues.get(1).getValue());
    assertEquals(PRODUCT_1_NAME + SPACE + PRODUCT_ITEM_ATTRIBUTE_VALUE_5,
        product.getProductItems().get(0).getGeneratedItemName());
    assertEquals(6, product.getProductAttributes().size());
    assertTrue(product.getProductItems().get(0).getProductItemAttributeValues().get(1).isMarkForDelete());
  }

  @Test
  public void updateProductContentWithInvalidProductVersionTest() throws Exception {
    Product request = generateProductRequest();
    product.setVersion(1L);
    request.setVersion(-1L);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> this.service.updateProductContent(request, true));
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        assertEquals(ErrorCategory.INVALID_STATE, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void updateProductImageTest() throws Exception {
    product.setProductItems(productItems);
    Product request = generateProductRequest();
    request.setVersion(1L);
    product.setVersion(1L);
    ProductImage productImage = new ProductImage();
    ProductItemImage productItemImage = new ProductItemImage();
    request.getProductImages().remove(0);
    request.getProductImages().add(productImage);
    request.getProductItems().get(0).getProductItemImages().remove(0);
    request.getProductItems().get(0).getProductItemImages().add(productItemImage);
    this.service.updateProductImage(request);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(
        ProductServiceTest.DEFAULT_STORE_ID, ProductServiceTest.PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(DEFAULT_STORE_ID, product, true);
    verify(this.repository).saveAndFlush(product);
    assertEquals(DEFAULT_MERCHANT_CODE, request.getCreatedMerchant());
  }

  @Test
  public void updateProductImagePostLiveTest() throws Exception {
    product.setProductItems(productItems);
    product.setReviewPending(true);
    Product request = generateProductRequest();
    request.setReviewPending(true);
    request.setVersion(1L);
    product.setVersion(1L);
    request.setDistributionInfo(PRODUCT_CODE);
    this.service.updateProductImage(request);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(
        ProductServiceTest.DEFAULT_STORE_ID, ProductServiceTest.PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(DEFAULT_STORE_ID, product, true);
    verify(this.repository).saveAndFlush(product);
    assertEquals(4, product.getProductImages().size());
    assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
    assertEquals(1,
        product.getProductImages().stream().filter(ProductImage::isMarkForDelete).count());
    assertEquals(1, product.getProductItems().get(0).getProductItemImages().stream()
        .filter(ProductItemImage::isMarkForDelete).count());
  }

  @Test
  public void updateProductImagePreLiveTest() throws Exception {
    Product request = generateProductRequest();
    request.setReviewPending(false);
    product.setProductItems(productItems);
    request.setVersion(1L);
    product.setVersion(1L);
    ProductImage productImage = new ProductImage();
    ProductItemImage productItemImage = new ProductItemImage();
    request.getProductImages().remove(0);
    request.getProductImages().add(productImage);
    request.getProductItems().get(0).getProductItemImages().remove(0);
    request.getProductItems().get(0).getProductItemImages().add(productItemImage);
    this.service.updateProductImage(request);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(
        ProductServiceTest.DEFAULT_STORE_ID, ProductServiceTest.PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(DEFAULT_STORE_ID, product, true);
    verify(this.repository).saveAndFlush(product);
    assertEquals(5, product.getProductImages().size());
    assertEquals(3, product.getProductItems().get(0).getProductItemImages().size());
    assertEquals(2,
        product.getProductImages().stream().filter(ProductImage::isMarkForDelete).count());
    assertEquals(2, product.getProductItems().get(0).getProductItemImages().stream()
        .filter(ProductItemImage::isMarkForDelete).count());
  }

  @Test
  public void updateProductImageWithProductNotFoundTest() throws Exception {
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(null);
    Product request = generateProductRequest();
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.service.updateProductImage(request));
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void updateProductImageWithInvalidProductVersionTest() throws Exception {
    Product request = generateProductRequest();
    request.setVersion(-1L);
    product.setVersion(1L);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> this.service.updateProductImage(request));
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        assertEquals(ErrorCategory.INVALID_STATE, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void checkBrandChangesTest() throws Exception {
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setValue("Value");
    productItemAttributeValue.setAttribute(new Attribute());
    productItemAttributeValue.getAttribute().setName("Brand");
    List<ProductItemAttributeValue> productItemAttributeValueList =
        new ArrayList<ProductItemAttributeValue>();
    productItemAttributeValueList.add(productItemAttributeValue);
    ProductItem productItem = new ProductItem();
    productItem.setProductItemAttributeValues(productItemAttributeValueList);
    List<ProductItem> productItemList = new ArrayList<ProductItem>();
    productItemList.add(productItem);
    Product savedProduct = new Product();
    savedProduct.setBrand("Brand");
    savedProduct.setProductItems(productItemList);
    this.service.checkBrandChanges(savedProduct);
  }

  @Test
  public void checkBrandChangesTest_attributeNameNotEqualsBrand() throws Exception {
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setValue("Value");
    productItemAttributeValue.setAttribute(new Attribute());
    productItemAttributeValue.getAttribute().setName("Value");
    List<ProductItemAttributeValue> productItemAttributeValueList =
        new ArrayList<ProductItemAttributeValue>();
    productItemAttributeValueList.add(productItemAttributeValue);
    ProductItem productItem = new ProductItem();
    productItem.setProductItemAttributeValues(productItemAttributeValueList);
    List<ProductItem> productItemList = new ArrayList<ProductItem>();
    productItemList.add(productItem);
    Product savedProduct = new Product();
    savedProduct.setBrand("Other");
    savedProduct.setProductItems(productItemList);
    this.service.checkBrandChanges(savedProduct);
  }

  @Test
  public void checkBrandChangesTest_attributeNameNotEqualsBrandNull() throws Exception {
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setValue("Value");
    productItemAttributeValue.setAttribute(new Attribute());
    productItemAttributeValue.getAttribute().setName(null);
    List<ProductItemAttributeValue> productItemAttributeValueList =
        new ArrayList<ProductItemAttributeValue>();
    productItemAttributeValueList.add(productItemAttributeValue);
    ProductItem productItem = new ProductItem();
    productItem.setProductItemAttributeValues(productItemAttributeValueList);
    List<ProductItem> productItemList = new ArrayList<ProductItem>();
    productItemList.add(productItem);
    Product savedProduct = new Product();
    savedProduct.setBrand("Other");
    savedProduct.setProductItems(productItemList);
    this.service.checkBrandChanges(savedProduct);
  }

  @Test
  public void checkBrandChangesTest_sameBrand() throws Exception {
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setValue("Brand");
    productItemAttributeValue.setAttribute(new Attribute());
    productItemAttributeValue.getAttribute().setName("Brand");
    List<ProductItemAttributeValue> productItemAttributeValueList =
        new ArrayList<ProductItemAttributeValue>();
    productItemAttributeValueList.add(productItemAttributeValue);
    ProductItem productItem = new ProductItem();
    productItem.setProductItemAttributeValues(productItemAttributeValueList);
    List<ProductItem> productItemList = new ArrayList<ProductItem>();
    productItemList.add(productItem);
    Product savedProduct = new Product();
    savedProduct.setBrand("Brand");
    savedProduct.setProductItems(productItemList);
    this.service.checkBrandChanges(savedProduct);
  }

  @Test
  public void findByStoreIdAndCategoriesCodeTest() {
    this.service.findByStoreIdAndCategoriesCode(STORE_ID, this.Ids, DEFAULT_PAGEABLE);
    Mockito.verify(productRepositoryCustom).findByStoreIdAndCategoriesCode(STORE_ID, this.Ids,
        DEFAULT_PAGEABLE);
  }

  @Test
  public void adjustProductItemTest_differentGeneratedItemName() throws Exception {
    Product oldProduct = generateProduct();
    oldProduct.getProductItems().get(0).setHash(new byte[1024]);
    oldProduct.getProductItems().remove(1);
    oldProduct.getProductItems().get(0).setGeneratedItemName(DEFAULT_GENERATED_ITEM_NAME);
    oldProduct.getProductItems().get(0).getProductItemAttributeValues().remove(1);
    Product newProduct = generateProduct();
    newProduct.getProductItems().get(0).setHash(new byte[1024]);
    newProduct.getProductItems().get(0).setGeneratedItemName("DIFFERENT_GENERATED_ITEM_NAME");
    newProduct.getProductItems().remove(1);
    newProduct.getProductAttributes().remove(3);
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    when(productImageCleanupRepository.findLocationPathByProductCode(oldProduct.getProductCode())).thenReturn(new ArrayList<>());
    Method method =
      ProductServiceBean.class.getDeclaredMethod("adjustProductItem", String.class,
        Product.class, Product.class, boolean.class, boolean.class, boolean.class);
    method.setAccessible(true);
    method.invoke(this.service, STORE_ID, oldProduct, newProduct, false, false, false);
    Mockito.verify(this.repository).saveAndFlush((Product) any());
    verify(productImageCleanupRepository).findLocationPathByProductCode(oldProduct.getProductCode());
  }

  @Test
  public void adjustProductItemTestDifferentGeneratedItemNameAddDeleteSwitchOn() throws Exception {
    ReflectionTestUtils.setField(service, "addDeleteVariantsSwitch", true);
    Product oldProduct = generateProduct();
    oldProduct.getProductItems().get(0).setHash(new byte[1024]);
    oldProduct.getProductItems().remove(1);
    oldProduct.getProductItems().get(0).setGeneratedItemName(DEFAULT_GENERATED_ITEM_NAME);
    oldProduct.getProductItems().get(0).getProductItemAttributeValues().remove(1);
    Product newProduct = generateProduct();
    newProduct.getProductItems().get(0).setHash(new byte[1024]);
    newProduct.getProductItems().get(0).setGeneratedItemName("DIFFERENT_GENERATED_ITEM_NAME");
    newProduct.getProductItems().remove(1);
    newProduct.getProductAttributes().remove(3);
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    when(productImageCleanupRepository.findLocationPathByProductCode(oldProduct.getProductCode())).thenReturn(new ArrayList<>());
    Method method =
      ProductServiceBean.class.getDeclaredMethod("adjustProductItem", String.class,
        Product.class, Product.class, boolean.class, boolean.class, boolean.class);
    method.setAccessible(true);
    method.invoke(this.service, STORE_ID, oldProduct, newProduct, false, false, false);
    Mockito.verify(this.repository).saveAndFlush((Product) any());
    verify(productImageCleanupRepository).findLocationPathByProductCode(oldProduct.getProductCode());
  }

  @Test
  public void adjustProductItemTestDifferentGeneratedItemNameAddDeleteWithCombinedUpdate() throws Exception {
    ReflectionTestUtils.setField(service, "addDeleteVariantsSwitch", true);
    Product oldProduct = generateProduct();
    oldProduct.getProductItems().get(0).setHash(new byte[1024]);
    oldProduct.getProductItems().remove(1);
    oldProduct.getProductItems().get(0).setGeneratedItemName(DEFAULT_GENERATED_ITEM_NAME);
    oldProduct.getProductItems().get(0).getProductItemAttributeValues().remove(1);
    Product newProduct = generateProduct();
    newProduct.getProductItems().get(0).setHash(new byte[1024]);
    newProduct.getProductItems().get(0).setGeneratedItemName("DIFFERENT_GENERATED_ITEM_NAME");
    newProduct.getProductItems().remove(1);
    newProduct.getProductAttributes().remove(3);
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    when(productImageCleanupRepository.findLocationPathByProductCode(oldProduct.getProductCode())).thenReturn(new ArrayList<>());
    Method method =
      ProductServiceBean.class.getDeclaredMethod("adjustProductItem", String.class,
        Product.class, Product.class, boolean.class, boolean.class, boolean.class);
    method.setAccessible(true);
    method.invoke(this.service, STORE_ID, oldProduct, newProduct, false, false, true);
  }

  @Test
  public void adjustProductItemItemDeletedAddDeleteSwitchOn() throws Exception {
    ReflectionTestUtils.setField(service, "addDeleteVariantsSwitch", true);
    ReflectionTestUtils.setField(service, "ranchIntegrationEnabled", true);
    Product oldProduct = generateProduct();
    oldProduct.getProductItems().get(0).setHash(new byte[1024]);
    oldProduct.getProductItems().get(0).setGeneratedItemName(DEFAULT_GENERATED_ITEM_NAME);
    oldProduct.getProductItems().get(1).setProductItemUomInfo(new ProductItemUomInfo());
    oldProduct.getProductItems().get(0).getProductItemAttributeValues().remove(1);
    Product newProduct = generateProduct();
    newProduct.getProductItems().get(0).setHash(new byte[1024]);
    newProduct.getProductItems().get(0).setGeneratedItemName("DIFFERENT_GENERATED_ITEM_NAME");
    newProduct.getProductItems().get(0).setMarkForDelete(true);
    newProduct.getProductItems().get(0).setProductItemUomInfo(new ProductItemUomInfo());
    newProduct.getProductItems().remove(1);
    newProduct.getProductAttributes().remove(3);
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    when(productImageCleanupRepository.findLocationPathByProductCode(oldProduct.getProductCode())).thenReturn(new ArrayList<>());
    Pair<Map<String, ProductDTO>, Map<ProductItem, String>> response =
        service.adjustProductItem(STORE_ID, oldProduct, newProduct, false, false, false);
    ProductDTO productDTO = response.getKey().get(Constants.OLD_PRODUCT);
    assertFalse(CollectionUtils.isEmpty(productDTO.getDeletedItems()));
    Mockito.verify(this.repository).saveAndFlush((Product) any());
    verify(productImageCleanupRepository).findLocationPathByProductCode(oldProduct.getProductCode());
  }

  @Test
  public void adjustProductItemTest_differentHash() throws Exception {
    List<ProductCategory> productCategoryList = new ArrayList<ProductCategory>();
    List<Attribute> attributeList = new ArrayList<>();
    List<String> attributIds = new ArrayList<>();
    attributIds.add("warnaId");
    productCategoryList.add(new ProductCategory());
    Product oldProduct = generateProduct();
    oldProduct.getProductItems().get(0).setHash(new byte[1024]);
    oldProduct.getProductItems().remove(1);
    oldProduct.getProductItems().get(0).setGeneratedItemName(DEFAULT_GENERATED_ITEM_NAME);
//    oldProduct.getProductItems().get(0).setMarkForDelete(true);
    Product newProduct = generateProduct();
    newProduct.setBrand("Value");
    newProduct.getProductItems().get(0).setHash(new byte[1025]);
    newProduct.getProductItems().get(0).setGeneratedItemName("DIFFERENT_GENERATED_ITEM_NAME");
    newProduct.getProductItems().remove(1);
    newProduct.getProductAttributes().remove(3);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(3);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(2);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(1);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).getAttribute()
        .setName("Brand");
    newProduct.getProductItems().get(0).setNewlyAddedItem(false);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).setValue("Brand");
    newProduct.setProductCategories(productCategoryList);
    newProduct.getProductCategories().get(0).setCategory(new Category());
    newProduct.getProductCategories().get(0).getCategory().setId("1");
    newProduct.getProductImages().get(0).setId("1");
    Mockito.when(this.attributeService.findByAttributeIds(STORE_ID, attributIds))
        .thenReturn(attributeList);
    Mockito.when(this.categoryService.findByStoreIdAndId(anyString(), anyString()))
        .thenReturn(new Category());
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    when(productImageCleanupRepository.findLocationPathByProductCode(oldProduct.getProductCode())).thenReturn(new ArrayList<>());
    Method method =
      ProductServiceBean.class.getDeclaredMethod("adjustProductItem", String.class,
        Product.class, Product.class, boolean.class, boolean.class, boolean.class);
    method.setAccessible(true);
    method.invoke(this.service, STORE_ID, oldProduct, newProduct, false, false, false);
    Mockito.verify(this.repository).saveAndFlush((Product) any());
    Mockito.verify(this.categoryService).getCategoryByStoreIdAndIdCached(
        anyString(), anyString());
    Mockito.verify(attributeService).getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID_2);
    verify(productImageCleanupRepository).findLocationPathByProductCode(oldProduct.getProductCode());
  }

  @Test
  public void adjustProductItemTest_WIthAllMfdTrue() throws Exception {
    ReflectionTestUtils.setField(service, "validateAtleastOneItemForAProduct", true);
    List<ProductCategory> productCategoryList = new ArrayList<ProductCategory>();
    List<Attribute> attributeList = new ArrayList<>();
    List<String> attributIds = new ArrayList<>();
    attributIds.add("warnaId");
    productCategoryList.add(new ProductCategory());
    Product oldProduct = generateProduct();
    oldProduct.getProductItems().get(0).setHash(new byte[1024]);
    oldProduct.getProductItems().remove(1);
    oldProduct.getProductItems().get(0).setGeneratedItemName(DEFAULT_GENERATED_ITEM_NAME);
    //    oldProduct.getProductItems().get(0).setMarkForDelete(true);
    Product newProduct = generateProduct();
    newProduct.setBrand("Value");
    newProduct.getProductItems().get(0).setHash(new byte[1025]);
    newProduct.getProductItems().get(0).setGeneratedItemName("DIFFERENT_GENERATED_ITEM_NAME");
    newProduct.getProductItems().remove(1);
    newProduct.getProductAttributes().remove(3);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(3);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(2);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(1);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).getAttribute()
        .setName("Brand");
    newProduct.getProductItems().get(0).setNewlyAddedItem(false);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).setValue("Brand");
    newProduct.setProductCategories(productCategoryList);
    newProduct.getProductCategories().get(0).setCategory(new Category());
    newProduct.getProductCategories().get(0).getCategory().setId("1");
    newProduct.getProductImages().get(0).setId("1");
    Mockito.when(this.attributeService.findByAttributeIds(STORE_ID, attributIds))
        .thenReturn(attributeList);
    Mockito.when(this.categoryService.findByStoreIdAndId(anyString(), anyString()))
        .thenReturn(new Category());
    oldProduct.getProductItems().stream()
        .forEach(productItem1 -> productItem1.setMarkForDelete(true));
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    when(productImageCleanupRepository.findLocationPathByProductCode(
        oldProduct.getProductCode())).thenReturn(new ArrayList<>());
    Method method =
        ProductServiceBean.class.getDeclaredMethod("adjustProductItem", String.class, Product.class,
            Product.class, boolean.class, boolean.class, boolean.class);
    method.setAccessible(true);
    newProduct.getProductItems().stream()
        .forEach(productItem1 -> productItem1.setMarkForDelete(true));
    try {
      Assertions.assertThrows(Exception.class,
          () -> method.invoke(this.service, STORE_ID, oldProduct, newProduct, false, false, false));
    } catch (ValidationException exception) {
      Assertions.assertEquals(exception.getErrorMessage(),
          ErrorMessage.PRODUCT_SHOULD_HAVE_AT_LEAST_ONE_VARIANT.getMessage());
    } finally {
      Mockito.verify(this.categoryService)
          .getCategoryByStoreIdAndIdCached(anyString(), anyString());
      Mockito.verify(attributeService)
          .getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID_2);
    }
  }

  @Test
  public void adjustProductItemTest_withNullProductImageId() throws Exception {
    List<ProductCategory> productCategoryList = new ArrayList<ProductCategory>();
    productCategoryList.add(new ProductCategory());
    Product oldProduct = generateProduct();
    oldProduct.getProductItems().get(0).setHash(new byte[1024]);
    oldProduct.getProductItems().remove(1);
    oldProduct.getProductItems().get(0).setGeneratedItemName(DEFAULT_GENERATED_ITEM_NAME);
    oldProduct.getProductItems().get(0).setMarkForDelete(true);
    oldProduct.getProductItems().get(0).getProductItemAttributeValues().remove(1);
    Product newProduct = generateProduct();
    newProduct.setBrand("Value");
    newProduct.getProductItems().get(0).setHash(new byte[1024]);
    newProduct.getProductItems().get(0).setGeneratedItemName("DIFFERENT_GENERATED_ITEM_NAME");
    newProduct.getProductItems().remove(1);
    newProduct.getProductAttributes().remove(3);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(3);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(2);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(1);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).getAttribute()
      .setName("Brand");
    newProduct.getProductImages().get(0).setId(null);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).setValue("Brand");
    newProduct.setProductCategories(productCategoryList);
    newProduct.getProductCategories().get(0).setCategory(new Category());
    newProduct.getProductCategories().get(0).getCategory().setId("1");
    newProduct.getProductItems().forEach(productItem1 -> productItem1.setNewlyAddedItem(true));
    Mockito.when(this.categoryService.findByStoreIdAndId(anyString(), anyString()))
      .thenReturn(new Category());
    when(productImageCleanupRepository.findLocationPathByProductCode(oldProduct.getProductCode())).thenReturn(new ArrayList<>());
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    Method method =
      ProductServiceBean.class.getDeclaredMethod("adjustProductItem", String.class,
        Product.class, Product.class, boolean.class, boolean.class, boolean.class);
    method.setAccessible(true);
    method.invoke(this.service, STORE_ID, oldProduct, newProduct, true, true, false);
    Mockito.verify(this.repository).saveAndFlush((Product) any());
    Mockito.verify(this.categoryService).getCategoryByStoreIdAndIdCached(
      anyString(), anyString());
    verify(productImageCleanupRepository).findLocationPathByProductCode(oldProduct.getProductCode());
    Mockito.verify(attributeService).getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID_2);
  }

  @Test
  public void adjustProductItemTest_deletedOldProductItem() throws Exception {
    List<ProductCategory> productCategoryList = new ArrayList<ProductCategory>();
    productCategoryList.add(new ProductCategory());
    Product oldProduct = generateProduct();
    oldProduct.getProductItems().get(0).setHash(new byte[1024]);
    oldProduct.getProductItems().remove(1);
    oldProduct.getProductItems().get(0).setGeneratedItemName(DEFAULT_GENERATED_ITEM_NAME);
    oldProduct.getProductItems().get(0).setMarkForDelete(true);
    oldProduct.getProductItems().get(0).getProductItemAttributeValues().remove(1);
    Product newProduct = generateProduct();
    newProduct.setBrand("Value");
    newProduct.getProductItems().get(0).setHash(new byte[1024]);
    newProduct.getProductItems().get(0).setGeneratedItemName("DIFFERENT_GENERATED_ITEM_NAME");
    newProduct.getProductItems().remove(1);
    newProduct.getProductAttributes().remove(3);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(3);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(2);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(1);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).getAttribute()
        .setName("Brand");
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).setValue("Brand");
    newProduct.setProductCategories(productCategoryList);
    newProduct.getProductCategories().get(0).setCategory(new Category());
    newProduct.getProductCategories().get(0).getCategory().setId("1");
    Mockito.when(this.categoryService.findByStoreIdAndId(anyString(), anyString()))
        .thenReturn(new Category());
    when(productImageCleanupRepository.findLocationPathByProductCode(oldProduct.getProductCode())).thenReturn(new ArrayList<>());
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    Method method =
      ProductServiceBean.class.getDeclaredMethod("adjustProductItem", String.class,
        Product.class, Product.class, boolean.class, boolean.class, boolean.class);
    method.setAccessible(true);
    method.invoke(this.service, STORE_ID, oldProduct, newProduct, true, false, false);
    Mockito.verify(this.repository).saveAndFlush((Product) any());
    Mockito.verify(this.categoryService).getCategoryByStoreIdAndIdCached(
        anyString(), anyString());
    verify(productImageCleanupRepository).findLocationPathByProductCode(oldProduct.getProductCode());
    Mockito.verify(attributeService).getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID_2);
  }

  @Test
  public void adjustProductItemTest_deletedOldProductItemAndAddingBack() throws Exception {
    List<ProductCategory> productCategoryList = new ArrayList<ProductCategory>();
    productCategoryList.add(new ProductCategory());
    Product oldProduct = generateProduct();
    oldProduct.getProductItems().get(0).setHash(new byte[1024]);
    oldProduct.getProductItems().remove(1);
    oldProduct.getProductItems().get(0).setGeneratedItemName(DEFAULT_GENERATED_ITEM_NAME);
    oldProduct.getProductItems().get(0).setMarkForDelete(true);
    oldProduct.getProductItems().get(0).getProductItemAttributeValues().remove(1);
    Product newProduct = generateProduct();
    newProduct.setBrand("Value");
    newProduct.getProductItems().get(0).setHash(new byte[1024]);
    newProduct.getProductItems().get(0).setGeneratedItemName("DIFFERENT_GENERATED_ITEM_NAME");
    newProduct.getProductItems().remove(1);
    newProduct.getProductAttributes().remove(3);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(3);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(2);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(1);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).getAttribute()
        .setName("Brand");
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).setValue("Brand");
    newProduct.setProductCategories(productCategoryList);
    newProduct.getProductCategories().get(0).setCategory(new Category());
    newProduct.getProductCategories().get(0).getCategory().setId("1");
    newProduct.getProductItems().forEach(productItem1 -> productItem1.setNewlyAddedItem(true));
    Mockito.when(this.categoryService.findByStoreIdAndId(anyString(), anyString()))
        .thenReturn(new Category());
    when(productImageCleanupRepository.findLocationPathByProductCode(oldProduct.getProductCode())).thenReturn(new ArrayList<>());
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    Method method =
      ProductServiceBean.class.getDeclaredMethod("adjustProductItem", String.class,
        Product.class, Product.class, boolean.class, boolean.class, boolean.class);
    method.setAccessible(true);
    method.invoke(this.service, STORE_ID, oldProduct, newProduct, true, false, false);
    Mockito.verify(this.repository).saveAndFlush((Product) any());
    Mockito.verify(this.categoryService).getCategoryByStoreIdAndIdCached(
        anyString(), anyString());
    verify(productImageCleanupRepository).findLocationPathByProductCode(oldProduct.getProductCode());
    Mockito.verify(attributeService).getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID_2);
  }

  @Test
  public void adjustProductItemTest_deletedOldProductItemAndAddingBackWithCombinedUpdate() throws Exception {
    List<ProductCategory> productCategoryList = new ArrayList<ProductCategory>();
    productCategoryList.add(new ProductCategory());
    Product oldProduct = generateProduct();
    oldProduct.getProductItems().get(0).setHash(new byte[1024]);
    oldProduct.getProductItems().remove(1);
    oldProduct.getProductItems().get(0).setGeneratedItemName(DEFAULT_GENERATED_ITEM_NAME);
    oldProduct.getProductItems().get(0).setMarkForDelete(true);
    oldProduct.getProductItems().get(0).getProductItemAttributeValues().remove(1);
    Product newProduct = generateProduct();
    newProduct.setBrand("Value");
    newProduct.getProductItems().get(0).setHash(new byte[1024]);
    newProduct.getProductItems().get(0).setGeneratedItemName("DIFFERENT_GENERATED_ITEM_NAME");
    newProduct.getProductItems().remove(1);
    newProduct.getProductAttributes().remove(3);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(3);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(2);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(1);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).getAttribute()
      .setName("Brand");
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).setValue("Brand");
    newProduct.setProductCategories(productCategoryList);
    newProduct.getProductCategories().get(0).setCategory(new Category());
    newProduct.getProductCategories().get(0).getCategory().setId("1");
    newProduct.getProductItems().forEach(productItem1 -> productItem1.setNewlyAddedItem(true));
    Mockito.when(this.categoryService.findByStoreIdAndId(anyString(), anyString()))
      .thenReturn(new Category());
    when(productImageCleanupRepository.findLocationPathByProductCode(oldProduct.getProductCode())).thenReturn(new ArrayList<>());
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    Method method =
      ProductServiceBean.class.getDeclaredMethod("adjustProductItem", String.class,
        Product.class, Product.class, boolean.class, boolean.class, boolean.class);
    method.setAccessible(true);
    method.invoke(this.service, STORE_ID, oldProduct, newProduct, false, false, true);
    Mockito.verify(this.categoryService).getCategoryByStoreIdAndIdCached(
      anyString(), anyString());
    Mockito.verify(attributeService).getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID_2);
  }

  @Test
  public void adjustProductItemTest_deletedOldProductItem_differentHash() throws Exception {
    List<ProductCategory> productCategoryList = new ArrayList<ProductCategory>();
    List<Attribute> attributeList = new ArrayList<>();
    productCategoryList.add(new ProductCategory());
    Product oldProduct = generateProduct();
    oldProduct.getProductItems().get(0).setHash(new byte[1024]);
    oldProduct.getProductItems().remove(1);
    oldProduct.getProductItems().get(0).setGeneratedItemName(DEFAULT_GENERATED_ITEM_NAME);
    oldProduct.getProductItems().get(0).setMarkForDelete(true);
    Product newProduct = generateProduct();
    newProduct.setBrand("Brand");
    newProduct.getProductItems().get(0).setHash(new byte[1025]);
    newProduct.getProductItems().get(0).setGeneratedItemName("DIFFERENT_GENERATED_ITEM_NAME");
    newProduct.getProductItems().remove(1);
    newProduct.getProductAttributes().remove(3);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(3);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(2);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(1);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).getAttribute()
        .setName("Brand");
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).setValue("Brand");
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).setStoreId(STORE_ID);
    newProduct.setProductCategories(productCategoryList);
    newProduct.getProductCategories().get(0).setCategory(new Category());
    newProduct.getProductCategories().get(0).getCategory().setId("1");
    Mockito.when(this.repository.findById(anyString())).thenReturn(Optional.of(newProduct));
    newProduct.getProductImages().get(0).setId("1");
    Mockito.when(
        this.service.getCategoryService().findByStoreIdAndId(anyString(),
            anyString())).thenReturn(new Category());
    Mockito.when(this.attributeService.findByAttributeIds(anyString(), anyList()))
        .thenReturn(attributeList);
    when(productImageCleanupRepository.findLocationPathByProductCode(oldProduct.getProductCode())).thenReturn(new ArrayList<>());
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    Method method =
      ProductServiceBean.class.getDeclaredMethod("adjustProductItem", String.class,
        Product.class, Product.class, boolean.class, boolean.class, boolean.class);
    method.setAccessible(true);
    method.invoke(this.service, STORE_ID, oldProduct, newProduct, true, false, false);
    Mockito.verify(this.repository).saveAndFlush((Product) any());
    Mockito.verify(this.categoryService).getCategoryByStoreIdAndIdCached(
        anyString(), anyString());
    Mockito.verify(attributeService).getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID_2);
    verify(productImageCleanupRepository).findLocationPathByProductCode(oldProduct.getProductCode());
  }

  @Test
  public void adjustProductItemTestAddDeleteVariantsSwitch() throws Exception {
    ReflectionTestUtils.setField(service, "addDeleteVariantsSwitch", true);
    ReflectionTestUtils.setField(service, "ranchIntegrationEnabled", true);
    List<ProductCategory> productCategoryList = new ArrayList<>();
    List<Attribute> attributeList = new ArrayList<>();
    productCategoryList.add(new ProductCategory());
    Product oldProduct = generateProduct();
    oldProduct.getProductItems().get(0).setHash(new byte[1024]);
    oldProduct.getProductItems().remove(1);
    oldProduct.getProductItems().get(0).setGeneratedItemName(DEFAULT_GENERATED_ITEM_NAME);
    oldProduct.getProductItems().get(0).setMarkForDelete(true);
    Product newProduct = generateProduct();
    newProduct.setBrand("Brand");
    newProduct.getProductItems().get(0).setHash(new byte[1025]);
    newProduct.getProductItems().get(0).setGeneratedItemName("DIFFERENT_GENERATED_ITEM_NAME");
    newProduct.getProductItems().remove(1);
    newProduct.getProductAttributes().remove(3);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(3);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(2);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(1);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).getAttribute()
        .setName("Brand");
    newProduct.getProductItems().get(0).setNewlyAddedItem(false);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).setValue("Brand");
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).setStoreId(STORE_ID);
    newProduct.setProductCategories(productCategoryList);
    newProduct.getProductCategories().get(0).setCategory(new Category());
    newProduct.getProductCategories().get(0).getCategory().setId("1");
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    newProduct.getProductImages().get(0).setId("1");
    Mockito.when(
        this.service.getCategoryService().findByStoreIdAndId(anyString(),
            anyString())).thenReturn(new Category());
    Mockito.when(this.attributeService.findByAttributeIds(anyString(), anyList()))
        .thenReturn(attributeList);
    when(productImageCleanupRepository.findLocationPathByProductCode(oldProduct.getProductCode())).thenReturn(new ArrayList<>());
    Method method =
      ProductServiceBean.class.getDeclaredMethod("adjustProductItem", String.class,
        Product.class, Product.class, boolean.class, boolean.class, boolean.class);
    method.setAccessible(true);
    method.invoke(this.service, STORE_ID, oldProduct, newProduct, false, false, false);;
    Mockito.verify(this.repository).saveAndFlush((Product) any());
    Mockito.verify(this.categoryService).getCategoryByStoreIdAndIdCached(
        anyString(), anyString());
    Mockito.verify(attributeService).getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID_2);
    verify(productImageCleanupRepository).findLocationPathByProductCode(oldProduct.getProductCode());
  }

  @Test
  public void adjustProductItemTestAddDeleteVariantsSwitchTest() throws Exception {
    ReflectionTestUtils.setField(service, "addDeleteVariantsSwitch", true);
    ReflectionTestUtils.setField(service, "ranchIntegrationEnabled", true);
    List<ProductCategory> productCategoryList = new ArrayList<>();
    List<Attribute> attributeList = new ArrayList<>();
    productCategoryList.add(new ProductCategory());
    Product oldProduct = generateProduct();
    oldProduct.getProductItems().get(0).setHash(new byte[1024]);
    oldProduct.getProductItems().remove(1);
    oldProduct.getProductItems().get(0).setGeneratedItemName(DEFAULT_GENERATED_ITEM_NAME);
    oldProduct.getProductItems().get(0).setMarkForDelete(true);
    Product newProduct = generateProduct();
    newProduct.getProductItems().get(0).setNewlyAddedItem(true);
    newProduct.getProductItems().get(1).setNewlyAddedItem(true);
    newProduct.setBrand("Brand");
    newProduct.getProductItems().get(0).setHash(new byte[1025]);
    newProduct.getProductItems().get(0).setGeneratedItemName("DIFFERENT_GENERATED_ITEM_NAME");
    newProduct.getProductItems().remove(1);
    newProduct.getProductAttributes().remove(3);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(3);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(2);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(1);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).getAttribute()
        .setName("Brand");
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).setValue("Brand");
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).setStoreId(STORE_ID);
    newProduct.getProductItems().get(0).setProductItemUomInfo(new ProductItemUomInfo());
    newProduct.setProductCategories(productCategoryList);
    newProduct.getProductCategories().get(0).setCategory(new Category());
    newProduct.getProductCategories().get(0).getCategory().setId("1");
    Mockito.when(this.repository.findById(anyString())).thenReturn(Optional.of(newProduct));
    newProduct.getProductImages().get(0).setId("1");
    Mockito.when(
        this.service.getCategoryService().findByStoreIdAndId(anyString(),
            anyString())).thenReturn(new Category());
    Mockito.when(this.attributeService.findByAttributeIds(anyString(), anyList()))
        .thenReturn(attributeList);
    when(productImageCleanupRepository.findLocationPathByProductCode(oldProduct.getProductCode())).thenReturn(new ArrayList<>());
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    newProduct.getProductItems().forEach(productItem1 -> productItem1.getProductItemImages()
        .forEach(productItemImage -> productItemImage.setStoreId(STORE_ID)));
    Method method =
      ProductServiceBean.class.getDeclaredMethod("adjustProductItem", String.class,
        Product.class, Product.class, boolean.class, boolean.class, boolean.class);
    method.setAccessible(true);
    method.invoke(this.service, STORE_ID, oldProduct, newProduct, false, false, false);
    Mockito.verify(this.repository).saveAndFlush(productArgumentCaptor.capture());
    assertTrue(productArgumentCaptor.getValue().getProductItems().get(0).isNewlyAddedItem());
    Mockito.verify(this.categoryService).getCategoryByStoreIdAndIdCached(
        anyString(), anyString());
    Mockito.verify(attributeService).getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID_2);
    verify(productImageCleanupRepository).findLocationPathByProductCode(oldProduct.getProductCode());
  }

  @Test
  public void adjustProductItemTest_deletedOldProductItem_differentHash_attributeNameNotEqualsBrand() throws Exception {
    List<ProductCategory> productCategoryList = new ArrayList<ProductCategory>();
    List<String> attributeIds = new ArrayList<>();
    attributeIds.add(WARNA_ID);
    List<Attribute> attributeList = new ArrayList<>();
    productCategoryList.add(new ProductCategory());
    Product oldProduct = generateProduct();
    oldProduct.getProductItems().get(0).setHash(new byte[1024]);
    oldProduct.getProductItems().remove(1);
    oldProduct.getProductItems().get(0).setGeneratedItemName(DEFAULT_GENERATED_ITEM_NAME);
    oldProduct.getProductItems().get(0).setMarkForDelete(true);
    Product newProduct = generateProduct();
    newProduct.setBrand("Brand");
    newProduct.getProductItems().get(0).setHash(new byte[1025]);
    newProduct.getProductItems().get(0).setGeneratedItemName("DIFFERENT_GENERATED_ITEM_NAME");
    newProduct.getProductItems().remove(1);
    newProduct.getProductAttributes().remove(3);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(3);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(2);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().remove(1);
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).getAttribute()
        .setName("Value");
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).setValue("Brand");
    newProduct.getProductItems().get(0).getProductItemAttributeValues().get(0).setStoreId(STORE_ID);
    newProduct.setProductCategories(productCategoryList);
    newProduct.getProductCategories().get(0).setCategory(new Category());
    newProduct.getProductCategories().get(0).getCategory().setId("1");
    when(productImageCleanupRepository.findLocationPathByProductCode(oldProduct.getProductCode())).thenReturn(new ArrayList<>());
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    newProduct.getProductImages().get(0).setId("1");
    Mockito.when(this.categoryService.findByStoreIdAndId(anyString(), anyString()))
        .thenReturn(new Category());
    Mockito.when(this.attributeService.findByAttributeIds(STORE_ID, attributeIds))
        .thenReturn(attributeList);
    Method method =
      ProductServiceBean.class.getDeclaredMethod("adjustProductItem", String.class,
        Product.class, Product.class, boolean.class, boolean.class, boolean.class);
    method.setAccessible(true);
    method.invoke(this.service, STORE_ID, oldProduct, newProduct, false, false, false);
    Mockito.verify(this.repository).saveAndFlush((Product) any());
    Mockito.verify(this.categoryService).getCategoryByStoreIdAndIdCached(
        anyString(), anyString());
    Mockito.verify(attributeService).getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID_2);
    verify(productImageCleanupRepository).findLocationPathByProductCode(oldProduct.getProductCode());
  }

  @Test
  public void adjustProductAttributeValueTest_descriptiveAttributeValueTypeNone() throws Exception {
    ProductAttribute oldProductAttribute = new ProductAttribute();
    Attribute attribute = new Attribute();
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    oldProductAttribute.setProductAttributeValues(new ArrayList<ProductAttributeValue>());
    oldProductAttribute.getProductAttributeValues().add(new ProductAttributeValue());
    oldProductAttribute.getProductAttributeValues().get(0).setId("1");
    oldProductAttribute.setAttribute(attribute);
    ProductAttribute newProductAttribute = new ProductAttribute();
    newProductAttribute.setProductAttributeValues(new ArrayList<ProductAttributeValue>());
    newProductAttribute.getProductAttributeValues().add(new ProductAttributeValue());
    newProductAttribute.getProductAttributeValues().get(0).setProductAttribute(newProductAttribute);
    newProductAttribute.getProductAttributeValues().get(0).setId(null);
    newProductAttribute.getProductAttributeValues().get(0)
        .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
    newProductAttribute.getProductAttributeValues().get(0).setAllowedAttributeValue(new AllowedAttributeValue());
    newProductAttribute.setAttribute(attribute);
    Mockito.when(
        this.allowedAttributeValueService.findByStoreIdAndId(anyString(),
            anyString())).thenReturn(new AllowedAttributeValue());
    Method method =
        ProductServiceBean.class.getDeclaredMethod("adjustProductAttributeValue", String.class,
            ProductAttribute.class, ProductAttribute.class);
    method.setAccessible(true);
    method.invoke(this.service, STORE_ID, oldProductAttribute, newProductAttribute);
    Mockito.verify(this.allowedAttributeValueService).findByStoreIdAndId(any(),
        any());
  }

  @Test
  public void adjustProductAttributeValueTest_descriptiveAttributeValueTypePredefined()
      throws Exception {
    Attribute attribute1 = new Attribute();
    attribute1.setSkuValue(false);
    ProductAttribute oldProductAttribute = new ProductAttribute();
    oldProductAttribute.setProductAttributeValues(new ArrayList<ProductAttributeValue>());
    oldProductAttribute.getProductAttributeValues().add(new ProductAttributeValue());
    oldProductAttribute.getProductAttributeValues().get(0).setId("1");
    oldProductAttribute.setAttribute(attribute1);
    ProductAttribute newProductAttribute = new ProductAttribute();
    newProductAttribute.setProductAttributeValues(new ArrayList<ProductAttributeValue>());
    newProductAttribute.getProductAttributeValues().add(new ProductAttributeValue());
    newProductAttribute.getProductAttributeValues().get(0).setId(null);
    newProductAttribute.getProductAttributeValues().get(0)
        .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
    newProductAttribute.getProductAttributeValues().get(0)
        .setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValue());
    newProductAttribute.setAttribute(attribute1);
    newProductAttribute.getProductAttributeValues().get(0).setProductAttribute(newProductAttribute);
    Mockito.when(
        this.predefinedAllowedAttributeValueService.findByStoreIdAndId(anyString(),
            anyString())).thenReturn(new PredefinedAllowedAttributeValue());
    Method method =
        ProductServiceBean.class.getDeclaredMethod("adjustProductAttributeValue", String.class,
            ProductAttribute.class, ProductAttribute.class);
    method.setAccessible(true);
    method.invoke(this.service, STORE_ID, oldProductAttribute, newProductAttribute);
    Mockito.verify(this.predefinedAllowedAttributeValueService).findByStoreIdAndId(
        any(), any());
  }

  @Test
  public void adjustProductAttributeValueTest_descriptiveAttributeValueTypePredefinedSkuValueTrue()
      throws Exception {
    Attribute attribute1 = new Attribute();
    attribute1.setSkuValue(true);
    ProductAttribute oldProductAttribute = new ProductAttribute();
    oldProductAttribute.setProductAttributeValues(new ArrayList<ProductAttributeValue>());
    oldProductAttribute.getProductAttributeValues().add(new ProductAttributeValue());
    oldProductAttribute.getProductAttributeValues().get(0).setId("1");
    oldProductAttribute.setAttribute(attribute1);
    ProductAttribute newProductAttribute = new ProductAttribute();
    newProductAttribute.setProductAttributeValues(new ArrayList<ProductAttributeValue>());
    newProductAttribute.getProductAttributeValues().add(new ProductAttributeValue());
    newProductAttribute.getProductAttributeValues().get(0).setId(null);
    newProductAttribute.getProductAttributeValues().get(0)
        .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
    newProductAttribute.getProductAttributeValues().get(0)
        .setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValue());
    newProductAttribute.setAttribute(attribute1);
    newProductAttribute.getProductAttributeValues().get(0).setProductAttribute(newProductAttribute);
    Mockito.when(
        this.predefinedAllowedAttributeValueService.findByStoreIdAndId(anyString(),
            anyString())).thenReturn(new PredefinedAllowedAttributeValue());
    Method method =
        ProductServiceBean.class.getDeclaredMethod("adjustProductAttributeValue", String.class,
            ProductAttribute.class, ProductAttribute.class);
    method.setAccessible(true);
    method.invoke(this.service, STORE_ID, oldProductAttribute, newProductAttribute);
    Mockito.verify(this.predefinedAllowedAttributeValueService, Mockito.times(0)).findByStoreIdAndId(
        anyString(), anyString());
  }

  @Test
  public void adjustProductAttributeValueTest_descriptiveAttributeValueTypePredefinedSkuValueFalseAndPredefinedAllowedAttributeValueNull()
          throws Exception {
    Attribute attribute1 = new Attribute();
    attribute1.setSkuValue(false);
    ProductAttribute oldProductAttribute = new ProductAttribute();
    oldProductAttribute.setProductAttributeValues(new ArrayList<ProductAttributeValue>());
    oldProductAttribute.getProductAttributeValues().add(new ProductAttributeValue());
    oldProductAttribute.getProductAttributeValues().get(0).setId("1");
    oldProductAttribute.setAttribute(attribute1);
    ProductAttribute newProductAttribute = new ProductAttribute();
    newProductAttribute.setProductAttributeValues(new ArrayList<ProductAttributeValue>());
    newProductAttribute.getProductAttributeValues().add(new ProductAttributeValue());
    newProductAttribute.getProductAttributeValues().get(0).setId(null);
    newProductAttribute.getProductAttributeValues().get(0)
            .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
    newProductAttribute.setAttribute(attribute1);
    newProductAttribute.getProductAttributeValues().get(0).setProductAttribute(newProductAttribute);
    Mockito.when(
            this.predefinedAllowedAttributeValueService.findByStoreIdAndId(anyString(),
                    anyString())).thenReturn(new PredefinedAllowedAttributeValue());
    Method method =
            ProductServiceBean.class.getDeclaredMethod("adjustProductAttributeValue", String.class,
                    ProductAttribute.class, ProductAttribute.class);
    method.setAccessible(true);
    method.invoke(this.service, STORE_ID, oldProductAttribute, newProductAttribute);
    Mockito.verify(this.predefinedAllowedAttributeValueService, Mockito.times(0)).findByStoreIdAndId(
            anyString(), anyString());
  }


  @Test
  public void adjustProductAttributeValueTest_descriptiveAttributeValueTypeOther() throws Exception {
    ProductAttribute oldProductAttribute = new ProductAttribute();
    oldProductAttribute.setProductAttributeValues(new ArrayList<ProductAttributeValue>());
    oldProductAttribute.getProductAttributeValues().add(new ProductAttributeValue());
    oldProductAttribute.getProductAttributeValues().get(0).setId("1");
    ProductAttribute newProductAttribute = new ProductAttribute();
    newProductAttribute.setProductAttributeValues(new ArrayList<ProductAttributeValue>());
    newProductAttribute.getProductAttributeValues().add(new ProductAttributeValue());
    newProductAttribute.getProductAttributeValues().get(0).setId(null);
    newProductAttribute.getProductAttributeValues().get(0)
        .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    newProductAttribute.getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue("Descriptive attribute value");
    Method method =
        ProductServiceBean.class.getDeclaredMethod("adjustProductAttributeValue", String.class,
            ProductAttribute.class, ProductAttribute.class);
    method.setAccessible(true);
    method.invoke(this.service, STORE_ID, oldProductAttribute, newProductAttribute);
  }

  @Test
  public void generateProductItemTest() throws Exception {
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setAttribute(new Attribute());
    productItemAttributeValue.getAttribute().setName("Brand");
    List<ProductItemAttributeValue> productItemAttributeValues =
        new ArrayList<ProductItemAttributeValue>();
    productItemAttributeValues.add(productItemAttributeValue);
    Method method =
        ProductServiceBean.class.getDeclaredMethod("generateProductItem", Product.class,
            List.class, List.class, int.class, int.class, String.class, String.class, List.class);
    method.setAccessible(true);
    method.invoke(this.service, new Product(), new ArrayList<ProductAttribute>(),
        new ArrayList<ProductAttribute>(), 2, 1, "item name", "result", productItemAttributeValues);
    Mockito.verify(this.productItemService).getSequence(any());
  }

  @Test
  public void republishProductByProductCodesTest() throws Exception {
    List<String> productCodes = new ArrayList<>();
    productCodes.add(PRODUCT_CODE);
    this.service.republishProductByProductCodes(STORE_ID, productCodes, "operation type");
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verifyProductDetailInteractionsForMarkForDeleteFalseCached();
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(DEFAULT_STORE_ID, product, false);
    Mockito.verify(this.domainEventPublisherService)
        .publishProductChangeCategory(productArgumentCaptor.capture(), productSalesCategoryMappingArgumentCaptor.capture(),
            anyBoolean(), eq(Boolean.TRUE), eq(Boolean.TRUE), eq(false), eq(new HashSet<>()));
  }

  @Test
  public void republishProductByProductCodesPublishItemPickupPointChangeEventTest() throws Exception {
    List<String> productCodes = new ArrayList<>();
    productCodes.add(PRODUCT_CODE);
    this.service.republishProductByProductCodes(STORE_ID, productCodes, ProductPublishEventType.PUBLISH_ITEM_PICKUP_POINT_DATA_CHANGE_EVENT.name());
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verifyProductDetailInteractionsForMarkForDeleteFalseCached();
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(DEFAULT_STORE_ID, product, false);
    Mockito.verify(this.domainEventPublisherService)
        .publishProductChangeCategory(productArgumentCaptor.capture(), productSalesCategoryMappingArgumentCaptor.capture(),
            anyBoolean(), eq(Boolean.TRUE), eq(Boolean.TRUE), eq(false), eq(ImmutableSet.of(ProductPublishEventType.PUBLISH_ITEM_PICKUP_POINT_DATA_CHANGE_EVENT.name())));
  }

  @Test
  public void republishProductByProductCodesTest_emptyProductCodes() throws Exception {
    List<String> productCodes = new ArrayList<String>();
    productCodes.add(new String());
    this.service
        .republishProductByProductCodes(STORE_ID, productCodes, "operation type");
  }

  @Test
  public void findByProductCodeExactMatchTest() {
    this.service.findByProductCodeExactMatch(STORE_ID, PRODUCT_CODE, DEFAULT_PAGEABLE);
    Mockito.verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_CODE, DEFAULT_PAGEABLE);
  }

  @Test
  public void getProductCountForViewableTest() {
    this.service.getProductCountForViewable(STORE_ID, true);
    Mockito.verify(this.repository).countByStoreIdAndViewableAndMarkForDeleteFalse(STORE_ID, true);
  }

  @Test
  public void getProductCountByBrandNameTest() {
    this.service.getProductCountByBrandName(STORE_ID, BRAND_1_NAME);
    Mockito.verify(this.repository).countByStoreIdAndBrandIgnoreCase(STORE_ID, BRAND_1_NAME);
  }

  @Test
  public void replaceProductImagesTest() throws Exception {
    product.setProductItems(productItems);
    product.setActivated(false);
    product.setViewable(false);
    this.service.replaceProductImages(STORE_ID, this.replaceProductImages);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(DEFAULT_STORE_ID, ProductServiceTest.PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(DEFAULT_STORE_ID, product, true);
    verify(this.productImageRepository).deleteAll(anyList());
    verify(this.productItemImageRepository, Mockito.times(2)).deleteAll(anyList());
    verify(applicationCacheServiceBean).evictProductCacheByStoreIdAndProductCode(DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(applicationCacheServiceBean).evictProductCacheByStoreIdAndProductCode(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductItemsCacheByStoreIdAndProductId(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductItemImagesCacheByStoreIdAndProductId(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(this.repository).save(product);
  }

  @Test
  public void replaceProductImagesWhenErrorActiveTrue() throws Exception {
    this.product.setProductItems(new ArrayList<ProductItem>());
    ProductItem productItem = new ProductItem();
    productItem.setSkuCode(PRODUCT_CODE);
    this.product.getProductItems().add(productItem);
    this.product.setActivated(true);
    this.product.setViewable(false);

    productItem = new ProductItem();
    this.product.getProductItems().add(productItem);
    when(this.repository.findByStoreIdAndProductCode(anyString(), anyString())).thenReturn(this.product);

    try{
      Assertions.assertThrows(ApplicationException.class, () -> this.service.replaceProductImages(STORE_ID, this.replaceProductImages));
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void replaceProductImagesWhenErrorViewableTrue() throws Exception {
    this.product.setProductItems(new ArrayList<ProductItem>());
    ProductItem productItem = new ProductItem();
    productItem.setSkuCode(PRODUCT_CODE);
    this.product.getProductItems().add(productItem);
    this.product.setActivated(false);
    this.product.setViewable(true);

    productItem = new ProductItem();
    this.product.getProductItems().add(productItem);
    try{
      Assertions.assertThrows(ApplicationException.class, () -> this.service.replaceProductImages(STORE_ID, this.replaceProductImages));
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void replaceProductImagesWhenErrorActivatedAndViewableTrue() throws Exception {
    this.product.setActivated(true);
    this.product.setViewable(true);
    try{
      Assertions.assertThrows(ApplicationException.class, () -> this.service.replaceProductImages(STORE_ID, this.replaceProductImages));
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void validateProductPromoSkuTest() {
    product.setPromoSKU(true);
    assertTrue(service.validateProductPromoSku(PRODUCT_ID, STORE_ID, IS_PROMO_SKU));
    Mockito.verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void validateProductPromoSkuWithReturnFalseTest() {
    Assertions.assertFalse(service.validateProductPromoSku(PRODUCT_ID, STORE_ID, IS_PROMO_SKU));
    Mockito.verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void findProductsByStoreIdAndUpdatedDateBetweenTest() {
    Date startDate = new Date();
    Date endDate = new Date();
    service.findProductsByStoreIdAndUpdatedDateBetween(STORE_ID, startDate, endDate, DEFAULT_PAGEABLE);
    Mockito.verify(repository).findByStoreIdAndReviewPendingFalseAndUpdatedDateBetween(STORE_ID, startDate, endDate, DEFAULT_PAGEABLE);
  }

  @Test
  public void findByViewableAndActivatedWithItemsInitializedTest() {
    Mockito.when(repository
        .findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseAndReviewPendingFalseOrderByUpdatedDateDesc(
            ProductServiceTest.STORE_ID, true, true, ProductServiceTest.DEFAULT_PAGEABLE))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    this.service.findByViewableAndActivatedWithItemsInitialized(ProductServiceTest.STORE_ID, true, true,
        ProductServiceTest.DEFAULT_PAGEABLE);
    verify(this.repository, times(1))
        .findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseAndReviewPendingFalseOrderByUpdatedDateDesc(
            ProductServiceTest.STORE_ID, true, true, ProductServiceTest.DEFAULT_PAGEABLE);
  }

  @Test
  public void findByCategoryIdWithItemsInitializedTest(){
    when(this.repository.findByStoreIdAndCategoryIdAndMarkForDeleteFalseAndActivatedTrue(
        STORE_ID, CATEGORY_ID, DEFAULT_PAGEABLE)).thenReturn(this.productPage);
    this.service.findByCategoryIdWithItemsInitialized(STORE_ID, CATEGORY_ID, DEFAULT_PAGEABLE);
    verify(this.repository, times(1)).findByStoreIdAndCategoryIdAndMarkForDeleteFalseAndActivatedTrue(
        STORE_ID, CATEGORY_ID, ProductServiceTest.DEFAULT_PAGEABLE);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(DEFAULT_STORE_ID, CATEGORY_ID_1);
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(DEFAULT_STORE_ID, product, false);
  }

  @Test
  public void findByViewableAndActivatedTest() throws Exception {
    product.setProductItems(productItems);
    when(
        this.repository.findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseAndReviewPendingFalseOrderByUpdatedDateDesc(
            ProductServiceTest.STORE_ID, true, true, ProductServiceTest.DEFAULT_PAGEABLE))
        .thenReturn(new PageImpl<>(Arrays.asList(product)));
    Page<Product> products = this.service
        .findByViewableAndActivatedWithItemsInitialized(ProductServiceTest.STORE_ID, true, true,
            ProductServiceTest.DEFAULT_PAGEABLE);
    verify(this.repository)
        .findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseAndReviewPendingFalseOrderByUpdatedDateDesc(
            ProductServiceTest.STORE_ID, true, true, ProductServiceTest.DEFAULT_PAGEABLE);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    assertEquals(1, products.getContent().size());
    assertEquals(1, products.getContent().get(0).getProductItems().get(0).getProductItemImages().size());
  }

  @Test
  public void findProductsByStoreIdAndUpdatedDateBetweenWithInitializationTest(){
    when(repository.findByStoreIdAndReviewPendingFalseAndUpdatedDateBetween(anyString(), any(), any(), any()))
      .thenReturn(productPage);
    Date startDate = new Date();
    Date endDate = new Date();
    this.service.findProductsByStoreIdAndUpdatedDateBetweenWithInitialization(STORE_ID, startDate,
        endDate, DEFAULT_PAGEABLE);
    Mockito.verify(repository).findByStoreIdAndReviewPendingFalseAndUpdatedDateBetween(STORE_ID, startDate, endDate,
        DEFAULT_PAGEABLE);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_2);
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
  }

  @Test
  public void publishProductByStoreIdAndUpdatedByTest() throws Exception {
    when(repository.findByStoreIdAndUpdatedByAndMarkForDeleteFalse(eq(STORE_ID), eq(UPDATED_BY),
        any())).thenReturn(productPage);
    this.service.publishProductByStoreIdAndUpdatedBy(STORE_ID, UPDATED_BY);
    verify(repository).findByStoreIdAndUpdatedByAndMarkForDeleteFalse(eq(STORE_ID),
        eq(UPDATED_BY), pageRequestArgumentCaptor.capture());
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    verify(domainEventPublisherService).publishProduct(product);
    PageRequest pageRequest = pageRequestArgumentCaptor.getValue();
    assertEquals(100, pageRequest.getPageSize());
    assertEquals(0, pageRequest.getPageNumber());
  }

  @Test
  public void publishProductByStoreIdAndUpdatedByExceptionTest() throws Exception {
    when(repository.findByStoreIdAndUpdatedByAndMarkForDeleteFalse(eq(STORE_ID), eq(UPDATED_BY), any()))
        .thenReturn(productPage);
    when(domainEventPublisherService.publishProduct(any())).thenThrow(new ApplicationRuntimeException());
    this.service.publishProductByStoreIdAndUpdatedBy(STORE_ID, UPDATED_BY);
    verify(repository).findByStoreIdAndUpdatedByAndMarkForDeleteFalse(eq(STORE_ID), eq(UPDATED_BY),
        pageRequestArgumentCaptor.capture());
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    verify(domainEventPublisherService).publishProduct(product);
  }

  @Test
  public void getActiveProductCodes() {
    this.service.getActiveProductCodes(STORE_ID, DEFAULT_PAGEABLE);
    verify(repository, times(1)).getAllProductCodes(STORE_ID, DEFAULT_PAGEABLE);
  }

  @Test
  public void getCategoryHierarchyByUPCCodeTest() throws Exception {
    when(productItemService.getCategoryIdsWithProductCountForUPCCode(UPC_CODE, true)).thenReturn(categoryCountMap);
    when(categoryService.findCategoryHierarchyByCategoryId(STORE_ID, CATEGORY_ID)).thenReturn(Arrays.asList(category));
    List<CategoryHierarchyResponse> response = this.service.getCategoryHierarchyByUPCCode(STORE_ID, UPC_CODE, true);
    verify(productItemService).getCategoryIdsWithProductCountForUPCCode(UPC_CODE, true);
    verify(categoryService).findCategoryHierarchyByCategoryId(STORE_ID, CATEGORY_ID);
    assertEquals(1, response.size());
    assertEquals(CATEGORY_ID, response.get(0).getCategoryId());
  }

  @Test
  public void getCategoryHierarchyByUPCCodeExceptionTest() throws Exception {
    when(productItemService.getCategoryIdsWithProductCountForUPCCode(UPC_CODE, true)).thenReturn(categoryCountMap);
    when(categoryService.findCategoryHierarchyByCategoryId(STORE_ID, CATEGORY_ID))
        .thenThrow(new ApplicationRuntimeException());
    try {
      List<CategoryHierarchyResponse> response = this.service.getCategoryHierarchyByUPCCode(STORE_ID, UPC_CODE, true);
    } catch (ApplicationRuntimeException e) {
      verify(productItemService).getCategoryIdsWithProductCountForUPCCode(UPC_CODE, true);
      verify(categoryService).findCategoryHierarchyByCategoryId(STORE_ID, CATEGORY_ID);
    }
  }

  @Test
  public void setCompleteProductDetailsTest() {
    service.setCompleteProductDetailsCached(DEFAULT_STORE_ID, product, false);
    verifyProductDetailInteractionsForMarkForDeleteFalseCached();
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(DEFAULT_STORE_ID, product, false);
    assertEquals(3, product.getProductImages().size());
    assertTrue(product.getProductImages().get(0).isMainImages());
    Assertions.assertFalse(product.getProductImages().get(1).isMainImages());
    assertTrue(product.getProductImages().get(2).isMainImages());
    assertTrue(product.getProductImages().get(2).isActive());
  }

  @Test
  public void setCompleteProductDetailsIncludeMFDImagesTest() {
    service.setCompleteProductDetailsCached(DEFAULT_STORE_ID, product, true);
    verifyProductDetailInteractionsForMarkForDeleteTrueCached();
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(DEFAULT_STORE_ID, product, true);
    assertEquals(4, product.getProductImages().size());
    assertTrue(product.getProductImages().get(0).isMainImages());
    Assertions.assertFalse(product.getProductImages().get(1).isMainImages());
    assertTrue(product.getProductImages().get(2).isMainImages());
    assertTrue(product.getProductImages().get(2).isActive());
    assertTrue(product.getProductImages().get(3).isMarkForDelete());
  }

  @Test
  public void setCompleteProductDetailsWithOriginalImagesTwoMainImagesTest() {
    service.setCompleteProductDetailsCached(DEFAULT_STORE_ID, product, false);
    verifyProductDetailInteractionsForMarkForDeleteFalseCached();
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(DEFAULT_STORE_ID, product, false);
    ProductImage productImage = new ProductImage();
    productImage.setOriginalImage(true);
    productImage.setMainImages(true);
    ProductImage productImage1 = new ProductImage();
    productImage1.setOriginalImage(false);
    productImage1.setMainImages(true);
    ProductImage productImage2 = new ProductImage();
    productImage2.setOriginalImage(null);
    productImage2.setMainImages(true);
    ProductImage productImage3 = new ProductImage();
    productImage3.setOriginalImage(true);
    productImage3.setMainImages(true);
    ProductImage productImage4 = new ProductImage();
    productImage4.setOriginalImage(false);
    productImage4.setMainImages(true);
    ProductImage productImage5 = new ProductImage();
    productImage5.setOriginalImage(null);
    productImage5.setMainImages(true);
    productImages = Arrays.asList(productImage, productImage1, productImage2, productImage3, productImage4, productImage5);
    assertEquals(3, product.getProductImages().size());
  }

  @Test
  public void setCompleteProductDetailsNoMainImageTest() {
    service.setCompleteProductDetailsCached(DEFAULT_STORE_ID, product, false);
    ProductImage productImage1 = new ProductImage();
    productImage1.setMainImages(false);
    productImage1.setSequence(1);
    ProductImage productImage2 = new ProductImage();
    productImage2.setMarkForDelete(true);
    ProductImage productImage3 = new ProductImage();
    productImage3.setMainImages(false);
    productImage3.setSequence(0);
    ProductImage productImage4 = new ProductImage();
    productImage4.setMainImages(false);
    productImage4.setSequence(2);
    verifyProductDetailInteractionsForMarkForDeleteFalseCached();
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(DEFAULT_STORE_ID, product, false);
    assertEquals(3, product.getProductImages().size());
    assertTrue(product.getProductImages().get(0).isMainImages());
    Assertions.assertFalse(product.getProductImages().get(1).isMainImages());
    assertTrue(product.getProductImages().get(2).isMainImages());
  }

  @Test
  public void setCompleteProductDetailsWithMarkForDeleteTrueTest() {
    service.setCompleteProductDetailsCached(DEFAULT_STORE_ID, product, true);
    verifyProductDetailInteractionsForMarkForDeleteTrueCached();
  }

  @Test
  public void publishProductTest() throws Exception {
    service.publishProduct(product, true);
    verify(domainEventPublisherService).publishProduct(product, true);
  }

  private void verifyProductDetailInteractionsForMarkForDeleteFalseCached() {
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(DEFAULT_STORE_ID, CATEGORY_ID_1);
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(DEFAULT_STORE_ID, product, false);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
  }

  private void verifyProductDetailInteractionsForMarkForDeleteTrueCached() {
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(DEFAULT_STORE_ID, CATEGORY_ID_1);
    verify(categoryService).getCategoryByStoreIdAndIdCached(DEFAULT_STORE_ID, CATEGORY_ID_2);
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(DEFAULT_STORE_ID, product, true);
  }

  @Test
  public void evictAllProductDetailCacheByProductCodeTest() {
    product.setProductItems(productItems);
    service.evictAllProductDetailCacheByProductCode(DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(DEFAULT_STORE_ID, PRODUCT_CODE);
    verifyEvictAllProductDetailCache();
  }

  @Test
  public void evictAllProductDetailCacheTest() {
    service.evictAllProductDetailCache(DEFAULT_STORE_ID, product);
    verifyEvictAllProductDetailCache();
  }

  private void verifyEvictAllProductDetailCache() {
    verify(applicationCacheServiceBean).evictProductCacheByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(applicationCacheServiceBean).evictProductCacheByStoreIdAndProductCode(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductItemsCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductAttributesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductCategoriesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductItemImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductItemAttributeValuesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }

  private void verifyEvictAllProductDetailCacheByProduct(Product product) {
    verify(applicationCacheServiceBean).evictProductCacheByStoreIdAndProductCode(product.getStoreId(), product.getId());
    verify(applicationCacheServiceBean).evictProductCacheByStoreIdAndProductCode(product.getStoreId(), product.getProductCode());
    verify(applicationCacheServiceBean).evictProductItemsCacheByStoreIdAndProductId(product.getStoreId(), product.getId());
    verify(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(product.getStoreId(), product.getId());
    verify(applicationCacheServiceBean).evictProductCategoriesCacheByStoreIdAndProductId(product.getStoreId(), product.getId());
    verify(applicationCacheServiceBean).evictProductAttributesCacheByStoreIdAndProductId(product.getStoreId(), product.getId());
    verify(applicationCacheServiceBean).evictProductItemImagesCacheByStoreIdAndProductId(product.getStoreId(), product.getId());
    verify(applicationCacheServiceBean).evictProductItemAttributeValuesCacheByStoreIdAndProductId(
        product.getStoreId(), product.getId());
  }

  @Test
  public void deleteImageFoldersTest() throws IOException {
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(BASE_FOLDER, PRODUCT_CODE1);
    directory1.mkdir();
    File file1 = new File(directory1, FILE_NAME1);
    File file2 = new File(directory1, FILE_NAME2);
    file1.createNewFile();
    file2.createNewFile();
    service.deleteImageFolders(Arrays.asList(PRODUCT_CODE1, PRODUCT_CODE2));
    Assertions.assertFalse(directory1.exists());
    Assertions.assertFalse(file1.exists());
    Assertions.assertFalse(file2.exists());
    FileUtils.deleteDirectory(mainDirectory);
  }

  @Test
  public void deleteImagesForDeletedProductTest() throws IOException {
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(mainDirectory, PRODUCT_CODE2);
    directory1.mkdir();
    File directory2 = new File(mainDirectory, PRODUCT_CODE1);
    directory2.mkdir();
    File file1 = new File(directory1, "Testing1.txt");
    File file2 = new File(directory2, "Testing2.txt");
    file1.createNewFile();
    file2.createNewFile();
    when(repository.findProductCodeByStoreIdAndMarkForDeleteTrueAndUpdatedDateBetween(eq(STORE_ID),
        any(Date.class), any(Date.class), eq(PageRequest.of(0, 10))))
        .thenReturn(new PageImpl<>(productList, PageRequest.of(0, 10), 2));
    service.deleteImagesForDeletedProduct(STORE_ID, 1, 1, 10);
    Mockito.verify(repository).findProductCodeByStoreIdAndMarkForDeleteTrueAndUpdatedDateBetween(eq(STORE_ID),
        any(Date.class), any(Date.class), eq(PageRequest.of(0, 10)));
    Assertions.assertFalse(directory1.exists());
    Assertions.assertFalse(directory2.exists());
    Assertions.assertFalse(file1.exists());
    Assertions.assertFalse(file2.exists());
    FileUtils.deleteDirectory(mainDirectory);
  }

  @Test
  public void deleteImagesForDeletedProductExceptionTest() throws IOException {
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(mainDirectory, PRODUCT_CODE2);
    directory1.mkdir();
    File directory2 = new File(mainDirectory, PRODUCT_CODE1);
    directory2.mkdir();
    File file1 = new File(directory1, "Testing1.txt");
    File file2 = new File(directory2, "Testing2.txt");
    file1.createNewFile();
    file2.createNewFile();
    Mockito.doThrow(ApplicationRuntimeException.class).when(repository).findProductCodeByStoreIdAndMarkForDeleteTrueAndUpdatedDateBetween(
        eq(STORE_ID),
        any(Date.class), any(Date.class), eq(PageRequest.of(0, 10)));
    service.deleteImagesForDeletedProduct(STORE_ID, 1, 1, 10);
    Mockito.verify(repository).findProductCodeByStoreIdAndMarkForDeleteTrueAndUpdatedDateBetween(eq(STORE_ID),
        any(Date.class), any(Date.class), eq(PageRequest.of(0, 10)));
    assertTrue(directory1.exists());
    assertTrue(directory2.exists());
    assertTrue(file1.exists());
    assertTrue(file2.exists());
    FileUtils.deleteDirectory(mainDirectory);
  }
  
  @Test
  public void deleteImagesForDeletedProductWith0SpanTest() throws IOException {
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(mainDirectory, PRODUCT_CODE2);
    directory1.mkdir();
    File directory2 = new File(mainDirectory, PRODUCT_CODE1);
    directory2.mkdir();
    File file1 = new File(directory1, FILE_NAME1);
    File file2 = new File(directory2, FILE_NAME2);
    file1.createNewFile();
    file2.createNewFile();
    when(repository.findProductCodeByStoreIdAndMarkForDeleteTrueAndUpdatedDateLessThan(eq(STORE_ID),
        any(Date.class), eq(PageRequest.of(0, 10))))
        .thenReturn(new PageImpl<>(productList, PageRequest.of(0, 10), 2));
    service.deleteImagesForDeletedProduct(STORE_ID, 1, 0, 10);
    Mockito.verify(repository).findProductCodeByStoreIdAndMarkForDeleteTrueAndUpdatedDateLessThan(eq(STORE_ID),
        any(Date.class), eq(PageRequest.of(0, 10)));
    Assertions.assertFalse(directory1.exists());
    Assertions.assertFalse(directory2.exists());
    Assertions.assertFalse(file1.exists());
    Assertions.assertFalse(file2.exists());
    FileUtils.deleteDirectory(mainDirectory);
  }

  @Test
  public void deleteImagesForUpdatedProductTest() throws IOException {
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(mainDirectory, "folder1");
    directory1.mkdir();
    File directory2 = new File(mainDirectory, "folder2");
    directory2.mkdir();
    File file1 = new File(directory1, FILE_NAME1);
    File file2 = new File(directory2, FILE_NAME2);
    file1.createNewFile();
    file2.createNewFile();
    imageLocation.add(FILE_NAME1);
    imageLocation.add(FILE_NAME2);
    when(productImageCleanupRepository
        .findLocationPathByUpdatedDateLessThan(any(Date.class), eq(PageRequest.of(0, 10))))
        .thenReturn(new PageImpl<>(imageLocation, PageRequest.of(0, 10), 2));
    Mockito.doNothing().when(productImageCleanupRepository).deleteByLocationPath(imageLocation);
    service.deleteImagesForUpdatedProduct(1, 10);
    Mockito.verify(productImageCleanupRepository)
        .findLocationPathByUpdatedDateLessThan(any(Date.class), eq(PageRequest.of(0, 10)));
    Mockito.verify(productImageCleanupRepository).deleteByLocationPath(imageLocation);
    FileUtils.deleteDirectory(mainDirectory);
  }

  @Test
  public void deleteImagesForUpdatedProductExceptionTest() throws IOException {
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(mainDirectory, "folder1");
    directory1.mkdir();
    File directory2 = new File(mainDirectory, "folder2");
    directory2.mkdir();
    File file1 = new File(directory1, FILE_NAME1);
    File file2 = new File(directory2, FILE_NAME2);
    file1.createNewFile();
    file2.createNewFile();
    imageLocation.add(FILE_NAME1);
    imageLocation.add(FILE_NAME2);
    Mockito.doThrow(ApplicationRuntimeException.class).when(productImageCleanupRepository)
        .findLocationPathByUpdatedDateLessThan(any(Date.class), eq(PageRequest.of(0, 10)));
    service.deleteImagesForUpdatedProduct(1, 10);
    Mockito.verify(productImageCleanupRepository)
        .findLocationPathByUpdatedDateLessThan(any(Date.class), eq(PageRequest.of(0, 10)));
    Mockito.verify(productImageCleanupRepository, times(0)).deleteByLocationPath(imageLocation);
    assertTrue(file1.exists());
    assertTrue(file2.exists());
    FileUtils.deleteDirectory(mainDirectory);
  }

  @Test
  public void deleteImageByImageLocationsTest() throws IOException {
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(FOLDER1);
    directory1.mkdir();
    File directory2 = new File(FOLDER2);
    directory2.mkdir();
    File directory3 = new File(FOLDER3);
    directory3.mkdir();
    File directory4 = new File(directory1, "1");
    directory4.mkdir();
    File directory5 = new File(directory2, "1");
    directory5.mkdir();
    File directory6 = new File(directory3, "1");
    directory6.mkdir();
    File directory7 = new File(directory4, PRODUCT_CODE2);
    directory7.mkdir();
    File directory8 = new File(directory5, PRODUCT_CODE2);
    directory8.mkdir();
    File directory9 = new File(directory6, PRODUCT_CODE2);
    directory9.mkdir();
    File file1 = new File(directory7 + File.separator + FILE_NAME1);
    File file2 = new File(directory7 + File.separator + FILE_NAME2);
    file1.createNewFile();
    file2.createNewFile();
    File file3 = new File(directory8 + File.separator + FILE_NAME1);
    File file4 = new File(directory8 + File.separator + FILE_NAME2);
    file3.createNewFile();
    file4.createNewFile();
    File file5 = new File(directory9 + File.separator + FILE_NAME1);
    File file6 = new File(directory9 + File.separator + FILE_NAME2);
    file5.createNewFile();
    file6.createNewFile();
    Set<String> set = new HashSet<>();
    set.add(FILE_NAME_PATH1);
    set.add(FILE_NAME_PATH2);
    service.deleteImageByImageLocations(set, false);
    FileUtils.deleteDirectory(new File(BASE_FOLDER));
  }

  @Test
  public void deleteSourceImageByImageLocationsTest() throws IOException {
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File file1 = new File(mainDirectory + File.separator + FILE_NAME1);
    File file2 = new File(mainDirectory + File.separator + FILE_NAME2);
    file1.createNewFile();
    file2.createNewFile();
    Set<String> set = new HashSet<>();
    set.add(FILE_NAME1);
    set.add(File.separator + FILE_NAME2);
    set.add(FILE_NAME_PATH1);
    service.deleteImageByImageLocations(set);
    FileUtils.deleteDirectory(new File(BASE_FOLDER));
  }

  @Test
  public void deleteImageByImageLocationsWithSourceLocationTest() throws IOException {
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(FOLDER1);
    directory1.mkdir();
    File directory2 = new File(FOLDER2);
    directory2.mkdir();
    File directory3 = new File(FOLDER3);
    directory3.mkdir();
    File directory4 = new File(directory1, "1");
    directory4.mkdir();
    File directory5 = new File(directory2, "1");
    directory5.mkdir();
    File directory6 = new File(directory3, "1");
    directory6.mkdir();
    File directory7 = new File(directory4, PRODUCT_CODE2);
    directory7.mkdir();
    File directory8 = new File(directory5, PRODUCT_CODE2);
    directory8.mkdir();
    File directory9 = new File(directory6, PRODUCT_CODE2);
    directory9.mkdir();
    File file1 = new File(directory7 + File.separator + FILE_NAME1);
    File file2 = new File(directory7 + File.separator + FILE_NAME2);
    file1.createNewFile();
    file2.createNewFile();
    File file3 = new File(directory8 + File.separator + FILE_NAME1);
    File file4 = new File(directory8 + File.separator + FILE_NAME2);
    file3.createNewFile();
    file4.createNewFile();
    File file5 = new File(directory9 + File.separator + FILE_NAME1);
    File file6 = new File(directory9 + File.separator + FILE_NAME2);
    file5.createNewFile();
    file6.createNewFile();
    Set<String> set = new HashSet<>();
    set.add(FILE_NAME_PATH1);
    set.add(FILE_NAME_PATH2);
    service.deleteImageByImageLocations(set, true);
    FileUtils.deleteDirectory(new File(BASE_FOLDER));
  }

  @Test
  public void updateMasterProductDataProductNameChangeTest() throws Exception {
    simpleMasterProductUpdateDTO.setPostLive(true);
    product.setName(PRODUCT_NAME);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    product.setProductAttributes(new ArrayList<>());
    product.getProductAttributes().add(brandProductAttribute);
    productCategories.get(0).setId(ID);
    productCategories.get(1).setId(ID_1);
    when(productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString())).thenReturn(productCategories);
    when(categoryService.getCategoryByStoreIdAndIdCached(anyString(), anyString())).thenReturn(
        category);
    when(categoryShippingService.generateShippingWeight(anyString(), anyString(), Mockito.anyDouble(),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble())).thenReturn(SHIPPING_WEIGHT);
    MasterProductDataUpdateDTO masterProductDataUpdateDTO =
        service.updateProductAndGetDimensionChanged(STORE_ID, simpleMasterProductUpdateDTO);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        DEFAULT_STORE_ID, product, false);
    verify(brandService).findByBrandName(BRAND, false);
    verify(predefinedAllowedAttributeValueService).
        findByStoreIdAndPredefinedAllowedAttributeCode(STORE_ID, BRAND_ATTRIBUTE_CODE);
    verify(repository).findById(PRODUCT_ID);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString());
    verify(categoryService).getCategoryByStoreIdAndIdCached(anyString(), anyString());
    verify(categoryShippingService).generateShippingWeight(anyString(), anyString(),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble());
    assertEquals(PRODUCT_NAME1, productArgumentCaptor.getAllValues().get(0).getName());
    assertEquals(BRAND, productArgumentCaptor.getAllValues().get(0).getBrand());
    assertEquals(LENGTH, productArgumentCaptor.getAllValues().get(0).getLength());
    assertEquals(WIDTH, productArgumentCaptor.getAllValues().get(0).getWidth());
    assertEquals(HEIGHT, productArgumentCaptor.getAllValues().get(0).getHeight());
    assertEquals(WEIGHT, productArgumentCaptor.getAllValues().get(0).getWeight());
    assertEquals(SHIPPING_WEIGHT, productArgumentCaptor.getAllValues().get(0).getShippingWeight());
    assertEquals(PRODUCT_NAME1 + ITEM_NAME,
        productArgumentCaptor.getAllValues().get(0).getProductItems().get(0).getGeneratedItemName());
    assertEquals(DANGEROUS_GOODS_LEVEL,
        productArgumentCaptor.getAllValues().get(0).getProductItems().get(0).getDangerousGoodsLevel());
    assertEquals(PRODUCT_CODE, productArgumentCaptor.getAllValues().get(0).getProductCode());
    productItem.setProduct(product);
    assertTrue(masterProductDataUpdateDTO.isBrandUpdated());
  }

  @Test
  public void updateMasterProductDataProductNameChangeLengthNullTest() throws Exception {
    simpleMasterProductUpdateDTO.setPostLive(true);
    product.setName(PRODUCT_NAME);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    product.setProductAttributes(new ArrayList<>());
    product.getProductAttributes().add(brandProductAttribute);
    productCategories.get(0).setId(ID);
    productCategories.get(1).setId(ID_1);
    when(productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString())).thenReturn(productCategories);
    when(categoryService.getCategoryByStoreIdAndIdCached(anyString(), anyString())).thenReturn(
        category);
    simpleMasterProductUpdateDTO.setLength(null);
    service.updateProductAndGetDimensionChanged(STORE_ID, simpleMasterProductUpdateDTO);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        DEFAULT_STORE_ID, product, false);
    verify(brandService).findByBrandName(BRAND, false);
    verify(predefinedAllowedAttributeValueService).
        findByStoreIdAndPredefinedAllowedAttributeCode(STORE_ID, BRAND_ATTRIBUTE_CODE);
    verify(repository).findById(PRODUCT_ID);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString());
    verify(categoryService).getCategoryByStoreIdAndIdCached(anyString(), anyString());
    assertEquals(PRODUCT_NAME1, productArgumentCaptor.getAllValues().get(0).getName());
    assertEquals(BRAND, productArgumentCaptor.getAllValues().get(0).getBrand());
    assertEquals(ORIGINAL_LENGTH, productArgumentCaptor.getAllValues().get(0).getLength());
    assertEquals(ORIGINAL_WIDTH, productArgumentCaptor.getAllValues().get(0).getWidth());
    assertEquals(ORIGINAL_HEIGHT, productArgumentCaptor.getAllValues().get(0).getHeight());
    assertEquals(ORIGINAL_WEIGHT, productArgumentCaptor.getAllValues().get(0).getWeight());
    assertEquals(ORIGINAL_SHIPPING_WEIGHT, productArgumentCaptor.getAllValues().get(0).getShippingWeight());
    assertEquals(PRODUCT_NAME1 + ITEM_NAME,
        productArgumentCaptor.getAllValues().get(0).getProductItems().get(0).getGeneratedItemName());
    assertEquals(DANGEROUS_GOODS_LEVEL,
        productArgumentCaptor.getAllValues().get(0).getProductItems().get(0).getDangerousGoodsLevel());
    assertEquals(PRODUCT_CODE, productArgumentCaptor.getAllValues().get(0).getProductCode());
    productItem.setProduct(product);
  }

  @Test
  public void updateMasterProductDataNotPostLiveTest() throws Exception {
    product.setName(PRODUCT_NAME);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    product.setProductAttributes(new ArrayList<>());
    product.getProductAttributes().add(brandProductAttribute);
    productCategories.get(0).setId(ID);
    productCategories.get(1).setId(ID_1);
    when(productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString())).thenReturn(productCategories);
    when(categoryService.getCategoryByStoreIdAndIdCached(anyString(), anyString())).thenReturn(
        category);
    when(categoryShippingService.generateShippingWeight(anyString(), anyString(), Mockito.anyDouble(),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble())).thenReturn(SHIPPING_WEIGHT);
    service.updateProductAndGetDimensionChanged(STORE_ID, simpleMasterProductUpdateDTO);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        DEFAULT_STORE_ID, product, false);
    verify(brandService).findByBrandName(BRAND, false);
    verify(predefinedAllowedAttributeValueService).
        findByStoreIdAndPredefinedAllowedAttributeCode(STORE_ID, BRAND_ATTRIBUTE_CODE);
    verify(repository).findById(PRODUCT_ID);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString());
    verify(categoryService).getCategoryByStoreIdAndIdCached(anyString(), anyString());
    verify(categoryShippingService).generateShippingWeight(anyString(), anyString(),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble());
    assertEquals(PRODUCT_NAME1, productArgumentCaptor.getAllValues().get(0).getName());
    assertEquals(BRAND, productArgumentCaptor.getAllValues().get(0).getBrand());
    assertEquals(LENGTH, productArgumentCaptor.getAllValues().get(0).getLength());
    assertEquals(WIDTH, productArgumentCaptor.getAllValues().get(0).getWidth());
    assertEquals(HEIGHT, productArgumentCaptor.getAllValues().get(0).getHeight());
    assertEquals(WEIGHT, productArgumentCaptor.getAllValues().get(0).getWeight());
    assertEquals(SHIPPING_WEIGHT, productArgumentCaptor.getAllValues().get(0).getShippingWeight());
    assertEquals(PRODUCT_NAME1 + ITEM_NAME,
        productArgumentCaptor.getAllValues().get(0).getProductItems().get(0).getGeneratedItemName());
    assertEquals(DANGEROUS_GOODS_LEVEL,
        productArgumentCaptor.getAllValues().get(0).getProductItems().get(0).getDangerousGoodsLevel());
    assertEquals(PRODUCT_CODE, productArgumentCaptor.getAllValues().get(0).getProductCode());
    productItem.setProduct(product);
  }

  @Test
  public void updateMasterProductDataTest() throws Exception {
    simpleMasterProductUpdateDTO.setPostLive(true);
    product.setReviewPending(true);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    product.setBrand(BRAND_1_NAME);
    product.setProductAttributes(new ArrayList<>());
    product.getProductAttributes().add(brandProductAttribute);
    productCategories.get(0).setId(ID);
    productCategories.get(1).setId(ID_1);
    when(productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString())).thenReturn(productCategories);
    when(categoryService.getCategoryByStoreIdAndIdCached(anyString(), anyString())).thenReturn(
        category);
    when(categoryShippingService.generateShippingWeight(anyString(), anyString(), Mockito.anyDouble(),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble())).thenReturn(SHIPPING_WEIGHT);
    service.updateProductAndGetDimensionChanged(STORE_ID, simpleMasterProductUpdateDTO);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        DEFAULT_STORE_ID, product, false);
    verify(repository).findById(PRODUCT_ID);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString());
    verify(categoryService).getCategoryByStoreIdAndIdCached(anyString(), anyString());
    verify(categoryShippingService).generateShippingWeight(anyString(), anyString(),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble());
    assertEquals(PRODUCT_NAME1, productArgumentCaptor.getAllValues().get(0).getName());
    assertEquals(BRAND_1_NAME, productArgumentCaptor.getAllValues().get(0).getBrand());
    assertEquals(LENGTH, productArgumentCaptor.getAllValues().get(0).getLength());
    assertEquals(WIDTH, productArgumentCaptor.getAllValues().get(0).getWidth());
    assertEquals(HEIGHT, productArgumentCaptor.getAllValues().get(0).getHeight());
    assertEquals(WEIGHT, productArgumentCaptor.getAllValues().get(0).getWeight());
    assertEquals(SHIPPING_WEIGHT, productArgumentCaptor.getAllValues().get(0).getShippingWeight());
    assertEquals(PRODUCT_NAME + ITEM_NAME,
        productArgumentCaptor.getAllValues().get(0).getProductItems().get(0).getGeneratedItemName());
    assertEquals(SHIPPING_WEIGHT, productArgumentCaptor.getAllValues().get(0).getShippingWeight());
    assertEquals(DANGEROUS_GOODS_LEVEL,
        productArgumentCaptor.getAllValues().get(0).getProductItems().get(0).getDangerousGoodsLevel());
    assertEquals(ORIGINAL_BRAND, productArgumentCaptor.getAllValues().get(0).getProductItems().get(0)
        .getProductItemAttributeValues().get(4).getValue());
  }


  @Test
  public void updateMasterProductDataReviewPendingFalseTest() throws Exception {
    simpleMasterProductUpdateDTO.setPostLive(true);
    product.setReviewPending(false);
    product.setProductItems(
        productItems.stream().filter(productItem -> !productItem.isMarkForDelete()).collect(toList()));
    product.setProductAttributes(new ArrayList<>());
    product.getProductAttributes().add(brandProductAttribute);
    productCategories.get(0).setId(ID);
    productCategories.get(1).setId(ID_1);
    when(productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString())).thenReturn(productCategories);
    when(categoryService.getCategoryByStoreIdAndIdCached(anyString(), anyString())).thenReturn(
        category);
    when(categoryShippingService.generateShippingWeight(anyString(), anyString(), Mockito.anyDouble(),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble())).thenReturn(SHIPPING_WEIGHT);
    service.updateProductAndGetDimensionChanged(STORE_ID, simpleMasterProductUpdateDTO);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(DEFAULT_STORE_ID,
        product, false);
    verify(brandService).findByBrandName(BRAND, false);
    verify(predefinedAllowedAttributeValueService).findByStoreIdAndPredefinedAllowedAttributeCode(STORE_ID,
        BRAND_ATTRIBUTE_CODE);
    verify(repository).findById(PRODUCT_ID);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString());
    verify(categoryService).getCategoryByStoreIdAndIdCached(anyString(), anyString());
    verify(categoryShippingService).generateShippingWeight(anyString(), anyString(),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble());
    assertEquals(BRAND, productArgumentCaptor.getAllValues().get(0).getBrand());
    assertEquals(LENGTH, productArgumentCaptor.getAllValues().get(0).getLength());
    assertEquals(WIDTH, productArgumentCaptor.getAllValues().get(0).getWidth());
    assertEquals(HEIGHT, productArgumentCaptor.getAllValues().get(0).getHeight());
    assertEquals(WEIGHT, productArgumentCaptor.getAllValues().get(0).getWeight());
    assertEquals(SHIPPING_WEIGHT, productArgumentCaptor.getAllValues().get(0).getShippingWeight());
    assertEquals(SHIPPING_WEIGHT, productArgumentCaptor.getAllValues().get(0).getShippingWeight());
    assertEquals(DANGEROUS_GOODS_LEVEL,
        productArgumentCaptor.getAllValues().get(0).getProductItems().get(0).getDangerousGoodsLevel());
  }

  @Test
  public void updateMasterProductDataDGLevelNullSetDefaultValueAndReviewPendingTrueTest() throws Exception {
    simpleMasterProductUpdateDTO.setPostLive(true);
    simpleMasterProductUpdateDTO.setDangerousGoodsLevel(DEFAULT_DANGEROUS_GOODS_LEVEL);
    product.setReviewPending(true);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    product.setBrand(BRAND_1_NAME);
    product.setProductAttributes(new ArrayList<>());
    product.getProductAttributes().add(brandProductAttribute);
    product.getProductItems().get(0).setDangerousGoodsLevel(null);
    productCategories.get(0).setId(ID);
    productCategories.get(1).setId(ID_1);
    when(productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString())).thenReturn(productCategories);
    when(categoryService.getCategoryByStoreIdAndIdCached(anyString(), anyString())).thenReturn(
        category);
    when(categoryShippingService.generateShippingWeight(anyString(), anyString(), Mockito.anyDouble(),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble())).thenReturn(SHIPPING_WEIGHT);
    MasterProductDataUpdateDTO masterProductDataUpdateDTO =
        service.updateProductAndGetDimensionChanged(STORE_ID, simpleMasterProductUpdateDTO);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        DEFAULT_STORE_ID, product, false);
    verify(repository).findById(PRODUCT_ID);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString());
    verify(categoryService).getCategoryByStoreIdAndIdCached(anyString(), anyString());
    verify(categoryShippingService).generateShippingWeight(anyString(), anyString(),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble());
    assertEquals(PRODUCT_NAME1, productArgumentCaptor.getAllValues().get(0).getName());
    assertEquals(BRAND_1_NAME, productArgumentCaptor.getAllValues().get(0).getBrand());
    assertEquals(LENGTH, productArgumentCaptor.getAllValues().get(0).getLength());
    assertEquals(WIDTH, productArgumentCaptor.getAllValues().get(0).getWidth());
    assertEquals(HEIGHT, productArgumentCaptor.getAllValues().get(0).getHeight());
    assertEquals(WEIGHT, productArgumentCaptor.getAllValues().get(0).getWeight());
    assertEquals(SHIPPING_WEIGHT, productArgumentCaptor.getAllValues().get(0).getShippingWeight());
    assertEquals(PRODUCT_NAME + ITEM_NAME,
        productArgumentCaptor.getAllValues().get(0).getProductItems().get(0).getGeneratedItemName());
    assertEquals(SHIPPING_WEIGHT, productArgumentCaptor.getAllValues().get(0).getShippingWeight());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL,
        productArgumentCaptor.getAllValues().get(0).getProductItems().get(0).getDangerousGoodsLevel());
    assertEquals(ORIGINAL_BRAND, productArgumentCaptor.getAllValues().get(0).getProductItems().get(0)
        .getProductItemAttributeValues().get(4).getValue());
    Assertions.assertFalse(masterProductDataUpdateDTO.isBrandUpdated());
  }

  @Test
  public void updateMasterProductDataDGLevelNullSetDefaultValueAndBrandUnchangedTest() throws Exception {
    simpleMasterProductUpdateDTO.setPostLive(true);
    simpleMasterProductUpdateDTO.setDangerousGoodsLevel(DEFAULT_DANGEROUS_GOODS_LEVEL);
    product.setReviewPending(false);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    product.setBrand(BRAND);
    product.setProductAttributes(new ArrayList<>());
    product.getProductAttributes().add(brandProductAttribute);
    product.getProductItems().get(0).setDangerousGoodsLevel(null);
    productCategories.get(0).setId(ID);
    productCategories.get(1).setId(ID_1);
    when(productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString())).thenReturn(productCategories);
    when(categoryService.getCategoryByStoreIdAndIdCached(anyString(), anyString())).thenReturn(
        category);
    when(categoryShippingService.generateShippingWeight(anyString(), anyString(), Mockito.anyDouble(),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble())).thenReturn(SHIPPING_WEIGHT);
    MasterProductDataUpdateDTO masterProductDataUpdateDTO =
        service.updateProductAndGetDimensionChanged(STORE_ID, simpleMasterProductUpdateDTO);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        DEFAULT_STORE_ID, product, false);
    verify(repository).findById(PRODUCT_ID);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString());
    verify(categoryService).getCategoryByStoreIdAndIdCached(anyString(), anyString());
    verify(categoryShippingService).generateShippingWeight(anyString(), anyString(),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble());
    assertEquals(PRODUCT_NAME1, productArgumentCaptor.getAllValues().get(0).getName());
    assertEquals(BRAND, productArgumentCaptor.getAllValues().get(0).getBrand());
    assertEquals(LENGTH, productArgumentCaptor.getAllValues().get(0).getLength());
    assertEquals(WIDTH, productArgumentCaptor.getAllValues().get(0).getWidth());
    assertEquals(HEIGHT, productArgumentCaptor.getAllValues().get(0).getHeight());
    assertEquals(WEIGHT, productArgumentCaptor.getAllValues().get(0).getWeight());
    assertEquals(SHIPPING_WEIGHT, productArgumentCaptor.getAllValues().get(0).getShippingWeight());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL,
        productArgumentCaptor.getAllValues().get(0).getProductItems().get(0).getDangerousGoodsLevel());
    assertEquals(BRAND, productArgumentCaptor.getAllValues().get(0).getProductItems().get(0)
        .getProductItemAttributeValues().get(4).getValue());
    Assertions.assertFalse(masterProductDataUpdateDTO.isBrandUpdated());
  }


  @Test
  public void updateMasterProductDataDGLevelChangeValueTest() throws Exception {
    simpleMasterProductUpdateDTO.setPostLive(true);
    simpleMasterProductUpdateDTO.setDangerousGoodsLevel(DEFAULT_DANGEROUS_GOODS_LEVEL);
    product.setReviewPending(true);
    product.setBrand(BRAND_1_NAME);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    product.setProductAttributes(new ArrayList<>());
    product.getProductAttributes().add(brandProductAttribute);
    product.getProductItems().get(0).setDangerousGoodsLevel(1);
    productCategories.get(0).setId(ID);
    productCategories.get(1).setId(ID_1);
    when(productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString())).thenReturn(productCategories);
    when(categoryService.getCategoryByStoreIdAndIdCached(anyString(), anyString())).thenReturn(
        category);
    when(categoryShippingService.generateShippingWeight(anyString(), anyString(), Mockito.anyDouble(),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble())).thenReturn(SHIPPING_WEIGHT);
    service.updateProductAndGetDimensionChanged(STORE_ID, simpleMasterProductUpdateDTO);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        DEFAULT_STORE_ID, product, false);
    verify(repository).findById(PRODUCT_ID);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString());
    verify(categoryService).getCategoryByStoreIdAndIdCached(anyString(), anyString());
    verify(categoryShippingService).generateShippingWeight(anyString(), anyString(),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble());
    assertEquals(PRODUCT_NAME1, productArgumentCaptor.getAllValues().get(0).getName());
    assertEquals(BRAND_1_NAME, productArgumentCaptor.getAllValues().get(0).getBrand());
    assertEquals(LENGTH, productArgumentCaptor.getAllValues().get(0).getLength());
    assertEquals(WIDTH, productArgumentCaptor.getAllValues().get(0).getWidth());
    assertEquals(HEIGHT, productArgumentCaptor.getAllValues().get(0).getHeight());
    assertEquals(WEIGHT, productArgumentCaptor.getAllValues().get(0).getWeight());
    assertEquals(SHIPPING_WEIGHT, productArgumentCaptor.getAllValues().get(0).getShippingWeight());
    assertEquals(PRODUCT_NAME + ITEM_NAME,
        productArgumentCaptor.getAllValues().get(0).getProductItems().get(0).getGeneratedItemName());
    assertEquals(SHIPPING_WEIGHT, productArgumentCaptor.getAllValues().get(0).getShippingWeight());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL,
        productArgumentCaptor.getAllValues().get(0).getProductItems().get(0).getDangerousGoodsLevel());
    assertEquals(ORIGINAL_BRAND, productArgumentCaptor.getAllValues().get(0).getProductItems().get(0)
        .getProductItemAttributeValues().get(4).getValue());
  }

  @Test
  public void updateMasterProductDataDGLevelSameValueTest() throws Exception {
    simpleMasterProductUpdateDTO.setPostLive(true);
    simpleMasterProductUpdateDTO.setDangerousGoodsLevel(DEFAULT_DANGEROUS_GOODS_LEVEL);
    product.setReviewPending(false);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    product.setProductAttributes(new ArrayList<>());
    product.getProductAttributes().add(brandProductAttribute);
    product.getProductItems().get(0).setDangerousGoodsLevel(0);
    productCategories.get(0).setId(ID);
    productCategories.get(1).setId(ID_1);
    when(productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString())).thenReturn(productCategories);
    when(categoryService.getCategoryByStoreIdAndIdCached(anyString(), anyString())).thenReturn(
        category);
    when(categoryShippingService.generateShippingWeight(anyString(), anyString(), Mockito.anyDouble(),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble())).thenReturn(SHIPPING_WEIGHT);
    service.updateProductAndGetDimensionChanged(STORE_ID, simpleMasterProductUpdateDTO);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        DEFAULT_STORE_ID, product, false);
    verify(brandService).findByBrandName(BRAND, false);
    verify(predefinedAllowedAttributeValueService).
        findByStoreIdAndPredefinedAllowedAttributeCode(STORE_ID, BRAND_ATTRIBUTE_CODE);
    verify(repository).findById(PRODUCT_ID);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString());
    verify(categoryService).getCategoryByStoreIdAndIdCached(anyString(), anyString());
    verify(categoryShippingService).generateShippingWeight(anyString(), anyString(),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble());
    assertEquals(PRODUCT_NAME1, productArgumentCaptor.getAllValues().get(0).getName());
    assertEquals(BRAND, productArgumentCaptor.getAllValues().get(0).getBrand());
    assertEquals(LENGTH, productArgumentCaptor.getAllValues().get(0).getLength());
    assertEquals(WIDTH, productArgumentCaptor.getAllValues().get(0).getWidth());
    assertEquals(HEIGHT, productArgumentCaptor.getAllValues().get(0).getHeight());
    assertEquals(WEIGHT, productArgumentCaptor.getAllValues().get(0).getWeight());
    assertEquals(SHIPPING_WEIGHT, productArgumentCaptor.getAllValues().get(0).getShippingWeight());
    assertEquals(SHIPPING_WEIGHT, productArgumentCaptor.getAllValues().get(0).getShippingWeight());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL,
        productArgumentCaptor.getAllValues().get(0).getProductItems().get(0).getDangerousGoodsLevel());
  }

  @Test
  public void updateMasterProductDataProductNotFoundTest() throws Exception {
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> service.updateProductAndGetDimensionChanged(STORE_ID, simpleMasterProductUpdateDTO));
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void updateMasterProductDataCategoryNotFoundTest() throws Exception {
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    product.setProductAttributes(new ArrayList<>());
    product.getProductAttributes().add(brandProductAttribute);
    productCategories.get(0).setId(ID);
    productCategories.get(1).setId(ID_1);
    when(productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString())).thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(anyString(), anyString())).thenReturn(
        category);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> service.updateProductAndGetDimensionChanged(STORE_ID, simpleMasterProductUpdateDTO));
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
      verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
          anyString());
    }
  }

  @Test
  public void updateMasterProductDataBrandNotFoundTest() throws Exception {
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    product.setProductAttributes(new ArrayList<>());
    product.getProductAttributes().add(brandProductAttribute);
    when(brandService.findByBrandName(BRAND, false)).thenReturn(null);
    productCategories.get(0).setId(ID);
    productCategories.get(1).setId(ID_1);
    when(productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString())).thenReturn(productCategories);
    when(categoryService.getCategoryByStoreIdAndIdCached(anyString(), anyString())).thenReturn(
        category);
    when(categoryShippingService.generateShippingWeight(anyString(), anyString(), Mockito.anyDouble(),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble())).thenReturn(SHIPPING_WEIGHT);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> service.updateProductAndGetDimensionChanged(STORE_ID, simpleMasterProductUpdateDTO));
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
      verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
      verify(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
          DEFAULT_STORE_ID, product, false);
      verify(brandService).findByBrandName(BRAND, false);
      verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
          anyString());
      verify(categoryService).getCategoryByStoreIdAndIdCached(anyString(), anyString());
      verify(categoryShippingService).generateShippingWeight(anyString(), anyString(),
          Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble());
    }
  }

  @Test
  public void updateProductCategoryByStoreIdAndProductCodeTest() throws Exception {
    Category categoryNew = new Category();
    categoryNew.setActivated(true);
    categoryNew.setCategoryCode(CATEGORY_CODE_NEW);
    categoryNew.setId(CATEGORY_ID_NEW);
    categoryNew.setName(CATEGORY_NAME_NEW);
    when(categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(
        STORE_ID, CATEGORY_CODE_NEW)).thenReturn(categoryNew);
    Mockito.when(categoryService.validateIsCategoryCn(STORE_ID, CATEGORY_CODE_NEW)).thenReturn(true);
    Pair<Product, CategorySummaryResponse> categorySummaryResponse =
        service.updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE_NEW,true);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_2);
    verify(categoryService).getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE_NEW);
    verify(repository).findById(PRODUCT_ID);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    verify(categoryService).validateIsCategoryCn(STORE_ID, CATEGORY_CODE_NEW);
    verify(productAttributeExtractionService)
        .addProductsToProductAttributeExtraction(anyString(), any(Category.class));
    assertEquals(CATEGORY_ID_1, productArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getId());
    assertTrue(productArgumentCaptor.getValue().getProductCategories().get(0).isMarkForDelete());
    assertEquals(CATEGORY_ID_2, productArgumentCaptor.getValue().getProductCategories().get(1).getCategory().getId());
    assertTrue(productArgumentCaptor.getValue().getProductCategories().get(1).isMarkForDelete());
    assertEquals(CATEGORY_ID_NEW, productArgumentCaptor.getValue().getProductCategories().get(2).getCategory().getId());
    Assertions.assertFalse(productArgumentCaptor.getValue().getProductCategories().get(2).isMarkForDelete());
    assertEquals(3, productArgumentCaptor.getValue().getProductCategories().size());
    assertEquals(CATEGORY_CODE_NEW, categorySummaryResponse.getRight().getCategoryCode());
    assertEquals(CATEGORY_NAME_NEW, categorySummaryResponse.getRight().getCategoryName());
    assertEquals(CATEGORY_ID_NEW, categorySummaryResponse.getRight().getNewCategoryId());
    assertEquals(CATEGORY_ID_1, categorySummaryResponse.getRight().getOldCategoryId());
  }

  @Test
  public void updateProductCategoryByStoreIdAndProductCodeProductNotFoundTest() throws Exception {
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(null);
    Pair<Product, CategorySummaryResponse> categorySummaryResponse = null;
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->
          service.updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE_NEW,true));
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      Mockito.verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
      Assertions.assertNull(categorySummaryResponse);
    }
  }

  @Test
  public void updateProductCategoryByStoreIdAndProductCodeCategoryMasterCategoryTest() throws Exception {
    Category categoryNew = new Category();
    Catalog catalog = new Catalog();
    catalog.setCatalogType(CatalogType.SALES_CATALOG);
    categoryNew.setActivated(true);
    categoryNew.setCategoryCode(CATEGORY_CODE_NEW);
    categoryNew.setId(CATEGORY_ID_NEW);
    categoryNew.setName(CATEGORY_NAME_NEW);
    categoryNew.setCatalog(catalog);
    when(categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID,
        CATEGORY_CODE_NEW)).thenReturn(categoryNew);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->  service.updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE_NEW,true));
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
      Mockito.verify(categoryService)
          .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE_NEW);
    }
  }

  @Test
  public void updateProductCategoryByStoreIdAndProductCodeCategoryNotActiveTest() throws Exception {
    Category categoryNew = new Category();
    categoryNew.setActivated(false);
    categoryNew.setCategoryCode(CATEGORY_CODE_NEW);
    categoryNew.setId(CATEGORY_ID_NEW);
    categoryNew.setName(CATEGORY_NAME_NEW);
    when(categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(
        STORE_ID, CATEGORY_CODE_NEW)).thenReturn(categoryNew);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> service.updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE_NEW,true));
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
      Mockito.verify(categoryService).getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(
          STORE_ID, CATEGORY_CODE_NEW);
    }
  }

  @Test
  public void updateProductCategoryByStoreIdAndProductCodeAddingExistingActiveCategoryTest() throws Exception {
    Pair<Product, CategorySummaryResponse> categorySummaryResponse = null;
    Mockito.when(categoryService.validateIsCategoryCn(STORE_ID, CATEGORY_CODE_1)).thenReturn(true);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->
          service.updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE_1,true));
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      Mockito.verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
      Mockito.verify(categoryService).getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(
          STORE_ID, CATEGORY_CODE_1);
      verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
      verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
      verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_2);
      verify(categoryService).validateIsCategoryCn(STORE_ID, CATEGORY_CODE_1);
      Assertions.assertNull(categorySummaryResponse);
    }
  }

  @Test
  public void updateProductCategoryByStoreIdAndProductCodeAddingNoActiveCategory() throws Exception {
    Pair<Product, CategorySummaryResponse> categorySummaryResponse = null;
    Mockito.when(categoryService.validateIsCategoryCn(STORE_ID, CATEGORY_CODE_1)).thenReturn(true);
    for(ProductCategory productCategory : productCategories) {
      productCategory.setMarkForDelete(true);
    }
    when(productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID))
        .thenReturn(productCategories);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->
          service.updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE_1,true));
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      Mockito.verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
      Mockito.verify(categoryService).getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(
          STORE_ID, CATEGORY_CODE_1);
      verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
      verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
      verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_2);
      verify(categoryService).validateIsCategoryCn(STORE_ID, CATEGORY_CODE_1);
      Assertions.assertNull(categorySummaryResponse);
    }
  }

  @Test
  public void updateProductCategoryByStoreIdException() throws Exception {
    Pair<Product, CategorySummaryResponse> categorySummaryResponse = null;
    Mockito.when(categoryService.validateIsCategoryCn(STORE_ID, CATEGORY_CODE_1)).thenReturn(true);
    Mockito.doThrow(ApplicationRuntimeException.class).when(productCategoryService)
        .getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->
          service.updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE_1,true));
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      Mockito.verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
      Mockito.verify(categoryService)
          .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE_1);
      verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
      verify(categoryService).validateIsCategoryCn(STORE_ID, CATEGORY_CODE_1);
      Assertions.assertNull(categorySummaryResponse);
    }
  }

  @Test
  public void updateProductCategoryByStoreIdCategoryException() throws Exception {
    Pair<Product, CategorySummaryResponse> categorySummaryResponse = null;
    Mockito.doThrow(ApplicationRuntimeException.class).when(categoryService)
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE_1);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->
          service.updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE_1,true));
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      Mockito.verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
      Mockito.verify(categoryService)
          .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE_1);
      Assertions.assertNull(categorySummaryResponse);
    }
  }

  @Test
  public void updateProductCategoryByStoreIdAndProductCodeNotCn() throws Exception {
    Pair<Product, CategorySummaryResponse> categorySummaryResponse = null;
    Mockito.when(categoryService.validateIsCategoryCn(STORE_ID, CATEGORY_CODE_1)).thenReturn(false);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->
          service.updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE_1,true));
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      Mockito.verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
      Mockito.verify(categoryService).getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(
          STORE_ID, CATEGORY_CODE_1);
      verify(categoryService).validateIsCategoryCn(STORE_ID, CATEGORY_CODE_1);
      Assertions.assertNull(categorySummaryResponse);
    }
  }

  @Test
  public void updateProductCategoryByStoreIdAndProductCodeAddingExistingInActiveCategoryTest() throws Exception {
    when(categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(
        STORE_ID, CATEGORY_CODE_NEW)).thenReturn(category2);
    Mockito.when(categoryService.validateIsCategoryCn(STORE_ID, CATEGORY_CODE_2)).thenReturn(true);
    Pair<Product, CategorySummaryResponse> categorySummaryResponse =
        service.updateProductCategoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE_2,true);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_2);
    verify(categoryService).getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE_2);
    verify(repository).findById(PRODUCT_ID);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    verify(categoryService).validateIsCategoryCn(STORE_ID, CATEGORY_CODE_2);
    verify(productAttributeExtractionService)
        .addProductsToProductAttributeExtraction(anyString(), any(Category.class));
    assertEquals(CATEGORY_ID_1, productArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getId());
    assertTrue(productArgumentCaptor.getValue().getProductCategories().get(0).isMarkForDelete());
    assertEquals(CATEGORY_ID_2, productArgumentCaptor.getValue().getProductCategories().get(1).getCategory().getId());
    Assertions.assertFalse(productArgumentCaptor.getValue().getProductCategories().get(1).isMarkForDelete());
    assertEquals(2, productArgumentCaptor.getValue().getProductCategories().size());
    assertEquals(CATEGORY_CODE_2, categorySummaryResponse.getRight().getCategoryCode());
  }

  @Test
  public void updateProductDimensionsTest() throws Exception {
    ProductItem productItem = new ProductItem();
    productItem.setUpcCode(UPC_CODE);
    productItem.setProduct(product);
    product.setProductItems(Collections.singletonList(productItem));
    when(repository.findByStoreIdAndId(DEFAULT_STORE_ID, product.getId())).thenReturn(product);
    when(repository.findById(PRODUCT_ID)).thenReturn(Optional.of(product));
    service.updateProductDimensions(warehouseMasterSKUEvent, PRODUCT_ID);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductIdCached(DEFAULT_STORE_ID, product.getId());
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(productItemServiceWrapper).setProductItemsCached(STORE_ID, product, false);
    verify(categoryShippingService)
        .generateShippingWeight(DEFAULT_STORE_ID, CATEGORY_CODE_1, LENGTH, HEIGHT, WEIGHT, WIDTH);
    verify(repository).findById(PRODUCT_ID);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    assertEquals(UPC_CODE, product.getProductItems().get(0).getUpcCode());
  }

  @Test
  public void updateProductAndItemImagesByProductCode() throws Exception {
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setCommonImage(true);
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(image, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    ProductAndItemImageRequest productAndItemImageRequest = new ProductAndItemImageRequest();
    productAndItemImageRequest.setProductCode(PRODUCT_CODE);
    productAndItemImageRequest.setProductImages(Collections.singletonList(image));
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    productItemImageRequest.setSkuCode(ITEM_SKU_CODE_1);
    productItemImageRequest.setItemImages(Collections.singletonList(image));
    productAndItemImageRequest.setProductItemImages(Collections.singletonList(productItemImageRequest));
    service.updateProductAndItemImagesByProductCode(productAndItemImageRequest, STORE_ID,true);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(STORE_ID, product, false);
    verify(repository).findById(PRODUCT_ID);
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(STORE_ID,
      PRODUCT_ID);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    assertTrue(product.getProductImages().stream().anyMatch(img -> !img.isCommonImage()));
    assertTrue(product.getProductItems().stream().flatMap(item -> item.getProductItemImages().stream())
        .anyMatch(img -> !img.isCommonImage()));
  }

  @Test
  public void updateProductAndItemImagesByProductCodeSetDgLevelFalse() throws Exception {
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(image, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    ProductAndItemImageRequest productAndItemImageRequest = new ProductAndItemImageRequest();
    productAndItemImageRequest.setProductCode(PRODUCT_CODE);
    productAndItemImageRequest.setProductImages(Collections.singletonList(image));
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    productItemImageRequest.setSkuCode(ITEM_SKU_CODE_1);
    productItemImageRequest.setItemImages(Collections.singletonList(image));
    productAndItemImageRequest.setProductItemImages(Collections.singletonList(productItemImageRequest));
    service.updateProductAndItemImagesByProductCode(productAndItemImageRequest, STORE_ID,false);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(STORE_ID, product, false);
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(STORE_ID,
      PRODUCT_ID);
    verify(repository).findById(PRODUCT_ID);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
  }

  @Test
  public void getAllowedAttributeValueOrDescriptiveValueTest() throws Exception {
    Attribute attribute = new Attribute();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    ProductAttribute newProductAttribute = new ProductAttribute();
    newProductAttribute.setProductAttributeValues(new ArrayList<ProductAttributeValue>());
    newProductAttribute.getProductAttributeValues().add(new ProductAttributeValue());
    newProductAttribute.getProductAttributeValues().get(0).setProductAttribute(newProductAttribute);
    newProductAttribute.getProductAttributeValues().get(0).setId(null);
    newProductAttribute.getProductAttributeValues().get(0)
        .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
    newProductAttribute.getProductAttributeValues().get(0).setAllowedAttributeValue(new AllowedAttributeValue());
    newProductAttribute.setAttribute(attribute);
    newProductAttribute.getProductAttributeValues().get(0).setDescriptiveAttributeValue("DESCRIPTIVE_VALUE");
    Method method = service.getClass().getDeclaredMethod("getAllowedAttributeValueOrDescriptiveValue", ProductAttributeValue.class);
    method.setAccessible(true);
    String result = (String)method.invoke(service, newProductAttribute.getProductAttributeValues().get(0));
    assertEquals("DESCRIPTIVE_VALUE", result);
  }

  @Test
  public void getAllowedAttributeIdTest() throws Exception {
    Attribute attribute = new Attribute();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    ProductAttribute newProductAttribute = new ProductAttribute();
    newProductAttribute.setProductAttributeValues(new ArrayList<ProductAttributeValue>());
    newProductAttribute.getProductAttributeValues().add(new ProductAttributeValue());
    newProductAttribute.getProductAttributeValues().get(0).setProductAttribute(newProductAttribute);
    newProductAttribute.getProductAttributeValues().get(0).setId(null);
    newProductAttribute.getProductAttributeValues().get(0)
        .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
    newProductAttribute.getProductAttributeValues().get(0).setAllowedAttributeValue(new AllowedAttributeValue());
    newProductAttribute.setAttribute(attribute);
    newProductAttribute.getProductAttributeValues().get(0).setDescriptiveAttributeValue("DESCRIPTIVE_VALUE");
    Method method = service.getClass().getDeclaredMethod("getAllowedAttributeId", ProductAttributeValue.class);
    method.setAccessible(true);
    String result = (String)method.invoke(service, newProductAttribute.getProductAttributeValues().get(0));
    assertEquals("DESCRIPTIVE_VALUE", result);
  }

  @Test
  public void checkAllowAttributeIdTest() throws Exception {
    ReflectionTestUtils.setField(service, "validateMissingAllowedAttribute", true);
    Attribute attribute = new Attribute();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    ProductAttribute newProductAttribute = new ProductAttribute();
    AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
    allowedAttributeValue.setId("allowed_attribute_id");
    newProductAttribute.setProductAttributeValues(new ArrayList<ProductAttributeValue>());
    newProductAttribute.getProductAttributeValues().add(new ProductAttributeValue());
    newProductAttribute.getProductAttributeValues().get(0).setProductAttribute(newProductAttribute);
    newProductAttribute.getProductAttributeValues().get(0).setId(null);
    newProductAttribute.getProductAttributeValues().get(0)
        .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
    newProductAttribute.getProductAttributeValues().get(0).setAllowedAttributeValue(allowedAttributeValue);
    newProductAttribute.setAttribute(attribute);
    newProductAttribute.getProductAttributeValues().get(0).setDescriptiveAttributeValue("DESCRIPTIVE_VALUE");
    when(allowedAttributeValueService.findByStoreIdAndId(STORE_ID, "allowed_attribute_id")).thenReturn(allowedAttributeValue);
    Method method = service.getClass().getDeclaredMethod("checkAllowAttributeId", String.class,ProductAttributeValue.class);
    method.setAccessible(true);
    AllowedAttributeValue result1 = (AllowedAttributeValue) method.invoke(service, STORE_ID, newProductAttribute.getProductAttributeValues().get(0));
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    AllowedAttributeValue result2 = (AllowedAttributeValue) method.invoke(service, STORE_ID, newProductAttribute.getProductAttributeValues().get(0));
    Assertions.assertNull(result1);
    assertEquals(allowedAttributeValue, newProductAttribute.getProductAttributeValues().get(0).getAllowedAttributeValue());
    verify(allowedAttributeValueService).findByStoreIdAndId(STORE_ID, "allowed_attribute_id");
  }

  @Test
  public void checkAllowAttributeIdNullValueExceptionTestCase() throws Exception {
    ReflectionTestUtils.setField(service, "validateMissingAllowedAttribute", true);
    when(allowedAttributeValueService.findByStoreIdAndId(STORE_ID, "allowed_attribute_id")).thenReturn(null);
    Attribute attribute = new Attribute();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    ProductAttribute newProductAttribute = new ProductAttribute();
    AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
    allowedAttributeValue.setId("allowed_attribute_id");
    newProductAttribute.setProductAttributeValues(new ArrayList<ProductAttributeValue>());
    newProductAttribute.getProductAttributeValues().add(new ProductAttributeValue());
    newProductAttribute.getProductAttributeValues().get(0).setProductAttribute(newProductAttribute);
    newProductAttribute.getProductAttributeValues().get(0).setId(null);
    newProductAttribute.getProductAttributeValues().get(0)
        .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
    newProductAttribute.getProductAttributeValues().get(0).setAllowedAttributeValue(null);
    newProductAttribute.setAttribute(attribute);
    newProductAttribute.getProductAttributeValues().get(0).setDescriptiveAttributeValue("DESCRIPTIVE_VALUE");
    when(allowedAttributeValueService.findByStoreIdAndId(STORE_ID, "allowed_attribute_id")).thenReturn(allowedAttributeValue);
    Method method = service.getClass().getDeclaredMethod("checkAllowAttributeId", String.class,ProductAttributeValue.class);
    method.setAccessible(true);
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    Assertions.assertThrows(InvocationTargetException.class, () -> {
      method.invoke(service, STORE_ID, newProductAttribute.getProductAttributeValues().get(0));
    });
  }


  @Test
  public void getSalesCategoryMappingTest() {
    productCategory.setCategory(category);
    productCategory.setCategoryId(CATEGORY_ID);
    productCategory.setMarkForDelete(true);
    productCategory1.setMarkForDelete(false);
    productCategory1.setCategory(category1);
    productCategory1.setCategoryId(CATEGORY_ID_1);
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(
        STORE_ID, DEFAULT_PRODUCT_CODE)).thenReturn(product);
    when(productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productCategory, productCategory1));
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID)).thenReturn(category);
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1)).thenReturn(category1);
    Mockito.when(this.categoryReferenceService.getSalesCategoryReferenceByMasterCategory(CATEGORY_ID, CATEGORY_ID_1, false))
        .thenReturn(productSalesCategoryMapping);
    ProductSalesCategoryMappingResponse productSalesCategoryMappingResponse =
        this.service.getProductSalesCategoryMapping(STORE_ID, DEFAULT_PRODUCT_CODE, false);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, DEFAULT_PRODUCT_CODE);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(this.categoryReferenceService).getSalesCategoryReferenceByMasterCategory(CATEGORY_ID, CATEGORY_ID_1, false);
    assertEquals(OLD_SALES_CATEGORY, productSalesCategoryMappingResponse.getOldSalesCategoryCodes().get(0));
    assertEquals(NEW_SALES_CATEGORY, productSalesCategoryMappingResponse.getNewSalesCategoryCodes().get(0));
  }

  @Test
  public void getSalesCategoryMappingNullTest() {
    productCategory.setCategory(category);
    productCategory.setCategoryId(CATEGORY_ID);
    productCategory.setMarkForDelete(true);
    productCategory1.setMarkForDelete(false);
    productCategory1.setCategory(category1);
    productCategory1.setCategoryId(CATEGORY_ID_1);
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(
        STORE_ID, DEFAULT_PRODUCT_CODE)).thenReturn(product);
    when(productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productCategory, productCategory1));
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID)).thenReturn(category);
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1)).thenReturn(category1);
    Mockito.when(this.categoryReferenceService.getSalesCategoryReferenceByMasterCategory(CATEGORY_ID, CATEGORY_ID_1, true))
        .thenReturn(null);
    ProductSalesCategoryMappingResponse productSalesCategoryMappingResponse =
        this.service.getProductSalesCategoryMapping(STORE_ID, DEFAULT_PRODUCT_CODE, true);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, DEFAULT_PRODUCT_CODE);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(this.categoryReferenceService).getSalesCategoryReferenceByMasterCategory(CATEGORY_ID, CATEGORY_ID_1, true);
    Assertions.assertNull(productSalesCategoryMappingResponse.getOldSalesCategoryCodes());
    Assertions.assertNull(productSalesCategoryMappingResponse.getNewSalesCategoryCodes());
  }

  @Test
  public void getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCachedTest() {
    service.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void getProductByStoreIdAndProductIdAndMarkForDeleteFalseCachedTest() {
    service.getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_ID);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void getProductByStoreIdAndProductIdAndMarkForDeleteFalseCachedTestProductNotFound() {
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> service.getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE));
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductIdCached(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void getProductByStoreIdAndProductIdCached() {
    service.getProductByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(repository).findByStoreIdAndId(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void getProductByStoreIdAndProductIdProductNoFoundCached() {
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> service.getProductByStoreIdAndProductIdCached(STORE_ID, PRODUCT_CODE));
    } finally {
      verify(repository).findByStoreIdAndId(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void getProductByStoreIdAndProductCodeCached() {
    service.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(repository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void getProductByStoreIdAndProductCodeProductNotFoundCached() {
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> service.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_ID));
    } finally {
      verify(repository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_ID);
    }
  }

  @Test
  public void updateProductContentBrandChangeTest() throws Exception {
    Product request = generateProductRequest();
    request.setVersion(1L);
    request.setBrand(BRAND_1_NAME);
    product.setVersion(1L);
    product.setProductItems(productItems);
    when(categoryReferenceService.getSalesCategoryReferenceByMasterCategory(CATEGORY_ID_1, CATEGORY_ID_2, false))
        .thenReturn(new ProductSalesCategoryMapping(Collections.singletonList(SALES_CATEGORY_1),
            Collections.singletonList(SALES_CATEGORY_2), Collections.singletonList(SALES_CATEGORY_3),
            new ArrayList<>(), new ArrayList<>(),true));
    this.service.updateProductContent(request, true);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(ProductServiceTest.STORE_ID, ProductServiceTest.PRODUCT_CODE);
    verify(this.repository).saveAndFlush(product);
    verifyProductDetailInteractionsForMarkForDeleteTrueCached();
    verify(categoryReferenceService).getSalesCategoryReferenceByMasterCategory(CATEGORY_ID_1, CATEGORY_ID_2, false);
    verifyEvictAllProductDetailCache();
    verify(this.domainEventPublisherService)
        .publishProductChangeCategory(eq(product), productSalesCategoryMappingArgumentCaptor.capture(),
            eq(Boolean.TRUE), eq(Boolean.FALSE), eq(Boolean.FALSE), eq(false), eq(new HashSet<>()));
    verify(productAttributeExtractionService)
        .addProductsToProductAttributeExtraction(anyString(), any(Category.class));
  }

  @Test
  public void findByUniqueSellingCodeLikeTest() {
    when(this.repository
        .findByStoreIdAndUniqueSellingPointStartingWithAndMarkForDeleteFalse(DEFAULT_STORE_ID, UNIQUE_SELLING_POINT,
            DEFAULT_PAGEABLE)).thenReturn(productPage);
    Page<Product> response =
        service.findByUniqueSellingCodeLike(DEFAULT_STORE_ID, UNIQUE_SELLING_POINT, DEFAULT_PAGEABLE);
    verify(this.repository)
        .findByStoreIdAndUniqueSellingPointStartingWithAndMarkForDeleteFalse(DEFAULT_STORE_ID, UNIQUE_SELLING_POINT,
            DEFAULT_PAGEABLE);
    assertEquals(1, response.getContent().size());
  }

  @Test
  public void findByViewableTest() {
    when(this.repository.findByStoreIdAndViewableAndMarkForDeleteFalse(DEFAULT_STORE_ID, true, DEFAULT_PAGEABLE))
        .thenReturn(productPage);
    Page<Product> response = service.findByViewable(DEFAULT_STORE_ID, true, DEFAULT_PAGEABLE);
    verify(this.repository).findByStoreIdAndViewableAndMarkForDeleteFalse(DEFAULT_STORE_ID, true, DEFAULT_PAGEABLE);
    assertEquals(1, response.getContent().size());
  }

  @Test
  public void findByViewableAndActivated_Test() {
    when(this.repository
        .findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseOrderByUpdatedDateDesc(DEFAULT_STORE_ID, true, true,
            DEFAULT_PAGEABLE)).thenReturn(productPage);
    Page<Product> response = service.findByViewableAndActivated(DEFAULT_STORE_ID, true, true, DEFAULT_PAGEABLE);
    verify(this.repository)
        .findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseOrderByUpdatedDateDesc(DEFAULT_STORE_ID, true, true,
            DEFAULT_PAGEABLE);
    assertEquals(1, response.getContent().size());
  }

  @Test
  public void findByBrandLikeTest(){
    when(this.repository.findByStoreIdAndBrandStartingWithAndMarkForDeleteFalse(STORE_ID, BRAND, DEFAULT_PAGEABLE))
        .thenReturn(productPage);
    service.findByBrandLike(STORE_ID, BRAND, DEFAULT_PAGEABLE);
    verify(this.repository).findByStoreIdAndBrandStartingWithAndMarkForDeleteFalse(STORE_ID, BRAND, DEFAULT_PAGEABLE);
  }

  @Test
  void findByStoreIdAndBrandNameTest() {
    when(this.repository.findByStoreIdAndBrandIgnoreCaseAndMarkForDeleteFalse(STORE_ID, BRAND,
        DEFAULT_PAGEABLE)).thenReturn(productPage);
    service.findByStoreIdAndBrandName(STORE_ID, BRAND, DEFAULT_PAGEABLE);
    verify(this.repository).findByStoreIdAndBrandIgnoreCaseAndMarkForDeleteFalse(STORE_ID, BRAND,
        DEFAULT_PAGEABLE);
  }

  @Test
  public void findByCategoryIdTest(){
    when(this.repository.findByStoreIdAndCategoryIdAndMarkForDeleteFalse(STORE_ID, BRAND, DEFAULT_PAGEABLE))
        .thenReturn(productPage);
    service.findByCategoryId(STORE_ID, BRAND, DEFAULT_PAGEABLE);
    verify(this.repository).findByStoreIdAndCategoryIdAndMarkForDeleteFalse(STORE_ID, BRAND, DEFAULT_PAGEABLE);
  }

  @Test
  public void findByMarkForDeleteTest(){
    when(this.repository.findByStoreIdAndMarkForDeleteOrderByUpdatedDateDesc(STORE_ID, true, DEFAULT_PAGEABLE))
        .thenReturn(productPage);
    service.findByMarkForDelete(STORE_ID, true, DEFAULT_PAGEABLE);
    verify(this.repository).findByStoreIdAndMarkForDeleteOrderByUpdatedDateDesc(STORE_ID, true, DEFAULT_PAGEABLE);
  }

  @Test
  public void findByNameTest() {
    when(this.repository
        .findByStoreIdAndNameContainingIgnoreCaseAndMarkForDeleteFalse(STORE_ID, BRAND_1_NAME, DEFAULT_PAGEABLE))
        .thenReturn(productPage);
    service.findByName(STORE_ID, BRAND_1_NAME, DEFAULT_PAGEABLE);
    verify(this.repository)
        .findByStoreIdAndNameContainingIgnoreCaseAndMarkForDeleteFalse(STORE_ID, BRAND_1_NAME, DEFAULT_PAGEABLE);
  }

  @Test
  public void findByNameAndCreatedByTest() {
    when(this.repository
        .findByStoreIdAndNameContainingIgnoreCaseAndCreatedByContainingIgnoreCaseAndMarkForDeleteFalseOrderByUpdatedDateDesc(
            STORE_ID, BRAND_1_NAME, CREATED_BY, DEFAULT_PAGEABLE)).thenReturn(productPage);
    service.findByNameAndCreatedBy(STORE_ID, BRAND_1_NAME, CREATED_BY, DEFAULT_PAGEABLE);
    verify(this.repository)
        .findByStoreIdAndNameContainingIgnoreCaseAndCreatedByContainingIgnoreCaseAndMarkForDeleteFalseOrderByUpdatedDateDesc(
            STORE_ID, BRAND_1_NAME, CREATED_BY, DEFAULT_PAGEABLE);
  }

  @Test
  public void findByNameAndViewableAndActivatedTest() {
    when(this.repository
        .findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseAndNameContainingIgnoreCaseOrderByUpdatedDateDesc(
            STORE_ID, true, true, BRAND_1_NAME, DEFAULT_PAGEABLE)).thenReturn(productPage);
    service.findByNameAndViewableAndActivated(STORE_ID, BRAND_1_NAME, true, true, DEFAULT_PAGEABLE);
    verify(this.repository)
        .findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseAndNameContainingIgnoreCaseOrderByUpdatedDateDesc(
            STORE_ID, true, true, BRAND_1_NAME, DEFAULT_PAGEABLE);
  }

  @Test
  public void findByNameAndViewableAndActivatedAndUpdatedByTest() {
    when(this.repository
        .findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseAndNameContainingIgnoreCaseAndUpdatedByContainingIgnoreCaseOrderByUpdatedDateDesc(
            STORE_ID, true, true, BRAND_1_NAME, UPDATED_BY, DEFAULT_PAGEABLE)).thenReturn(productPage);
    service.findByNameAndViewableAndActivatedAndUpdatedBy(STORE_ID, BRAND_1_NAME, true, true, UPDATED_BY,
        DEFAULT_PAGEABLE);
    verify(this.repository)
        .findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseAndNameContainingIgnoreCaseAndUpdatedByContainingIgnoreCaseOrderByUpdatedDateDesc(
            STORE_ID, true, true, BRAND_1_NAME, UPDATED_BY, DEFAULT_PAGEABLE);
  }

  @Test
  public void findByProductCodeTest() {
    when(this.repository
        .findByStoreIdAndProductCodeContainingIgnoreCaseAndMarkForDeleteFalse(STORE_ID, BRAND_1_NAME, DEFAULT_PAGEABLE))
        .thenReturn(productPage);
    service.findByProductCode(STORE_ID, BRAND_1_NAME, DEFAULT_PAGEABLE);
    verify(this.repository)
        .findByStoreIdAndProductCodeContainingIgnoreCaseAndMarkForDeleteFalse(STORE_ID, BRAND_1_NAME, DEFAULT_PAGEABLE);
  }

  @Test
  public void findByShippingWeightBiggerOrEqualThan() {
    when(this.repository.findByStoreIdAndShippingWeightGreaterThanEqualAndMarkForDeleteFalse(STORE_ID, SHIPPING_WEIGHT,
        DEFAULT_PAGEABLE)).thenReturn(productPage);
    service.findByShippingWeightBiggerOrEqualThan(STORE_ID, SHIPPING_WEIGHT, DEFAULT_PAGEABLE);
    verify(this.repository)
        .findByStoreIdAndShippingWeightGreaterThanEqualAndMarkForDeleteFalse(STORE_ID, SHIPPING_WEIGHT,
            DEFAULT_PAGEABLE);
  }

  @Test
  public void findByShippingWeightLesserOrEqualThan() {
    when(this.repository
        .findByStoreIdAndShippingWeightLessThanEqualAndMarkForDeleteFalse(STORE_ID, SHIPPING_WEIGHT, DEFAULT_PAGEABLE))
        .thenReturn(productPage);
    service.findByShippingWeightLesserOrEqualThan(STORE_ID, SHIPPING_WEIGHT, DEFAULT_PAGEABLE);
    verify(this.repository)
        .findByStoreIdAndShippingWeightLessThanEqualAndMarkForDeleteFalse(STORE_ID, SHIPPING_WEIGHT,
            DEFAULT_PAGEABLE);
  }

  @Test
  public void getProductByStoreIdAndProductCodeAndMarkForDeleteFalse() throws Exception {
    when(service.getRepository().findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(product);
    service.getProductByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void evictProductCacheByProductCodes() throws Exception {
    when(repository.findByStoreIdAndProductCodeInAndMarkForDeleteFalse(STORE_ID, productCodes)).thenReturn(productList);
    doNothing().when(applicationCacheServiceBean).evictProductCacheByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    service.evictProductCacheByProductCodes(STORE_ID, productCodes);
    verify(repository).findByStoreIdAndProductCodeInAndMarkForDeleteFalse(STORE_ID, productCodes);
    verify(applicationCacheServiceBean, times(4)).evictProductCacheByStoreIdAndProductCode(any(), any());
  }

  @Test
  public void republishProductByProductCodesForMigratedProductsTest() throws Exception {
    List<String> productCodes = new ArrayList<>();
    productCodes.add(PRODUCT_CODE);
    this.service.republishProductByProductCodes(STORE_ID, productCodes, Constants.MIGRATION);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verifyProductDetailInteractionsForMarkForDeleteFalseCached();
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(DEFAULT_STORE_ID, product, false);
    Mockito.verify(this.domainEventPublisherService).publishProductForMigratedProducts(productArgumentCaptor.capture(), eq(true));
  }

  @Test
  public void copyProductTest() throws Exception {
    Product request = generateProductRequest();
    request.setVersion(1L);
    product.setVersion(1L);
    product.setProductItems(productItems);
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, OLD_PRODUCT_CODE)).thenReturn(request);
    when(repository.save(any(Product.class))).thenReturn(product);
    this.service.copyProduct(OLD_PRODUCT_CODE, NEW_PRODUCT_CODE, STORE_ID, null, CREATED_MERCHANT);
    verify(this.repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, OLD_PRODUCT_CODE);
    verify(this.repository).save(productArgumentCaptor.capture());
    assertEquals(NEW_PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    assertEquals(CREATED_MERCHANT, productArgumentCaptor.getValue().getCreatedMerchant());
    assertEquals(2, productArgumentCaptor.getValue().getProductItems().size());
  }

  @Test
  public void copyProductWithProductNotNullTest() throws Exception {
    Product request = generateProductRequest();
    request.setVersion(1L);
    product.setVersion(1L);
    product.setProductItems(productItems);
    for(ProductItem productItem : request.getProductItems()) {
      productItem.setSkuCode(null);
    }
    when(productItemService.getSequence(NEW_PRODUCT_CODE)).thenReturn("00001");
    when(repository.save(any(Product.class))).thenReturn(product);
    this.service.copyProduct(null, NEW_PRODUCT_CODE, STORE_ID, request, CREATED_MERCHANT);
    verify(this.repository).save(productArgumentCaptor.capture());
    verify(this.productItemService, times(2)).getSequence(NEW_PRODUCT_CODE);
    assertEquals(NEW_PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    assertEquals(CREATED_MERCHANT, productArgumentCaptor.getValue().getCreatedMerchant());
    assertEquals(2, productArgumentCaptor.getValue().getProductItems().size());
    assertEquals(NEW_PRODUCT_SKU_CODE, productArgumentCaptor.getValue().getProductItems().get(0).getSkuCode());
    assertEquals(2, productArgumentCaptor.getValue().getProductItems().size());
  }

  @Test
  public void copyProductWithNewProductTest() throws Exception {
    Product request = new Product();
    when(productItemService.getSequence(NEW_PRODUCT_CODE)).thenReturn("00001");
    when(repository.save(any(Product.class))).thenReturn(product);
    this.service.copyProduct(null, NEW_PRODUCT_CODE, STORE_ID, request, CREATED_MERCHANT);
    verify(this.repository).save(productArgumentCaptor.capture());
    verify(this.productItemService, times(0)).getSequence(NEW_PRODUCT_CODE);
    assertEquals(NEW_PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    assertEquals(CREATED_MERCHANT, productArgumentCaptor.getValue().getCreatedMerchant());
  }

  @Test
  public void copyProductUpdateRequestTest() throws Exception {
    Product request = generateProductRequest();
    request.setVersion(1L);
    product.setVersion(1L);
    product.setProductItems(productItems);
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE)).thenReturn(request);
    when(productItemService.getSequence(PRODUCT_CODE)).thenReturn("00001");
    when(repository.save(any(Product.class))).thenReturn(product);
    this.service.copyProduct(PRODUCT_CODE, PRODUCT_CODE, STORE_ID, request, CREATED_MERCHANT);
    verify(this.repository).save(productArgumentCaptor.capture());
    verify(this.repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(applicationCacheServiceBean).evictProductItemsCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductAttributesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductCategoriesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductItemImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductItemAttributeValuesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductCacheByStoreIdAndProductCode(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductCacheByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    assertEquals(2, productArgumentCaptor.getValue().getProductItems().size());
  }


  @Test
  public void copyProductUpdateRequestWithEmptyRequestTest() throws Exception {
    Product request = new Product();
    request.setVersion(1L);
    request.setProductCode(PRODUCT_CODE);
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE)).thenReturn(request);
    when(productItemService.getSequence(PRODUCT_CODE)).thenReturn("00001");
    when(repository.save(any(Product.class))).thenReturn(product);
    this.service.copyProduct(PRODUCT_CODE, PRODUCT_CODE, STORE_ID, request, CREATED_MERCHANT);
    verify(this.repository).save(productArgumentCaptor.capture());
    verify(this.repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(applicationCacheServiceBean).evictProductItemsCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductAttributesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductCategoriesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductItemImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductItemAttributeValuesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductCacheByStoreIdAndProductCode(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductCacheByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
  }


  @Test
  public void copyProductUpdateRequestWithExtraAttributesTest() throws Exception {
    Product request = generateProductRequest();
    Product response = generateProductRequest();
    ProductAttribute productAttribute = new ProductAttribute();
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(ATTRIBUTE_ID_4);
    productAttribute.setAttribute(attribute);
    request.getProductAttributes().add(productAttribute);
    request.setVersion(1L);
    product.setVersion(1L);
    product.setProductItems(productItems);
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE)).thenReturn(response);
    when(productItemService.getSequence(PRODUCT_CODE)).thenReturn("00001");
    when(repository.save(any(Product.class))).thenReturn(product);
    this.service.copyProduct(PRODUCT_CODE, PRODUCT_CODE, STORE_ID, request, CREATED_MERCHANT);
    verify(this.repository).save(productArgumentCaptor.capture());
    verify(this.repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(applicationCacheServiceBean).evictProductItemsCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductAttributesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductCategoriesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductItemImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductItemAttributeValuesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductCacheByStoreIdAndProductCode(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductCacheByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    assertEquals(2, productArgumentCaptor.getValue().getProductItems().size());
  }

  @Test
  public void copyProductUpdateRequestWidtRemovingDeletedAttributeTest() throws Exception {
    Product request = generateProductRequest();
    Product response = generateProductRequest();
    request.getProductAttributes().remove(0);
    request.setVersion(1L);
    product.setVersion(1L);
    product.setProductItems(productItems);
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE)).thenReturn(response);
    when(productItemService.getSequence(PRODUCT_CODE)).thenReturn("00001");
    when(repository.save(any(Product.class))).thenReturn(product);
    this.service.copyProduct(PRODUCT_CODE, PRODUCT_CODE, STORE_ID, request, CREATED_MERCHANT);
    verify(this.repository).save(productArgumentCaptor.capture());
    verify(this.repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(applicationCacheServiceBean).evictProductItemsCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductAttributesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductCategoriesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductItemImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductItemAttributeValuesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductCacheByStoreIdAndProductCode(STORE_ID, PRODUCT_ID);
    verify(applicationCacheServiceBean).evictProductCacheByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    assertEquals(2, productArgumentCaptor.getValue().getProductItems().size());
  }

  @Test
  public void updateProductItemImagesByProductCodeCopyToAllVariantImagesTest() throws Exception {
    ProductItemImageUpdateRequest productItemImageUpdateRequest = new ProductItemImageUpdateRequest();
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath("/xyz.png");
    image.setMainImages(false);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    copyToAllVariantImages.add(image);
    productItemImageUpdateRequest.setCopyToAllVariantImages(copyToAllVariantImages);
    productItemImageUpdateRequest.setProductCode(PRODUCT_CODE);
    productItemImageUpdateRequest.setNeedCorrection(true);
    productItemImageUpdateRequest.setActivatedBefore(true);
    List<ProductItemImageRequest> productItemImageRequestList = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    List<Image> itemImage1 = new ArrayList<>();
    Image image1 = new Image();
    image1.setLocationPath(LOCATION_PATH1);
    itemImage1.add(image1);
    productItemImageRequest.setItemImages(itemImage1);
    productItemImageRequestList.add(productItemImageRequest);
    productItemImageUpdateRequest.setNewProductItemImages(productItemImageRequestList);

    Image itemImage = new Image();
    itemImage.setLocationPath(LOCATION_PATH);
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    product.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    product.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH1);
    product.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    product.getProductItems().get(0).getProductItemImages().get(1).setLocationPath(LOCATION_PATH_2);
    Mockito.when(repository.saveAndFlush(any())).thenReturn(product);
    Mockito.when(
        systemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH))
        .thenReturn(new SystemParameter(Constants.ACTIVE_IMAGE_SWITCH, "false", Constants.ACTIVE_IMAGE_SWITCH));

    this.service.updateProductItemImagesByProductCode(productItemImageUpdateRequest, STORE_ID);

    verify(systemParameterService).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(repository).saveAndFlush(any());
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    assertEquals(3, product.getProductItems().get(0).getProductItemImages().size());
    assertEquals(5, product.getProductImages().size());
    assertTrue(!product.isEdited());
  }

  @Test
  public void updateProductItemImagesByProductCodeCopyToAllVariantImagesCommonImageTrueTest() throws Exception {
    ProductItemImageUpdateRequest productItemImageUpdateRequest = new ProductItemImageUpdateRequest();
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath("/xyz.png");
    image.setMainImages(false);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    image.setCommonImage(true);
    copyToAllVariantImages.add(image);
    productItemImageUpdateRequest.setCopyToAllVariantImages(copyToAllVariantImages);
    productItemImageUpdateRequest.setProductCode(PRODUCT_CODE);
    productItemImageUpdateRequest.setNeedCorrection(true);
    productItemImageUpdateRequest.setActivatedBefore(true);
    List<ProductItemImageRequest> productItemImageRequestList = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    List<Image> itemImage1 = new ArrayList<>();
    Image image1 = new Image();
    image1.setLocationPath(LOCATION_PATH1);
    image1.setCommonImage(true);
    itemImage1.add(image1);
    productItemImageRequest.setItemImages(itemImage1);
    productItemImageRequestList.add(productItemImageRequest);
    productItemImageUpdateRequest.setNewProductItemImages(productItemImageRequestList);

    Image itemImage = new Image();
    itemImage.setLocationPath(LOCATION_PATH);
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductImages(productImages);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    product.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    product.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH1);
    product.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    product.getProductItems().get(0).getProductItemImages().get(1).setLocationPath(LOCATION_PATH_2);
    Mockito.when(repository.saveAndFlush(any())).thenReturn(product);
    Mockito.when(
            systemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH))
        .thenReturn(new SystemParameter(Constants.ACTIVE_IMAGE_SWITCH, "false", Constants.ACTIVE_IMAGE_SWITCH));
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);

    this.service.updateProductItemImagesByProductCode(productItemImageUpdateRequest, STORE_ID);

    verify(systemParameterService).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(repository).saveAndFlush(any());
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    assertEquals(3, product.getProductItems().get(0).getProductItemImages().size());
    assertEquals(5, product.getProductImages().size());
    assertTrue(!product.isEdited());
  }

  @Test
  public void updateProductItemImagesByProductCodeCopyToAllVariantImagesCommonImageTrueMainImageTrueTest() throws Exception {
    ProductItemImageUpdateRequest productItemImageUpdateRequest = new ProductItemImageUpdateRequest();
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath("/xyz.png");
    image.setMainImages(true);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    image.setCommonImage(false);
    copyToAllVariantImages.add(image);
    productItemImageUpdateRequest.setCopyToAllVariantImages(copyToAllVariantImages);
    productItemImageUpdateRequest.setProductCode(PRODUCT_CODE);
    productItemImageUpdateRequest.setNeedCorrection(true);
    productItemImageUpdateRequest.setActivatedBefore(true);
    List<ProductItemImageRequest> productItemImageRequestList = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    List<Image> itemImage1 = new ArrayList<>();
    Image image1 = new Image();
    image1.setLocationPath(LOCATION_PATH1);
    image1.setCommonImage(false);
    image1.setMainImages(true);
    itemImage1.add(image1);
    productItemImageRequest.setItemImages(itemImage1);
    productItemImageRequestList.add(productItemImageRequest);
    productItemImageUpdateRequest.setNewProductItemImages(productItemImageRequestList);

    Image itemImage = new Image();
    itemImage.setLocationPath(LOCATION_PATH);
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductImages(productImages);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    product.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    product.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH1);
    product.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    product.getProductItems().get(0).getProductItemImages().get(1).setLocationPath(LOCATION_PATH_2);
    Mockito.when(repository.saveAndFlush(any())).thenReturn(product);
    Mockito.when(
            systemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH))
        .thenReturn(new SystemParameter(Constants.ACTIVE_IMAGE_SWITCH, "false", Constants.ACTIVE_IMAGE_SWITCH));
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);

    this.service.updateProductItemImagesByProductCode(productItemImageUpdateRequest, STORE_ID);

    verify(systemParameterService).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    Mockito.verify(repository).saveAndFlush(any());
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    assertEquals(3, product.getProductItems().get(0).getProductItemImages().size());
    assertEquals(5, product.getProductImages().size());
    assertTrue(!product.isEdited());
  }

  @Test
  public void updateProductItemImagesByProductExceptionTest() throws Exception {
    ProductItemImageUpdateRequest productItemImageUpdateRequest = new ProductItemImageUpdateRequest();
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath("/xyz.png");
    image.setMainImages(false);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    copyToAllVariantImages.add(image);
    productItemImageUpdateRequest.setCopyToAllVariantImages(copyToAllVariantImages);
    productItemImageUpdateRequest.setProductCode(PRODUCT_CODE);

    Image itemImage = new Image();
    itemImage.setLocationPath(LOCATION_PATH);
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductItems(
        productItems.stream().filter(productItem -> !productItem.isMarkForDelete()).collect(toList()));
    product.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    product.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);

    try {
      this.service.updateProductItemImagesByProductCode(productItemImageUpdateRequest, STORE_ID);
    } catch (Exception e) {

    } finally {
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
      verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
      verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
      verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
      verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    }
  }

  @Test
  public void updateProductItemImagesByProductSysParamValueEmptyTest() throws Exception {
    ProductItemImageUpdateRequest productItemImageUpdateRequest = new ProductItemImageUpdateRequest();
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath("/xyz.png");
    image.setMainImages(false);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    copyToAllVariantImages.add(image);
    productItemImageUpdateRequest.setCopyToAllVariantImages(copyToAllVariantImages);
    productItemImageUpdateRequest.setProductCode(PRODUCT_CODE);

    Image itemImage = new Image();
    itemImage.setLocationPath(LOCATION_PATH);
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductItems(
        productItems.stream().filter(productItem -> !productItem.isMarkForDelete()).collect(toList()));
    product.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    product.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);

    Mockito.when(
        systemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH))
        .thenReturn(new SystemParameter(Constants.ACTIVE_IMAGE_SWITCH, "", Constants.ACTIVE_IMAGE_SWITCH));

    try {
      Assertions.assertThrows(ApplicationException.class, () -> this.service.updateProductItemImagesByProductCode(productItemImageUpdateRequest, STORE_ID));
    } catch (Exception e) {

    } finally {
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
      verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
      verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
      verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
      verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
      verify(systemParameterService).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH);
    }
  }

  @Test
  public void updateProductItemImagesByProductSysParamNullTest() throws Exception {
    ProductItemImageUpdateRequest productItemImageUpdateRequest = new ProductItemImageUpdateRequest();
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath("/xyz.png");
    image.setMainImages(false);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    copyToAllVariantImages.add(image);
    productItemImageUpdateRequest.setCopyToAllVariantImages(copyToAllVariantImages);
    productItemImageUpdateRequest.setProductCode(PRODUCT_CODE);

    Image itemImage = new Image();
    itemImage.setLocationPath(LOCATION_PATH);
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductItems(
        productItems.stream().filter(productItem -> !productItem.isMarkForDelete()).collect(toList()));
    product.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    product.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);

    Mockito.when(
        systemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH))
        .thenReturn(null);

    try {
      Assertions.assertThrows(ApplicationException.class, () -> this.service.updateProductItemImagesByProductCode(productItemImageUpdateRequest, STORE_ID));
    } catch (Exception e) {
      throw e;
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
      verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
      verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
      verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
      verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
      verify(systemParameterService).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH);
    }
  }

  @Test
  public void updateProductItemImagesByProductCodeUpdateProductItemImagesTest() throws Exception {
    ProductItemImageUpdateRequest productItemImageUpdateRequest = new ProductItemImageUpdateRequest();
    List<ProductItemImageRequest> updatedProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(true);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(image);
    productItemImageRequest.setSkuCode(SKU_CODE);
    updatedProductItemImages.add(productItemImageRequest);
    productItemImageUpdateRequest.setUpdateProductItemImages(updatedProductItemImages);
    productItemImageUpdateRequest.setProductCode(PRODUCT_CODE);


    Image itemImage = new Image();
    itemImage.setLocationPath(LOCATION_PATH);
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));

    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setMainImages(true);
    productItemImage.setMarkForDelete(false);
    productItemImage.setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).setProductItemImages(Arrays.asList(productItemImage));
    List<ProductItemImageRequest> productItemImageRequestList = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest1 = new ProductItemImageRequest();
    List<Image> itemImage1 = new ArrayList<>();
    Image image1 = new Image();
    image1.setLocationPath(LOCATION_PATH1);
    itemImage1.add(image1);
    productItemImageRequest1.setItemImages(itemImage1);
    productItemImageRequestList.add(productItemImageRequest1);
    productItemImageUpdateRequest.setNewProductItemImages(productItemImageRequestList);
    Mockito.when(repository.saveAndFlush(any())).thenReturn(product);
    Mockito.when(
        systemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH))
        .thenReturn(new SystemParameter(Constants.ACTIVE_IMAGE_SWITCH, "false", Constants.ACTIVE_IMAGE_SWITCH));


    this.service.updateProductItemImagesByProductCode(productItemImageUpdateRequest, STORE_ID);
    verify(systemParameterService).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(repository).saveAndFlush(any());
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    assertEquals(1, product.getProductItems().get(0).getProductItemImages().size());
  }

  @Test
  public void updateProductItemImagesByProductCodeNewProductItemImagesTest() throws Exception {
    ProductItemImageUpdateRequest productItemImageUpdateRequest = new ProductItemImageUpdateRequest();
    List<ProductItemImageRequest> newProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    Image image = new Image();
    image.setLocationPath("/xyz.png");
    image.setMainImages(false);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(image);
    productItemImageRequest.setSkuCode(ITEM_SKU_CODE_1);
    newProductItemImages.add(productItemImageRequest);
    productItemImageUpdateRequest.setNewProductItemImages(newProductItemImages);
    productItemImageUpdateRequest.setProductCode(PRODUCT_CODE);

    Image itemImage = new Image();
    itemImage.setLocationPath(LOCATION_PATH);
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    product.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH1);
    product.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    product.getProductItems().get(0).getProductItemImages().get(1).setLocationPath(LOCATION_PATH_2);
    Mockito.when(repository.saveAndFlush(any())).thenReturn(product);
    Mockito.when(
        systemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH))
        .thenReturn(new SystemParameter(Constants.ACTIVE_IMAGE_SWITCH, "false", Constants.ACTIVE_IMAGE_SWITCH));

    this.service.updateProductItemImagesByProductCode(productItemImageUpdateRequest, STORE_ID);
    verify(systemParameterService).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    verify(repository).saveAndFlush(any());
    assertEquals(3, product.getProductItems().get(0).getProductItemImages().size());
    assertEquals(4, product.getProductImages().size());
    assertTrue(product.isEdited());
  }

  @Test
  public void updateProductItemImagesByProductCodeUpdateProductImageTest() throws Exception {
    ProductItemImageUpdateRequest productItemImageUpdateRequest = new ProductItemImageUpdateRequest();
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath("/xyz.png");
    image.setMainImages(false);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    copyToAllVariantImages.add(image);
    productItemImageUpdateRequest.setCopyToAllVariantImages(copyToAllVariantImages);
    productItemImageUpdateRequest.setProductCode(PRODUCT_CODE);

    List<ProductItemImageRequest> newProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    Image newImage = new Image();
    newImage.setLocationPath("/xyz.png");
    newImage.setMainImages(false);
    newImage.setHashCode("hashCode");
    newImage.setSequence(1);
    newImage.setOriginalImage(true);
    newImage.setActive(false);
    newImage.setMarkForDelete(false);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(newImage);
    productItemImageRequest.setSkuCode(ITEM_SKU_CODE_1);
    newProductItemImages.add(productItemImageRequest);
    productItemImageUpdateRequest.setNewProductItemImages(newProductItemImages);
    productItemImageUpdateRequest.setProductCode(PRODUCT_CODE);

    Image itemImage = new Image();
    itemImage.setLocationPath(LOCATION_PATH);
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));

    product.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    product.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH1);
    product.getProductItems().get(0).getProductItemImages().get(1).setLocationPath(LOCATION_PATH_2);
    Mockito.when(repository.saveAndFlush(any())).thenReturn(product);
    Mockito.when(
        systemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH))
        .thenReturn(new SystemParameter(Constants.ACTIVE_IMAGE_SWITCH, "false", Constants.ACTIVE_IMAGE_SWITCH));


    this.service.updateProductItemImagesByProductCode(productItemImageUpdateRequest, STORE_ID);
    verify(systemParameterService).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(repository).saveAndFlush(any());
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    assertEquals(3, product.getProductItems().get(0).getProductItemImages().size());
    assertEquals(4, product.getProductImages().size());
    assertTrue(product.isEdited());
  }

  @Test
  public void updateProductItemImagesByProductCodeCopyToAllVariantExistingImagesTest() throws Exception {
    ProductItemImageUpdateRequest productItemImageUpdateRequest = new ProductItemImageUpdateRequest();
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(false);
    image.setHashCode("hashcode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    copyToAllVariantImages.add(image);
    productItemImageUpdateRequest.setCopyToAllVariantImages(copyToAllVariantImages);
    productItemImageUpdateRequest.setProductCode(PRODUCT_CODE);

    Image itemImage = new Image();
    itemImage.setLocationPath(LOCATION_PATH);
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(itemImage, productImage);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    product.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    product.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    product.getProductItems().get(0).getProductItemImages().get(1).setLocationPath(LOCATION_PATH_2);
    List<ProductItemImageRequest> productItemImageRequestList = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest1 = new ProductItemImageRequest();
    List<Image> itemImage1 = new ArrayList<>();
    Image image1 = new Image();
    image1.setLocationPath(LOCATION_PATH1);
    itemImage1.add(image1);
    productItemImageRequest1.setItemImages(itemImage1);
    productItemImageRequestList.add(productItemImageRequest1);
    productItemImageUpdateRequest.setNewProductItemImages(productItemImageRequestList);
    Mockito.when(repository.saveAndFlush(any())).thenReturn(product);
    Mockito.when(
        systemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH))
        .thenReturn(new SystemParameter(Constants.ACTIVE_IMAGE_SWITCH, "false", Constants.ACTIVE_IMAGE_SWITCH));

    this.service.updateProductItemImagesByProductCode(productItemImageUpdateRequest, STORE_ID);
    verify(systemParameterService).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(repository).saveAndFlush(any());
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
    assertEquals(4, product.getProductImages().size());
    assertTrue(product.isEdited());
  }

  @Test
  public void updateProductItemImagesByProductCodeUpdateProductItemMainImagesTest() throws Exception {
    ProductItemImageUpdateRequest productItemImageUpdateRequest = new ProductItemImageUpdateRequest();
    List<ProductItemImageRequest> updatedProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(true);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(true);
    image.setMarkForDelete(false);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(image);
    Image image1 = new Image();
    image1.setLocationPath(LOCATION_PATH1);
    image1.setMainImages(false);
    image1.setHashCode("hashCode");
    image1.setSequence(1);
    image1.setOriginalImage(true);
    image1.setActive(true);
    image1.setMarkForDelete(false);
    productItemImageRequest.setSkuCode(SKU_CODE);
    productItemImageRequest.getItemImages().add(image1);
    updatedProductItemImages.add(productItemImageRequest);
    productItemImageUpdateRequest.setUpdateProductItemImages(updatedProductItemImages);
    productItemImageUpdateRequest.setProductCode(PRODUCT_CODE);
    productItemImageUpdateRequest.setNeedCorrection(true);
    productItemImageUpdateRequest.setActivatedBefore(false);

    Image itemImage = new Image();
    itemImage.setLocationPath(LOCATION_PATH);
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));

    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setMainImages(false);
    productItemImage.setMarkForDelete(false);
    productItemImage.setActive(true);
    productItemImage.setOriginalImage(true);
    productItemImage.setEdited(true);
    productItemImage.setLocationPath(LOCATION_PATH);

    List<ProductItemImage> productItemImages = new ArrayList<>();
    productItemImages.add(productItemImage);
    product.getProductItems().get(0).setProductItemImages(productItemImages);
    product.getProductItems().get(0).setSkuCode(SKU_CODE);
    ProductItemImage productItemImage3 = new ProductItemImage();
    productItemImage3.setLocationPath(RESIZE_LOCATION_PATH);
    productItemImage3.setSequence(1);
    productItemImage3.setMarkForDelete(false);
    productItemImage3.setMainImages(false);
    productItemImage3.setActive(false);
    productItemImage3.setEdited(true);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage3);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(RESIZE_LOCATION_PATH1);
    productItemImage1.setSequence(1);
    productItemImage1.setMarkForDelete(false);
    productItemImage1.setMainImages(true);
    productItemImage1.setActive(false);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage1);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setLocationPath(LOCATION_PATH1);
    productItemImage2.setSequence(1);
    productItemImage2.setMarkForDelete(false);
    productItemImage2.setMainImages(true);
    productItemImage2.setActive(true);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage2);
    /*product.getProductImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductImages().get(1).setLocationPath(RESIZE_LOCATION_PATH);
    product.getProductImages().get(2).setLocationPath(LOCATION_PATH1);*/
    List<ProductItemImageRequest> productItemImageRequestList = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest1 = new ProductItemImageRequest();
    List<Image> itemImage1 = new ArrayList<>();
    Image image2 = new Image();
    image2.setLocationPath(LOCATION_PATH1);
    itemImage1.add(image2);
    productItemImageRequest1.setItemImages(itemImage1);
    productItemImageRequestList.add(productItemImageRequest1);
    productItemImageUpdateRequest.setNewProductItemImages(productItemImageRequestList);

    List<ProductItemImageRequest> productItemImageRequestList1 = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest2 = new ProductItemImageRequest();
    List<Image> itemImage2 = new ArrayList<>();
    Image image3 = new Image();
    image2.setLocationPath(LOCATION_PATH1);
    itemImage1.add(image3);
    productItemImageRequest2.setItemImages(itemImage2);
    productItemImageRequestList1.add(productItemImageRequest2);
    productItemImageUpdateRequest.setNewProductItemImages(productItemImageRequestList1);
    Mockito.when(repository.saveAndFlush(any())).thenReturn(product);
    Mockito.when(
        systemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH))
        .thenReturn(new SystemParameter(Constants.ACTIVE_IMAGE_SWITCH, "false", Constants.ACTIVE_IMAGE_SWITCH));
    this.service.updateProductItemImagesByProductCode(productItemImageUpdateRequest, STORE_ID);
    verify(systemParameterService).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH);
    verify(applicationContext).getBean(ProductService.class);
    verify(repository).saveAndFlush(any());
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    assertEquals(4, product.getProductItems().get(0).getProductItemImages().size());
  }

  @Test
  public void updateProductItemImagesByProductCode_emptyProductItemImages() throws Exception {
    ProductItemImageUpdateRequest productItemImageUpdateRequest =
        new ProductItemImageUpdateRequest();
    productItemImageUpdateRequest.setCopyToAllVariantImages(Collections.EMPTY_LIST);
    productItemImageUpdateRequest.setProductCode(PRODUCT_CODE);

    productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductItems(
        productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
            .collect(toList()));
    product.getProductItems().get(0).setProductItemImages(Collections.EMPTY_LIST);
    Mockito.when(systemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH))
        .thenReturn(new SystemParameter(Constants.ACTIVE_IMAGE_SWITCH, "false",
            Constants.ACTIVE_IMAGE_SWITCH));
    try {
      Assertions.assertThrows(ApplicationException.class, () -> this.service.updateProductItemImagesByProductCode(productItemImageUpdateRequest, STORE_ID));
    } finally {
      verify(systemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH);
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
      verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
      verify(productCategoryService)
          .getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
      verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
      verify(productItemServiceWrapper)
          .setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    }
  }

  @Test
  public void getImagesByProductCodeTest() throws Exception {
    service.getImagesByProductCode(STORE_ID, PRODUCT_CODE);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(DEFAULT_STORE_ID, product, false);
  }

  @Test
  public void updateAndMarkForNeedRevisionTest() throws Exception {
    productCategory.setCategory(category);
    productCategory.setCategoryId(CATEGORY_ID);
    Product productRequest = generateProductRequest();
    productRequest.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH_1);
    productRequest.getProductItems().get(0).getProductItemImages().get(1).setLocationPath(LOCATION_PATH_2);
    product = generateProduct();
    ProductImage productImageDuplicate1 = new ProductImage();
    productImageDuplicate1.setLocationPath(LOCATION_PATH_2);
    productImageDuplicate1.setMarkForDelete(true);
    ProductImage productImageDuplicate2 = new ProductImage();
    productImageDuplicate2.setId(PRODUCT_IMAGE_ID_4);
    productImageDuplicate2.setLocationPath(LOCATION_PATH_1);
    productImageDuplicate2.setMarkForDelete(false);
    ProductImage productImageDuplicate3 = new ProductImage();
    productImageDuplicate3.setId(PRODUCT_IMAGE_ID_3);
    productImageDuplicate3.setLocationPath(LOCATION_PATH_3);
    productImageDuplicate3.setMarkForDelete(false);

    ProductImage productImage = productImages.set(1, productImageDuplicate1);
    productImages.add(productImage);
    productImages.add(productImageDuplicate2);
    productImages.add(productImageDuplicate3);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH_2);
    product.getProductItems().get(0).getProductItemImages().get(1).setLocationPath(LOCATION_PATH_3);
    product.getProductItems().get(0).setSkuCode(ITEM_SKU_CODE_1);
    product.getProductItems().get(0).getProductItemAttributeValues()
        .remove(product.getProductItems().get(0).getProductItemAttributeValues().size() - 1);
    productRequest.setVersion(Long.valueOf(1));
    product.setId(PRODUCT_ID);
    product.setStoreId(STORE_ID);
    product.setVersion(Long.valueOf(0));
    when(imageService.getProductImagesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID)).thenReturn(productImages);
    when(categoryReferenceService.getSalesCategoryReferenceByMasterCategory(CATEGORY_ID, CATEGORY_ID_3, false)).thenReturn(
        productSalesCategoryMapping);
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    when(this.productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID)).thenReturn(
        new ArrayList<>(Collections.singletonList(productCategory)));
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID)).thenReturn(category);
    when(productAttributeService.getProductAttributesByStoreIdAndProductIdCached(STORE_ID, product.getId())).thenReturn(
        new ArrayList<>(Collections.singletonList(productAttribute1)));
    productRequest.setDistributionInfo(PRODUCT_CODE);
    this.service.updateAndMarkForNeedRevision(productRequest);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(this.productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString());
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(anyString(),
        anyString());
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(anyString(), any(Product.class),
        eq(true));
    verify(repository).saveAndFlush(any(Product.class));
    verify(this.imageService, times(2)).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, true);
    verify(applicationContext).getBean(ProductService.class);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID);
    verify(categoryReferenceService).getSalesCategoryReferenceByMasterCategory(CATEGORY_ID, CATEGORY_ID_3, false);
  }

  @Test
  public void updateAndMarkForNeedRevision_changedBrandTest() throws Exception {
    Product productRequest = generateProductRequest();
    product = generateProduct();
    product.setBrand(ORIGINAL_BRAND);
    productRequest.setVersion(Long.valueOf(1));
    product.setVersion(Long.valueOf(0));
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    when(this.productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID)).thenReturn(
        Arrays.asList(productCategory));
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID)).thenReturn(category);
    when(productAttributeService.getProductAttributesByStoreIdAndProductIdCached(STORE_ID, product.getId())).thenReturn(
        Arrays.asList(productAttribute1));
    UpdateNeedRevisionDTO updateAndMarkForNeedRevision = this.service.updateAndMarkForNeedRevision(productRequest);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(this.productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(any(),
        any());
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(any(),
        any());
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(any(), any(Product.class),
        eq(true));
    verify(repository).saveAndFlush(any(Product.class));
    verify(this.imageService, times(2)).getProductImagesByStoreIdAndProductIdCached(any(),
        any());
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, true);
    verify(applicationContext).getBean(ProductService.class);
    assertNull(updateAndMarkForNeedRevision.getProductSalesCategoryMapping());
  }

  @Test
  public void updateAndMarkForNeedRevision_contentExpiredTest() throws Exception {
    Product productRequest = generateProductRequest();
    product = generateProduct();
    product.setBrand(ORIGINAL_BRAND);
    productRequest.setVersion(Long.valueOf(0));
    product.setVersion(Long.valueOf(1));
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> this.service.updateAndMarkForNeedRevision(productRequest));
    } finally {
      verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
      verify(applicationContext).getBean(ProductService.class);
    }
  }

  @Test
  public void testProcessCompressedUpdatedVideo_WithExistingVideo() throws Exception {
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setVideo(EXISTING_VIDEO_JSON);
    VideoDTO videoDTO = new VideoDTO();
    videoDTO.setVideoId(VIDEO_ID);
    videoDTO.setFinalUrl(FINAL_URL);
    videoDTO.setCoverImagePath(COVER_IMAGE_PATH);
    videoDTO.setVideoName(TEST_VIDEO);
    videoDTO.setSourceUrl(SOURCE_URL);

    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(anyString(), eq(PRODUCT_CODE)))
        .thenReturn(product);
    when(objectMapper.writeValueAsString(any(VideoDTO.class))).thenReturn(UPDATED_VIDEO_JSON);
    service.processCompressedUpdatedVideo(PRODUCT_CODE, videoDTO);
    verify(repository).updateVideoByStoreIDAndProductCode(Constants.DEFAULT_STORE_ID, PRODUCT_CODE,
      UPDATED_VIDEO_JSON);
  }


  @Test
  public void updateAndMarkForNeedRevision_withCategoryChangeTest() throws Exception {
    Product productRequest = generateProductRequest();
    productRequest.getProductCategories().get(0).getCategory().setCategoryCode(CATEGORY_CODE_2);
    product = generateProduct();
    product.getProductCategories().add(productCategory1);
    getProductWithDifferentCategory(product, new ArrayList<>());
    productRequest.setVersion(Long.valueOf(1));
    product.setVersion(Long.valueOf(0));
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    when(this.productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID)).thenReturn(
        Arrays.asList(productCategory));
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID)).thenReturn(category);
    when(productAttributeService.getProductAttributesByStoreIdAndProductIdCached(STORE_ID, product.getId())).thenReturn(
        Arrays.asList(productAttribute1));
    this.service.updateAndMarkForNeedRevision(productRequest);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(this.productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(any(),
        any());
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(any(),
        any());
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(any(), any(Product.class),
        eq(true));
    verify(repository).saveAndFlush(any(Product.class));
    verify(this.imageService, times(2)).getProductImagesByStoreIdAndProductIdCached(any(),
        any());
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, true);
    verify(applicationContext).getBean(ProductService.class);
  }

  @Test
  public void publishVatUpdateEventTest() {
    ProductItem productItem1 = new ProductItem();
    productItem1.setSkuCode(SKU_CODE);
    productItem1.setVatApplicable(true);
    ProductItem productItem2 = new ProductItem();
    productItem2.setSkuCode(SKU_CODE);
    productItem2.setVatApplicable(false);

    when(domainEventPublisherService.publishVatApplicableUpdateEvent(anyString(), anyBoolean()))
        .thenReturn(new VatUpdateDomainEventModel());

    service.publishVatUpdateEvent(Arrays.asList(productItem1, productItem2));

    verify(domainEventPublisherService).publishVatApplicableUpdateEvent(SKU_CODE, true);
    verify(domainEventPublisherService).publishVatApplicableUpdateEvent(SKU_CODE, false);
  }

  @Test
  public void publishVatUpdateEventEmptyItemListTest() {
    service.publishVatUpdateEvent(new ArrayList<>());
  }

  @Test
  public void testRegenerateProduct_withVatChangedItem() throws Exception {
    Product product = this.createAdvancedProduct();
    List<String> attributeIds = new ArrayList<>();
    Product savedProduct = getProduct(product, attributeIds);
    setProductImage(savedProduct);
    savedProduct.getProductItems().get(0).setViewable(true);
    savedProduct.getProductItems().get(0).setActivated(true);
    savedProduct.getProductItems().get(0).setVatApplicable(false);
    when(repository.saveAndFlush(any(Product.class))).thenReturn(product);
    when(productImageCleanupRepository.findLocationPathByProductCode(product.getProductCode())).thenReturn(new ArrayList<>());
    when(productImageCleanupRepository.saveAll(anyList())).thenReturn(new ArrayList());
    product.getProductItems().get(0).setVatApplicable(true);
    this.service.regenerateProductItem(ProductServiceTest.STORE_ID, savedProduct, product, false,
      true, true, false, true, false, false);
    verify(this.repository).saveAndFlush(savedProduct);
    verify(attributeService).getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, "warnaId");
    verify(productImageCleanupRepository).findLocationPathByProductCode(product.getProductCode());
    verify(productImageCleanupRepository).saveAll(listArgumentCaptor.capture());
    assertEquals(1, listArgumentCaptor.getValue().size());
  }

  @Test
  public void updateImagesTest() throws Exception {
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setCopy(true);
    productImageEditRequest.setImagePath("/xyz.png");
    productImageEditRequest.setCopyToAllVariantImages(copyImageEditRequest);
    productImageEditRequest.setProductCode(PRODUCT_CODE);
    productImageEditRequest.setNeedRevision(true);
    productImageEditRequest.setActivatedBefore(false);
    Image itemImage = new Image();
    itemImage.setLocationPath(LOCATION_PATH);
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    product.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    product.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    product.getProductItems().get(0).getProductItemImages().get(1).setLocationPath(LOCATION_PATH1);

    this.service.updateImages(STORE_ID, true, Arrays.asList(productImageEditRequest), null, false);

    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCachedWithoutFilteringMainImages(STORE_ID,
        product, false);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    assertEquals(3, product.getProductItems().get(0).getProductItemImages().size());
  }

  @Test
  public void updateImagesAddMainImageTest() throws Exception {
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setAdd(true);
    copyImageEditRequest.setMainImage(true);
    productImageEditRequest.setImagePath("/xyz.png");
    productImageEditRequest.setCopyToAllVariantImages(copyImageEditRequest);
    productImageEditRequest.setProductCode(PRODUCT_CODE);
    Image itemImage = new Image();
    itemImage.setLocationPath(LOCATION_PATH);
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(itemImage, productImage);
    productImage.setCommonImage(true);
    productImage.setMainImages(true);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    ProductImage productImage1 = new ProductImage();
    productImage1.setMarkForDelete(true);
    productImages.add(productImage1);
    when(imageService.getProductImagesByStoreIdAndProductIdCached(STORE_ID, product.getId())).thenReturn(productImages);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    product.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    product.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    product.getProductItems().get(0).getProductItemImages().get(1).setLocationPath(LOCATION_PATH1);

    this.service.updateImages(STORE_ID, true, Arrays.asList(productImageEditRequest), null, false);

    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCachedWithoutFilteringMainImages(STORE_ID,
        product, false);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    assertEquals(3, product.getProductItems().get(0).getProductItemImages().size());
  }

  @Test
  public void updateImagesAddMainImageCombinedUpdateTest() throws Exception {
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setAdd(true);
    copyImageEditRequest.setMainImage(true);
    productImageEditRequest.setImagePath("/xyz.png");
    productImageEditRequest.setCopyToAllVariantImages(copyImageEditRequest);
    productImageEditRequest.setProductCode(PRODUCT_CODE);
    Image itemImage = new Image();
    itemImage.setLocationPath(LOCATION_PATH);
    ProductImage productImage = new ProductImage();
    ProductDTO productDTO = new ProductDTO();
    BeanUtils.copyProperties(itemImage, productImage);
    productImage.setCommonImage(true);
    productImage.setMainImages(true);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    ProductImage productImage1 = new ProductImage();
    productImage1.setMarkForDelete(true);
    productImages.add(productImage1);
    productDTO = ConverterUtil.convertProductToDTO(product);
    when(imageService.getProductImagesByStoreIdAndProductIdCached(STORE_ID, product.getId())).thenReturn(productImages);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
      .collect(toList()));
    product.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    product.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    product.getProductItems().get(0).getProductItemImages().get(1).setLocationPath(LOCATION_PATH1);
    this.service.updateImages(STORE_ID, true, Arrays.asList(productImageEditRequest), productDTO, true);
    assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
    verify(productItemService).removeDeletedProductItemImagesWithoutFilteringMainImages(any());
  }


  @Test
  public void updateImagesAllFailedTest() throws Exception {
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setCopy(true);
    productImageEditRequest.setImagePath(LOCATION_PATH);
    productImageEditRequest.setCopyToAllVariantImages(copyImageEditRequest);
    productImageEditRequest.setProductCode(PRODUCT_CODE);

    Image itemImage = new Image();
    itemImage.setLocationPath(LOCATION_PATH);
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    product.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    product.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    product.getProductItems().get(0).getProductItemImages().get(1).setLocationPath(LOCATION_PATH1);

    this.service.updateImages(STORE_ID, false, Arrays.asList(productImageEditRequest), null, false);

    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCachedWithoutFilteringMainImages(STORE_ID,
        product, false);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
  }

  @Test
  public void updateImagesPartialSuccessTest() throws Exception {
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setCopy(true);
    productImageEditRequest.setImagePath(LOCATION_PATH);
    productImageEditRequest.setCopyToAllVariantImages(copyImageEditRequest);
    productImageEditRequest.setProductCode(PRODUCT_CODE);
    productImageEditRequest.setNeedRevision(true);
    productImageEditRequest.setActivatedBefore(true);
    Image itemImage = new Image();
    itemImage.setLocationPath(LOCATION_PATH);
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    product.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    product.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    product.getProductItems().get(0).getProductItemImages().get(1).setLocationPath(LOCATION_PATH1);
    product.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(true);
    ProductItem productItem = new ProductItem();
    productItem.setSkuCode("ABCD");
    ProductItemImage itemImage1 = new ProductItemImage();
    itemImage1.setLocationPath(LOCATION_PATH_3);
    itemImage1.setSequence(1);
    itemImage1.setMainImages(true);
    productItem.setProductItemImages(Arrays.asList(itemImage1));
    product.getProductItems().add(productItem);
    this.service.updateImages(STORE_ID, false, Arrays.asList(productImageEditRequest), null, false);

    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCachedWithoutFilteringMainImages(STORE_ID,
        product, false);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
    assertEquals(2, product.getProductItems().get(1).getProductItemImages().size());
  }

  @Test
  public void updateImagesPartialSuccessWithMainImageTest() throws Exception {
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setAdd(false);
    copyImageEditRequest.setMainImage(false);
    copyImageEditRequest.setCopy(true);
    copyImageEditRequest.setMarkForDelete(true);
    productImageEditRequest.setImagePath(LOCATION_PATH);
    productImageEditRequest.setCopyToAllVariantImages(copyImageEditRequest);
    productImageEditRequest.setProductCode(PRODUCT_CODE);
    productImageEditRequest.setNeedRevision(true);
    productImageEditRequest.setActivatedBefore(true);
    Image itemImage = new Image();
    itemImage.setLocationPath(LOCATION_PATH);
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
      .collect(toList()));
    product.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    product.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    product.getProductItems().get(0).getProductItemImages().get(1).setLocationPath(LOCATION_PATH1);
    product.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(true);
    ProductItem productItem = new ProductItem();
    productItem.setSkuCode("ABCD");
    ProductItemImage itemImage1 = new ProductItemImage();
    itemImage1.setLocationPath(LOCATION_PATH_3);
    itemImage1.setSequence(1);
    itemImage1.setMainImages(true);
    productItem.setProductItemImages(Arrays.asList(itemImage1));
    product.getProductItems().add(productItem);
    this.service.updateImages(STORE_ID, false, Arrays.asList(productImageEditRequest), null, false);

    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCachedWithoutFilteringMainImages(STORE_ID,
      product, false);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
    assertEquals(2, product.getProductItems().get(1).getProductItemImages().size());
  }

  @Test
  public void updateImagesPartialSuccessRevertTest() throws Exception {
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setAdd(true);
    productImageEditRequest.setImagePath(LOCATION_PATH);
    productImageEditRequest.setCopyToAllVariantImages(copyImageEditRequest);
    productImageEditRequest.setProductCode(PRODUCT_CODE);
    productImageEditRequest.setNeedRevision(true);
    productImageEditRequest.setActivatedBefore(true);
    Image itemImage = new Image();
    itemImage.setLocationPath(LOCATION_PATH);
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    product.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    product.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    product.getProductItems().get(0).getProductItemImages().get(1).setLocationPath(LOCATION_PATH1);
    ProductItem productItem = new ProductItem();
    productItem.setSkuCode("ABCD");
    ProductItemImage itemImage1 = new ProductItemImage();
    itemImage1.setLocationPath(LOCATION_PATH_3);
    itemImage1.setSequence(1);
    itemImage1.setMainImages(true);
    productItem.setProductItemImages(Arrays.asList(itemImage1));
    product.getProductItems().add(productItem);
    this.service.updateImages(STORE_ID, false, Arrays.asList(productImageEditRequest), null, false);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCachedWithoutFilteringMainImages(STORE_ID,
        product, false);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
    assertEquals(2, product.getProductItems().get(1).getProductItemImages().size());
  }

  @Test
  public void evictCacheOnMarkForNeedRevision() throws Exception {
    this.service.evictCacheOnMarkForNeedRevision(product,Boolean.TRUE,productSalesCategoryMapping);
    verify(applicationCacheServiceBean, times(2)).evictProductCacheByStoreIdAndProductCode(anyString(),
      anyString());
    verify(applicationCacheServiceBean, times(2)).evictProductItemsCacheByStoreIdAndProductId(anyString(),
      anyString());
    verify(applicationCacheServiceBean, times(2)).evictProductImagesCacheByStoreIdAndProductId(anyString(),
      anyString());
    verify(applicationCacheServiceBean).evictProductAttributesCacheByStoreIdAndProductId(anyString(),
      anyString());
    verify(applicationCacheServiceBean).evictProductCategoriesCacheByStoreIdAndProductId(anyString(),
      anyString());
    verify(applicationCacheServiceBean, times(2)).evictProductItemImagesCacheByStoreIdAndProductId(anyString(),
      anyString());
    verify(applicationCacheServiceBean).evictProductItemAttributeValuesCacheByStoreIdAndProductId(anyString(),
      anyString());
    verify(this.domainEventPublisherService).publishProductChangeCategory(eq(product), any(ProductSalesCategoryMapping.class),
      eq(true), eq(false), eq(false), eq(false), eq(new HashSet<>()));
  }

  @Test
  public void updateImagesForVariantTest() throws Exception {
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    ItemImageEditRequest copyImageEditRequest = new ItemImageEditRequest();
    copyImageEditRequest.setCopy(true);
    copyImageEditRequest.setItemCode(SKU_CODE);
    productImageEditRequest.setImagePath("/xyz.png");
    productImageEditRequest.setProductItems(Arrays.asList(copyImageEditRequest));
    productImageEditRequest.setProductCode(PRODUCT_CODE);
    productImageEditRequest.setNeedRevision(true);
    Image itemImage = new Image();
    itemImage.setLocationPath(LOCATION_PATH);
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductItems(productItems.stream().filter(productItem -> !productItem.isMarkForDelete())
        .collect(toList()));
    product.getProductItems().get(0).setSkuCode(SKU_CODE);
    product.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    product.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    product.getProductItems().get(0).getProductItemImages().get(1).setLocationPath(LOCATION_PATH1);

    this.service.updateImages(STORE_ID, true, Arrays.asList(productImageEditRequest), null, false);

    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCachedWithoutFilteringMainImages(STORE_ID,
        product, false);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    assertEquals(3, product.getProductItems().get(0).getProductItemImages().size());
  }

  @Test
  public void updateImagesExceptionTest() throws Exception {
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    ItemImageEditRequest copyImageEditRequest = new ItemImageEditRequest();
    copyImageEditRequest.setCopy(true);
    copyImageEditRequest.setItemCode(SKU_CODE);
    productImageEditRequest.setImagePath("/xyz.png");
    productImageEditRequest.setProductItems(Arrays.asList(copyImageEditRequest));
    productImageEditRequest.setProductCode(PRODUCT_CODE);

    Image itemImage = new Image();
    itemImage.setLocationPath(LOCATION_PATH);
    ProductImage productImage = new ProductImage();
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductItems(
        productItems.stream().filter(productItem -> !productItem.isMarkForDelete()).collect(toList()));
    product.getProductItems().get(0).setSkuCode(SKU_CODE);
    product.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    product.getProductItems().get(0).getProductItemImages().get(0).setMainImages(false);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    product.getProductItems().get(0).getProductItemImages().get(1).setLocationPath(LOCATION_PATH1);
    try {
      this.service.updateImages(STORE_ID, false, Arrays.asList(productImageEditRequest), null,
        false);
    } catch (Exception e) {

    } finally {
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
      verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
      verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
      verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID_1);
      verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCachedWithoutFilteringMainImages(STORE_ID,
          product, false);
    }
  }

  /*
  ProductImage [isMainImages=true, locationPath=MTA-0859726/puma_edited_product_name_artmjjphgq_raghav_sb_1_full01_maeb6fy8.jpeg, sequence=0, active=false, originalImage=true, hashCode=4219e0399bdc306ac1bafc06ce732f3e, superClass=GdnBaseEntity(id=3ff55662-0c6d-44ed-aafc-3759f150b137, storeId=10001, version=0, createdDate=2022-03-09 22:09:43.887, createdBy=raghav.agarwal@gdn-commerce.com, updatedDate=2022-03-09 22:09:43.887, updatedBy=raghav.agarwal@gdn-commerce.com, markForDelete=false)
  ProductImage [isMainImages=true, locationPath=/MTA-0859726/resize/puma_edited_product_name_artmjjphgq_raghav_sb_1_full01_maeb6fy8.jpg, sequence=0, active=false, originalImage=false, hashCode=4046f38ea11094908343e52172732e91, superClass=GdnBaseEntity(id=2a0ae2ad-a4b6-4e39-bba0-49030317afc6, storeId=10001, version=0, createdDate=2022-03-09 22:09:51.071, createdBy=System, updatedDate=2022-03-09 22:09:51.071, updatedBy=System, markForDelete=false)
  ProductImage [isMainImages=true, locationPath=/MTA-0859726/resize/puma_edited_product_name_artmjjphgq_raghav_sb_1_full01_maeb6fy8.jpg, sequence=0, active=false, originalImage=false, hashCode=4046f38ea11094908343e52172732e91, superClass=GdnBaseEntity(id=9c63e4ab-d9da-4e11-9337-4e87366fb893, storeId=10001, version=1, createdDate=2022-03-09 22:09:44.69, createdBy=SYSTEM, updatedDate=2022-03-09 22:09:51.088, updatedBy=System, markForDelete=true)
  */

  @Test
  public void setProductImagesCachedTest() {
    ProductImage productImage1 = new ProductImage();
    productImage1.setMainImages(true);
    productImage1.setLocationPath("MTA-0859726/puma_edited_product_name_artmjjphgq_raghav_sb_1_full01_maeb6fy8.jpeg");
    productImage1.setSequence(0);
    productImage1.setActive(false);
    productImage1.setOriginalImage(true);
    productImage1.setHashCode("4219e0399bdc306ac1bafc06ce732f3e");
    productImage1.setId("3ff55662-0c6d-44ed-aafc-3759f150b137");
    productImage1.setMarkForDelete(false);

    ProductImage productImage2 = new ProductImage();
    productImage2.setMainImages(true);
    productImage2.setLocationPath("/MTA-0859726/resize/puma_edited_product_name_artmjjphgq_raghav_sb_1_full01_maeb6fy8.jpg");
    productImage2.setSequence(0);
    productImage2.setActive(false);
    productImage2.setOriginalImage(false);
    productImage2.setHashCode("4046f38ea11094908343e52172732e91");
    productImage2.setId("2a0ae2ad-a4b6-4e39-bba0-49030317afc6");
    productImage2.setMarkForDelete(false);

    ProductImage productImage3 = new ProductImage();
    productImage3.setMainImages(true);
    productImage3.setLocationPath("/MTA-0859726/resize/puma_edited_product_name_artmjjphgq_raghav_sb_1_full01_maeb6fy8.jpg");
    productImage3.setSequence(0);
    productImage3.setActive(false);
    productImage3.setOriginalImage(false);
    productImage3.setHashCode("4046f38ea11094908343e52172732e91");
    productImage3.setId("9c63e4ab-d9da-4e11-9337-4e87366fb893");
    productImage3.setMarkForDelete(true);

    Product product = new Product();

    List<ProductImage> productImages = new ArrayList<>();
    productImages.add(productImage1);
    productImages.add(productImage2);
    productImages.add(productImage3);

    when(imageService.getProductImagesByStoreIdAndProductIdCached(STORE_ID, product.getId())).thenReturn(productImages);

    service.setProductImagesCached(STORE_ID, product, false);

    verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, product.getId());
  }

  @Test
  public void getListOfProductCodesEligibleForDeletionTest() {
    Date date = new Date();

    Mockito.when(
            repository.findByStoreIdAndUpdatedDateLessThanAndMarkForDeleteTrueAndPickedForDeletionFalse(STORE_ID, date, 1))
        .thenReturn(Arrays.asList(PRODUCT_CODE));

    service.getListOfProductCodesEligibleForDeletion(STORE_ID, date, 1);

    Mockito.verify(repository)
        .findByStoreIdAndUpdatedDateLessThanAndMarkForDeleteTrueAndPickedForDeletionFalse(STORE_ID, date, 1);
  }

  @Test
  public void updatePickedForDeletionFlagByProductCodeTest() {
    Mockito.doNothing().when(repository)
        .updatePickedForDeletionByProductCode(STORE_ID, Arrays.asList(PRODUCT_CODE), true);

    service.updatePickedForDeletionFlagByProductCode(STORE_ID, Arrays.asList(PRODUCT_CODE), true);

    Mockito.verify(repository).updatePickedForDeletionByProductCode(STORE_ID, Arrays.asList(PRODUCT_CODE), true);
  }

  @Test
  public void setProductAndItemImagesCachedTest() {
    ProductImage productImage1 = new ProductImage();
    productImage1.setMainImages(true);
    productImage1.setLocationPath("MTA-0859726/puma_edited_product_name_artmjjphgq_raghav_sb_1_full01_maeb6fy8.jpeg");
    productImage1.setSequence(0);
    productImage1.setActive(false);
    productImage1.setOriginalImage(true);
    productImage1.setHashCode("4219e0399bdc306ac1bafc06ce732f3e");
    productImage1.setId("3ff55662-0c6d-44ed-aafc-3759f150b137");
    productImage1.setMarkForDelete(false);

    ProductImage productImage2 = new ProductImage();
    productImage2.setMainImages(true);
    productImage2.setLocationPath("/MTA-0859726/resize/puma_edited_product_name_artmjjphgq_raghav_sb_1_full01_maeb6fy8.jpg");
    productImage2.setSequence(0);
    productImage2.setActive(false);
    productImage2.setOriginalImage(false);
    productImage2.setHashCode("4046f38ea11094908343e52172732e91");
    productImage2.setId("2a0ae2ad-a4b6-4e39-bba0-49030317afc6");
    productImage2.setMarkForDelete(false);

    ProductImage productImage3 = new ProductImage();
    productImage3.setMainImages(true);
    productImage3.setLocationPath("/MTA-0859726/resize/puma_edited_product_name_artmjjphgq_raghav_sb_1_full01_maeb6fy8.jpg");
    productImage3.setSequence(0);
    productImage3.setActive(false);
    productImage3.setOriginalImage(false);
    productImage3.setHashCode("4046f38ea11094908343e52172732e91");
    productImage3.setId("9c63e4ab-d9da-4e11-9337-4e87366fb893");
    productImage3.setMarkForDelete(true);

    Product product = new Product();

    List<ProductImage> productImages = new ArrayList<>();
    productImages.add(productImage1);
    productImages.add(productImage2);
    productImages.add(productImage3);

    when(imageService.getProductImagesByStoreIdAndProductIdCached(STORE_ID, product.getId())).thenReturn(productImages);
    doNothing().when(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);

    service.setProductAndItemImagesCached(STORE_ID, product, false);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, product.getId());
    verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
  }

  @Test
  public void evictProductAndItemImageCacheTest() {
    Product product = new Product();
    product.setStoreId(STORE_ID);
    product.setId(PRODUCT_ID);

    Mockito.doNothing().when(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.doNothing().when(applicationCacheServiceBean).evictProductItemImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);

    service.evictProductAndItemImageCache(product);

    Mockito.verify(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(applicationCacheServiceBean).evictProductItemImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void evictProductAndItemCacheTest() {
    Product product = new Product();
    product.setStoreId(STORE_ID);
    product.setId(PRODUCT_ID);

    Mockito.doNothing().when(applicationCacheServiceBean).evictProductItemsCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);

    service.evictProductItemCache(product);

    Mockito.verify(applicationCacheServiceBean).evictProductItemsCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void evictCacheAndPublishForProductItemUpdateTest() throws Exception {
    Product product = this.createAdvancedProduct();
    List<String> attributeIds = new ArrayList<>();
    Product savedProduct = getProductWithDifferentCategory(product, attributeIds);
    when(this.repository.findById(savedProduct.getId())).thenReturn(Optional.of(savedProduct));
    Category category2 = createDefaultCategory();
    ProductSalesCategoryMapping productSalesCategoryMapping = new ProductSalesCategoryMapping(Collections.singletonList(SALES_CATEGORY_1),
        Collections.singletonList(SALES_CATEGORY_2), Collections.singletonList(SALES_CATEGORY_3), new ArrayList<>(),
        new ArrayList<>(),true);
    category2.setId(CATEGORY_ID_2);
    category2.setCategoryCode(CATEGORY_CODE_2);
    ProductItem productItem1 = new ProductItem();
    productItem1.setHash(HASH.getBytes());
    productItem1.setVatApplicable(true);
    productItem.setVatApplicable(false);
    productItem.setSkuCode(SKU_CODE);
    productItem1.setSkuCode(SKU_CODE);
    product.getProductItems().get(0).setContentChanged(true);
    product.getProductItems().get(0).setViewable(true);
    product.getProductItems().get(0).setActivated(true);
    when(this.domainEventPublisherService.publishProduct((Product) any())).thenReturn(null);
    Map<ProductItem, String> productItemMap = new HashMap<>();
    productItemMap.put(productItem,String.valueOf(productItem.getVatApplicable()));
    productItemMap.put(productItem1,String.valueOf(productItem.getVatApplicable()));
    when(this.repository.findById(savedProduct.getId())).thenReturn(Optional.of(savedProduct));
    when(categoryReferenceService.getSalesCategoryReferenceByMasterCategory(CATEGORY_ID_1, CATEGORY_ID_2, false))
      .thenReturn(productSalesCategoryMapping);
    when(productImageCleanupRepository.findLocationPathByProductCode(product.getProductCode())).thenReturn(new ArrayList<>());
    this.service.evictCacheAndPublishForProductItemUpdate(STORE_ID,savedProduct,product
      , false, false, false,productItemMap, false, new ProductAndItemLevelUpdatesDTO(), new HashSet<>(),
        new HashSet<>());
    verify(applicationCacheServiceBean).evictProductCacheByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(applicationCacheServiceBean, times(2)).evictProductCacheByStoreIdAndProductCode(anyString(),
      anyString());
    verify(applicationCacheServiceBean).evictProductItemsCacheByStoreIdAndProductId(anyString(),
      anyString());
    verify(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(anyString(),
      anyString());
    verify(applicationCacheServiceBean).evictProductAttributesCacheByStoreIdAndProductId(anyString(),
      anyString());
    verify(applicationCacheServiceBean).evictProductCategoriesCacheByStoreIdAndProductId(anyString(),
      anyString());
    verify(applicationCacheServiceBean).evictProductItemImagesCacheByStoreIdAndProductId(anyString(),
      anyString());
    verify(applicationCacheServiceBean).evictProductItemAttributeValuesCacheByStoreIdAndProductId(anyString(),
      anyString());
    verify(categoryReferenceService).getSalesCategoryReferenceByMasterCategory(CATEGORY_ID_1, CATEGORY_ID_2, false);
    verify(domainEventPublisherService).publishVatApplicableUpdateEvent(SKU_CODE, false);
    verify(domainEventPublisherService).publishVatApplicableExternalHistoryEvent(any(),
        any(),any(),any(),any(),any(),any(),any());
    verify(this.domainEventPublisherService).publishProduct(eq(savedProduct),
      eq(productSalesCategoryMapping),
      eq(false), eq(false),eq(true), eq(false), eq(false), eq(new ProductAndItemLevelUpdatesDTO()), eq(new HashSet<>()),
        eq(new HashSet<>()));
  }

  @Test
  public void evictCacheAndPublishForProductItemUpdateOnlyVatChangeTest() throws Exception {
    Product product = this.createAdvancedProduct();
    List<String> attributeIds = new ArrayList<>();
    Product savedProduct = getProductWithDifferentCategory(product, attributeIds);
    when(this.repository.findById(savedProduct.getId())).thenReturn(Optional.of(savedProduct));
    Category category2 = createDefaultCategory();
    ProductSalesCategoryMapping productSalesCategoryMapping =
        new ProductSalesCategoryMapping(Collections.singletonList(SALES_CATEGORY_1),
            Collections.singletonList(SALES_CATEGORY_2), Collections.singletonList(SALES_CATEGORY_3),
            new ArrayList<>(), new ArrayList<>(),true);
    category2.setId(CATEGORY_ID_2);
    category2.setCategoryCode(CATEGORY_CODE_2);
    ProductItem productItem1 = new ProductItem();
    productItem1.setHash(HASH.getBytes());
    productItem1.setVatApplicable(true);
    productItem.setVatApplicable(false);
    productItem.setSkuCode(SKU_CODE);
    productItem1.setSkuCode(SKU_CODE);
    product.getProductItems().get(0).setContentChanged(true);
    product.getProductItems().get(0).setViewable(true);
    product.getProductItems().get(0).setActivated(true);
    when(this.domainEventPublisherService.publishProduct((Product) any())).thenReturn(
      null);
    Map<ProductItem, String> productItemMap = new HashMap<>();
    when(this.repository.findById(savedProduct.getId())).thenReturn(Optional.of(savedProduct));
    when(categoryReferenceService.getSalesCategoryReferenceByMasterCategory(CATEGORY_ID_1,
      CATEGORY_ID_2, false)).thenReturn(productSalesCategoryMapping);
    when(productImageCleanupRepository.findLocationPathByProductCode(product.getProductCode())).thenReturn(new ArrayList<>());
    this.service.evictCacheAndPublishForProductItemUpdate(STORE_ID, savedProduct, product, false,
      true, false, productItemMap, false, new ProductAndItemLevelUpdatesDTO(), new HashSet<>(), new HashSet<>());
    verify(applicationCacheServiceBean).evictProductCacheByStoreIdAndProductCode(STORE_ID,
      PRODUCT_CODE);
    verify(applicationCacheServiceBean, times(2)).evictProductCacheByStoreIdAndProductCode(anyString(),
      anyString());
    verify(applicationCacheServiceBean).evictProductItemsCacheByStoreIdAndProductId(anyString(),
      anyString());
    verify(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(anyString(),
      anyString());
    verify(applicationCacheServiceBean).evictProductAttributesCacheByStoreIdAndProductId(anyString(),
      anyString());
    verify(applicationCacheServiceBean).evictProductCategoriesCacheByStoreIdAndProductId(anyString(),
      anyString());
    verify(applicationCacheServiceBean).evictProductItemImagesCacheByStoreIdAndProductId(anyString(),
      anyString());
    verify(applicationCacheServiceBean).evictProductItemAttributeValuesCacheByStoreIdAndProductId(
      anyString(), anyString());
    verify(categoryReferenceService).getSalesCategoryReferenceByMasterCategory(CATEGORY_ID_1,
      CATEGORY_ID_2, false);
  }

  @Test
  public void setProductItemIdForNewlyAddedItems() {
    String id = UUID.randomUUID().toString();
    productItem.setSkuCode(SKU_CODE);
    productItem.setId(id);
    productItem.setGeneratedItemName(GENERATED_ITEM_NAME);
    NewlySavedItemResponse newlySavedItemResponse = new NewlySavedItemResponse();
    newlySavedItemResponse.setItemCode(SKU_CODE);
    newlySavedItemResponse.setGeneratedItemName(GENERATED_ITEM_NAME);
    Mockito.when(productItemService.getProductItemsByStoreIdAndProductIdCached(any(), any()))
        .thenReturn(Arrays.asList(productItem));

    service.setProductItemIdForNewlyAddedItems(Arrays.asList(newlySavedItemResponse), product);

    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(any(), any());
    assertEquals(id, newlySavedItemResponse.getProductItemId());
  }

  @Test
  public void setProductItemIdForNewlyAddedWithDifferntItemNameItems() {
    String id = UUID.randomUUID().toString();
    productItem.setSkuCode(SKU_CODE);
    productItem.setId(id);
    productItem.setGeneratedItemName(GENERATED_ITEM_NAME.concat("-red"));
    NewlySavedItemResponse newlySavedItemResponse = new NewlySavedItemResponse();
    newlySavedItemResponse.setItemCode(SKU_CODE);
    newlySavedItemResponse.setGeneratedItemName(GENERATED_ITEM_NAME);
    Mockito.when(productItemService.getProductItemsByStoreIdAndProductIdCached(any(), any()))
      .thenReturn(Arrays.asList(productItem));

    service.setProductItemIdForNewlyAddedItems(Arrays.asList(newlySavedItemResponse), product);

    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(any(), any());
    Assertions.assertNull(newlySavedItemResponse.getId());
  }

  @Test
  public void updateProductBrandDataTest() throws Exception {
    product.setProductItems(
        productItems.stream().filter(productItem -> !productItem.isMarkForDelete()).collect(toList()));
    product.setProductAttributes(new ArrayList<>());
    product.getProductAttributes().add(brandProductAttribute);
    productBrandUpdateDTO.setBusinessPartnerCodes(null);
    when(brandService.findByBrandCodeCached(eq(STORE_ID), eq(BRAND_ATTRIBUTE_CODE))).thenReturn(
        oldBrand);
    when(brandService.findByBrandCodeCached(eq(STORE_ID), eq(NEW_BRAND_CODE))).thenReturn(newBrand);
    service.updateProductBrand(STORE_ID, productBrandUpdateDTO);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(DEFAULT_STORE_ID,
        product, false);
    verify(brandService).findByBrandCodeCached(STORE_ID, BRAND_ATTRIBUTE_CODE);
    verify(brandService).findByBrandCodeCached(STORE_ID, NEW_BRAND_CODE);
    verify(predefinedAllowedAttributeValueService).findByStoreIdAndPredefinedAllowedAttributeCode(STORE_ID,
        NEW_BRAND_CODE);
    verify(repository).findById(PRODUCT_ID);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    assertEquals(NEW_BRAND_NAME, productArgumentCaptor.getAllValues().get(0).getBrand());
    assertEquals(PRODUCT_CODE, productArgumentCaptor.getAllValues().get(0).getProductCode());
  }

  @Test
  public void updateProductBrandDataTest_brandAuthCheckSuccess() throws Exception {
    product.setProductItems(
        productItems.stream().filter(productItem -> !productItem.isMarkForDelete()).collect(toList()));
    product.setProductAttributes(new ArrayList<>());
    product.getProductAttributes().add(brandProductAttribute);
    productBrandUpdateDTO.setBusinessPartnerCodes(Set.of(SELLER_CODE));
    when(brandService.findByBrandCodeCached(eq(STORE_ID), eq(BRAND_ATTRIBUTE_CODE))).thenReturn(
        oldBrand);
    when(brandService.findByBrandCodeCached(eq(STORE_ID), eq(NEW_BRAND_CODE))).thenReturn(newBrand);
    when(brandAuthorisationServiceBean.checkBrandAuthBySellerCode(STORE_ID, SELLER_CODE,
        NEW_BRAND_CODE, false)).thenReturn(true);
    service.updateProductBrand(STORE_ID, productBrandUpdateDTO);
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(DEFAULT_STORE_ID,
        product, false);
    verify(brandService).findByBrandCodeCached(STORE_ID, BRAND_ATTRIBUTE_CODE);
    verify(brandService).findByBrandCodeCached(STORE_ID, NEW_BRAND_CODE);
    verify(predefinedAllowedAttributeValueService).findByStoreIdAndPredefinedAllowedAttributeCode(STORE_ID,
        NEW_BRAND_CODE);
    verify(brandAuthorisationServiceBean).checkBrandAuthBySellerCode(STORE_ID, SELLER_CODE,
        NEW_BRAND_CODE, false);
    verify(repository).findById(PRODUCT_ID);
    verify(repository).saveAndFlush(productArgumentCaptor.capture());
    assertEquals(NEW_BRAND_NAME, productArgumentCaptor.getAllValues().get(0).getBrand());
    assertEquals(PRODUCT_CODE, productArgumentCaptor.getAllValues().get(0).getProductCode());
  }

  @Test
  public void updateProductBrandDataTest_brandAuthCheckFailed() throws Exception {
    product.setProductItems(
        productItems.stream().filter(productItem -> !productItem.isMarkForDelete()).collect(toList()));
    product.setProductAttributes(new ArrayList<>());
    product.getProductAttributes().add(brandProductAttribute);
    Set<String> set = new LinkedHashSet<>();
    set.add(SELLER_CODE);
    set.add(SELLER_CODE+"2");
    productBrandUpdateDTO.setBusinessPartnerCodes(set);
    when(brandService.findByBrandCodeCached(eq(STORE_ID), eq(BRAND_ATTRIBUTE_CODE))).thenReturn(
        oldBrand);
    when(brandService.findByBrandCodeCached(eq(STORE_ID), eq(NEW_BRAND_CODE))).thenReturn(newBrand);
    when(brandAuthorisationServiceBean.checkBrandAuthBySellerCode(STORE_ID, SELLER_CODE,
        NEW_BRAND_CODE, false)).thenReturn(true);
    when(brandAuthorisationServiceBean.checkBrandAuthBySellerCode(STORE_ID, SELLER_CODE+"2",
        NEW_BRAND_CODE, false)).thenReturn(false);
    try {
      service.updateProductBrand(STORE_ID, productBrandUpdateDTO);
    } catch (Exception ex) {
      Assertions.assertEquals(
          "Can not process invalid input data :Brand Authorisation mappings was not found for "
              + "Seller and Brand Code : sellerCode2 newBrandCode",
          ex.getMessage());
    }
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(brandService).findByBrandCodeCached(STORE_ID, BRAND_ATTRIBUTE_CODE);
    verify(brandService).findByBrandCodeCached(STORE_ID, NEW_BRAND_CODE);
    verify(brandAuthorisationServiceBean).checkBrandAuthBySellerCode(STORE_ID, SELLER_CODE,
        NEW_BRAND_CODE, false);
    verify(brandAuthorisationServiceBean).checkBrandAuthBySellerCode(STORE_ID, SELLER_CODE+"2",
        NEW_BRAND_CODE, false);
  }

  @Test
  public void updateProductBrandDataBrandNotFoundTest() throws Exception {
    when(brandService.findByBrandCodeCached(STORE_ID, BRAND_ATTRIBUTE_CODE)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> service.updateProductBrand(STORE_ID, productBrandUpdateDTO));
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
      verify(brandService).findByBrandCodeCached(STORE_ID, BRAND_ATTRIBUTE_CODE);
      verify(brandService).findByBrandCodeCached(STORE_ID, NEW_BRAND_CODE);
    }
  }


  @Test
  public void updateProductBrandDataProductNotFoundTest() throws Exception {
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> service.updateProductBrand(STORE_ID, productBrandUpdateDTO));
    } finally {
      verify(applicationContext).getBean(ProductService.class);
      verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void testUpdateProductContentAndImages_withProductDetailChanges() throws Exception {
    Product oldProduct = generateProductRequest();
    oldProduct.setVersion(1L);
    oldProduct.setMarkForDelete(false);
    Product newProduct = generateProductRequest();
    newProduct.setStoreId(STORE_ID);
    newProduct.setMarkForDelete(false);
    newProduct.getProductItems().remove(1);
    oldProduct.getProductItems().remove(1);
    newProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
      .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(false));
    oldProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
      .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(false));
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setValue("Value");
    productItemAttributeValue.setAttribute(new Attribute());
    productItemAttributeValue.getAttribute().setName("Brand");
    List<ProductItemAttributeValue> productItemAttributeValueList =
      new ArrayList<ProductItemAttributeValue>();
    productItemAttributeValueList.add(productItemAttributeValue);
    oldProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
    });
    newProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
    });
    oldProduct.setBrand("Brand");
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setCopy(true);
    productImageEditRequest.setImagePath("/xyz.png");
    productImageEditRequest.setCopyToAllVariantImages(copyImageEditRequest);
    productImageEditRequest.setProductCode(PRODUCT_CODE);
    productImageEditRequest.setNeedRevision(true);
    productImageEditRequest.setActivatedBefore(false);
    Image itemImage = new Image();
    itemImage.setMainImages(true);
    itemImage.setLocationPath(LOCATION_PATH);
    itemImage.setMarkForDelete(false);
    ProductImage productImage = new ProductImage();
    productImage.setMarkForDelete(false);
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    when(
      productService.getProductByStoreIdAndProductCodeCached(anyString(), anyString())).thenReturn(
      oldProduct);
    oldProduct.setProductCategories(productCategories);
    newProduct.setProductImages(productImages);
    EditProductDetailRequest editProductDetailRequest = new EditProductDetailRequest();
    productRequest.setPristineCategory(true);
    productRequest.setScoreUpdated(true);
    productRequest.getProductItems().stream()
      .forEach(t -> t.setImages(Collections.singletonList(itemImage)));
    oldProduct.setProductImages(productImages);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0)
      .setLocationPath(LOCATION_PATH);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1)
      .setLocationPath(LOCATION_PATH1);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    newProduct.setProductImages(productImages);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    newProduct.getProductItems().get(0).getProductItemImages().get(0)
      .setLocationPath(LOCATION_PATH);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    newProduct.getProductItems().get(0).getProductItemImages().get(1)
      .setLocationPath(LOCATION_PATH1);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    editProductDetailRequest.setProductRequest(productRequest);
    productImageEditRequest.setProductCode(productRequest.getProductCode());
    editProductDetailRequest.setProductImageEditRequests(
      Collections.singletonList(productImageEditRequest));
    product.setProductItems(productItems);
    Mockito.when(repository.saveAndFlush(any())).thenReturn(product);
    ProductDetailEditDTO result =
      service.updateProductContentAndImages(STORE_ID, newProduct, editProductDetailRequest,
        new ArrayList<>());
    Mockito.verify(repository, times(1)).saveAndFlush(any(Product.class));
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
      anyString());
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(anyString(),
      anyString());
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(anyString(), any(),
      anyBoolean());
    verify(categoryService, times(2)).getCategoryByStoreIdAndIdCached(anyString(), anyString());
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(anyString(), anyString());
    verify(productItemService).removeDeletedProductItemImagesWithoutFilteringMainImages(any());
    verify(applicationContext).getBean(ProductService.class);
  }

  @Test
  public void testUpdateProductContentAndImages_withProductDetailChangesForNewlyAddedVarinats() throws Exception {
    ReflectionTestUtils.setField(service, "processVariantImageNewlyAddedItems", true);
    Product oldProduct = generateProductRequest();
    oldProduct.setVersion(1L);
    oldProduct.setMarkForDelete(false);
    Product newProduct = generateProductRequest();
    newProduct.setStoreId(STORE_ID);
    newProduct.setMarkForDelete(false);
    newProduct.getProductItems().remove(1);
    oldProduct.getProductItems().remove(1);
    newProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
      .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(false));
    oldProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
      .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(false));
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setValue("Value");
    productItemAttributeValue.setAttribute(new Attribute());
    productItemAttributeValue.getAttribute().setName("Brand");
    List<ProductItemAttributeValue> productItemAttributeValueList =
      new ArrayList<ProductItemAttributeValue>();
    productItemAttributeValueList.add(productItemAttributeValue);
    oldProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
    });
    newProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
      productItem1.setNewlyAddedItem(true);
      productItem1.setId(null);
    });
    oldProduct.setBrand("Brand");
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setCopy(true);
    productImageEditRequest.setImagePath("/xyz.png");
    productImageEditRequest.setCopyToAllVariantImages(copyImageEditRequest);
    productImageEditRequest.setProductCode(PRODUCT_CODE);
    productImageEditRequest.setNeedRevision(true);
    productImageEditRequest.setActivatedBefore(false);
    Image itemImage = new Image();
    itemImage.setMainImages(true);
    itemImage.setLocationPath(LOCATION_PATH);
    itemImage.setMarkForDelete(false);
    ProductImage productImage = new ProductImage();
    productImage.setMarkForDelete(false);
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    when(
      productService.getProductByStoreIdAndProductCodeCached(anyString(), anyString())).thenReturn(
      oldProduct);
    oldProduct.setProductCategories(productCategories);
    newProduct.setProductImages(productImages);
    EditProductDetailRequest editProductDetailRequest = new EditProductDetailRequest();
    productRequest.setPristineCategory(true);
    productRequest.setScoreUpdated(true);
    productRequest.getProductItems().stream()
      .forEach(t -> t.setImages(Collections.singletonList(itemImage)));
    oldProduct.setProductImages(productImages);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0)
      .setLocationPath(LOCATION_PATH);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1)
      .setLocationPath("/xyz.png");
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    newProduct.setProductImages(productImages);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    newProduct.getProductItems().get(0).getProductItemImages().get(0)
      .setLocationPath(LOCATION_PATH);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    newProduct.getProductItems().get(0).getProductItemImages().get(1)
      .setLocationPath("/xyz.png");
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    editProductDetailRequest.setProductRequest(productRequest);
    productImageEditRequest.setProductCode(productRequest.getProductCode());
    editProductDetailRequest.setProductImageEditRequests(
      Collections.singletonList(productImageEditRequest));
    product.setProductItems(productItems);
    oldProduct.getProductItems().forEach(productItem1 -> productItem1.setNewlyAddedItem(true));
    newProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> productItem1.setGeneratedItemName(ITEM_NAME));
    oldProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> productItem1.setGeneratedItemName(ITEM_NAME));
    oldProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> {
      productItem1.setGeneratedItemName(ITEM_NAME);
      productItem1.setId(null);
    });
    Mockito.when(repository.saveAndFlush(any())).thenReturn(product);
    ProductDetailEditDTO result =
      service.updateProductContentAndImages(STORE_ID, newProduct, editProductDetailRequest,
        new ArrayList<>());
    Mockito.verify(repository, times(1)).saveAndFlush(any(Product.class));
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
      anyString());
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(anyString(),
      anyString());
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(anyString(), any(),
      anyBoolean());
    verify(categoryService, times(2)).getCategoryByStoreIdAndIdCached(anyString(), anyString());
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(anyString(), anyString());
    verify(productItemService).removeDeletedProductItemImagesWithoutFilteringMainImages(any());
    verify(applicationContext).getBean(ProductService.class);
  }

  @Test
  public void testUpdateProductContentAndImages_withProductDetailChangesForNewlyAddedVarinatsEmptyImageTest() throws Exception {
    ReflectionTestUtils.setField(service, "processVariantImageNewlyAddedItems", true);
    Product oldProduct = generateProductRequest();
    oldProduct.setVersion(1L);
    oldProduct.setMarkForDelete(false);
    Product newProduct = generateProductRequest();
    newProduct.setStoreId(STORE_ID);
    newProduct.setMarkForDelete(false);
    newProduct.getProductItems().remove(1);
    oldProduct.getProductItems().remove(1);
    newProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
      .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(true));
    oldProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
      .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(true));
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setValue("Value");
    productItemAttributeValue.setAttribute(new Attribute());
    productItemAttributeValue.getAttribute().setName("Brand");
    List<ProductItemAttributeValue> productItemAttributeValueList =
      new ArrayList<ProductItemAttributeValue>();
    productItemAttributeValueList.add(productItemAttributeValue);
    oldProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
    });
    newProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
      productItem1.setNewlyAddedItem(true);
      productItem1.setId(null);
    });
    oldProduct.setBrand("Brand");
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setCopy(true);
    copyImageEditRequest.setMainImage(true);
    copyImageEditRequest.setMarkForDelete(false);
    productImageEditRequest.setImagePath("/xyz.png");
    productImageEditRequest.setCopyToAllVariantImages(copyImageEditRequest);
    productImageEditRequest.setProductCode(PRODUCT_CODE);
    productImageEditRequest.setNeedRevision(true);
    productImageEditRequest.setActivatedBefore(false);
    Image itemImage = new Image();
    itemImage.setMainImages(true);
    itemImage.setLocationPath(LOCATION_PATH);
    itemImage.setMarkForDelete(false);
    ProductImage productImage = new ProductImage();
    productImage.setMarkForDelete(false);
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    when(
      productService.getProductByStoreIdAndProductCodeCached(anyString(), anyString())).thenReturn(
      oldProduct);
    oldProduct.setProductCategories(productCategories);
    newProduct.setProductImages(productImages);
    EditProductDetailRequest editProductDetailRequest = new EditProductDetailRequest();
    productRequest.setPristineCategory(true);
    productRequest.setScoreUpdated(true);
    oldProduct.setProductImages(productImages);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0)
      .setLocationPath(LOCATION_PATH);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1)
      .setLocationPath(LOCATION_PATH1);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    newProduct.setProductImages(productImages);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    newProduct.getProductItems().get(0).getProductItemImages().get(0)
      .setLocationPath(LOCATION_PATH);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    newProduct.getProductItems().get(0).getProductItemImages().get(1)
      .setLocationPath(LOCATION_PATH1);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    editProductDetailRequest.setProductRequest(productRequest);
    productImageEditRequest.setProductCode(productRequest.getProductCode());
    editProductDetailRequest.setProductImageEditRequests(
      Collections.singletonList(productImageEditRequest));
    product.setProductItems(productItems);
    oldProduct.getProductItems().forEach(productItem1 -> productItem1.setNewlyAddedItem(true));
    newProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> productItem1.setGeneratedItemName(ITEM_NAME));
    oldProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> productItem1.setGeneratedItemName(ITEM_NAME));
    oldProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> {
      productItem1.setGeneratedItemName(ITEM_NAME);
      productItem1.setId(null);
    });
    newProduct.getProductItems().forEach(productItem1 -> productItem1.setProductItemImages(Collections.emptyList()));
    Mockito.when(repository.saveAndFlush(any())).thenReturn(product);
    ProductDetailEditDTO result =
      service.updateProductContentAndImages(STORE_ID, newProduct, editProductDetailRequest,
        new ArrayList<>());
    Mockito.verify(repository, times(1)).saveAndFlush(any(Product.class));
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
      anyString());
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(anyString(),
      anyString());
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(anyString(), any(),
      anyBoolean());
    verify(categoryService, times(2)).getCategoryByStoreIdAndIdCached(anyString(), anyString());
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(anyString(), anyString());
    verify(productItemService).removeDeletedProductItemImagesWithoutFilteringMainImages(any());
    verify(applicationContext).getBean(ProductService.class);
  }

  @Test
  public void testUpdateProductContentAndImages_withProductDetailChangesForNewlyAddedVarinatsEmptyImageMFDTrueTest() throws Exception {
    ReflectionTestUtils.setField(service, "processVariantImageNewlyAddedItems", true);
    Product oldProduct = generateProductRequest();
    oldProduct.setVersion(1L);
    oldProduct.setMarkForDelete(false);
    Product newProduct = generateProductRequest();
    newProduct.setStoreId(STORE_ID);
    newProduct.setMarkForDelete(false);
    newProduct.getProductItems().remove(1);
    oldProduct.getProductItems().remove(1);
    newProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
      .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(true));
    oldProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
      .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(true));
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setValue("Value");
    productItemAttributeValue.setAttribute(new Attribute());
    productItemAttributeValue.getAttribute().setName("Brand");
    List<ProductItemAttributeValue> productItemAttributeValueList =
      new ArrayList<ProductItemAttributeValue>();
    productItemAttributeValueList.add(productItemAttributeValue);
    oldProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
    });
    newProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
      productItem1.setNewlyAddedItem(true);
      productItem1.setId(null);
    });
    oldProduct.setBrand("Brand");
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setCopy(true);
    copyImageEditRequest.setMainImage(false);
    copyImageEditRequest.setMarkForDelete(true);
    productImageEditRequest.setImagePath("/xyz.png");
    productImageEditRequest.setCopyToAllVariantImages(copyImageEditRequest);
    productImageEditRequest.setProductCode(PRODUCT_CODE);
    productImageEditRequest.setNeedRevision(true);
    productImageEditRequest.setActivatedBefore(false);
    Image itemImage = new Image();
    itemImage.setMainImages(true);
    itemImage.setLocationPath(LOCATION_PATH);
    itemImage.setMarkForDelete(false);
    ProductImage productImage = new ProductImage();
    productImage.setMarkForDelete(false);
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    when(
      productService.getProductByStoreIdAndProductCodeCached(anyString(), anyString())).thenReturn(
      oldProduct);
    oldProduct.setProductCategories(productCategories);
    newProduct.setProductImages(productImages);
    EditProductDetailRequest editProductDetailRequest = new EditProductDetailRequest();
    productRequest.setPristineCategory(true);
    productRequest.setScoreUpdated(true);
    oldProduct.setProductImages(productImages);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0)
      .setLocationPath(LOCATION_PATH);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1)
      .setLocationPath(LOCATION_PATH1);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    newProduct.setProductImages(productImages);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    newProduct.getProductItems().get(0).getProductItemImages().get(0)
      .setLocationPath(LOCATION_PATH);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    newProduct.getProductItems().get(0).getProductItemImages().get(1)
      .setLocationPath(LOCATION_PATH1);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    editProductDetailRequest.setProductRequest(productRequest);
    productImageEditRequest.setProductCode(productRequest.getProductCode());
    editProductDetailRequest.setProductImageEditRequests(
      Collections.singletonList(productImageEditRequest));
    product.setProductItems(productItems);
    oldProduct.getProductItems().forEach(productItem1 -> productItem1.setNewlyAddedItem(true));
    newProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> productItem1.setGeneratedItemName(ITEM_NAME));
    oldProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> productItem1.setGeneratedItemName(ITEM_NAME));
    oldProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> {
      productItem1.setGeneratedItemName(ITEM_NAME);
      productItem1.setId(null);
    });
    newProduct.getProductItems().forEach(productItem1 -> productItem1.setProductItemImages(Collections.emptyList()));
    Mockito.when(repository.saveAndFlush(any())).thenReturn(product);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->
          service.updateProductContentAndImages(STORE_ID, newProduct, editProductDetailRequest, new ArrayList<>()));
    }
    finally {
      verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString());
      verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(anyString(),
        anyString());
      verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(anyString(), any(),
        anyBoolean());
      verify(categoryService, times(2)).getCategoryByStoreIdAndIdCached(anyString(), anyString());
      verify(imageService).getProductImagesByStoreIdAndProductIdCached(anyString(), anyString());
      verify(productItemService).removeDeletedProductItemImagesWithoutFilteringMainImages(any());
      verify(applicationContext).getBean(ProductService.class);
    }

  }

  @Test
  public void testUpdateProductContentAndImages_withProductDetailChangesForNewlyAddedVarinatsEmptyImageWithNoMainImageTest() throws Exception {
    ReflectionTestUtils.setField(service, "processVariantImageNewlyAddedItems", true);
    Product oldProduct = generateProductRequest();
    oldProduct.setVersion(1L);
    oldProduct.setMarkForDelete(false);
    Product newProduct = generateProductRequest();
    newProduct.setStoreId(STORE_ID);
    newProduct.setMarkForDelete(false);
    newProduct.getProductItems().remove(1);
    oldProduct.getProductItems().remove(1);
    newProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
      .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(true));
    oldProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
      .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(true));
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setValue("Value");
    productItemAttributeValue.setAttribute(new Attribute());
    productItemAttributeValue.getAttribute().setName("Brand");
    List<ProductItemAttributeValue> productItemAttributeValueList =
      new ArrayList<ProductItemAttributeValue>();
    productItemAttributeValueList.add(productItemAttributeValue);
    oldProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
    });
    newProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
      productItem1.setNewlyAddedItem(true);
      productItem1.setId(null);
    });
    oldProduct.setBrand("Brand");
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setCopy(true);
    copyImageEditRequest.setMainImage(false);
    copyImageEditRequest.setMarkForDelete(false);
    productImageEditRequest.setImagePath("/xyz.png");
    productImageEditRequest.setCopyToAllVariantImages(copyImageEditRequest);
    productImageEditRequest.setProductCode(PRODUCT_CODE);
    productImageEditRequest.setNeedRevision(true);
    productImageEditRequest.setActivatedBefore(false);
    Image itemImage = new Image();
    itemImage.setMainImages(true);
    itemImage.setLocationPath(LOCATION_PATH);
    itemImage.setMarkForDelete(false);
    ProductImage productImage = new ProductImage();
    productImage.setMarkForDelete(false);
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    when(
      productService.getProductByStoreIdAndProductCodeCached(anyString(), anyString())).thenReturn(
      oldProduct);
    oldProduct.setProductCategories(productCategories);
    newProduct.setProductImages(productImages);
    EditProductDetailRequest editProductDetailRequest = new EditProductDetailRequest();
    productRequest.setPristineCategory(true);
    productRequest.setScoreUpdated(true);
    oldProduct.setProductImages(productImages);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0)
      .setLocationPath(LOCATION_PATH);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1)
      .setLocationPath(LOCATION_PATH1);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    newProduct.setProductImages(productImages);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    newProduct.getProductItems().get(0).getProductItemImages().get(0)
      .setLocationPath(LOCATION_PATH);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    newProduct.getProductItems().get(0).getProductItemImages().get(1)
      .setLocationPath(LOCATION_PATH1);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    editProductDetailRequest.setProductRequest(productRequest);
    productImageEditRequest.setProductCode(productRequest.getProductCode());
    editProductDetailRequest.setProductImageEditRequests(
      Collections.singletonList(productImageEditRequest));
    product.setProductItems(productItems);
    oldProduct.getProductItems().forEach(productItem1 -> productItem1.setNewlyAddedItem(true));
    newProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> productItem1.setGeneratedItemName(ITEM_NAME));
    oldProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> productItem1.setGeneratedItemName(ITEM_NAME));
    oldProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> {
      productItem1.setGeneratedItemName(ITEM_NAME);
      productItem1.setId(null);
    });
    newProduct.getProductItems().forEach(productItem1 -> productItem1.setProductItemImages(Collections.emptyList()));
    Mockito.when(repository.saveAndFlush(any())).thenReturn(product);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->
          service.updateProductContentAndImages(STORE_ID, newProduct, editProductDetailRequest, new ArrayList<>()));
    }
    finally {
      verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString());
      verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(anyString(),
        anyString());
      verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(anyString(), any(),
        anyBoolean());
      verify(categoryService, times(2)).getCategoryByStoreIdAndIdCached(anyString(), anyString());
      verify(imageService).getProductImagesByStoreIdAndProductIdCached(anyString(), anyString());
      verify(productItemService).removeDeletedProductItemImagesWithoutFilteringMainImages(any());
      verify(applicationContext).getBean(ProductService.class);
    }

  }

  @Test
  public void testUpdateProductContentAndImages_withProductDetailChangesForNewlyAddedVarinats2() throws Exception {
    ReflectionTestUtils.setField(service, "processVariantImageNewlyAddedItems", true);
    Product oldProduct = generateProductRequest();
    oldProduct.setVersion(1L);
    oldProduct.setMarkForDelete(false);
    oldProduct.setProductCategories(Collections.singletonList(productCategory));
    Product newProduct = generateProductRequest();
    newProduct.setStoreId(STORE_ID);
    newProduct.setProductCategories(Collections.singletonList(productCategory));

    newProduct.setMarkForDelete(false);
    newProduct.getProductItems().remove(1);
    oldProduct.getProductItems().remove(1);
    newProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
      .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(false));
    oldProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
      .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(false));
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setValue("Value");
    productItemAttributeValue.setAttribute(new Attribute());
    productItemAttributeValue.getAttribute().setName("Brand");
    List<ProductItemAttributeValue> productItemAttributeValueList =
      new ArrayList<ProductItemAttributeValue>();
    productItemAttributeValueList.add(productItemAttributeValue);
    oldProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
    });
    newProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
      productItem1.setNewlyAddedItem(true);
      productItem1.setId(null);
    });
    oldProduct.setBrand("Brand");
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setCopy(true);
    productImageEditRequest.setImagePath("/xyz.png");
    productImageEditRequest.setCopyToAllVariantImages(copyImageEditRequest);
    productImageEditRequest.setProductCode(PRODUCT_CODE);
    productImageEditRequest.setNeedRevision(true);
    productImageEditRequest.setActivatedBefore(false);
    Image itemImage = new Image();
    itemImage.setMainImages(false);
    itemImage.setLocationPath(LOCATION_PATH);
    itemImage.setMarkForDelete(false);
    ProductImage productImage = new ProductImage();
    productImage.setCommonImage(true);
    productImage.setMarkForDelete(false);
    productImage.setId("id");
    productImage.setLocationPath(LOCATION_PATH);
    productImages.add(productImage);
    when(
      productService.getProductByStoreIdAndProductCodeCached(anyString(), anyString())).thenReturn(
      oldProduct);
    oldProduct.setProductCategories(productCategories);
    newProduct.setProductImages(productImages);
    EditProductDetailRequest editProductDetailRequest = new EditProductDetailRequest();
    productRequest.setPristineCategory(true);
    productRequest.setScoreUpdated(true);
    productRequest.getProductItems().stream()
      .forEach(t -> t.setImages(Collections.singletonList(itemImage)));
    oldProduct.setProductImages(productImages);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(false);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0)
      .setLocationPath(LOCATION_PATH);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1)
      .setLocationPath(LOCATION_PATH1);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    newProduct.setProductImages(productImages);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    newProduct.getProductItems().get(0).getProductItemImages().get(0)
      .setLocationPath(LOCATION_PATH);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    newProduct.getProductItems().get(0).getProductItemImages().get(1)
      .setLocationPath(LOCATION_PATH1);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    editProductDetailRequest.setProductRequest(productRequest);
    productImageEditRequest.setProductCode(productRequest.getProductCode());
    editProductDetailRequest.setProductImageEditRequests(
      Collections.singletonList(productImageEditRequest));
    product.setProductItems(productItems);
    oldProduct.getProductItems().forEach(productItem1 -> productItem1.setNewlyAddedItem(true));
    newProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> productItem1.setGeneratedItemName(ITEM_NAME));
    oldProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> productItem1.setGeneratedItemName(ITEM_NAME));
    oldProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> {
      productItem1.setGeneratedItemName(ITEM_NAME);
      productItem1.setId(null);
    });
    ProductCategory productCategory2 = new ProductCategory();
    Category category3 = new Category();
    category3.setCategoryCode(CATEGORY_CODE_1);
    productCategory2.setCategory(category3);
    newProduct.setProductCategories(Collections.singletonList(productCategory2));
    oldProduct.setProductCategories(Collections.singletonList(productCategory2));
    Mockito.when(repository.saveAndFlush(any())).thenReturn(product);
    oldProduct.getProductImages().forEach(productImage1 -> productImage1.setMainImages(true));
    newProduct.getProductImages().forEach(productImage1 -> productImage1.setMainImages(true));
    product.getProductImages().forEach(productImage1 -> productImage1.setMainImages(true));

    ProductDetailEditDTO result =
      service.updateProductContentAndImages(STORE_ID, newProduct, editProductDetailRequest,
        new ArrayList<>());
    Mockito.verify(repository, times(1)).saveAndFlush(any(Product.class));
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
      anyString());
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(anyString(),
      anyString());
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(anyString(), any(),
      anyBoolean());
    verify(categoryService, times(2)).getCategoryByStoreIdAndIdCached(any(), any());
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(anyString(), anyString());
    verify(productItemService).removeDeletedProductItemImagesWithoutFilteringMainImages(any());
    verify(applicationContext).getBean(ProductService.class);
  }

  @Test
  public void testUpdateProductContentAndImages_withProductDetailChangesForNewlyAddedVarinatsDifferentMissingImage() throws Exception {
    ReflectionTestUtils.setField(service, "processVariantImageNewlyAddedItems", true);
    Product oldProduct = generateProductRequest();
    oldProduct.setVersion(1L);
    oldProduct.setMarkForDelete(false);
    oldProduct.setProductCategories(Collections.singletonList(productCategory));
    Product newProduct = generateProductRequest();
    newProduct.setStoreId(STORE_ID);
    newProduct.setProductCategories(Collections.singletonList(productCategory));

    newProduct.setMarkForDelete(false);
    newProduct.getProductItems().remove(1);
    oldProduct.getProductItems().remove(1);
    newProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
      .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(false));
    oldProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
      .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(false));
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setValue("Value");
    productItemAttributeValue.setAttribute(new Attribute());
    productItemAttributeValue.getAttribute().setName("Brand");
    List<ProductItemAttributeValue> productItemAttributeValueList =
      new ArrayList<ProductItemAttributeValue>();
    productItemAttributeValueList.add(productItemAttributeValue);
    oldProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
    });
    newProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
      productItem1.setNewlyAddedItem(true);
      productItem1.setId(null);
    });
    oldProduct.setBrand("Brand");
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setCopy(true);
    productImageEditRequest.setImagePath("/xyz.png");
    productImageEditRequest.setCopyToAllVariantImages(copyImageEditRequest);
    productImageEditRequest.setProductCode(PRODUCT_CODE);
    productImageEditRequest.setNeedRevision(true);
    productImageEditRequest.setActivatedBefore(false);
    Image itemImage = new Image();
    itemImage.setMainImages(false);
    itemImage.setLocationPath(LOCATION_PATH);
    itemImage.setMarkForDelete(false);
    ProductImage productImage = new ProductImage();
    productImage.setCommonImage(true);
    productImage.setMarkForDelete(false);
    productImage.setId("id");
    productImage.setLocationPath(LOCATION_PATH.concat("-1"));
    productImages.add(productImage);
    when(
      productService.getProductByStoreIdAndProductCodeCached(anyString(), anyString())).thenReturn(
      oldProduct);
    oldProduct.setProductCategories(productCategories);
    newProduct.setProductImages(productImages);
    EditProductDetailRequest editProductDetailRequest = new EditProductDetailRequest();
    productRequest.setPristineCategory(true);
    productRequest.setScoreUpdated(true);
    productRequest.getProductItems().stream()
      .forEach(t -> t.setImages(Collections.singletonList(itemImage)));
    oldProduct.setProductImages(productImages);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(false);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0)
      .setLocationPath(LOCATION_PATH);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1)
      .setLocationPath(LOCATION_PATH1);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    newProduct.setProductImages(productImages);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    newProduct.getProductItems().get(0).getProductItemImages().get(0)
      .setLocationPath(LOCATION_PATH);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    newProduct.getProductItems().get(0).getProductItemImages().get(1)
      .setLocationPath(LOCATION_PATH1);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    editProductDetailRequest.setProductRequest(productRequest);
    productImageEditRequest.setProductCode(productRequest.getProductCode());
    editProductDetailRequest.setProductImageEditRequests(
      Collections.singletonList(productImageEditRequest));
    product.setProductItems(productItems);
    oldProduct.getProductItems().forEach(productItem1 -> productItem1.setNewlyAddedItem(true));
    newProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> productItem1.setGeneratedItemName(ITEM_NAME));
    oldProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> productItem1.setGeneratedItemName(ITEM_NAME));
    oldProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> {
      productItem1.setGeneratedItemName(ITEM_NAME);
      productItem1.setId(null);
    });
    ProductCategory productCategory2 = new ProductCategory();
    Category category3 = new Category();
    category3.setCategoryCode(CATEGORY_CODE_1);
    productCategory2.setCategory(category3);
    newProduct.setProductCategories(Collections.singletonList(productCategory2));
    oldProduct.setProductCategories(Collections.singletonList(productCategory2));
    Mockito.when(repository.saveAndFlush(any())).thenReturn(product);
    oldProduct.getProductImages().forEach(productImage1 -> productImage1.setMainImages(true));
    newProduct.getProductImages().forEach(productImage1 -> productImage1.setMainImages(true));
    product.getProductImages().forEach(productImage1 -> productImage1.setMainImages(true));

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->
          service.updateProductContentAndImages(STORE_ID, newProduct, editProductDetailRequest, new ArrayList<>()));
    }
    finally {
      verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString());
      verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(anyString(),
        anyString());
      verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(anyString(), any(),
        anyBoolean());
      verify(categoryService, times(2)).getCategoryByStoreIdAndIdCached(any(), any());
      verify(imageService).getProductImagesByStoreIdAndProductIdCached(anyString(), anyString());
      verify(productItemService).removeDeletedProductItemImagesWithoutFilteringMainImages(any());
      verify(applicationContext).getBean(ProductService.class);
    }

  }

  @Test
  public void testUpdateProductContentAndImages_withProductDetailChangesForNewlyAddedVarinatsNullTest() throws Exception {
    ReflectionTestUtils.setField(service, "processVariantImageNewlyAddedItems", true);
    Product oldProduct = generateProductRequest();
    oldProduct.setVersion(1L);
    oldProduct.setMarkForDelete(false);
    Product newProduct = generateProductRequest();
    newProduct.setStoreId(STORE_ID);
    newProduct.setMarkForDelete(false);
    newProduct.getProductItems().remove(1);
    oldProduct.getProductItems().remove(1);
    newProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
      .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(false));
    oldProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
      .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(false));
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setValue("Value");
    productItemAttributeValue.setAttribute(new Attribute());
    productItemAttributeValue.getAttribute().setName("Brand");
    List<ProductItemAttributeValue> productItemAttributeValueList =
      new ArrayList<ProductItemAttributeValue>();
    productItemAttributeValueList.add(productItemAttributeValue);
    oldProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
    });
    newProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
      productItem1.setNewlyAddedItem(true);
      productItem1.setId(null);
    });
    oldProduct.setBrand("Brand");
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setCopy(true);
    productImageEditRequest.setImagePath("/xyz.png");
    productImageEditRequest.setCopyToAllVariantImages(copyImageEditRequest);
    productImageEditRequest.setProductCode(PRODUCT_CODE);
    productImageEditRequest.setNeedRevision(true);
    productImageEditRequest.setActivatedBefore(false);
    Image itemImage = new Image();
    itemImage.setMainImages(true);
    itemImage.setLocationPath(LOCATION_PATH);
    itemImage.setMarkForDelete(false);
    ProductImage productImage = new ProductImage();
    productImage.setMarkForDelete(false);
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    when(
      productService.getProductByStoreIdAndProductCodeCached(anyString(), anyString())).thenReturn(
      oldProduct);
    oldProduct.setProductCategories(productCategories);
    newProduct.setProductImages(productImages);
    EditProductDetailRequest editProductDetailRequest = new EditProductDetailRequest();
    productRequest.setPristineCategory(true);
    productRequest.setScoreUpdated(true);
    productRequest.getProductItems().stream()
      .forEach(t -> t.setImages(Collections.singletonList(itemImage)));
    oldProduct.setProductImages(productImages);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0)
      .setLocationPath(LOCATION_PATH);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1)
      .setLocationPath(LOCATION_PATH1);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    newProduct.setProductImages(productImages);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    newProduct.getProductItems().get(0).getProductItemImages().get(0)
      .setLocationPath(LOCATION_PATH);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    newProduct.getProductItems().get(0).getProductItemImages().get(1)
      .setLocationPath(LOCATION_PATH1);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    editProductDetailRequest.setProductRequest(productRequest);
    productImageEditRequest.setProductCode(productRequest.getProductCode());
    editProductDetailRequest.setProductImageEditRequests(
      Collections.singletonList(productImageEditRequest));
    product.setProductItems(productItems);
    oldProduct.getProductItems().forEach(productItem1 -> productItem1.setNewlyAddedItem(true));
    newProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> productItem1.setGeneratedItemName(ITEM_NAME));
    oldProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> {
      productItem1.setGeneratedItemName(ITEM_NAME.concat("1"));
      productItem1.setId(null);
    });
    Mockito.when(repository.saveAndFlush(any())).thenReturn(product);
    ProductDetailEditDTO result =
      service.updateProductContentAndImages(STORE_ID, newProduct, editProductDetailRequest,
        new ArrayList<>());
    Mockito.verify(repository, times(1)).saveAndFlush(any(Product.class));
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
      anyString());
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(anyString(),
      anyString());
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(anyString(), any(),
      anyBoolean());
    verify(categoryService, times(2)).getCategoryByStoreIdAndIdCached(anyString(), anyString());
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(anyString(), anyString());
    verify(productItemService).removeDeletedProductItemImagesWithoutFilteringMainImages(any());
    verify(applicationContext).getBean(ProductService.class);
  }

  @Test
  public void testUpdateProductContentAndImages_withProductDetailChangesForNewlyAddedVarinatsNonNull_IdTest() throws Exception {
    ReflectionTestUtils.setField(service, "processVariantImageNewlyAddedItems", true);
    Product oldProduct = generateProductRequest();
    oldProduct.setVersion(1L);
    oldProduct.setMarkForDelete(false);
    Product newProduct = generateProductRequest();
    newProduct.setStoreId(STORE_ID);
    newProduct.setMarkForDelete(false);
    newProduct.getProductItems().remove(1);
    oldProduct.getProductItems().remove(1);
    newProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
        .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(false));
    oldProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
        .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(false));
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setValue("Value");
    productItemAttributeValue.setAttribute(new Attribute());
    productItemAttributeValue.getAttribute().setName("Brand");
    List<ProductItemAttributeValue> productItemAttributeValueList =
        new ArrayList<ProductItemAttributeValue>();
    productItemAttributeValueList.add(productItemAttributeValue);
    oldProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
    });
    newProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
      productItem1.setNewlyAddedItem(true);
      productItem1.setId(ID);
    });
    oldProduct.setBrand("Brand");
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setCopy(true);
    productImageEditRequest.setImagePath("/xyz.png");
    productImageEditRequest.setCopyToAllVariantImages(copyImageEditRequest);
    productImageEditRequest.setProductCode(PRODUCT_CODE);
    productImageEditRequest.setNeedRevision(true);
    productImageEditRequest.setActivatedBefore(false);
    Image itemImage = new Image();
    itemImage.setMainImages(true);
    itemImage.setLocationPath(LOCATION_PATH);
    itemImage.setMarkForDelete(false);
    ProductImage productImage = new ProductImage();
    productImage.setMarkForDelete(false);
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    when(
        productService.getProductByStoreIdAndProductCodeCached(anyString(), anyString())).thenReturn(
        oldProduct);
    oldProduct.setProductCategories(productCategories);
    newProduct.setProductImages(productImages);
    EditProductDetailRequest editProductDetailRequest = new EditProductDetailRequest();
    productRequest.setPristineCategory(true);
    productRequest.setScoreUpdated(true);
    productRequest.getProductItems().stream()
        .forEach(t -> t.setImages(Collections.singletonList(itemImage)));
    oldProduct.setProductImages(productImages);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0)
        .setLocationPath(LOCATION_PATH);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1)
        .setLocationPath(LOCATION_PATH1);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    oldProduct.getProductItems().get(0).setId(ID);
    newProduct.setProductImages(productImages);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    newProduct.getProductItems().get(0).getProductItemImages().get(0)
        .setLocationPath(LOCATION_PATH);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    newProduct.getProductItems().get(0).getProductItemImages().get(1)
        .setLocationPath(LOCATION_PATH1);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    newProduct.getProductItems().get(0).setId(ID);
    editProductDetailRequest.setProductRequest(productRequest);
    productImageEditRequest.setProductCode(productRequest.getProductCode());
    editProductDetailRequest.setProductImageEditRequests(
        Collections.singletonList(productImageEditRequest));
    product.setProductItems(productItems);
    oldProduct.getProductItems().forEach(productItem1 -> productItem1.setNewlyAddedItem(true));
    newProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> productItem1.setGeneratedItemName(ITEM_NAME));
    oldProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> {
      productItem1.setGeneratedItemName(ITEM_NAME.concat("1"));
      productItem1.setId(ID);
      productItem1.setNewlyAddedItem(false);
    });
    Mockito.when(repository.saveAndFlush(any())).thenReturn(product);
    ProductDetailEditDTO result =
        service.updateProductContentAndImages(STORE_ID, newProduct, editProductDetailRequest,
            new ArrayList<>());
    Mockito.verify(repository, times(1)).saveAndFlush(any(Product.class));
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString());
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(anyString(),
        anyString());
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(anyString(), any(),
        anyBoolean());
    verify(categoryService, times(2)).getCategoryByStoreIdAndIdCached(anyString(), anyString());
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(anyString(), anyString());
    verify(productItemService).removeDeletedProductItemImagesWithoutFilteringMainImages(any());
    verify(applicationContext).getBean(ProductService.class);
  }

  @Test
  public void testUpdateProductContentAndImages_withProductDetailChangesForNewlyAddedVarinats_NewlyAddedItemsTest() throws Exception {
    ReflectionTestUtils.setField(service, "processVariantImageNewlyAddedItems", true);
    Product oldProduct = generateProductRequest();
    oldProduct.setVersion(1L);
    oldProduct.setMarkForDelete(false);
    Product newProduct = generateProductRequest();
    newProduct.setStoreId(STORE_ID);
    newProduct.setMarkForDelete(false);
    newProduct.getProductItems().remove(1);
    oldProduct.getProductItems().remove(1);
    newProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
        .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(false));
    oldProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
        .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(false));
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setValue("Value");
    productItemAttributeValue.setAttribute(new Attribute());
    productItemAttributeValue.getAttribute().setName("Brand");
    List<ProductItemAttributeValue> productItemAttributeValueList =
        new ArrayList<ProductItemAttributeValue>();
    productItemAttributeValueList.add(productItemAttributeValue);
    oldProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
    });
    newProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
      productItem1.setNewlyAddedItem(true);
      productItem1.setId(ID);
    });
    oldProduct.setBrand("Brand");
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setCopy(true);
    productImageEditRequest.setImagePath("/xyz.png");
    productImageEditRequest.setCopyToAllVariantImages(copyImageEditRequest);
    productImageEditRequest.setProductCode(PRODUCT_CODE);
    productImageEditRequest.setNeedRevision(true);
    productImageEditRequest.setActivatedBefore(false);
    Image itemImage = new Image();
    itemImage.setMainImages(true);
    itemImage.setLocationPath(LOCATION_PATH);
    itemImage.setMarkForDelete(false);
    ProductImage productImage = new ProductImage();
    productImage.setMarkForDelete(false);
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    when(
        productService.getProductByStoreIdAndProductCodeCached(anyString(), anyString())).thenReturn(
        oldProduct);
    oldProduct.setProductCategories(productCategories);
    newProduct.setProductImages(productImages);
    EditProductDetailRequest editProductDetailRequest = new EditProductDetailRequest();
    productRequest.setPristineCategory(true);
    productRequest.setScoreUpdated(true);
    productRequest.getProductItems().stream()
        .forEach(t -> t.setImages(Collections.singletonList(itemImage)));
    oldProduct.setProductImages(productImages);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0)
        .setLocationPath(LOCATION_PATH);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1)
        .setLocationPath(LOCATION_PATH1);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    oldProduct.getProductItems().get(0).setId(ID);
    newProduct.setProductImages(productImages);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    newProduct.getProductItems().get(0).getProductItemImages().get(0)
        .setLocationPath(LOCATION_PATH);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    newProduct.getProductItems().get(0).getProductItemImages().get(1)
        .setLocationPath(LOCATION_PATH1);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    newProduct.getProductItems().get(0).setId(ID);
    editProductDetailRequest.setProductRequest(productRequest);
    productImageEditRequest.setProductCode(productRequest.getProductCode());
    editProductDetailRequest.setProductImageEditRequests(
        Collections.singletonList(productImageEditRequest));
    product.setProductItems(productItems);
    oldProduct.getProductItems().forEach(productItem1 -> productItem1.setNewlyAddedItem(true));
    newProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> productItem1.setGeneratedItemName(ITEM_NAME));
    oldProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).forEach(productItem1 -> {
      productItem1.setGeneratedItemName(ITEM_NAME.concat("1"));
      productItem1.setId(null);
      productItem1.setNewlyAddedItem(false);
    });
    Mockito.when(repository.saveAndFlush(any())).thenReturn(product);
    ProductDetailEditDTO result =
        service.updateProductContentAndImages(STORE_ID, newProduct, editProductDetailRequest,
            new ArrayList<>());
    Mockito.verify(repository, times(1)).saveAndFlush(any(Product.class));
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString());
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(anyString(),
        anyString());
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(anyString(), any(),
        anyBoolean());
    verify(categoryService, times(2)).getCategoryByStoreIdAndIdCached(anyString(), anyString());
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(anyString(), anyString());
    verify(productItemService).removeDeletedProductItemImagesWithoutFilteringMainImages(any());
    verify(applicationContext).getBean(ProductService.class);
  }


  @Test
  public void testUpdateProductContentAndImages_withProductDetailChangesNewAddedNotEmptyTest() throws Exception {
    Product oldProduct = generateProductRequest();
    oldProduct.setVersion(1L);
    oldProduct.setMarkForDelete(false);
    Product newProduct = generateProductRequest();
    newProduct.setStoreId(STORE_ID);
    newProduct.setMarkForDelete(false);
    newProduct.getProductItems().remove(1);
    oldProduct.getProductItems().remove(1);
    newProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
        .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(false));
    oldProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
        .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(false));
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setValue("Value");
    productItemAttributeValue.setAttribute(new Attribute());
    productItemAttributeValue.getAttribute().setName("Brand");
    List<ProductItemAttributeValue> productItemAttributeValueList =
        new ArrayList<ProductItemAttributeValue>();
    productItemAttributeValueList.add(productItemAttributeValue);
    oldProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
    });
    newProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
    });
    oldProduct.setBrand("Brand");
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setCopy(true);
    productImageEditRequest.setImagePath("/xyz.png");
    productImageEditRequest.setCopyToAllVariantImages(copyImageEditRequest);
    productImageEditRequest.setProductCode(PRODUCT_CODE);
    productImageEditRequest.setNeedRevision(true);
    productImageEditRequest.setActivatedBefore(false);
    Image itemImage = new Image();
    itemImage.setMainImages(true);
    itemImage.setLocationPath(LOCATION_PATH);
    itemImage.setMarkForDelete(false);
    ProductImage productImage = new ProductImage();
    productImage.setMarkForDelete(false);
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    when(
        productService.getProductByStoreIdAndProductCodeCached(anyString(), anyString())).thenReturn(
        oldProduct);
    oldProduct.setProductCategories(productCategories);
    newProduct.setProductImages(productImages);
    EditProductDetailRequest editProductDetailRequest = new EditProductDetailRequest();
    productRequest.setPristineCategory(true);
    productRequest.setScoreUpdated(true);
    productRequest.getProductItems().stream()
        .forEach(t -> t.setImages(Collections.singletonList(itemImage)));
    oldProduct.setProductImages(productImages);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0)
        .setLocationPath(LOCATION_PATH);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1)
        .setLocationPath(LOCATION_PATH1);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    newProduct.setProductImages(productImages);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    newProduct.getProductItems().get(0).getProductItemImages().get(0)
        .setLocationPath(LOCATION_PATH);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    newProduct.getProductItems().get(0).getProductItemImages().get(1)
        .setLocationPath(LOCATION_PATH1);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    editProductDetailRequest.setProductRequest(productRequest);
    productImageEditRequest.setProductCode(productRequest.getProductCode());
    editProductDetailRequest.setProductImageEditRequests(
        Collections.singletonList(productImageEditRequest));
    product.setProductItems(productItems);
    Mockito.when(repository.saveAndFlush(any())).thenReturn(product);
    List<NewlySavedItemResponse> newlySavedItemResponseslist = new ArrayList<>();
    NewlySavedItemResponse newlySavedItemResponse = new NewlySavedItemResponse();
    newlySavedItemResponse.setDangerousGoodsLevel(2);
    newlySavedItemResponseslist.add(newlySavedItemResponse);
    ProductDetailEditDTO result =
        service.updateProductContentAndImages(STORE_ID, newProduct, editProductDetailRequest,
            newlySavedItemResponseslist);
    Mockito.verify(repository, times(1)).saveAndFlush(any(Product.class));
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString());
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(anyString(),
        anyString());
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(anyString(), any(),
        anyBoolean());
    verify(categoryService, times(2)).getCategoryByStoreIdAndIdCached(anyString(), anyString());
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(anyString(), anyString());
    verify(productItemService).removeDeletedProductItemImagesWithoutFilteringMainImages(any());

    verify(applicationContext).getBean(ProductService.class);
  }


  @Test
  public void testUpdateProductContentAndImages_withProductDetailChangesImageUpadteException() throws Exception {
    Product oldProduct = generateProductRequest();
    oldProduct.setVersion(1L);
    oldProduct.setMarkForDelete(false);
    Product newProduct = generateProductRequest();
    newProduct.setStoreId(STORE_ID);
    newProduct.setMarkForDelete(false);
    newProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
      .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(false));
    oldProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
      .flatMap(List::stream).peek(productItemImage -> productItemImage.setMarkForDelete(false));
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setValue("Value");
    productItemAttributeValue.setAttribute(new Attribute());
    productItemAttributeValue.getAttribute().setName("Brand");
    List<ProductItemAttributeValue> productItemAttributeValueList =
      new ArrayList<ProductItemAttributeValue>();
    productItemAttributeValueList.add(productItemAttributeValue);
    oldProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
    });
    newProduct.getProductItems().forEach(productItem1 -> {
      productItem1.setProductItemAttributeValues(productItemAttributeValueList);
      productItem1.setHash(HASH.getBytes());
    });
    oldProduct.setBrand("Brand");
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setCopy(true);
    productImageEditRequest.setImagePath("/xyz.png");
    productImageEditRequest.setCopyToAllVariantImages(copyImageEditRequest);
    productImageEditRequest.setProductCode(PRODUCT_CODE);
    productImageEditRequest.setNeedRevision(true);
    productImageEditRequest.setActivatedBefore(false);
    Image itemImage = new Image();
    itemImage.setMainImages(true);
    itemImage.setLocationPath(LOCATION_PATH);
    itemImage.setMarkForDelete(false);
    ProductImage productImage = new ProductImage();
    productImage.setMarkForDelete(false);
    BeanUtils.copyProperties(itemImage, productImage);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    when(
      productService.getProductByStoreIdAndProductCodeCached(anyString(), anyString())).thenReturn(
      oldProduct);
    oldProduct.setProductCategories(productCategories);
    newProduct.setProductImages(productImages);
    EditProductDetailRequest editProductDetailRequest = new EditProductDetailRequest();
    productRequest.setPristineCategory(true);
    productRequest.setScoreUpdated(true);
    productRequest.getProductItems().stream()
      .forEach(t -> t.setImages(Collections.singletonList(itemImage)));
    oldProduct.setProductImages(productImages);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    oldProduct.getProductItems().get(0).getProductItemImages().get(0)
      .setLocationPath(LOCATION_PATH);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1)
      .setLocationPath(LOCATION_PATH1);
    oldProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    newProduct.setProductImages(productImages);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    newProduct.getProductItems().get(0).getProductItemImages().get(0)
      .setLocationPath(LOCATION_PATH);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setSequence(11);
    newProduct.getProductItems().get(0).getProductItemImages().get(1)
      .setLocationPath(LOCATION_PATH1);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setSequence(10);
    newProduct.getProductItems().get(0).getProductItemImages().get(1).setMarkForDelete(false);
    editProductDetailRequest.setProductRequest(productRequest);
    productImageEditRequest.setProductCode(productRequest.getProductCode());
    editProductDetailRequest.setProductImageEditRequests(
      Collections.singletonList(productImageEditRequest));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->
          service.updateProductContentAndImages(STORE_ID, newProduct, editProductDetailRequest,
          new ArrayList<>()));
    }
    finally {
      verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(anyString(),
        anyString());
      verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(anyString(),
        anyString());
      verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(anyString(), any(),
        anyBoolean());
      verify(categoryService, times(2)).getCategoryByStoreIdAndIdCached(anyString(), anyString());
      verify(imageService).getProductImagesByStoreIdAndProductIdCached(anyString(), anyString());
      verify(productItemService).removeDeletedProductItemImagesWithoutFilteringMainImages(any());

      verify(applicationContext).getBean(ProductService.class);
    }
  }

  @Test
  public void getProductByStoreIdAndProductCodeTest() {
    when(repository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    Product newProduct = service.getProductByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(repository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    assertEquals(product, newProduct);
  }

  @Test
  public void findByStoreIdAndProductCodeAndMarkForDeleteFalseTest() {
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_CODE)).thenReturn(product);
    Product newProduct =
        service.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Assertions.assertEquals(product, newProduct);
  }

  @Test
  public void findByStoreIdAndIdAndMarkForDeleteFalseTest() {
    when(repository.findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_CODE)).thenReturn(product);
    Product newProduct =
        service.findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(repository).findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Assertions.assertEquals(product, newProduct);
  }

  @Test
  public void fetchProductAndInsertMissingProductAttributesTest_2() {
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_CODE)).thenReturn(null);
    CommonImageBackfillingEventModel productAttributeDataBackFillingEventModel =
        new CommonImageBackfillingEventModel();
    productAttributeDataBackFillingEventModel.setProductCode(PRODUCT_CODE);
    productAttributeDataBackFillingEventModel.setStoreId(STORE_ID);
    service.fetchProductAndInsertMissingProductAttributes(
        productAttributeDataBackFillingEventModel);
    verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void fetchProductAndInsertMissingProductAttributesTest() {
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    ProductCategory productCategory = new ProductCategory();
    ProductAttribute productAttribute1 = new ProductAttribute();
    //ProductAttribute productAttribute2 = new ProductAttribute();
    ProductAttribute productAttribute3 = new ProductAttribute();
    ProductAttribute productAttribute4 = new ProductAttribute();
    ProductAttribute productAttribute5 = new ProductAttribute();
    Attribute attribute = new Attribute();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setId(ATTRIBUTE_ID);
    attribute.setMustShowOnCustomerSide(true);

    Attribute attribute2 = new Attribute();
    attribute2.setAttributeCode(ATTRIBUTE_CODE_2);
    attribute2.setId(ATTRIBUTE_ID_2);
    attribute2.setMustShowOnCustomerSide(true);

    Attribute attribute3 = new Attribute();
    attribute3.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute3.setAttributeCode(ATTRIBUTE_CODE_3);
    attribute3.setId(ATTRIBUTE_ID_3);
    attribute3.setMustShowOnCustomerSide(true);

    Attribute attribute5 = new Attribute();
    attribute5.setAttributeCode(ATTRIBUTE_CODE_5);
    attribute5.setId(ATTRIBUTE_ID_5);
    attribute5.setMustShowOnCustomerSide(true);

    Attribute attribute4 = new Attribute();
    attribute4.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attribute4.setAttributeCode(ATTRIBUTE_CODE_4);
    attribute4.setId(ATTRIBUTE_ID_4);
    attribute4.setMustShowOnCustomerSide(true);

    productAttribute1.setAttribute(attribute);
    //    productAttribute2.setAttribute(attribute2);
    productAttribute3.setAttribute(attribute3);
    productAttribute4.setAttribute(attribute4);
    productAttribute5.setAttribute(attribute5);

    productAttribute1.setAttributeId(ATTRIBUTE_ID);
    //    productAttribute2.setAttributeId(ATTRIBUTE_ID_2);
    productAttribute3.setAttributeId(ATTRIBUTE_ID_3);
    productAttribute4.setAttributeId(ATTRIBUTE_ID_4);
    productAttribute5.setAttributeId(ATTRIBUTE_ID_5);
    product.setProductAttributes(
        List.of(productAttribute1, productAttribute3, productAttribute4, productAttribute5));
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE_1);
    category.setId(CATEGORY_ID_1);
    productCategory.setCategory(category);
    productCategory.setCategoryId(CATEGORY_ID_1);
    product.setProductCategories(List.of(productCategory));

    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(attribute);
    CategoryAttribute categoryAttribute2 = new CategoryAttribute();
    categoryAttribute2.setAttribute(attribute2);

    CategoryAttribute categoryAttribute3 = new CategoryAttribute();
    categoryAttribute3.setAttribute(attribute3);

    CategoryAttribute categoryAttribute4 = new CategoryAttribute();
    categoryAttribute4.setAttribute(attribute4);
    CommonImageBackfillingEventModel productAttributeDataBackFillingEventModel =
        new CommonImageBackfillingEventModel();
    productAttributeDataBackFillingEventModel.setProductCode(PRODUCT_CODE);
    productAttributeDataBackFillingEventModel.setStoreId(STORE_ID);

    Mockito.when(
        repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
            PRODUCT_CODE)).thenReturn(product);
    AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
    allowedAttributeValue.setId(ID);
    Mockito.when(
            allowedAttributeValueService.findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
                Constants.DEFAULT_STORE_ID, attribute3, Constants.HYPHEN))
        .thenReturn(allowedAttributeValue);
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
        new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setId(ID);
    Mockito.when(
            predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
                Constants.DEFAULT_STORE_ID, attribute4, Constants.HYPHEN))
        .thenReturn(predefinedAllowedAttributeValue);
    Mockito.when(categoryService.getCategoryAttributes(Constants.DEFAULT_STORE_ID, CATEGORY_ID_1))
        .thenReturn(
            List.of(categoryAttribute, categoryAttribute2, categoryAttribute3, categoryAttribute4));
    Mockito.when(attributeService.findByAttributeIds(Mockito.anyString(), anyList()))
        .thenReturn(List.of(attribute, attribute2, attribute3, attribute4));
    Mockito.when(productAttributeService.saveMissedProductAttributes(anyList())).thenReturn(
        List.of(productAttribute1, productAttribute3, productAttribute4, productAttribute5));

    service.fetchProductAndInsertMissingProductAttributes(
        productAttributeDataBackFillingEventModel);

    Mockito.verify(repository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(categoryService)
        .getCategoryAttributes(Constants.DEFAULT_STORE_ID, CATEGORY_ID_1);
    Mockito.verify(attributeService).findByAttributeIds(Mockito.anyString(), anyList());
    Mockito.verify(productAttributeService).saveMissedProductAttributes(
        List.of(productAttribute1, productAttribute3, productAttribute4, productAttribute5));
    Mockito.when(productAttributeValueService.saveProductAttributeValues(anyList()))
        .thenReturn(new ArrayList<>());
    Mockito.verify(productAttributeValueService).saveProductAttributeValues(anyList());
    Mockito.verify(predefinedAllowedAttributeValueService)
        .findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
            attribute4, Constants.HYPHEN);
    Mockito.verify(allowedAttributeValueService)
        .findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
            attribute3, Constants.HYPHEN);
  }

  @Test
  public void fetchProductAndInsertMissingProductAttributesWithMigrationPayloadTest() {
    CommonImageBackfillingEventModel productAttributeDataBackFillingEventModel =
        getCommonImageBackfillingEventModel();
    Product product = new Product();
    ProductAttribute productAttribute = new ProductAttribute();
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCategoryId(CATEGORY_ID_1);
    productCategory.setProduct(product);
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setId(ATTRIBUTE_ID);
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    productAttribute.setAttributeId(ATTRIBUTE_ID);
    productAttribute.setProduct(product);
    productAttribute.setAttribute(attribute);
    product.setProductAttributes(List.of(productAttribute));
    product.setProductCategories(List.of(productCategory));
    Mockito.when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        productAttributeDataBackFillingEventModel.getStoreId(), PRODUCT_CODE)).thenReturn(product);
    Mockito.when(attributeService.findByAttributeIds(Mockito.eq(Constants.DEFAULT_STORE_ID),
        anyList())).thenReturn(new ArrayList<>());
    service.fetchProductAndInsertMissingProductAttributes(
        productAttributeDataBackFillingEventModel);
    Mockito.verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        productAttributeDataBackFillingEventModel.getStoreId(),
        productAttributeDataBackFillingEventModel.getProductCode());
    Mockito.verify(attributeService).findByAttributeIds(Mockito.anyString(), anyList());
    Mockito.verify(productAttributeService).saveMissedProductAttributes(anyList());
  }

  @Test
  public void fetchProductAndInsertMissingProductAttributes_WhenProductNotFound_ShouldReturnNullProduct() {
    // Setup
    CommonImageBackfillingEventModel eventModel = new CommonImageBackfillingEventModel();
    eventModel.setProductCode(PRODUCT_CODE);
    eventModel.setStoreId(STORE_ID);

    // Mock repository to return null
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(null);

    // Execute
    Pair<Product, List<ProductAttributeValue>> result = service.fetchProductAndInsertMissingProductAttributes(eventModel);

    // Verify
    verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    assertNotNull(result);
    assertNull(result.getKey());
    assertTrue(result.getValue().isEmpty());
  }

  @Test
  public void fetchProductAndInsertMissingProductAttributes_WhenProductHasNoCategories_ShouldThrowException() {
    // Setup
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setProductCategories(new ArrayList<>()); // Empty categories

    CommonImageBackfillingEventModel eventModel = new CommonImageBackfillingEventModel();
    eventModel.setProductCode(PRODUCT_CODE);
    eventModel.setStoreId(STORE_ID);

    // Mock repository
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(product);

    // Execute & Verify
    assertThrows(NoSuchElementException.class, () -> 
        service.fetchProductAndInsertMissingProductAttributes(eventModel));
    
    verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void fetchProductAndInsertMissingProductAttributes_WithDescriptiveAttribute_ShouldCreateDescriptiveValue() {
    // Setup
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    category.setId(CATEGORY_ID_1);
    productCategory.setCategory(category);
    productCategory.setCategoryId(CATEGORY_ID_1);
    product.setProductCategories(List.of(productCategory));
    product.setProductAttributes(new ArrayList<>());

    // Create descriptive attribute
    Attribute descriptiveAttribute = new Attribute();
    descriptiveAttribute.setId(ATTRIBUTE_ID);
    descriptiveAttribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    descriptiveAttribute.setMustShowOnCustomerSide(true);
    descriptiveAttribute.setVariantCreation(false);

    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(descriptiveAttribute);

    CommonImageBackfillingEventModel eventModel = new CommonImageBackfillingEventModel();
    eventModel.setProductCode(PRODUCT_CODE);
    eventModel.setStoreId(STORE_ID);

    // Mock services
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(product);
    when(categoryService.getCategoryAttributes(STORE_ID, CATEGORY_ID_1))
        .thenReturn(List.of(categoryAttribute));
    when(attributeService.findByAttributeIds(eq(STORE_ID), anyList()))
        .thenReturn(List.of(descriptiveAttribute));
    when(productAttributeService.saveMissedProductAttributes(anyList()))
        .thenAnswer(invocation -> {
            List<ProductAttribute> attrs = invocation.getArgument(0);
            return attrs.stream().map(attr -> {
                ProductAttribute saved = new ProductAttribute();
                saved.setId("saved-" + attr.getAttributeId());
                saved.setAttribute(attr.getAttribute());
                saved.setAttributeId(attr.getAttributeId());
                return saved;
            }).collect(Collectors.toList());
        });
    when(productAttributeValueService.saveProductAttributeValues(anyList()))
        .thenReturn(new ArrayList<>());

    // Set IMEI migration disabled to test normal flow
    ReflectionTestUtils.setField(service, "imeiMigrationEnabled", false);

    // Execute
    Pair<Product, List<ProductAttributeValue>> result = service.fetchProductAndInsertMissingProductAttributes(eventModel);

    // Verify
    verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(categoryService).getCategoryAttributes(STORE_ID, CATEGORY_ID_1);
    verify(attributeService).findByAttributeIds(eq(STORE_ID), anyList());
    verify(productAttributeService).saveMissedProductAttributes(anyList());
    verify(productAttributeValueService).saveProductAttributeValues(argThat(list -> 
        list.stream().anyMatch(pav -> 
            pav.getDescriptiveAttributeValueType() == DescriptiveAttributeValueType.SINGLE &&
            Constants.HYPHEN.equals(pav.getDescriptiveAttributeValue()))));
    assertNotNull(result);
    assertEquals(product, result.getKey());
  }

  @Test
  public void fetchProductAndInsertMissingProductAttributes_WithDefiningAttribute_ShouldCreateDefiningValue() {
    // Setup
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    category.setId(CATEGORY_ID_1);
    productCategory.setCategory(category);
    productCategory.setCategoryId(CATEGORY_ID_1);
    product.setProductCategories(List.of(productCategory));
    product.setProductAttributes(new ArrayList<>());

    // Create defining attribute
    Attribute definingAttribute = new Attribute();
    definingAttribute.setId(ATTRIBUTE_ID);
    definingAttribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    definingAttribute.setMustShowOnCustomerSide(true);
    definingAttribute.setVariantCreation(false);

    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(definingAttribute);

    CommonImageBackfillingEventModel eventModel = new CommonImageBackfillingEventModel();
    eventModel.setProductCode(PRODUCT_CODE);
    eventModel.setStoreId(STORE_ID);

    AllowedAttributeValue allowedValue = new AllowedAttributeValue();
    allowedValue.setId(ID);

    // Mock services
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(product);
    when(categoryService.getCategoryAttributes(STORE_ID, CATEGORY_ID_1))
        .thenReturn(List.of(categoryAttribute));
    when(attributeService.findByAttributeIds(eq(STORE_ID), anyList()))
        .thenReturn(List.of(definingAttribute));
    when(allowedAttributeValueService.findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
        eq(STORE_ID), any(Attribute.class), eq(Constants.HYPHEN)))
        .thenReturn(allowedValue);
    when(productAttributeService.saveMissedProductAttributes(anyList()))
        .thenAnswer(invocation -> {
            List<ProductAttribute> attrs = invocation.getArgument(0);
            return attrs.stream().map(attr -> {
                ProductAttribute saved = new ProductAttribute();
                saved.setId("saved-" + attr.getAttributeId());
                saved.setAttribute(attr.getAttribute());
                saved.setAttributeId(attr.getAttributeId());
                return saved;
            }).collect(Collectors.toList());
        });
    when(productAttributeValueService.saveProductAttributeValues(anyList()))
        .thenReturn(new ArrayList<>());

    // Set IMEI migration disabled
    ReflectionTestUtils.setField(service, "imeiMigrationEnabled", false);

    // Execute
    Pair<Product, List<ProductAttributeValue>> result = service.fetchProductAndInsertMissingProductAttributes(eventModel);

    // Verify
    verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(categoryService).getCategoryAttributes(STORE_ID, CATEGORY_ID_1);
    verify(attributeService).findByAttributeIds(eq(STORE_ID), anyList());
    verify(productAttributeService).saveMissedProductAttributes(anyList());
    verify(allowedAttributeValueService).findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
        eq(STORE_ID), any(Attribute.class), eq(Constants.HYPHEN));
    verify(productAttributeValueService).saveProductAttributeValues(argThat(list -> 
        list.stream().anyMatch(pav -> 
            pav.getDescriptiveAttributeValueType() == DescriptiveAttributeValueType.NONE &&
            pav.getAllowedAttributeValue() != null)));
    assertNotNull(result);
    assertEquals(product, result.getKey());
  }

  @Test
  public void fetchProductAndInsertMissingProductAttributes_WithVariantCreationAttribute_ShouldFilterOut() {
    // Setup
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    category.setId(CATEGORY_ID_1);
    productCategory.setCategory(category);
    productCategory.setCategoryId(CATEGORY_ID_1);
    product.setProductCategories(List.of(productCategory));
    product.setProductAttributes(new ArrayList<>());

    // Create variant creation attribute
    Attribute variantAttribute = new Attribute();
    variantAttribute.setId(ATTRIBUTE_ID);
    variantAttribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    variantAttribute.setMustShowOnCustomerSide(true);
    variantAttribute.setVariantCreation(true); // This should be filtered out

    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(variantAttribute);

    CommonImageBackfillingEventModel eventModel = new CommonImageBackfillingEventModel();
    eventModel.setProductCode(PRODUCT_CODE);
    eventModel.setStoreId(STORE_ID);

    // Mock services
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(product);
    when(categoryService.getCategoryAttributes(STORE_ID, CATEGORY_ID_1))
        .thenReturn(List.of(categoryAttribute));
    when(attributeService.findByAttributeIds(eq(STORE_ID), anyList()))
        .thenReturn(List.of(variantAttribute));
    when(productAttributeService.saveMissedProductAttributes(anyList()))
        .thenReturn(new ArrayList<>());

    // Set IMEI migration disabled
    ReflectionTestUtils.setField(service, "imeiMigrationEnabled", false);

    // Execute
    Pair<Product, List<ProductAttributeValue>> result = service.fetchProductAndInsertMissingProductAttributes(eventModel);

    // Verify - should return empty list because variant creation attributes are filtered out
    verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(categoryService).getCategoryAttributes(STORE_ID, CATEGORY_ID_1);
    verify(attributeService).findByAttributeIds(eq(STORE_ID), anyList());
    verify(productAttributeService).saveMissedProductAttributes(anyList());
    // No product attribute values should be created because variant creation attributes are filtered
    assertNotNull(result);
    if (result.getKey() != null) {
        assertEquals(product, result.getKey());
        assertTrue(result.getValue().isEmpty());
    } else {
        // When no valid attributes are found, the method may return null for the product
        assertNull(result.getKey());
        assertTrue(result.getValue().isEmpty());
    }
  }

  @Test
  public void fetchProductAndInsertMissingProductAttributes_WithNoAttributesToMap_ShouldReturnEmptyList() {
    // Setup
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    category.setId(CATEGORY_ID_1);
    productCategory.setCategory(category);
    productCategory.setCategoryId(CATEGORY_ID_1);
    product.setProductCategories(List.of(productCategory));

    // Product already has all category attributes
    Attribute existingAttribute = new Attribute();
    existingAttribute.setId(ATTRIBUTE_ID);
    ProductAttribute existingProductAttribute = new ProductAttribute();
    existingProductAttribute.setAttribute(existingAttribute);
    existingProductAttribute.setAttributeId(ATTRIBUTE_ID);
    product.setProductAttributes(List.of(existingProductAttribute));

    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(existingAttribute);

    CommonImageBackfillingEventModel eventModel = new CommonImageBackfillingEventModel();
    eventModel.setProductCode(PRODUCT_CODE);
    eventModel.setStoreId(STORE_ID);

    // Mock services
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(product);
    when(categoryService.getCategoryAttributes(STORE_ID, CATEGORY_ID_1))
        .thenReturn(List.of(categoryAttribute));

    // Set IMEI migration disabled
    ReflectionTestUtils.setField(service, "imeiMigrationEnabled", false);

    // Execute
    Pair<Product, List<ProductAttributeValue>> result = service.fetchProductAndInsertMissingProductAttributes(eventModel);

    // Verify - should return empty list because no new attributes to add
    verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(categoryService).getCategoryAttributes(STORE_ID, CATEGORY_ID_1);
    // Should not call attribute service because no missing attributes
    verify(attributeService, never()).findByAttributeIds(anyString(), anyList());
    assertNotNull(result);
    assertEquals(product, result.getKey());
    assertTrue(result.getValue().isEmpty());
  }

  @Test
  public void fetchProductAndInsertMissingProductAttributes_WithMigrationPayload_ShouldUsePayloadValues() {
    // Setup
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    category.setId(CATEGORY_ID_1);
    productCategory.setCategory(category);
    productCategory.setCategoryId(CATEGORY_ID_1);
    product.setProductCategories(List.of(productCategory));
    product.setProductAttributes(new ArrayList<>());

    // Create attribute
    Attribute attribute = new Attribute();
    attribute.setId(ATTRIBUTE_ID);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setVariantCreation(false);

    // Create migration payload
    MigrationPayload migrationPayload = new MigrationPayload();
    migrationPayload.setAttributeId(ATTRIBUTE_ID);
    migrationPayload.setAttributeValue("Custom Value");

    CommonImageBackfillingEventModel eventModel = new CommonImageBackfillingEventModel();
    eventModel.setProductCode(PRODUCT_CODE);
    eventModel.setStoreId(STORE_ID);
    eventModel.setMigrationPayloadList(List.of(migrationPayload));

    // Mock services
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(product);
    when(attributeService.findByAttributeIds(eq(STORE_ID), anyList()))
        .thenReturn(List.of(attribute));
    when(productAttributeService.saveMissedProductAttributes(anyList()))
        .thenAnswer(invocation -> {
            List<ProductAttribute> attrs = invocation.getArgument(0);
            return attrs.stream().map(attr -> {
                ProductAttribute saved = new ProductAttribute();
                saved.setId("saved-" + attr.getAttributeId());
                saved.setAttribute(attr.getAttribute());
                saved.setAttributeId(attr.getAttributeId());
                return saved;
            }).collect(Collectors.toList());
        });
    when(productAttributeValueService.saveProductAttributeValues(anyList()))
        .thenReturn(new ArrayList<>());

    // Execute
    Pair<Product, List<ProductAttributeValue>> result = service.fetchProductAndInsertMissingProductAttributes(eventModel);

    // Verify - should use migration payload value instead of default hyphen
    verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(attributeService).findByAttributeIds(eq(STORE_ID), anyList());
    verify(productAttributeService).saveMissedProductAttributes(anyList());
    verify(productAttributeValueService).saveProductAttributeValues(argThat(list -> 
        list.stream().anyMatch(pav -> 
            "Custom Value".equals(pav.getDescriptiveAttributeValue()))));
    assertNotNull(result);
    assertEquals(product, result.getKey());
  }

  @Test
  public void fetchProductAndInsertMissingProductAttributes_WithImeiMigrationDisabled_ShouldOnlyIncludeMustShowOnCustomer() {
    // Setup
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    category.setId(CATEGORY_ID_1);
    productCategory.setCategory(category);
    productCategory.setCategoryId(CATEGORY_ID_1);
    product.setProductCategories(List.of(productCategory));
    product.setProductAttributes(new ArrayList<>());

    // Create attributes - one with mustShowOnCustomerSide=false, one with true
    Attribute hiddenAttribute = new Attribute();
    hiddenAttribute.setId(ATTRIBUTE_ID);
    hiddenAttribute.setMustShowOnCustomerSide(false);
    hiddenAttribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    hiddenAttribute.setVariantCreation(false);

    Attribute visibleAttribute = new Attribute();
    visibleAttribute.setId(ATTRIBUTE_ID_2);
    visibleAttribute.setMustShowOnCustomerSide(true);
    visibleAttribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    visibleAttribute.setVariantCreation(false);

    CategoryAttribute categoryAttribute1 = new CategoryAttribute();
    categoryAttribute1.setAttribute(hiddenAttribute);
    CategoryAttribute categoryAttribute2 = new CategoryAttribute();
    categoryAttribute2.setAttribute(visibleAttribute);

    CommonImageBackfillingEventModel eventModel = new CommonImageBackfillingEventModel();
    eventModel.setProductCode(PRODUCT_CODE);
    eventModel.setStoreId(STORE_ID);

    // Mock services
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(product);
    when(categoryService.getCategoryAttributes(STORE_ID, CATEGORY_ID_1))
        .thenReturn(List.of(categoryAttribute1, categoryAttribute2));
    when(attributeService.findByAttributeIds(eq(STORE_ID), anyList()))
        .thenReturn(List.of(visibleAttribute)); // Only visible attribute should be returned
    when(productAttributeService.saveMissedProductAttributes(anyList()))
        .thenAnswer(invocation -> {
            List<ProductAttribute> attrs = invocation.getArgument(0);
            return attrs.stream().map(attr -> {
                ProductAttribute saved = new ProductAttribute();
                saved.setId("saved-" + attr.getAttributeId());
                saved.setAttribute(attr.getAttribute());
                saved.setAttributeId(attr.getAttributeId());
                return saved;
            }).collect(Collectors.toList());
        });
    when(predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
        eq(STORE_ID), any(Attribute.class), eq(Constants.HYPHEN)))
        .thenReturn(new PredefinedAllowedAttributeValue());
    when(productAttributeValueService.saveProductAttributeValues(anyList()))
        .thenReturn(new ArrayList<>());

    // Set IMEI migration disabled - this should filter out mustShowOnCustomerSide=false
    ReflectionTestUtils.setField(service, "imeiMigrationEnabled", false);

    // Execute
    Pair<Product, List<ProductAttributeValue>> result = service.fetchProductAndInsertMissingProductAttributes(eventModel);

    // Verify - should only process the visible attribute
    verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(categoryService).getCategoryAttributes(STORE_ID, CATEGORY_ID_1);
    verify(attributeService).findByAttributeIds(eq(STORE_ID), argThat(list -> 
        list.size() == 1 && list.contains(ATTRIBUTE_ID_2)));
    verify(productAttributeService).saveMissedProductAttributes(argThat(list -> 
        list.size() == 1 && list.get(0).getAttributeId().equals(ATTRIBUTE_ID_2)));
    verify(predefinedAllowedAttributeValueService).findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
        eq(STORE_ID), any(Attribute.class), eq(Constants.HYPHEN));
    verify(productAttributeValueService).saveProductAttributeValues(anyList());
    assertNotNull(result);
    assertEquals(product, result.getKey());
  }

  @Test
  public void fetchProductAndInsertMissingProductAttributes_WithImeiMigrationEnabledAndExistingProductAttribute_ShouldReuseExisting() {
    // Setup
    Product product = new Product();
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setStoreId(STORE_ID);
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    category.setId(CATEGORY_ID_1);
    productCategory.setCategory(category);
    productCategory.setCategoryId(CATEGORY_ID_1);
    product.setProductCategories(List.of(productCategory));

    // Create an attribute that exists in both category and product (to trigger reuse)
    Attribute sharedAttribute = new Attribute();
    sharedAttribute.setId(ATTRIBUTE_ID);
    sharedAttribute.setName("Shared Attribute");
    sharedAttribute.setMustShowOnCustomerSide(true);
    sharedAttribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    sharedAttribute.setVariantCreation(false);
    sharedAttribute.setStoreId(STORE_ID);

    // Create an existing product attribute that MATCHES the category attribute ID
    ProductAttribute existingProductAttribute = new ProductAttribute();
    existingProductAttribute.setId("existing-product-attr-id");
    existingProductAttribute.setAttribute(sharedAttribute);
    existingProductAttribute.setAttributeId(ATTRIBUTE_ID); // This MATCHES the category attribute ID
    existingProductAttribute.setProduct(product);
    existingProductAttribute.setProductId(product.getId());
    existingProductAttribute.setStoreId(STORE_ID);
    
    // Create a second attribute that doesn't exist in product (to trigger new creation)
    Attribute newAttribute = new Attribute();
    newAttribute.setId(ATTRIBUTE_ID_2);
    newAttribute.setName("New Attribute");
    newAttribute.setMustShowOnCustomerSide(true);
    newAttribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    newAttribute.setVariantCreation(false);
    newAttribute.setStoreId(STORE_ID);
    
    // Set the existing product attributes (only has one attribute)
    product.setProductAttributes(List.of(existingProductAttribute));

    CategoryAttribute categoryAttribute1 = new CategoryAttribute();
    categoryAttribute1.setAttribute(sharedAttribute);
    CategoryAttribute categoryAttribute2 = new CategoryAttribute();
    categoryAttribute2.setAttribute(newAttribute);

    CommonImageBackfillingEventModel eventModel = new CommonImageBackfillingEventModel();
    eventModel.setProductCode(PRODUCT_CODE);
    eventModel.setStoreId(STORE_ID);

    // Mock services
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(product);
    when(categoryService.getCategoryAttributes(STORE_ID, CATEGORY_ID_1))
        .thenReturn(List.of(categoryAttribute1, categoryAttribute2));
    when(attributeService.findByAttributeIds(eq(STORE_ID), anyList()))
        .thenReturn(List.of(sharedAttribute, newAttribute));
    
    // Mock the product attribute service to return both existing and new product attributes
    when(productAttributeService.saveMissedProductAttributes(anyList()))
        .thenAnswer(invocation -> {
            List<ProductAttribute> attrs = invocation.getArgument(0);
            // Should have 2 attributes: one reused existing, one new
            assertEquals(2, attrs.size());
            
            // Find the reused attribute (should have existing ID)
            boolean hasReusedAttribute = attrs.stream().anyMatch(attr -> 
                existingProductAttribute.getId().equals(attr.getId()) && 
                ATTRIBUTE_ID.equals(attr.getAttributeId()));
            assertTrue(hasReusedAttribute, "Should reuse existing product attribute");
            
            // Find the new attribute (should have null ID and ATTRIBUTE_ID_2)
            boolean hasNewAttribute = attrs.stream().anyMatch(attr -> 
                attr.getId() == null && 
                ATTRIBUTE_ID_2.equals(attr.getAttributeId()));
            assertTrue(hasNewAttribute, "Should create new product attribute");
            
            // Set IDs for the returned attributes to avoid null pointer issues
            return attrs.stream().map(attr -> {
                ProductAttribute savedAttr = new ProductAttribute();
                savedAttr.setId(attr.getId() != null ? attr.getId() : "new-attr-id");
                savedAttr.setAttribute(attr.getAttribute());
                savedAttr.setAttributeId(attr.getAttributeId());
                savedAttr.setProduct(attr.getProduct());
                savedAttr.setProductId(attr.getProductId());
                savedAttr.setStoreId(attr.getStoreId());
                return savedAttr;
            }).collect(Collectors.toList());
        });
    
    when(predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
        eq(STORE_ID), any(Attribute.class), eq(Constants.HYPHEN)))
        .thenReturn(new PredefinedAllowedAttributeValue());
    
    when(productAttributeValueService.getProductAttributeValuesByStoreIdAndProductAttributeIdsAndMarkForDeleteFalseCached(
        eq(STORE_ID), any(Set.class)))
        .thenReturn(new ArrayList<>());
    
    when(productAttributeValueService.saveProductAttributeValues(anyList()))
        .thenReturn(new ArrayList<>());

    // Set IMEI migration enabled - this should trigger reuse of existing product attributes
    ReflectionTestUtils.setField(service, "imeiMigrationEnabled", true);

    // Execute
    Pair<Product, List<ProductAttributeValue>> result = service.fetchProductAndInsertMissingProductAttributes(eventModel);

    // Verify
    verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(categoryService).getCategoryAttributes(STORE_ID, CATEGORY_ID_1);
    verify(attributeService).findByAttributeIds(eq(STORE_ID), anyList());
    
    // Verify that both existing product attribute is reused AND new one is created
    verify(productAttributeService).saveMissedProductAttributes(argThat(list -> {
        if (list.size() != 2) return false;
        
        // Check for reused attribute
        boolean hasReused = list.stream().anyMatch(attr -> 
            existingProductAttribute.getId().equals(attr.getId()) && 
            ATTRIBUTE_ID.equals(attr.getAttributeId()));
            
        // Check for new attribute
        boolean hasNew = list.stream().anyMatch(attr -> 
            attr.getId() == null && 
            ATTRIBUTE_ID_2.equals(attr.getAttributeId()));
            
        return hasReused && hasNew;
    }));
    
    verify(productAttributeValueService, times(2)).getProductAttributeValuesByStoreIdAndProductAttributeIdsAndMarkForDeleteFalseCached(
        eq(STORE_ID), any(Set.class));
    verify(predefinedAllowedAttributeValueService, times(2)).findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
        eq(STORE_ID), any(Attribute.class), eq(Constants.HYPHEN));
    verify(productAttributeValueService).saveProductAttributeValues(anyList());
    
    assertNotNull(result);
    assertEquals(product, result.getKey());
  }

  @Test
  public void fetchProductAndInsertMissingProductAttributes_WithExistingProductAttributeValues_ShouldMarkForDelete() {
    // Setup
    Product product = new Product();
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setStoreId(STORE_ID);
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    category.setId(CATEGORY_ID_1);
    productCategory.setCategory(category);
    productCategory.setCategoryId(CATEGORY_ID_1);
    product.setProductCategories(List.of(productCategory));
    product.setProductAttributes(new ArrayList<>());

    // Create a predefined attribute
    Attribute predefinedAttribute = new Attribute();
    predefinedAttribute.setId(ATTRIBUTE_ID);
    predefinedAttribute.setName("Predefined Attribute");
    predefinedAttribute.setMustShowOnCustomerSide(true);
    predefinedAttribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    predefinedAttribute.setVariantCreation(false);
    predefinedAttribute.setStoreId(STORE_ID);

    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(predefinedAttribute);

    CommonImageBackfillingEventModel eventModel = new CommonImageBackfillingEventModel();
    eventModel.setProductCode(PRODUCT_CODE);
    eventModel.setStoreId(STORE_ID);

    // Create existing product attribute values that should be marked for deletion
    ProductAttributeValue existingValue1 = new ProductAttributeValue();
    existingValue1.setId("existing-value-1");
    existingValue1.setMarkForDelete(false);
    existingValue1.setStoreId(STORE_ID);
    
    ProductAttributeValue existingValue2 = new ProductAttributeValue();
    existingValue2.setId("existing-value-2");
    existingValue2.setMarkForDelete(false);
    existingValue2.setStoreId(STORE_ID);

    List<ProductAttributeValue> existingValues = List.of(existingValue1, existingValue2);

    // Mock services
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(product);
    when(categoryService.getCategoryAttributes(STORE_ID, CATEGORY_ID_1))
        .thenReturn(List.of(categoryAttribute));
    when(attributeService.findByAttributeIds(eq(STORE_ID), anyList()))
        .thenReturn(List.of(predefinedAttribute));
    when(predefinedAllowedAttributeValueService.findByStoreIdAndId(anyString(),
        any())).thenReturn(new PredefinedAllowedAttributeValue());
    
    when(productAttributeService.saveMissedProductAttributes(anyList()))
        .thenAnswer(invocation -> {
            List<ProductAttribute> attrs = invocation.getArgument(0);
            return attrs.stream().map(attr -> {
                ProductAttribute saved = new ProductAttribute();
                saved.setId("saved-" + attr.getAttributeId());
                saved.setAttribute(attr.getAttribute());
                saved.setAttributeId(attr.getAttributeId());
                return saved;
            }).collect(Collectors.toList());
        });
    
    // Mock to return existing values that need to be marked for deletion
    when(productAttributeValueService.getProductAttributeValuesByStoreIdAndProductAttributeIdsAndMarkForDeleteFalseCached(
        eq(STORE_ID), any(Set.class)))
        .thenReturn(new ArrayList<>(existingValues)); // Return mutable list
    
    when(predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
        eq(STORE_ID), any(Attribute.class), eq(Constants.HYPHEN)))
        .thenReturn(new PredefinedAllowedAttributeValue());
    
    when(productAttributeValueService.saveProductAttributeValues(anyList()))
        .thenAnswer(invocation -> {
            List<ProductAttributeValue> values = invocation.getArgument(0);
            // Verify that existing values are marked for deletion
            long markedForDeletionCount = values.stream()
                .filter(ProductAttributeValue::isMarkForDelete)
                .count();
            assertTrue(markedForDeletionCount >= 2, "Should have at least 2 values marked for deletion");
            return values;
        });

    // Set IMEI migration enabled to trigger the forEach logic
    ReflectionTestUtils.setField(service, "imeiMigrationEnabled", true);

    // Execute
    Pair<Product, List<ProductAttributeValue>> result = service.fetchProductAndInsertMissingProductAttributes(eventModel);

    // Verify
    verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(categoryService).getCategoryAttributes(STORE_ID, CATEGORY_ID_1);
    verify(attributeService).findByAttributeIds(eq(STORE_ID), anyList());
    verify(productAttributeService).saveMissedProductAttributes(anyList());
    verify(productAttributeValueService).getProductAttributeValuesByStoreIdAndProductAttributeIdsAndMarkForDeleteFalseCached(
        eq(STORE_ID), any(Set.class));
    verify(predefinedAllowedAttributeValueService).findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
        eq(STORE_ID), any(Attribute.class), eq(Constants.HYPHEN));
    verify(productAttributeValueService).saveProductAttributeValues(anyList());
    verify(predefinedAllowedAttributeValueService, times(2)).findByStoreIdAndId(anyString(), any());

    assertNotNull(result);
    assertEquals(product, result.getKey());
  }

  @Test
  public void fetchProductAndInsertMissingProductAttributes_WithDuplicateProductAttributeIds_ShouldUseMergeFunction() {
    // Setup
    Product product = new Product();
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setStoreId(STORE_ID);
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    category.setId(CATEGORY_ID_1);
    productCategory.setCategory(category);
    productCategory.setCategoryId(CATEGORY_ID_1);
    product.setProductCategories(List.of(productCategory));

    // Create attributes
    Attribute existingAttribute = new Attribute();
    existingAttribute.setId(ATTRIBUTE_ID);
    existingAttribute.setName("Existing Attribute");
    existingAttribute.setMustShowOnCustomerSide(true);
    existingAttribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    existingAttribute.setVariantCreation(false);
    existingAttribute.setStoreId(STORE_ID);

    Attribute newAttribute = new Attribute();
    newAttribute.setId(ATTRIBUTE_ID_2);
    newAttribute.setName("New Attribute");
    newAttribute.setMustShowOnCustomerSide(true);
    newAttribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    newAttribute.setVariantCreation(false);
    newAttribute.setStoreId(STORE_ID);

    // Create product attributes with DUPLICATE attribute IDs to trigger merge function
    ProductAttribute productAttribute1 = new ProductAttribute();
    productAttribute1.setId("product-attr-1");
    productAttribute1.setAttribute(existingAttribute);
    productAttribute1.setAttributeId(ATTRIBUTE_ID); // Same attribute ID
    productAttribute1.setProduct(product);
    productAttribute1.setStoreId(STORE_ID);

    ProductAttribute productAttribute2 = new ProductAttribute();
    productAttribute2.setId("product-attr-2");
    productAttribute2.setAttribute(existingAttribute);
    productAttribute2.setAttributeId(ATTRIBUTE_ID); // Same attribute ID - this will trigger merge function
    productAttribute2.setProduct(product);
    productAttribute2.setStoreId(STORE_ID);

    // Set product attributes with duplicates (this will test the merge function)
    product.setProductAttributes(List.of(productAttribute1, productAttribute2));

    CategoryAttribute categoryAttribute1 = new CategoryAttribute();
    categoryAttribute1.setAttribute(existingAttribute);
    CategoryAttribute categoryAttribute2 = new CategoryAttribute();
    categoryAttribute2.setAttribute(newAttribute);

    CommonImageBackfillingEventModel eventModel = new CommonImageBackfillingEventModel();
    eventModel.setProductCode(PRODUCT_CODE);
    eventModel.setStoreId(STORE_ID);

    // Mock services
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(product);
    when(categoryService.getCategoryAttributes(STORE_ID, CATEGORY_ID_1))
        .thenReturn(List.of(categoryAttribute1, categoryAttribute2));
    when(attributeService.findByAttributeIds(eq(STORE_ID), anyList()))
        .thenReturn(List.of(existingAttribute, newAttribute));
    
    when(productAttributeService.saveMissedProductAttributes(anyList()))
        .thenAnswer(invocation -> {
            List<ProductAttribute> attrs = invocation.getArgument(0);
            // Should have 2 attributes: 1 reused (with merge function applied), 1 new
            assertEquals(2, attrs.size());
            
            // Verify merge function worked - should have the first product attribute (oldValue)
            boolean hasReusedWithMerge = attrs.stream().anyMatch(attr -> 
                "product-attr-1".equals(attr.getId()) && ATTRIBUTE_ID.equals(attr.getAttributeId()));
            assertTrue(hasReusedWithMerge, "Should have reused first product attribute due to merge function");
            
            // Should also have new attribute
            boolean hasNewAttribute = attrs.stream().anyMatch(attr -> 
                attr.getId() == null && ATTRIBUTE_ID_2.equals(attr.getAttributeId()));
            assertTrue(hasNewAttribute, "Should have new product attribute");
            
            return attrs.stream().map(attr -> {
                ProductAttribute saved = new ProductAttribute();
                saved.setId(attr.getId() != null ? attr.getId() : "new-saved-id");
                saved.setAttribute(attr.getAttribute());
                saved.setAttributeId(attr.getAttributeId());
                return saved;
            }).collect(Collectors.toList());
        });
    
    when(productAttributeValueService.getProductAttributeValuesByStoreIdAndProductAttributeIdsAndMarkForDeleteFalseCached(
        eq(STORE_ID), any(Set.class)))
        .thenReturn(new ArrayList<>());
    
    when(predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
        eq(STORE_ID), any(Attribute.class), eq(Constants.HYPHEN)))
        .thenReturn(new PredefinedAllowedAttributeValue());
    
    when(productAttributeValueService.saveProductAttributeValues(anyList()))
        .thenReturn(new ArrayList<>());

    // Set IMEI migration enabled to trigger reuse logic
    ReflectionTestUtils.setField(service, "imeiMigrationEnabled", true);

    // Execute
    Pair<Product, List<ProductAttributeValue>> result = service.fetchProductAndInsertMissingProductAttributes(eventModel);

    // Verify
    verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(categoryService).getCategoryAttributes(STORE_ID, CATEGORY_ID_1);
    verify(attributeService).findByAttributeIds(eq(STORE_ID), anyList());
    verify(productAttributeService).saveMissedProductAttributes(argThat(list -> {
        if (list.size() != 2) return false;
        
        // Check for reused attribute (merge function applied)
        boolean hasReused = list.stream().anyMatch(attr -> 
            "product-attr-1".equals(attr.getId()) && ATTRIBUTE_ID.equals(attr.getAttributeId()));
            
        // Check for new attribute
        boolean hasNew = list.stream().anyMatch(attr -> 
            attr.getId() == null && ATTRIBUTE_ID_2.equals(attr.getAttributeId()));
            
        return hasReused && hasNew;
    }));
    verify(productAttributeValueService, times(2)).getProductAttributeValuesByStoreIdAndProductAttributeIdsAndMarkForDeleteFalseCached(
        eq(STORE_ID), any(Set.class));
    verify(predefinedAllowedAttributeValueService, times(2)).findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
        eq(STORE_ID), any(Attribute.class), eq(Constants.HYPHEN));
    verify(productAttributeValueService).saveProductAttributeValues(anyList());
    
    assertNotNull(result);
    assertEquals(product, result.getKey());
  }

  @Test
  public void fetchProductAndInsertMissingProductAttributesWithMigrationPayloadTestWithNoMissingAttr() {
    CommonImageBackfillingEventModel productAttributeDataBackFillingEventModel =
        getCommonImageBackfillingEventModel();
    productAttributeDataBackFillingEventModel.setMigrationPayloadList(new ArrayList<>());
    Product product = new Product();
    ProductAttribute productAttribute = new ProductAttribute();
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCategoryId(CATEGORY_ID_1);
    productCategory.setProduct(product);
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setId(ATTRIBUTE_ID);
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    productAttribute.setAttributeId(ATTRIBUTE_ID);
    productAttribute.setProduct(product);
    productAttribute.setAttribute(attribute);
    categoryAttribute.setAttribute(attribute);
    product.setProductAttributes(List.of(productAttribute));
    product.setProductCategories(List.of(productCategory));
    Mockito.when(categoryService.getCategoryAttributes(Constants.DEFAULT_STORE_ID, CATEGORY_ID_1))
        .thenReturn(List.of(categoryAttribute));
    Mockito.when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        productAttributeDataBackFillingEventModel.getStoreId(), PRODUCT_CODE)).thenReturn(product);
    Mockito.when(attributeService.findByAttributeIds(Mockito.eq(Constants.DEFAULT_STORE_ID),
        anyList())).thenReturn(List.of(attribute));
    service.fetchProductAndInsertMissingProductAttributes(
        productAttributeDataBackFillingEventModel);
    Mockito.verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        productAttributeDataBackFillingEventModel.getStoreId(),
        productAttributeDataBackFillingEventModel.getProductCode());
    Mockito.verify(categoryService).getCategoryAttributes(STORE_ID, CATEGORY_ID_1);
  }

  @Test
  void regenerateProductAttributeContent_notDeleteDsExtractedAttribute() {
    Product savedProduct = new Product();
    Attribute dsAttribute = new Attribute();
    dsAttribute.setDsExtraction(true);
    dsAttribute.setAttributeCode(ATTRIBUTE_CODE+"2");
    dsAttribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    Attribute attribute = new Attribute();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    ProductAttribute productAttribute_1 = new ProductAttribute();
    productAttribute_1.setAttribute(dsAttribute);
    ProductAttribute productAttribute_2 = new ProductAttribute();
    productAttribute_2.setAttribute(attribute);
    Product newProduct = new Product();
    newProduct.getProductAttributes().add(productAttribute2);
    savedProduct.getProductAttributes().addAll(Arrays.asList(productAttribute_1, productAttribute_2));
    service.regenerateProductAttributeContent(savedProduct, newProduct, true, true);
    Assertions.assertTrue(savedProduct.getProductAttributes().get(0).isMarkForDelete());
  }

  private static CommonImageBackfillingEventModel getCommonImageBackfillingEventModel() {
    CommonImageBackfillingEventModel productAttributeDataBackFillingEventModel =
        new CommonImageBackfillingEventModel();
    MigrationPayload migrationPayload = new MigrationPayload();
    migrationPayload.setAttributeId(ATTRIBUTE_ID);
    migrationPayload.setAttributeValue(ATTRIBUTE_VALUE);
    migrationPayload.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    productAttributeDataBackFillingEventModel.setStoreId(Constants.DEFAULT_STORE_ID);
    productAttributeDataBackFillingEventModel.setProductCode(PRODUCT_CODE);
    productAttributeDataBackFillingEventModel.setMigrationPayloadList(List.of(migrationPayload));
    return productAttributeDataBackFillingEventModel;
  }

  @Test
  public void testGetProductBasicInfoByProductCodes() throws Exception {
    List<String> productCodes = Arrays.asList(PRODUCT_CODE);
    Product mockProduct = new Product();
    mockProduct.setDescription(DESKRIPSI_BARANG.getBytes());
    mockProduct.setId(PRODUCT_ID);
    mockProduct.setProductImages(new ArrayList<>());
    when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(mockProduct);
    when(imageService.getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID)).thenReturn(productImages);
    List<BasicInfoProductResponse> responses = service.getProductBasicInfoByProductCodes(STORE_ID, productCodes);
    Assertions.assertNotNull(responses);
    assertEquals(productCodes.size(), responses.size());
    verify(applicationContext).getBean(ProductService.class);
    verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
  }
  @Test
  public void deleteMfdTrueRowsFromProduct_AllIdListsEmpty_NoDeletions() throws Exception {
    Product product = new Product();
    product.setProductAttributes(new ArrayList<>());
    service.deleteMfdTrueRowsFromProduct(product);
    Assertions.assertNotNull(product);
  }

  @Test
  public void deleteMfdTrueRowsFromProduct_AllIdListsEmpty_NoDeletions1() throws Exception {
    Product product = new Product();
    product.setId(PRODUCT_ID);
    product.setStoreId(STORE_ID);
    ProductCategory productCategory = new ProductCategory();
    productCategory.setId("categoryId1");
    productCategory.setMarkForDelete(true);
    product.setProductCategories(Collections.singletonList(productCategory));
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setId("attributeId1");
    productAttribute.setMarkForDelete(true);
    ProductAttributeValue attributeValue = new ProductAttributeValue();
    attributeValue.setId("attributeValueId1");
    attributeValue.setMarkForDelete(true);
    productAttribute.setProductAttributeValues(Collections.singletonList(attributeValue));
    product.setProductAttributes(Collections.singletonList(productAttribute));
    ProductImage productImage = new ProductImage();
    productImage.setId("imageId1");
    productImage.setMarkForDelete(true);
    product.setProductImages(Collections.singletonList(productImage));
    ProductItem productItem = new ProductItem();
    productItem.setId("itemId1");
    productItem.setMarkForDelete(true);
    ProductItemImage itemImage = new ProductItemImage();
    itemImage.setId("itemImageId1");
    itemImage.setMarkForDelete(true);
    productItem.setProductItemImages(Collections.singletonList(itemImage));
    ProductItemAttributeValue itemAttributeValue = new ProductItemAttributeValue();
    itemAttributeValue.setId("itemAttributeValueId1");
    itemAttributeValue.setMarkForDelete(true);
    productItem.setProductItemAttributeValues(Collections.singletonList(itemAttributeValue));
    product.setProductItems(Collections.singletonList(productItem));
    service.deleteMfdTrueRowsFromProduct(product);
    verify(productCategoryService).deleteByProductCategoryIds(Collections.singletonList("categoryId1"));
    verify(imageService).deleteProductImagesByIds(Collections.singletonList("imageId1"));
    verify(imageService).deleteProductItemImagesByIds(Collections.singletonList("itemImageId1"));
    verify(productAttributeService).deleteByProductAttributeIds(Collections.singletonList("attributeId1"));
    verify(productItemAttributeValueService).deleteByProductItemAttributeIds(Collections.singletonList("itemAttributeValueId1"));
  }

  @Test
  public void updateMasterDataAndGenerateHistory_AllFieldsUpdated_Success() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = "testUser";
    ProductMasterDataUpdateRequest request = createProductMasterDataUpdateRequest();
    Product savedProduct = createSavedProduct();
    
    // Mock the productItemServiceWrapper method
    doNothing().when(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        storeId, savedProduct, false);
    
    // When
    AuditTrailListResponse response = service.updateMasterDataAndGenerateHistory(storeId, username, request, savedProduct);
    
    // Then
    Assertions.assertNotNull(response);
    Assertions.assertEquals(8, response.getAuditTrailResponseList().size()); // name, description, dimensions (5), url
    Assertions.assertEquals(Constants.CLIENT_ID, response.getClientId());
    Assertions.assertEquals(Constants.CLIENT_ID, response.getAccessChannel());
    Assertions.assertEquals(username, response.getChangedBy());
    Assertions.assertTrue(response.isUpdateDirectly());
    Assertions.assertTrue(response.isUpdateDirectlyToDB());
    Assertions.assertNotNull(response.getRequestId());
    
    // Verify audit trail entries
    AuditTrailDto nameAudit = response.getAuditTrailResponseList().stream()
        .filter(audit -> Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_NAME.equals(audit.getActionKey()))
        .findFirst().orElse(null);
    Assertions.assertNotNull(nameAudit);
    Assertions.assertEquals("New Product Name", nameAudit.getNewValue());
    Assertions.assertEquals("Old Product Name", nameAudit.getOldValue());
    
    // Verify product was updated
    Assertions.assertEquals("New Product Name", savedProduct.getName());
    Assertions.assertEquals("New Description", new String(savedProduct.getDescription()));
    Assertions.assertEquals(100.0, savedProduct.getLength());
    Assertions.assertEquals(50.0, savedProduct.getWidth());
    Assertions.assertEquals(30.0, savedProduct.getHeight());
    Assertions.assertEquals(5.0, savedProduct.getWeight());
    Assertions.assertEquals(6.0, savedProduct.getShippingWeight());
    Assertions.assertEquals("http://newvideo.com", savedProduct.getUrl());
    
    // Verify the mock was called
    verify(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        storeId, savedProduct, false);
  }

  @Test
  public void updateMasterDataAndGenerateHistory_OnlyNameUpdated_Success() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = "testUser";
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku("TEST-SKU");
    request.setName("New Product Name");
    request.setDescription("Same Description".getBytes());
    request.setBusinessPartnerCode("BP001");
    request.setUrl("http://samevideo.com");
    
    Product savedProduct = new Product();
    savedProduct.setName("Old Product Name");
    savedProduct.setDescription("Same Description".getBytes());
    savedProduct.setUrl("http://samevideo.com");
    
    // Mock the productItemServiceWrapper method
    doNothing().when(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        storeId, savedProduct, false);
    
    // When
    AuditTrailListResponse response = service.updateMasterDataAndGenerateHistory(storeId, username, request, savedProduct);
    
    // Then
    Assertions.assertNotNull(response);
    Assertions.assertEquals(1, response.getAuditTrailResponseList().size());
    
    AuditTrailDto nameAudit = response.getAuditTrailResponseList().get(0);
    Assertions.assertEquals(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_NAME, nameAudit.getActionKey());
    Assertions.assertEquals("New Product Name", nameAudit.getNewValue());
    Assertions.assertEquals("Old Product Name", nameAudit.getOldValue());
    Assertions.assertEquals("TEST-SKU", nameAudit.getProductSku());
    Assertions.assertEquals("BP001", nameAudit.getBusinessPartnerCode());
    
    // Verify product was updated
    Assertions.assertEquals("New Product Name", savedProduct.getName());
    
    // Verify the mock was called
    verify(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        storeId, savedProduct, false);
  }

  @Test
  public void updateMasterDataAndGenerateHistory_OnlyDescriptionUpdated_Success() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = "testUser";
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku("TEST-SKU");
    request.setName("Same Name");
    request.setDescription("New Description".getBytes());
    request.setBusinessPartnerCode("BP001");
    
    Product savedProduct = new Product();
    savedProduct.setName("Same Name");
    savedProduct.setDescription("Old Description".getBytes());
    savedProduct.setUrl(StringUtils.EMPTY);
    
    // When
    AuditTrailListResponse response = service.updateMasterDataAndGenerateHistory(storeId, username, request, savedProduct);
    
    // Then
    Assertions.assertNotNull(response);
    Assertions.assertEquals(1, response.getAuditTrailResponseList().size());
    
    AuditTrailDto descAudit = response.getAuditTrailResponseList().get(0);
    Assertions.assertEquals(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_DESCRIPTION, descAudit.getActionKey());
    Assertions.assertEquals("New Description", descAudit.getNewValue());
    Assertions.assertEquals("Old Description", descAudit.getOldValue());
    
    // Verify product was updated
    Assertions.assertEquals("New Description", new String(savedProduct.getDescription()));
  }

  @Test
  public void updateMasterDataAndGenerateHistory_OnlyDimensionsUpdated_Success() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = "testUser";
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku("TEST-SKU");
    request.setName("Same Name");
    request.setDescription("Same Description".getBytes());
    request.setLength(100.0);
    request.setWidth(50.0);
    request.setHeight(30.0);
    request.setWeight(5.0);
    request.setShippingWeight(6.0);
    request.setBusinessPartnerCode("BP001");
    
    Product savedProduct = new Product();
    savedProduct.setName("Same Name");
    savedProduct.setDescription("Same Description".getBytes());
    savedProduct.setLength(80.0);
    savedProduct.setWidth(40.0);
    savedProduct.setHeight(25.0);
    savedProduct.setWeight(4.0);
    savedProduct.setShippingWeight(5.0);
    savedProduct.setUrl(StringUtils.EMPTY);
    
    // When
    AuditTrailListResponse response = service.updateMasterDataAndGenerateHistory(storeId, username, request, savedProduct);
    
    // Then
    Assertions.assertNotNull(response);
    Assertions.assertEquals(5, response.getAuditTrailResponseList().size()); // length, width, height, weight, shipping weight
    
    // Verify all dimension audit trails
    Map<String, AuditTrailDto> auditMap = response.getAuditTrailResponseList().stream()
        .collect(java.util.stream.Collectors.toMap(AuditTrailDto::getActionKey, audit -> audit));
    
    Assertions.assertEquals("100.0", auditMap.get(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_LENGTH).getNewValue());
    Assertions.assertEquals("80.0", auditMap.get(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_LENGTH).getOldValue());
    
    Assertions.assertEquals("50.0", auditMap.get(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_WIDTH).getNewValue());
    Assertions.assertEquals("40.0", auditMap.get(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_WIDTH).getOldValue());
    
    Assertions.assertEquals("30.0", auditMap.get(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_HEIGHT).getNewValue());
    Assertions.assertEquals("25.0", auditMap.get(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_HEIGHT).getOldValue());
    
    Assertions.assertEquals("5.0", auditMap.get(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_WEIGHT).getNewValue());
    Assertions.assertEquals("4.0", auditMap.get(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_WEIGHT).getOldValue());
    
    Assertions.assertEquals("6.0", auditMap.get(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_SHIPPING_WEIGHT).getNewValue());
    Assertions.assertEquals("5.0", auditMap.get(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_SHIPPING_WEIGHT).getOldValue());
    
    // Verify product was updated
    Assertions.assertEquals(100.0, savedProduct.getLength());
    Assertions.assertEquals(50.0, savedProduct.getWidth());
    Assertions.assertEquals(30.0, savedProduct.getHeight());
    Assertions.assertEquals(5.0, savedProduct.getWeight());
    Assertions.assertEquals(6.0, savedProduct.getShippingWeight());
  }

  @Test
  public void updateMasterDataAndGenerateHistory_OnlyUrlUpdated_Success() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = "testUser";
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku("TEST-SKU");
    request.setName("Same Name");
    request.setDescription("Same Description".getBytes());
    request.setUrl("http://newvideo.com");
    request.setBusinessPartnerCode("BP001");
    
    Product savedProduct = new Product();
    savedProduct.setName("Same Name");
    savedProduct.setDescription("Same Description".getBytes());
    savedProduct.setUrl("http://oldvideo.com");
    
    // When
    AuditTrailListResponse response = service.updateMasterDataAndGenerateHistory(storeId, username, request, savedProduct);
    
    // Then
    Assertions.assertNotNull(response);
    Assertions.assertEquals(1, response.getAuditTrailResponseList().size());
    
    AuditTrailDto urlAudit = response.getAuditTrailResponseList().get(0);
    Assertions.assertEquals(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_URL, urlAudit.getActionKey());
    Assertions.assertEquals("http://newvideo.com", urlAudit.getNewValue());
    Assertions.assertEquals("http://oldvideo.com", urlAudit.getOldValue());
    
    // Verify product was updated
    Assertions.assertEquals("http://newvideo.com", savedProduct.getUrl());
  }

  @Test
  public void updateMasterDataAndGenerateHistory_VideoUpdated_Success_withoutHistory() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = "testUser";

    VideoDTO videoDTO = new VideoDTO();
    videoDTO.setVideoId("VIDEO-ID");
    videoDTO.setSourceUrl("http://newvideo.com");

    VideoAddEditRequest videoRequest = new VideoAddEditRequest();
    videoRequest.setVideoId("VIDEO-ID");
    videoRequest.setVideoUrl("http://newvideo.com");
    
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku("TEST-SKU");
    request.setName("Same Name");
    request.setDescription("Same Description".getBytes());
    request.setVideoAddEditRequest(videoRequest);
    request.setBusinessPartnerCode("BP001");
    
    Product savedProduct = new Product();
    savedProduct.setName("Same Name");
    savedProduct.setDescription("Same Description".getBytes());
    savedProduct.setVideo("{\"sourceUrl\":\"http://oldvideo.com\"}");
    savedProduct.setUrl(StringUtils.EMPTY);
    
    // Mock the objectMapper call
    String expectedVideoJson = "{\"sourceUrl\":\"http://newvideo.com\"}";
    // When
    AuditTrailListResponse response = service.updateMasterDataAndGenerateHistory(storeId, username, request, savedProduct);
    
    // Then
    Assertions.assertNull(response);
    // Video update doesn't generate audit trail in current implementation, but video is updated
  }

  @Test
  public void updateMasterDataAndGenerateHistory_VideoUpdated_withEmptyVideoId_Success_withoutHistory() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = "testUser";
    VideoAddEditRequest videoRequest = new VideoAddEditRequest();

    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku("TEST-SKU");
    request.setName("Same Name");
    request.setDescription("Same Description".getBytes());
    request.setVideoAddEditRequest(videoRequest);
    request.setBusinessPartnerCode("BP001");
    request.setVideoDelete(true);

    Product savedProduct = new Product();
    savedProduct.setName("Same Name");
    savedProduct.setDescription("Same Description".getBytes());
    savedProduct.setVideo("{\"videoUrl\":\"http://oldvideo.com\"}");
    savedProduct.setUrl(StringUtils.EMPTY);

    // Mock the objectMapper call
    String expectedVideoJson = null;
    when(objectMapper.writeValueAsString(videoRequest)).thenReturn(expectedVideoJson);

    // When
    AuditTrailListResponse response = service.updateMasterDataAndGenerateHistory(storeId, username, request, savedProduct);

    // Then
    Assertions.assertNull(response);
    // Video update doesn't generate audit trail in current implementation, but video is updated
    Assertions.assertEquals(expectedVideoJson, savedProduct.getVideo());
  }

  @Test
  public void updateMasterDataAndGenerateHistory_WithNullDescription_Success() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = "testUser";
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku("TEST-SKU");
    request.setName("New Product Name");
    request.setDescription(null); // Null description
    request.setBusinessPartnerCode("BP001");
    request.setUrl("http://newvideo.com");
    
    Product savedProduct = new Product();
    savedProduct.setName("Old Product Name");
    savedProduct.setDescription("Old Description".getBytes());
    savedProduct.setUrl("http://oldvideo.com");
    
    // Mock the productItemServiceWrapper method
    doNothing().when(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        storeId, savedProduct, false);
    
    // When
    AuditTrailListResponse response = service.updateMasterDataAndGenerateHistory(storeId, username, request, savedProduct);
    
    // Then
    Assertions.assertNotNull(response);
    Assertions.assertEquals(3, response.getAuditTrailResponseList().size()); // name and url (description is null so no audit trail)
    
    // Verify audit trail entries
    Map<String, AuditTrailDto> auditMap = response.getAuditTrailResponseList().stream()
        .collect(java.util.stream.Collectors.toMap(AuditTrailDto::getActionKey, audit -> audit));
    
    // Name audit trail
    AuditTrailDto nameAudit = auditMap.get(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_NAME);
    Assertions.assertNotNull(nameAudit);
    Assertions.assertEquals("New Product Name", nameAudit.getNewValue());
    Assertions.assertEquals("Old Product Name", nameAudit.getOldValue());
    
    // URL audit trail
    AuditTrailDto urlAudit = auditMap.get(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_URL);
    Assertions.assertNotNull(urlAudit);
    Assertions.assertEquals("http://newvideo.com", urlAudit.getNewValue());
    Assertions.assertEquals("http://oldvideo.com", urlAudit.getOldValue());

    // Description audit trail
    AuditTrailDto descriptionAudit = auditMap.get(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_DESCRIPTION);
    Assertions.assertNotNull(descriptionAudit);
    Assertions.assertEquals("", descriptionAudit.getNewValue());
    Assertions.assertEquals("Old Description", descriptionAudit.getOldValue());

    // Verify product was updated
    Assertions.assertEquals("New Product Name", savedProduct.getName());
    Assertions.assertNull(savedProduct.getDescription()); // Description should be null
    Assertions.assertEquals("http://newvideo.com", savedProduct.getUrl());
    
    // Verify the mock was called
    verify(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        storeId, savedProduct, false);
  }

  @Test
  public void updateMasterDataAndGenerateHistory_WithNullExistingDescription_Success() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = "testUser";
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku("TEST-SKU");
    request.setName("New Product Name");
    request.setDescription("New Description".getBytes());
    request.setBusinessPartnerCode("BP001");
    request.setUrl("http://newvideo.com");

    Product savedProduct = new Product();
    savedProduct.setName("Old Product Name");
    savedProduct.setDescription(null);
    savedProduct.setUrl("http://oldvideo.com");

    // Mock the productItemServiceWrapper method
    doNothing().when(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        storeId, savedProduct, false);

    // When
    AuditTrailListResponse response = service.updateMasterDataAndGenerateHistory(storeId, username, request, savedProduct);

    // Then
    Assertions.assertNotNull(response);
    Assertions.assertEquals(3, response.getAuditTrailResponseList().size()); // name and url (description is null so no audit trail)

    // Verify audit trail entries
    Map<String, AuditTrailDto> auditMap = response.getAuditTrailResponseList().stream()
        .collect(java.util.stream.Collectors.toMap(AuditTrailDto::getActionKey, audit -> audit));

    // Name audit trail
    AuditTrailDto nameAudit = auditMap.get(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_NAME);
    Assertions.assertNotNull(nameAudit);
    Assertions.assertEquals("New Product Name", nameAudit.getNewValue());
    Assertions.assertEquals("Old Product Name", nameAudit.getOldValue());

    // URL audit trail
    AuditTrailDto urlAudit = auditMap.get(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_URL);
    Assertions.assertNotNull(urlAudit);
    Assertions.assertEquals("http://newvideo.com", urlAudit.getNewValue());
    Assertions.assertEquals("http://oldvideo.com", urlAudit.getOldValue());

    // Description audit trail
    AuditTrailDto descriptionAudit = auditMap.get(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_DESCRIPTION);
    Assertions.assertNotNull(descriptionAudit);
    Assertions.assertEquals("", descriptionAudit.getOldValue());
    Assertions.assertEquals("New Description", descriptionAudit.getNewValue());

    // Verify product was updated
    Assertions.assertEquals("New Product Name", savedProduct.getName());
    Assertions.assertNotNull(savedProduct.getDescription()); // Description should not be null
    Assertions.assertEquals("http://newvideo.com", savedProduct.getUrl());
    
    // Verify the mock was called
    verify(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        storeId, savedProduct, false);
  }

  @Test
  public void updateMasterDataAndGenerateHistory_ReviewPendingUpdated_Success() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = "testUser";
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku("TEST-SKU");
    request.setName("Same Name");
    request.setDescription("Same Description".getBytes());
    request.setReviewPending(true);
    request.setBusinessPartnerCode("BP001");
    
    Product savedProduct = new Product();
    savedProduct.setName("Same Name");
    savedProduct.setDescription("Same Description".getBytes());
    savedProduct.setReviewPending(false);
    
    // When
    AuditTrailListResponse response = service.updateMasterDataAndGenerateHistory(storeId, username, request, savedProduct);
    
    // Then
    Assertions.assertTrue(savedProduct.isReviewPending());
  }

  @Test
  public void updateMasterDataAndGenerateHistory_NoChanges_NoAuditTrail() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = "testUser";
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku("TEST-SKU");
    request.setName("Same Name");
    request.setDescription("Same Description".getBytes());
    request.setUrl("http://samevideo.com");
    request.setBusinessPartnerCode("BP001");
    
    Product savedProduct = new Product();
    savedProduct.setName("Same Name");
    savedProduct.setDescription("Same Description".getBytes());
    savedProduct.setUrl("http://samevideo.com");
    
    // When
    AuditTrailListResponse response = service.updateMasterDataAndGenerateHistory(storeId, username, request, savedProduct);
    
    // Then
    Assertions.assertNull(response);
  }

  @Test
  public void updateMasterDataAndGenerateHistory_NullShippingWeight_NoDimensionUpdate() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = "testUser";
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku("TEST-SKU");
    request.setName("New Name");
    request.setDescription("Same Description".getBytes());
    request.setShippingWeight(null); // null shipping weight
    request.setBusinessPartnerCode("BP001");
    
    Product savedProduct = new Product();
    savedProduct.setName("Old Name");
    savedProduct.setDescription("Same Description".getBytes());
    savedProduct.setShippingWeight(5.0);
    savedProduct.setUrl(StringUtils.EMPTY);
    
    // Mock the productItemServiceWrapper method
    doNothing().when(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        storeId, savedProduct, false);
    
    // When
    AuditTrailListResponse response = service.updateMasterDataAndGenerateHistory(storeId, username, request, savedProduct);
    
    // Then
    Assertions.assertNotNull(response);
    Assertions.assertEquals(1, response.getAuditTrailResponseList().size()); // Only name change
    
    AuditTrailDto nameAudit = response.getAuditTrailResponseList().get(0);
    Assertions.assertEquals(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_NAME, nameAudit.getActionKey());
    
    // Verify shipping weight was not changed
    Assertions.assertEquals(5.0, savedProduct.getShippingWeight());
    
    // Verify the mock was called
    verify(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        storeId, savedProduct, false);
  }

  @Test
  public void updateMasterDataAndGenerateHistory_SameDimensions_NoDimensionAuditTrail() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = "testUser";
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku("TEST-SKU");
    request.setName("New Name");
    request.setDescription("Same Description".getBytes());
    request.setLength(100.0);
    request.setWidth(50.0);
    request.setHeight(30.0);
    request.setWeight(5.0);
    request.setShippingWeight(6.0);
    request.setBusinessPartnerCode("BP001");
    
    Product savedProduct = new Product();
    savedProduct.setName("Old Name");
    savedProduct.setDescription("Same Description".getBytes());
    savedProduct.setLength(100.0);
    savedProduct.setWidth(50.0);
    savedProduct.setHeight(30.0);
    savedProduct.setWeight(5.0);
    savedProduct.setShippingWeight(6.0);
    savedProduct.setUrl(StringUtils.EMPTY);
    
    // Mock the productItemServiceWrapper method
    doNothing().when(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        storeId, savedProduct, false);
    
    // When
    AuditTrailListResponse response = service.updateMasterDataAndGenerateHistory(storeId, username, request, savedProduct);
    
    // Then
    Assertions.assertNotNull(response);
    Assertions.assertEquals(1, response.getAuditTrailResponseList().size()); // Only name change
    
    AuditTrailDto nameAudit = response.getAuditTrailResponseList().get(0);
    Assertions.assertEquals(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_NAME, nameAudit.getActionKey());
    
    // Verify the mock was called
    verify(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        storeId, savedProduct, false);
  }

  // Test for updateDimensions private method coverage
  @Test
  public void updateMasterDataAndGenerateHistory_OnlyPartialDimensionsUpdated_Success() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = "testUser";
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku("TEST-SKU");
    request.setName("Same Name");
    request.setDescription("Same Description".getBytes());
    request.setLength(100.0); // Only length changed
    request.setWidth(40.0);   // Same width
    request.setHeight(25.0);  // Same height
    request.setWeight(4.0);   // Same weight
    request.setShippingWeight(6.0); // Changed shipping weight - this triggers updateDimensions
    request.setBusinessPartnerCode("BP001");
    
    Product savedProduct = new Product();
    savedProduct.setName("Same Name");
    savedProduct.setDescription("Same Description".getBytes());
    savedProduct.setLength(80.0);
    savedProduct.setWidth(40.0);
    savedProduct.setHeight(25.0);
    savedProduct.setWeight(4.0);
    savedProduct.setShippingWeight(5.0);
    savedProduct.setUrl(StringUtils.EMPTY);
    
    // When
    AuditTrailListResponse response = service.updateMasterDataAndGenerateHistory(storeId, username, request, savedProduct);
    
    // Then
    Assertions.assertNotNull(response);
    Assertions.assertEquals(2, response.getAuditTrailResponseList().size()); // length and shipping weight
    
    Map<String, AuditTrailDto> auditMap = response.getAuditTrailResponseList().stream()
        .collect(java.util.stream.Collectors.toMap(AuditTrailDto::getActionKey, audit -> audit));
    
    // Verify only changed dimensions are in audit trail
    Assertions.assertTrue(auditMap.containsKey(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_LENGTH));
    Assertions.assertTrue(auditMap.containsKey(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_SHIPPING_WEIGHT));
    Assertions.assertFalse(auditMap.containsKey(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_WIDTH));
    Assertions.assertFalse(auditMap.containsKey(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_HEIGHT));
    Assertions.assertFalse(auditMap.containsKey(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_WEIGHT));
  }

  // Test for getAuditTrailListResponse private method coverage
  @Test
  public void updateMasterDataAndGenerateHistory_EmptyAuditTrailList_ReturnsNull() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = "testUser";
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku("TEST-SKU");
    request.setName("Same Name");
    request.setDescription("Same Description".getBytes());
    request.setBusinessPartnerCode("BP001");
    
    Product savedProduct = new Product();
    savedProduct.setName("Same Name"); // Same name, no change
    savedProduct.setDescription("Same Description".getBytes()); // Same description, no change
    savedProduct.setUrl(StringUtils.EMPTY); // No URL change
    
    // When
    AuditTrailListResponse response = service.updateMasterDataAndGenerateHistory(storeId, username, request, savedProduct);
    
    // Then
    Assertions.assertNull(response); // Empty audit trail list should return null
  }

  @Test
  public void updateMasterDataAndGenerateHistory_ProductNameUpdated_GeneratedItemNamesUpdated_Success() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = "testUser";
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku("TEST-SKU");
    request.setName("New Product Name");
    request.setDescription("Same Description".getBytes());
    request.setBusinessPartnerCode("BP001");
    
    Product savedProduct = new Product();
    savedProduct.setName("Old Product Name");
    savedProduct.setDescription("Same Description".getBytes());
    savedProduct.setUrl(StringUtils.EMPTY);
    
    // Mock the productItemServiceWrapper method
    doNothing().when(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        storeId, savedProduct, false);
    
    // Create product items with generated names that include the old product name
    List<ProductItem> productItems = new ArrayList<>();
    
    ProductItem item1 = new ProductItem();
    item1.setId("item1");
    item1.setSkuCode("SKU-001");
    item1.setGeneratedItemName("Old Product Name Red Large");
    item1.setProductItemAttributeValues(createDefiningAttributeValues("Red", "Large"));
    productItems.add(item1);
    
    ProductItem item2 = new ProductItem();
    item2.setId("item2");
    item2.setSkuCode("SKU-002");
    item2.setGeneratedItemName("Old Product Name Blue Medium");
    item2.setProductItemAttributeValues(createDefiningAttributeValues("Blue", "Medium"));
    productItems.add(item2);
    
    ProductItem item3 = new ProductItem();
    item3.setId("item3");
    item3.setSkuCode("SKU-003");
    item3.setGeneratedItemName("Old Product Name Green Small");
    item3.setProductItemAttributeValues(createDefiningAttributeValues("Green", "Small"));
    productItems.add(item3);
    
    savedProduct.setProductItems(productItems);
    
    // When
    AuditTrailListResponse response = service.updateMasterDataAndGenerateHistory(storeId, username, request, savedProduct);
    
    // Then
    Assertions.assertNotNull(response);
    Assertions.assertEquals(1, response.getAuditTrailResponseList().size());
    
    AuditTrailDto nameAudit = response.getAuditTrailResponseList().get(0);
    Assertions.assertEquals(Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_NAME, nameAudit.getActionKey());
    Assertions.assertEquals("New Product Name", nameAudit.getNewValue());
    Assertions.assertEquals("Old Product Name", nameAudit.getOldValue());
    
    // Verify product name was updated
    Assertions.assertEquals("New Product Name", savedProduct.getName());
    
    // Verify all generated item names were updated to use the new product name
    Assertions.assertEquals("New Product Name Red Large", savedProduct.getProductItems().get(0).getGeneratedItemName());
    Assertions.assertEquals("New Product Name Blue Medium", savedProduct.getProductItems().get(1).getGeneratedItemName());
    Assertions.assertEquals("New Product Name Green Small", savedProduct.getProductItems().get(2).getGeneratedItemName());
    
    // Verify the mock was called
    verify(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        storeId, savedProduct, false);
  }

  @Test
  public void updateMasterDataAndGenerateHistory_ProductNameUpdated_ItemNamesNotStartingWithProductName_Success() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = "testUser";
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku("TEST-SKU");
    request.setName("New Product Name");
    request.setDescription("Same Description".getBytes());
    request.setBusinessPartnerCode("BP001");
    
    Product savedProduct = new Product();
    savedProduct.setName("Old Product Name");
    savedProduct.setDescription("Same Description".getBytes());
    savedProduct.setUrl(StringUtils.EMPTY);
    
    // Mock the productItemServiceWrapper method
    doNothing().when(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        storeId, savedProduct, false);
    
    // Create product items with generated names that don't start with the old product name
    List<ProductItem> productItems = new ArrayList<>();
    
    ProductItem item1 = new ProductItem();
    item1.setId("item1");
    item1.setSkuCode("SKU-001");
    item1.setGeneratedItemName("Different Name Red Large"); // Doesn't start with old product name
    item1.setProductItemAttributeValues(createDefiningAttributeValues("Red", "Large"));
    productItems.add(item1);
    
    ProductItem item2 = new ProductItem();
    item2.setId("item2");
    item2.setSkuCode("SKU-002");
    item2.setGeneratedItemName("Another Name Blue Medium"); // Doesn't start with old product name
    item2.setProductItemAttributeValues(createDefiningAttributeValues("Blue", "Medium"));
    productItems.add(item2);
    
    savedProduct.setProductItems(productItems);
    
    // When
    AuditTrailListResponse response = service.updateMasterDataAndGenerateHistory(storeId, username, request, savedProduct);
    
    // Then
    Assertions.assertNotNull(response);
    Assertions.assertEquals(1, response.getAuditTrailResponseList().size());
    
    // Verify product name was updated
    Assertions.assertEquals("New Product Name", savedProduct.getName());
    
    // Verify generated item names were reconstructed with new product name + defining attributes
    Assertions.assertEquals("New Product Name Red Large", savedProduct.getProductItems().get(0).getGeneratedItemName());
    Assertions.assertEquals("New Product Name Blue Medium", savedProduct.getProductItems().get(1).getGeneratedItemName());
    
    // Verify the mock was called
    verify(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        storeId, savedProduct, false);
  }

  @Test
  public void updateMasterDataAndGenerateHistory_ProductNameUpdated_ItemsWithNoDefiningAttributes_Success() throws Exception {
    // Given
    String storeId = STORE_ID;
    String username = "testUser";
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku("TEST-SKU");
    request.setName("New Product Name");
    request.setDescription("Same Description".getBytes());
    request.setBusinessPartnerCode("BP001");
    
    Product savedProduct = new Product();
    savedProduct.setName("Old Product Name");
    savedProduct.setDescription("Same Description".getBytes());
    savedProduct.setUrl(StringUtils.EMPTY);
    
    // Mock the productItemServiceWrapper method
    doNothing().when(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        storeId, savedProduct, false);
    
    // Create product items with no defining attributes
    List<ProductItem> productItems = new ArrayList<>();
    
    ProductItem item1 = new ProductItem();
    item1.setId("item1");
    item1.setSkuCode("SKU-001");
    item1.setGeneratedItemName("Different Name"); // Doesn't start with old product name
    item1.setProductItemAttributeValues(new ArrayList<>()); // No defining attributes
    productItems.add(item1);
    
    savedProduct.setProductItems(productItems);
    
    // When
    AuditTrailListResponse response = service.updateMasterDataAndGenerateHistory(storeId, username, request, savedProduct);
    
    // Then
    Assertions.assertNotNull(response);
    Assertions.assertEquals(1, response.getAuditTrailResponseList().size());
    
    // Verify product name was updated
    Assertions.assertEquals("New Product Name", savedProduct.getName());
    
    // Verify generated item name was updated to just the new product name (no defining attributes)
    Assertions.assertEquals("New Product Name", savedProduct.getProductItems().get(0).getGeneratedItemName());
    
    // Verify the mock was called
    verify(productItemServiceWrapper).setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        storeId, savedProduct, false);
  }

  // Helper method to create defining attribute values for testing
  private List<ProductItemAttributeValue> createDefiningAttributeValues(String color, String size) {
    List<ProductItemAttributeValue> attributeValues = new ArrayList<>();
    
    // Color attribute (defining attribute)
    Attribute colorAttribute = new Attribute();
    colorAttribute.setId("color-attr-id");
    colorAttribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    colorAttribute.setName("Color");
    
    ProductItemAttributeValue colorValue = new ProductItemAttributeValue();
    colorValue.setAttribute(colorAttribute);
    colorValue.setValue(color);
    attributeValues.add(colorValue);
    
    // Size attribute (defining attribute)
    Attribute sizeAttribute = new Attribute();
    sizeAttribute.setId("size-attr-id");
    sizeAttribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    sizeAttribute.setName("Size");
    
    ProductItemAttributeValue sizeValue = new ProductItemAttributeValue();
    sizeValue.setAttribute(sizeAttribute);
    sizeValue.setValue(size);
    attributeValues.add(sizeValue);
    
    return attributeValues;
  }

  // Helper methods for test data creation
  private ProductMasterDataUpdateRequest createProductMasterDataUpdateRequest() {
    ProductMasterDataUpdateRequest request = new ProductMasterDataUpdateRequest();
    request.setProductSku("TEST-SKU");
    request.setName("New Product Name");
    request.setDescription("New Description".getBytes());
    request.setLength(100.0);
    request.setWidth(50.0);
    request.setHeight(30.0);
    request.setWeight(5.0);
    request.setShippingWeight(6.0);
    request.setUrl("http://newvideo.com");
    request.setBusinessPartnerCode("BP001");
    return request;
  }

  private Product createSavedProduct() {
    Product product = new Product();
    product.setName("Old Product Name");
    product.setDescription("Old Description".getBytes());
    product.setLength(80.0);
    product.setWidth(40.0);
    product.setHeight(25.0);
    product.setWeight(4.0);
    product.setShippingWeight(5.0);
    product.setUrl("http://oldvideo.com");
    return product;
  }

  @Test
  void testCheckOmniChannelSkuExistsInSeller_someExist() throws Exception {
    String sellerCode = "sellerA";
    List<String> omniChannelSkus = Arrays.asList("SKU1", "SKU2", "SKU3");
    when(
        productItemService.findByStoreIdAndCreatedMerchantAndOmniChannelSkuInAndMarkForDeleteFalse(STORE_ID, sellerCode,
            omniChannelSkus)).thenReturn(Collections.singletonList(productItems.get(0)));
    OmniChannelSkuRequest request = new OmniChannelSkuRequest(sellerCode, omniChannelSkus);
    ValidOmniChannelSkuResponse response = service.checkOmniChannelSkuExistsInSeller(STORE_ID, request, false);
    verify(productItemService).findByStoreIdAndCreatedMerchantAndOmniChannelSkuInAndMarkForDeleteFalse(STORE_ID,
        sellerCode, omniChannelSkus);
    assertFalse(response.getExistingOmniChannelSkusAndProductDetailsMap().isEmpty());
  }

  @Test
  void testCheckOmniChannelSkuExistsInSeller_noneExist() throws Exception {
    String sellerCode = "sellerA";
    List<String> omniChannelSkus = Arrays.asList("SKU1", "SKU2");
    OmniChannelSkuRequest request = new OmniChannelSkuRequest(sellerCode, omniChannelSkus);

    ValidOmniChannelSkuResponse response =
        service.checkOmniChannelSkuExistsInSeller(STORE_ID, request, false);
    verify(productItemService).findByStoreIdAndCreatedMerchantAndOmniChannelSkuInAndMarkForDeleteFalse(STORE_ID,
        sellerCode, omniChannelSkus);
    assertTrue(response.getExistingOmniChannelSkusAndProductDetailsMap().isEmpty());
  }

  @Test
  void testCheckOmniChannelSkuExistsInSeller_emptyRequest() throws Exception {
    String sellerCode = "sellerA";
    OmniChannelSkuRequest request = new OmniChannelSkuRequest(sellerCode, Collections.emptyList());

    ValidOmniChannelSkuResponse response =
        service.checkOmniChannelSkuExistsInSeller(STORE_ID, request, false);
    assertTrue(response.getExistingOmniChannelSkusAndProductDetailsMap().isEmpty());
  }

  @Test
  public void setCompleteProductDetailsCachedWithUomInfoTest() {
    service.setCompleteProductDetailsCached(STORE_ID, product, false);
    verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(DEFAULT_STORE_ID, CATEGORY_ID_1);
    verify(productAttributeService).getProductAttributesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(imageService).getProductImagesByStoreIdAndProductIdCached(DEFAULT_STORE_ID, PRODUCT_ID);
    verify(productItemServiceWrapper).setCompleteProductItemDetailsCached(DEFAULT_STORE_ID, product, false);
    assertEquals(3, product.getProductImages().size());
    assertTrue(product.getProductImages().get(0).isMainImages());
    Assertions.assertFalse(product.getProductImages().get(1).isMainImages());
    assertTrue(product.getProductImages().get(2).isMainImages());
    assertTrue(product.getProductImages().get(2).isActive());
  }

  @Test
  public void testUpdateOnlyBrandNameOfProduct_Success() {
    // Given
    String storeId = STORE_ID;
    ProductBrandDataUpdateRequest request = new ProductBrandDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setNewBrandName(NEW_BRAND_NAME);

    Product product = createDefaultProduct();
    product.setId(PRODUCT_ID);
    product.setBrand(ORIGINAL_BRAND);

    // Create product item with brand attribute
    ProductItem productItem = new ProductItem();
    productItem.setId(PRODUCT_ITEM_ID_1);
    productItem.setSkuCode(DEFAULT_SKU_CODE);
    productItem.setProductItemAttributeValues(new ArrayList<>());

    ProductItem productItem2 = new ProductItem();
    productItem2.setId(PRODUCT_ITEM_ID_2);
    productItem2.setSkuCode(DEFAULT_SKU_CODE);
    productItem2.setProductItemAttributeValues(new ArrayList<>());
    productItem2.setMarkForDelete(true);

    // Add brand attribute to product item
    ProductItemAttributeValue brandAttributeValue = new ProductItemAttributeValue();
    Attribute brandAttr = new Attribute();
    brandAttr.setId(ATTRIBUTE_ID_1);
    brandAttr.setName(BRAND_ATTRIBUTE_NAME);
    brandAttributeValue.setAttribute(brandAttr);
    brandAttributeValue.setValue(ORIGINAL_BRAND);
    productItem.getProductItemAttributeValues().add(brandAttributeValue);

    List<ProductItem> productItems = Arrays.asList(productItem, productItem2);

    Attribute brandAttribute = new Attribute();
    brandAttribute.setId(ATTRIBUTE_ID_1);
    brandAttribute.setAttributeCode("BR-M036969");
    brandAttribute.setName(BRAND_ATTRIBUTE_NAME);

    // Mock repository and service calls
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, PRODUCT_CODE))
        .thenReturn(product);
    when(productItemService.getProductItemsByStoreIdAndProductIdCached(storeId, PRODUCT_ID))
        .thenReturn(productItems);
    when(attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(storeId, "BR-M036969"))
        .thenReturn(brandAttribute);
    doNothing().when(productItemAttributeValueService)
        .updateOnlyValueForProductItemAttributeValuesByAttributeId(eq(storeId), eq(ATTRIBUTE_ID_1), 
            eq(NEW_BRAND_NAME), eq(productItem));
    when(productRepository.save(product)).thenReturn(product);

    // When
    service.updateOnlyBrandNameOfProduct(storeId, request);

    // Then
    assertEquals(NEW_BRAND_NAME, product.getBrand());
    verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, PRODUCT_CODE);
    verify(productItemService).getProductItemsByStoreIdAndProductIdCached(storeId, PRODUCT_ID);
    verify(attributeService).getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(storeId, "BR-M036969");
    verify(productItemAttributeValueService)
        .updateOnlyValueForProductItemAttributeValuesByAttributeId(storeId, ATTRIBUTE_ID_1, NEW_BRAND_NAME, productItem);
    verify(productRepository).save(product);
  }

  @Test
  public void testUpdateOnlyBrandNameOfProduct_SuccessWithMultipleItems() {
    // Given
    String storeId = STORE_ID;
    ProductBrandDataUpdateRequest request = new ProductBrandDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setNewBrandName(NEW_BRAND_NAME);

    Product product = createDefaultProduct();
    product.setId(PRODUCT_ID);
    product.setBrand(ORIGINAL_BRAND);

    // Create multiple product items
    ProductItem productItem1 = new ProductItem();
    productItem1.setId(PRODUCT_ITEM_ID_1);
    productItem1.setSkuCode(DEFAULT_SKU_CODE);

    ProductItem productItem2 = new ProductItem();
    productItem2.setId(PRODUCT_ITEM_ID_2);
    productItem2.setSkuCode(ITEM_SKU_CODE_2);

    List<ProductItem> productItems = Arrays.asList(productItem1, productItem2);

    Attribute brandAttribute = new Attribute();
    brandAttribute.setId(ATTRIBUTE_ID_1);
    brandAttribute.setAttributeCode("BR-M036969");
    brandAttribute.setName(BRAND_ATTRIBUTE_NAME);

    // Mock repository and service calls
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, PRODUCT_CODE))
        .thenReturn(product);
    when(productItemService.getProductItemsByStoreIdAndProductIdCached(storeId, PRODUCT_ID))
        .thenReturn(productItems);
    when(attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(storeId, "BR-M036969"))
        .thenReturn(brandAttribute);
    doNothing().when(productItemAttributeValueService)
        .updateOnlyValueForProductItemAttributeValuesByAttributeId(eq(storeId), eq(ATTRIBUTE_ID_1), 
            eq(NEW_BRAND_NAME), any(ProductItem.class));
    when(productRepository.save(product)).thenReturn(product);

    // When
    service.updateOnlyBrandNameOfProduct(storeId, request);

    // Then
    assertEquals(NEW_BRAND_NAME, product.getBrand());
    verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, PRODUCT_CODE);
    verify(productItemService).getProductItemsByStoreIdAndProductIdCached(storeId, PRODUCT_ID);
    verify(attributeService).getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(storeId, "BR-M036969");
    verify(productItemAttributeValueService, times(2))
        .updateOnlyValueForProductItemAttributeValuesByAttributeId(eq(storeId), eq(ATTRIBUTE_ID_1), 
            eq(NEW_BRAND_NAME), any(ProductItem.class));
    verify(productRepository).save(product);
  }

  @Test
  public void testUpdateOnlyBrandNameOfProduct_ProductNotFound() {
    // Given
    String storeId = STORE_ID;
    ProductBrandDataUpdateRequest request = new ProductBrandDataUpdateRequest();
    request.setProductCode(WRONG_PRODUCT_CODE);
    request.setNewBrandName(NEW_BRAND_NAME);

    // Mock repository to return null (product not found)
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, WRONG_PRODUCT_CODE))
        .thenReturn(null);

    // When & Then
    assertThrows(NullPointerException.class, () -> {
      service.updateOnlyBrandNameOfProduct(storeId, request);
    });

    verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, WRONG_PRODUCT_CODE);
    verify(productItemService, never()).getProductItemsByStoreIdAndProductIdCached(anyString(), anyString());
    verify(attributeService, never()).getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(anyString(), anyString());
    verify(productRepository, never()).save(any(Product.class));
  }

  @Test
  public void testUpdateOnlyBrandNameOfProduct_NoProductItems() {
    // Given
    String storeId = STORE_ID;
    ProductBrandDataUpdateRequest request = new ProductBrandDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setNewBrandName(NEW_BRAND_NAME);

    Product product = createDefaultProduct();
    product.setId(PRODUCT_ID);
    product.setBrand(ORIGINAL_BRAND);

    List<ProductItem> emptyProductItems = new ArrayList<>();

    Attribute brandAttribute = new Attribute();
    brandAttribute.setId(ATTRIBUTE_ID_1);
    brandAttribute.setAttributeCode("BR-M036969");
    brandAttribute.setName(BRAND_ATTRIBUTE_NAME);

    // Mock repository and service calls
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, PRODUCT_CODE))
        .thenReturn(product);
    when(productItemService.getProductItemsByStoreIdAndProductIdCached(storeId, PRODUCT_ID))
        .thenReturn(emptyProductItems);
    when(attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(storeId, "BR-M036969"))
        .thenReturn(brandAttribute);
    when(productRepository.save(product)).thenReturn(product);

    // When
    service.updateOnlyBrandNameOfProduct(storeId, request);

    // Then
    assertEquals(NEW_BRAND_NAME, product.getBrand());
    verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, PRODUCT_CODE);
    verify(productItemService).getProductItemsByStoreIdAndProductIdCached(storeId, PRODUCT_ID);
    verify(attributeService).getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(storeId, "BR-M036969");
    verify(productItemAttributeValueService, never())
        .updateOnlyValueForProductItemAttributeValuesByAttributeId(anyString(), anyString(), anyString(), any(ProductItem.class));
    verify(productRepository).save(product);
  }

  @Test
  public void testUpdateOnlyBrandNameOfProduct_BrandAttributeNotFound() {
    // Given
    String storeId = STORE_ID;
    ProductBrandDataUpdateRequest request = new ProductBrandDataUpdateRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setNewBrandName(NEW_BRAND_NAME);

    Product product = createDefaultProduct();
    product.setId(PRODUCT_ID);
    product.setBrand(ORIGINAL_BRAND);

    ProductItem productItem = new ProductItem();
    productItem.setId(PRODUCT_ITEM_ID_1);
    productItem.setSkuCode(DEFAULT_SKU_CODE);
    List<ProductItem> productItems = Arrays.asList(productItem);

    // Mock repository and service calls
    when(repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, PRODUCT_CODE))
        .thenReturn(product);
    when(productItemService.getProductItemsByStoreIdAndProductIdCached(storeId, PRODUCT_ID))
        .thenReturn(productItems);
    when(attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(storeId, "BR-M036969"))
        .thenReturn(null);

    // When & Then
    assertThrows(NullPointerException.class, () -> {
      service.updateOnlyBrandNameOfProduct(storeId, request);
    });

    verify(repository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, PRODUCT_CODE);
    verify(productItemService).getProductItemsByStoreIdAndProductIdCached(storeId, PRODUCT_ID);
    verify(attributeService).getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(storeId, "BR-M036969");
    verify(productItemAttributeValueService, never())
        .updateOnlyValueForProductItemAttributeValuesByAttributeId(anyString(), anyString(), anyString(), any(ProductItem.class));
    verify(productRepository, never()).save(any(Product.class));
  }

  @Test
  public void testUpdateOnlyBrandNameOfProduct_NullRequest() {
    // Given
    String storeId = STORE_ID;
    ProductBrandDataUpdateRequest request = null;

    // When & Then
    assertThrows(NullPointerException.class, () -> {
      service.updateOnlyBrandNameOfProduct(storeId, request);
    });

    verify(repository, never()).findByStoreIdAndProductCodeAndMarkForDeleteFalse(anyString(), anyString());
    verify(productItemService, never()).getProductItemsByStoreIdAndProductIdCached(anyString(), anyString());
    verify(attributeService, never()).getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(anyString(), anyString());
    verify(productRepository, never()).save(any(Product.class));
  }

@Test
  void testCheckOmniChannelSkuExistsInSeller_needUomInfo_true_withUomPresent() throws Exception {
    when(productItemService.findByStoreIdAndCreatedMerchantAndOmniChannelSkuInAndMarkForDeleteFalse(STORE_ID, "sellerA",
        Arrays.asList("SKU1", "SKU2"))).thenReturn(Collections.singletonList(productItems.get(0)));
    when(productItemUomInfoService.findByStoreIdAndSkuCodeInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Collections.singletonList(new ProductItemUomInfo() {{
      setSkuCode(productItems.get(0).getSkuCode());
      setStoreId(STORE_ID);
    }}));
    assertFalse(service.checkOmniChannelSkuExistsInSeller(STORE_ID,
            new OmniChannelSkuRequest("sellerA", Arrays.asList("SKU1", "SKU2")), IS_PROMO_SKU)
        .getExistingOmniChannelSkusAndProductDetailsMap().isEmpty());
    verify(productItemService).findByStoreIdAndCreatedMerchantAndOmniChannelSkuInAndMarkForDeleteFalse(STORE_ID,
        "sellerA", Arrays.asList("SKU1", "SKU2"));
    verify(productItemUomInfoService).findByStoreIdAndSkuCodeInAndMarkForDeleteFalse(eq(STORE_ID), Mockito.anyList());
    Assertions.assertNotNull(SALES_CATEGORY_1);
  }

  @Test
  void testCheckOmniChannelSkuExistsInSeller_needUomInfo_true_withoutUomPresent() throws Exception {
    when(productItemService.findByStoreIdAndCreatedMerchantAndOmniChannelSkuInAndMarkForDeleteFalse(STORE_ID, "sellerB",
        Arrays.asList("SKU3"))).thenReturn(Collections.singletonList(productItems.get(0)));
    when(productItemUomInfoService.findByStoreIdAndSkuCodeInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList())).thenReturn(null);
    assertFalse(
        service.checkOmniChannelSkuExistsInSeller(STORE_ID, new OmniChannelSkuRequest("sellerB", Arrays.asList("SKU3")),
            IS_PROMO_SKU).getExistingOmniChannelSkusAndProductDetailsMap().isEmpty());
    verify(productItemService).findByStoreIdAndCreatedMerchantAndOmniChannelSkuInAndMarkForDeleteFalse(STORE_ID,
        "sellerB", Arrays.asList("SKU3"));
    verify(productItemUomInfoService).findByStoreIdAndSkuCodeInAndMarkForDeleteFalse(eq(STORE_ID), Mockito.anyList());
    Assertions.assertNotNull(SALES_CATEGORY_1);
  }
}