package com.gdn.x.product.service.impl;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.openMocks;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.CurationStatus;
import com.gdn.x.product.enums.DistributionStatus;
import com.gdn.x.product.enums.SolrFieldNames;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.MasterDataProductImage;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductScore;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.rest.web.model.FieldValueObject;
import com.gdn.x.product.service.api.BusinessPartnerService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.productcategorybase.domain.event.model.CatalogDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductSalesCategoryMapping;
import com.gdn.x.productcategorybase.dto.response.ImageResponse;

public class ProductSolrConstructorServiceImplTest {

  private static final String PRODUCT_CODE = "product_code";
  private static final String PRODUCT_SKU = "product_sku";
  private static final String CATALOG_CODE = "catalog_code";
  private static final String CATEGORY_CODE = "category_code";
  private static final String CATEGORY_CODE_1 = "category_code_1";
  private static final String STORE_ID = "store_id";
  private static final String MERCHANT_CODE = "merchant_code";
  private static final String SALES_CATALOG_CODE = "12051";
  private static final String PRODUCT_NAME = "product_name";
  private static final String PRODUCT_NAME1 = "product_name1";
  private static final String OLD_PRODUCT_NAME = "product_name-old";
  private static final String BRAND = "brand";
  private static final String OLD_BRAND = "brand-old";
  private static final String OLD_MASTER_CATALOG = "masterCatalogOld";
  private static final String PRODUCT_MAIN_IMAGE = "productMainimage";
  private static final String OLD_PRODUCT_MAIN_IMAGE = "oldProductMainimage";
  private static final String XPRODUCT_SOLR_REINDEX = "xproduct-solr-reindex";
  private static final String LOCATION_PATH = "location_path";
  private static final String OLD_SALES_CATEGORY_CODE = "old_sales";
  private static final String NEW_SALES_CATEGORY_CODE = "new_sales";
  private static final String UMKM_SALES_CATEGORY_CODE  = "umkm_sales";
  private static final String SOLR_STRING_DELIMITER = "#_#";
  private static final String SALES_CATEGORY_CATALOG_CODE = "12051";
  private static final String PRODUCT_MASTER_CATALOG = "catalog_code#_#category_code";
  private static final String PRODUCT_SALES_CATALOG = "12051#_#new_sales";
  private static final String PRODUCT_UMKM_SALES_CATALOG = "12051#_#umkm_sales";
  private static final String PRODUCT_OLD_SALES_CATALOG = "12051#_#old_sales";

  private Product product = new Product();
  private SalesCatalog salesCatalog = new SalesCatalog();
  private Category category = new Category();
  private Category category1 = new Category();
  private MasterDataProductImage masterDataProductImage = new MasterDataProductImage();
  private ProductSolr productSolr = new ProductSolr();
  private ProductDomainEventModel productDomainEventModel = new ProductDomainEventModel();
  private ProductSalesCategoryMapping productSalesCategoryMapping = new ProductSalesCategoryMapping();
  private CategoryDomainEventModel categoryDomainEventModel = new CategoryDomainEventModel();
  private CatalogDomainEventModel catalogDomainEventModel = new CatalogDomainEventModel();
  private ImageDomainEventModel imageDomainEventModel = new ImageDomainEventModel();
  private Map<String, Double> productAndTotalScoreMap = new HashMap<>();
  private ImageResponse imageResponse = new ImageResponse();
  private SystemParameter systemParameter = new SystemParameter();

  @Mock
  private ProductHelperService productHelperService;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @Mock
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Mock
  private SystemParameterService systemParameterService;

  @InjectMocks
  private ProductSolrConstructorServiceImpl productSolrConstructorService;

  @Captor
  private ArgumentCaptor<String> stringArgumentCaptor;

  @Captor
  private ArgumentCaptor<Product> productArgumentCaptor;

  @BeforeEach
  public void init() throws Exception {
    openMocks(this);
    ReflectionTestUtils
        .setField(productSolrConstructorService, "salesCategoryCatalogCode", SALES_CATEGORY_CATALOG_CODE);
    ReflectionTestUtils.setField(productSolrConstructorService, "solrStringDelimiter", SOLR_STRING_DELIMITER);
    product.setProductCode(PRODUCT_CODE);
    product.setProductSku(PRODUCT_SKU);
    product.setCurationStatus(CurationStatus.NONE);
    product.setMasterCatalog(new MasterCatalog());
    product.getMasterCatalog().setCatalogCode(CATALOG_CODE);

    category.setCategoryCode(CATEGORY_CODE);
    category1.setCategoryCode(CATEGORY_CODE_1);
    product.getMasterCatalog().setCategory(category);

    salesCatalog.setCatalogCode(SALES_CATALOG_CODE);
    salesCatalog.setListOfCategories(Arrays.asList(category, category1));
    product.setSalesCatalogs(Arrays.asList(salesCatalog));

    product.setCreatedDate(new Date());
    product.setUpdatedDate(new Date());
    product.setStoreId(STORE_ID);
    product.setSynchronized(Boolean.FALSE);
    product.setSuspended(Boolean.FALSE);
    product.setMerchantCode(MERCHANT_CODE);
    product.setMarkForDelete(Boolean.FALSE);

    product.setMasterDataProduct(new MasterDataProduct());
    product.getMasterDataProduct().setProductName(PRODUCT_NAME);
    product.getMasterDataProduct().setBrand(BRAND);
    product.getMasterDataProduct().setMasterCatalog(new MasterCatalog(CATALOG_CODE, category));

    product.setProductCenterUpdatedDate(new SimpleDateFormat( "yyyy/MM/dd" ).parse( "2021/01/20" ));

    masterDataProductImage.setLocationPath(LOCATION_PATH);
    masterDataProductImage.setMainImage(Boolean.TRUE);
    masterDataProductImage.setProductCode(PRODUCT_CODE);

    product.getMasterDataProduct().setMasterDataProductImages(Arrays.asList(masterDataProductImage));
    product.setProductScore(new ProductScore(10, 10, 10, 10, 10, 20, 10, 10, 10, 1, 80));

    productSolr.setMerchantCode(MERCHANT_CODE);
    productSolr.setProductCode(PRODUCT_CODE);
    productSolr.setProductSku(PRODUCT_SKU);
    productSolr.setSynchronized(true);
    productSolr.setProductName(OLD_PRODUCT_NAME);
    productSolr.setMasterCatalog(OLD_MASTER_CATALOG);
    productSolr.setBrand(OLD_BRAND);
    productSolr.setProductMainImage(OLD_PRODUCT_MAIN_IMAGE);

    productSalesCategoryMapping.setOldSalesCategoryCodes(Arrays.asList(OLD_SALES_CATEGORY_CODE));
    productSalesCategoryMapping.setNewSalesCategoryCodes(Arrays.asList(NEW_SALES_CATEGORY_CODE));
    productSalesCategoryMapping.setNewUmkmSalesCategoryCodes(Arrays.asList(UMKM_SALES_CATEGORY_CODE));

    catalogDomainEventModel.setCatalogCode(CATALOG_CODE);
    categoryDomainEventModel.setCatalog(catalogDomainEventModel);
    categoryDomainEventModel.setCategoryCode(CATEGORY_CODE);

    imageDomainEventModel.setLocationPath(LOCATION_PATH);
    imageDomainEventModel.setMainImage(Boolean.TRUE);

    productDomainEventModel.setProductCode(PRODUCT_CODE);
    productDomainEventModel.setName(PRODUCT_NAME);
    productDomainEventModel.setBrand(BRAND);
    productDomainEventModel.setProductSalesCategoryMapping(productSalesCategoryMapping);
    productDomainEventModel
        .setProductCategories(Arrays.asList(new ProductCategoryDomainEventModel(categoryDomainEventModel)));
    productDomainEventModel.setImages(Arrays.asList(imageDomainEventModel));

    imageResponse.setLocationPath(LOCATION_PATH);
    imageResponse.setMainImage(true);
    imageResponse.setMarkForDelete(false);
    imageResponse.setSequence(0);

    systemParameter.setValue("true");
  }

  @AfterEach
  public void teardown() {
    verifyNoMoreInteractions(productHelperService);
    verifyNoMoreInteractions(businessPartnerService);
  }

  @Test
  public void constructProductTest() throws Exception {
    ProductSolr productSolr = new ProductSolr();
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    product.setDistributionStatus(DistributionStatus.PURE_DISTRIBUTION);
    this.productSolrConstructorService.constructProduct(productSolr, product, true);
    Assertions.assertEquals(PRODUCT_CODE, productSolr.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productSolr.getProductName());
    Assertions.assertEquals(BRAND, productSolr.getBrand());
    Assertions.assertEquals(80.0, productSolr.getProductScoreTotal(), 0);
    Assertions.assertFalse(productSolr.getIsArchived());
    Assertions.assertEquals(1, productSolr.getDistributionStatus());
    Assertions.assertFalse(productSolr.getOff2OnChannelActive());
  }

  @Test
  public void constructProduct_fetchMasterDataTest() throws Exception {
    product.setSynchronized(Boolean.TRUE);
    product.setProductName(PRODUCT_NAME1);
    product.setBrand(BRAND);
    product.setCategoryCode(CATEGORY_CODE);
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    ProductSolr productSolr = new ProductSolr();
    this.productSolrConstructorService.constructProduct(productSolr, product, false);
    Assertions.assertEquals(PRODUCT_CODE, productSolr.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME1, productSolr.getProductName());
    Assertions.assertEquals(BRAND, productSolr.getBrand());
    Assertions.assertEquals(Constants.MASTER_CATALOG + SOLR_STRING_DELIMITER + CATEGORY_CODE, productSolr.getMasterCatalog());
    Assertions.assertEquals(StringUtils.EMPTY, productSolr.getProductMainImage());
  }

  @Test
  public void constructProduct_fetchMasterDataWithMasterDataTest() throws Exception {
    product.setSynchronized(Boolean.TRUE);
    product.setProductName(PRODUCT_NAME1);
    product.setBrand(BRAND);
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    Mockito.when(this.productCategoryBaseOutbound.getProductImagesByProductCode(product.getProductCode()))
        .thenReturn(Arrays.asList(imageResponse));
    ProductSolr productSolr = new ProductSolr();
    this.productSolrConstructorService.constructProduct(productSolr, product, true);
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(anyString(), anyString());
    Assertions.assertEquals(PRODUCT_CODE, productSolr.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME1, productSolr.getProductName());
    Assertions.assertEquals(BRAND, productSolr.getBrand());
    Assertions.assertEquals(LOCATION_PATH, productSolr.getProductMainImage());
  }

  @Test
  public void constructProduct_fetchMasterDataTest_throwsExcpetion() throws Exception {
    product.setSynchronized(Boolean.FALSE);
    ProductSolr productSolr = new ProductSolr();
    this.productSolrConstructorService.constructProduct(productSolr, product, true);
    Assertions.assertEquals(PRODUCT_CODE, productSolr.getProductCode());
  }

  @Test
  public void constructProduct_fetchMasterDataTest_noMainImage() throws Exception {
    product.setSynchronized(Boolean.FALSE);
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    product.getMasterDataProduct().getMasterDataProductImages().get(0).setMainImage(Boolean.FALSE);
    ProductSolr productSolr = new ProductSolr();
    this.productSolrConstructorService.constructProduct(productSolr, product, true);
    Assertions.assertEquals(PRODUCT_CODE, productSolr.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productSolr.getProductName());
    Assertions.assertEquals(BRAND, productSolr.getBrand());
    Assertions.assertEquals(new SimpleDateFormat("yyyy/MM/dd").parse("2021/01/20"),
        productSolr.getProductCenterUpdatedDate());
  }

  @Test
  public void constructProductWithTotalScore() throws Exception {
    product.setProductScore(new ProductScore());
    product.getProductScore().setTotalScore(80.900019);
    ProductSolr productSolr = new ProductSolr();
    this.productSolrConstructorService.constructProduct(productSolr, product, true);
    Assertions.assertEquals(80.90, productSolr.getProductScoreTotal(), 1);
  }

  @Test
  public void constructProductFromMasterDataChangesTest() throws Exception {
    productSolr.setProductScoreTotal(10.17000001);
    productAndTotalScoreMap.put(PRODUCT_SKU, 20.7100001);
    Mockito.when(businessPartnerService
        .isBusinessPartnerUmkmMerchant(productDomainEventModel.getStoreId(), productSolr.getMerchantCode()))
        .thenReturn(true);
    Map<String, FieldValueObject> result =
        this.productSolrConstructorService.constructProductFromMasterDataChanges(productSolr, productDomainEventModel,
            productAndTotalScoreMap);
    Mockito.verify(this.businessPartnerService)
        .isBusinessPartnerUmkmMerchant(productDomainEventModel.getStoreId(), productSolr.getMerchantCode());
    Assertions.assertEquals(productDomainEventModel.getBrand(), result.get(SolrFieldNames.BRAND).getNewValue());
    Assertions.assertEquals(PRODUCT_MASTER_CATALOG, result.get(SolrFieldNames.MASTER_CATALOG).getNewValue());
    Assertions.assertEquals(PRODUCT_SALES_CATALOG, result.get(SolrFieldNames.SALES_CATALOG).getNewListValues().get(0));
    Assertions.assertEquals(PRODUCT_UMKM_SALES_CATALOG, result.get(SolrFieldNames.SALES_CATALOG).getNewListValues().get(1));
    Assertions.assertEquals(20.71, Double.parseDouble(result.get(SolrFieldNames.TOTAL_SCORE).getNewValue()), 0.0);
  }

  @Test
  public void constructProductFromMasterDataChangesTest_syncProduct() throws Exception {
    productSolr.setSynchronized(true);
    Mockito.when(businessPartnerService
        .isBusinessPartnerUmkmMerchant(productDomainEventModel.getStoreId(), productSolr.getMerchantCode()))
        .thenReturn(true);
    Map<String, FieldValueObject> result =
        this.productSolrConstructorService.constructProductFromMasterDataChanges(productSolr, productDomainEventModel,
            productAndTotalScoreMap);
    Mockito.verify(this.businessPartnerService)
        .isBusinessPartnerUmkmMerchant(productDomainEventModel.getStoreId(), productSolr.getMerchantCode());
    Assertions.assertEquals(productDomainEventModel.getBrand(), result.get(SolrFieldNames.BRAND).getNewValue());
    Assertions.assertEquals(PRODUCT_MASTER_CATALOG, result.get(SolrFieldNames.MASTER_CATALOG).getNewValue());
    Assertions.assertEquals(PRODUCT_SALES_CATALOG, result.get(SolrFieldNames.SALES_CATALOG).getNewListValues().get(0));
    Assertions.assertEquals(PRODUCT_UMKM_SALES_CATALOG, result.get(SolrFieldNames.SALES_CATALOG).getNewListValues().get(1));
  }

  @Test
  public void constructProductFromMasterDataChanges_emptyMasterCatalogCodeTest() throws Exception {
    productDomainEventModel.getProductCategories().get(0).getCategory().getCatalog().setCatalogCode(StringUtils.EMPTY);
    productSolr.setMasterCatalog(PRODUCT_MASTER_CATALOG);
    Mockito.when(businessPartnerService
        .isBusinessPartnerUmkmMerchant(productDomainEventModel.getStoreId(), productSolr.getMerchantCode()))
        .thenReturn(true);
    Map<String, FieldValueObject> result =
        this.productSolrConstructorService.constructProductFromMasterDataChanges(productSolr, productDomainEventModel,
            productAndTotalScoreMap);
    Mockito.verify(this.businessPartnerService)
        .isBusinessPartnerUmkmMerchant(productDomainEventModel.getStoreId(), productSolr.getMerchantCode());
    Assertions.assertEquals(productDomainEventModel.getBrand(), result.get(SolrFieldNames.BRAND).getNewValue());
    Assertions.assertEquals(BRAND, result.get(SolrFieldNames.BRAND).getNewValue());
    Assertions.assertEquals(PRODUCT_NAME, result.get(SolrFieldNames.PRODUCT_NAME).getNewValue());
    Assertions.assertEquals(PRODUCT_SALES_CATALOG, result.get(SolrFieldNames.SALES_CATALOG).getNewListValues().get(0));
    Assertions.assertEquals(PRODUCT_UMKM_SALES_CATALOG, result.get(SolrFieldNames.SALES_CATALOG).getNewListValues().get(1));
  }

  @Test
  public void constructProductFromMasterDataChanges_existingSalesCatalogTest() throws Exception {
    productSolr.setSalesCatalog(new ArrayList<>());
    productSolr.getSalesCatalog().add(PRODUCT_OLD_SALES_CATALOG);
    Mockito.when(businessPartnerService
        .isBusinessPartnerUmkmMerchant(productDomainEventModel.getStoreId(), productSolr.getMerchantCode()))
        .thenReturn(true);
    Map<String, FieldValueObject> result =
        this.productSolrConstructorService.constructProductFromMasterDataChanges(productSolr, productDomainEventModel,
            productAndTotalScoreMap);
    Mockito.verify(this.businessPartnerService)
        .isBusinessPartnerUmkmMerchant(productDomainEventModel.getStoreId(), productSolr.getMerchantCode());
    Assertions.assertTrue(result.size() == 4);
    Assertions.assertEquals(productDomainEventModel.getBrand(), result.get(SolrFieldNames.BRAND).getNewValue());
    Assertions.assertEquals(PRODUCT_MASTER_CATALOG, result.get(SolrFieldNames.MASTER_CATALOG).getNewValue());
    Assertions.assertEquals(LOCATION_PATH, result.get(SolrFieldNames.PRODUCT_MAIN_IMAGE).getNewValue());
    Assertions.assertEquals(BRAND, result.get(SolrFieldNames.BRAND).getNewValue());
  }

  @Test
  public void constructProductFromMasterDataChanges_noMainImageTest() throws Exception {
    productDomainEventModel.getImages().get(0).setMainImage(Boolean.FALSE);
    Mockito.when(businessPartnerService
        .isBusinessPartnerUmkmMerchant(productDomainEventModel.getStoreId(), productSolr.getMerchantCode()))
        .thenReturn(true);
    Map<String, FieldValueObject> result =
        this.productSolrConstructorService.constructProductFromMasterDataChanges(productSolr, productDomainEventModel,
            productAndTotalScoreMap);
    Mockito.verify(this.businessPartnerService)
        .isBusinessPartnerUmkmMerchant(productDomainEventModel.getStoreId(), productSolr.getMerchantCode());
    Assertions.assertEquals(productDomainEventModel.getBrand(), result.get(SolrFieldNames.BRAND).getNewValue());
    Assertions.assertEquals(PRODUCT_MASTER_CATALOG, result.get(SolrFieldNames.MASTER_CATALOG).getNewValue());
    Assertions.assertEquals(PRODUCT_SALES_CATALOG, result.get(SolrFieldNames.SALES_CATALOG).getNewListValues().get(0));
    Assertions.assertEquals(PRODUCT_UMKM_SALES_CATALOG, result.get(SolrFieldNames.SALES_CATALOG).getNewListValues().get(1));
  }

  @Test
  public void constructMasterCatalogTest() {
    MasterCatalog masterCatalog = this.productSolrConstructorService.constructMasterCatalog(PRODUCT_MASTER_CATALOG);
    Assertions.assertEquals(masterCatalog.getCatalogCode(), CATALOG_CODE);
    Assertions.assertEquals(masterCatalog.getCategory().getCategoryCode(), CATEGORY_CODE);
  }

  @Test
  public void constructMasterCatalogTest_emptyCatalogTest() {
    MasterCatalog masterCatalog = this.productSolrConstructorService.constructMasterCatalog(StringUtils.EMPTY);
    Assertions.assertNull(masterCatalog);
  }

  @Test
  public void constructProduct_nullCategoryOnL3() throws Exception {
    product.setSynchronized(Boolean.FALSE);
    product.setMasterCatalog(null);
    ProductSolr productSolr = new ProductSolr();
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    this.productSolrConstructorService.constructProduct(productSolr, product, true);
    Assertions.assertEquals(PRODUCT_CODE, productSolr.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productSolr.getProductName());
    Assertions.assertEquals(BRAND, productSolr.getBrand());
    Assertions.assertEquals(LOCATION_PATH, productSolr.getProductMainImage());
  }

  @Test
  public void constructProductFromMasterDataChangesTest_unsyncProduct() throws Exception {
    productSolr.setSynchronized(false);
    Mockito.when(businessPartnerService
        .isBusinessPartnerUmkmMerchant(productDomainEventModel.getStoreId(), productSolr.getMerchantCode()))
        .thenReturn(true);
    Map<String, FieldValueObject> fieldValueObjectMap =
        this.productSolrConstructorService.constructProductFromMasterDataChanges(productSolr, productDomainEventModel,
            productAndTotalScoreMap);
    Mockito.verify(this.businessPartnerService)
        .isBusinessPartnerUmkmMerchant(productDomainEventModel.getStoreId(), productSolr.getMerchantCode());
    Assertions.assertEquals(PRODUCT_MASTER_CATALOG, fieldValueObjectMap.get(SolrFieldNames.MASTER_CATALOG).getNewValue());
    Assertions.assertEquals(PRODUCT_SALES_CATALOG,
        fieldValueObjectMap.get(SolrFieldNames.SALES_CATALOG).getNewListValues().get(0));
    Assertions.assertEquals(PRODUCT_UMKM_SALES_CATALOG,
        fieldValueObjectMap.get(SolrFieldNames.SALES_CATALOG).getNewListValues().get(1));
  }

  @Test
  public void constructProductTest_unsyncProduct() throws Exception {
    product.setSynchronized(false);
    ProductSolr productSolr = new ProductSolr();
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    this.productSolrConstructorService.constructProduct(productSolr, product, true);
    Assertions.assertEquals(PRODUCT_CODE, productSolr.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productSolr.getProductName());
    Assertions.assertEquals(BRAND, productSolr.getBrand());
  }

  @Test
  public void constructProductTest_unsyncProductTest() throws Exception {
    product.setSynchronized(false);
    ProductSolr productSolr = new ProductSolr();
    systemParameter.setValue("false");
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    Mockito.when(productHelperService
        .setMasterDataProductFromMasterData(stringArgumentCaptor.capture(), stringArgumentCaptor.capture(),
            stringArgumentCaptor.capture(), productArgumentCaptor.capture())).thenReturn(product);
    this.productSolrConstructorService.constructProduct(productSolr, product, true);
    Mockito.verify(productHelperService).setMasterDataProductFromMasterData(stringArgumentCaptor.getAllValues().get(0),
        stringArgumentCaptor.getAllValues().get(1), stringArgumentCaptor.getAllValues().get(2),
        productArgumentCaptor.getValue());
    Assertions.assertEquals(PRODUCT_CODE, productSolr.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productSolr.getProductName());
    Assertions.assertEquals(BRAND, productSolr.getBrand());
    Assertions.assertEquals(product.getProductCode(), productArgumentCaptor.getValue().getProductCode());
  }

  @Test
  public void constructProduct_nullCategoryOnL3Test() throws Exception {
    product.setSynchronized(Boolean.TRUE);
    product.setMasterCatalog(null);
    systemParameter.setValue("false");
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    Mockito.when(this.productHelperService
        .setMasterDataProductFromMasterData(eq(STORE_ID), Mockito.anyString(), eq(XPRODUCT_SOLR_REINDEX),
            Mockito.any(Product.class))).thenReturn(product);
    ProductSolr productSolr = new ProductSolr();
    this.productSolrConstructorService.constructProduct(productSolr, product, true);
    Mockito.verify(this.productHelperService)
        .setMasterDataProductFromMasterData(eq(STORE_ID), Mockito.anyString(), eq(XPRODUCT_SOLR_REINDEX),
            Mockito.any(Product.class));
    Assertions.assertEquals(PRODUCT_CODE, productSolr.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productSolr.getProductName());
    Assertions.assertEquals(BRAND, productSolr.getBrand());
    Assertions.assertEquals(LOCATION_PATH, productSolr.getProductMainImage());
  }

  @Test
  public void constructProductWithTotalScoreTest() throws Exception {
    product.setProductScore(new ProductScore());
    product.getProductScore().setTotalScore(80.900019);
    systemParameter.setValue("false");
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    Mockito.when(this.productHelperService
        .setMasterDataProductFromMasterData(eq(STORE_ID), Mockito.anyString(), eq(XPRODUCT_SOLR_REINDEX),
            Mockito.any(Product.class))).thenReturn(product);
    ProductSolr productSolr = new ProductSolr();
    this.productSolrConstructorService.constructProduct(productSolr, product, true);
    Mockito.verify(this.productHelperService)
        .setMasterDataProductFromMasterData(eq(STORE_ID), Mockito.anyString(), eq(XPRODUCT_SOLR_REINDEX),
            Mockito.any(Product.class));
    Assertions.assertEquals(80.90, productSolr.getProductScoreTotal(), 1);
  }

  @Test
  public void constructProductTest1() throws Exception {
    ProductSolr productSolr = new ProductSolr();
    systemParameter.setValue("false");
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    Mockito.when(productHelperService
        .setMasterDataProductFromMasterData(stringArgumentCaptor.capture(), stringArgumentCaptor.capture(),
            stringArgumentCaptor.capture(), productArgumentCaptor.capture())).thenReturn(product);
    this.productSolrConstructorService.constructProduct(productSolr, product, true);
    Mockito.verify(productHelperService).setMasterDataProductFromMasterData(stringArgumentCaptor.getAllValues().get(0),
        stringArgumentCaptor.getAllValues().get(1), stringArgumentCaptor.getAllValues().get(2),
        productArgumentCaptor.getValue());
    Assertions.assertEquals(PRODUCT_CODE, productSolr.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productSolr.getProductName());
    Assertions.assertEquals(BRAND, productSolr.getBrand());
    Assertions.assertEquals(80.0, productSolr.getProductScoreTotal(), 0);
    Assertions.assertEquals(product.getProductCode(), productArgumentCaptor.getValue().getProductCode());
    Assertions.assertFalse(productSolr.getIsArchived());
    Assertions.assertFalse(productSolr.getOff2OnChannelActive());
  }

  @Test
  public void constructProduct_fetchMasterDataTest1() throws Exception {
    product.setSynchronized(Boolean.TRUE);
    systemParameter.setValue("false");
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    Mockito.when(this.productHelperService
        .setMasterDataProductFromMasterData(eq(STORE_ID), Mockito.anyString(), eq(XPRODUCT_SOLR_REINDEX),
            Mockito.any(Product.class))).thenReturn(product);
    ProductSolr productSolr = new ProductSolr();
    this.productSolrConstructorService.constructProduct(productSolr, product, true);
    Mockito.verify(this.productHelperService)
        .setMasterDataProductFromMasterData(eq(STORE_ID), Mockito.anyString(), eq(XPRODUCT_SOLR_REINDEX),
            Mockito.any(Product.class));
    Assertions.assertEquals(PRODUCT_CODE, productSolr.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productSolr.getProductName());
    Assertions.assertEquals(BRAND, productSolr.getBrand());
    Assertions.assertEquals(LOCATION_PATH, productSolr.getProductMainImage());
  }

  @Test
  public void constructProduct_fetchMasterDataTest_throwsTestExcpetion() throws Exception {
    product.setSynchronized(Boolean.TRUE);
    systemParameter.setValue("false");
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    Mockito.when(this.productHelperService
        .setMasterDataProductFromMasterData(eq(STORE_ID), Mockito.anyString(), eq(XPRODUCT_SOLR_REINDEX),
            Mockito.any(Product.class))).thenThrow(Exception.class);
    ProductSolr productSolr = new ProductSolr();
    this.productSolrConstructorService.constructProduct(productSolr, product, true);
    Mockito.verify(this.productHelperService)
        .setMasterDataProductFromMasterData(eq(STORE_ID), Mockito.anyString(), eq(XPRODUCT_SOLR_REINDEX),
            Mockito.any(Product.class));
    Assertions.assertEquals(PRODUCT_CODE, productSolr.getProductCode());
  }

  @Test
  public void constructProduct_fetchMasterDataTest_noMainImageTest() throws Exception {
    product.setSynchronized(Boolean.TRUE);
    systemParameter.setValue("false");
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    product.getMasterDataProduct().getMasterDataProductImages().get(0).setMainImage(Boolean.FALSE);
    Mockito.when(this.productHelperService
        .setMasterDataProductFromMasterData(eq(STORE_ID), Mockito.anyString(), eq(XPRODUCT_SOLR_REINDEX),
            Mockito.any(Product.class))).thenReturn(product);
    ProductSolr productSolr = new ProductSolr();
    this.productSolrConstructorService.constructProduct(productSolr, product, true);
    Mockito.verify(this.productHelperService)
        .setMasterDataProductFromMasterData(eq(STORE_ID), Mockito.anyString(), eq(XPRODUCT_SOLR_REINDEX),
            Mockito.any(Product.class));
    Assertions.assertEquals(PRODUCT_CODE, productSolr.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productSolr.getProductName());
    Assertions.assertEquals(BRAND, productSolr.getBrand());
    Assertions.assertEquals(new SimpleDateFormat("yyyy/MM/dd").parse("2021/01/20"),
        productSolr.getProductCenterUpdatedDate());
  }

}
