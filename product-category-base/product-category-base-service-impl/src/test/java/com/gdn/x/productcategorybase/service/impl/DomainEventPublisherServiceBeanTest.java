package com.gdn.x.productcategorybase.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.model.BrandHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.InternalProductHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.PBPProductAttributeBackFillingEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCreationFailureDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.VendorPublishEventModel;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.CategoryChangeEventType;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.ExtractionType;
import com.gdn.x.productcategorybase.ProductMasterEvent;
import com.gdn.x.productcategorybase.ProductPublishEventType;
import com.gdn.x.productcategorybase.config.KafkaPublisher;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.AggregateImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AggregateProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AggregateProductCategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AggregateProductItemDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.BrandApprovedOrRejectedDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.BrandAuthDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.BrandDeleteDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.BrandDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CategoryHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ImagePathUpdateDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeExtractionModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCodeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductMasterEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductSalesCategoryMapping;
import com.gdn.x.productcategorybase.domain.event.model.ProductScoreUpdateDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddBatchPcbProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddBrandListDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddPcbProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrDeleteBatchPcbProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrDeleteBrandDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrDeletePcbProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrUpdateBrandDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.TerminatedSellerSkuCleanupStatusEventModel;
import com.gdn.x.productcategorybase.domain.event.model.TerminatedSellerSkuImageCleanupEventModel;
import com.gdn.x.productcategorybase.domain.event.model.VatUpdateDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.VatUpdateHistoryDomainEventModel;
import com.gdn.x.productcategorybase.dto.ProductAndItemLevelUpdatesDTO;
import com.gdn.x.productcategorybase.dto.ProductPublishUpdateDTO;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.OriginalSalesCategory;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;
import com.gdn.x.productcategorybase.entity.solr.SolrUpdateBrandModel;
import com.gdn.x.productcategorybase.enums.UpdatedFields;
import com.gdn.x.productcategorybase.service.brand.BrandService;
import com.gdn.x.productcategorybase.service.brand.BrandWipService;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import com.gdn.x.productcategorybase.util.AggregateUtil;
import com.google.common.collect.ImmutableSet;
import org.springframework.test.util.ReflectionTestUtils;

public class DomainEventPublisherServiceBeanTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String BRAND_CODE = "dummy-brand-code";
  private static final String BRAND_NAME = "dummy-brand-name";
  private static final String ID = "id";
  private static final String OLD_SALES_CATEGORY = "sales_1";
  private static final String NEW_SALES_CATEGORY = "sales_2";
  private static final String NEW_UMKM_SALES_CATEGORY = "sales_3";
  private static final String ATTRIBUTE_NAME = "ATTRIBUTE_NAME";
  private static final String ATTRIBUTE_CODE = "ATTRIBUTE_CODE";
  private static final String BRAND_REQUEST_CODE = "brandRequestCode";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String PRODUCT_CODE = "productCode";
  private static final String REQUEST_ID = "requestId";
  private static final String ITEM_ID = "itemId";
  private static final String ITEM_CODE = "itemCode";
  private static final String ITEM_NAME = "itemName";
  private static final String UPDATED_BY = "updatedBy";
  private static final String SELLER_CODE = "seller-code";
  private static final String OLD_VALUE_AUTH = "Inactive";
  private static final String NEW_VALUE_AUTH = "Active";
  private static final String ACTIVITY_AUTH = "Change status";
  private static final String IMAGE_PATH_1 = "imagePath1";
  private static final String IMAGE_PATH_2 = "imagePath2";
  private static final String SKU_CODE_1 = "skuCode1";
  private static final String SKU_CODE_2 = "skuCode2";
  private static final String EVENT_NAME = "eventName";
  private static final String PRODUCT_NAME = "productName";

  private BrandWip brandWip;
  private AttributeDomainEventModel attributeDomainEventModel;
  private BrandAuthorisationHistory brandAuthorisationHistory;
  private TerminatedSellerSkuCleanupStatusEventModel terminatedSellerSkuCleanupStatusEventModel;
  private TerminatedSellerSkuImageCleanupEventModel terminatedSellerSkuImageCleanupEventModel;
  private RestrictedKeywordHistoryEventModel restrictedKeywordHistoryEventModel;
  private CategoryHistoryEventModel categoryHistoryEventModels;

  @Mock
  private BrandService brandServiceBean;

  @Mock
  private BrandWipService brandWipService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Captor
  private ArgumentCaptor<ProductCodeDomainEventModel> productCodeDomainEventModelArgumentCaptor;

  @Captor
  private ArgumentCaptor<ImagePathUpdateDomainEventModel> imagePathUpdateDomainEventModel;

  @InjectMocks
  private DomainEventPublisherServiceBean domainEventPublisherServiceBean;

  private Product generateProduct() {
    Product product = new Product();
    ProductItem productItem = new ProductItem();
    productItem.getProductItemImages().add(new ProductItemImage());
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setMarkForDelete(true);
    productItem.getProductItemImages().add(productItemImage);
    ProductItem productItem2 = new ProductItem();
    productItem2.setMarkForDelete(true);
    product.getProductItems().add(productItem);
    product.getProductItems().add(productItem2);
    ProductCategory productCategory = new ProductCategory();
    ProductCategory productCategory2 = new ProductCategory();
    Category category = new Category();
    category.setCatalog(new Catalog());
    category.getCatalog().setCatalogType(CatalogType.MASTER_CATALOG);
    category.setParentCategory(new Category());
    productCategory.setCategory(category);
    productCategory2.setMarkForDelete(true);
    product.getProductCategories().add(productCategory);
    product.getProductCategories().add(productCategory2);
    ProductAttributeValue productAttributeValue1 = new ProductAttributeValue();
    productAttributeValue1.setAllowedAttributeValue(new AllowedAttributeValue());
    productAttributeValue1.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);

    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode(BRAND_CODE);
    ProductAttributeValue productAttributeValue2 = new ProductAttributeValue();
    productAttributeValue2.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
    productAttributeValue2.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);

    ProductAttributeValue productAttributeValue3 = new ProductAttributeValue();
    productAttributeValue3.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    ProductAttributeValue productAttributeValue4 = new ProductAttributeValue();
    productAttributeValue4.setMarkForDelete(true);
    Attribute attribute1 = new Attribute();
    attribute1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    Attribute attribute2 = new Attribute();
    attribute2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    Attribute attribute3 = new Attribute();
    attribute3.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    ProductAttribute productAttribute1 = new ProductAttribute();
    productAttribute1.setAttribute(attribute1);
    productAttribute1.getProductAttributeValues().add(productAttributeValue1);
    ProductAttribute productAttribute2 = new ProductAttribute();
    productAttribute2.setAttribute(attribute2);
    productAttribute2.setProductAttributeName(DomainEventPublisherServiceBean.BRAND);
    productAttribute2.getProductAttributeValues().add(productAttributeValue2);
    ProductAttribute productAttribute3 = new ProductAttribute();
    productAttribute3.setAttribute(attribute3);
    productAttribute3.getProductAttributeValues().add(productAttributeValue3);
    productAttribute3.getProductAttributeValues().add(productAttributeValue4);
    ProductAttribute productAttribute4 = new ProductAttribute();
    productAttribute4.setMarkForDelete(true);
    product.getProductAttributes().add(productAttribute1);
    product.getProductAttributes().add(productAttribute2);
    product.getProductAttributes().add(productAttribute3);
    product.getProductAttributes().add(productAttribute4);
    ProductImage productImage2 = new ProductImage();
    productImage2.setMarkForDelete(true);
    product.getProductImages().add(new ProductImage());
    product.getProductImages().add(productImage2);
    return product;
  }

  private ProductAttributeValue generateProductAttributeValue() {
    return Optional.ofNullable(generateProduct())
        .map(Product::getProductAttributes)
        .map(vals -> vals.stream()
            .filter(val -> DomainEventPublisherServiceBean.BRAND.equals(val.getProductAttributeName()))
            .findFirst()
            .orElse(null))
        .map(ProductAttribute::getProductAttributeValues)
        .map(List::iterator)
        .map(Iterator::next)
        .orElse(null);
  }

  private String generateBrandCode() {
    return Optional.ofNullable(generateProductAttributeValue())
        .map(ProductAttributeValue::getPredefinedAllowedAttributeValue)
        .map(PredefinedAllowedAttributeValue::getPredefinedAllowedAttributeCode)
        .orElse(null);
  }

  private Brand generateBrand() {
    return new Brand(generateBrandCode(),"brandName",null,"brandLogoPath");
  }

  private BrandWipResponse generateBrandWip() {
    Brand brand = generateBrand();
    return BrandWipResponse.builder().id(brand.getId()).brandCode(brand.getBrandCode()).brandName(brand.getBrandName())
        .brandLogoPath(brand.getBrandLogoPath()).profileBannerPath(brand.getProfileBannerPath())
        .state(null).brandCode(brand.getBrandCode()).validBrand(true).build();
  }

  private Category generateCategory() throws Exception {
    Category category = new Category();
    category.setCatalog(new Catalog());
    category.getCatalog().setCatalogType(CatalogType.MASTER_CATALOG);
    category.setParentCategory(new Category());
    category.setUmkm(true);
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute.setName(ATTRIBUTE_NAME);
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(attribute);
    categoryAttribute.setCategory(category);
    categoryAttribute.setMainDefiningAttribute(true);
    category.setCategoryAttributes(Arrays.asList(categoryAttribute));
    return category;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);

    brandWip = new BrandWip();
    brandWip.setBrandName(BRAND_NAME);
    brandWip.setBrandRequestCode(BRAND_REQUEST_CODE);
    brandWip.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    brandWip.setProtectedBrand(true);

    attributeDomainEventModel = new AttributeDomainEventModel();
    attributeDomainEventModel.setAttributeCode(ATTRIBUTE_CODE);

    Mockito.doNothing().when(kafkaProducer).send(Mockito.anyString(), Mockito.any());
    Mockito.doNothing().when(kafkaProducer).send(Mockito.anyString(), Mockito.anyString(), Mockito.any());

    brandAuthorisationHistory = new BrandAuthorisationHistory();
    brandAuthorisationHistory.setBrandCode(BRAND_CODE);
    brandAuthorisationHistory.setSellerCode(SELLER_CODE);
    brandAuthorisationHistory.setOldStatus(OLD_VALUE_AUTH);
    brandAuthorisationHistory.setNewStatus(NEW_VALUE_AUTH);
    brandAuthorisationHistory.setActivity(ACTIVITY_AUTH);

    terminatedSellerSkuCleanupStatusEventModel = new TerminatedSellerSkuCleanupStatusEventModel();
    terminatedSellerSkuCleanupStatusEventModel.setProductCode(PRODUCT_CODE);
    terminatedSellerSkuImageCleanupEventModel = new TerminatedSellerSkuImageCleanupEventModel();
    terminatedSellerSkuImageCleanupEventModel.setProductCode(PRODUCT_CODE);
  }

  @Test
  public void publishProductTest() throws Exception {
    Product product = generateProduct();
    product.setReviewPending(true);
    ProductDomainEventModel response = this.domainEventPublisherServiceBean.publishProduct(product);
    assertTrue(response.isReviewPending());
  }

  @Test
  public void publishProductTest_RootCategory() throws Exception {
    Product product = generateProduct();
    Category rootCategory = generateCategory();
    rootCategory.setParentCategory(null);
    product.getProductCategories()
        .add(new ProductCategory(product, rootCategory, DEFAULT_STORE_ID));
    this.domainEventPublisherServiceBean.publishProduct(product);
  }

  @Test
  public void getBrandLogoUrlTest() throws Exception {
    ProductAttributeValue productAttributeValue = generateProductAttributeValue();
    String brandCode = generateBrandCode();
    Brand brand = generateBrand();

    when(brandServiceBean.findByBrandCode(brandCode)).thenReturn(brand);

    String result = this.domainEventPublisherServiceBean.getBrandLogoUrl(productAttributeValue);

    assertEquals(String.format("/%s/%s",brandCode,brand.getBrandLogoPath()),result);
    verify(brandServiceBean).findByBrandCode(brandCode);
  }

  @Test
  public void getBrandLogoUrlTest_brandNotFoundBecauseOfError() throws Exception {
    ProductAttributeValue productAttributeValue = generateProductAttributeValue();
    String brandCode = generateBrandCode();
    BrandWipResponse brandWip = generateBrandWip();

    when(brandServiceBean.findByBrandCode(brandCode)).thenReturn(null);
    when(brandWipService.getBrandWipDetail(DomainEventPublisherServiceBean.STORE_ID, brandCode)).thenReturn(brandWip);

    String result = this.domainEventPublisherServiceBean.getBrandLogoUrl(productAttributeValue);

    assertEquals(String.format("/%s/%s", brandCode, brandWip.getBrandLogoPath()), result);
    verify(brandServiceBean).findByBrandCode(brandCode);
    verify(brandWipService).getBrandWipDetail(DomainEventPublisherServiceBean.STORE_ID, brandCode);
  }

  @Test
  public void getBrandLogoUrlTest_brandNotFoundBecauseException() throws Exception {
    ProductAttributeValue productAttributeValue = generateProductAttributeValue();
    String brandCode = generateBrandCode();
    BrandWipResponse brandWip = generateBrandWip();

    when(brandServiceBean.findByBrandCode(brandCode)).thenThrow(Exception.class);

    this.domainEventPublisherServiceBean.getBrandLogoUrl(productAttributeValue);

    verify(brandServiceBean).findByBrandCode(brandCode);
  }

  @Test
  public void getBrandLogoUrlTest_brandNotFound() throws Exception {
    ProductAttributeValue productAttributeValue = generateProductAttributeValue();
    String brandCode = generateBrandCode();
    BrandWipResponse brandWip = generateBrandWip();

    when(brandServiceBean.findByBrandCode(brandCode)).thenReturn(null);
    when(brandWipService.getBrandWipDetail(DomainEventPublisherServiceBean.STORE_ID,brandCode)).thenReturn(brandWip);

    String result = this.domainEventPublisherServiceBean.getBrandLogoUrl(productAttributeValue);

    assertEquals(String.format("/%s/%s",brandCode,brandWip.getBrandLogoPath()),result);
    verify(brandServiceBean).findByBrandCode(brandCode);
    verify(brandWipService).getBrandWipDetail(DomainEventPublisherServiceBean.STORE_ID,brandCode);
  }

  @Test
  public void getBrandLogoUrlTest_brandNotFound_brandWipNotFound() throws Exception {
    ProductAttributeValue productAttributeValue = generateProductAttributeValue();
    String brandCode = generateBrandCode();
    BrandWipResponse brandWip = generateBrandWip();

    when(brandServiceBean.findByBrandCode(brandCode)).thenReturn(null);
    when(brandWipService.getBrandWipDetail(DomainEventPublisherServiceBean.STORE_ID,brandCode)).thenReturn(null);

    String result = this.domainEventPublisherServiceBean.getBrandLogoUrl(productAttributeValue);

    assertNull(result);
    verify(brandServiceBean).findByBrandCode(brandCode);
    verify(brandWipService).getBrandWipDetail(DomainEventPublisherServiceBean.STORE_ID,brandCode);
  }

  @Test
  public void republishProductToAgp() throws Exception {
    Product product = generateProduct();
    product.setReviewPending(true);
    ProductDomainEventModel productDomainEventModel =
        this.domainEventPublisherServiceBean.toProductDomainEventModel(product, false,
          StringUtils.EMPTY);
    this.domainEventPublisherServiceBean.republishProductToAgp(productDomainEventModel);
    assertTrue(productDomainEventModel.isReviewPending());
  }

  @Test
  public void toProductDomainEventModel() throws Exception {
    ReflectionTestUtils.setField(domainEventPublisherServiceBean, "ranchIntegrationEnabled", true);
    Product product = generateProduct();
    this.domainEventPublisherServiceBean.toProductDomainEventModel(product,false, StringUtils.EMPTY);
  }

  @Test
  public void toProductDomainEventDistributionInfoModel() throws Exception {
    ReflectionTestUtils.setField(domainEventPublisherServiceBean, "ranchIntegrationEnabled", true);
    Product product = generateProduct();
    Map<String, String> map = new HashMap<>();
    map.put(PRODUCT_NAME, PRODUCT_CODE);
    ObjectMapper objectMapper = new ObjectMapper();
    product.setDistributionInfo(objectMapper.writeValueAsString(map));
    this.domainEventPublisherServiceBean.toProductDomainEventModel(product,false, StringUtils.EMPTY);
    Assertions.assertNotNull(product);
  }

  @Test
  public void republishImageToAgp() throws Exception {
    Product product = generateProduct();
    AggregateImageDomainEventModel aggregateImageDomainEventModel =
        AggregateUtil.toAggregateImageDomainEventModel(product.getProductImages());
    this.domainEventPublisherServiceBean.republishImageToAgp(aggregateImageDomainEventModel);
  }

  @Test
  public void republishProductItemToAgp() throws Exception {
    Product product = generateProduct();
    AggregateProductItemDomainEventModel aggregateProductItemDomainEventModel =
        AggregateUtil.toAggregateProductItemDomainEventModel(product.getProductItems());
    this.domainEventPublisherServiceBean.republishProductItemToAgp(aggregateProductItemDomainEventModel);
  }

  @Test
  public void republishProductCategoryToAgp() throws Exception {
    Product product = generateProduct();
    AggregateProductCategoryDomainEventModel aggregateProductCategoryDomainEventModel =
        AggregateUtil.toAggregateProductCategoryDomainEventModel(product.getProductCategories());
    this.domainEventPublisherServiceBean.republishProductCategoryToAgp(aggregateProductCategoryDomainEventModel);
  }

  @Test
  public void republishProductAttributeToAgp() throws Exception {
    Product product = generateProduct();
    AggregateProductAttributeDomainEventModel aggregateProductAttributeDomainEventModel =
        AggregateUtil.toAggregateProductAttributeDomainEventModel(product.getProductAttributes());
    this.domainEventPublisherServiceBean.republishProductAttributeToAgp(aggregateProductAttributeDomainEventModel);
  }

  @Test
  public void publishNewProductTest() throws Exception {
    Product product = generateProduct();
    this.domainEventPublisherServiceBean.publishProduct(product, true);
  }

  @Test
  public void publishProductWithProcessedImages() throws Exception {
    Product product = generateProduct();
    ProductImage productImage = new ProductImage();
    productImage.setOriginalImage(true);
    ProductImage productImage1 = new ProductImage();
    productImage1.setActive(true);
    ProductImage productImage2 = new ProductImage();
    productImage2.setActive(false);
    ProductImage productImage3 = new ProductImage();
    productImage3.setOriginalImage(false);
    product.setProductImages(Arrays.asList(productImage, productImage1, productImage2, productImage3));
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setOriginalImage(false);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setOriginalImage(true);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setActive(false);
    ProductItemImage productItemImage3 = new ProductItemImage();
    productItemImage3.setActive(true);
    product.getProductItems().get(0)
        .setProductItemImages(Arrays.asList(productItemImage, productItemImage1, productItemImage2, productItemImage3));
    product.setReviewPending(true);
    ProductDomainEventModel productDomainEventModel =
        this.domainEventPublisherServiceBean.publishProduct(product, true);
    verify(kafkaProducer).send(Mockito.eq(DomainEventName.PRODUCT_PUBLISH),Mockito.any(),Mockito.any());
    assertEquals(3, productDomainEventModel.getImages().size());
    assertEquals(3, productDomainEventModel.getProductItems().get(0).getImages().size());
    assertTrue(productDomainEventModel.isReviewPending());
  }

  @Test
  public void publishNewProductEmptyCategoryChangeTest() throws Exception {
    Product product = generateProduct();
    product.setReviewPending(true);
    ProductDomainEventModel productDomainEventModel =
        this.domainEventPublisherServiceBean.publishProduct(product, null);
    assertNull(productDomainEventModel.getProductSalesCategoryMapping());
    assertTrue(productDomainEventModel.isReviewPending());
  }

  @Test
  public void publishNewProductCategoryChangeTest() throws Exception {
    Product product = generateProduct();
    ProductDomainEventModel productDomainEventModel = this.domainEventPublisherServiceBean.publishProduct(product,
        new ProductSalesCategoryMapping(Collections.singletonList(OLD_SALES_CATEGORY),
            Collections.singletonList(NEW_SALES_CATEGORY), Collections.singletonList(NEW_UMKM_SALES_CATEGORY),
            new ArrayList<>(), new ArrayList<>(),true));
    Assertions.assertNotNull(productDomainEventModel.getProductSalesCategoryMapping());
    Assertions.assertEquals(OLD_SALES_CATEGORY,
        productDomainEventModel.getProductSalesCategoryMapping().getOldSalesCategoryCodes().get(0));
    assertEquals(NEW_SALES_CATEGORY,
        productDomainEventModel.getProductSalesCategoryMapping().getNewSalesCategoryCodes().get(0));
    assertEquals(NEW_UMKM_SALES_CATEGORY,
        productDomainEventModel.getProductSalesCategoryMapping().getNewUmkmSalesCategoryCodes().get(0));
  }

  @Test
  public void publishNewProductTest_RootCategory() throws Exception {
    Product product = generateProduct();
    Category rootCategory = generateCategory();
    rootCategory.setParentCategory(null);
    product.getProductCategories()
        .add(new ProductCategory(product, rootCategory, DEFAULT_STORE_ID));
    this.domainEventPublisherServiceBean.publishProduct(product, true);
  }

  @Test
  public void publishCategoryTest() throws Exception {
    Category category = generateCategory();
    category.setOriginalSalesCategory(new OriginalSalesCategory());
    category.setB2bExclusive(true);
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    categoryAttribute.setAttribute(attribute);
    category.setCategoryAttributes(Arrays.asList(categoryAttribute));
    CategoryDomainEventModel categoryDomainEventModel = this.domainEventPublisherServiceBean.publishCategory(category,
        Arrays.asList(CategoryChangeEventType.DATA_CHANGE), new HashSet<>(), false);
    assertTrue(categoryDomainEventModel.isUmkm());
    assertTrue(categoryDomainEventModel.isB2bExclusive());
    assertEquals(CategoryChangeEventType.DATA_CHANGE, categoryDomainEventModel.getCategoryChangeTypes().get(0));
    assertEquals(ATTRIBUTE_CODE, categoryDomainEventModel.getAttributeCodes().get(0));
  }

  @Test
  public void publishNewCategoryTest() throws Exception {
    Category category = generateCategory();
    category.setOriginalSalesCategory(new OriginalSalesCategory());
    CategoryDomainEventModel categoryDomainEventModel = this.domainEventPublisherServiceBean.publishCategory(category,
        Arrays.asList(CategoryChangeEventType.DATA_CHANGE), new HashSet<>(), true);
    assertTrue(categoryDomainEventModel.isUmkm());
    assertEquals(CategoryChangeEventType.DATA_CHANGE, categoryDomainEventModel.getCategoryChangeTypes().get(0));
    assertTrue(categoryDomainEventModel.isNewData());
  }

  @Test
  public void publishCategoryStatusChangeTrueTest() throws Exception {
    Category category = generateCategory();
    CategoryDomainEventModel categoryDomainEventModel = this.domainEventPublisherServiceBean.publishCategory(category,
        Arrays.asList(CategoryChangeEventType.ACTIVATE_DEACTIVATE_CHANGE), new HashSet<>(), false);
    assertTrue(categoryDomainEventModel.isUmkm());
    assertEquals(CategoryChangeEventType.ACTIVATE_DEACTIVATE_CHANGE, categoryDomainEventModel.getCategoryChangeTypes().get(0));
  }

  @Test
  public void publishAllCategoryTest() throws Exception {
    Category category = generateCategory();
    CategoryDomainEventModel result = this.domainEventPublisherServiceBean.publishAllCategory(category);
    assertEquals(result.getId(), category.getId());
  }

  @Test
  public void publishCategoryTest_rootCategory() throws Exception {
    Category category = generateCategory();
    category.setParentCategory(null);
    this.domainEventPublisherServiceBean.publishCategory(category, null, new HashSet<>(), false);
  }

  @Test
  public void publishEvent_ProductActivated_Success() throws Exception {
    Product product = generateProduct();
    ProductMasterEventModel resultModel = this.domainEventPublisherServiceBean.publishEvent(product,
            ProductMasterEvent.ACTIVATED);
    assertEquals(product.getProductCode(),
            resultModel.getProductMasterCode());
    assertEquals(ProductMasterEvent.ACTIVATED.toString().toLowerCase(),
            resultModel.getEventName());
  }

  @Test
  public void testBrandDeletedEvent() throws Exception {
    BrandDeleteDomainEventModel resultModel = this.domainEventPublisherServiceBean.publishBrandDeleted(DEFAULT_STORE_ID, BRAND_CODE);
    assertEquals(BRAND_CODE, resultModel.getBrandCode());
  }

  @Test
  public void testBrandUpdatedEvent() throws Exception {
    Brand brand = new Brand();
    brand.setBrandCode(BRAND_CODE);
    brand.setValidBrand(true);
    BrandDomainEventModel resultModel = this.domainEventPublisherServiceBean.publishBrandUpdated(brand);
    assertEquals(BRAND_CODE, resultModel.getBrandCode());
    assertTrue(resultModel.isValidBrand());
    }

  @Test
  public void testPublishSolrAddBrandEvent() throws Exception {
    SolrAddBrandListDomainEventModel solrAddBrandListDomainEventModel = new SolrAddBrandListDomainEventModel();
    solrAddBrandListDomainEventModel.setSolrBrandModels(new ArrayList<>());
    SolrAddBrandListDomainEventModel resultModel =
        this.domainEventPublisherServiceBean.publishSolrAddBrandEvent(solrAddBrandListDomainEventModel);
    //assertEquals(BRAND_CODE, resultModel.getBrandCode());
  }

  @Test
  public void testPublishSolrUpdateBrandEvent() throws Exception {
    SolrUpdateBrandModel solrUpdateBrandModel = new SolrUpdateBrandModel();
    solrUpdateBrandModel.setBrandCode(BRAND_CODE);
    SolrUpdateBrandDomainEventModel resultModel = this.domainEventPublisherServiceBean.publishSolrUpdateBrandEvent(
        SolrUpdateBrandDomainEventModel.builder().updateBrandModels(Arrays.asList(solrUpdateBrandModel)).build());
    assertEquals(BRAND_CODE, resultModel.getUpdateBrandModels().get(0).getBrandCode());
  }

  @Test
  public void testPublishDeleteBrandEvent() throws Exception {
    SolrDeleteBrandDomainEventModel solrDeleteBrandDomainEventModel = new SolrDeleteBrandDomainEventModel();
    solrDeleteBrandDomainEventModel.setIds(Arrays.asList(ID));
    SolrDeleteBrandDomainEventModel resultModel =
        this.domainEventPublisherServiceBean.publishSolrDeleteBrandEvent(solrDeleteBrandDomainEventModel);
    assertEquals(ID, resultModel.getIds().get(0));
  }

  @Test
  public void testPublishBrandApprovedOrRejectedDomainEventModel() throws Exception {
    BrandApprovedOrRejectedDomainEventModel brandApprovedOrRejectedDomainEventModel = new BrandApprovedOrRejectedDomainEventModel();
    brandApprovedOrRejectedDomainEventModel.setBrandApprovalStatus(BrandWipState.APPROVED.name());
    brandApprovedOrRejectedDomainEventModel.setBrandCode(BRAND_CODE);
    brandApprovedOrRejectedDomainEventModel.setBrandName(BRAND_NAME);
    brandApprovedOrRejectedDomainEventModel.setBrandRequestCode(BRAND_CODE);
    BrandApprovedOrRejectedDomainEventModel resultModel =
        this.domainEventPublisherServiceBean.publishBrandApprovedOrRejectedDomainEventModel(brandApprovedOrRejectedDomainEventModel);
    assertEquals(BrandWipState.APPROVED.name(), resultModel.getBrandApprovalStatus());
    assertEquals(BRAND_CODE, resultModel.getBrandCode());
    assertEquals(BRAND_NAME, resultModel.getBrandName());
  }

  @Test
  public void publishMasterAttributeInfoEvent() throws Exception {
    AttributeDomainEventModel attributeDomainEventModel = new AttributeDomainEventModel();
    attributeDomainEventModel.setAttributeCode(ATTRIBUTE_CODE);
    attributeDomainEventModel.setName(ATTRIBUTE_NAME);
    AttributeDomainEventModel resultModel =
        this.domainEventPublisherServiceBean.publishMasterAttributeInfoEvent(attributeDomainEventModel);
    assertEquals(ATTRIBUTE_CODE, resultModel.getAttributeCode());
    assertEquals(ATTRIBUTE_NAME, resultModel.getName());
  }


  @Test
  public void testPublishSolrAddBatchPcbProductEvent() {
    SolrAddBatchPcbProductDomainEventModel batchPcbProductDomainEventModel =
        SolrAddBatchPcbProductDomainEventModel.builder()
            .productDomainEventModelList(Arrays.asList(SolrAddPcbProductDomainEventModel.builder()
                .id(ID).build())).build();
    SolrAddBatchPcbProductDomainEventModel resultModel =
        this.domainEventPublisherServiceBean.publishSolrAddBatchPcbProductEvent(
            batchPcbProductDomainEventModel);
    assertEquals(ID, resultModel.getProductDomainEventModelList().get(0).getId());
  }

  @Test
  public void testPublishSolrDeleteBatchPcbProductEvent() {
    SolrDeleteBatchPcbProductDomainEventModel solrDeleteBatchPcbProductDomainEventModel =
        SolrDeleteBatchPcbProductDomainEventModel.builder().solrDeletePcbProductDomainEventModels(
            Arrays.asList(SolrDeletePcbProductDomainEventModel.builder().id(ID).build())).build();
    SolrDeleteBatchPcbProductDomainEventModel resultModel =
        this.domainEventPublisherServiceBean.publishSolrBatchDeletePcbProductEvent(
            solrDeleteBatchPcbProductDomainEventModel);
    assertEquals(ID, resultModel.getSolrDeletePcbProductDomainEventModels().get(0).getId());
  }

  @Test
  public void convertProductToProductDomainEventModelTest() throws Exception {
    Product productA = generateProduct();
    productA.setCreatedMerchant("internal");
    Product productB = generateProduct();
    productB.setCreatedMerchant(null);
    ProductDomainEventModel domainEventModelA =
        domainEventPublisherServiceBean.toProductDomainEventModel(productA, true, StringUtils.EMPTY);
    ProductDomainEventModel domainEventModelB =
        domainEventPublisherServiceBean.toProductDomainEventModel(productB, true, StringUtils.EMPTY);
    assertEquals("FLOW-3", domainEventModelA.getFlowType());
    assertEquals("FLOW-1", domainEventModelB.getFlowType());
  }

  @Test
  public void publishProductBrandChangeTest() throws Exception {
    Product product = generateProduct();
    product.setReviewPending(true);
    ProductDomainEventModel productDomainEventModel =
        this.domainEventPublisherServiceBean.publishProductChangeCategory(product, null, false, false, false, false, new HashSet<>());
    assertNull(productDomainEventModel.getProductSalesCategoryMapping());
    Assertions.assertFalse(productDomainEventModel.isBrandChanged());
    assertNull(productDomainEventModel.getPristineCategory());
    assertTrue(productDomainEventModel.isReviewPending());
    Assertions.assertFalse(productDomainEventModel.getUpdatedFields().contains(UpdatedFields.BRAND_UPDATE.name()));
    Assertions.assertFalse(productDomainEventModel.getUpdatedFields().contains(UpdatedFields.CATEGORY_UPDATE.name()));
  }

  @Test
  public void publishProductBrandChangeTestCategoryChange() throws Exception {
    Product product = generateProduct();
    product.setReviewPending(true);
    ProductDomainEventModel productDomainEventModel =
        this.domainEventPublisherServiceBean.publishProductChangeCategory(product, null, true, false, false, true, new HashSet<>());
    assertNull(productDomainEventModel.getProductSalesCategoryMapping());
    assertTrue(productDomainEventModel.isBrandChanged());
    assertNull(productDomainEventModel.getPristineCategory());
    assertTrue(productDomainEventModel.isReviewPending());
    assertTrue(productDomainEventModel.getUpdatedFields().contains(UpdatedFields.BRAND_UPDATE.name()));
    assertTrue(productDomainEventModel.getUpdatedFields().contains(UpdatedFields.CATEGORY_UPDATE.name()));
  }

  @Test
  public void publishProductBrandChangeHvingProductPublishUpdateDTOTest() throws Exception {
    Product product = generateProduct();
    product.getProductItems().get(0).setSkuCode(SKU_CODE_1);
    product.getProductItems().get(1).setSkuCode(SKU_CODE_2);
    product.getProductItems().get(1).setMarkForDelete(false);
    product.setReviewPending(true);
    ProductDomainEventModel productDomainEventModel = this.domainEventPublisherServiceBean.publishProductChangeCategory(
        new ProductPublishUpdateDTO(product, false, new HashSet<>(Arrays.asList(SKU_CODE_2))), null, true, false, false,
        false, false, new HashSet<>(), false, false,
        Set.of(UpdatedFields.BRAND_UPDATE.name()));
    assertNull(productDomainEventModel.getProductSalesCategoryMapping());
    assertTrue(productDomainEventModel.isBrandChanged());
    assertNull(productDomainEventModel.getPristineCategory());
    assertTrue(productDomainEventModel.isReviewPending());
    Assertions.assertFalse(productDomainEventModel.getProductItems().get(0).isPublishL4());
    assertTrue(productDomainEventModel.getProductItems().get(1).isPublishL4());
    assertTrue(productDomainEventModel.getEventTypes().contains(ProductPublishEventType.PUBLISH_SPECIFIC_ITEM_DATA_CHANGE_EVENT.name()));
    assertTrue(productDomainEventModel.getUpdatedFields().contains(UpdatedFields.BRAND_UPDATE.name()));
    Assertions.assertFalse(productDomainEventModel.getUpdatedFields().contains(UpdatedFields.CATEGORY_UPDATE.name()));
  }

  @Test
  public void publishProductWithSalesCatalogTest() throws Exception {
    Product product = generateProduct();
    product.setReviewPending(true);
    ProductDomainEventModel productDomainEventModel =
        domainEventPublisherServiceBean.publishProduct(product, null, false, false, false, false, false, new ProductAndItemLevelUpdatesDTO(),
            new HashSet<>(), new HashSet<>());
    assertTrue(productDomainEventModel.isReviewPending());
    assertNull(productDomainEventModel.getEventTypes());
    Assertions.assertFalse(productDomainEventModel.getUpdatedFields().contains(UpdatedFields.BRAND_UPDATE.name()));
    Assertions.assertFalse(productDomainEventModel.getUpdatedFields().contains(UpdatedFields.CATEGORY_UPDATE.name()));
  }

  @Test
  public void publishProductWithSalesCatalogTest1() throws Exception {
    Product product = generateProduct();
    product.getProductItems().get(0).setSkuCode(SKU_CODE_1);
    product.setReviewPending(true);
    Set<String> updatedFields = new HashSet<>();
    updatedFields.add(UpdatedFields.CATEGORY_UPDATE.name());
    updatedFields.add(UpdatedFields.NAME_UPDATE.name());
    updatedFields.add(UpdatedFields.BRAND_UPDATE.name());
    ProductDomainEventModel productDomainEventModel =
        domainEventPublisherServiceBean.publishProduct(product, new ProductSalesCategoryMapping(), true, false, false, false, false,
            new ProductAndItemLevelUpdatesDTO(false, new HashSet<>(Arrays.asList(SKU_CODE_1))), updatedFields,
            new HashSet<>());
    assertTrue(productDomainEventModel.isReviewPending());
    assertTrue(productDomainEventModel.getEventTypes().contains(ProductPublishEventType.PUBLISH_SPECIFIC_ITEM_DATA_CHANGE_EVENT.name()));
    assertTrue(productDomainEventModel.getProductItems().get(0).isPublishL4());
    assertTrue(productDomainEventModel.getUpdatedFields().contains(UpdatedFields.BRAND_UPDATE.name()));
    assertTrue(productDomainEventModel.getUpdatedFields().contains(UpdatedFields.CATEGORY_UPDATE.name()));
  }

  @Test
  public void publishProductCategoryChangeBrandChangeTest() throws Exception {
    Product product = generateProduct();
    ProductDomainEventModel productDomainEventModel = this.domainEventPublisherServiceBean.publishProductChangeCategory(product,
        new ProductSalesCategoryMapping(Collections.singletonList(OLD_SALES_CATEGORY),
            Collections.singletonList(NEW_SALES_CATEGORY), Collections.singletonList(NEW_UMKM_SALES_CATEGORY),
            new ArrayList<>(), new ArrayList<>(),true), true,
        false, false, false, new HashSet<>());
    Assertions.assertNotNull(productDomainEventModel.getProductSalesCategoryMapping());
    assertEquals(OLD_SALES_CATEGORY,
        productDomainEventModel.getProductSalesCategoryMapping().getOldSalesCategoryCodes().get(0));
    assertEquals(NEW_SALES_CATEGORY,
        productDomainEventModel.getProductSalesCategoryMapping().getNewSalesCategoryCodes().get(0));
    assertEquals(NEW_UMKM_SALES_CATEGORY,
        productDomainEventModel.getProductSalesCategoryMapping().getNewUmkmSalesCategoryCodes().get(0));
    assertTrue(productDomainEventModel.isBrandChanged());
    assertNull(productDomainEventModel.getPristineCategory());
  }

  @Test
  public void publishBrandCreatedTest() {
    BrandDomainEventModel brandDomainEventModel = this.domainEventPublisherServiceBean.publishBrandCreated(brandWip);
    assertEquals(BRAND_REQUEST_CODE, brandDomainEventModel.getBrandRequestCode());
    assertEquals(BRAND_NAME, brandDomainEventModel.getBrandName());
    assertEquals(BUSINESS_PARTNER_CODE, brandDomainEventModel.getBusinessPartnerCode());
    assertTrue(brandDomainEventModel.isProtectedBrand());
  }

  @Test
  public void publishMasterAttributeInfoEventTest(){
    AttributeDomainEventModel attributeDomainEventModel1 =
        this.domainEventPublisherServiceBean.publishMasterAttributeInfoEvent(attributeDomainEventModel);
    assertEquals(ATTRIBUTE_CODE, attributeDomainEventModel1.getAttributeCode());
  }

  @Test
  public void publishProductScoreUpdateTest() {
    ProductScoreUpdateDomainEventModel productScoreUpdateDomainEventModel =
        this.domainEventPublisherServiceBean.publishProductScoreUpdate(Arrays.asList(PRODUCT_CODE));
    assertEquals(1, productScoreUpdateDomainEventModel.getProductCodes().size());
    assertEquals(PRODUCT_CODE, productScoreUpdateDomainEventModel.getProductCodes().get(0));
  }

  @Test
  public void publishProductPristineCategoryTest() throws Exception {
    Product product = generateProduct();
    product.setReviewPending(true);
    product.setCreatedMerchant(SELLER_CODE);
    ProductDomainEventModel productDomainEventModel =
        this.domainEventPublisherServiceBean.publishProductForEdit(product, null, true, false,
          true, true, true);
    assertTrue(productDomainEventModel.isReviewPending());
    assertNull(productDomainEventModel.getProductSalesCategoryMapping());
    assertTrue(productDomainEventModel.isBrandChanged());
    assertNull(productDomainEventModel.getPristineCategory());
    assertTrue(productDomainEventModel.getUpdatedFields().contains(UpdatedFields.BRAND_UPDATE.name()));
    assertEquals(SELLER_CODE, productDomainEventModel.getSellerCode());
    Assertions.assertFalse(productDomainEventModel.getUpdatedFields().contains(UpdatedFields.CATEGORY_UPDATE.name()));
  }

  @Test
  public void publishProductForMigratedProductsTest() throws Exception {
    Product product = generateProduct();
    product.setReviewPending(true);
    ProductDomainEventModel productDomainEventModel =
        this.domainEventPublisherServiceBean.publishProductForMigratedProducts(product, true);
    assertTrue(productDomainEventModel.isReviewPending());
    assertTrue(productDomainEventModel.isNewProduct());
    Assertions.assertFalse(productDomainEventModel.isScoreUpdated());
    assertTrue(productDomainEventModel.isSolrUpdateRequired());
    Assertions.assertFalse(productDomainEventModel.isBrandChanged());
    assertTrue(productDomainEventModel.isMigratedProduct());
  }

  @Test
  public void publishProductForMigratedProductsNewproductFalseTest() throws Exception {
    Product product = generateProduct();
    ProductDomainEventModel productDomainEventModel =
        this.domainEventPublisherServiceBean.publishProductForMigratedProducts(product, false);
    Assertions.assertFalse(productDomainEventModel.isNewProduct());
    Assertions.assertFalse(productDomainEventModel.isScoreUpdated());
    assertTrue(productDomainEventModel.isSolrUpdateRequired());
    Assertions.assertFalse(productDomainEventModel.isBrandChanged());
    assertTrue(productDomainEventModel.isMigratedProduct());
  }

  @Test
  public void publishIsItemUpdatePublishTest() throws Exception {
    Product product = generateProduct();
    ProductDomainEventModel productDomainEventModel =
        this.domainEventPublisherServiceBean.publishProductForEdit(product, new ProductSalesCategoryMapping(), false, false,
          false, true, true);
    Assertions.assertNotNull(productDomainEventModel.getProductSalesCategoryMapping());
    assertTrue(productDomainEventModel.isItemUpdatePublish());
    assertNull(productDomainEventModel.getPristineCategory());
  }

  @Test
  public void publishIsItemUpdatePublishEventTest() throws Exception {
    Product product = generateProduct();
    product.getProductItems().get(0).setSkuCode(SKU_CODE_1);
    ProductDomainEventModel productDomainEventModel =
        this.domainEventPublisherServiceBean.publishProductForEdit(new ProductPublishUpdateDTO(product, false, new HashSet<>(Arrays.asList(SKU_CODE_1))),
            new ProductSalesCategoryMapping(), false, false, false, true, true);
    Assertions.assertNotNull(productDomainEventModel.getProductSalesCategoryMapping());
    assertTrue(productDomainEventModel.isItemUpdatePublish());
    assertNull(productDomainEventModel.getPristineCategory());
    assertTrue(productDomainEventModel.getEventTypes().contains(
        ProductPublishEventType.PUBLISH_SPECIFIC_ITEM_DATA_CHANGE_EVENT.name()));
  }

  @Test
  public void publishIsItemUpdatePublishSolrUpdateTest() throws Exception {
    Product product = generateProduct();
    ProductDomainEventModel productDomainEventModel =
        this.domainEventPublisherServiceBean.publishProductForEdit(product, null, false, false,
          true, true, false);
    assertNull(productDomainEventModel.getProductSalesCategoryMapping());
    assertTrue(productDomainEventModel.isItemUpdatePublish());
    assertTrue(productDomainEventModel.isSolrUpdateRequired());
    assertNull(productDomainEventModel.getPristineCategory());
  }

  @Test
  public void publishVatApplicableUpdateEventTest() {
    VatUpdateDomainEventModel vatUpdateDomainEventModel =
        domainEventPublisherServiceBean.publishVatApplicableUpdateEvent(ITEM_CODE, true);
    assertEquals(ITEM_CODE, vatUpdateDomainEventModel.getItemCode());
    assertTrue(vatUpdateDomainEventModel.isVatApplicable());
  }

  @Test
  public void publishVatApplicableExternalHistoryEventTest() {
    VatUpdateHistoryDomainEventModel vatUpdateHistoryDomainEventModel = domainEventPublisherServiceBean
        .publishVatApplicableExternalHistoryEvent(REQUEST_ID, DEFAULT_STORE_ID, ITEM_ID, ITEM_CODE, ITEM_NAME, UPDATED_BY,
            Boolean.FALSE.toString(), Boolean.TRUE.toString());
    assertEquals(REQUEST_ID, vatUpdateHistoryDomainEventModel.getRequestId());
    assertEquals(DEFAULT_STORE_ID, vatUpdateHistoryDomainEventModel.getStoreId());
    assertEquals(ITEM_ID, vatUpdateHistoryDomainEventModel.getProductItemId());
    assertEquals(ITEM_CODE, vatUpdateHistoryDomainEventModel.getItemCode());
    assertEquals(ITEM_NAME, vatUpdateHistoryDomainEventModel.getItemName());
    assertEquals(UPDATED_BY, vatUpdateHistoryDomainEventModel.getUpdatedBy());
    assertEquals(Boolean.FALSE.toString(), vatUpdateHistoryDomainEventModel.getOldValue());
    assertEquals(Boolean.TRUE.toString(), vatUpdateHistoryDomainEventModel.getNewValue());
  }

  @Test
  public void publishProductAttributeExtractionBackfillingEventTest() {
    ProductAttributeExtractionModel productAttributeExtractionModel = domainEventPublisherServiceBean
        .publishProductAttributeExtractionBackfillingEvent(DEFAULT_STORE_ID, UPDATED_BY, PRODUCT_CODE,
            NEW_SALES_CATEGORY, NEW_SALES_CATEGORY, ExtractionType.TEXT.name());
    assertEquals(DEFAULT_STORE_ID, productAttributeExtractionModel.getStoreId());
    assertEquals(UPDATED_BY, productAttributeExtractionModel.getUsername());
    assertEquals(PRODUCT_CODE, productAttributeExtractionModel.getProductCode());
    assertEquals(NEW_SALES_CATEGORY, productAttributeExtractionModel.getCnCategoryCode());
    assertEquals(NEW_SALES_CATEGORY, productAttributeExtractionModel.getCnCategoryName());
    assertEquals(ExtractionType.TEXT.name(), productAttributeExtractionModel.getExtractionType());
  }

  @Test
  public void publishProductForMasterDataMigrationTest() throws Exception {
    ProductDomainEventModel productDomainEventModel =
        domainEventPublisherServiceBean.publishProductForMasterDataMigration(generateProduct(),
          StringUtils.EMPTY);
    Assertions.assertFalse(productDomainEventModel.isNewProduct());
  }

  @Test
  public void publishBrandAuthHistoryEventTest() {
    BrandAuthDomainEventModel brandAuthDomainEventModel = domainEventPublisherServiceBean
      .publishBrandAuthHistoryEvent(BRAND_CODE, SELLER_CODE, brandAuthorisationHistory);
    Assertions.assertNotNull(brandAuthDomainEventModel);
  }

  @Test
  public void publishProductToBeDeletedTest() {
    domainEventPublisherServiceBean.publishProductToBeDeleted(DEFAULT_STORE_ID, Arrays.asList(PRODUCT_CODE));
    verify(kafkaProducer).send(eq(DomainEventName.DELETE_REJECTED_PRODUCT), eq(PRODUCT_CODE),
        productCodeDomainEventModelArgumentCaptor.capture());
    assertEquals(DEFAULT_STORE_ID, productCodeDomainEventModelArgumentCaptor.getValue().getStoreId());
    assertEquals(PRODUCT_CODE, productCodeDomainEventModelArgumentCaptor.getValue().getProductCode());
  }

  @Test
  public void publishImagePathUpdateEvent() {
    domainEventPublisherServiceBean.publishImagePathUpdateEvent(DEFAULT_STORE_ID, PRODUCT_CODE,
        ImmutableSet.of(Pair.of(IMAGE_PATH_1, IMAGE_PATH_2)));
    Mockito.verify(kafkaProducer)
        .send(eq(DomainEventName.UPDATE_PRODUCT_ITEM_IMAGE_PATH), eq(PRODUCT_CODE),
            imagePathUpdateDomainEventModel.capture());
    assertEquals(IMAGE_PATH_1,
        imagePathUpdateDomainEventModel.getValue().getImageUpdatedPath().iterator().next().getOldPath());
    assertEquals(IMAGE_PATH_2,
        imagePathUpdateDomainEventModel.getValue().getImageUpdatedPath().iterator().next().getNewPath());
  }

  @Test
  public void publishTerminatedSellerSkuCleanupStatusEventTest() {
    when(kafkaTopicProperties.getTerminatedSellerSkuCleanupStatus()).thenReturn(EVENT_NAME);
    domainEventPublisherServiceBean.publishTerminatedSellerSkuCleanupStatusEvent(
        terminatedSellerSkuCleanupStatusEventModel);
    verify(kafkaTopicProperties, times(2)).getTerminatedSellerSkuCleanupStatus();
    verify(kafkaProducer).send(EVENT_NAME, PRODUCT_CODE, terminatedSellerSkuCleanupStatusEventModel);
  }

  @Test
  public void publishTerminatedSellerSkuImageCleanupEventTest() {
    when(kafkaTopicProperties.getTerminatedSellerSkuImageCleanup()).thenReturn(EVENT_NAME);
    domainEventPublisherServiceBean.publishTerminatedSellerSkuImageCleanupEvent(
        terminatedSellerSkuImageCleanupEventModel);
    verify(kafkaTopicProperties, times(2)).getTerminatedSellerSkuImageCleanup();
    verify(kafkaProducer).send(EVENT_NAME, PRODUCT_CODE, terminatedSellerSkuImageCleanupEventModel);
  }

  @Test
  public void publishRestrictedKeywordHistoryTest() {
    restrictedKeywordHistoryEventModel = new RestrictedKeywordHistoryEventModel();
    restrictedKeywordHistoryEventModel.setKeywordId(ID);
    when(kafkaTopicProperties.getRestrictedKeywordHistoryEvent()).thenReturn(EVENT_NAME);
    domainEventPublisherServiceBean.publishRestrictedKeywordHistory(
      Collections.singletonList(restrictedKeywordHistoryEventModel));
    verify(kafkaTopicProperties, times(2)).getRestrictedKeywordHistoryEvent();
    verify(kafkaProducer).send(EVENT_NAME, ID, restrictedKeywordHistoryEventModel);
  }

  @Test
  public void publishCategoryUpdateHistoryTest() {
    categoryHistoryEventModels = new CategoryHistoryEventModel();
    categoryHistoryEventModels.setCategoryCode(ID);
    when(kafkaTopicProperties.getCategoryUpdateHistoryEvent()).thenReturn(EVENT_NAME);
    domainEventPublisherServiceBean.publishCategoryUpdateHistory(
      Collections.singletonList(categoryHistoryEventModels));
    verify(kafkaTopicProperties, times(2)).getCategoryUpdateHistoryEvent();
    verify(kafkaProducer).send(EVENT_NAME, ID, categoryHistoryEventModels);
  }

  @Test
  public void publishDeletedProductTest() {
    domainEventPublisherServiceBean.publishDeletedProduct(DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(kafkaProducer).send(eq(DomainEventName.DELETE_REJECTED_MERCHANT_PRODUCT), eq(PRODUCT_CODE), Mockito.any());
  }

  @Test
  void publishBrandHistoryTest() {
    BrandHistoryEventModel brandHistoryEventModel = new BrandHistoryEventModel();
    brandHistoryEventModel.setBrandRequestCode(ID);
    when(kafkaTopicProperties.getBrandHistoryEvent()).thenReturn(EVENT_NAME);
    domainEventPublisherServiceBean.publishBrandHistory(brandHistoryEventModel);
    verify(kafkaTopicProperties, times(2)).getBrandHistoryEvent();
    verify(kafkaProducer).send(EVENT_NAME, ID, brandHistoryEventModel);
  }

  @Test
  public void publishPBPAttributeMigrationEvent_Defining_Attribute_Test() {
    when(kafkaTopicProperties.getPbpAttributeMigrationEvent()).thenReturn(EVENT_NAME);
    PBPProductAttributeBackFillingEventModel.builder().attributeId(ID).attributeCode(ATTRIBUTE_CODE)
        .attributeName(ATTRIBUTE_NAME).attributeValue("value").productCode(PRODUCT_CODE)
        .skuValue(false);
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    Attribute attribute = new Attribute();
    attribute.setId(ID);
    attribute.setName(ATTRIBUTE_NAME);
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    ProductAttribute productAttribute = new ProductAttribute();
    AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
    allowedAttributeValue.setValue("value");
    productAttribute.setAttribute(attribute);
    productAttributeValue.setProductAttribute(productAttribute);
    productAttributeValue.setAllowedAttributeValue(allowedAttributeValue);
    domainEventPublisherServiceBean.publishPBPAttributeMigrationEvent(productAttributeValue,
        PRODUCT_CODE);
    verify(kafkaTopicProperties).getPbpAttributeMigrationEvent();
    verify(kafkaProducer).send(EVENT_NAME,
        PBPProductAttributeBackFillingEventModel.builder().attributeId(ID)
            .attributeCode(ATTRIBUTE_CODE).attributeName(ATTRIBUTE_NAME).attributeValue("value")
            .productCode(PRODUCT_CODE).skuValue(false).build());
  }

  @Test
  public void publishPBPAttributeMigrationEvent_PreDefined_Attribute_Test() {
    when(kafkaTopicProperties.getPbpAttributeMigrationEvent()).thenReturn(EVENT_NAME);
    PBPProductAttributeBackFillingEventModel.builder().attributeId(ID).attributeCode(ATTRIBUTE_CODE)
        .attributeName(ATTRIBUTE_NAME).attributeValue("value").productCode(PRODUCT_CODE)
        .skuValue(false);
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    Attribute attribute = new Attribute();
    attribute.setId(ID);
    attribute.setName(ATTRIBUTE_NAME);
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    ProductAttribute productAttribute = new ProductAttribute();
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setValue("value");
    productAttribute.setAttribute(attribute);
    productAttributeValue.setProductAttribute(productAttribute);
    productAttributeValue.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
    domainEventPublisherServiceBean.publishPBPAttributeMigrationEvent(productAttributeValue,
        PRODUCT_CODE);
    verify(kafkaTopicProperties).getPbpAttributeMigrationEvent();
    verify(kafkaProducer).send(EVENT_NAME,
        PBPProductAttributeBackFillingEventModel.builder().attributeId(ID)
            .attributeCode(ATTRIBUTE_CODE).attributeName(ATTRIBUTE_NAME).attributeValue("value")
            .productCode(PRODUCT_CODE).skuValue(false).build());
  }

  @Test
  public void publishPBPAttributeMigrationEvent_PreDefined_AttributeNull_Test_productAttributeMigrationPredefinedAttributeValueFilteringEnabled() {
    ReflectionTestUtils.setField(domainEventPublisherServiceBean,
        "productAttributeMigrationPredefinedAttributeValueFilteringEnabled", true);
    PBPProductAttributeBackFillingEventModel.builder().attributeId(ID).attributeCode(ATTRIBUTE_CODE)
        .attributeName(ATTRIBUTE_NAME).attributeValue("value").productCode(PRODUCT_CODE)
        .skuValue(false);
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    Attribute attribute = new Attribute();
    attribute.setId(ID);
    attribute.setName(ATTRIBUTE_NAME);
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttribute(attribute);
    productAttributeValue.setProductAttribute(productAttribute);
    productAttributeValue.setPredefinedAllowedAttributeValue(null);
    domainEventPublisherServiceBean.publishPBPAttributeMigrationEvent(productAttributeValue,
        PRODUCT_CODE);
    verifyNoInteractions(kafkaProducer);
  }

  @Test
  public void publishPBPAttributeMigrationEvent_PreDefined_Attribute_Test_productAttributeMigrationPredefinedAttributeValueFilteringEnabledTrue() {
    ReflectionTestUtils.setField(domainEventPublisherServiceBean,
        "productAttributeMigrationPredefinedAttributeValueFilteringEnabled", true);
    when(kafkaTopicProperties.getPbpAttributeMigrationEvent()).thenReturn(EVENT_NAME);
    PBPProductAttributeBackFillingEventModel.builder().attributeId(ID).attributeCode(ATTRIBUTE_CODE)
        .attributeName(ATTRIBUTE_NAME).attributeValue("value").productCode(PRODUCT_CODE)
        .skuValue(false);
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    Attribute attribute = new Attribute();
    attribute.setId(ID);
    attribute.setName(ATTRIBUTE_NAME);
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    ProductAttribute productAttribute = new ProductAttribute();
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setValue("value");
    productAttribute.setAttribute(attribute);
    productAttributeValue.setProductAttribute(productAttribute);
    productAttributeValue.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
    domainEventPublisherServiceBean.publishPBPAttributeMigrationEvent(productAttributeValue,
        PRODUCT_CODE);
    verify(kafkaTopicProperties).getPbpAttributeMigrationEvent();
    verify(kafkaProducer).send(EVENT_NAME,
        PBPProductAttributeBackFillingEventModel.builder().attributeId(ID)
            .attributeCode(ATTRIBUTE_CODE).attributeName(ATTRIBUTE_NAME).attributeValue("value")
            .productCode(PRODUCT_CODE).skuValue(false).build());
  }

  @Test
  public void publishPBPAttributeMigrationEvent_Descriptive_Attribute_Test() {
    when(kafkaTopicProperties.getPbpAttributeMigrationEvent()).thenReturn(EVENT_NAME);
    PBPProductAttributeBackFillingEventModel.builder().attributeId(ID).attributeCode(ATTRIBUTE_CODE)
        .attributeName(ATTRIBUTE_NAME).attributeValue("value").productCode(PRODUCT_CODE)
        .skuValue(false);
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    Attribute attribute = new Attribute();
    attribute.setId(ID);
    attribute.setName(ATTRIBUTE_NAME);
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttribute(attribute);
    productAttributeValue.setProductAttribute(productAttribute);
    productAttributeValue.setDescriptiveAttributeValue("value");
    domainEventPublisherServiceBean.publishPBPAttributeMigrationEvent(productAttributeValue,
        PRODUCT_CODE);
    verify(kafkaTopicProperties).getPbpAttributeMigrationEvent();
    verify(kafkaProducer).send(EVENT_NAME,
        PBPProductAttributeBackFillingEventModel.builder().attributeId(ID)
            .attributeCode(ATTRIBUTE_CODE).attributeName(ATTRIBUTE_NAME).attributeValue("value")
            .productCode(PRODUCT_CODE).skuValue(false).build());
  }

  @Test
  public void publishPBPAttributeMigrationEvent_No_Attribute_Type_Test() {
    when(kafkaTopicProperties.getPbpAttributeMigrationEvent()).thenReturn(EVENT_NAME);
    PBPProductAttributeBackFillingEventModel.builder().attributeId(ID).attributeCode(ATTRIBUTE_CODE)
        .attributeName(ATTRIBUTE_NAME).attributeValue("value").productCode(PRODUCT_CODE)
        .skuValue(false);
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    Attribute attribute = new Attribute();
    attribute.setId(ID);
    attribute.setName(ATTRIBUTE_NAME);
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setAttributeType(AttributeType.ALL);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttribute(attribute);
    productAttributeValue.setProductAttribute(productAttribute);
    productAttributeValue.setDescriptiveAttributeValue("value");
    domainEventPublisherServiceBean.publishPBPAttributeMigrationEvent(productAttributeValue,
        PRODUCT_CODE);
    verify(kafkaTopicProperties).getPbpAttributeMigrationEvent();
    verify(kafkaProducer).send(EVENT_NAME,
        PBPProductAttributeBackFillingEventModel.builder().attributeId(ID)
            .attributeCode(ATTRIBUTE_CODE).attributeName(ATTRIBUTE_NAME).attributeValue(null)
            .productCode(PRODUCT_CODE).skuValue(false).build());
  }

  @Test
  public void publishVendorEventTest() {
    VendorPublishEventModel vendorPublishEventModel = new VendorPublishEventModel();
    vendorPublishEventModel.setProductCode(PRODUCT_CODE);
    when(kafkaTopicProperties.getPublishVendorEvent()).thenReturn(EVENT_NAME);
    domainEventPublisherServiceBean.publishVendorEvent(vendorPublishEventModel);
    verify(kafkaTopicProperties, times(2)).getPublishVendorEvent();
    verify(kafkaProducer).send(EVENT_NAME, PRODUCT_CODE, vendorPublishEventModel);
  }

  @Test
  public void publishInternalHistoryEventTest() {
    InternalProductHistoryEventModel internalProductHistoryEventModel = new InternalProductHistoryEventModel();
    internalProductHistoryEventModel.setProductCode(PRODUCT_CODE);
    when(kafkaTopicProperties.getProductInternalHistoryEvent()).thenReturn(EVENT_NAME);
    domainEventPublisherServiceBean.publishInternalHistoryEvent(internalProductHistoryEventModel);
    verify(kafkaTopicProperties, times(2)).getProductInternalHistoryEvent();
    verify(kafkaProducer).send(EVENT_NAME, PRODUCT_CODE, internalProductHistoryEventModel);
  }

  @Test
  public void publishProductFailure_Test() {
    ProductCreationFailureDomainEventModel productCreationFailureDomainEventModel = new ProductCreationFailureDomainEventModel();
    productCreationFailureDomainEventModel.setProductCode(PRODUCT_CODE);
    productCreationFailureDomainEventModel.setCreatedMerchant(SELLER_CODE);
    this.domainEventPublisherServiceBean.publishProductFailure(productCreationFailureDomainEventModel);
    assertEquals(SELLER_CODE, productCreationFailureDomainEventModel.getSellerCode());
  }
}
