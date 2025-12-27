package com.gdn.x.productcategorybase.service.impl;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.Spy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.messaging.support.GenericMessage;

import com.gdn.x.productcategorybase.AggregateCommandDesc;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.config.KafkaPublisher;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.AggregateImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AggregateProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AggregateProductCategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AggregateProductItemDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.properties.AggregatorProperties;
import com.gdn.x.productcategorybase.service.AsyncProcessor;
import com.gdn.x.productcategorybase.service.ImageService;
import com.gdn.x.productcategorybase.service.ProductAttributeService;
import com.gdn.x.productcategorybase.service.ProductCategoryService;
import com.gdn.x.productcategorybase.service.ProductItemService;
import com.gdn.x.productcategorybase.service.ProductService;
import com.gdn.x.productcategorybase.service.ProductServiceWrapper;
import com.gdn.x.productcategorybase.service.brand.BrandService;

public class AggregateServiceBeanTest {

  private static final String JSON = "{}";
  private static final String DOMAIN_EVENT_NAME = DomainEventName.PRODUCT_PUBLISH_ALL;
  private static final String STORE_ID = "10001";
  private static final int START_PAGE = 0;
  private static final int PAGE_SIZE = 10;
  private static final String HEADER_NAME = "kafka_topic";
  private static final String PRODUCT_CODE = "product-code";
  private static final String BRAND_CODE = "dummy-brand-code";
  private static final List<String> PRODUCT_CODES = Collections.singletonList(PRODUCT_CODE);
  private static final Page<String> PRODUCT_CODE_PAGE = new PageImpl<>(PRODUCT_CODES);
  private Product PRODUCT = new Product();
  private Product PRODUCT_EMPTY = new Product();
  private Page<Product> PRODUCT_PAGE = new PageImpl<>(Collections.singletonList(PRODUCT));
  private ProductCategory PRODUCT_CATEGORY = new ProductCategory();
  private Page<ProductCategory> PRODUCT_CATEGORY_PAGE = new PageImpl<>(Collections.singletonList(PRODUCT_CATEGORY));
  private ProductAttribute PRODUCT_ATTRIBUTE = new ProductAttribute();
  private Page<ProductAttribute> PRODUCT_ATTRIBUTE_PAGE = new PageImpl<>(Collections.singletonList(PRODUCT_ATTRIBUTE));
  private ProductImage PRODUCT_IMAGE = new ProductImage();
  private Page<ProductImage> PRODUCT_IMAGE_PAGE = new PageImpl<>(Collections.singletonList(PRODUCT_IMAGE));
  private ProductItem PRODUCT_ITEM = new ProductItem();
  private Page<ProductItem> PRODUCT_ITEM_PAGE = new PageImpl<>(Collections.singletonList(PRODUCT_ITEM));

  @Captor
  private ArgumentCaptor<ProductDomainEventModel> captorProductEvent;

  @Captor
  private ArgumentCaptor<AggregateProductCategoryDomainEventModel> captorProductCategoryEvent;

  @Captor
  private ArgumentCaptor<AggregateProductAttributeDomainEventModel> captorProductAttributeEvent;

  @Captor
  private ArgumentCaptor<AggregateImageDomainEventModel> captorImageEvent;

  @Captor
  private ArgumentCaptor<AggregateProductItemDomainEventModel> captorProductItemEvent;

  @Captor
  private ArgumentCaptor<GenericMessage> captorGenericMessage;

  @Mock
  private AsyncProcessor asyncProcessor;

  @Mock
  private ProductService productService;

  @Mock
  private ProductCategoryService productCategoryService;

  @Mock
  private ProductAttributeService productAttributeService;

  @Mock
  private ImageService imageService;

  @Mock
  private ProductItemService productItemService;

  @Mock
  private BrandService brandServiceBean;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @Mock
  private KafkaPublisher kafkaPublisher;

  @Spy
  private AggregatorProperties aggregatorProperties = new AggregatorProperties(1,1);

  @InjectMocks
  private AggregateServiceBean aggregateServiceBean;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);

    aggregateServiceBean = new AggregateServiceBean(aggregatorProperties, asyncProcessor, productService, productServiceWrapper, kafkaPublisher);

    Product product = new Product();
    product.setStoreId(STORE_ID);
    product.setActivated(true);
    product.setViewable(true);
    product.setMarkForDelete(false);
    PRODUCT = new Product();
    PRODUCT.setProductCode(PRODUCT_CODE);
    PRODUCT_CATEGORY = new ProductCategory();
    PRODUCT_CATEGORY.setProduct(product);
    PRODUCT_CATEGORY_PAGE = new PageImpl<>(Collections.singletonList(PRODUCT_CATEGORY));

    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode(BRAND_CODE);

    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
    productAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);

    PRODUCT_ATTRIBUTE = new ProductAttribute();
    PRODUCT_ATTRIBUTE.setProduct(product);
    PRODUCT_ATTRIBUTE.getProductAttributeValues().add(productAttributeValue);
    PRODUCT_ATTRIBUTE.setProductAttributeName("Brand");

    PRODUCT_ATTRIBUTE_PAGE = new PageImpl<>(Collections.singletonList(PRODUCT_ATTRIBUTE));
    PRODUCT_IMAGE = new ProductImage();
    PRODUCT_IMAGE.setProduct(product);
    PRODUCT_IMAGE_PAGE = new PageImpl<>(Collections.singletonList(PRODUCT_IMAGE));
    PRODUCT_ITEM = new ProductItem();
    PRODUCT_ITEM.setProduct(product);
    PRODUCT_ITEM_PAGE = new PageImpl<>(Collections.singletonList(PRODUCT_ITEM));
    PRODUCT_EMPTY = new Product();
    PRODUCT_EMPTY.setProductCode(PRODUCT_CODE);
    PRODUCT.setProductCategories(Collections.singletonList(PRODUCT_CATEGORY));
    PRODUCT.setProductAttributes(Collections.singletonList(PRODUCT_ATTRIBUTE));
    PRODUCT.setProductImages(Collections.singletonList(PRODUCT_IMAGE));
    PRODUCT.setProductItems(Collections.singletonList(PRODUCT_ITEM));
    PRODUCT_EMPTY.setProductCategories(Collections.emptyList());
    PRODUCT_EMPTY.setProductAttributes(Collections.emptyList());
    PRODUCT_EMPTY.setProductImages(Collections.emptyList());
    PRODUCT_EMPTY.setProductItems(Collections.emptyList());
  }

  @Test
  public void testPublishAllProducts() throws Exception {
    mockPublishAllProducts(false);
    aggregateServiceBean.publishPageOfProducts(STORE_ID,START_PAGE);
    verifyPublishAllProducts();
  }

  @Test
  public void testPublishAllProducts_Empty() throws Exception {
    mockPublishAllProducts(true);
    aggregateServiceBean.publishPageOfProducts(STORE_ID,START_PAGE);
    verifyPublishAllProducts();
  }

  @Test
  public void testPublishAllProductsFailed() throws Exception {
    mockPublishAllProductsFailed();
    aggregateServiceBean.publishPageOfProducts(STORE_ID,START_PAGE);
    verifyPublishAllProducts();
  }

  @Test
  public void testPublishAllProductsError() throws Exception {
    mockPublishAllProductsError();
    aggregateServiceBean.publishPageOfProducts(STORE_ID,START_PAGE);
    verifyPublishAllProductsError();
  }

  @Test
  public void testPublishAllProductCategories() throws Exception {
    mockPublishAllProductCategories(false);
    aggregateServiceBean.publishPageOfProductCategories(STORE_ID,START_PAGE);
    verifyPublishAllProductCategories(false);
  }

  @Test
  public void testPublishAllProductCategories_Empty() throws Exception {
    mockPublishAllProductCategories(true);
    aggregateServiceBean.publishPageOfProductCategories(STORE_ID,START_PAGE);
    verifyPublishAllProductCategories(true);
  }

  @Test
  public void testPublishAllProductCategoriesFailed() throws Exception {
    mockPublishAllProductCategoriesFailed();
    aggregateServiceBean.publishPageOfProductCategories(STORE_ID,START_PAGE);
    verifyPublishAllProductCategories(false);
  }

  @Test
  public void testPublishAllProductAttributes() throws Exception {
    mockPublishAllProductAttributes(false);
    aggregateServiceBean.publishPageOfProductAttributes(STORE_ID,START_PAGE);
    verifyPublishAllProductAttributes(false);
  }

  @Test
  public void testPublishAllProductAttributes_Empty() throws Exception {
    mockPublishAllProductAttributes(true);
    aggregateServiceBean.publishPageOfProductAttributes(STORE_ID,START_PAGE);
    verifyPublishAllProductAttributes(true);
  }

  @Test
  public void testPublishAllProductAttributesFailed() throws Exception {
    mockPublishAllProductAttributesFailed();
    aggregateServiceBean.publishPageOfProductAttributes(STORE_ID,START_PAGE);
    verifyPublishAllProductAttributes(false);
  }

  @Test
  public void testPublishAllProductImages() throws Exception {
    mockPublishAllProductImages(false);
    aggregateServiceBean.publishPageOfImages(STORE_ID,START_PAGE);
    verifyPublishAllProductImages(false);
  }

  @Test
  public void testPublishAllProductImages_Empty() throws Exception {
    mockPublishAllProductImages(true);
    aggregateServiceBean.publishPageOfImages(STORE_ID,START_PAGE);
    verifyPublishAllProductImages(true);
  }

  @Test
  public void testPublishAllProductImagesFailed() throws Exception {
    mockPublishAllProductImagesFailed();
    aggregateServiceBean.publishPageOfImages(STORE_ID,START_PAGE);
    verifyPublishAllProductImages(false);
  }

  @Test
  public void testPublishAllProductItems() throws Exception {
    mockPublishAllProductItems(false);
    aggregateServiceBean.publishPageOfProductItems(STORE_ID, START_PAGE);
    verifyPublishAllProductItems(false);
  }

  @Test
  public void testPublishAllProductItems_Empty() throws Exception {
    mockPublishAllProductItems(true);
    aggregateServiceBean.publishPageOfProductItems(STORE_ID,START_PAGE);
    verifyPublishAllProductItems(true);
  }

  @Test
  public void testPublishAllProductItemsFailed() throws Exception {
    mockPublishAllProductItemsFailed();
    aggregateServiceBean.publishPageOfProductItems(STORE_ID,START_PAGE);
    verifyPublishAllProductItems(false);
  }

  @Test
  public void testSend() throws Exception {
    aggregateServiceBean.send(JSON ,DOMAIN_EVENT_NAME);
  }

  private void mockPublishAllProducts(boolean isEmpty) throws Exception {
    when(productService.getActiveProductCodes(anyString(), Mockito.any(PageRequest.class)))
        .thenReturn(PRODUCT_CODE_PAGE);
    when(productServiceWrapper.getCompleteProductDetailByProductCodeInAllProducts(eq(STORE_ID), eq(PRODUCT_CODE)))
        .thenReturn(isEmpty ? PRODUCT_EMPTY : PRODUCT);
  }

  private void mockPublishAllProductsFailed() throws Exception {
    when(productService.getActiveProductCodes(anyString(), Mockito.any(PageRequest.class)))
        .thenReturn(PRODUCT_CODE_PAGE);
    when(productServiceWrapper.getCompleteProductDetailByProductCodeInAllProducts(eq(STORE_ID), eq(PRODUCT_CODE)))
        .thenReturn(PRODUCT);
  }

  private void mockPublishAllProductsError() throws Exception {
    when(productService.getActiveProductCodes(anyString(), Mockito.any(PageRequest.class)))
        .thenReturn(PRODUCT_CODE_PAGE);
    when(productServiceWrapper.getCompleteProductDetailByProductCodeInAllProducts(eq(STORE_ID), eq(PRODUCT_CODE)))
        .thenReturn(PRODUCT);
    doThrow(RuntimeException.class)
        .when(productServiceWrapper).getCompleteProductDetailByProductCodeInAllProducts(STORE_ID,PRODUCT_CODE);
  }

  private void verifyPublishAllProducts() throws Exception{
    verify(productService).getActiveProductCodes(anyString(), Mockito.any(PageRequest.class));
    verify(productServiceWrapper).getCompleteProductDetailByProductCodeInAllProducts(eq(STORE_ID), eq(PRODUCT_CODE));
    verify(asyncProcessor)
        .submitWithBackoff(eq(AggregateCommandDesc.PRODUCT), any(Runnable.class));
  }

  private void verifyPublishAllProductsError() throws Exception{
    verify(productService).getActiveProductCodes(anyString(), Mockito.any(PageRequest.class));
    verify(productServiceWrapper).getCompleteProductDetailByProductCodeInAllProducts(eq(STORE_ID), eq(PRODUCT_CODE));
  }

  private void mockPublishAllProductCategories(boolean isEmpty) throws Exception {
    when(productService.getActiveProductCodes(anyString(), Mockito.any(PageRequest.class)))
        .thenReturn(PRODUCT_CODE_PAGE);
    when(productServiceWrapper.getCompleteProductDetailByProductCodeInAllProducts(eq(STORE_ID), eq(PRODUCT_CODE)))
        .thenReturn(isEmpty ? PRODUCT_EMPTY : PRODUCT);
  }

  private void mockPublishAllProductCategoriesFailed() throws Exception {
    when(productService.getActiveProductCodes(anyString(), Mockito.any(PageRequest.class)))
        .thenReturn(PRODUCT_CODE_PAGE);
    when(productServiceWrapper.getCompleteProductDetailByProductCodeInAllProducts(eq(STORE_ID), eq(PRODUCT_CODE)))
        .thenReturn(PRODUCT);
  }

  private void verifyPublishAllProductCategories(boolean isEmpty) throws Exception{
    verify(productService).getActiveProductCodes(anyString(), Mockito.any(PageRequest.class));
    verify(productServiceWrapper).getCompleteProductDetailByProductCodeInAllProducts(eq(STORE_ID), eq(PRODUCT_CODE));
    if(!isEmpty) {
      verify(asyncProcessor)
          .submitWithBackoff(eq(AggregateCommandDesc.PRODUCT_CATEGORY), any(Runnable.class));
    }
  }

  private void mockPublishAllProductAttributes(boolean isEmpty) throws Exception {
    when(productService.getActiveProductCodes(anyString(), Mockito.any(PageRequest.class)))
        .thenReturn(PRODUCT_CODE_PAGE);
    when(productServiceWrapper.getCompleteProductDetailByProductCodeInAllProducts(eq(STORE_ID), eq(PRODUCT_CODE)))
        .thenReturn(isEmpty ? PRODUCT_EMPTY : PRODUCT);
  }

  private void mockPublishAllProductAttributesFailed() throws Exception {
    when(productService.getActiveProductCodes(anyString(), Mockito.any(PageRequest.class)))
        .thenReturn(PRODUCT_CODE_PAGE);
    when(productServiceWrapper.getCompleteProductDetailByProductCodeInAllProducts(eq(STORE_ID), eq(PRODUCT_CODE)))
        .thenReturn(PRODUCT);
  }

  private void verifyPublishAllProductAttributes(boolean isEmpty) throws Exception{
    verify(productService).getActiveProductCodes(anyString(), Mockito.any(PageRequest.class));
    verify(productServiceWrapper).getCompleteProductDetailByProductCodeInAllProducts(eq(STORE_ID), eq(PRODUCT_CODE));
    if(!isEmpty) {
      verify(asyncProcessor)
          .submitWithBackoff(eq(AggregateCommandDesc.PRODUCT_ATTRIBUTE), any(Runnable.class));
    }
  }

  private void mockPublishAllProductImages(boolean isEmpty) throws Exception {
    when(productService.getActiveProductCodes(anyString(), Mockito.any(PageRequest.class)))
        .thenReturn(PRODUCT_CODE_PAGE);
    when(productServiceWrapper.getCompleteProductDetailByProductCodeInAllProducts(eq(STORE_ID), eq(PRODUCT_CODE)))
        .thenReturn(isEmpty ? PRODUCT_EMPTY : PRODUCT);
  }

  private void mockPublishAllProductImagesFailed() throws Exception {
    when(productService.getActiveProductCodes(anyString(), Mockito.any(PageRequest.class)))
        .thenReturn(PRODUCT_CODE_PAGE);
    when(productServiceWrapper.getCompleteProductDetailByProductCodeInAllProducts(eq(STORE_ID), eq(PRODUCT_CODE)))
        .thenReturn(PRODUCT);
  }

  private void verifyPublishAllProductImages(boolean isEmpty) throws Exception{
    verify(productService).getActiveProductCodes(anyString(), Mockito.any(PageRequest.class));
    verify(productServiceWrapper).getCompleteProductDetailByProductCodeInAllProducts(eq(STORE_ID), eq(PRODUCT_CODE));
    if(!isEmpty) {
      verify(asyncProcessor)
          .submitWithBackoff(eq(AggregateCommandDesc.IMAGE), any(Runnable.class));
    }
  }

  private void mockPublishAllProductItems(boolean isEmpty) throws Exception {
    when(productService.getActiveProductCodes(anyString(), Mockito.any(PageRequest.class)))
        .thenReturn(PRODUCT_CODE_PAGE);
    when(productServiceWrapper.getCompleteProductDetailByProductCodeInAllProducts(eq(STORE_ID), eq(PRODUCT_CODE)))
        .thenReturn(isEmpty ? PRODUCT_EMPTY : PRODUCT);
  }

  private void mockPublishAllProductItemsFailed() throws Exception {
    when(productService.getActiveProductCodes(anyString(), Mockito.any(PageRequest.class)))
        .thenReturn(PRODUCT_CODE_PAGE);
    when(productServiceWrapper.getCompleteProductDetailByProductCodeInAllProducts(eq(STORE_ID), eq(PRODUCT_CODE)))
        .thenReturn(PRODUCT);
  }

  private void verifyPublishAllProductItems(boolean isEmpty) throws Exception{
    verify(productService).getActiveProductCodes(anyString(), Mockito.any(PageRequest.class));
    verify(productServiceWrapper).getCompleteProductDetailByProductCodeInAllProducts(eq(STORE_ID), eq(PRODUCT_CODE));

    if(!isEmpty) {
      verify(asyncProcessor)
          .submitWithBackoff(eq(AggregateCommandDesc.PRODUCT_ITEM), any(Runnable.class));
    }
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.productCategoryService);
    verifyNoMoreInteractions(this.productAttributeService);
    verifyNoMoreInteractions(this.imageService);
    verifyNoMoreInteractions(this.productItemService);
    verifyNoMoreInteractions(this.brandServiceBean);
  }

}
