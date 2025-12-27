package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.entity.SystemParameter;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.ImageService;
import com.gdn.x.productcategorybase.service.ProductArchivalService;
import com.gdn.x.productcategorybase.service.ProductAttributeService;
import com.gdn.x.productcategorybase.service.ProductAttributeValueService;
import com.gdn.x.productcategorybase.service.ProductCategoryService;
import com.gdn.x.productcategorybase.service.ProductDeletionService;
import com.gdn.x.productcategorybase.service.ProductItemAttributeValueService;
import com.gdn.x.productcategorybase.service.ProductItemService;
import com.gdn.x.productcategorybase.service.ProductService;
import com.gdn.x.productcategorybase.service.SystemParameterService;
import com.google.common.collect.ImmutableSet;

public class ProductDeletionWrapperServiceTest {
  private static final String STORE_ID = "10001";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_ID = "productId";
  private static final String PRODUCT_ITEM_ID = "productItemId";
  private static final String PRODUCT_ATTRIBUTE_ID = "productAttributeId";

  @InjectMocks
  private ProductDeletionWrapperServiceImpl productDeletionWrapperService;

  @Mock
  private ProductService productService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private DomainEventPublisherService domainEventPublisherService;

  @Mock
  private ProductArchivalService productArchivalService;

  @Mock
  private ProductCategoryService productCategoryService;

  @Mock
  private ImageService imageService;

  @Mock
  private ProductItemService productItemService;

  @Mock
  private ProductAttributeService productAttributeService;

  @Mock
  private ProductAttributeValueService productAttributeValueService;

  @Mock
  private ProductItemAttributeValueService productItemAttributeValueService;

  @Mock
  private ProductDeletionService productDeletionService;

  private Product product;
  private ProductItem productItem;
  private ProductAttribute productAttribute;
  private ProductAttributeValue productAttributeValue;
  private ProductImage productImage;
  private ProductCategory productCategory;
  private ProductItemImage productItemImage;
  private ProductItemAttributeValue productItemAttributeValue;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);


    productCategory = new ProductCategory();
    productCategory.setProductId(PRODUCT_ID);

    productImage = new ProductImage();
    productImage.setProductId(PRODUCT_ID);

    productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setProductAttributeId(PRODUCT_ATTRIBUTE_ID);

    productAttribute = new ProductAttribute();
    productAttribute.setProductId(PRODUCT_ID);
    productAttribute.setId(PRODUCT_ATTRIBUTE_ID);

    productItemImage = new ProductItemImage();
    productItemImage.setProductItemId(PRODUCT_ITEM_ID);

    productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setProductItemId(PRODUCT_ITEM_ID);

    productItem = new ProductItem();
    productItem.setId(PRODUCT_ITEM_ID);

    product = new Product();
    product.setId(PRODUCT_ID);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(domainEventPublisherService, productService, systemParameterService,
        productCategoryService, imageService, productItemService, productAttributeService, productAttributeValueService,
        productItemAttributeValueService);
    Mockito.verifyNoMoreInteractions(productDeletionService);
  }

  @Test
  public void publishProductForDeletionTest() {
    Mockito.when(
            systemParameterService.findByStoreIdAndVariable(STORE_ID, Constants.BATCH_SIZE_TO_PUBLISH_REJECTED_PRODUCTS))
        .thenReturn(new SystemParameter(Constants.BATCH_SIZE_TO_PUBLISH_REJECTED_PRODUCTS, "1",
            Constants.BATCH_SIZE_TO_PUBLISH_REJECTED_PRODUCTS));
    Mockito.when(
            systemParameterService.findByStoreIdAndVariable(STORE_ID, Constants.DAYS_THRESHOLD_FOR_PRODUCT_DELETION))
        .thenReturn(new SystemParameter(Constants.DAYS_THRESHOLD_FOR_PRODUCT_DELETION, "1",
            Constants.DAYS_THRESHOLD_FOR_PRODUCT_DELETION));
    Mockito.when(productService.getListOfProductCodesEligibleForDeletion(Mockito.eq(STORE_ID), Mockito.any(Date.class),
        Mockito.eq(1))).thenReturn(Arrays.asList(PRODUCT_CODE));
    Mockito.doNothing().when(productService)
        .updatePickedForDeletionFlagByProductCode(STORE_ID, Arrays.asList(PRODUCT_CODE), true);
    Mockito.doNothing().when(domainEventPublisherService)
        .publishProductToBeDeleted(STORE_ID, Arrays.asList(PRODUCT_CODE));

    productDeletionWrapperService.publishProductForDeletion(STORE_ID);

    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.BATCH_SIZE_TO_PUBLISH_REJECTED_PRODUCTS);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.DAYS_THRESHOLD_FOR_PRODUCT_DELETION);
    Mockito.verify(productService)
        .getListOfProductCodesEligibleForDeletion(Mockito.eq(STORE_ID), Mockito.any(Date.class), Mockito.eq(1));
    Mockito.verify(productService)
        .updatePickedForDeletionFlagByProductCode(STORE_ID, Arrays.asList(PRODUCT_CODE), true);
    Mockito.verify(domainEventPublisherService).publishProductToBeDeleted(STORE_ID, Arrays.asList(PRODUCT_CODE));
  }

  @Test
  public void publishProductForDeletionEmptyProductListTest() {
    Mockito.when(
            systemParameterService.findByStoreIdAndVariable(STORE_ID, Constants.BATCH_SIZE_TO_PUBLISH_REJECTED_PRODUCTS))
        .thenReturn(new SystemParameter(Constants.BATCH_SIZE_TO_PUBLISH_REJECTED_PRODUCTS, "1",
            Constants.BATCH_SIZE_TO_PUBLISH_REJECTED_PRODUCTS));
    Mockito.when(
            systemParameterService.findByStoreIdAndVariable(STORE_ID, Constants.DAYS_THRESHOLD_FOR_PRODUCT_DELETION))
        .thenReturn(new SystemParameter(Constants.DAYS_THRESHOLD_FOR_PRODUCT_DELETION, "1",
            Constants.DAYS_THRESHOLD_FOR_PRODUCT_DELETION));
    Mockito.when(productService.getListOfProductCodesEligibleForDeletion(Mockito.eq(STORE_ID), Mockito.any(Date.class),
        Mockito.eq(1))).thenReturn(new ArrayList<>());

    productDeletionWrapperService.publishProductForDeletion(STORE_ID);

    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.BATCH_SIZE_TO_PUBLISH_REJECTED_PRODUCTS);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.DAYS_THRESHOLD_FOR_PRODUCT_DELETION);
    Mockito.verify(productService)
        .getListOfProductCodesEligibleForDeletion(Mockito.eq(STORE_ID), Mockito.any(Date.class), Mockito.eq(1));
  }

  @Test
  public void publishProductForDeletionBatchSize0Test() {
    Mockito.when(
            systemParameterService.findByStoreIdAndVariable(STORE_ID, Constants.BATCH_SIZE_TO_PUBLISH_REJECTED_PRODUCTS))
        .thenReturn(new SystemParameter(Constants.BATCH_SIZE_TO_PUBLISH_REJECTED_PRODUCTS, "0",
            Constants.BATCH_SIZE_TO_PUBLISH_REJECTED_PRODUCTS));

    productDeletionWrapperService.publishProductForDeletion(STORE_ID);

    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.BATCH_SIZE_TO_PUBLISH_REJECTED_PRODUCTS);
  }

  @Test
  public void archiveAndDeleteProductDataTest() throws Exception {
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    Mockito.when(productItemService.getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productItem));
    Mockito.when(imageService.getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productImage));
    Mockito.when(productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productCategory));
    Mockito.when(productAttributeService.getProductAttributesByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productAttribute));
    Mockito.when(productAttributeValueService.getProductAttributeValuesByStoreIdAndProductAttributeIds(STORE_ID,
        ImmutableSet.of(PRODUCT_ATTRIBUTE_ID))).thenReturn(Arrays.asList(productAttributeValue));
    Mockito.when(
        productItemAttributeValueService.getDetachedProductItemAttributeValuesByStoreIdAndProductItemIds(STORE_ID,
            Arrays.asList(PRODUCT_ITEM_ID))).thenReturn(Arrays.asList(productItemAttributeValue));
    Mockito.when(
            imageService.getDetachedProductItemImagesByStoreIdAndProductItemIds(STORE_ID, Arrays.asList(PRODUCT_ITEM_ID)))
        .thenReturn(Arrays.asList(productItemImage));
    Mockito.doNothing().when(productArchivalService)
        .copyProductDetailsToArchiveTablesAndHardDeleteProduct(Mockito.any(Product.class));
    Mockito.doNothing().when(domainEventPublisherService).publishDeletedProduct(STORE_ID, PRODUCT_CODE);

    productDeletionWrapperService.archiveAndDeleteProductData(STORE_ID, PRODUCT_CODE);

    Mockito.verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productAttributeService).getProductAttributesByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(productAttributeValueService)
        .getProductAttributeValuesByStoreIdAndProductAttributeIds(STORE_ID, ImmutableSet.of(PRODUCT_ATTRIBUTE_ID));
    Mockito.verify(productItemAttributeValueService)
        .getDetachedProductItemAttributeValuesByStoreIdAndProductItemIds(STORE_ID, Arrays.asList(PRODUCT_ITEM_ID));
    Mockito.verify(imageService)
        .getDetachedProductItemImagesByStoreIdAndProductItemIds(STORE_ID, Arrays.asList(PRODUCT_ITEM_ID));
    Mockito.verify(productArchivalService)
        .copyProductDetailsToArchiveTablesAndHardDeleteProduct(Mockito.any(Product.class));
    Mockito.verify(domainEventPublisherService).publishDeletedProduct(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void archiveAndDeleteProductDataNullAttributeValuesTest() throws Exception {
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    Mockito.when(productItemService.getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productItem));
    Mockito.when(imageService.getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productImage));
    Mockito.when(productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productCategory));
    Mockito.when(productAttributeService.getProductAttributesByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productAttribute));
    Mockito.when(productAttributeValueService.getProductAttributeValuesByStoreIdAndProductAttributeIds(STORE_ID,
        ImmutableSet.of(PRODUCT_ATTRIBUTE_ID))).thenReturn(null);
    Mockito.when(
        productItemAttributeValueService.getDetachedProductItemAttributeValuesByStoreIdAndProductItemIds(STORE_ID,
            Arrays.asList(PRODUCT_ITEM_ID))).thenReturn(null);
    Mockito.when(
            imageService.getDetachedProductItemImagesByStoreIdAndProductItemIds(STORE_ID, Arrays.asList(PRODUCT_ITEM_ID)))
        .thenReturn(null);
    Mockito.doNothing().when(productArchivalService)
        .copyProductDetailsToArchiveTablesAndHardDeleteProduct(Mockito.any(Product.class));
    Mockito.doNothing().when(domainEventPublisherService).publishDeletedProduct(STORE_ID, PRODUCT_CODE);

    productDeletionWrapperService.archiveAndDeleteProductData(STORE_ID, PRODUCT_CODE);

    Mockito.verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productAttributeService).getProductAttributesByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(productAttributeValueService)
        .getProductAttributeValuesByStoreIdAndProductAttributeIds(STORE_ID, ImmutableSet.of(PRODUCT_ATTRIBUTE_ID));
    Mockito.verify(productItemAttributeValueService)
        .getDetachedProductItemAttributeValuesByStoreIdAndProductItemIds(STORE_ID, Arrays.asList(PRODUCT_ITEM_ID));
    Mockito.verify(imageService)
        .getDetachedProductItemImagesByStoreIdAndProductItemIds(STORE_ID, Arrays.asList(PRODUCT_ITEM_ID));
    Mockito.verify(productArchivalService)
        .copyProductDetailsToArchiveTablesAndHardDeleteProduct(Mockito.any(Product.class));
    Mockito.verify(domainEventPublisherService).publishDeletedProduct(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void archiveAndDeleteProductDataNullAttributesAndItemsTest() throws Exception {
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    Mockito.when(productItemService.getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID))
        .thenReturn(null);
    Mockito.when(imageService.getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productImage));
    Mockito.when(productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productCategory));
    Mockito.when(productAttributeService.getProductAttributesByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(null);
    Mockito.doNothing().when(productArchivalService)
        .copyProductDetailsToArchiveTablesAndHardDeleteProduct(Mockito.any(Product.class));
    Mockito.doNothing().when(domainEventPublisherService).publishDeletedProduct(STORE_ID, PRODUCT_CODE);

    productDeletionWrapperService.archiveAndDeleteProductData(STORE_ID, PRODUCT_CODE);

    Mockito.verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productAttributeService).getProductAttributesByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(productArchivalService)
        .copyProductDetailsToArchiveTablesAndHardDeleteProduct(Mockito.any(Product.class));
    Mockito.verify(domainEventPublisherService).publishDeletedProduct(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void getProductDetailsTest() {
    Mockito.when(productService.getProductByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    Mockito.when(productItemService.getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID))
        .thenReturn(null);
    Mockito.when(imageService.getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productImage));
    Mockito.when(productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productCategory));
    Mockito.when(productAttributeService.getProductAttributesByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(null);

    product = productDeletionWrapperService.getProductDetails(STORE_ID, PRODUCT_CODE);

    Mockito.verify(productService).getProductByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(imageService).getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productCategoryService).getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productAttributeService).getProductAttributesByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Assertions.assertNotNull(product);
  }

  @Test
  public void getProductDetailsProductNullTest() {
    Mockito.when(productService.getProductByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(null);
    product = productDeletionWrapperService.getProductDetails(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).getProductByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Assertions.assertNull(product);
  }

  @Test
  public void hardDeleteProductAndClearCacheTest() throws Exception {
    product.setProductCode(PRODUCT_CODE);
    product.setStoreId(STORE_ID);
    productDeletionWrapperService.hardDeleteProductAndClearCache(STORE_ID, product);
    Mockito.verify(productDeletionService).deleteProductData(product);
    Mockito.verify(productDeletionService).
        hardDeleteProductAttributeExtracted(PRODUCT_CODE);
    Mockito.verify(productService).evictAllProductDetailCache(STORE_ID, product);
  }

}
