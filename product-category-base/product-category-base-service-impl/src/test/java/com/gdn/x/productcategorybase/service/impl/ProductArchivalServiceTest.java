package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;


import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductArchive;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeArchive;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductAttributeValueArchive;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductCategoryArchive;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductImageArchive;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemArchive;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValueArchive;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.entity.ProductItemImageArchive;
import com.gdn.x.productcategorybase.entity.SystemParameter;
import com.gdn.x.productcategorybase.repository.ProductArchiveRepository;
import com.gdn.x.productcategorybase.repository.ProductAttributeArchiveRepository;
import com.gdn.x.productcategorybase.repository.ProductAttributeValueArchiveRepository;
import com.gdn.x.productcategorybase.repository.ProductCategoryArchiveRepository;
import com.gdn.x.productcategorybase.repository.ProductImageArchiveRepository;
import com.gdn.x.productcategorybase.repository.ProductItemArchiveRepository;
import com.gdn.x.productcategorybase.repository.ProductItemAttributeValueArchiveRepository;
import com.gdn.x.productcategorybase.repository.ProductItemImageArchiveRepository;
import com.gdn.x.productcategorybase.service.ProductArchivalService;
import com.gdn.x.productcategorybase.service.ProductDeletionService;
import com.gdn.x.productcategorybase.service.ProductService;
import com.gdn.x.productcategorybase.service.SystemParameterService;

public class ProductArchivalServiceTest {
  private static final String STORE_ID = "10001";
  private static final String PRODUCT_ID = "productId";
  private static final String PRODUCT_ITEM_ID = "productItemId";
  private static final String PRODUCT_ATTRIBUTE_ID = "productAttributeId";
  private static final String PRODUCT_ATTRIBUTE_VALUE_ID = "productAttributeValueId";
  private static final String PRODUCT_ITEM_ATTRIBUTE_VALUE_ID = "productItemAttributeValueId";
  private static final String PRODUCT_CATEGORY_ID = "productCategoryId";
  private static final String PRODUCT_IMAGE_ID = "productImageId";
  private static final String PRODUCT_ITEM_IMAGE_ID = "productItemImageId";

  @InjectMocks
  private ProductArchivalServiceImpl productArchivalService;

  @Mock
  private ProductArchiveRepository productArchiveRepository;

  @Mock
  private ProductItemArchiveRepository productItemArchiveRepository;

  @Mock
  private ProductAttributeArchiveRepository productAttributeArchiveRepository;

  @Mock
  private ProductAttributeValueArchiveRepository productAttributeValueArchiveRepository;

  @Mock
  private ProductItemAttributeValueArchiveRepository productItemAttributeValueArchiveRepository;

  @Mock
  private ProductCategoryArchiveRepository productCategoryArchiveRepository;

  @Mock
  private ProductImageArchiveRepository productImageArchiveRepository;

  @Mock
  private ProductItemImageArchiveRepository productItemImageArchiveRepository;

  @Mock
  private ProductDeletionService productDeletionService;

  @Mock
  private ProductService productService;

  @Mock
  private SystemParameterService systemParameterService;

  private Product product;
  private ProductItem productItem;
  private ProductAttribute productAttribute;
  private ProductAttributeValue productAttributeValue;
  private ProductItemAttributeValue productItemAttributeValue;
  private ProductCategory productCategory;
  private ProductImage productImage;
  private ProductItemImage productItemImage;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    product = new Product();
    product.setId(PRODUCT_ID);

    productItem = new ProductItem();
    productItem.setId(PRODUCT_ITEM_ID);

    productAttribute = new ProductAttribute();
    productAttribute.setId(PRODUCT_ATTRIBUTE_ID);

    productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setId(PRODUCT_ATTRIBUTE_VALUE_ID);

    productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setId(PRODUCT_ITEM_ATTRIBUTE_VALUE_ID);

    productCategory = new ProductCategory();
    productCategory.setId(PRODUCT_CATEGORY_ID);

    productImage = new ProductImage();
    productImage.setId(PRODUCT_IMAGE_ID);

    productItemImage = new ProductItemImage();
    productItemImage.setId(PRODUCT_ITEM_IMAGE_ID);

    productAttribute.setProductAttributeValues(Arrays.asList(productAttributeValue));
    productItem.setProductItemAttributeValues(Arrays.asList(productItemAttributeValue));
    productItem.setProductItemImages(Arrays.asList(productItemImage));
    product.setProductAttributes(Arrays.asList(productAttribute));
    product.setProductItems(Arrays.asList(productItem));
    product.setProductCategories(Arrays.asList(productCategory));
    product.setProductImages(Arrays.asList(productImage));

    Mockito.when(productArchiveRepository.save(Mockito.any(ProductArchive.class))).thenReturn(new ProductArchive());
    Mockito.when(productItemArchiveRepository.saveAll(Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    Mockito.when(productAttributeArchiveRepository.saveAll(Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    Mockito.when(productAttributeValueArchiveRepository.saveAll(Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    Mockito.when(productItemImageArchiveRepository.saveAll(Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    Mockito.when(productImageArchiveRepository.saveAll(Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    Mockito.when(
            productItemAttributeValueArchiveRepository.saveAll(Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    Mockito.when(productCategoryArchiveRepository.saveAll(Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    Mockito.doNothing().when(productDeletionService).deleteProductData(product);
    Mockito.doNothing().when(productService).evictAllProductDetailCache(product.getStoreId(), product);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productArchiveRepository, productItemArchiveRepository, productAttributeArchiveRepository,
        productAttributeValueArchiveRepository, productItemImageArchiveRepository, productImageArchiveRepository,
        productItemAttributeValueArchiveRepository, productCategoryArchiveRepository, productDeletionService, productService);
  }

  @Test
  public void copyProductDetailsToArchiveTablesAndHardDeleteProductTest() throws Exception {
    productArchivalService.copyProductDetailsToArchiveTablesAndHardDeleteProduct(product);

    Mockito.verify(productArchiveRepository).save(Mockito.any(ProductArchive.class));
    Mockito.verify(productItemArchiveRepository).saveAll(Mockito.anyList());
    Mockito.verify(productAttributeArchiveRepository).saveAll(Mockito.anyList());
    Mockito.verify(productAttributeValueArchiveRepository).saveAll(Mockito.anyList());
    Mockito.verify(productItemImageArchiveRepository).saveAll(Mockito.anyList());
    Mockito.verify(productImageArchiveRepository).saveAll(Mockito.anyList());
    Mockito.verify(productItemAttributeValueArchiveRepository)
        .saveAll(Mockito.anyList());
    Mockito.verify(productCategoryArchiveRepository).saveAll(Mockito.anyList());
    Mockito.verify(productDeletionService).deleteProductData(product);
    Mockito.verify(productService).evictAllProductDetailCache(product.getStoreId(), product);
  }

  @Test
  public void copyProductDetailsToArchiveTablesNullDataTest() throws Exception {
    product.setProductItems(null);
    product.setProductAttributes(null);
    product.setProductImages(null);
    product.setProductCategories(null);
    productArchivalService.copyProductDetailsToArchiveTablesAndHardDeleteProduct(product);
    Mockito.verify(productArchiveRepository).save(Mockito.any(ProductArchive.class));
    Mockito.verify(productDeletionService).deleteProductData(product);
    Mockito.verify(productService).evictAllProductDetailCache(product.getStoreId(), product);
  }

  @Test
  public void copyProductDetailsToArchiveTablesNullValuesTest() throws Exception {
    product.getProductItems().get(0).setProductItemImages(null);
    product.getProductItems().get(0).setProductItemAttributeValues(null);
    product.getProductAttributes().get(0).setProductAttributeValues(null);
    productArchivalService.copyProductDetailsToArchiveTablesAndHardDeleteProduct(product);
    Mockito.verify(productArchiveRepository).save(Mockito.any(ProductArchive.class));
    Mockito.verify(productItemArchiveRepository).saveAll(Mockito.anyList());
    Mockito.verify(productAttributeArchiveRepository).saveAll(Mockito.anyList());
    Mockito.verify(productImageArchiveRepository).saveAll(Mockito.anyList());
    Mockito.verify(productCategoryArchiveRepository).saveAll(Mockito.anyList());
    Mockito.verify(productDeletionService).deleteProductData(product);
    Mockito.verify(productService).evictAllProductDetailCache(product.getStoreId(), product);
  }

  @Test
  public void deleteDataFromArchivalTableTest() throws Exception {
    Mockito.when(
            systemParameterService.findByStoreIdAndVariable(STORE_ID, Constants.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS))
        .thenReturn(new SystemParameter(Constants.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS, "1",
            Constants.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS));
    Mockito.when(systemParameterService.findByStoreIdAndVariable(STORE_ID,
        Constants.DAYS_THRESHOLD_FOR_ARCHIVED_PRODUCT_DELETION)).thenReturn(
        new SystemParameter(Constants.DAYS_THRESHOLD_FOR_ARCHIVED_PRODUCT_DELETION, "1",
            Constants.DAYS_THRESHOLD_FOR_ARCHIVED_PRODUCT_DELETION));
    Mockito.when(productArchiveRepository.findIdByCreatedDateLessThan(Mockito.any(Date.class), Mockito.eq(1)))
        .thenReturn(Arrays.asList(PRODUCT_ID));

    productArchivalService.deleteDataFromArchivalTable(STORE_ID);

    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.DAYS_THRESHOLD_FOR_ARCHIVED_PRODUCT_DELETION);
    Mockito.verify(productArchiveRepository).findIdByCreatedDateLessThan(Mockito.any(Date.class), Mockito.eq(1));
    Mockito.verify(productAttributeValueArchiveRepository).deleteByProductIdIn(Arrays.asList(PRODUCT_ID));
    Mockito.verify(productItemAttributeValueArchiveRepository).deleteByProductIdIn(Arrays.asList(PRODUCT_ID));
    Mockito.verify(productItemImageArchiveRepository).deleteByProductIdIn(Arrays.asList(PRODUCT_ID));
    Mockito.verify(productImageArchiveRepository).deleteByProductIdIn(Arrays.asList(PRODUCT_ID));
    Mockito.verify(productAttributeArchiveRepository).deleteByProductIdIn(Arrays.asList(PRODUCT_ID));
    Mockito.verify(productCategoryArchiveRepository).deleteByProductIdIn(Arrays.asList(PRODUCT_ID));
    Mockito.verify(productItemArchiveRepository).deleteByProductIdIn(Arrays.asList(PRODUCT_ID));
    Mockito.verify(productArchiveRepository).deleteByProductIdIn(Arrays.asList(PRODUCT_ID));
  }

  @Test
  public void deleteDataFromArchivalTableNoProductsFoundTest() throws Exception {
    Mockito.when(
            systemParameterService.findByStoreIdAndVariable(STORE_ID, Constants.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS))
        .thenReturn(new SystemParameter(Constants.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS, "1",
            Constants.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS));
    Mockito.when(systemParameterService.findByStoreIdAndVariable(STORE_ID,
        Constants.DAYS_THRESHOLD_FOR_ARCHIVED_PRODUCT_DELETION)).thenReturn(
        new SystemParameter(Constants.DAYS_THRESHOLD_FOR_ARCHIVED_PRODUCT_DELETION, "1",
            Constants.DAYS_THRESHOLD_FOR_ARCHIVED_PRODUCT_DELETION));
    Mockito.when(productArchiveRepository.findIdByCreatedDateLessThan(Mockito.any(Date.class), Mockito.eq(1)))
        .thenReturn(null);

    productArchivalService.deleteDataFromArchivalTable(STORE_ID);

    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.DAYS_THRESHOLD_FOR_ARCHIVED_PRODUCT_DELETION);
    Mockito.verify(productArchiveRepository).findIdByCreatedDateLessThan(Mockito.any(Date.class), Mockito.eq(1));
  }

  @Test
  public void deleteDataFromArchivalTableBatchSize0Test() throws Exception {
    Mockito.when(
            systemParameterService.findByStoreIdAndVariable(STORE_ID, Constants.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS))
        .thenReturn(new SystemParameter(Constants.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS, "0",
            Constants.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS));

    productArchivalService.deleteDataFromArchivalTable(STORE_ID);

    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS);
  }
}
