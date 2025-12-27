package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;

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
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.x.productcategorybase.domain.event.model.TerminatedSellerSkuCleanupStatusEventModel;
import com.gdn.x.productcategorybase.domain.event.model.TerminatedSellerSkuImageCleanupEventModel;
import com.gdn.x.productcategorybase.dto.TerminatedSellerSkuCleanupStatusDTO;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.repository.ProductAttributeExtractedRepository;
import com.gdn.x.productcategorybase.repository.ProductAttributeRepository;
import com.gdn.x.productcategorybase.repository.ProductAttributeValueRepository;
import com.gdn.x.productcategorybase.repository.ProductCategoryRepository;
import com.gdn.x.productcategorybase.repository.ProductImageRepository;
import com.gdn.x.productcategorybase.repository.ProductItemAttributeRepository;
import com.gdn.x.productcategorybase.repository.ProductItemImageRepository;
import com.gdn.x.productcategorybase.repository.ProductItemRepository;
import com.gdn.x.productcategorybase.repository.ProductRepository;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;

public class ProductDeletionServiceTest {

  private static final String PRODUCT_ID = "productId";
  private static final String PRODUCT_ITEM_ID = "productItemId";
  private static final String PRODUCT_ATTRIBUTE_ID = "productAttributeId";
  private static final String PRODUCT_ATTRIBUTE_VALUE_ID = "productAttributeValueId";
  private static final String PRODUCT_ITEM_ATTRIBUTE_VALUE_ID = "productItemAttributeValueId";
  private static final String PRODUCT_CATEGORY_ID = "productCategoryId";
  private static final String PRODUCT_IMAGE_ID = "productImageId";
  private static final String PRODUCT_ITEM_IMAGE_ID = "productItemImageId";
  private static final String PRODUCT_CODE = "productCode";
  private static final String SELLER_CODE = "sellerCode";
  private static final String ITEM_CODE = "itemCode";
  private static final long THRESHOLD_IN_MIN = 30;

  private Product product;
  private ProductItem productItem;
  private ProductAttribute productAttribute;
  private ProductAttributeValue productAttributeValue;
  private ProductItemAttributeValue productItemAttributeValue;
  private ProductCategory productCategory;
  private ProductImage productImage;
  private ProductItemImage productItemImage;
  private TerminatedSellerSkuCleanupStatusDTO statusDTO;
  private TerminatedSellerSkuCleanupStatusEventModel statusEventModel;
  private TerminatedSellerSkuImageCleanupEventModel imageCleanupEventModel;

  @InjectMocks
  private ProductDeletionServiceImpl productDeletionService;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductItemRepository productItemRepository;

  @Mock
  private ProductAttributeRepository productAttributeRepository;

  @Mock
  private ProductAttributeValueRepository productAttributeValueRepository;

  @Mock
  private ProductItemAttributeRepository productItemAttributeRepository;

  @Mock
  private ProductCategoryRepository productCategoryRepository;

  @Mock
  private ProductImageRepository productImageRepository;

  @Mock
  private ProductItemImageRepository productItemImageRepository;

  @Mock
  private ProductAttributeExtractedRepository productAttributeExtractedRepository;

  @Mock
  private DomainEventPublisherService domainEventPublisherService;

  @Captor
  private ArgumentCaptor<Product> productArgumentCaptor;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    product = new Product();
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);

    productItem = new ProductItem();
    productItem.setId(PRODUCT_ITEM_ID);
    productItem.setProductId(PRODUCT_ID);

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

    statusDTO = new TerminatedSellerSkuCleanupStatusDTO();
    statusDTO.setProductCode(PRODUCT_CODE);
    statusDTO.setSellerCode(SELLER_CODE);

    statusEventModel = new TerminatedSellerSkuCleanupStatusEventModel();
    statusEventModel.setProductCode(PRODUCT_CODE);
    statusEventModel.setSellerCode(SELLER_CODE);

    imageCleanupEventModel = new TerminatedSellerSkuImageCleanupEventModel();
    imageCleanupEventModel.setProductCode(PRODUCT_CODE);
    imageCleanupEventModel.setSellerCode(SELLER_CODE);

    Mockito.doNothing().when(productRepository).deleteByIds(Arrays.asList(PRODUCT_ID));
    Mockito.doNothing().when(productItemRepository).deleteByIds(Arrays.asList(PRODUCT_ITEM_ID));
    Mockito.doNothing().when(productAttributeRepository).deleteByIds(Arrays.asList(PRODUCT_ATTRIBUTE_ID));
    Mockito.doNothing().when(productAttributeValueRepository).deleteByIds(Arrays.asList(PRODUCT_ATTRIBUTE_VALUE_ID));
    Mockito.doNothing().when(productItemAttributeRepository)
        .deleteByIds(Arrays.asList(PRODUCT_ITEM_ATTRIBUTE_VALUE_ID));
    Mockito.doNothing().when(productCategoryRepository).deleteByIds(Arrays.asList(PRODUCT_CATEGORY_ID));
    Mockito.doNothing().when(productImageRepository).deleteByIds(Arrays.asList(PRODUCT_IMAGE_ID));
    Mockito.doNothing().when(productItemImageRepository).deleteByIds(Arrays.asList(PRODUCT_ITEM_IMAGE_ID));
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productRepository, productItemRepository, productAttributeRepository,
        productAttributeValueRepository, productItemAttributeRepository, productCategoryRepository,
        productImageRepository, productItemImageRepository);
  }

  @Test
  public void deleteProductDataTest() throws Exception {
    productDeletionService.deleteProductData(product);
    Mockito.verify(productRepository).deleteByIds(Arrays.asList(PRODUCT_ID));
    Mockito.verify(productItemRepository).deleteByIds(Arrays.asList(PRODUCT_ITEM_ID));
    Mockito.verify(productAttributeRepository).deleteByIds(Arrays.asList(PRODUCT_ATTRIBUTE_ID));
    Mockito.verify(productAttributeValueRepository).deleteByIds(Arrays.asList(PRODUCT_ATTRIBUTE_VALUE_ID));
    Mockito.verify(productItemAttributeRepository)
        .deleteByIds(Arrays.asList(PRODUCT_ITEM_ATTRIBUTE_VALUE_ID));
    Mockito.verify(productCategoryRepository).deleteByIds(Arrays.asList(PRODUCT_CATEGORY_ID));
    Mockito.verify(productImageRepository).deleteByIds(Arrays.asList(PRODUCT_IMAGE_ID));
    Mockito.verify(productItemImageRepository).deleteByIds(Arrays.asList(PRODUCT_ITEM_IMAGE_ID));
  }

  @Test
  public void deleteProductDataNullTest() throws Exception {
    product.setProductItems(null);
    product.setProductAttributes(null);
    product.setProductImages(null);
    product.setProductCategories(null);
    productDeletionService.deleteProductData(product);
    Mockito.verify(productRepository).deleteByIds(Arrays.asList(PRODUCT_ID));
  }

  @Test
  public void deleteProductDataValuesNullTest() throws Exception {
    product.getProductItems().get(0).setProductItemImages(null);
    product.getProductItems().get(0).setProductItemAttributeValues(null);
    product.getProductAttributes().get(0).setProductAttributeValues(null);
    productDeletionService.deleteProductData(product);
    Mockito.verify(productRepository).deleteByIds(Arrays.asList(PRODUCT_ID));
    Mockito.verify(productItemRepository).deleteByIds(Arrays.asList(PRODUCT_ITEM_ID));
    Mockito.verify(productAttributeRepository).deleteByIds(Arrays.asList(PRODUCT_ATTRIBUTE_ID));
    Mockito.verify(productCategoryRepository).deleteByIds(Arrays.asList(PRODUCT_CATEGORY_ID));
    Mockito.verify(productImageRepository).deleteByIds(Arrays.asList(PRODUCT_IMAGE_ID));
  }

  @Test
  public void publishEventToUpdateStatusAndDeleteImageWithoutImagePublishEventTest() {
    productDeletionService.publishEventToUpdateStatusAndDeleteImage(statusDTO);
    Mockito.verify(domainEventPublisherService).publishTerminatedSellerSkuCleanupStatusEvent(statusEventModel);
  }

  @Test
  public void publishEventToUpdateStatusAndDeleteImageWithImagePublishEventTest() {
    statusDTO.setPublishImageDeletionEvent(true);
    productDeletionService.publishEventToUpdateStatusAndDeleteImage(statusDTO);
    Mockito.verify(domainEventPublisherService).publishTerminatedSellerSkuCleanupStatusEvent(statusEventModel);
    Mockito.verify(domainEventPublisherService).publishTerminatedSellerSkuImageCleanupEvent(imageCleanupEventModel);
  }

  @Test
  public void pickedForDeletionTest() {
    Calendar cal = Calendar.getInstance();
    cal.setTime(new Date());
    cal.add(Calendar.MINUTE, (-1 * (int)(THRESHOLD_IN_MIN - 1)));
    ReflectionTestUtils.setField(productDeletionService,
        "terminatedSellerSkuPickedForDeletionThresholdInMinutes", THRESHOLD_IN_MIN);
    product.setPickedForDeletion(true);
    product.setUpdatedDate(cal.getTime());
    boolean isPickedForDeletion = productDeletionService.pickedForDeletion(product);
    Assertions.assertTrue(isPickedForDeletion);
  }

  @Test
  public void hardDeleteProductAttributeExtractedTest() {
    productDeletionService.hardDeleteProductAttributeExtracted(PRODUCT_CODE);
    Mockito.verify(productAttributeExtractedRepository).deleteByProductCode(PRODUCT_CODE);
  }

  @Test
  public void pickedForDeletionFalseTest() {
    product.setPickedForDeletion(false);
    boolean isPickedForDeletion = productDeletionService.pickedForDeletion(product);
    Assertions.assertFalse(isPickedForDeletion);
  }

  @Test
  public void pickedForDeletionMoreThanThresholdTest() {
    Calendar cal = Calendar.getInstance();
    cal.setTime(new Date());
    cal.add(Calendar.MINUTE, (-1 * (int)(THRESHOLD_IN_MIN + 1)));
    ReflectionTestUtils.setField(productDeletionService,
        "terminatedSellerSkuPickedForDeletionThresholdInMinutes", THRESHOLD_IN_MIN);
    product.setPickedForDeletion(true);
    product.setUpdatedDate(cal.getTime());
    boolean isPickedForDeletion = productDeletionService.pickedForDeletion(product);
    Assertions.assertFalse(isPickedForDeletion);
  }

  @Test
  public void updatePickedForDeletionFlagTest() {
    productDeletionService.updatePickedForDeletionFlag(product, Boolean.TRUE);
    Mockito.verify(productRepository).save(productArgumentCaptor.capture());
    Assertions.assertTrue(productArgumentCaptor.getValue().isPickedForDeletion());
  }

  @Test
  public void isAllowedToDeleteImageFalseTest()  {
    boolean isAllowedToDeleteImage = productDeletionService.isAllowedToDeleteImage(new ArrayList<>());
    Assertions.assertFalse(isAllowedToDeleteImage);
  }

  @Test
  public void isAllowedToDeleteImageTrueTest()  {
    productItem.setSkuCode(ITEM_CODE);
    boolean isAllowedToDeleteImage = productDeletionService.isAllowedToDeleteImage(Arrays.asList(productItem));
    Mockito.verify(productItemRepository).findFirstBySourceItemCodeIn(Arrays.asList(ITEM_CODE));
    Assertions.assertTrue(isAllowedToDeleteImage);
  }

  @Test
  public void isAllowedToDeleteImageHavingSourceItemCodeTest()  {
    productItem.setSourceItemCode(ITEM_CODE);
    boolean isAllowedToDeleteImage = productDeletionService.isAllowedToDeleteImage(Arrays.asList(productItem));
    Assertions.assertFalse(isAllowedToDeleteImage);
  }

  @Test
  public void isAllowedToDeleteImageHavingSourceItemCodeForAnotherItemTest()  {
    productItem.setSkuCode(ITEM_CODE);
    Mockito.when(productItemRepository.findFirstBySourceItemCodeIn(Arrays.asList(ITEM_CODE))).thenReturn(productItem);
    boolean isAllowedToDeleteImage = productDeletionService.isAllowedToDeleteImage(Arrays.asList(productItem));
    Mockito.verify(productItemRepository).findFirstBySourceItemCodeIn(Arrays.asList(ITEM_CODE));
    Assertions.assertFalse(isAllowedToDeleteImage);
  }

}
