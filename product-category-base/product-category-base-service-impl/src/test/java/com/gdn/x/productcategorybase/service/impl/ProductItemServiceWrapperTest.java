package com.gdn.x.productcategorybase.service.impl;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.dto.ProductPublishUpdateDTO;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.UPCCodeSearchRequest;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.ImageService;
import com.gdn.x.productcategorybase.service.ProductCategoryService;
import com.gdn.x.productcategorybase.service.ProductItemService;
import com.gdn.x.productcategorybase.service.ProductService;
import com.gdn.x.productcategorybase.entity.ProductItemUomInfo;
import com.gdn.x.productcategorybase.service.ProductItemUomInfoService;

public class ProductItemServiceWrapperTest {

  @Mock
  private ProductItemService productItemService;

  @Mock
  private CategoryService categoryService;

  @Mock
  private ProductCategoryService productCategoryService;

  @Mock
  ImageService imageService;

  @Mock
  ApplicationCacheServiceBean applicationCacheServiceBean;

  @Mock
  ProductServiceBean productServiceBean;

  @Mock
  DomainEventPublisherService domainEventPublisherService;

  @Mock
  ProductItemUomInfoService productItemUomInfoService;

  @InjectMocks
  private ProductItemServiceWrapperImpl productItemServiceWrapper;

  private static final String STORE_ID = "storeId";
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String PRODUCT_ID = "id";
  private static final String PRODUCT_ITEM_ID_1 = "productItemId1";
  private static final String PRODUCT_ITEM_ID_2 = "productItemId2";
  private static final String ITEM_SKU_CODE_1 = "itemSkuCode1";
  private static final String ITEM_SKU_CODE_2 = "itemSkuCode2";
  private static final String PRODUCT_ITEM_ATTRIBUTE_VALUE_1 = "productItemAttributeValue1";
  private static final String PRODUCT_ITEM_ATTRIBUTE_VALUE_2 = "productItemAttributeValue2";
  private static final String PRODUCT_ITEM_ATTRIBUTE_VALUE_3 = "productItemAttributeValue3";
  private static final String PRODUCT_ITEM_NAME_1 = "productItemName";
  private static final String ATTRIBUTE_ID_1 = "attributeId1";
  private static final String ATTRIBUTE_ID_2 = "attributeId2";
  private static final String ATTRIBUTE_ID_3 = "attributeId3";
  private static final String ATTRIBUTE_NAME_1 = "attributeName1";
  private static final String ATTRIBUTE_NAME_2 = "attributeName2";
  private static final String ATTRIBUTE_NAME_3 = "attributeName3";
  private static final String ATTRIBUTE_CODE_1 = "ATTR-0000001";
  private static final String ATTRIBUTE_CODE_2 = "ATTR-0000002";
  private static final String ATTRIBUTE_CODE_3 = "ATTR-0000003";
  private static final String IMAGE_ID_1 = "imageId1";
  private static final String IMAGE_ID_2 = "imageId2";
  private static final String CATEGORY_ID = "categoryId";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String UPC_CODE = "1234567";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private UPCCodeSearchRequest upcCodeSearchRequest;
  private Product product;
  private ProductItem productItem1;
  private ProductItem productItem2;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    product = new Product();
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);

    Attribute attribute1 = new Attribute();
    attribute1.setId(ATTRIBUTE_ID_1);
    attribute1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute1.setAttributeCode(ATTRIBUTE_CODE_1);
    attribute1.setName(ATTRIBUTE_NAME_1);

    Attribute attribute2 = new Attribute();
    attribute2.setId(ATTRIBUTE_ID_2);
    attribute2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attribute2.setAttributeCode(ATTRIBUTE_CODE_2);
    attribute2.setName(ATTRIBUTE_NAME_2);

    Attribute attribute3 = new Attribute();
    attribute3.setId(ATTRIBUTE_ID_3);
    attribute3.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute3.setAttributeCode(ATTRIBUTE_CODE_3);
    attribute3.setName(ATTRIBUTE_NAME_3);

    productItem1 = new ProductItem();
    productItem1.setId(PRODUCT_ITEM_ID_1);
    productItem1.setProductId(PRODUCT_ID);
    productItem1.setSkuCode(ITEM_SKU_CODE_1);
    productItem1.setGeneratedItemName(PRODUCT_ITEM_NAME_1);
    productItem1.setProductItemAttributeValues(new ArrayList<>());
    productItem1.setProductItemImages(new ArrayList<>());

    ProductItemAttributeValue productItemAttributeValue1 = new ProductItemAttributeValue();
    productItemAttributeValue1.setAttributeId(ATTRIBUTE_ID_1);
    productItemAttributeValue1.setValue(PRODUCT_ITEM_ATTRIBUTE_VALUE_1);
    productItemAttributeValue1.setAttribute(attribute1);
    productItemAttributeValue1.setProductItemId(PRODUCT_ITEM_ID_1);

    ProductItemAttributeValue productItemAttributeValue2 = new ProductItemAttributeValue();
    productItemAttributeValue2.setAttributeId(ATTRIBUTE_ID_2);
    productItemAttributeValue2.setMarkForDelete(true);
    productItemAttributeValue2.setValue(PRODUCT_ITEM_ATTRIBUTE_VALUE_2);
    productItemAttributeValue2.setAttribute(attribute2);
    productItemAttributeValue2.setProductItemId(PRODUCT_ITEM_ID_1);

    ProductItemAttributeValue productItemAttributeValue3 = new ProductItemAttributeValue();
    productItemAttributeValue3.setAttributeId(ATTRIBUTE_ID_3);
    productItemAttributeValue3.setValue(PRODUCT_ITEM_ATTRIBUTE_VALUE_3);
    productItemAttributeValue3.setAttribute(attribute3);
    productItemAttributeValue3.setProductItemId(PRODUCT_ITEM_ID_1);

    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setId(IMAGE_ID_1);
    productItemImage1.setOriginalImage(Boolean.TRUE);
    productItemImage1.setProductItemId(PRODUCT_ITEM_ID_1);

    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setId(IMAGE_ID_2);
    productItemImage2.setMarkForDelete(true);
    productItemImage2.setOriginalImage(Boolean.FALSE);
    productItemImage2.setProductItemId(PRODUCT_ITEM_ID_1);

    productItem2 = new ProductItem();
    productItem2.setId(PRODUCT_ITEM_ID_2);
    productItem2.setSkuCode(ITEM_SKU_CODE_2);
    productItem2.setMarkForDelete(true);

    List<ProductItem> productItems = new ArrayList<>(Arrays.asList(productItem1, productItem2));

    upcCodeSearchRequest = new UPCCodeSearchRequest();
    upcCodeSearchRequest.setUpcCode(UPC_CODE);
    upcCodeSearchRequest.setCategoryIds(Collections.singletonList(CATEGORY_ID));
    upcCodeSearchRequest.setCategoryCodes(Collections.singletonList(CATEGORY_CODE));

    Mockito.when(productItemService.getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID))
        .thenReturn(productItems);
    Mockito.when(productItemService.getProductItemsByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(productItems);
    when(this.categoryService.findIdsByStoreIdAndCategoryCodes(STORE_ID, upcCodeSearchRequest.getCategoryCodes()))
        .thenReturn(Collections.singletonList(CATEGORY_ID));
    when(productItemService.getProductItemsIdsByUpcCodeAndCategoryIds(
        UPC_CODE, true, Collections.singletonList(CATEGORY_ID)))
        .thenReturn(Collections.singletonList(PRODUCT_ITEM_ID_1));
    when(productItemService.getProductItemsIdsByUpcCodeAndCategoryIds(
        UPC_CODE, false, Collections.singletonList(CATEGORY_ID)))
        .thenReturn(Collections.singletonList(PRODUCT_ITEM_ID_1));
    when(productItemService.getProductItemsByStoreIdAndIds(STORE_ID, Collections.singletonList(PRODUCT_ITEM_ID_1),
        PageRequest.of(PAGE, SIZE))).thenReturn(
        new PageImpl<>(Collections.singletonList(productItem1)));
    when(productItemService.getProductItemImagesCached(Mockito.eq(STORE_ID), any(Product.class)))
        .thenReturn(Arrays.asList(productItemImage1, productItemImage2));
    when(productItemService.getProductItemAttributeValuesWithAttributesCached(Mockito.eq(STORE_ID), any(Product.class)))
        .thenReturn(Arrays.asList(productItemAttributeValue1, productItemAttributeValue2, productItemAttributeValue3));
  }

  @Test
  public void setProductItemsWithProductItemImagesCachedTest() {
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setProductItemId(PRODUCT_ITEM_ID_1);
    productItemImage1.setMarkForDelete(false);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setProductItemId(PRODUCT_ITEM_ID_2);
    productItemImage2.setMarkForDelete(true);
    List<ProductItemImage> productItemImageList = new ArrayList<>();
    productItemImageList.add(productItemImage1);
    productItemImageList.add(productItemImage2);
    ProductItem productItem1 = new ProductItem();
    productItem1.setProductItemImages(productItemImageList);
    product.setProductItems(Collections.singletonList(productItem1));
    Mockito.when(productItemService.getProductItemImagesCached(STORE_ID, product)).thenReturn(productItemImageList);
    productItemServiceWrapper.setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productItemService).getProductItemImagesCached(STORE_ID, product);
    for (ProductItem productItem: product.getProductItems()) {
      Mockito.verify(productItemService).removeDeletedProductItemImages(productItem);
    }
    Assertions.assertEquals(1, product.getProductItems().size());
    Assertions.assertEquals(PRODUCT_ITEM_ID_1, product.getProductItems().get(0).getId());
  }

  @Test
  public void setProductItemsWithProductItemImagesIncludeMarkForDeleteTrueTest() {
    productItemServiceWrapper.setProductItemsWithProductItemImagesCached(STORE_ID, product, true);
    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productItemService).getProductItemImagesCached(STORE_ID, product);
    Assertions.assertEquals(2, product.getProductItems().size());
    Assertions.assertEquals(PRODUCT_ITEM_ID_1, product.getProductItems().get(0).getId());
    Assertions.assertEquals(PRODUCT_ITEM_ID_2, product.getProductItems().get(1).getId());
  }

  @Test
  public void setProductItemsWithProductItemImagesCachedWithoutFilteringMainImagesTest() {
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setProductItemId(PRODUCT_ITEM_ID_1);
    productItemImage1.setMarkForDelete(false);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setProductItemId(PRODUCT_ITEM_ID_2);
    productItemImage2.setMarkForDelete(true);
    List<ProductItemImage> productItemImageList = new ArrayList<>();
    productItemImageList.add(productItemImage1);
    productItemImageList.add(productItemImage2);
    ProductItem productItem1 = new ProductItem();
    productItem1.setProductItemImages(productItemImageList);
    product.setProductItems(Collections.singletonList(productItem1));
    productItem1.setId(STORE_ID);
    List<ProductItem> productItems = new ArrayList<>(Arrays.asList(productItem1, productItem2));
    Mockito.when(productItemService.getProductItemImagesCached(STORE_ID, product)).thenReturn(productItemImageList);
    productItemServiceWrapper.setProductItemsWithProductItemImagesCachedWithoutFilteringMainImages(STORE_ID, product, false);
    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productItemService).getProductItemImagesCached(STORE_ID, product);
    for (ProductItem productItem: product.getProductItems()) {
      Mockito.verify(productItemService).removeDeletedProductItemImagesWithoutFilteringMainImages(productItem);
    }
    Assertions.assertEquals(1, product.getProductItems().size());
    Assertions.assertEquals(PRODUCT_ITEM_ID_1, product.getProductItems().get(0).getId());
  }

  @Test
  public void setProductItemsNotContainsInOriginalMapTest() {
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setProductItemId(PRODUCT_ITEM_ID_1);
    productItemImage1.setMarkForDelete(false);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setProductItemId(PRODUCT_ITEM_ID_2);
    productItemImage2.setMarkForDelete(false);
    List<ProductItemImage> productItemImageList = new ArrayList<>();
    productItemImageList.add(productItemImage1);
    productItemImageList.add(productItemImage2);
    ProductItem productItem1 = new ProductItem();
    productItem1.setProductItemImages(productItemImageList);
    product.setProductItems(Collections.singletonList(productItem1));
    productItem1.setId(STORE_ID);
    List<ProductItem> productItems = new ArrayList<>(Arrays.asList(productItem1, productItem2));
    Mockito.when(productItemService.getProductItemImagesCached(STORE_ID, product)).thenReturn(productItemImageList);
    productItemServiceWrapper.setProductItemsWithProductItemImagesCachedWithoutFilteringMainImages(STORE_ID, product, false);
    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productItemService).getProductItemImagesCached(STORE_ID, product);
    for (ProductItem productItem: product.getProductItems()) {
      Mockito.verify(productItemService).removeDeletedProductItemImagesWithoutFilteringMainImages(productItem);
    }
    Assertions.assertEquals(1, product.getProductItems().size());
    Assertions.assertEquals(PRODUCT_ITEM_ID_1, product.getProductItems().get(0).getId());
  }

  @Test
  public void setProductItemsWithProductItemImagesIncludeMarkForDeleteTrueWithoutFilteringMainImagesTest() {
    productItemServiceWrapper.setProductItemsWithProductItemImagesCachedWithoutFilteringMainImages(STORE_ID, product, true);
    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productItemService).getProductItemImagesCached(STORE_ID, product);
    Assertions.assertEquals(2, product.getProductItems().size());
    Assertions.assertEquals(PRODUCT_ITEM_ID_1, product.getProductItems().get(0).getId());
    Assertions.assertEquals(PRODUCT_ITEM_ID_2, product.getProductItems().get(1).getId());
  }

  @Test
  public void setCompleteProductItemDetailsCachedTest() {
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setProductItemId(PRODUCT_ITEM_ID_1);
    productItemImage1.setMarkForDelete(false);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setProductItemId(PRODUCT_ITEM_ID_2);
    productItemImage2.setMarkForDelete(true);
    List<ProductItemImage> productItemImageList = new ArrayList<>();
    productItemImageList.add(productItemImage1);
    productItemImageList.add(productItemImage2);
    ProductItem productItem1 = new ProductItem();
    productItem1.setProductItemImages(productItemImageList);
    product.setProductItems(Collections.singletonList(productItem1));
    Mockito.when(productItemService.getProductItemImagesCached(STORE_ID, product)).thenReturn(productItemImageList);
    productItemServiceWrapper.setCompleteProductItemDetailsCached(STORE_ID, product, false);
    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productItemService).getProductItemImagesCached(STORE_ID, product);
    Mockito.verify(productItemService).getProductItemAttributeValuesWithAttributesCached(STORE_ID, product);
    for (ProductItem productItem: product.getProductItems()) {
      Mockito.verify(productItemService).removeDeletedProductItemImages(productItem);
    }
    Assertions.assertEquals(1, product.getProductItems().size());
    Assertions.assertEquals(PRODUCT_ITEM_ID_1, product.getProductItems().get(0).getId());
  }

  @Test
  public void setCompleteProductItemDetailsCachedItemNotExistsTest() {
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setProductItemId(PRODUCT_ITEM_ID_1);
    productItemImage1.setMarkForDelete(false);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setProductItemId(PRODUCT_ITEM_ID_2);
    productItemImage2.setMarkForDelete(true);
    List<ProductItemImage> productItemImageList = new ArrayList<>();
    productItemImageList.add(productItemImage1);
    productItemImageList.add(productItemImage2);
    ProductItem productItem1 = new ProductItem();
    productItem1.setProductItemImages(productItemImageList);
    product.setProductItems(Collections.singletonList(productItem1));
    productItem1.setId(STORE_ID);
    List<ProductItem> productItems = new ArrayList<>(Arrays.asList(productItem1, productItem2));
    Mockito.when(productItemService.getProductItemImagesCached(STORE_ID, product)).thenReturn(productItemImageList);
    Mockito.when(productItemService.getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID))
        .thenReturn(productItems);
    productItemServiceWrapper.setCompleteProductItemDetailsCached(STORE_ID, product, false);
    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productItemService).getProductItemImagesCached(STORE_ID, product);
    Mockito.verify(productItemService).getProductItemAttributeValuesWithAttributesCached(STORE_ID, product);
    for (ProductItem productItem: product.getProductItems()) {
      Mockito.verify(productItemService).removeDeletedProductItemImages(productItem);
    }
    Assertions.assertEquals(1, product.getProductItems().size());
    Assertions.assertEquals(STORE_ID, product.getProductItems().get(0).getId());
  }

  @Test
  public void setCompleteProductItemDetailsIncludeMarkForDeleteTrueTest() {
    productItemServiceWrapper.setCompleteProductItemDetailsCached(STORE_ID, product, true);
    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productItemService).getProductItemImagesCached(STORE_ID, product);
    Mockito.verify(productItemService).getProductItemAttributeValuesWithAttributesCached(STORE_ID, product);
    Assertions.assertEquals(2, product.getProductItems().size());
    Assertions.assertEquals(PRODUCT_ITEM_ID_1, product.getProductItems().get(0).getId());
    Assertions.assertEquals(PRODUCT_ITEM_ID_2, product.getProductItems().get(1).getId());
  }

  @Test
  public void setProductItemsCachedTest() {
    productItemServiceWrapper.setProductItemsCached(STORE_ID, product, false);
    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Assertions.assertEquals(1, product.getProductItems().size());
    Assertions.assertEquals(PRODUCT_ITEM_ID_1, product.getProductItems().get(0).getId());
  }

  @Test
  public void setProductItemsCachedIncludeMarkForDeleteTrueTest() {
    productItemServiceWrapper.setProductItemsCached(STORE_ID, product, true);
    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Assertions.assertEquals(2, product.getProductItems().size());
    Assertions.assertEquals(PRODUCT_ITEM_ID_1, product.getProductItems().get(0).getId());
    Assertions.assertEquals(PRODUCT_ITEM_ID_2, product.getProductItems().get(1).getId());
  }

  @Test
  public void setProductItemsWithProductItemAttributeValuesAndAttributeCachedTest() {
    productItemServiceWrapper.setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        STORE_ID, product, false);
    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productItemService).getProductItemAttributeValuesWithAttributesCached(STORE_ID, product);
    Assertions.assertEquals(1, product.getProductItems().size());
    Assertions.assertEquals(PRODUCT_ITEM_ID_1, product.getProductItems().get(0).getId());
  }

  @Test
  public void findByUPCCodeAndCategoryIds_IsOnlyExternalFlagTrue() {
    Page<ProductItem> productItemList =
        productItemServiceWrapper.findByUPCCodeAndCategoryIds(
            STORE_ID, upcCodeSearchRequest, true, PAGE, SIZE);
    verify(categoryService).findIdsByStoreIdAndCategoryCodes(STORE_ID, upcCodeSearchRequest.getCategoryCodes());
    verify(productItemService).getProductItemsIdsByUpcCodeAndCategoryIds(
        UPC_CODE, true, Collections.singletonList(CATEGORY_ID));
    verify(productItemService).getProductItemsByStoreIdAndIds(
        STORE_ID, Collections.singletonList(PRODUCT_ITEM_ID_1), PageRequest.of(PAGE, SIZE));
    verify(productItemService).setProductCached(STORE_ID, productItem1);
    Assertions.assertEquals(1, productItemList.getContent().size());
  }

  @Test
  public void findByUPCCodeAndCategoryIds_IsOnlyExternalFlagFalse() {
    Page<ProductItem> productItemList =
        productItemServiceWrapper.findByUPCCodeAndCategoryIds(
            STORE_ID, upcCodeSearchRequest, false, PAGE, SIZE);
    verify(categoryService).findIdsByStoreIdAndCategoryCodes(STORE_ID, upcCodeSearchRequest.getCategoryCodes());
    verify(productItemService).getProductItemsIdsByUpcCodeAndCategoryIds(
        UPC_CODE, false, Collections.singletonList(CATEGORY_ID));
    verify(productItemService).getProductItemsByStoreIdAndIds(
        STORE_ID, Collections.singletonList(PRODUCT_ITEM_ID_1), PageRequest.of(PAGE, SIZE));
    verify(productItemService).setProductCached(STORE_ID, productItem1);
    Assertions.assertEquals(1, productItemList.getContent().size());
  }

  @Test
  public void findByUPCCodeAndCategoryIds_emptyListReturned() {
    when(productItemService.getProductItemsIdsByUpcCodeAndCategoryIds(
        UPC_CODE, false, Collections.singletonList(CATEGORY_ID))).thenReturn(null);
    Page<ProductItem> productItemList =
        productItemServiceWrapper.findByUPCCodeAndCategoryIds(
            STORE_ID, upcCodeSearchRequest, false, PAGE, SIZE);
    verify(categoryService).findIdsByStoreIdAndCategoryCodes(STORE_ID, upcCodeSearchRequest.getCategoryCodes());
    verify(productItemService).getProductItemsIdsByUpcCodeAndCategoryIds(
        UPC_CODE, false, Collections.singletonList(CATEGORY_ID));
    Assertions.assertEquals(0, productItemList.getContent().size());
  }

  @Test
  public void findByUPCCodeAndCategoryIds_ExceptionTest() {
    when(productItemService.getProductItemsIdsByUpcCodeAndCategoryIds(
        UPC_CODE, true, Collections.singletonList(CATEGORY_ID)))
        .thenThrow(new ApplicationRuntimeException());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productItemServiceWrapper.findByUPCCodeAndCategoryIds(
          STORE_ID, upcCodeSearchRequest, true, PAGE, SIZE));
    } finally {
      verify(categoryService).findIdsByStoreIdAndCategoryCodes(STORE_ID, upcCodeSearchRequest.getCategoryCodes());
      verify(productItemService).getProductItemsIdsByUpcCodeAndCategoryIds(
          UPC_CODE, true, Collections.singletonList(CATEGORY_ID));
    }
  }

  @Test
  public void findProductItemImagesByItemCodesTest(){
    ProductItemImage productItemImage = new ProductItemImage();
    List<String> itemCodes = new ArrayList<>();
    List<String> productItemIds = new ArrayList<>();
    List<ProductItem> productItems = new ArrayList<>();
    productItem1.setProductId(PRODUCT_ID);
    productItems.add(productItem1);
    productItemIds.add(PRODUCT_ITEM_ID_1);
    itemCodes.add(ITEM_SKU_CODE_1);
    productItemImage.setProductItemId(PRODUCT_ITEM_ID_1);
    List<ProductItemImage> productItemImages = new ArrayList<>();
    productItemImages.add(productItemImage);
    Mockito.when(productItemService.findItemsBySkuCodesAndMarkForDeleteFalse(STORE_ID,
      itemCodes)).thenReturn(productItems);
    doNothing().when(productItemService).removeDeletedProductItemImages(productItem1);
    Mockito.when(imageService.getDetachedProductItemImagesByStoreIdAndProductItemIds(STORE_ID,
      productItemIds)).thenReturn(productItemImages);
    productItemServiceWrapper.findProductItemImagesByItemCodes(STORE_ID,
        itemCodes, false, null);
    verify(productItemService).findItemsBySkuCodesAndMarkForDeleteFalse(STORE_ID,
      itemCodes);
    verify(productItemService).removeDeletedProductItemImages(productItem1);
    verify(imageService).getDetachedProductItemImagesByStoreIdAndProductItemIds(STORE_ID,
      productItemIds);

  }

  @Test
  public void findProductItemImagesByItemCodesImageFetchTrueTest(){
    ProductItemImage productItemImage = new ProductItemImage();
    List<String> itemCodes = new ArrayList<>();
    List<String> productItemIds = new ArrayList<>();
    List<ProductItem> productItems = new ArrayList<>();
    productItem1.setProductId(PRODUCT_ID);
    productItems.add(productItem1);
    productItemIds.add(PRODUCT_ITEM_ID_1);
    itemCodes.add(ITEM_SKU_CODE_1);
    productItemImage.setProductItemId(PRODUCT_ITEM_ID_1);
    List<ProductItemImage> productItemImages = new ArrayList<>();
    productItemImages.add(productItemImage);
    Mockito.when(productItemService.findItemsBySkuCodesAndMarkForDeleteFalse(STORE_ID,
      itemCodes)).thenReturn(productItems);
    doNothing().when(productItemService).removeDeletedProductItemImages(productItem1);
    Mockito.when(imageService.getDetachedProductItemImagesByStoreIdAndProductItemIds(STORE_ID,
      productItemIds)).thenReturn(productItemImages);
    productItemServiceWrapper.findProductItemImagesByItemCodes(STORE_ID,
        itemCodes, false, true);
    verify(productItemService).findItemsBySkuCodesAndMarkForDeleteFalse(STORE_ID,
      itemCodes);
    verify(productItemService).removeDeletedProductItemImages(productItem1);
    verify(imageService).getDetachedProductItemImagesByStoreIdAndProductItemIds(STORE_ID,
      productItemIds);

  }


  @Test
  public void findProductItemImagesByItemCodesWithFetchImagesFalseTest(){
    ProductItemImage productItemImage = new ProductItemImage();
    List<String> itemCodes = new ArrayList<>();
    List<String> productItemIds = new ArrayList<>();
    List<ProductItem> productItems = new ArrayList<>();
    productItem1.setProductId(PRODUCT_ID);
    productItem1.setGeneratedItemName(PRODUCT_ITEM_NAME_1);
    productItems.add(productItem1);
    productItemIds.add(PRODUCT_ITEM_ID_1);
    itemCodes.add(ITEM_SKU_CODE_1);
    productItemImage.setProductItemId(PRODUCT_ITEM_ID_1);
    List<ProductItemImage> productItemImages = new ArrayList<>();
    productItemImages.add(productItemImage);
    Mockito.when(productItemService.findItemsBySkuCodesAndMarkForDeleteFalse(STORE_ID,
      itemCodes)).thenReturn(productItems);
    doNothing().when(productItemService).removeDeletedProductItemImages(productItem1);
    Mockito.when(imageService.getDetachedProductItemImagesByStoreIdAndProductItemIds(STORE_ID,
      productItemIds)).thenReturn(productItemImages);
    List<ItemImageResponse> productItemImagesByItemCodes =
      productItemServiceWrapper.findProductItemImagesByItemCodes(STORE_ID,
        itemCodes, false, false);
    verify(productItemService).findItemsBySkuCodesAndMarkForDeleteFalse(STORE_ID,
      itemCodes);
    Assertions.assertEquals(PRODUCT_ITEM_NAME_1, productItemImagesByItemCodes.get(0).getItemName());
  }

  @Test
  public void updateProductItemUpcCodeAndEvictCacheTest() throws Exception {
    ProductItemUpcCodeUpdateRequest productItemUpcCodeUpdateRequest =
      new ProductItemUpcCodeUpdateRequest();
    product.setId(PRODUCT_ID);
    productItemUpcCodeUpdateRequest.setUpcCode(UPC_CODE);
    productItemUpcCodeUpdateRequest.setSkuCode(ITEM_SKU_CODE_1);
    when(this.productServiceBean.getProductByStoreIdAndProductCodeCached(STORE_ID,
      PRODUCT_CODE)).thenReturn(product);
    productItemServiceWrapper.updateProductItemUpcCodeAndEvictCache(STORE_ID,
      Collections.singletonList(productItemUpcCodeUpdateRequest), PRODUCT_CODE);
    verify(productItemService).updateProductItemUpcCode(STORE_ID,
      Collections.singletonList(productItemUpcCodeUpdateRequest), PRODUCT_CODE);
    verify(productServiceBean).evictProductCache(STORE_ID, product);
    verify(domainEventPublisherService).publishProductForEdit(new ProductPublishUpdateDTO(product, false,
            new HashSet<>()), null, false, false, false, true, false);
    verify(applicationCacheServiceBean).evictProductItemsCacheByStoreIdAndProductId(STORE_ID,
      PRODUCT_ID);
    verify(productServiceBean).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void updateProductItemUpcCodeAndEvictCacheNullProductTest() throws Exception {
    ProductItemUpcCodeUpdateRequest productItemUpcCodeUpdateRequest =
      new ProductItemUpcCodeUpdateRequest();
    product.setId(PRODUCT_ID);
    productItemUpcCodeUpdateRequest.setUpcCode(UPC_CODE);
    productItemUpcCodeUpdateRequest.setSkuCode(ITEM_SKU_CODE_1);
    when(this.productServiceBean.getProductByStoreIdAndProductCodeCached(STORE_ID,
      PRODUCT_CODE)).thenReturn(null);
    productItemServiceWrapper.updateProductItemUpcCodeAndEvictCache(STORE_ID,
      Collections.singletonList(productItemUpcCodeUpdateRequest), PRODUCT_CODE);
    verify(productItemService).updateProductItemUpcCode(STORE_ID,
      Collections.singletonList(productItemUpcCodeUpdateRequest), PRODUCT_CODE);
    verify(productServiceBean).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void setCompleteProductItemDetailsFilterMarkForDeleteTest() {
    productItem1.setId(STORE_ID);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setProductItemId(PRODUCT_ITEM_ID_1);
    productItemImage1.setMarkForDelete(false);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setProductItemId(PRODUCT_ITEM_ID_2);
    productItemImage2.setMarkForDelete(true);
    List<ProductItemImage> productItemImageList = new ArrayList<>();
    productItemImageList.add(productItemImage1);
    productItemImageList.add(productItemImage2);
    ProductItem productItem1 = new ProductItem();
    productItem1.setProductItemImages(productItemImageList);
    product.setProductItems(Collections.singletonList(productItem1));
    List<ProductItem> productItems = new ArrayList<>(Arrays.asList(productItem1, productItem2));
    Mockito.when(productItemService.getProductItemImagesCached(STORE_ID, product)).thenReturn(productItemImageList);
    productItemServiceWrapper.setCompleteProductItemDetailsCached(STORE_ID, product, false);
    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productItemService).getProductItemImagesCached(STORE_ID, product);
    Mockito.verify(productItemService).getProductItemAttributeValuesWithAttributesCached(STORE_ID, product);
    for (ProductItem productItem : product.getProductItems()) {
      Mockito.verify(productItemService).removeDeletedProductItemImages(productItem);
    }
  }

  @Test
  public void setCompleteProductItemForProductItemImageIdNotExistsInOrginalMapTest() {
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setProductItemId(PRODUCT_ITEM_ID_1);
    productItemImage1.setMarkForDelete(false);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setProductItemId(PRODUCT_ITEM_ID_2);
    productItemImage2.setMarkForDelete(true);
    List<ProductItemImage> productItemImageList = new ArrayList<>();
    productItemImageList.add(productItemImage1);
    productItemImageList.add(productItemImage2);
    ProductItem productItem1 = new ProductItem();
    productItem1.setProductItemImages(productItemImageList);
    product.setProductItems(Collections.singletonList(productItem1));
    productItem1.setId(STORE_ID);
    List<ProductItem> productItems = new ArrayList<>(Arrays.asList(productItem1, productItem2));
    Mockito.when(productItemService.getProductItemImagesCached(STORE_ID, product)).thenReturn(productItemImageList);
    Mockito.when(productItemService.getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID))
        .thenReturn(productItems);
    productItemServiceWrapper.setCompleteProductItemDetailsCached(STORE_ID, product, false);
    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productItemService).getProductItemImagesCached(STORE_ID, product);
    Mockito.verify(productItemService).getProductItemAttributeValuesWithAttributesCached(STORE_ID, product);
    for (ProductItem productItem: product.getProductItems()) {
      Mockito.verify(productItemService).removeDeletedProductItemImages(productItem);
    }
  }

  @Test
  public void setCompleteProductItemDetailsCachedWithUomInfoRanchIntegrationDisabledTest() {
    ReflectionTestUtils.setField(productItemServiceWrapper, "ranchIntegrationEnabled", false);
    ReflectionTestUtils.setField(productItemServiceWrapper, "distributionSellerList", new HashSet<>());
    
    productItemServiceWrapper.setCompleteProductItemDetailsCached(STORE_ID, product, false);
    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productItemService).getProductItemImagesCached(STORE_ID, product);
    Mockito.verify(productItemService).getProductItemAttributeValuesWithAttributesCached(STORE_ID, product);
    for (ProductItem productItem: product.getProductItems()) {
      Mockito.verify(productItemService).removeDeletedProductItemImages(productItem);
    }
    Assertions.assertEquals(1, product.getProductItems().size());
    Assertions.assertEquals(PRODUCT_ITEM_ID_1, product.getProductItems().get(0).getId());
  }

  @Test
  public void setCompleteProductItemDetailsCachedWithUomInfoRanchIntegrationEnabledButSellerNotInDistributionListTest() {
    ReflectionTestUtils.setField(productItemServiceWrapper, "ranchIntegrationEnabled", true);
    Set<String> distributionSellerList = new HashSet<>();
    distributionSellerList.add("DISTRIBUTION_SELLER");
    ReflectionTestUtils.setField(productItemServiceWrapper, "distributionSellerList", distributionSellerList);
    
    product.setCreatedMerchant("NON_DISTRIBUTION_SELLER");
    productItemServiceWrapper.setCompleteProductItemDetailsCached(STORE_ID, product, false);
    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productItemService).getProductItemImagesCached(STORE_ID, product);
    Mockito.verify(productItemService).getProductItemAttributeValuesWithAttributesCached(STORE_ID, product);
    for (ProductItem productItem: product.getProductItems()) {
      Mockito.verify(productItemService).removeDeletedProductItemImages(productItem);
    }
    Assertions.assertEquals(1, product.getProductItems().size());
    Assertions.assertEquals(PRODUCT_ITEM_ID_1, product.getProductItems().get(0).getId());
  }

  @Test
  public void setCompleteProductItemDetailsCachedWithUomInfoRanchIntegrationEnabledAndSellerInDistributionListTest() {
    ReflectionTestUtils.setField(productItemServiceWrapper, "ranchIntegrationEnabled", true);
    Set<String> distributionSellerList = new HashSet<>();
    distributionSellerList.add("DISTRIBUTION_SELLER");
    ReflectionTestUtils.setField(productItemServiceWrapper, "distributionSellerList", distributionSellerList);
    
    product.setCreatedMerchant("DISTRIBUTION_SELLER");
    ProductItemUomInfo productItemUomInfo1 = new ProductItemUomInfo();
    productItemUomInfo1.setSkuCode(ITEM_SKU_CODE_1);
    ProductItemUomInfo productItemUomInfo2 = new ProductItemUomInfo();
    productItemUomInfo2.setSkuCode(ITEM_SKU_CODE_2);
    List<ProductItemUomInfo> productItemUomInfoList = Arrays.asList(productItemUomInfo1, productItemUomInfo2);
    
    Mockito.when(productItemUomInfoService.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productItemUomInfoList);
    
    productItemServiceWrapper.setCompleteProductItemDetailsCached(STORE_ID, product, false);
    
    Mockito.verify(productItemService).getProductItemsByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    Mockito.verify(productItemService).getProductItemImagesCached(STORE_ID, product);
    Mockito.verify(productItemService).getProductItemAttributeValuesWithAttributesCached(STORE_ID, product);
    Mockito.verify(productItemUomInfoService).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    
    for (ProductItem productItem: product.getProductItems()) {
      Mockito.verify(productItemService).removeDeletedProductItemImages(productItem);
    }
    
    Assertions.assertEquals(1, product.getProductItems().size());
    Assertions.assertEquals(PRODUCT_ITEM_ID_1, product.getProductItems().get(0).getId());
    Assertions.assertEquals(productItemUomInfo1, product.getProductItems().get(0).getProductItemUomInfo());
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productItemService);
    Mockito.verifyNoMoreInteractions(categoryService);
    Mockito.verifyNoMoreInteractions(productCategoryService);
    Mockito.verifyNoMoreInteractions(imageService);
    Mockito.verifyNoMoreInteractions(applicationCacheServiceBean, domainEventPublisherService,
      productServiceBean, productItemUomInfoService);
  }
}