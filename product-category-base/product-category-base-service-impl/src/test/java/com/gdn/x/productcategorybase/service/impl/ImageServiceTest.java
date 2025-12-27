package com.gdn.x.productcategorybase.service.impl;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verifyNoMoreInteractions;

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
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.dto.ActivateImageDTO;
import com.gdn.x.productcategorybase.dto.ProductActivateImageDTO;
import com.gdn.x.productcategorybase.dto.ProductImageDTO;
import com.gdn.x.productcategorybase.entity.ActivateImage;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductImageSingle;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.repository.ProductImageRepository;
import com.gdn.x.productcategorybase.repository.ProductItemImageRepository;
import com.gdn.x.productcategorybase.repository.ProductItemRepository;
import com.gdn.x.productcategorybase.repository.ProductRepository;
import com.gdn.x.productcategorybase.service.ProductItemService;
import com.gdn.x.productcategorybase.service.ProductItemServiceWrapper;
import com.gdn.x.productcategorybase.service.ProductService;

public class ImageServiceTest {

  private static final String STORE_ID = "STORE_ID";
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String LOCATION_PATH = "LOCATION_PATH";
  private static final String LOCATION_PATH_1 = "LOCATION_PATH-1";
  private static final String HASH_CODE = "HASH_CODE";
  private static final String NAME = "NAME";
  private static final String BRAND = "BRAND";
  private static final String UNIQUE_SELLING_POINT = "UNIQUE_SELLING_POINT";
  private static final String PRODUCT_ID = "87c66c40-64ac-443b-949a-3729077bef35";
  private static final String PRODUCT_ITEM_ID = "productItemId";
  private static final String PRODUCT_ITEM_ID_1 = "productItemId1";

  private Product product;
  private ProductImage productImage;
  private ProductItemImage productItemImage1;

  @Mock
  private ProductImageRepository productImageRepository;

  @Mock
  private ProductItemImageRepository productItemImageRepository;

  @Mock
  private ProductRepository productRepository;
  
  @Mock
  private ProductItemRepository productItemRepository;
  
  @Mock
  private ApplicationCacheServiceBean cacheService;

  @InjectMocks
  private ImageServiceBean imageServiceBean;

  @Mock
  private ProductService productService;

  @Mock
  private ProductItemService productItemService;

  @Mock
  private ProductItemServiceWrapper productItemServiceWrapper;

  @Captor
  private ArgumentCaptor<List<ProductImage>> productImagesCaptor;

  @Captor
  private ArgumentCaptor<List<ProductItemImage>> productItemImageCaptor;

  @Captor
  private ArgumentCaptor<String> stringArgumentCaptor;

  @BeforeEach
  public void initialize() {
    MockitoAnnotations.initMocks(this);
    
    Attribute attribute =
        new Attribute(ImageServiceTest.NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE, true, ImageServiceTest.STORE_ID);
    
    product =
        new Product.Builder().productCode(ImageServiceTest.PRODUCT_CODE + "001")
        .name(ImageServiceTest.NAME).length(90.0).width(80.0).height(15.0)
        .weight(5000.0).shippingWeight(5.0).brand(ImageServiceTest.BRAND)
        .uniqueSellingPoint(ImageServiceTest.UNIQUE_SELLING_POINT).uom(null)
        .storeId(ImageServiceTest.STORE_ID).build();
    product.setId(PRODUCT_ID);
    ProductItem productItem = new ProductItem();
    productItem.setId(PRODUCT_ITEM_ID);
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setHashCode(HASH_CODE);
    productItemImage.setProductItem(productItem);
    List<ProductItemImage> productItemImages = new ArrayList<>();
    productItemImages.add(productItemImage);
    productItem.setProductItemImages(productItemImages);
    product.getProductImages().add(new ProductImage());
    product.getProductImages().get(0).setHashCode(ImageServiceTest.HASH_CODE);
    product.getProductImages().get(0).setActive(false);
    product.getProductItems().add(productItem);
    product.getProductItems().get(0).getProductItemImages().add(new ProductItemImage());
    product.getProductItems().get(0).getProductItemImages().get(0)
        .setHashCode(ImageServiceTest.HASH_CODE);
    product.getProductItems().get(0).getProductItemImages().get(0).setProductItem(product.getProductItems().get(0));
    product.getProductItems().get(0).getProductItemImages().get(0).setActive(false);
    ProductItem productItem1 = new ProductItem();
    productItem1.setId(PRODUCT_ITEM_ID_1);
    product.getProductItems().add(productItem1);
    product.getProductItems().get(1).getProductItemImages().add(new ProductItemImage());
    product.getProductItems().get(1).getProductItemImages().get(0)
        .setHashCode(ImageServiceTest.HASH_CODE);
    product.getProductItems().get(1).getProductItemImages().get(0).setProductItem(product.getProductItems().get(1));
    product.getProductItems().get(1).getProductItemImages().get(0).setActive(false);
    
    product.getProductAttributes().add(new ProductAttribute());
    product.getProductAttributes().get(0).setAttribute(attribute);

    productImage = new ProductImage();
    productImage.setProductId(PRODUCT_ID);
    productImage.setLocationPath(LOCATION_PATH);

    productItemImage1 = new ProductItemImage();
    productItemImage1.setProductItemId(PRODUCT_ITEM_ID);
    productItemImage1.setLocationPath(LOCATION_PATH);

    Mockito.when(
        productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
            ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE)).thenReturn(product);
  }

  @AfterEach
  public void postTest() {
    verifyNoMoreInteractions(productImageRepository);
    verifyNoMoreInteractions(productItemImageRepository);
    verifyNoMoreInteractions(productRepository);
    verifyNoMoreInteractions(this.cacheService);
    verifyNoMoreInteractions(this.productItemService);
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(productItemServiceWrapper);
  }

  @Test
  public void testUpdateImageNameSuccessfully() throws Exception {
    product.getProductImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductImages().get(0).setActive(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(0).setActive(true);

    List<Object[]> productImages = new ArrayList<Object[]>();
    List<Object[]> productItemImages = new ArrayList<Object[]>();

    for (ProductImage productImage : product.getProductImages()) {
      Object[] image = {productImage.getLocationPath(),productImage.isActive()};
      productImages.add(image);
    }

    for (ProductItemImage productItemImage : product.getProductItems().get(0)
        .getProductItemImages()) {
      Object[] image = {productItemImage.getLocationPath(),productItemImage.isActive()};
      productItemImages.add(image);
    }

    Mockito.doNothing().when(productItemImageRepository)
        .updateProductItemImageName(ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.doNothing().when(productImageRepository)
        .updateProductImageName(ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.when(
        this.productRepository.getAllProductImagesByProductCode(ImageServiceTest.STORE_ID,
            ImageServiceTest.PRODUCT_CODE)).thenReturn(productImages);
    Mockito.when(
        this.productItemRepository.getAllProductItemImagesByProductCode(
            ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE))
        .thenReturn(productItemImages);

    imageServiceBean.updateImageName(ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE,
        ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);

    Mockito.verify(productItemImageRepository, Mockito.times(1)).updateProductItemImageName(
        ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.verify(productImageRepository, Mockito.times(1)).updateProductImageName(
        ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.verify(productRepository, Mockito.times(1)).getAllProductImagesByProductCode(
        ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);
    Mockito.verify(productItemRepository, Mockito.times(1))
        .getAllProductItemImagesByProductCode(ImageServiceTest.STORE_ID,
            ImageServiceTest.PRODUCT_CODE);
    Mockito.verify(productService, Mockito.times(1))
        .getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);
    Mockito.verify(productService).setCompleteProductDetailsCached(STORE_ID, product, false);
    Mockito.verify(cacheService).evictProductImagesCacheByStoreIdAndProductId(STORE_ID, product.getId());
    Mockito.verify(cacheService).evictProductItemImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void testUpdateImageNamePostLiveSuccessfully() throws Exception {
    product.getProductImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductImages().get(0).setActive(true);
    product.getProductImages().get(0).setOriginalImage(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(0).setActive(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setOriginalImage(true);
    product.getProductItems().get(0).getProductItemImages().get(1).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(1).setActive(true);
    product.getProductItems().get(0).getProductItemImages().get(1).setOriginalImage(true);

    List<Object[]> productImages = new ArrayList<Object[]>();
    List<Object[]> productItemImages = new ArrayList<Object[]>();

    for (ProductImage productImage : product.getProductImages()) {
      Object[] image = {productImage.getLocationPath(), productImage.isActive(), productImage.getOriginalImage()};
      productImages.add(image);
    }

    for (ProductItemImage productItemImage : product.getProductItems().get(0)
        .getProductItemImages()) {
      Object[] image =
          {productItemImage.getLocationPath(), productItemImage.isActive(), productItemImage.getOriginalImage()};
      productItemImages.add(image);
    }

    Mockito.doNothing().when(productItemImageRepository)
        .updateProductItemImageName(ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.doNothing().when(productImageRepository)
        .updateProductImageName(ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.when(
        productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
            ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE)).thenReturn(product);
    Mockito.when(
        this.productRepository.getAllProductImagesByProductCode(ImageServiceTest.STORE_ID,
            ImageServiceTest.PRODUCT_CODE)).thenReturn(productImages);
    Mockito.when(
        this.productItemRepository.getAllProductItemImagesByProductCode(
            ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE))
        .thenReturn(productItemImages);

    ActivateImage activateImage = imageServiceBean
        .updateImageName(ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE, ImageServiceTest.LOCATION_PATH,
            ImageServiceTest.HASH_CODE);

    Mockito.verify(productItemImageRepository, Mockito.times(1)).updateProductItemImageName(
        ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.verify(productImageRepository, Mockito.times(1)).updateProductImageName(
        ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.verify(productRepository, Mockito.times(1)).getAllProductImagesByProductCode(
        ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);
    Mockito.verify(productItemRepository, Mockito.times(1))
        .getAllProductItemImagesByProductCode(ImageServiceTest.STORE_ID,
            ImageServiceTest.PRODUCT_CODE);
    Mockito.verify(productService, Mockito.times(1))
        .getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);
    Mockito.verify(productService).setCompleteProductDetailsCached(STORE_ID, product, false);
    Mockito.verify(cacheService).evictProductImagesCacheByStoreIdAndProductId(STORE_ID, product.getId());
    Mockito.verify(cacheService).evictProductItemImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Assertions.assertEquals(true, activateImage.isActive());
    Assertions.assertEquals(1, activateImage.getFilenames().size());
  }

  @Test
  public void testUpdateImageNameFailedProductImageFalse() throws Exception {
    product.getProductImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductImages().get(0).setActive(false);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(0).setActive(true);

    List<Object[]> productImages = new ArrayList<Object[]>();
    List<Object[]> productItemImages = new ArrayList<Object[]>();

    for (ProductImage productImage : product.getProductImages()) {
      Object[] image = {productImage.getLocationPath(),productImage.isActive()};
      productImages.add(image);
    }

    for (ProductItemImage productItemImage : product.getProductItems().get(0)
        .getProductItemImages()) {
      Object[] image = {productItemImage.getLocationPath(),productItemImage.isActive()};
      productItemImages.add(image);
    }

    Mockito.doNothing().when(productItemImageRepository)
        .updateProductItemImageName(ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.doNothing().when(productImageRepository)
        .updateProductImageName(ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.when(
        this.productRepository.getAllProductImagesByProductCode(ImageServiceTest.STORE_ID,
            ImageServiceTest.PRODUCT_CODE)).thenReturn(productImages);
    Mockito.when(
        this.productItemRepository.getAllProductItemImagesByProductCode(
            ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE))
        .thenReturn(productItemImages);
    Mockito.when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    imageServiceBean.updateImageName(ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE,
          ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);

    Mockito.verify(productItemImageRepository, Mockito.times(1)).updateProductItemImageName(
        ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.verify(productImageRepository, Mockito.times(1)).updateProductImageName(
        ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.verify(productRepository, Mockito.times(1)).getAllProductImagesByProductCode(
        ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);
    Mockito.verify(productItemRepository, Mockito.times(1))
        .getAllProductItemImagesByProductCode(ImageServiceTest.STORE_ID,
            ImageServiceTest.PRODUCT_CODE);
    Mockito.verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).setCompleteProductDetailsCached(STORE_ID, product, false);
    Mockito.verify(this.cacheService).evictProductImagesCacheByStoreIdAndProductId(STORE_ID, product.getId());
    Mockito.verify(cacheService).evictProductItemImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }
  
  @Test
  public void testUpdateImageNameFailedProductItemImageFalse() throws Exception {
    product.getProductImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductImages().get(0).setActive(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(0).setActive(false);

    List<Object[]> productImages = new ArrayList<Object[]>();
    List<Object[]> productItemImages = new ArrayList<Object[]>();

    for (ProductImage productImage : product.getProductImages()) {
      Object[] image = {productImage.getLocationPath(),productImage.isActive()};
      productImages.add(image);
    }

    for (ProductItemImage productItemImage : product.getProductItems().get(0)
        .getProductItemImages()) {
      Object[] image = {productItemImage.getLocationPath(),productItemImage.isActive()};
      productItemImages.add(image);
    }

    Mockito.doNothing().when(productItemImageRepository)
        .updateProductItemImageName(ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.doNothing().when(productImageRepository)
        .updateProductImageName(ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.when(
        productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(
            ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE)).thenReturn(product);
    Mockito.when(
        this.productRepository.getAllProductImagesByProductCode(ImageServiceTest.STORE_ID,
            ImageServiceTest.PRODUCT_CODE)).thenReturn(productImages);
    Mockito.when(
        this.productItemRepository.getAllProductItemImagesByProductCode(
            ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE))
        .thenReturn(productItemImages);
    Mockito.when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
        STORE_ID, PRODUCT_CODE)).thenReturn(product);
    imageServiceBean.updateImageName(ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE,
        ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);

    Mockito.verify(productItemImageRepository, Mockito.times(1)).updateProductItemImageName(
        ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.verify(productImageRepository, Mockito.times(1)).updateProductImageName(
        ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.verify(productRepository, Mockito.times(1)).getAllProductImagesByProductCode(
        ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);
    Mockito.verify(productItemRepository, Mockito.times(1))
        .getAllProductItemImagesByProductCode(ImageServiceTest.STORE_ID,
            ImageServiceTest.PRODUCT_CODE);
    Mockito.verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).setCompleteProductDetailsCached(STORE_ID, product, false);
    Mockito.verify(this.cacheService).evictProductImagesCacheByStoreIdAndProductId(STORE_ID, product.getId());
    Mockito.verify(cacheService).evictProductItemImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void testUpdateImageNameFailProductImage() throws Exception {
    Mockito.doNothing().when(productItemImageRepository)
        .updateProductItemImageName(ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.doNothing().when(productImageRepository)
        .updateProductImageName(ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.when(
        productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(
            ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE)).thenReturn(product);
    Mockito.when(
        this.productRepository.getAllProductImagesByProductCode(ImageServiceTest.STORE_ID,
            ImageServiceTest.PRODUCT_CODE)).thenReturn(null);
    Mockito.when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
        STORE_ID, PRODUCT_CODE)).thenReturn(product);

    imageServiceBean.updateImageName(ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE,
        ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);

    Mockito.verify(productItemImageRepository, Mockito.times(1)).updateProductItemImageName(
        ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.verify(productImageRepository, Mockito.times(1)).updateProductImageName(
        ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.verify(productRepository, Mockito.times(1)).getAllProductImagesByProductCode(
        ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);
    Mockito.verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).setCompleteProductDetailsCached(STORE_ID, product, false);
    Mockito.verify(this.cacheService).evictProductImagesCacheByStoreIdAndProductId(STORE_ID, product.getId());
    Mockito.verify(cacheService).evictProductItemImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void testUpdateImageNameFailProductItemImage() throws Exception {
    product.getProductImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductImages().get(0).setActive(true);

    List<Object[]> productImages = new ArrayList<Object[]>();
    List<Object[]> productItemImages = new ArrayList<Object[]>();

    for (ProductImage productImage : product.getProductImages()) {
      Object[] image = {productImage.getLocationPath(),productImage.isActive()};
      productImages.add(image);
    }

    Mockito.doNothing().when(productItemImageRepository)
        .updateProductItemImageName(ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.doNothing().when(productImageRepository)
        .updateProductImageName(ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.when(
        productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(
            ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE)).thenReturn(product);
    Mockito.when(
        this.productRepository.getAllProductImagesByProductCode(ImageServiceTest.STORE_ID,
            ImageServiceTest.PRODUCT_CODE)).thenReturn(productImages);
    Mockito.when(
        this.productItemRepository.getAllProductItemImagesByProductCode(
            ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE))
        .thenReturn(productItemImages);
    Mockito.when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
        STORE_ID, PRODUCT_CODE)).thenReturn(product);

    imageServiceBean.updateImageName(ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE,
        ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);

    Mockito.verify(productItemImageRepository, Mockito.times(1)).updateProductItemImageName(
        ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.verify(productImageRepository, Mockito.times(1)).updateProductImageName(
        ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.verify(productRepository, Mockito.times(1)).getAllProductImagesByProductCode(
        ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);
    Mockito.verify(productItemRepository, Mockito.times(1))
        .getAllProductItemImagesByProductCode(ImageServiceTest.STORE_ID,
            ImageServiceTest.PRODUCT_CODE);
    Mockito.verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).setCompleteProductDetailsCached(STORE_ID, product, false);
    Mockito.verify(this.cacheService).evictProductImagesCacheByStoreIdAndProductId(STORE_ID, product.getId());
    Mockito.verify(cacheService).evictProductItemImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }
  
  @Test
  public void testActivateAndUpdateImageName() {
    Mockito.doNothing().when(productItemImageRepository)
        .updateProductItemImageName(ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.doNothing().when(productImageRepository)
        .updateProductImageName(ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.when(productItemService.getProductItemsByStoreIdAndProductIdCached(STORE_ID, product.getId()))
        .thenReturn(product.getProductItems());
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    imageServiceBean.activateAndUpdateImageName(ImageServiceTest.STORE_ID,
        ImageServiceTest.PRODUCT_CODE, ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.verify(productItemImageRepository, Mockito.times(1)).updateProductItemImageName(
        ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.verify(productImageRepository, Mockito.times(1)).updateProductImageName(
        ImageServiceTest.LOCATION_PATH, ImageServiceTest.HASH_CODE);
    Mockito.verify(productService).getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.cacheService).evictProductImagesCacheByStoreIdAndProductId(STORE_ID, product.getId());
    Mockito.verify(cacheService).evictProductItemImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }
  
  @Test
  public void testIsProductImagesActivatedSuccessfully() throws Exception {
    product.getProductImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductImages().get(0).setActive(true);
    product.getProductImages().get(0).setOriginalImage(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(0).setActive(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setOriginalImage(true);
    product.getProductItems().get(0).getProductItemImages().get(1).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(1).setActive(true);
    product.getProductItems().get(0).getProductItemImages().get(1).setOriginalImage(true);

    List<Object[]> productImages = new ArrayList<Object[]>();
    List<Object[]> productItemImages = new ArrayList<Object[]>();

    for (ProductImage productImage : product.getProductImages()) {
      Object[] image = {productImage.getLocationPath(), productImage.isActive(), productImage.getOriginalImage()};
      productImages.add(image);
    }

    for (ProductItemImage productItemImage : product.getProductItems().get(0)
        .getProductItemImages()) {
      Object[] image =
          {productItemImage.getLocationPath(), productItemImage.isActive(), productItemImage.getOriginalImage()};
      productItemImages.add(image);
    }

    Mockito.when(
        this.productRepository.getAllProductImagesByProductCode(ImageServiceTest.STORE_ID,
            ImageServiceTest.PRODUCT_CODE)).thenReturn(productImages);
    Mockito.when(
        this.productItemRepository.getAllProductItemImagesByProductCode(
            ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE))
        .thenReturn(productItemImages);

    imageServiceBean.isProductImagesActivated(ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);

    Mockito.verify(productRepository, Mockito.times(1)).getAllProductImagesByProductCode(
        ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);
    Mockito.verify(productItemRepository, Mockito.times(1))
    .getAllProductItemImagesByProductCode(ImageServiceTest.STORE_ID,
        ImageServiceTest.PRODUCT_CODE);
  }
  
  @Test
  public void testIsProductImagesActivatedEmptyProductImagesSuccessfully() throws Exception {
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(0).setActive(true);

    List<Object[]> productImages = new ArrayList<Object[]>();
    List<Object[]> productItemImages = new ArrayList<Object[]>();

    for (ProductItemImage productItemImage : product.getProductItems().get(0)
        .getProductItemImages()) {
      Object[] image = {productItemImage.getLocationPath(),productItemImage.isActive()};
      productItemImages.add(image);
    }

    Mockito.when(
        this.productRepository.getAllProductImagesByProductCode(ImageServiceTest.STORE_ID,
            ImageServiceTest.PRODUCT_CODE)).thenReturn(productImages);
    Mockito.when(
        this.productItemRepository.getAllProductItemImagesByProductCode(
            ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE))
        .thenReturn(productItemImages);

    imageServiceBean.isProductImagesActivated(ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);

    Mockito.verify(productRepository, Mockito.times(1)).getAllProductImagesByProductCode(
        ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);
    Mockito.verify(productItemRepository, Mockito.times(1))
    .getAllProductItemImagesByProductCode(ImageServiceTest.STORE_ID,
        ImageServiceTest.PRODUCT_CODE);
  }
  
  @Test
  public void testIsProductImagesActivatedEmptyProductItemImagesSuccessfully() throws Exception {
    product.getProductImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductImages().get(0).setActive(true);

    List<Object[]> productImages = new ArrayList<Object[]>();
    List<Object[]> productItemImages = new ArrayList<Object[]>();

    for (ProductImage productImage : product.getProductImages()) {
      Object[] image = {productImage.getLocationPath(), productImage.isActive()};
      Object[] image1 = {productImage.getLocationPath(), false, productImage.isActive()};
      productImages.add(image);
      productImages.add(image1);
    }

    Mockito.when(
        this.productRepository.getAllProductImagesByProductCode(ImageServiceTest.STORE_ID,
            ImageServiceTest.PRODUCT_CODE)).thenReturn(productImages);
    Mockito.when(
        this.productItemRepository.getAllProductItemImagesByProductCode(ImageServiceTest.STORE_ID,
            ImageServiceTest.PRODUCT_CODE)).thenReturn(productItemImages);

    imageServiceBean.isProductImagesActivated(ImageServiceTest.STORE_ID,
        ImageServiceTest.PRODUCT_CODE);

    Mockito.verify(productRepository, Mockito.times(1)).getAllProductImagesByProductCode(
        ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);
    Mockito.verify(productItemRepository, Mockito.times(1)).getAllProductItemImagesByProductCode(
        ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);
  }
  
  @Test
  public void testIsProductImagesActivatedFalseSuccessfully() throws Exception {
    product.getProductImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductImages().get(0).setActive(false);
    product.getProductImages().get(0).setOriginalImage(false);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(0).setActive(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setOriginalImage(false);

    List<Object[]> productImages = new ArrayList<Object[]>();
    List<Object[]> productItemImages = new ArrayList<Object[]>();

    for (ProductImage productImage : product.getProductImages()) {
      Object[] image = {productImage.getLocationPath(), productImage.isActive(), productImage.getOriginalImage()};
      productImages.add(image);
    }

    for (ProductItemImage productItemImage : product.getProductItems().get(0)
        .getProductItemImages()) {
      Object[] image =
          {productItemImage.getLocationPath(), productItemImage.isActive(), productItemImage.getOriginalImage()};
      Object[] image1 =
          {productItemImage.getLocationPath(), false, productItemImage.getOriginalImage()};
      productItemImages.add(image);
      productItemImages.add(image1);
    }

    Mockito.when(
        this.productRepository.getAllProductImagesByProductCode(ImageServiceTest.STORE_ID,
            ImageServiceTest.PRODUCT_CODE)).thenReturn(productImages);
    Mockito.when(
        this.productItemRepository.getAllProductItemImagesByProductCode(
            ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE))
        .thenReturn(productItemImages);

    ActivateImage productImagesActivated =
        imageServiceBean.isProductImagesActivated(ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);

    Mockito.verify(productRepository, Mockito.times(1)).getAllProductImagesByProductCode(
        ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);
    Mockito.verify(productItemRepository, Mockito.times(1))
    .getAllProductItemImagesByProductCode(ImageServiceTest.STORE_ID,
        ImageServiceTest.PRODUCT_CODE);
    Assertions.assertEquals(false, productImagesActivated.isActive());
    Assertions.assertEquals(0, productImagesActivated.getFilenames().size());
  }

  @Test
  public void testIsProductImagesActivatedPostLiveProductSuccessfully() throws Exception {
    product.getProductImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductImages().get(0).setActive(false);
    product.getProductImages().get(0).setOriginalImage(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(0).setActive(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setOriginalImage(true);
    product.getProductItems().get(0).getProductItemImages().get(1).setOriginalImage(true);
    product.getProductItems().get(0).getProductItemImages().get(1).setLocationPath(LOCATION_PATH);

    List<Object[]> productImages = new ArrayList<>();
    List<Object[]> productItemImages = new ArrayList<>();

    for (ProductImage productImage : product.getProductImages()) {
      Object[] image = {productImage.getLocationPath(), productImage.isActive(), productImage.getOriginalImage()};
      productImages.add(image);
    }

    for (ProductItemImage productItemImage : product.getProductItems().get(0).getProductItemImages()) {
      Object[] image =
          {productItemImage.getLocationPath(), productItemImage.isActive(), productItemImage.getOriginalImage()};
      productItemImages.add(image);
    }

    Mockito.doReturn(productImages).when(this.productRepository)
        .getAllProductImagesByProductCode(ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);
    Mockito.doReturn(productItemImages).when(this.productItemRepository)
        .getAllProductItemImagesByProductCode(ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);

    ActivateImage productImagesActivated =
        imageServiceBean.isProductImagesActivated(ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);

    Mockito.verify(productRepository, Mockito.times(1))
        .getAllProductImagesByProductCode(ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);
    Mockito.verify(productItemRepository, Mockito.times(1))
        .getAllProductItemImagesByProductCode(ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);
    Assertions.assertEquals(true, productImagesActivated.isActive());
    Assertions.assertEquals(1, productImagesActivated.getFilenames().size());
  }
  
  @Test
  public void testIsProductItemImagesActivatedFalseSuccessfully() throws Exception {
    product.getProductImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductImages().get(0).setActive(true);
    product.getProductImages().get(0).setOriginalImage(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(0).setActive(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setOriginalImage(true);
    product.getProductItems().get(0).getProductItemImages().get(1).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(1).setActive(true);
    product.getProductItems().get(0).getProductItemImages().get(1).setOriginalImage(true);

    List<Object[]> productImages = new ArrayList<Object[]>();
    List<Object[]> productItemImages = new ArrayList<Object[]>();

    for (ProductImage productImage : product.getProductImages()) {
      Object[] image = {productImage.getLocationPath(), productImage.isActive(), productImage.getOriginalImage()};
      productImages.add(image);
    }

    for (ProductItemImage productItemImage : product.getProductItems().get(0)
        .getProductItemImages()) {
      Object[] image =
          {productItemImage.getLocationPath(), productItemImage.isActive(), productItemImage.getOriginalImage()};
      productItemImages.add(image);
    }

    Mockito.when(
        this.productRepository.getAllProductImagesByProductCode(ImageServiceTest.STORE_ID,
            ImageServiceTest.PRODUCT_CODE)).thenReturn(productImages);
    Mockito.when(
        this.productItemRepository.getAllProductItemImagesByProductCode(
            ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE))
        .thenReturn(productItemImages);

    imageServiceBean.isProductImagesActivated(ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);

    Mockito.verify(productRepository, Mockito.times(1)).getAllProductImagesByProductCode(
        ImageServiceTest.STORE_ID, ImageServiceTest.PRODUCT_CODE);
    Mockito.verify(productItemRepository, Mockito.times(1))
    .getAllProductItemImagesByProductCode(ImageServiceTest.STORE_ID,
        ImageServiceTest.PRODUCT_CODE);
  }

  @Test
  public void testActivateAndUpdateImagesName() throws Exception {
    ProductActivateImageDTO productActivateImageDTO = generateProductActivationDTO();
    ProductImage productImage = new ProductImage();
    productImage.setOriginalImage(false);
    product.getProductItems().get(0).getProductItemImages().get(0).setHashCode("hash1");
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(1).getProductItemImages().get(0).setLocationPath(LOCATION_PATH_1);
    product.getProductImages().add(productImage);
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    Mockito.when(productImageRepository.saveAll(Mockito.anyList()))
        .thenReturn(product.getProductImages());
    Mockito.when(productItemImageRepository.saveAll(Mockito.anyList()))
        .thenReturn(product.getProductItems().get(0).getProductItemImages());
    Mockito.doNothing().when(productImageRepository).flush();
    Mockito.doNothing().when(productItemImageRepository).flush();
    this.imageServiceBean.activateAndUpdateImagesName(STORE_ID, productActivateImageDTO, false);
    Mockito.verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).setProductImagesCached(STORE_ID, product, false);
    Mockito.verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    Mockito.verify(productImageRepository).saveAll(Mockito.anyList());
    Mockito.verify(productItemImageRepository).saveAll(Mockito.anyList());
    Mockito.verify(productImageRepository).flush();
    Mockito.verify(productItemImageRepository).flush();
  }

  @Test
  public void testActivateAndUpdateImagesNameCommonImageTrueTest() throws Exception {
    ProductActivateImageDTO productActivateImageDTO = generateProductActivationDTO();
    ProductImage productImage = new ProductImage();
    productImage.setOriginalImage(false);
    product.getProductItems().get(0).getProductItemImages().get(0).setHashCode("hash1");
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(1).getProductItemImages().get(0).setLocationPath(LOCATION_PATH_1);
    product.getProductImages().add(productImage);
    product.getProductImages().get(0).setCommonImage(true);
    product.getProductImages().get(1).setCommonImage(true);
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    Mockito.when(productImageRepository.saveAll(Mockito.anyList()))
        .thenReturn(product.getProductImages());
    Mockito.when(productItemImageRepository.saveAll(Mockito.anyList()))
        .thenReturn(product.getProductItems().get(0).getProductItemImages());
    Mockito.doNothing().when(productImageRepository).flush();
    Mockito.doNothing().when(productItemImageRepository).flush();
    this.imageServiceBean.activateAndUpdateImagesName(STORE_ID, productActivateImageDTO, false);
    Mockito.verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).setProductImagesCached(STORE_ID, product, false);
    Mockito.verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    Mockito.verify(productImageRepository).saveAll(Mockito.anyList());
    Mockito.verify(productItemImageRepository).saveAll(Mockito.anyList());
    Mockito.verify(productImageRepository).flush();
    Mockito.verify(productItemImageRepository).flush();
  }

  @Test
  public void testActivateAndUpdateImagesNameWithOriginalImageTrue() throws Exception {
    ProductActivateImageDTO productActivateImageDTO = generateProductActivationDTO();
    product.getProductImages().get(0).setOriginalImage(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setOriginalImage(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(1).getProductItemImages().get(0).setLocationPath(LOCATION_PATH_1);
    Mockito.when(productImageRepository.saveAll(Mockito.anyList()))
        .thenReturn(product.getProductImages());
    for(ProductItemImage itemImage : product.getProductItems().get(0).getProductItemImages()) {
      itemImage.setProductItem(product.getProductItems().get(0));
    }
    Mockito.when(productItemImageRepository.saveAll(Mockito.anyList()))
        .thenReturn(product.getProductItems().get(0).getProductItemImages());
    Mockito.doNothing().when(productImageRepository).flush();
    Mockito.doNothing().when(productItemImageRepository).flush();
    this.imageServiceBean.activateAndUpdateImagesName(STORE_ID, productActivateImageDTO, false);
    Mockito.verify(productImageRepository).saveAll(productImagesCaptor.capture());
    Mockito.verify(productItemImageRepository).saveAll(productItemImageCaptor.capture());
    Mockito.verify(productImageRepository).flush();
    Mockito.verify(productItemImageRepository).flush();
    Mockito.verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).setProductImagesCached(STORE_ID, product, false);
    Mockito.verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    Assertions.assertFalse(productImagesCaptor.getValue().get(0).isActive());
  }

  @Test
  public void testActivateAndUpdateImagesNameWithOriginalImageTrueSkipReviewTrue() throws Exception {
    ProductActivateImageDTO productActivateImageDTO = generateProductActivationDTO();
    product.getProductImages().get(0).setOriginalImage(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setOriginalImage(true);
    product.getProductItems().get(0).getProductItemImages().remove(1);
    for(ProductItemImage itemImage : product.getProductItems().get(0).getProductItemImages()) {
      itemImage.setProductItem(product.getProductItems().get(0));
    }
    Mockito.when(productImageRepository.saveAll(Mockito.anyList()))
        .thenReturn(product.getProductImages());
    Mockito.when(productItemImageRepository.saveAll(Mockito.anyList()))
        .thenReturn(product.getProductItems().get(0).getProductItemImages());
    Mockito.doNothing().when(productImageRepository).flush();
    Mockito.doNothing().when(productItemImageRepository).flush();
    this.imageServiceBean.activateAndUpdateImagesName(STORE_ID, productActivateImageDTO, true);
    Mockito.verify(productImageRepository).saveAll(productImagesCaptor.capture());
    Mockito.verify(productItemImageRepository).saveAll(productItemImageCaptor.capture());
    Mockito.verify(productImageRepository).flush();
    Mockito.verify(productItemImageRepository).flush();
    Mockito.verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).setProductImagesCached(STORE_ID, product, false);
    Mockito.verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    Assertions.assertTrue(productImagesCaptor.getValue().get(0).isMarkForDelete());
    Assertions.assertTrue(productImagesCaptor.getValue().get(1).isActive());
    Assertions.assertFalse(productImagesCaptor.getValue().get(1).isRevised());
  }

  @Test
  public void testActivateAndUpdateImagesNameWithOriginalImageFalse() throws Exception {
    ProductActivateImageDTO productActivateImageDTO = generateProductActivationDTO();
    product.getProductImages().get(0).setOriginalImage(false);
    product.getProductItems().get(0).getProductItemImages().get(0).setOriginalImage(false);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(1).getProductItemImages().get(0).setLocationPath(LOCATION_PATH_1);
    Mockito.when(productImageRepository.saveAll(Mockito.anyList()))
        .thenReturn(product.getProductImages());
    Mockito.when(productItemImageRepository.saveAll(Mockito.anyList()))
        .thenReturn(product.getProductItems().get(0).getProductItemImages());
    Mockito.doNothing().when(productImageRepository).flush();
    Mockito.doNothing().when(productItemImageRepository).flush();
    this.imageServiceBean.activateAndUpdateImagesName(STORE_ID, productActivateImageDTO, false);
    Mockito.verify(productImageRepository).saveAll(productImagesCaptor.capture());
    Mockito.verify(productItemImageRepository).saveAll(productItemImageCaptor.capture());
    Mockito.verify(productImageRepository).flush();
    Mockito.verify(productItemImageRepository).flush();
    Mockito.verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).setProductImagesCached(STORE_ID, product, false);
    Mockito.verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    Assertions.assertTrue(productImagesCaptor.getValue().get(0).isActive());
  }

  @Test
  public void testActivateAndUpdateImagesNameWithOriginalImageActiveTrueFalse() throws Exception {
    ProductActivateImageDTO productActivateImageDTO = generateProductActivationDTO();
    product.getProductImages().get(0).setOriginalImage(false);
    product.getProductItems().get(0).getProductItemImages().get(0).setOriginalImage(false);
    product.getProductItems().get(0).getProductItemImages().get(0).setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).getProductItemImages().get(0).setActive(true);
    product.getProductItems().get(1).getProductItemImages().get(0).setLocationPath(LOCATION_PATH_1);
    product.getProductItems().get(1).getProductItemImages().get(0).setActive(true);
    Mockito.when(productImageRepository.saveAll(Mockito.anyList()))
        .thenReturn(product.getProductImages());
    Mockito.when(productItemImageRepository.saveAll(Mockito.anyList()))
        .thenReturn(product.getProductItems().get(0).getProductItemImages());
    Mockito.doNothing().when(productImageRepository).flush();
    Mockito.doNothing().when(productItemImageRepository).flush();
    this.imageServiceBean.activateAndUpdateImagesName(STORE_ID, productActivateImageDTO, false);
    Mockito.verify(productImageRepository).saveAll(productImagesCaptor.capture());
    Mockito.verify(productItemImageRepository).saveAll(productItemImageCaptor.capture());
    Mockito.verify(productImageRepository).flush();
    Mockito.verify(productItemImageRepository).flush();
    Mockito.verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).setProductImagesCached(STORE_ID, product, false);
    Mockito.verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    Assertions.assertTrue(productImagesCaptor.getValue().get(0).isActive());
  }

  @Test
  public void testActivateAndUpdateImagesNameWithOriginalImageFalseEmptyImagePathMap() throws Exception {
    ProductActivateImageDTO productActivateImageDTO = generateProductActivationDTO();
    product.getProductImages().get(0).setOriginalImage(false);
    product.getProductItems().get(0).getProductItemImages().get(0).setOriginalImage(false);
    productActivateImageDTO.setImageRequests(new HashSet<>());
    Mockito.when(productImageRepository.saveAll(Mockito.anyList()))
        .thenReturn(product.getProductImages());
    Mockito.when(productItemImageRepository.saveAll(Mockito.anyList()))
        .thenReturn(product.getProductItems().get(0).getProductItemImages());
    Mockito.doNothing().when(productImageRepository).flush();
    Mockito.doNothing().when(productItemImageRepository).flush();
    this.imageServiceBean.activateAndUpdateImagesName(STORE_ID, productActivateImageDTO, false);
    Mockito.verify(productImageRepository).saveAll(productImagesCaptor.capture());
    Mockito.verify(productItemImageRepository).saveAll(productItemImageCaptor.capture());
    Mockito.verify(productImageRepository).flush();
    Mockito.verify(productItemImageRepository).flush();
    Mockito.verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).setProductImagesCached(STORE_ID, product, false);
    Mockito.verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
  }

  @Test
  public void testActivateAndUpdateImagesNameWithSkipReviewTrue() throws Exception {
    ProductActivateImageDTO productActivateImageDTO = generateProductActivationDTO();
    Mockito.when(productImageRepository.saveAll(Mockito.anyList()))
        .thenReturn(product.getProductImages());
    product.getProductItems().get(0).getProductItemImages().remove(1);
    for(ProductItemImage itemImage : product.getProductItems().get(0).getProductItemImages()) {
      itemImage.setProductItem(product.getProductItems().get(0));
    }
    Mockito.when(productItemImageRepository.saveAll(Mockito.anyList()))
        .thenReturn(product.getProductItems().get(0).getProductItemImages());
    Mockito.doNothing().when(productImageRepository).flush();
    Mockito.doNothing().when(productItemImageRepository).flush();
    this.imageServiceBean.activateAndUpdateImagesName(STORE_ID, productActivateImageDTO, true);
    Mockito.verify(productImageRepository).saveAll(productImagesCaptor.capture());
    Mockito.verify(productItemImageRepository).saveAll(productItemImageCaptor.capture());
    Mockito.verify(productImageRepository).flush();
    Mockito.verify(productItemImageRepository).flush();
    Mockito.verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).setProductImagesCached(STORE_ID, product, false);
    Mockito.verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    Assertions.assertTrue(productImagesCaptor.getValue().get(0).isMarkForDelete());
    Assertions.assertTrue(productImagesCaptor.getValue().get(1).isActive());
  }

  @Test
  public void testActivateAndUpdateImagesNameWithSkipReviewTrueItemImageUpdatedTest() throws Exception {
    ProductActivateImageDTO productActivateImageDTO = generateProductActivationDTO();
    for (ActivateImageDTO activateImageDTO : productActivateImageDTO.getImageRequests()) {
      activateImageDTO.setCommonImage(false);
    }

    Mockito.when(productImageRepository.saveAll(Mockito.anyList()))
        .thenReturn(product.getProductImages());
    product.getProductItems().get(0).getProductItemImages().remove(1);
    for(ProductItemImage itemImage : product.getProductItems().get(0).getProductItemImages()) {
      itemImage.setProductItem(product.getProductItems().get(0));
      itemImage.setCommonImage(true);
    }

    Mockito.when(
            this.productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE))
        .thenReturn(product);

    Mockito.when(productItemImageRepository.saveAll(Mockito.anyList()))
        .thenReturn(product.getProductItems().get(0).getProductItemImages());
    Mockito.doNothing().when(productImageRepository).flush();
    Mockito.doNothing().when(productItemImageRepository).flush();
    this.imageServiceBean.activateAndUpdateImagesName(STORE_ID, productActivateImageDTO, true);
    Mockito.verify(productImageRepository).saveAll(productImagesCaptor.capture());
    Mockito.verify(productItemImageRepository).saveAll(productItemImageCaptor.capture());
    Mockito.verify(productImageRepository).flush();
    Mockito.verify(productItemImageRepository).flush();
    Mockito.verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).setProductImagesCached(STORE_ID, product, false);
    Mockito.verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
    Assertions.assertTrue(productImagesCaptor.getValue().get(0).isMarkForDelete());
    Assertions.assertTrue(productImagesCaptor.getValue().get(1).isActive());
  }

  @Test
  public void testActivateAndUpdateImagesName_withoutImagesInformationRequest() throws Exception {
    ProductActivateImageDTO productActivateImageDTO = generateProductActivationDTO();
    product.setProductImages(new ArrayList<>());
    product.getProductItems().get(0).setProductItemImages(new ArrayList<>());
    product.getProductItems().get(1).setProductItemImages(new ArrayList<>());
    this.imageServiceBean.activateAndUpdateImagesName(STORE_ID, productActivateImageDTO, false);
    Mockito.verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).setProductImagesCached(STORE_ID, product, false);
    Mockito.verify(productItemServiceWrapper).setProductItemsWithProductItemImagesCached(STORE_ID, product, false);
  }

  @Test
  public void findProductAndItemLocationPathsByProductCodeListAndStoreIdTest() throws Exception {
    List<String> productCodeList = new ArrayList<>();
    productCodeList.add(PRODUCT_CODE);
    List<String> locationPathList = new ArrayList<>();
    locationPathList.add(LOCATION_PATH);
    Mockito.when(this.productImageRepository.findProductLocationPathByProductCodeAndStoreId(productCodeList, STORE_ID))
        .thenReturn(locationPathList);
    Mockito.when(
        this.productItemImageRepository.findProductItemImageLocationPathsByProductCodeListAndStoreId(productCodeList,
            STORE_ID)).thenReturn(locationPathList);
    this.imageServiceBean.findProductAndItemLocationPathsByProductCodeListAndStoreId(productCodeList, STORE_ID);
    Mockito.verify(this.productImageRepository)
        .findProductLocationPathByProductCodeAndStoreId(productCodeList, STORE_ID);
    Mockito.verify(this.productItemImageRepository)
        .findProductItemImageLocationPathsByProductCodeListAndStoreId(productCodeList, STORE_ID);
  }
  
  @Test
  public void filterProductImagesByProductIds_HappyFlow_Success() {
    List<String> request = Arrays.asList(PRODUCT_ID);
    Mockito.when(this.productImageRepository.findProductImagesByProductIds(request))
      .thenReturn(this.generateProductImageResp());
    this.imageServiceBean.filterProductImagesByProductIds(request);
    Mockito.verify(this.productImageRepository).findProductImagesByProductIds(request);
  }
  
  @Test
  public void filterProductImagesByProductCodes_happyFlow_success() {
    List<String> request = Arrays.asList(PRODUCT_CODE);
    Page<ProductImageDTO> images =
        new PageImpl<>(Arrays.asList(ProductImageDTO.builder().productId(PRODUCT_ID)
            .productCode(PRODUCT_CODE).locationPath(LOCATION_PATH).sequence(1).active(Boolean.TRUE)
            .isMainImage(Boolean.TRUE).build()));
    Mockito.when(
        this.productImageRepository.findProductImagesByProductCodes(STORE_ID, request,
            PageRequest.of(0, 10))).thenReturn(images);
    List<ProductImageDTO> result =
        this.imageServiceBean.filterProductImagesByProductCodes(STORE_ID, request);
    Assertions.assertNotNull(result);
    Assertions.assertTrue(!result.isEmpty());
    Mockito.verify(this.productImageRepository).findProductImagesByProductCodes(STORE_ID, request,
        PageRequest.of(0, 10));
  }
  
  @Test
  public void filterProductImagesByProductCodes_iteratePaging_success() {
    List<String> request = Arrays.asList(PRODUCT_CODE);    
    Page<ProductImageDTO> imagePage1 =
        new PageImpl<>(Arrays.asList(ProductImageDTO.builder().productId(PRODUCT_ID)
            .productCode(PRODUCT_CODE).locationPath(LOCATION_PATH).sequence(1).active(Boolean.TRUE)
            .isMainImage(Boolean.TRUE).build()), PageRequest.of(0, 10), 12);
    Page<ProductImageDTO> imagePage2 =
        new PageImpl<>(Arrays.asList(ProductImageDTO.builder().productId(PRODUCT_ID)
            .productCode(PRODUCT_CODE).locationPath(LOCATION_PATH).sequence(1).active(Boolean.TRUE)
            .isMainImage(Boolean.TRUE).build()), PageRequest.of(1, 10), 12);
    
    Mockito.when(
        this.productImageRepository.findProductImagesByProductCodes(STORE_ID, request,
            PageRequest.of(0, 10))).thenReturn(imagePage1);
    Mockito.when(
        this.productImageRepository.findProductImagesByProductCodes(STORE_ID, request,
            PageRequest.of(1, 10))).thenReturn(imagePage2);
    List<ProductImageDTO> result =
        this.imageServiceBean.filterProductImagesByProductCodes(STORE_ID, request);
    Assertions.assertNotNull(result);
    Assertions.assertTrue(!result.isEmpty());
    Mockito.verify(this.productImageRepository).findProductImagesByProductCodes(STORE_ID, request,
        PageRequest.of(0, 10));
    Mockito.verify(this.productImageRepository).findProductImagesByProductCodes(STORE_ID, request,
        PageRequest.of(1, 10));
  }
  
  @Test
  public void filterProductImagesByProductCodes_emptyRequestList_returnEmptyList() {
    List<String> request = Collections.emptyList();
    List<ProductImageDTO> result =
        this.imageServiceBean.filterProductImagesByProductCodes(STORE_ID, request);
    Assertions.assertNotNull(result);
    Assertions.assertTrue(result.isEmpty());
  }

  @Test
  public void getProductImagesByStoreIdAndProductIdTest() {
    Mockito.when(productImageRepository.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(Collections.singletonList(productImage));
    List<ProductImage> response = imageServiceBean.getProductImagesByStoreIdAndProductIdCached(STORE_ID, PRODUCT_ID);
    List<ProductImage> response1 = imageServiceBean.getProductImagesByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(productImageRepository, times(2))
        .findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Assertions.assertEquals(LOCATION_PATH, response.get(0).getLocationPath());
    Assertions.assertEquals(PRODUCT_ID, response.get(0).getProductId());
    Assertions.assertEquals(LOCATION_PATH, response1.get(0).getLocationPath());
    Assertions.assertEquals(PRODUCT_ID, response1.get(0).getProductId());
  }

  @Test
  public void getProductItemImagesByStoreIdAndProductItemIdTest() {
    Mockito.when(productItemImageRepository.findByStoreIdAndProductItemIdIn(
        STORE_ID, Collections.singletonList(PRODUCT_ITEM_ID))).thenReturn(Collections.singletonList(productItemImage1));
    List<ProductItemImage> response =
        imageServiceBean.getDetachedProductItemImagesByStoreIdAndProductItemIds(STORE_ID, Collections.singletonList(PRODUCT_ITEM_ID));
    Mockito.verify(productItemImageRepository)
        .findByStoreIdAndProductItemIdIn(STORE_ID, Collections.singletonList(PRODUCT_ITEM_ID));
    Assertions.assertEquals(LOCATION_PATH, response.get(0).getLocationPath());
    Assertions.assertEquals(PRODUCT_ITEM_ID, response.get(0).getProductItemId());
  }

  @Test
  void testDeleteProductImagesByIds() {
    List<String> imageIds = List.of("img1", "img2");
    imageServiceBean.deleteProductImagesByIds(imageIds);
    Mockito.verify(productImageRepository, times(1)).deleteAllById(imageIds);
  }

  @Test
  void testDeleteProductItemImagesByIds() {
    List<String> itemImageIds = List.of("itemImg1", "itemImg2");
    imageServiceBean.deleteProductItemImagesByIds(itemImageIds);
    Mockito.verify(productItemImageRepository, times(1)).deleteAllById(itemImageIds);
  }
  
  private List<ProductImageSingle> generateProductImageResp() {
    ProductImageSingle img = new ProductImageSingle();
    img.setLocationPath(LOCATION_PATH);
    img.setActive(true);
    img.setMainImages(true);
    img.setProductId(PRODUCT_ID);
    img.setSequence(0);
    return Arrays.asList(img);
  }
  
  private ProductActivateImageDTO generateProductActivationDTO() {
    ProductActivateImageDTO productActivateImageDTO = new ProductActivateImageDTO();
    productActivateImageDTO.setProductCode(PRODUCT_CODE);
    Set<ActivateImageDTO> activateImageDTOS = new HashSet<>();
    ActivateImageDTO dto = new ActivateImageDTO();
    dto.setFilenames(LOCATION_PATH);
    dto.setHashCode(HASH_CODE);
    dto.setProductCode(PRODUCT_CODE);
    dto.setCommonImage(true);
    activateImageDTOS.add(dto);
    productActivateImageDTO.setImageRequests(activateImageDTOS);
    return productActivateImageDTO;
  }

}
