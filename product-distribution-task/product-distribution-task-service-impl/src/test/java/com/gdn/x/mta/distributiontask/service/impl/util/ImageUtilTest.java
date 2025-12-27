package com.gdn.x.mta.distributiontask.service.impl.util;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductImageResponse;
import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductImage;
import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.model.ProductItemImage;
import com.gdn.x.mta.distributiontask.service.api.FileStorageService;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class ImageUtilTest {

  private static String IMAGE_FINAL_DIRECTORY = "images1";
  private static final String IMAGE_LOCATION_PATH = "location-path";

  private Product product;
  private File resourceDirectory;

  @Mock
  private FileStorageService fileStorageService;


  @BeforeEach
  public void setUp() throws IOException {
    resourceDirectory = new File(IMAGE_FINAL_DIRECTORY);
    File finalImage = new File(resourceDirectory +File.separator + IMAGE_LOCATION_PATH);
    FileUtils.touch(finalImage);

    ProductImage productImage1 = new ProductImage();
    productImage1.setActive(false);
    productImage1.setLocationPath(IMAGE_LOCATION_PATH);

    ProductImage productImage2 = new ProductImage();
    productImage2.setActive(true);
    productImage2.setLocationPath(IMAGE_LOCATION_PATH);

    ProductImage productImage3 = new ProductImage();
    productImage3.setActive(false);

    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setActive(false);
    productItemImage1.setLocationPath(IMAGE_LOCATION_PATH);

    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setActive(true);
    productItemImage2.setLocationPath(IMAGE_LOCATION_PATH);

    ProductItemImage productItemImage3 = new ProductItemImage();
    productItemImage3.setActive(false);

    ProductItem productItem = new ProductItem();
    productItem.setProductItemImages(Arrays.asList(productItemImage1, productItemImage2, productItemImage3));

    product = new Product();
    product.setProductImages(Arrays.asList(productImage1, productImage2, productImage3));
    product.setProductItems(List.of(productItem));

    ImageUtils.setFileStorageService(fileStorageService);
  }

  @AfterEach
  public void tearDown() throws IOException {
    FileUtils.deleteDirectory(resourceDirectory);
  }

  @Test
   void setActiveFlagInProductAndItemImagesTest() {
    Mockito.when(fileStorageService.isFinalImageFileExist(IMAGE_LOCATION_PATH)).thenReturn(Boolean.TRUE);
    ImageUtils.setActiveFlagInProductAndItemImages(product);
    Mockito.verify(fileStorageService,Mockito.times(1)).isFinalImageFileExist(IMAGE_LOCATION_PATH);
    Assertions.assertFalse(product.getProductImages().get(0).isActive());
    Assertions.assertTrue(fileStorageService.isFinalImageFileExist(IMAGE_LOCATION_PATH));
  }

  @Test
   void setActiveFlagInProductAndItemImagesEmptyImagesTest() {
    product.setProductImages(new ArrayList<>());
    product.getProductItems().get(0).setProductItemImages(new ArrayList<>());
    ImageUtils.setActiveFlagInProductAndItemImages(product);
  }

  @Test
   void setActiveFlagInProductAndItemImagesEmptyItemsTest() {
    product.setProductItems(new ArrayList<>());
    ImageUtils.setActiveFlagInProductAndItemImages(product);
  }

  @Test
  void overrideMainImageIfNotPresentTest() {
    DistributionProductImageResponse distributionProductImageResponse =
      new DistributionProductImageResponse();
    distributionProductImageResponse.setMainImage(true);
    Assertions.assertFalse(ImageUtils.overrideMainImageIfNotPresent(false, new ArrayList<>()));
    Assertions.assertFalse(ImageUtils.overrideMainImageIfNotPresent(true, new ArrayList<>()));
    Assertions.assertFalse(
      ImageUtils.overrideMainImageIfNotPresent(true, List.of(distributionProductImageResponse)));
    distributionProductImageResponse.setMainImage(false);
    Assertions.assertTrue(
      ImageUtils.overrideMainImageIfNotPresent(true, List.of(distributionProductImageResponse)));
  }

}
