package com.gdn.mta.product.service.util;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import com.gdn.mta.product.service.FileStorageService;
import com.gdn.mta.product.valueobject.CommonImagePathDto;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gdn.mta.product.service.util.ImageCheckService;
import com.gdn.x.productcategorybase.dto.Image;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class ImageCheckServiceTest {

  private static final String SOURCE_DIRECTORY = "source_directory";
  private static final String FULL_IMAGE_SOURCE_DIRECTORY = "full_image_source_directory";
  private static final String LOCATION_PATH = "locationPath";
  private static final String LOCATION_PATH_1 = "locationPath1";
  private static final String PRODUCT_CODE = "product-code";
  private static final String PRODUCT_CODE_1 = "product-code-1";
  private static final String IMAGE_HASH_CODE_1 = "hashCode1";
  private static final String IMAGE_HASH_CODE_2 = "hashCode2";

  private ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
  private Image image = new Image();
  private Image image1 = new Image();
  private ProductRequest productRequest;
  private ProductDetailResponse productDetailResponse;
  private ProductItemResponse productItemResponse;
  private CommonImagePathDto commonImagePathDto1;
  private CommonImagePathDto commonImagePathDto2;
  private Map<String, CommonImagePathDto> uniqueCommonImages = new HashMap<>();
  private Map<String, Image> uniqueImages = new HashMap<>();

  @InjectMocks
  private ImageCheckService imageCheckService;

  @Mock
  private FileStorageService fileStorageService;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    image.setMarkForDelete(false);
    image.setActive(true);
    image.setOriginalImage(true);
    image.setLocationPath(LOCATION_PATH);

    image1.setMarkForDelete(true);
    image1.setActive(false);
    image1.setOriginalImage(false);
    image1.setLocationPath(LOCATION_PATH_1);

    productItemCreationRequest.setImages(Arrays.asList(image, image1));

    productRequest = new ProductRequest();
    commonImagePathDto1 = new CommonImagePathDto();
    commonImagePathDto2 = new CommonImagePathDto();

    productDetailResponse = new ProductDetailResponse();
    productItemResponse = new ProductItemResponse();

  }

  @AfterEach
  public void after() {
    Mockito.verifyNoMoreInteractions(fileStorageService);
  }

  @Test
  public void addUniqueImagesAndGetInvalidImageTest() {
    uniqueCommonImages.put(PRODUCT_CODE, commonImagePathDto1);
    productRequest.setImages(Arrays.asList(image, image1));
    Mockito.when(fileStorageService
      .checkImageAvailabilityAndGetUnavailableImages(Mockito.anyString(), Mockito.anyMap()))
      .thenReturn(SOURCE_DIRECTORY);
    String result = imageCheckService
      .addUniqueImagesAndGetInvalidImage(productRequest, uniqueCommonImages,
        FULL_IMAGE_SOURCE_DIRECTORY).getKey();
    Mockito.verify(fileStorageService)
      .checkImageAvailabilityAndGetUnavailableImages(Mockito.anyString(), Mockito.anyMap());
    Assertions.assertEquals(SOURCE_DIRECTORY, result);
  }

  @Test
  public void addUniqueImagesAndGetInvalidDuplicateImagesTest() {
    uniqueCommonImages.put(PRODUCT_CODE, commonImagePathDto1);
    productRequest.setImages(Arrays.asList(image, image));
    Mockito.when(fileStorageService
      .checkImageAvailabilityAndGetUnavailableImages(Mockito.anyString(), Mockito.anyMap()))
      .thenReturn(SOURCE_DIRECTORY);
    String result = imageCheckService
      .addUniqueImagesAndGetInvalidImage(productRequest, uniqueCommonImages,
        FULL_IMAGE_SOURCE_DIRECTORY).getKey();
    Mockito.verify(fileStorageService)
      .checkImageAvailabilityAndGetUnavailableImages(Mockito.anyString(), Mockito.anyMap());
    Assertions.assertEquals(SOURCE_DIRECTORY, result);
  }

  @Test
  public void addUniqueImagesAndGetInvalidImageForEditedProductsTest(){
    uniqueImages.put(PRODUCT_CODE,image);
    uniqueImages.put(PRODUCT_CODE_1,image1);
    productRequest.setImages(Arrays.asList(image,image1));
    Mockito.when(fileStorageService
      .checkImageAvailabilityAndGetUnavailableImagesForEditedProducts(Mockito.anyString(), Mockito.anyMap()))
      .thenReturn(SOURCE_DIRECTORY);
    String result =
      imageCheckService.addUniqueImagesAndGetInvalidImageForEditedProducts(productRequest,
      uniqueImages, FULL_IMAGE_SOURCE_DIRECTORY);
    Mockito.verify(fileStorageService)
      .checkImageAvailabilityAndGetUnavailableImagesForEditedProducts(Mockito.anyString(), Mockito.anyMap());
    Assertions.assertEquals(SOURCE_DIRECTORY, result);
  }

  @Test
  public void addUniqueImagesAndGetInvalidImageForRevisedProduct() {
    productDetailResponse.setImages(Arrays.asList(image,image1));
    Mockito.when(fileStorageService
      .checkImageAvailabilityAndGetUnavailableImages(Mockito.anyString(), Mockito.anyMap()))
      .thenReturn(SOURCE_DIRECTORY);
    String result =
      imageCheckService.addUniqueImagesAndGetInvalidImageForRevisedProduct(productDetailResponse,
      uniqueCommonImages, FULL_IMAGE_SOURCE_DIRECTORY);
    Mockito.verify(fileStorageService)
      .checkImageAvailabilityAndGetUnavailableImages(Mockito.anyString(), Mockito.anyMap());
    Assertions.assertEquals(SOURCE_DIRECTORY, result);
  }

  @Test
  public void addUniqueImagesAndGetInvalidImageDuplicateForRevisedProduct() {
    productDetailResponse.setImages(Arrays.asList(image,image));
    Mockito.when(fileStorageService
      .checkImageAvailabilityAndGetUnavailableImages(Mockito.anyString(), Mockito.anyMap()))
      .thenReturn(SOURCE_DIRECTORY);
    String result =
      imageCheckService.addUniqueImagesAndGetInvalidImageForRevisedProduct(productDetailResponse,
        uniqueCommonImages, FULL_IMAGE_SOURCE_DIRECTORY);
    Mockito.verify(fileStorageService)
      .checkImageAvailabilityAndGetUnavailableImages(Mockito.anyString(), Mockito.anyMap());
    Assertions.assertEquals(SOURCE_DIRECTORY, result);
  }

  @Test
  public void addUniqueImagesAndGetInvalidImage() {
    uniqueCommonImages.put(PRODUCT_CODE, commonImagePathDto1);
    uniqueCommonImages.put(PRODUCT_CODE_1, commonImagePathDto2);
    productItemResponse.setImages(Arrays.asList(image, image1));
    HashSet<ProductItemResponse> set = new HashSet<>();
    set.add(productItemResponse);
    productDetailResponse.setProductItemResponses(set);
    Mockito.when(fileStorageService
      .checkImageAvailabilityAndGetUnavailableImages(Mockito.anyString(), Mockito.anyMap()))
      .thenReturn(SOURCE_DIRECTORY);
    imageCheckService.addUniqueImagesAndGetInvalidImage(productDetailResponse, uniqueCommonImages,
      FULL_IMAGE_SOURCE_DIRECTORY);
    Mockito.verify(fileStorageService)
      .checkImageAvailabilityAndGetUnavailableImages(Mockito.anyString(), Mockito.anyMap());
  }

  @Test
  public void addUniqueImagesAndGetInvalidDuplicateImage() {
    uniqueCommonImages.put(PRODUCT_CODE, commonImagePathDto1);
    uniqueCommonImages.put(PRODUCT_CODE_1, commonImagePathDto2);
    productItemResponse.setImages(Arrays.asList(image, image));
    HashSet<ProductItemResponse> set = new HashSet<>();
    set.add(productItemResponse);
    productDetailResponse.setProductItemResponses(set);
    Mockito.when(fileStorageService
      .checkImageAvailabilityAndGetUnavailableImages(Mockito.anyString(), Mockito.anyMap()))
      .thenReturn(SOURCE_DIRECTORY);
    imageCheckService.addUniqueImagesAndGetInvalidImage(productDetailResponse, uniqueCommonImages,
      FULL_IMAGE_SOURCE_DIRECTORY);
    Mockito.verify(fileStorageService)
      .checkImageAvailabilityAndGetUnavailableImages(Mockito.anyString(), Mockito.anyMap());
  }


  @Test
  public void getUniqueImagesTest(){
    uniqueCommonImages.put(PRODUCT_CODE, commonImagePathDto1);
    uniqueCommonImages.put(PRODUCT_CODE_1, commonImagePathDto2);
    image.setOriginalImage(true);
    image.setHashCode(IMAGE_HASH_CODE_1);
    image1.setHashCode(IMAGE_HASH_CODE_2);
    productDetailResponse.setImages(Arrays.asList(image, image1));
    productItemResponse.setImages(Arrays.asList(image,image1));
    HashSet<ProductItemResponse> set = new HashSet<>();
    set.add(productItemResponse);
    productDetailResponse.setProductItemResponses(set);
    Mockito.when(fileStorageService
      .checkImageAvailabilityAndGetUnavailableImages(Mockito.anyString(), Mockito.anyMap()))
      .thenReturn(SOURCE_DIRECTORY);
    imageCheckService.getUniqueImages(productDetailResponse,uniqueCommonImages,FULL_IMAGE_SOURCE_DIRECTORY);
    Mockito.verify(fileStorageService)
      .checkImageAvailabilityAndGetUnavailableImages(Mockito.anyString(), Mockito.anyMap());
  }

  @Test
  public void getUniqueImagesDuplicateTest(){
    uniqueCommonImages.put(PRODUCT_CODE, commonImagePathDto1);
    uniqueCommonImages.put(PRODUCT_CODE_1, commonImagePathDto2);
    image.setHashCode(IMAGE_HASH_CODE_1);
    image1.setHashCode(IMAGE_HASH_CODE_2);
    productDetailResponse.setImages(Arrays.asList(image, image));
    HashSet<ProductItemResponse> set = new HashSet<>();
    set.add(productItemResponse);
    productDetailResponse.setProductItemResponses(set);
    Mockito.when(fileStorageService
      .checkImageAvailabilityAndGetUnavailableImages(Mockito.anyString(), Mockito.anyMap()))
      .thenReturn(SOURCE_DIRECTORY);
    imageCheckService.getUniqueImages(productDetailResponse,uniqueCommonImages,FULL_IMAGE_SOURCE_DIRECTORY);
    Mockito.verify(fileStorageService)
      .checkImageAvailabilityAndGetUnavailableImages(Mockito.anyString(), Mockito.anyMap());
  }
}
