package com.gdn.mta.product.service;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductImageEditRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.UpdateItemsPriceStockImagesRequest;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.micro.graphics.web.model.ImageRequest;
import com.gdn.mta.product.entity.ProductFieldHistory;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.service.config.GcsProperties;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.mta.product.valueobject.CommonImagePathDto;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.google.common.collect.ImmutableSet;

public class FileStorageServiceTest {

  private static final String BUCKET_NAME = "bucket-name";
  private static final String FINAL_IMAGE_BUCKET_NAME = "final-image-bucket-name";
  private static final String GCS_PATH_PREFIX = "catalog-image";
  private static final String PRODUCT_CODE = "product-code";
  private static final String FILE_PATH = PRODUCT_CODE + "/file-path";
  private static final String FILESTORE_IMAGE_SOURCE_DIR = "image-source-dir";
  private static final String FULL_IMAGE_SOURCE_DIR = "full-image-source-dir";
  private static final String FINAL_FULL_IMAGE_SOURCE_DIR = "final-full-image-source-dir";
  private static final String FINAL_FULL_IMAGE_LOCATION_PATH = "full/final-full-image.jpg";
  private static final String GCS_DOMAIN_URL_PATH = "gcs-domain-url-path";
  private static final String MTA_API = "mta-api";
  private static final String CATALOG_IMAGE = "catalog-image";
  private static final String IMAGE_1 = "1.png";
  private static final String IMAGE_2 = "2.png";
  private static final String LOCATION_PATH = "/nike_test_product_and_item_images_full01_hhp7330d.jpg";
  private static final String FINAL_IMAGE_PATH = "full-image-source-dir/final-full-image-source-dir/nike_test_product_and_item_images_full01_hhp7330d.jpg";
  private static final Set<String> WHITELISTED_CLIENT_IDS = ImmutableSet.of(MTA_API);
  private CommonImagePathDto commonImagePathDto;
  private Image image;
  private Image image1;
  private ImageRequest imageRequest;
  private ProductItemCreationRequest productItemCreationRequest;
  private ProductCreationRequest productCreationRequest;
  private Map<String,CommonImagePathDto> imagePathDtoMap = new HashMap<>();
  private Map<String, Image> imageHashMap   = new HashMap<>();


  @InjectMocks
  private FileStorageServiceImpl fileStorageService;

  @Mock
  private GcsProperties gcsProperties;

  @Mock
  private GcsService gcsService;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @BeforeEach
  public void before() {
    MockitoAnnotations.initMocks(this);
    commonImagePathDto = new CommonImagePathDto();
    image = new Image();
    image1 = new Image();
    imageRequest = new ImageRequest();
    productCreationRequest = new ProductCreationRequest();
    productItemCreationRequest = new ProductItemCreationRequest();
  }

  @AfterEach
  public void after() {
    Mockito.verifyNoMoreInteractions(gcsProperties, gcsService);
  }

  @Test
  public void isFileExistsTest(){
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn(BUCKET_NAME);
    Mockito.when(gcsService.isFileExists(BUCKET_NAME,FILE_PATH)).thenReturn(true);
    boolean result = fileStorageService.isFileExists(FILE_PATH, false);
    Mockito.verify(gcsProperties).getSourceImageBucketName();
    Mockito.verify(gcsService).isFileExists(Mockito.any(), Mockito.anyString());
    Assertions.assertTrue(result);
  }

  @Test
  public void checkImageAvailabilityAndGetUnavailableImagesTest(){
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    commonImagePathDto.setLocationPath(FILE_PATH);
    imagePathDtoMap.put(PRODUCT_CODE,commonImagePathDto);
    fileStorageService.checkImageAvailabilityAndGetUnavailableImages(FILE_PATH,imagePathDtoMap);
    Mockito.verify(gcsProperties, Mockito.times(2)).getPathPrefix();
  }

  @Test
  public void checkImageAvailabilityAndGetUnavailableImagesGcsTest(){
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn(BUCKET_NAME);
    Mockito.when(gcsService.isFileExists(Mockito.any(),Mockito.any())).thenReturn(true);
    commonImagePathDto.setLocationPath(GCS_PATH_PREFIX + Constants.DELIMITER_SLASH + FILE_PATH);
    imagePathDtoMap.put(PRODUCT_CODE,commonImagePathDto);
    fileStorageService.checkImageAvailabilityAndGetUnavailableImages(FILE_PATH,imagePathDtoMap);
    Mockito.verify(gcsProperties).getSourceImageBucketName();
    Mockito.verify(gcsService).isFileExists(Mockito.any(),Mockito.any());
    Mockito.verify(gcsProperties, Mockito.times(2)).getPathPrefix();
    Mockito.verify(gcsProperties).getSourceImageDirectory();
  }

  @Test
  public void checkImageAvailabilityAndGetUnavailableImagesForEditedProductsTest(){
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    image.setLocationPath(FILE_PATH);
    imageHashMap.put(PRODUCT_CODE,image);
    fileStorageService.checkImageAvailabilityAndGetUnavailableImagesForEditedProducts(FILE_PATH,imageHashMap);
    Mockito.verify(gcsProperties, Mockito.times(2)).getPathPrefix();
  }

  @Test
  public void checkImageAvailabilityAndGetUnavailableImagesForEditedProductsExistingTest(){
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    image.setLocationPath("nike_test_product_and_item_images_full01_hhp7330d.jpg");
    imageHashMap.put(PRODUCT_CODE,image);
    ReflectionTestUtils.setField(fileStorageService, "imageSourceDirectory", "src/test/resources/resize");
    fileStorageService.checkImageAvailabilityAndGetUnavailableImagesForEditedProducts(FILE_PATH,imageHashMap);
    Mockito.verify(gcsProperties, Mockito.times(2)).getPathPrefix();
  }

  @Test
  public void checkImageAvailabilityAndGetUnavailableImagesForEditedProductsNonActiveTest(){
    image.setLocationPath(FILE_PATH);
    image.setActive(true);
    imageHashMap.put(PRODUCT_CODE,image);
    fileStorageService.checkImageAvailabilityAndGetUnavailableImagesForEditedProducts(FILE_PATH,imageHashMap);
  }

  @Test
  public void checkImageAvailabilityTest(){
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    imageRequest.setAbsoluteImagePath(FILE_PATH);
    fileStorageService.checkImageAvailability(Arrays.asList(imageRequest), false);
    Mockito.verify(gcsProperties).getPathPrefix();
  }

  @Test
  public void checkImageAvailabilityAndGetUnavailableImagesForEditedProductsNonExistentTest(){
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsService.isFileExists(BUCKET_NAME,FILE_PATH)).thenReturn(true);
    image.setLocationPath(FILE_PATH + Constants.DELIMITER_SLASH + GCS_PATH_PREFIX);
    imageHashMap.put(PRODUCT_CODE,image);
    fileStorageService.checkImageAvailabilityAndGetUnavailableImagesForEditedProducts(FILE_PATH,imageHashMap);
    Mockito.verify(gcsProperties, Mockito.times(2)).getPathPrefix();
    Mockito.verify(gcsProperties).getSourceImageDirectory();
    Mockito.verify(gcsProperties).getSourceImageBucketName();
    Mockito.verify(gcsService).isFileExists(Mockito.any(), Mockito.anyString());
  }

  @Test
  public void checkImageAvailabilityProductItemTest(){
    image.setMarkForDelete(false);
    image.setActive(true);
    image.setLocationPath(FILE_PATH);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(FINAL_FULL_IMAGE_SOURCE_DIR);
    productItemCreationRequest.setImages(Arrays.asList(image));
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    fileStorageService.checkImageAvailability(productCreationRequest, ProductCreationType.FLOW1.getProductCreationType());
    Mockito.verify(gcsProperties).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsService).isFileExists(Mockito.any(),Mockito.any());
  }

  @Test
  public void checkImageAvailabilityProductItemMigrationNotCompletedTest(){
    image.setMarkForDelete(false);
    image.setActive(true);
    image.setLocationPath(FILE_PATH);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(FINAL_FULL_IMAGE_SOURCE_DIR);
    productItemCreationRequest.setImages(Arrays.asList(image));
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    fileStorageService.checkImageAvailability(productCreationRequest, ProductCreationType.FLOW1.getProductCreationType());
    Mockito.verify(gcsProperties).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsService).isFileExists(Mockito.any(),Mockito.any());
  }

  @Test
  public void checkImageAvailabilityProductItemMigrationNotCompletedPathFoundTest(){
    image.setMarkForDelete(false);
    image.setActive(true);
    image.setLocationPath("path");
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn("src/test/resources/");
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn("/full-image-source-dir");
    productItemCreationRequest.setImages(Arrays.asList(image));
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    fileStorageService.checkImageAvailability(productCreationRequest, ProductCreationType.FLOW1.getProductCreationType());
    Mockito.verify(gcsProperties).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsService).isFileExists(Mockito.any(),Mockito.any());
  }

  @Test
  public void checkImageAvailabilityProductItemExistingTest(){
    String path = "src/test/resources/resize";
    image.setMarkForDelete(false);
    image.setActive(true);
    image.setLocationPath("nike_test_product_and_item_images_full01_hhp7330d.jpg");
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(FINAL_FULL_IMAGE_SOURCE_DIR);
    productItemCreationRequest.setImages(Arrays.asList(image));
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    fileStorageService.checkImageAvailability(productCreationRequest, ProductCreationType.FLOW1.getProductCreationType());
    Mockito.verify(gcsProperties).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsService).isFileExists(Mockito.any(),Mockito.any());
  }

  @Test
  public void checkImageAvailabilityProductItemGcsTest(){
    image.setMarkForDelete(false);
    image.setActive(false);
    image.setLocationPath(GCS_PATH_PREFIX + PRODUCT_CODE);
    image1.setMarkForDelete(true);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    productItemCreationRequest.setImages(Arrays.asList(image,image1));
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    fileStorageService.checkImageAvailability(productCreationRequest, ProductCreationType.FLOW1.getProductCreationType());
    Mockito.verify(gcsProperties).getPathPrefix();
    Mockito.verify(gcsProperties).getSourceImageDirectory();
    Mockito.verify(gcsProperties).getSourceImageBucketName();
    Mockito.verify(gcsService).isFileExists(Mockito.any(),Mockito.any());
  }


  @Test
  public void checkImageAvailabilityProductItemGcsWithoutPrefixTest(){
    image.setMarkForDelete(false);
    image.setActive(false);
    image.setLocationPath(PRODUCT_CODE);
    image1.setMarkForDelete(true);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    productItemCreationRequest.setImages(Arrays.asList(image,image1));
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    fileStorageService.checkImageAvailability(productCreationRequest, ProductCreationType.FLOW1.getProductCreationType());
    Mockito.verify(gcsProperties).getPathPrefix();
  }


  @Test
  public void checkImageAvailabilityProductItemExistingFlow2Test(){
    image.setMarkForDelete(false);
    image.setActive(false);
    image.setLocationPath("nike_test_product_and_item_images_full01_hhp7330d.jpg");
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(FINAL_FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    productItemCreationRequest.setImages(Arrays.asList(image));
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    fileStorageService.checkImageAvailability(productCreationRequest, ProductCreationType.FLOW2.getProductCreationType());
    Mockito.verify(gcsProperties).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsService, Mockito.times(2)).isFileExists(Mockito.any(),Mockito.any());
    Mockito.verify(gcsProperties).getSourceImageDirectory();
    Mockito.verify(gcsProperties).getSourceImageBucketName();
  }

  @Test
  public void checkImageAvailabilityProductItemExistingFlow2FalseTest() {
    image.setMarkForDelete(false);
    image.setActive(false);
    image.setOriginalImage(true);
    image.setLocationPath("nike_test_product_and_item_images_full01_hhp7330d.jpg");
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(FINAL_FULL_IMAGE_SOURCE_DIR);
    productItemCreationRequest.setImages(Arrays.asList(image));
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    fileStorageService.checkImageAvailability(productCreationRequest,
        ProductCreationType.FLOW2.getProductCreationType());
    Mockito.verify(gcsProperties).getSourceImageDirectory();
    Mockito.verify(gcsProperties).getSourceImageBucketName();
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsService, Mockito.times(2)).isFileExists(Mockito.any(),Mockito.any());
  }

  @Test
  public void checkImageAvailabilityProductItemExistingFinalLocationFlow2FalseTest() {
    image.setMarkForDelete(false);
    image.setActive(false);
    image.setOriginalImage(true);
    image.setLocationPath("nike_test_product_and_item_images_full01_hhp7330d.jpg");
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(FINAL_FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsService.isFileExists(gcsProperties.getFinalImageBucketName(),
        "full-image-source-dir/nike_test_product_and_item_images_full01_hhp7330d.jpg")).thenReturn(true);
    Mockito.when(gcsService.isFileExists(gcsProperties.getSourceImageBucketName(),
        "full-image-source-dir/nike_test_product_and_item_images_full01_hhp7330d.jpg")).thenReturn(true);
    productItemCreationRequest.setImages(Arrays.asList(image));
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    fileStorageService.checkImageAvailability(productCreationRequest,
        ProductCreationType.FLOW2.getProductCreationType());
    Mockito.verify(gcsProperties).getSourceImageDirectory();
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getFinalImageDirectory();
    Mockito.verify(gcsService).isFileExists(gcsProperties.getSourceImageBucketName(),
        "full-image-source-dir/nike_test_product_and_item_images_full01_hhp7330d.jpg");
    Mockito.verify(gcsService).isFileExists(gcsProperties.getFinalImageBucketName(),
        "full-image-source-dir/nike_test_product_and_item_images_full01_hhp7330d.jpg");
    Mockito.verify(gcsProperties, Mockito.times(3)).getSourceImageBucketName();
    Mockito.verify(gcsProperties, Mockito.times(3)).getFinalImageBucketName();
    Mockito.verify(gcsService, Mockito.times(2)).isFileExists(Mockito.any(), Mockito.anyString());
  }

  @Test
  public void checkImageAvailabilityProductItemExistingInSourceLocationFlow2FalseTest() {
    image.setMarkForDelete(false);
    image.setActive(false);
    image.setOriginalImage(true);
    image.setLocationPath("nike_test_product_and_item_images_full01_hhp7330d.jpg");
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(FINAL_FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsService.isFileExists(gcsProperties.getFinalImageBucketName(),
            "full-image-source-dir/final-full-image-source-dir/nike_test_product_and_item_images_full01_hhp7330d.jpg"))
        .thenReturn(true);
    productItemCreationRequest.setImages(Arrays.asList(image));
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    fileStorageService.checkImageAvailability(productCreationRequest,
        ProductCreationType.FLOW2.getProductCreationType());
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getFinalImageDirectory();
    Mockito.verify(gcsService).isFileExists(gcsProperties.getFinalImageBucketName(),
        "full-image-source-dir/final-full-image-source-dir/nike_test_product_and_item_images_full01_hhp7330d.jpg");
    Mockito.verify(gcsProperties, Mockito.times(3)).getFinalImageBucketName();
  }

  @Test
  public void checkImageAvailabilityProductItemExistingFlow2ActiveImageFalseTest(){
    image.setMarkForDelete(false);
    image.setActive(true);
    image.setOriginalImage(true);
    image.setLocationPath("nike_test_product_and_item_images_full01_hhp7330d.jpg");
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(FINAL_FULL_IMAGE_SOURCE_DIR);
    productItemCreationRequest.setImages(Arrays.asList(image));
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    fileStorageService.checkImageAvailability(productCreationRequest, ProductCreationType.FLOW1.getProductCreationType());
    Mockito.verify(gcsProperties).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsService).isFileExists(Mockito.any(),Mockito.any());
  }

  @Test
  public void checkImageAvailabilityProductItemGcsNonExistentTest(){
    image.setMarkForDelete(false);
    image.setActive(false);
    image.setLocationPath(GCS_PATH_PREFIX + PRODUCT_CODE);
    image1.setMarkForDelete(true);
    Mockito.when(gcsService.isFileExists(Mockito.any(),Mockito.any())).thenReturn(true);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    productItemCreationRequest.setImages(Arrays.asList(image,image1));
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    fileStorageService.checkImageAvailability(productCreationRequest,ProductCreationType.FLOW1.getProductCreationType());
    Mockito.verify(gcsProperties).getPathPrefix();
    Mockito.verify(gcsProperties).getSourceImageDirectory();
    Mockito.verify(gcsProperties).getSourceImageBucketName();
    Mockito.verify(gcsService).isFileExists(Mockito.any(),Mockito.any());
  }

  @Test
  public void editImageNameIfGcsEnabledCreationTest() {
    Image image = new Image();
    image.setLocationPath(FILE_PATH);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setCommonImages(Arrays.asList(image));
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setImages(Arrays.asList(image));
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));
    productCreationRequest.setProductCode(PRODUCT_CODE);

    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.getWhiteListedChannelIds()).thenReturn(WHITELISTED_CLIENT_IDS);
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn(null);
    fileStorageService.editImageNameIfGcsEnabled(productCreationRequest);

    image.setLocationPath("/filePath");
    fileStorageService.editImageNameIfGcsEnabled(productCreationRequest);

    productCreationRequest.setCommonImages(new ArrayList<>());
    productCreationRequest.setProductItemRequests(new ArrayList<>());
    fileStorageService.editImageNameIfGcsEnabled(productCreationRequest);

    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    fileStorageService.editImageNameIfGcsEnabled(productCreationRequest);

    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn(MTA_API);
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    fileStorageService.editImageNameIfGcsEnabled(productCreationRequest);

    productCreationRequest.setImages(Arrays.asList(image));
    fileStorageService.editImageNameIfGcsEnabled(productCreationRequest);

    Mockito.verify(gcsProperties, Mockito.times(5)).getWhiteListedChannelIds();
    Mockito.verify(gcsProperties, Mockito.times(6)).isSourceImageEnabled();
    Mockito.verify(gcsProperties, Mockito.times(4)).getPathPrefix();
  }

  @Test
  public void editImageNameIfGcsEnabledEditV1Test() {
    UpdateItemsPriceStockImagesRequest request = new UpdateItemsPriceStockImagesRequest();
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest1 =
        new ProductLevel3SummaryDetailsImageRequest();
    productLevel3SummaryDetailsImageRequest1.setLocationPath(FILE_PATH);
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest2 =
        new ProductLevel3SummaryDetailsImageRequest();
    productLevel3SummaryDetailsImageRequest2.setLocationPath(FILE_PATH);
    productLevel3SummaryDetailsImageRequest2.setReviewType(Constants.NEW);
    request.setCopyToAllVariantImages(
        Arrays.asList(productLevel3SummaryDetailsImageRequest1, productLevel3SummaryDetailsImageRequest2));
    ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest = new ProductPriceStockAndImagesRequest();
    productPriceStockAndImagesRequest.setImages(
        Arrays.asList(productLevel3SummaryDetailsImageRequest1, productLevel3SummaryDetailsImageRequest2));
    request.setProductItems(Arrays.asList(productPriceStockAndImagesRequest));
    request.setNeedCorrection(true);

    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.getWhiteListedChannelIds()).thenReturn(WHITELISTED_CLIENT_IDS);
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn(null);
    fileStorageService.editImageNameIfGcsEnabled(request);

    productLevel3SummaryDetailsImageRequest2.setReviewType(null);
    fileStorageService.editImageNameIfGcsEnabled(request);

    request.setProductItems(new ArrayList<>());
    fileStorageService.editImageNameIfGcsEnabled(request);

    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    fileStorageService.editImageNameIfGcsEnabled(request);

    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn(MTA_API);
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    fileStorageService.editImageNameIfGcsEnabled(request);

    request.setNeedCorrection(false);
    fileStorageService.editImageNameIfGcsEnabled(request);

    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(true);
    fileStorageService.editImageNameIfGcsEnabled(request);

    Mockito.verify(gcsProperties, Mockito.times(5)).getWhiteListedChannelIds();
    Mockito.verify(gcsProperties, Mockito.times(5)).isSourceImageEnabled();
    Mockito.verify(gcsProperties, Mockito.times(4)).getPathPrefix();
    Mockito.verify(gcsProperties, Mockito.times(2)).isFinalImageEnabled();
  }

  @Test
  public void editImageNameIfGcsEnabledEditV2Test() {
    ProductVariantUpdateRequest request = new ProductVariantUpdateRequest();
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest1 =
        new ProductLevel3SummaryDetailsImageRequest();
    productLevel3SummaryDetailsImageRequest1.setLocationPath(FILE_PATH);
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest2 =
        new ProductLevel3SummaryDetailsImageRequest();
    productLevel3SummaryDetailsImageRequest2.setLocationPath(FILE_PATH);
    productLevel3SummaryDetailsImageRequest2.setReviewType(Constants.NEW);
    request.setCopyToAllVariantImages(
        Arrays.asList(productLevel3SummaryDetailsImageRequest1, productLevel3SummaryDetailsImageRequest2));
    ProductVariantPriceStockAndImagesRequest productPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productPriceStockAndImagesRequest.setImages(
        Arrays.asList(productLevel3SummaryDetailsImageRequest1, productLevel3SummaryDetailsImageRequest2));
    request.setProductItems(Arrays.asList(productPriceStockAndImagesRequest));
    request.setNeedCorrection(true);

    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn(null);
    Mockito.when(gcsProperties.getWhiteListedChannelIds()).thenReturn(WHITELISTED_CLIENT_IDS);
    fileStorageService.editImageNameIfGcsEnabled(request);

    productLevel3SummaryDetailsImageRequest2.setReviewType(null);
    fileStorageService.editImageNameIfGcsEnabled(request);

    request.setProductItems(new ArrayList<>());
    fileStorageService.editImageNameIfGcsEnabled(request);

    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    fileStorageService.editImageNameIfGcsEnabled(request);

    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn(MTA_API);
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    fileStorageService.editImageNameIfGcsEnabled(request);

    request.setNeedCorrection(false);
    fileStorageService.editImageNameIfGcsEnabled(request);

    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(true);
    fileStorageService.editImageNameIfGcsEnabled(request);

    Mockito.verify(gcsProperties, Mockito.times(5)).getWhiteListedChannelIds();
    Mockito.verify(gcsProperties, Mockito.times(5)).isSourceImageEnabled();
    Mockito.verify(gcsProperties, Mockito.times(4)).getPathPrefix();
    Mockito.verify(gcsProperties, Mockito.times(2)).isFinalImageEnabled();
  }

  @Test
  public void editImageNameIfGcsEnabledEditV2CommonImagesEmptyTest() {
    ProductVariantUpdateRequest request = new ProductVariantUpdateRequest();
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest1 =
        new ProductLevel3SummaryDetailsImageRequest();
    productLevel3SummaryDetailsImageRequest1.setLocationPath(FILE_PATH);
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest2 =
        new ProductLevel3SummaryDetailsImageRequest();
    productLevel3SummaryDetailsImageRequest2.setLocationPath(FILE_PATH);
    productLevel3SummaryDetailsImageRequest2.setReviewType(Constants.NEW);
    ProductVariantPriceStockAndImagesRequest productPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productPriceStockAndImagesRequest.setImages(
        Arrays.asList(productLevel3SummaryDetailsImageRequest1, productLevel3SummaryDetailsImageRequest2));
    request.setProductItems(Arrays.asList(productPriceStockAndImagesRequest));
    request.setNeedCorrection(true);
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn(null);
    Mockito.when(gcsProperties.getWhiteListedChannelIds()).thenReturn(WHITELISTED_CLIENT_IDS);
    request.setCopyToAllVariantImages(new ArrayList<>());
    fileStorageService.editImageNameIfGcsEnabled(request);
    Mockito.verify(gcsProperties).getWhiteListedChannelIds();
    Mockito.verify(gcsProperties).isSourceImageEnabled();
    Mockito.verify(gcsProperties).getPathPrefix();
  }

  @Test
  public void getCompleteSourceUrlPrefixTest() {
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn(BUCKET_NAME);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(FILESTORE_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getGcsDomainUrlPath()).thenReturn(GCS_DOMAIN_URL_PATH);
    String gcsCompleteSourceUrl = fileStorageService.getCompleteSourceUrlPrefix();
    Mockito.verify(gcsProperties).getSourceImageBucketName();
    Mockito.verify(gcsProperties).getSourceImageDirectory();
    Mockito.verify(gcsProperties).getGcsDomainUrlPath();
    Assertions.assertEquals(GCS_DOMAIN_URL_PATH + File.separator + BUCKET_NAME + File.separator
        + FILESTORE_IMAGE_SOURCE_DIR + File.separator, gcsCompleteSourceUrl);
  }

  @Test
  public void getImagePathPrefixTest() {
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    String imagePathPrefix = fileStorageService.getImagePathPrefix();
    Mockito.verify(gcsProperties).getPathPrefix();
    Assertions.assertEquals(GCS_PATH_PREFIX, imagePathPrefix);
  }

  @Test
  public void getRevisedImageRequestsTest() {
    Image image1 = new Image();
    image1.setMarkForDelete(true);
    image1.setLocationPath(FILE_PATH);
    Image image2 = new Image();
    image2.setLocationPath(FILE_PATH);
    Image image3 = new Image();
    image3.setRevised(true);
    image3.setLocationPath(FILE_PATH);

    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(FILESTORE_IMAGE_SOURCE_DIR);
    List<ImageRequest> imageRequests =
        fileStorageService.getRevisedImageRequests(Arrays.asList(image1, image2, image3), FILESTORE_IMAGE_SOURCE_DIR);
    Assertions.assertEquals(FILESTORE_IMAGE_SOURCE_DIR + File.separator + FILE_PATH,
        imageRequests.get(0).getAbsoluteImagePath());

    image3.setLocationPath(GCS_PATH_PREFIX + File.separator + FILE_PATH);
    imageRequests = fileStorageService.getRevisedImageRequests(Arrays.asList(image1, image2, image3), FILESTORE_IMAGE_SOURCE_DIR);
    Assertions.assertEquals(FILESTORE_IMAGE_SOURCE_DIR + File.separator + GCS_PATH_PREFIX + File.separator + FILE_PATH,
        imageRequests.get(0).getAbsoluteImagePath());
    Mockito.verify(gcsProperties, Mockito.times(2)).getPathPrefix();
    Mockito.verify(gcsProperties).getSourceImageDirectory();
  }

  @Test
  public void checkImageAvailabilityNotFoundInFileStoreOrGcsTest(){
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(false);
    imageRequest.setAbsoluteImagePath(FILE_PATH);
    String image =
      fileStorageService.checkImageAvailabilityInFileStoreOrGcs(Arrays.asList(imageRequest));
    Assertions.assertNotNull(image);
    Mockito.verify(gcsProperties).isFileStoreToGcsMigrationCompleted();
    Mockito.verify(gcsProperties).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsService).isFileExists(Mockito.any(),Mockito.any());
  }

  @Test
  public void checkImageAvailabilityFoundInGcsTest(){
    Mockito.when(gcsProperties.getFinalImageBucketName()).thenReturn(FINAL_IMAGE_BUCKET_NAME);
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(FINAL_FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(true);
    Mockito.when(gcsService.isFileExists(FINAL_IMAGE_BUCKET_NAME,
      FULL_IMAGE_SOURCE_DIR + Constants.DELIMITER_SLASH + FINAL_FULL_IMAGE_SOURCE_DIR
        + Constants.DELIMITER_SLASH + FILE_PATH)).thenReturn(true);
    imageRequest.setAbsoluteImagePath(FILE_PATH);
    String image =
      fileStorageService.checkImageAvailabilityInFileStoreOrGcs(Arrays.asList(imageRequest));
    Assertions.assertNotNull(image);
    Mockito.verify(gcsProperties).isFileStoreToGcsMigrationCompleted();
    Mockito.verify(gcsProperties).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsService).isFileExists(Mockito.any(),Mockito.any());
  }

  @Test
  public void checkImageAvailabilityNotFoundInFileStoreTest(){
    Mockito.when(gcsProperties.getFinalImageBucketName()).thenReturn(FINAL_IMAGE_BUCKET_NAME);
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(FINAL_FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(false);
    imageRequest.setAbsoluteImagePath(FILE_PATH);
    String image =
      fileStorageService.checkImageAvailabilityInFileStoreOrGcs(Arrays.asList(imageRequest));
    Assertions.assertNotNull(image);
    Mockito.verify(gcsProperties).isFileStoreToGcsMigrationCompleted();
    Mockito.verify(gcsProperties).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsService).isFileExists(Mockito.any(), Mockito.anyString());
  }

  @Test
  public void checkImageAvailabilityFoundInFileStoreTest(){
    Mockito.when(gcsProperties.getFinalImageBucketName()).thenReturn(FINAL_IMAGE_BUCKET_NAME);
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(FINAL_FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(false);
    ReflectionTestUtils.setField(fileStorageService, "fullImageSourceDirectory", "src/test");
    imageRequest.setAbsoluteImagePath("resources/resize/nike_test_product_and_item_images_full01_hhp7330d.jpg");
    String image =
      fileStorageService.checkImageAvailabilityInFileStoreOrGcs(Arrays.asList(imageRequest));
    Assertions.assertNull(image);
    Mockito.verify(gcsProperties).isFileStoreToGcsMigrationCompleted();
    Mockito.verify(gcsProperties).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsService).isFileExists(Mockito.any(), Mockito.anyString());
  }

  @Test
  public void checkImageAvailabilityNotFoundTest(){
    Mockito.when(gcsProperties.getFinalImageBucketName()).thenReturn(FINAL_IMAGE_BUCKET_NAME);
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(FINAL_FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(true);
    ReflectionTestUtils.setField(fileStorageService, "fullImageSourceDirectory", "src/test");
    imageRequest.setAbsoluteImagePath("resources/resize/nike_test_product_and_item_images_full01_hhp7330d.jpg");
    String image =
      fileStorageService.checkImageAvailabilityInFileStoreOrGcs(Arrays.asList(imageRequest));
    Assertions.assertNotNull(image);
    Mockito.verify(gcsProperties).isFileStoreToGcsMigrationCompleted();
    Mockito.verify(gcsProperties).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsService).isFileExists(Mockito.any(), Mockito.anyString());
  }

  @Test
  public void getImageRequestTest(){
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest1 =
      new ProductLevel3SummaryDetailsImageRequest();
    productLevel3SummaryDetailsImageRequest1.setLocationPath(FILE_PATH);
    ReflectionTestUtils.setField(fileStorageService, "fullImageSourceDirectory", "src/test"
      + "/resources/fullSize");
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    fileStorageService.getImageRequest(productLevel3SummaryDetailsImageRequest1);
    Mockito.verify(gcsProperties).getPathPrefix();
  }

  @Test
  public void getImageRequestWithGcsTest() {
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest1 =
      new ProductLevel3SummaryDetailsImageRequest();
    productLevel3SummaryDetailsImageRequest1
      .setLocationPath(GCS_PATH_PREFIX + Constants.DELIMITER_SLASH + FILE_PATH);
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    ReflectionTestUtils
      .setField(fileStorageService, "fullImageSourceDirectory", "src/test" + "/resources/fullSize");
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(FINAL_IMAGE_BUCKET_NAME);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(FINAL_FULL_IMAGE_SOURCE_DIR);
    fileStorageService.getImageRequest(productLevel3SummaryDetailsImageRequest1);
    Mockito.verify(gcsProperties).getPathPrefix();
    Mockito.verify(gcsProperties).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
  }

  @Test
  public void checkImageAvailabilityProductItemGcsActiveImageNotFoundTest(){
    image.setMarkForDelete(false);
    image.setActive(true);
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(FINAL_FULL_IMAGE_SOURCE_DIR);
    image.setLocationPath(GCS_PATH_PREFIX+ Constants.DELIMITER_SLASH + FILE_PATH);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(FINAL_FULL_IMAGE_SOURCE_DIR);
    productItemCreationRequest.setImages(Arrays.asList(image));
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    fileStorageService.checkImageAvailability(productCreationRequest, ProductCreationType.FLOW1.getProductCreationType());
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsService).isFileExists(Mockito.any(),Mockito.any());
  }

  @Test
  public void checkImageAvailabilityProductItemGcsActiveImageFoundTest(){
    image.setMarkForDelete(false);
    image.setActive(true);
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(FINAL_FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsService.isFileExists(Mockito.any(),Mockito.any())).thenReturn(true);
    image.setLocationPath(GCS_PATH_PREFIX+ Constants.DELIMITER_SLASH + FILE_PATH);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(true);
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(FINAL_FULL_IMAGE_SOURCE_DIR);
    productItemCreationRequest.setImages(Arrays.asList(image));
    productCreationRequest.setProductItemRequests(Collections.singletonList(productItemCreationRequest));
    fileStorageService.checkImageAvailability(productCreationRequest, ProductCreationType.FLOW1.getProductCreationType());
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsService).isFileExists(Mockito.any(),Mockito.any());
  }

  @Test
  public void generateFinalImageFullPathTest(){
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    ReflectionTestUtils.setField(fileStorageService, "fullImageSourceDirectory", "src/test");
    fileStorageService.generateFinalImageFullPath(FINAL_FULL_IMAGE_LOCATION_PATH);
    Mockito.verify(gcsProperties).getPathPrefix();
  }

  @Test
  public void generateFinalImageFullPathGCSTest(){
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(FINAL_FULL_IMAGE_SOURCE_DIR);
    ReflectionTestUtils.setField(fileStorageService, "fullImageSourceDirectory", "src/test");
    fileStorageService.generateFinalImageFullPath(GCS_PATH_PREFIX.concat(FINAL_FULL_IMAGE_LOCATION_PATH));
    Mockito.verify(gcsProperties).getPathPrefix();
    Mockito.verify(gcsProperties).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
  }

  @Test
  public void editImageNameIfGcsEnabledMtaApiEdit() {
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    productImageEditRequest.setImagePath(FILE_PATH);

    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(false);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_PATH_PREFIX);
    fileStorageService.editImageNameIfGcsEnabled(productImageEditRequest);

    productImageEditRequest.setImageAdded(true);
    fileStorageService.editImageNameIfGcsEnabled(productImageEditRequest);

    productImageEditRequest.setImageAdded(true);
    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(true);
    fileStorageService.editImageNameIfGcsEnabled(productImageEditRequest);

    Mockito.verify(gcsProperties).getPathPrefix();
    Mockito.verify(gcsProperties, Mockito.times(2)).isFinalImageEnabled();
  }

  @Test
  public void updateNewlyAddedImagePathGcsEnableTest() {
    ProductImage productImage1 = new ProductImage();
    productImage1.setLocationPath(IMAGE_1);
    ProductImage productImage2 = new ProductImage();
    productImage2.setLocationPath(IMAGE_2);
    ProductImage productImage3 = new ProductImage();
    productImage3.setLocationPath(LOCATION_PATH);
    productImage3.setActive(true);

    String id = UUID.randomUUID().toString();
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(IMAGE_1);
    productItemImage1.setId(id);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setLocationPath(IMAGE_2);
    productItemImage2.setId(id);
    ProductItemImage productItemImage3 = new ProductItemImage();
    productItemImage3.setLocationPath(LOCATION_PATH);
    productItemImage3.setActive(true);
    productItemImage3.setId(id);

    Product newProduct = new Product();
    newProduct.setProductImages(Arrays.asList(productImage1, productImage2, productImage3));
    ProductItem newProductItem = new ProductItem();
    newProductItem.setId(id);
    newProductItem.setProductItemImages(Arrays.asList(productItemImage1, productItemImage2, productItemImage3));
    newProduct.setProductItems(Arrays.asList(newProductItem));

    Product existingProduct = new Product();
    existingProduct.setProductImages(Arrays.asList(productImage1));
    ProductItem existingProductItem = new ProductItem();
    existingProductItem.setId(id);
    existingProductItem.setProductItemImages(Arrays.asList(productItemImage1));
    existingProduct.setProductItems(Arrays.asList(existingProductItem));

    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(CATALOG_IMAGE);

    fileStorageService.editImageNameIfGcsEnabled(newProduct, existingProduct);

    Mockito.verify(gcsProperties, Mockito.times(4)).isFinalImageEnabled();
    Mockito.verify(gcsProperties, Mockito.times(8)).getPathPrefix();

    Assertions.assertTrue(newProduct.getProductImages().get(1).getLocationPath().contains(CATALOG_IMAGE));
    Assertions.assertTrue(
        newProduct.getProductItems().get(0).getProductItemImages().get(1).getLocationPath().contains(CATALOG_IMAGE));
  }

  @Test
  public void updateNewlyAddedImagePathGcsDisableTest() {
    ProductImage  productImage1 = new ProductImage();
    productImage1.setLocationPath(IMAGE_1);
    ProductImage productImage2 = new ProductImage();
    productImage2.setLocationPath(IMAGE_2);
    ProductImage productImage3 = new ProductImage();
    productImage3.setLocationPath(LOCATION_PATH);
    productImage3.setActive(true);

    String id = UUID.randomUUID().toString();
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(IMAGE_1);
    productItemImage1.setId(id);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setLocationPath(IMAGE_2);
    productItemImage2.setId(id);
    ProductItemImage productItemImage3 = new ProductItemImage();
    productItemImage3.setLocationPath(LOCATION_PATH);
    productItemImage3.setActive(true);
    productItemImage3.setId(id);

    Product newProduct = new Product();
    newProduct.setProductImages(Arrays.asList(productImage1, productImage2, productImage3));
    ProductItem newProductItem = new ProductItem();
    newProductItem.setId(id);
    newProductItem.setProductItemImages(Arrays.asList(productItemImage1, productItemImage2, productItemImage3));
    newProduct.setProductItems(Arrays.asList(newProductItem));

    Product existingProduct = new Product();
    existingProduct.setProductImages(Arrays.asList(productImage1));
    ProductItem existingProductItem = new ProductItem();
    existingProductItem.setId(id);
    existingProductItem.setProductItemImages(Arrays.asList(productItemImage1));
    existingProduct.setProductItems(Arrays.asList(existingProductItem));

    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(false);

    fileStorageService.editImageNameIfGcsEnabled(newProduct, existingProduct);

    Mockito.verify(gcsProperties, Mockito.times(4)).isFinalImageEnabled();

    Assertions.assertFalse(newProduct.getProductImages().get(1).getLocationPath().contains(CATALOG_IMAGE));
    Assertions.assertFalse(
        newProduct.getProductItems().get(0).getProductItemImages().get(1).getLocationPath().contains(CATALOG_IMAGE));
  }

  @Test
  public void updateNewlyAddedImagePathGcsEnableEmptyProductItemImagesTest() {
    ProductImage  productImage1 = new ProductImage();
    productImage1.setLocationPath(IMAGE_1);
    ProductImage productImage2 = new ProductImage();
    productImage2.setLocationPath(IMAGE_2);
    ProductImage productImage3 = new ProductImage();
    productImage3.setLocationPath(LOCATION_PATH);
    productImage3.setActive(true);

    Product newProduct = new Product();
    newProduct.setProductImages(Arrays.asList(productImage1, productImage2, productImage3));
    ProductItem newProductItem = new ProductItem();
    newProduct.setProductItems(Arrays.asList(newProductItem));

    Product existingProduct = new Product();
    existingProduct.setProductImages(Arrays.asList(productImage1));
    ProductItem existingProductItem = new ProductItem();
    existingProduct.setProductItems(Arrays.asList(existingProductItem));

    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(false);

    fileStorageService.editImageNameIfGcsEnabled(newProduct, existingProduct);

    Mockito.verify(gcsProperties, Mockito.times(2)).isFinalImageEnabled();

    Assertions.assertFalse(newProduct.getProductImages().get(1).getLocationPath().contains(CATALOG_IMAGE));
  }

  @Test
  public void updateNewlyAddedImagePathGcsEnableImagePrefixAddedTest() {
    ProductImage  productImage1 = new ProductImage();
    productImage1.setLocationPath(CATALOG_IMAGE + IMAGE_1);
    ProductImage productImage2 = new ProductImage();
    productImage2.setLocationPath(CATALOG_IMAGE + IMAGE_2);
    ProductImage productImage3 = new ProductImage();
    productImage3.setLocationPath(CATALOG_IMAGE + LOCATION_PATH);
    productImage3.setActive(true);

    String id = UUID.randomUUID().toString();
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(CATALOG_IMAGE + IMAGE_1);
    productItemImage1.setId(id);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setLocationPath(CATALOG_IMAGE + IMAGE_2);
    productItemImage2.setId(id);
    ProductItemImage productItemImage3 = new ProductItemImage();
    productItemImage3.setLocationPath(CATALOG_IMAGE + LOCATION_PATH);
    productItemImage3.setActive(true);
    productItemImage3.setId(id);

    Product newProduct = new Product();
    newProduct.setProductImages(Arrays.asList(productImage1, productImage2, productImage3));
    ProductItem newProductItem = new ProductItem();
    newProductItem.setId(id);
    newProductItem.setProductItemImages(Arrays.asList(productItemImage1, productItemImage2, productItemImage3));
    newProduct.setProductItems(Arrays.asList(newProductItem));

    Product existingProduct = new Product();
    existingProduct.setProductImages(Arrays.asList(productImage1));
    ProductItem existingProductItem = new ProductItem();
    existingProductItem.setId(id);
    existingProductItem.setProductItemImages(Arrays.asList(productItemImage1));
    existingProduct.setProductItems(Arrays.asList(existingProductItem));

    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(CATALOG_IMAGE);

    fileStorageService.editImageNameIfGcsEnabled(newProduct, existingProduct);

    Mockito.verify(gcsProperties, Mockito.times(4)).isFinalImageEnabled();
    Mockito.verify(gcsProperties, Mockito.times(4)).getPathPrefix();

    Assertions.assertTrue(newProduct.getProductImages().get(1).getLocationPath().contains(CATALOG_IMAGE));
    Assertions.assertTrue(
        newProduct.getProductItems().get(0).getProductItemImages().get(1).getLocationPath().contains(CATALOG_IMAGE));

  }

  @Test
  public void updateNewlyAddedImagePathNoImageAndItemsTest() {
    Product newProduct = new Product();
    Product existingProduct = new Product();

    fileStorageService.editImageNameIfGcsEnabled(newProduct, existingProduct);
  }

  @Test
  public void validateImageExistsTest() {
    ReflectionTestUtils.setField(fileStorageService, "validateImageExistsOnInternalUpdate", true);
    ProductFieldHistory productFieldHistory1 = new ProductFieldHistory(null, null, null);
    ProductFieldHistory productFieldHistory2 = new ProductFieldHistory(Constants.PRODUCT_NAME, null, null);
    ProductFieldHistory productFieldHistory3 =
        new ProductFieldHistory(Constants.ITEM_IMAGES_ADDED, null, LOCATION_PATH);

    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(FINAL_FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getFinalImageBucketName()).thenReturn(BUCKET_NAME);
    Mockito.when(gcsService.isFileExists(BUCKET_NAME, FINAL_IMAGE_PATH)).thenReturn(true);

    fileStorageService.validateImageExists(
        Arrays.asList(productFieldHistory1, productFieldHistory2, productFieldHistory3));

    Mockito.verify(gcsProperties).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsService).isFileExists(BUCKET_NAME, FINAL_IMAGE_PATH);
  }

  @Test
  public void validateImageExistsNoImageAddedTest() {
    ReflectionTestUtils.setField(fileStorageService, "validateImageExistsOnInternalUpdate", true);
    ProductFieldHistory productFieldHistory1 = new ProductFieldHistory(null, null, null);
    ProductFieldHistory productFieldHistory2 = new ProductFieldHistory(Constants.PRODUCT_NAME, null, null);
    fileStorageService.validateImageExists(
        Arrays.asList(productFieldHistory1, productFieldHistory2));
  }

  @Test
  public void validateImageExistsSwitchOffTest() {
    ReflectionTestUtils.setField(fileStorageService, "validateImageExistsOnInternalUpdate", false);
    ProductFieldHistory productFieldHistory1 = new ProductFieldHistory(null, null, null);
    ProductFieldHistory productFieldHistory2 = new ProductFieldHistory(Constants.PRODUCT_NAME, null, null);
    fileStorageService.validateImageExists(
        Arrays.asList(productFieldHistory1, productFieldHistory2));

  }

  @Test
  public void validateImageExistsFileNotFoundTest() {
    ReflectionTestUtils.setField(fileStorageService, "validateImageExistsOnInternalUpdate", true);
    ProductFieldHistory productFieldHistory1 = new ProductFieldHistory(null, null, null);
    ProductFieldHistory productFieldHistory2 = new ProductFieldHistory(Constants.PRODUCT_NAME, null, null);
    ProductFieldHistory productFieldHistory3 =
        new ProductFieldHistory(Constants.ITEM_IMAGES_ADDED, null, LOCATION_PATH);

    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(FINAL_FULL_IMAGE_SOURCE_DIR);
    Mockito.when(gcsProperties.getFinalImageBucketName()).thenReturn(BUCKET_NAME);
    Mockito.when(gcsService.isFileExists(BUCKET_NAME, FINAL_IMAGE_PATH)).thenReturn(false);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        fileStorageService.validateImageExists(
            Arrays.asList(productFieldHistory1, productFieldHistory2, productFieldHistory3));
      });
    } finally {
      Mockito.verify(gcsProperties).getFinalImageDirectory();
      Mockito.verify(gcsProperties).getFinalFullImageDirectory();
      Mockito.verify(gcsProperties).getFinalImageBucketName();
      Mockito.verify(gcsService).isFileExists(BUCKET_NAME, FINAL_IMAGE_PATH);
    }
  }
}
