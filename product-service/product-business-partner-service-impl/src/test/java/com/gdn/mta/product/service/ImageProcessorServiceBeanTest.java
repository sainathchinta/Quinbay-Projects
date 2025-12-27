package com.gdn.mta.product.service;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

import com.gdn.mta.product.service.util.ImageCheckService;
import org.apache.commons.io.FilenameUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
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

import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.micro.graphics.web.model.BulkImagesProcessRequest;
import com.gdn.micro.graphics.web.model.BulkResizeImageRequest;
import com.gdn.micro.graphics.web.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.web.model.ImageRequest;
import com.gdn.micro.graphics.web.model.ScaleEditedImageRequest;
import com.gdn.mta.product.repository.ImageProcessorRepository;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

public class ImageProcessorServiceBeanTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String STORE_ID = "storeId";
  private static final String SOURCE_DIRECTORY = "/tmp";
  private static final String IMAGE_LOCATION_PATH_1 = "path1";
  private static final String IMAGE_LOCATION_PATH_2 = "path2";
  private static final String IMAGE_LOCATION_PATH_3 = "path3";
  private static final String IMAGE_LOCATION_PATH_4 = "path4";
  private static final String IMAGE_HASH_CODE_1 = "hashCode1";
  private static final String IMAGE_HASH_CODE_2 = "hashCode2";
  private static final String IMAGE_HASH_CODE_3 = "hashCode3";
  private static final String IMAGE_HASH_CODE_4 = "hashCode4";

  @InjectMocks
  private ImageProcessorServiceBean imageProcessorServiceBean;

  @Mock
  private ImageProcessorRepository imageProcessorRepository;

  @Mock
  private ProductService productService;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private ImageCheckService imageCheckService;

  private ProductDetailResponse productDetailResponse;

  private BulkResizeImageRequest bulkResizeImageRequest;

  @Captor
  private ArgumentCaptor<BulkResizeImageRequest> bulkResizeImageRequestArgumentCaptor;

  @BeforeEach
  public void before() {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(imageProcessorServiceBean, "imageSourceDirectory", SOURCE_DIRECTORY);
    ReflectionTestUtils.setField(imageProcessorServiceBean, "customGraphicsSettings", new CustomGraphicsSettings());
    productDetailResponse = new ProductDetailResponse();
    Image image = new Image();
    image.setLocationPath(IMAGE_LOCATION_PATH_1);
    image.setHashCode(IMAGE_HASH_CODE_1);
    image.setOriginalImage(true);
    Image image1 = new Image();
    image1.setLocationPath(IMAGE_LOCATION_PATH_2);
    image1.setHashCode(IMAGE_HASH_CODE_2);
    image1.setOriginalImage(true);
    Image image2 = new Image();
    image2.setLocationPath(IMAGE_LOCATION_PATH_3);
    image2.setHashCode(IMAGE_HASH_CODE_3);
    image2.setOriginalImage(true);
    Image image3 = new Image();
    image3.setLocationPath(IMAGE_LOCATION_PATH_4);
    image3.setHashCode(IMAGE_HASH_CODE_4);
    image3.setOriginalImage(true);
    productDetailResponse.setImages(Arrays.asList(image, image1, image2, image3));
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setImages(Arrays.asList(image, image1));
    ProductItemResponse productItemResponse1 = new ProductItemResponse();
    productItemResponse1.setImages(Arrays.asList(image2, image1));
    productDetailResponse.setProductItemResponses(new HashSet<>(Arrays.asList(productItemResponse, productItemResponse1)));

    List<ImageRequest> imageRequests = new ArrayList<>();
    ImageRequest imageRequest1 = new ImageRequest();
    imageRequest1.setImageName(IMAGE_LOCATION_PATH_1);
    imageRequest1.setAbsoluteImagePath( SOURCE_DIRECTORY + File.separator +IMAGE_LOCATION_PATH_1);
    imageRequest1.setHashCode(IMAGE_HASH_CODE_1);
    ImageRequest imageRequest2 = new ImageRequest();
    imageRequest2.setImageName(IMAGE_LOCATION_PATH_2);
    imageRequest2.setAbsoluteImagePath( SOURCE_DIRECTORY + File.separator +IMAGE_LOCATION_PATH_2);
    imageRequest1.setHashCode(IMAGE_HASH_CODE_2);
    ImageRequest imageRequest3 = new ImageRequest();
    imageRequest3.setImageName(IMAGE_LOCATION_PATH_3);
    imageRequest3.setAbsoluteImagePath( SOURCE_DIRECTORY + File.separator +IMAGE_LOCATION_PATH_3);
    imageRequest1.setHashCode(IMAGE_HASH_CODE_3);
    ImageRequest imageRequest4 = new ImageRequest();
    imageRequest4.setImageName(IMAGE_LOCATION_PATH_4);
    imageRequest4.setAbsoluteImagePath( SOURCE_DIRECTORY + File.separator +IMAGE_LOCATION_PATH_4);
    imageRequest1.setHashCode(IMAGE_HASH_CODE_4);
    bulkResizeImageRequest = new BulkResizeImageRequest();
    bulkResizeImageRequest.setGroupCode(PRODUCT_CODE);
    bulkResizeImageRequest.setImageRequests(Arrays.asList(imageRequest1,imageRequest2,imageRequest3,imageRequest4));

  }

  @AfterEach
  public void after() {
    Mockito.verifyNoMoreInteractions(imageProcessorRepository, productService, fileStorageService
      , imageCheckService);
  }


  @Test
  public void scaleImageTest() throws Exception {
    Mockito.doNothing().when(imageProcessorRepository).scaleImage(Mockito.any(BulkImagesProcessRequest.class));
    imageProcessorServiceBean.scaleImage(new BulkImagesProcessRequest());
    Mockito.verify(imageProcessorRepository).scaleImage(Mockito.any(BulkImagesProcessRequest.class));
  }

  @Test
  public void scaleImageFaliureTest() throws Exception {
    Mockito.doThrow(new RuntimeException()).when(imageProcessorRepository)
        .scaleImage(Mockito.any(BulkImagesProcessRequest.class));
    try {
      imageProcessorServiceBean.scaleImage(new BulkImagesProcessRequest());
    } catch (Exception ex) {
      Mockito.verify(imageProcessorRepository).scaleImage(Mockito.any(BulkImagesProcessRequest.class));
    }
  }

  @Test
  public void resizeImageTest() throws Exception {
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE)).thenReturn(productDetailResponse);
    generateDummyImageFiles(productDetailResponse);
    Mockito.doNothing().when(imageProcessorRepository).resizeImage(Mockito.any(BulkResizeImageRequest.class));
    productDetailResponse.getImages().forEach(image -> image.setCommonImage(true));
    productDetailResponse.getProductItemResponses()
        .forEach(item -> item.getImages().forEach(image -> image.setCommonImage(true)));
    imageProcessorServiceBean.resizeImage(STORE_ID, PRODUCT_CODE, 0);
    Mockito.verify(imageProcessorRepository).resizeImage(bulkResizeImageRequestArgumentCaptor.capture());
    deleteDummyFiles(productDetailResponse);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(imageCheckService).getUniqueImages(Mockito.any(),Mockito.anyMap(),
      Mockito.anyString());
    Assertions.assertEquals(PRODUCT_CODE, bulkResizeImageRequestArgumentCaptor.getValue().getGroupCode());
    Assertions.assertFalse(bulkResizeImageRequestArgumentCaptor.getValue().getImageRequests().stream()
        .anyMatch(imageRequest -> !imageRequest.isCommonImage()));
  }

  @Test
  public void resizeImageAlreadyResize1Test() throws Exception {
    ReflectionTestUtils.setField(imageProcessorServiceBean, "checkResizeAlreadyDone", true);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE))
        .thenReturn(productDetailResponse);
    generateDummyImageFiles(productDetailResponse);
    Mockito.doNothing().when(imageProcessorRepository).resizeImage(Mockito.any(BulkResizeImageRequest.class));
    productDetailResponse.getImages().forEach(image -> image.setOriginalImage(true));
    productDetailResponse.getProductItemResponses()
        .forEach(item -> item.getImages().forEach(image -> image.setCommonImage(true)));
    imageProcessorServiceBean.resizeImage(STORE_ID, PRODUCT_CODE, 0);
    Mockito.verify(imageProcessorRepository).resizeImage(bulkResizeImageRequestArgumentCaptor.capture());
    deleteDummyFiles(productDetailResponse);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(imageCheckService).getUniqueImages(Mockito.any(), Mockito.anyMap(), Mockito.anyString());
    Assertions.assertEquals(PRODUCT_CODE, bulkResizeImageRequestArgumentCaptor.getValue().getGroupCode());
    Assertions.assertFalse(bulkResizeImageRequestArgumentCaptor.getValue().getImageRequests().stream()
        .anyMatch(imageRequest -> !imageRequest.isCommonImage()));
  }

  @Test
  public void resizeImageAlreadyResizeTest() throws Exception {
    ReflectionTestUtils.setField(imageProcessorServiceBean, "checkResizeAlreadyDone", true);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE))
        .thenReturn(productDetailResponse);
    generateDummyImageFiles(productDetailResponse);
    Mockito.doNothing().when(imageProcessorRepository).resizeImage(Mockito.any(BulkResizeImageRequest.class));
    productDetailResponse.getImages().forEach(image -> image.setOriginalImage(false));
    productDetailResponse.getImages().get(0).setMarkForDelete(true);
    productDetailResponse.getProductItemResponses()
        .forEach(item -> item.getImages().forEach(image -> image.setCommonImage(true)));
    boolean publishImages = imageProcessorServiceBean.resizeImage(STORE_ID, PRODUCT_CODE, 0);
    deleteDummyFiles(productDetailResponse);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Assertions.assertTrue(publishImages);
  }

  @Test
  public void resizeImageMarkForDeleteTrueTest() throws Exception {
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE)).thenReturn(productDetailResponse);
    generateDummyImageFiles(productDetailResponse);
    productDetailResponse.getImages().forEach(image -> image.setMarkForDelete(true));
    productDetailResponse.getProductItemResponses()
        .forEach(productItemRequest -> productItemRequest.getImages().forEach(image -> image.setMarkForDelete(true)));
    Mockito.doNothing().when(imageProcessorRepository).resizeImage(Mockito.any(BulkResizeImageRequest.class));
    productDetailResponse.getImages().forEach(image -> image.setCommonImage(true));
    productDetailResponse.getProductItemResponses()
        .forEach(item -> item.getImages().forEach(image -> image.setCommonImage(true)));
    imageProcessorServiceBean.resizeImage(STORE_ID, PRODUCT_CODE, 0);
    Mockito.verify(imageProcessorRepository).resizeImage(bulkResizeImageRequestArgumentCaptor.capture());
    deleteDummyFiles(productDetailResponse);
    Mockito.verify(imageCheckService).getUniqueImages(Mockito.any(),Mockito.anyMap(),
      Mockito.anyString());
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
  }

  @Test
  public void resizeImageOriginalImageFalseTest() throws Exception {
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE)).thenReturn(productDetailResponse);
    generateDummyImageFiles(productDetailResponse);
    productDetailResponse.getImages().forEach(image -> image.setOriginalImage(false));
    productDetailResponse.getProductItemResponses()
        .forEach(productItemRequest -> productItemRequest.getImages().forEach(image -> image.setOriginalImage(false)));
    Mockito.doNothing().when(imageProcessorRepository).resizeImage(Mockito.any(BulkResizeImageRequest.class));
    productDetailResponse.getImages().forEach(image -> image.setCommonImage(true));
    productDetailResponse.getProductItemResponses()
        .forEach(item -> item.getImages().forEach(image -> image.setCommonImage(true)));
    imageProcessorServiceBean.resizeImage(STORE_ID, PRODUCT_CODE, 0);
    Mockito.verify(imageProcessorRepository).resizeImage(bulkResizeImageRequestArgumentCaptor.capture());
    deleteDummyFiles(productDetailResponse);
    Mockito.verify(imageCheckService).getUniqueImages(Mockito.any(),Mockito.anyMap(),
      Mockito.anyString());
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
  }

  @Test
  public void resizeImageFailureTest() throws Exception {
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE)).thenReturn(productDetailResponse);
    generateDummyImageFiles(productDetailResponse);
    try {
      imageProcessorServiceBean.resizeImage(STORE_ID, PRODUCT_CODE, 0);
    } catch (Exception ex) {
    } finally {
      Mockito.verify(imageProcessorRepository).resizeImage(bulkResizeImageRequestArgumentCaptor.capture());
      Mockito.verify(imageCheckService).getUniqueImages(Mockito.any(),Mockito.anyMap(),
        Mockito.anyString());
      Assertions.assertEquals(PRODUCT_CODE, bulkResizeImageRequestArgumentCaptor.getValue().getGroupCode());
      Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
      deleteDummyFiles(productDetailResponse);
    }
  }

  @Test
  public void resizeImageNotPresentTest() throws Exception {
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE))
        .thenReturn(productDetailResponse);
    Mockito.when(imageCheckService.getUniqueImages(Mockito.any(),Mockito.anyMap(),
      Mockito.anyString())).thenReturn(SOURCE_DIRECTORY);
    try {
      imageProcessorServiceBean.resizeImage(STORE_ID, PRODUCT_CODE, 0);
    } catch (Exception ex) {
    } finally {
      Mockito.verify(imageCheckService).getUniqueImages(Mockito.any(),Mockito.anyMap(),
        Mockito.anyString());
      Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    }
  }

  @Test
  public void resizeEditedImageTest() throws Exception {
    generateDummyImageFiles(productDetailResponse);
    Mockito.doNothing().when(imageProcessorRepository).resizeEditedImage(Mockito.any(BulkResizeImageRequest.class));
    imageProcessorServiceBean.resizeEditedImage(bulkResizeImageRequest);
    Mockito.verify(imageProcessorRepository).resizeEditedImage(bulkResizeImageRequestArgumentCaptor.capture());
    Mockito.verify(fileStorageService).checkImageAvailability(Mockito.anyList(), Mockito.anyBoolean());
    deleteDummyFiles(productDetailResponse);
    Assertions.assertEquals(PRODUCT_CODE, bulkResizeImageRequestArgumentCaptor.getValue().getGroupCode());
    Assertions.assertEquals(4, bulkResizeImageRequestArgumentCaptor.getValue().getImageRequests().size());
    Mockito.verify(fileStorageService).checkImageAvailability(Mockito.anyList(), Mockito.anyBoolean());
  }

  @Test
  public void resizeEditedImageFailureTest() throws Exception {
    generateDummyImageFiles(productDetailResponse);
    Mockito.doThrow(ApplicationException.class).when(imageProcessorRepository)
        .resizeEditedImage(Mockito.any(BulkResizeImageRequest.class));
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        imageProcessorServiceBean.resizeEditedImage(bulkResizeImageRequest);
      });
    } finally {
      Mockito.verify(imageProcessorRepository).resizeEditedImage(bulkResizeImageRequestArgumentCaptor.capture());
      Mockito.verify(fileStorageService).checkImageAvailability(Mockito.anyList(), Mockito.anyBoolean());
      Assertions.assertEquals(PRODUCT_CODE, bulkResizeImageRequestArgumentCaptor.getValue().getGroupCode());
      Assertions.assertEquals(4, bulkResizeImageRequestArgumentCaptor.getValue().getImageRequests().size());
      deleteDummyFiles(productDetailResponse);
    }
  }

  @Test
  public void scaleEditedImageTest() throws Exception {
    Mockito.doNothing().when(imageProcessorRepository).scaleEditedImages(Mockito.any(ScaleEditedImageRequest.class));
    imageProcessorServiceBean.scaleEditedImages(new ScaleEditedImageRequest());
    Mockito.verify(imageProcessorRepository).scaleEditedImages(Mockito.any(ScaleEditedImageRequest.class));
  }

  @Test
  public void scaleEditedImageFaliureTest() throws Exception {
    Mockito.doThrow(new RuntimeException()).when(imageProcessorRepository)
        .scaleEditedImages(Mockito.any(ScaleEditedImageRequest.class));
    try {
      imageProcessorServiceBean.scaleEditedImages(new ScaleEditedImageRequest());
    } catch (Exception ex) {
      Mockito.verify(imageProcessorRepository).scaleEditedImages(Mockito.any(ScaleEditedImageRequest.class));
    }
  }

  @Test
  public void resizeEditedImageNotPresentTest() throws Exception {
    Mockito
      .when(fileStorageService.checkImageAvailability(bulkResizeImageRequest.getImageRequests(),
        true))
      .thenReturn(SOURCE_DIRECTORY);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        imageProcessorServiceBean.resizeEditedImage(bulkResizeImageRequest);
      });
    } finally {
      Mockito
        .verify(fileStorageService).checkImageAvailability(bulkResizeImageRequest.getImageRequests(),
          true);
    }
  }

  private void generateDummyImageFiles(ProductDetailResponse productDetailResponse) {
    productDetailResponse.getImages().forEach(productImage -> {
      try {
        new File(SOURCE_DIRECTORY, FilenameUtils.getName(productImage.getLocationPath())).createNewFile();
      } catch (IOException e) {
        e.printStackTrace();
      }
    });
    productDetailResponse.getProductItemResponses().forEach(productItemRequest -> {
      productItemRequest.getImages().forEach(image -> {
        try {
          new File(SOURCE_DIRECTORY, FilenameUtils.getName(image.getLocationPath())).createNewFile();
        } catch (IOException e) {
          e.printStackTrace();
        }
      });
    });
  }

  private void deleteDummyFiles(ProductDetailResponse productDetailResponse) {
    productDetailResponse.getImages().forEach(productImage -> {
      new File(SOURCE_DIRECTORY, FilenameUtils.getName(productImage.getLocationPath())).delete();
    });
    productDetailResponse.getProductItemResponses().forEach(productItemRequest -> {
      productItemRequest.getImages().forEach(image -> {
        new File(SOURCE_DIRECTORY, FilenameUtils.getName(image.getLocationPath())).delete();
      });
    });
  }

  @Test
  public void resizeRevisedImageTest() throws Exception {
    generateDummyImageFiles(productDetailResponse);
    Mockito.doNothing().when(imageProcessorRepository).resizeRevisedImage(Mockito.any(BulkResizeImageRequest.class));
    imageProcessorServiceBean.resizeRevisedImage(bulkResizeImageRequest);
    Mockito.verify(imageProcessorRepository).resizeRevisedImage(bulkResizeImageRequestArgumentCaptor.capture());
    Mockito.verify(fileStorageService).checkImageAvailability(Mockito.anyList(), Mockito.anyBoolean());
    deleteDummyFiles(productDetailResponse);
    Assertions.assertEquals(PRODUCT_CODE, bulkResizeImageRequestArgumentCaptor.getValue().getGroupCode());
    Assertions.assertEquals(4, bulkResizeImageRequestArgumentCaptor.getValue().getImageRequests().size());
  }

  @Test
  public void resizeRevisedImageExceptionTest() throws Exception {
    generateDummyImageFiles(productDetailResponse);
    Mockito.doThrow(ApplicationException.class).when(imageProcessorRepository)
        .resizeRevisedImage(Mockito.any(BulkResizeImageRequest.class));
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        imageProcessorServiceBean.resizeRevisedImage(bulkResizeImageRequest);
      });
    } finally {
      Mockito.verify(imageProcessorRepository).resizeRevisedImage(bulkResizeImageRequestArgumentCaptor.capture());
      Mockito.verify(fileStorageService).checkImageAvailability(Mockito.anyList(), Mockito.anyBoolean());
      Assertions.assertEquals(PRODUCT_CODE, bulkResizeImageRequestArgumentCaptor.getValue().getGroupCode());
      Assertions.assertEquals(4, bulkResizeImageRequestArgumentCaptor.getValue().getImageRequests().size());
      deleteDummyFiles(productDetailResponse);
    }
  }

  @Test
  public void resizeRevisedImageImageNotPresentTest() throws Exception {
    Mockito
      .when(fileStorageService.checkImageAvailability(bulkResizeImageRequest.getImageRequests(),
        false))
      .thenReturn(SOURCE_DIRECTORY);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        imageProcessorServiceBean.resizeRevisedImage(bulkResizeImageRequest);
      });
    } finally {
      Mockito.verify(fileStorageService)
        .checkImageAvailability(bulkResizeImageRequest.getImageRequests(), false);
    }
  }

  @Test
  public void resizeEditedImageInFileStoreOrGcsTest() throws Exception {
    generateDummyImageFiles(productDetailResponse);
    Mockito.doNothing().when(imageProcessorRepository).resizeEditedImage(Mockito.any(BulkResizeImageRequest.class));
    imageProcessorServiceBean.resizeEditedImageInFileStoreOrGcs(bulkResizeImageRequest);
    Mockito.verify(imageProcessorRepository).resizeEditedImage(bulkResizeImageRequestArgumentCaptor.capture());
    Mockito.verify(fileStorageService).checkImageAvailabilityInFileStoreOrGcs(Mockito.anyList());
    deleteDummyFiles(productDetailResponse);
    Assertions.assertEquals(PRODUCT_CODE, bulkResizeImageRequestArgumentCaptor.getValue().getGroupCode());
    Assertions.assertEquals(4, bulkResizeImageRequestArgumentCaptor.getValue().getImageRequests().size());
  }

  @Test
  public void resizeEditedImageInFileStoreOrGcsFailureTest() throws Exception {
    generateDummyImageFiles(productDetailResponse);
    Mockito.doThrow(ApplicationException.class).when(imageProcessorRepository)
      .resizeEditedImage(Mockito.any(BulkResizeImageRequest.class));
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        imageProcessorServiceBean.resizeEditedImageInFileStoreOrGcs(bulkResizeImageRequest);
      });
    } finally {
      Mockito.verify(imageProcessorRepository).resizeEditedImage(bulkResizeImageRequestArgumentCaptor.capture());
      Mockito.verify(fileStorageService).checkImageAvailabilityInFileStoreOrGcs(Mockito.anyList());
      Assertions.assertEquals(PRODUCT_CODE, bulkResizeImageRequestArgumentCaptor.getValue().getGroupCode());
      Assertions.assertEquals(4, bulkResizeImageRequestArgumentCaptor.getValue().getImageRequests().size());
      deleteDummyFiles(productDetailResponse);
    }
  }

  @Test
  public void resizeEditedImageInFileStoreOrGcsImageNotPresentTest() throws Exception {
    Mockito
      .when(fileStorageService.checkImageAvailabilityInFileStoreOrGcs(bulkResizeImageRequest.getImageRequests()))
      .thenReturn(SOURCE_DIRECTORY);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        imageProcessorServiceBean.resizeEditedImageInFileStoreOrGcs(bulkResizeImageRequest);
      });
    } finally {
      Mockito
        .verify(fileStorageService).checkImageAvailabilityInFileStoreOrGcs(bulkResizeImageRequest.getImageRequests());
    }
  }
}
