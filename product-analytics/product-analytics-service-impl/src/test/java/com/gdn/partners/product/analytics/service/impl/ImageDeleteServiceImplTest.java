package com.gdn.partners.product.analytics.service.impl;

import com.gdn.partners.product.analytics.entity.ImageDeletion;
import com.gdn.partners.product.analytics.model.enums.ImageDeleteResults;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.gdn.partners.product.analytics.repository.ImageDeletionRepository;
import com.gdn.partners.product.analytics.service.impl.helper.FileHelperImpl;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.Set;

@ExtendWith(MockitoExtension.class)
class ImageDeleteServiceImplTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String STORE_ID = "storeId";
  private static final String FINAL_IMAGE ="final-image";
  private static final String SOURCE_IMAGE = "source-image";
  private static final String IMAGE_PATH = "image-path";
  private static final String FINAL_BUCKET = "final-bucket";
  private static final Set<String> IMAGE_DELETION_STATUS_TO_FETCH =
      Set.of(new String[] {"PENDING", "PUBLISHED", "FAILED"});

  @InjectMocks
  private ImageDeleteServiceImpl imageDeleteService;

  @Mock
  private ImageDeletionRepository imageDeletionRepository;

  @Mock
  private GCPProperties gcpProperties;

  @Mock
  private FileHelperImpl fileHelper;
  private ImageDeletion imageDeletion;
  private static final Set<String> statusList =
    Set.of(ImageDeleteResults.PENDING.name(), ImageDeleteResults.FAILED.name(),
      ImageDeleteResults.PUBLISHED.name());

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(imageDeletionRepository);
  }

  @Test
  void testUpdateImageCollectionForProductDelete() {
    ImageDeletion imageDeletion = ImageDeletion.builder().productCode(PRODUCT_CODE).result(
      ImageDeleteResults.PENDING.name()).build();
    Mockito.when(imageDeletionRepository.save(imageDeletion)).thenReturn(imageDeletion);
    imageDeleteService.updateImageCollectionForProductDelete(PRODUCT_CODE);
    Mockito.verify(imageDeletionRepository).save(imageDeletion);
  }

  @Test
  void deleteImagesOfProductPublishedTimeNotLapsedTest() {
    ReflectionTestUtils.setField(imageDeleteService, "imageDeleteMinutesDifference", 30);
    ReflectionTestUtils.setField(imageDeleteService, "imageDeleteStatusToFetch", IMAGE_DELETION_STATUS_TO_FETCH);
    imageDeletion = new ImageDeletion();
    imageDeletion.setResult(ImageDeleteResults.PUBLISHED.name());
    imageDeletion.setUpdatedDate(new Date());
    Mockito.when(imageDeletionRepository.fetchProductCodeForImageDeletion(STORE_ID, statusList))
      .thenReturn(Collections.singletonList(imageDeletion));
    imageDeleteService.deleteImagesOfProduct(STORE_ID, StringUtils.EMPTY);
    Mockito.verify(imageDeletionRepository).fetchProductCodeForImageDeletion(STORE_ID, statusList);
  }

  @Test
  void deleteImagesOfProductPublishedTimeLapsedTest() {
    Calendar calendar = Calendar.getInstance();
    calendar.set(Calendar.YEAR, 2024);
    calendar.set(Calendar.MONTH, Calendar.FEBRUARY);
    calendar.set(Calendar.DAY_OF_MONTH, 13);
    Date date = calendar.getTime();

    ReflectionTestUtils.setField(imageDeleteService, "imageDeleteMinutesDifference", 30);
    ReflectionTestUtils.setField(imageDeleteService, "imageDeleteStatusToFetch", IMAGE_DELETION_STATUS_TO_FETCH);
    imageDeletion = new ImageDeletion();
    imageDeletion.setResult(ImageDeleteResults.PUBLISHED.name());
    imageDeletion.setUpdatedDate(date);
    imageDeletion.setProductCode(PRODUCT_CODE);
    Mockito.when(gcpProperties.getFinalImagePrefix()).thenReturn(FINAL_IMAGE);
    Mockito.when(gcpProperties.getCatalogImagePathPrefix()).thenReturn(IMAGE_PATH);
    Mockito.when(gcpProperties.getFinalImageBucketName()).thenReturn(FINAL_BUCKET);
    Mockito.when(imageDeletionRepository.fetchProductCodeForImageDeletion(STORE_ID, statusList))
      .thenReturn(Collections.singletonList(imageDeletion));
    imageDeleteService.deleteImagesOfProduct(STORE_ID, StringUtils.EMPTY);
    Mockito.verify(imageDeletionRepository).fetchProductCodeForImageDeletion(STORE_ID, statusList);
    Mockito.verify(gcpProperties, Mockito.times(3)).getFinalImagePrefix();
    Mockito.verify(gcpProperties, Mockito.times(3)).getFinalImageBucketName();
    Mockito.verify(gcpProperties, Mockito.times(4)).getCatalogImagePathPrefix();
    Mockito.verify(imageDeletionRepository, Mockito.times(2)).save(imageDeletion);
  }

  @Test
  void deleteImagesOfProductPendingTest() {
    ReflectionTestUtils.setField(imageDeleteService, "imageDeleteMinutesDifference", 30);
    ReflectionTestUtils.setField(imageDeleteService, "imageDeleteStatusToFetch", IMAGE_DELETION_STATUS_TO_FETCH);
    imageDeletion = new ImageDeletion();
    imageDeletion.setResult(ImageDeleteResults.PENDING.name());
    imageDeletion.setUpdatedDate(new Date());
    imageDeletion.setProductCode("12");
    Mockito.when(gcpProperties.getFinalImagePrefix()).thenReturn(FINAL_IMAGE);
    Mockito.when(gcpProperties.getCatalogImagePathPrefix()).thenReturn(IMAGE_PATH);
    Mockito.when(gcpProperties.getFinalImageBucketName()).thenReturn(FINAL_BUCKET);
    Mockito.when(imageDeletionRepository.fetchProductCodeForImageDeletion(STORE_ID, statusList))
      .thenReturn(Collections.singletonList(imageDeletion));
    imageDeleteService.deleteImagesOfProduct(STORE_ID, StringUtils.EMPTY);
    Mockito.verify(imageDeletionRepository).fetchProductCodeForImageDeletion(STORE_ID, statusList);
    Mockito.verify(gcpProperties, Mockito.times(3)).getFinalImagePrefix();
    Mockito.verify(gcpProperties, Mockito.times(3)).getFinalImageBucketName();
    Mockito.verify(gcpProperties, Mockito.times(4)).getCatalogImagePathPrefix();
    Mockito.verify(imageDeletionRepository, Mockito.times(2)).save(imageDeletion);
  }

  @Test
  void deleteImagesOfProductCodeSpecificTest() {
    ReflectionTestUtils.setField(imageDeleteService, "imageDeleteMinutesDifference", 30);
    ReflectionTestUtils.setField(imageDeleteService, "imageDeleteStatusToFetch", IMAGE_DELETION_STATUS_TO_FETCH);
    imageDeletion = new ImageDeletion();
    imageDeletion.setResult(ImageDeleteResults.PENDING.name());
    imageDeletion.setUpdatedDate(new Date());
    imageDeletion.setProductCode(PRODUCT_CODE);
    Mockito.when(gcpProperties.getFinalImagePrefix()).thenReturn(FINAL_IMAGE);
    Mockito.when(gcpProperties.getCatalogImagePathPrefix()).thenReturn(IMAGE_PATH);
    Mockito.when(gcpProperties.getFinalImageBucketName()).thenReturn(FINAL_BUCKET);
    Mockito.when(gcpProperties.getSourceImagePrefix()).thenReturn(SOURCE_IMAGE);
    imageDeleteService.deleteImagesOfProduct(STORE_ID, PRODUCT_CODE);
    imageDeletion.setResult(ImageDeleteResults.SUCCESS.name());
    Mockito.verify(gcpProperties, Mockito.times(3)).getFinalImagePrefix();
    Mockito.verify(gcpProperties, Mockito.times(3)).getFinalImageBucketName();
    Mockito.verify(gcpProperties, Mockito.times(4)).getCatalogImagePathPrefix();
    Mockito.verify(gcpProperties).getSourceImagePrefix();
    Mockito.verify(imageDeletionRepository, Mockito.times(2)).save(imageDeletion);
  }
}