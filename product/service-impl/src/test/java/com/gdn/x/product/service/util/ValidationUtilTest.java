package com.gdn.x.product.service.util;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import com.gdn.common.exception.ApplicationRuntimeException;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.service.exceptions.ValidationException;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.google.api.services.youtube.YouTube;
import com.google.api.services.youtube.model.PageInfo;
import com.google.api.services.youtube.model.VideoListResponse;

public class ValidationUtilTest {

  private static final String YOUTUBE_URL = "https://www.youtube.com/watch?v=P1xAhgKTqDA";
  private static final String YOUTUBE_INVALID_URL = "https://www.youtube.com/watch?v";
  private static final String WRONG_URL = "https://www.mp4.com/watch?v=P1xAhgKTqDA";
  private static final String API_KEY = "apiKey";
  private static final String VIDEO_LIST = "snippet,contentDetails,statistics";
  private static final String PRODUCT_SKU = "productSku";

  @Mock
  private YouTube youTube;

  @Mock
  private YouTube.Videos videos;

  @Mock
  private YouTube.Videos.List list;
  private VideoListResponse videoListResponse;
  private Product product;

  @BeforeEach
  public void before() {
    MockitoAnnotations.openMocks(this);
    videoListResponse = new VideoListResponse();
    PageInfo pageInfo = new PageInfo();
    pageInfo.setTotalResults(2);
    videoListResponse.setPageInfo(pageInfo);
    product = new Product();
    product.setProductSku(PRODUCT_SKU);
  }

  @Test
  public void validateYouTubeUrlTest() throws Exception {
    Mockito.when(youTube.videos()).thenReturn(videos);
    Mockito.when(youTube.videos().list(Collections.singletonList(VIDEO_LIST))).thenReturn(list);
    Mockito.when(list.execute()).thenReturn(videoListResponse);
    boolean youTubeUrlResponse = ValidationUtil.validateYouTubeUrl(YOUTUBE_URL, API_KEY, youTube, true);
    Mockito.verify(youTube, Mockito.times(2)).videos();
    Mockito.verify(list).execute();
    Assertions.assertTrue(youTubeUrlResponse);
  }

  @Test
  public void validateYouTubeUrlWhenUrlValidationSwitchFalseAndUrlValidTest() throws Exception {
    boolean youTubeUrlResponse = ValidationUtil.validateYouTubeUrl(YOUTUBE_URL, API_KEY, youTube, false);
    Assertions.assertTrue(youTubeUrlResponse);
  }

  @Test
  public void validateYouTubeUrlWithInvalidUrlTest() throws Exception {
    boolean youTubeUrlResponse = ValidationUtil.validateYouTubeUrl(YOUTUBE_INVALID_URL, API_KEY, youTube, true);
    Assertions.assertFalse(youTubeUrlResponse);
  }

  @Test
  public void validateYouTubeUrlWithBlankUrlTest() throws Exception {
    boolean youTubeUrlResponse = ValidationUtil.validateYouTubeUrl(StringUtils.EMPTY, API_KEY, youTube, true);
    Assertions.assertTrue(youTubeUrlResponse);
  }

  @Test
  public void isYouTubeUrlValidExceptionTest() throws Exception {
    Mockito.when(youTube.videos()).thenReturn(videos);
    Mockito.when(youTube.videos().list(Collections.singletonList(VIDEO_LIST))).thenReturn(list);
    Mockito.doThrow(ApplicationRuntimeException.class).when(list).execute();
    boolean youTubeUrlResponse = ValidationUtil.validateYouTubeUrl(YOUTUBE_URL, API_KEY, youTube, true);
    Mockito.verify(youTube, Mockito.times(2)).videos();
    Mockito.verify(list).execute();
    Assertions.assertFalse(youTubeUrlResponse);
  }

  @Test
  public void validateYouTubeUrlWhenUrlIsInActiveTest() throws Exception {
    videoListResponse = new VideoListResponse();
    PageInfo pageInfo = new PageInfo();
    pageInfo.setTotalResults(0);
    videoListResponse.setPageInfo(pageInfo);
    Mockito.when(youTube.videos()).thenReturn(videos);
    Mockito.when(youTube.videos().list(Collections.singletonList(VIDEO_LIST))).thenReturn(list);
    Mockito.when(list.execute()).thenReturn(videoListResponse);
    boolean youTubeUrlResponse = ValidationUtil.validateYouTubeUrl(YOUTUBE_URL, API_KEY, youTube, true);
    Mockito.verify(youTube, Mockito.times(2)).videos();
    Mockito.verify(list).execute();
    Assertions.assertFalse(youTubeUrlResponse);
  }

  @Test
  public void validateYouTubeUrlWithWrongUrlTest() throws Exception {
    boolean youTubeUrlResponse = ValidationUtil.validateYouTubeUrl(WRONG_URL, API_KEY, youTube, true);
    Assertions.assertFalse(youTubeUrlResponse);
  }

  @Test
  public void validateProductTest() {
    ValidationUtil.validateProduct(product);
  }

  @Test
  public void validateProductSuspensionTest() {
    product.setSuspended(true);
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> ValidationUtil.validateProduct(product));
  }

  @Test
  public void validateProductTakenDownTest() {
    product.setTakenDown(true);
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> ValidationUtil.validateProduct(product));
  }

  public void validateProductDeletedTest() {
    product.setMarkForDelete(true);
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> ValidationUtil.validateProduct(product));
  }

  @Test
  public void validateProductArchivedTest() {
    product.setArchived(true);
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> ValidationUtil.validateProduct(product));
  }

  @Test
  public void checkParameterExceptionTest() {
    Assertions.assertThrows(ValidationException.class, () -> ValidationUtil.checkParameter(false, ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_BLANK));
  }

  @Test
  public void checkParameterTest() {
    ValidationUtil.checkParameter(true, ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_BLANK);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.youTube);
  }

  @Test
  public void validateProductBasicInfoBatchSize() {
    List<String> productCodeList = Arrays.asList("code1", "code2");
    ValidationUtil.validateProductBasicInfoBatchSize(productCodeList, 10);
  }

  @Test
  public void validateProductBasicInfoBatchSize_False_Case() {
    List<String> productCodeList = Arrays.asList("code1", "code2");
    try {
      ValidationUtil.validateProductBasicInfoBatchSize(productCodeList, 1);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertTrue(e.getErrorMessage().contains(ErrorMessage.PRODUCT_BATCH_SIZE_EXCEEDED.getMessage()));
    }
  }
}
