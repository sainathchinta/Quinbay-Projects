package com.gdn.mta.product.util;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.google.api.services.youtube.YouTube;
import com.google.api.services.youtube.model.PageInfo;
import com.google.api.services.youtube.model.VideoListResponse;

public class ValidateUrlUtilTest {

  private static final String YOUTUBE_URL = "https://www.youtube.com/watch?v=P1xAhgKTqDA";
  private static final String YOUTUBE_URL_ACTIVE_FALSE = "https://www.youtube.com/watch?v=nonActive";
  private static final String YOUTUBE_URL_1 = "https://youtu.be/xowQkxFXTNg";
  private static final String YOUTUBE_INVALID_URL = "https://www.youtube.com/watch?v";
  private static final String YOUTUBE_INVALID_URL_2 =
      "https://www.youtube.com/watch?v=P1xAhgKTqDA,https://www.youtube.com/watch?v=P1xAhgKTqDA";
  private static final String YOUTUBE_INVALID_URL_3 = "https://youtu.be/xowQkxFXTNg, https://youtu.be/iF99iKlDpxA";
  private static final String API_KEY = "apiKey";
  private static final String VIDEO_LIST = "snippet,contentDetails,statistics";
  private static final String VIDEO_REGEX_WITH_SHORTS =
      "(?<=watch\\?v=|/videos/|embed\\/|youtu"
          + ".be\\/|\\/v\\/|\\/e\\/|watch\\?v%3D|watch\\?feature=player_embedded&v=|%2Fvideos%2F"
          + "|embed%\u200C\u200B2F|youtu.be%2F|%2Fv%2F|shorts\\/)[^#\\&\\?\\n]*";
  @Mock
  private YouTube youTube;

  @Mock
  private YouTube.Videos videos;

  @Mock
  private YouTube.Videos.List list;
  private VideoListResponse videoListResponse;

  @BeforeEach
  public void before() {
    MockitoAnnotations.initMocks(this);
    videoListResponse = new VideoListResponse();
    PageInfo pageInfo = new PageInfo();
    pageInfo.setTotalResults(2);
    videoListResponse.setPageInfo(pageInfo);
  }

  @Test
  public void validateYouTubeUrlTest() throws Exception {
    Mockito.when(youTube.videos()).thenReturn(videos);
    Mockito.when(youTube.videos().list(VIDEO_LIST)).thenReturn(list);
    Mockito.when(list.execute()).thenReturn(videoListResponse);
    boolean youTubeUrlResponse = ValidateUrlUtil.validateYouTubeUrl(YOUTUBE_URL, API_KEY, youTube, true,
        "");
    Mockito.verify(youTube, Mockito.times(2)).videos();
    Mockito.verify(list).execute();
    Assertions.assertTrue(youTubeUrlResponse);
  }

  @Test
  public void validateYouTubeUrlActiveCheckFalseTest() throws Exception {
    Mockito.when(youTube.videos()).thenReturn(videos);
    Mockito.when(youTube.videos().list(VIDEO_LIST)).thenReturn(list);
    Mockito.when(list.execute()).thenReturn(videoListResponse);
    boolean youTubeUrlResponse = ValidateUrlUtil.validateYouTubeUrl(YOUTUBE_URL, API_KEY, youTube, false,
        "");
    Mockito.verify(youTube, Mockito.times(1)).videos();
    Assertions.assertTrue(youTubeUrlResponse);
  }

  @Test
  public void validateYouTubeUrlDifferentUrlTest() throws Exception {
    Mockito.when(youTube.videos()).thenReturn(videos);
    Mockito.when(youTube.videos().list(VIDEO_LIST)).thenReturn(list);
    Mockito.when(list.execute()).thenReturn(videoListResponse);
    boolean youTubeUrlResponse = ValidateUrlUtil.validateYouTubeUrl(YOUTUBE_URL_1, API_KEY, youTube, true,
        "");
    Mockito.verify(youTube, Mockito.times(2)).videos();
    Mockito.verify(list).execute();
    Assertions.assertTrue(youTubeUrlResponse);
  }

  @Test
  public void validateYouTubeUrlDifferentUrlActiveCheckFalseTest() throws Exception {
    Mockito.when(youTube.videos()).thenReturn(videos);
    Mockito.when(youTube.videos().list(VIDEO_LIST)).thenReturn(list);
    PageInfo pageInfo1 = new PageInfo();
    pageInfo1.setTotalResults(0);
    videoListResponse.setPageInfo(pageInfo1);
    Mockito.when(list.execute()).thenReturn(videoListResponse);
    boolean youTubeUrlResponse = ValidateUrlUtil.validateYouTubeUrl(YOUTUBE_URL_1, API_KEY, youTube, false,
        "");
    Mockito.verify(youTube, Mockito.times(1)).videos();
    Assertions.assertTrue(youTubeUrlResponse);
  }

  @Test
  public void validateYouTubeUrlDifferentUrlActiveCheckTrueTest() throws Exception {
    Mockito.when(youTube.videos()).thenReturn(videos);
    Mockito.when(youTube.videos().list(VIDEO_LIST)).thenReturn(list);
    PageInfo pageInfo1 = new PageInfo();
    pageInfo1.setTotalResults(0);
    videoListResponse.setPageInfo(pageInfo1);
    Mockito.when(list.execute()).thenReturn(videoListResponse);
    boolean youTubeUrlResponse = ValidateUrlUtil.validateYouTubeUrl(YOUTUBE_URL_1, API_KEY, youTube, true,
        "");
    Mockito.verify(youTube, Mockito.times(2)).videos();
    Assertions.assertFalse(youTubeUrlResponse);
  }

  @Test
  public void validateYouTubeUrlWithInvalidUrlTest() throws Exception {
    boolean youTubeUrlResponse = ValidateUrlUtil.validateYouTubeUrl(YOUTUBE_INVALID_URL, API_KEY, youTube, true,
        VIDEO_REGEX_WITH_SHORTS);
    Assertions.assertFalse(youTubeUrlResponse);
  }

  @Test
  public void validateYouTubeUrlWithInvalidUrlActiveCheckFalseTest() throws Exception {
    boolean youTubeUrlResponse = ValidateUrlUtil.validateYouTubeUrl(YOUTUBE_INVALID_URL, API_KEY, youTube, false,
        VIDEO_REGEX_WITH_SHORTS);
    Assertions.assertFalse(youTubeUrlResponse);
  }

  @Test
  public void validateYouTubeUrlWithInvalidUrlMultipleTest() throws Exception {
    boolean youTubeUrlResponse = ValidateUrlUtil.validateYouTubeUrl(YOUTUBE_INVALID_URL_2, API_KEY, youTube, true,
        "");
    Assertions.assertFalse(youTubeUrlResponse);
  }

  @Test
  public void validateYouTubeUrlWithInvalidUrlMultipleActiveCheckFalseTest() throws Exception {
    boolean youTubeUrlResponse = ValidateUrlUtil.validateYouTubeUrl(YOUTUBE_INVALID_URL_2, API_KEY, youTube, false,
        "");
    Assertions.assertFalse(youTubeUrlResponse);
  }

  @Test
  public void validateYouTubeUrlWithInvalidActiveCheckFalseTest() throws Exception {
    boolean youTubeUrlResponse = ValidateUrlUtil.validateYouTubeUrl(YOUTUBE_URL_ACTIVE_FALSE, API_KEY, youTube, false,
        "");
    Assertions.assertTrue(youTubeUrlResponse);
  }

  @Test
  public void validateYouTubeUrlWithInvalidUrlMultipleDifferentUrlTest() throws Exception {
    boolean youTubeUrlResponse = ValidateUrlUtil.validateYouTubeUrl(YOUTUBE_INVALID_URL_3, API_KEY, youTube, true,
        "");
    Assertions.assertFalse(youTubeUrlResponse);
  }

  @Test
  public void validateYouTubeUrlWithInvalidUrlMultipleDifferentUrlActiveCheckFalseTest() throws Exception {
    boolean youTubeUrlResponse = ValidateUrlUtil.validateYouTubeUrl(YOUTUBE_INVALID_URL_3, API_KEY, youTube, false,
        "");
    Assertions.assertFalse(youTubeUrlResponse);
  }

  @Test
  public void validateYouTubeUrlWithBlankUrlTest() throws Exception {
    boolean youTubeUrlResponse = ValidateUrlUtil.validateYouTubeUrl(StringUtils.EMPTY, API_KEY, youTube, true,
        "");
    Assertions.assertTrue(youTubeUrlResponse);
  }

  @Test
  public void validateYouTubeUrlWithBlankUrlActiveCheckFalseTest() throws Exception {
    boolean youTubeUrlResponse = ValidateUrlUtil.validateYouTubeUrl(StringUtils.EMPTY, API_KEY, youTube, false,
        "");
    Assertions.assertTrue(youTubeUrlResponse);
  }

  @Test
  public void isYouTubeUrlValidExceptionActiveCheckFalseTest() throws Exception {
    Mockito.when(youTube.videos()).thenReturn(videos);
    Mockito.when(youTube.videos().list(VIDEO_LIST)).thenReturn(list);
    Mockito.doThrow(RuntimeException.class).when(list).execute();
    boolean youTubeUrlResponse = ValidateUrlUtil.validateYouTubeUrl(YOUTUBE_URL, API_KEY, youTube, true,
        "");
    Mockito.verify(youTube, Mockito.times(2)).videos();
    Mockito.verify(list).execute();
    Assertions.assertFalse(youTubeUrlResponse);
  }

  @Test
  public void isYouTubeUrlValidExceptionTest() throws Exception {
    Mockito.when(youTube.videos()).thenReturn(videos);
    Mockito.when(youTube.videos().list(VIDEO_LIST)).thenReturn(list);
    Mockito.doThrow(RuntimeException.class).when(list).execute();
    boolean youTubeUrlResponse = ValidateUrlUtil.validateYouTubeUrl(YOUTUBE_URL, API_KEY, youTube, false,
        "");
    Mockito.verify(youTube, Mockito.times(1)).videos();
    Assertions.assertTrue(youTubeUrlResponse);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.youTube);
  }
}