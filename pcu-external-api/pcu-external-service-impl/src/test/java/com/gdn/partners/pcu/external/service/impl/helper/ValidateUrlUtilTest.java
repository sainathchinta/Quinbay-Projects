package com.gdn.partners.pcu.external.service.impl.helper;

import com.gdn.partners.pcu.external.web.model.response.YouTubeAPIWebResponse;
import com.google.api.services.youtube.YouTube;
import com.google.api.services.youtube.model.PageInfo;
import com.google.api.services.youtube.model.Thumbnail;
import com.google.api.services.youtube.model.ThumbnailDetails;
import com.google.api.services.youtube.model.Video;
import com.google.api.services.youtube.model.VideoListResponse;
import com.google.api.services.youtube.model.VideoLocalization;
import com.google.api.services.youtube.model.VideoSnippet;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.io.IOException;
import java.util.Arrays;

public class ValidateUrlUtilTest {

  private static final String VIDEO_ID = "P1xAhgKTqDA";
  private static final String API_KEY = "apiKey";
  private static final String VIDEO_LIST = "snippet,contentDetails,statistics";
  private static final String ID = "id";
  private static final String DESCRIPTION = "description";
  private static final String URL = "url";
  private static final String TITLE = "title";

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
    Video video = new Video();
    video.setId(ID);
    VideoSnippet videoSnippet = new VideoSnippet();
    videoSnippet.setCategoryId(ID);
    videoSnippet.setDescription(DESCRIPTION);
    ThumbnailDetails thumbnailDetails = new ThumbnailDetails();
    Thumbnail thumbnail = new Thumbnail();
    thumbnail.setUrl(URL);
    thumbnailDetails.setDefault(thumbnail);
    videoSnippet.setThumbnails(thumbnailDetails);
    VideoLocalization videoLocalization = new VideoLocalization();
    videoLocalization.setDescription(DESCRIPTION);
    videoLocalization.setTitle(TITLE);
    videoListResponse.setItems(Arrays.asList(video));
    videoSnippet.setLocalized(videoLocalization);
    videoListResponse.getItems().get(0).setSnippet(videoSnippet);
  }

  @Test
  public void isYouTubeUrlValidTest() throws IOException {
    Mockito.when(youTube.videos()).thenReturn(videos);
    Mockito.when(youTube.videos().list(VIDEO_LIST)).thenReturn(list);
    Mockito.when(list.execute()).thenReturn(videoListResponse);
    YouTubeAPIWebResponse youTubeUrlActive = ValidateUrlUtil.isYouTubeUrlActive(VIDEO_ID, API_KEY, youTube);
    Mockito.verify(youTube, Mockito.times(2)).videos();
    Mockito.verify(list).execute();
    Assertions.assertTrue(youTubeUrlActive.isValid());
    Assertions.assertEquals(DESCRIPTION, youTubeUrlActive.getDescription());
    Assertions.assertEquals(TITLE, youTubeUrlActive.getTitle());
    Assertions.assertEquals(URL, youTubeUrlActive.getThumbnailImageUrl());
  }

  @Test
  public void isYouTubeUrlValidExceptionTest() throws IOException {
    Mockito.when(youTube.videos()).thenReturn(videos);
    Mockito.when(youTube.videos().list(VIDEO_LIST)).thenReturn(list);
    Mockito.doThrow(RuntimeException.class).when(list).execute();
    YouTubeAPIWebResponse youTubeUrlActive = ValidateUrlUtil.isYouTubeUrlActive(VIDEO_ID, API_KEY, youTube);
    Mockito.verify(youTube, Mockito.times(2)).videos();
    Mockito.verify(list).execute();
    Assertions.assertFalse(youTubeUrlActive.isValid());
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.youTube);
  }
}
